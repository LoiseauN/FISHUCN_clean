import subprocess
import sys
 
pkgs = ["pandas", "torch", "torchvision", "pyreadr", "joblib", "scikit-learn"]
 
# Find packages not installed
not_installed_pkgs = [pkg for pkg in pkgs if subprocess.call(["python3", "-m", "pip", "show", pkg]) != 0]
 
# Install packages and their dependencies
for pkg in not_installed_pkgs:
     subprocess.check_call(["python3", "-m", "pip", "install", pkg])
    
import pandas as pd
import torch.nn as nn
import numpy as np
import torch
import math
import pyreadr
import joblib
from pathlib import Path
from sklearn import preprocessing
from sklearn.metrics import accuracy_score

# Class for RNN model
class Model(nn.Module):
    def __init__(self, input_size):
        super().__init__()
        self.hidden = nn.Linear(input_size, 256)
        self.output = nn.Linear(256, 2)
        self.sigmoid = nn.Sigmoid()
        self.softmax = nn.Softmax(dim=1)

    def forward(self, x):
        x = self.hidden(x)
        x = self.sigmoid(x)
        x = self.output(x)
        x = self.softmax(x)
        return x

def prepare_data(data): # Remove all species without IUCN status and label encoder for categorical data
    le = preprocessing.LabelEncoder()
    num_cols = data._get_numeric_data().columns
    cat_cols = list(set(data.columns) - set(num_cols))
    del cat_cols[cat_cols.index('IUCN')]
    for category in cat_cols:
        data[category] = le.fit_transform(data[category])
        joblib.dump(le, str(Path(Path(__file__).parent, 'Label_Encoder', 'le_')) + category + '.joblib')
    data_no_na = data.dropna()
    data_no_na['IUCN']=le.fit_transform(data_no_na['IUCN'])
    joblib.dump(le, str(Path(Path(__file__).parent, 'Label_Encoder', 'le_IUCN.joblib')))
    return data_no_na

def split(data): # Create balanced database 
    data = data.sample(frac=1).reset_index(drop=True)
    df0 = data.loc[data["IUCN"] == 0]
    df1 = data.loc[data["IUCN"] == 1]
    nb_split = math.floor(max(len(df0), len(df1))/min(len(df0), len(df1)))
    if len(df0) >= len(df1):
        split_max = np.array_split(df0, nb_split)
        df_to_append = df1
    else:
        split_max = np.array_split(df1, nb_split)
        df_to_append = df0
    split = []
    for i in range(len(split_max)):
        s = pd.concat([split_max[i], df_to_append], axis=0)
        split.append(s)  
    return(split)

def train(df_all_split, nb_run_per_split): # Train XX models for accuracy with 80/20 
    inputs = df_all_split[0].columns
    inputs = inputs.delete(inputs =='IUCN')
    targets = ['IUCN']

    k = 0
    acc_total = []
    for df_split in df_all_split:
        acc_batch = []
        for run in range(int(nb_run_per_split)):
            #shuffle
            df_split = df_split.sample(frac=1).reset_index(drop=True)
            # data to tensor
            data = np.stack([df_split[col].values for col in inputs], 1)
            data = torch.tensor(data, dtype=torch.float)

            #Output to tensor
            outputs = torch.tensor(df_split[targets].values, dtype=torch.float).flatten()
            outputs = outputs.type(torch.LongTensor)

            # train / test separation (80% / 20%)
            total_records = len(data)
            test_records = int(total_records * .2)
            train_data = data[:total_records-test_records]
            test_data = data[total_records-test_records:total_records]
            train_outputs = outputs[:total_records-test_records]
            test_outputs = outputs[total_records-test_records:total_records]

            # Initialize model and loss function
            model = Model(train_data.shape[1])
            loss_function = nn.CrossEntropyLoss()
            # loss_function = nn.NLLLoss()
            optimizer = torch.optim.Adam(model.parameters(), lr=0.001)
            aggregated_losses = []
            epochs = 2500

            # train
            for i in range(epochs):
                i += 1
                y_pred = model(train_data)
                single_loss = loss_function(y_pred, train_outputs)
                
                optimizer.zero_grad()
                single_loss.backward()
                optimizer.step()

                aggregated_losses.append(single_loss)

            # test
            with torch.no_grad():
                y_val = model(test_data)
            y_val = np.argmax(y_val, axis=1)

            acc_total.append((accuracy_score(test_outputs, y_val)))
            acc_batch.append((accuracy_score(test_outputs, y_val)))
        print("batch", k , "acc :", sum(acc_batch)/len(acc_batch))
        k = k + 1
    print("mean acc total : ", sum(acc_total)/len(acc_total))

    return (sum(acc_total)/len(acc_total))

def predict(original_df, df_all_split, nb_run_per_split): # Train XX model with 100% and predict

    inputs = df_all_split[0].columns
    inputs = inputs.delete(inputs =='IUCN')
    targets = ['IUCN']

    le = preprocessing.LabelEncoder()
    num_cols = original_df._get_numeric_data().columns
    cat_cols = list(set(original_df.columns) - set(num_cols))
    del cat_cols[cat_cols.index('IUCN')]
    for category in cat_cols:
        le = joblib.load(str(Path(Path(__file__).parent, 'Label_Encoder', 'le_')) + category + '.joblib')
        original_df[category] = le.fit_transform(original_df[category])
        
    df_to_predict = original_df[original_df["IUCN"].isnull()]
    data_to_predict = np.stack([df_to_predict[col].values for col in inputs], 1)
    data_to_predict = torch.tensor(data_to_predict, dtype=torch.float)

    inputs = df_all_split[0].columns
    inputs = inputs.delete(inputs =='IUCN')
    targets = ['IUCN']

    k = 0
    acc_total = []
    res_all = []
    for df_split in df_all_split:
        acc_batch = []
        for run in range(int(nb_run_per_split)):
            #shuffle
            df_split = df_split.sample(frac=1).reset_index(drop=True)
            # data to tensor
            data = np.stack([df_split[col].values for col in inputs], 1)
            data = torch.tensor(data, dtype=torch.float)

            #Output to tensor
            outputs = torch.tensor(df_split[targets].values, dtype=torch.float).flatten()
            outputs = outputs.type(torch.LongTensor)

            # Initialize model and loss function
            model = Model(data.shape[1])
            loss_function = nn.CrossEntropyLoss()
            # loss_function = nn.NLLLoss()
            optimizer = torch.optim.Adam(model.parameters(), lr=0.001)
            aggregated_losses = []
            epochs = 2500

            # train
            for i in range(epochs):
                i += 1
                y_pred = model(data)
                single_loss = loss_function(y_pred, outputs)
                
                optimizer.zero_grad()
                single_loss.backward()
                optimizer.step()

                aggregated_losses.append(single_loss)

            model.eval()
            with torch.no_grad():
                res = model(data_to_predict)
            res = np.argmax(res, axis=1).tolist()
            res_all.append(res)

    # 80% threshold calculation
    nb_model = len(res_all) # res_all = nbr models
    s1 = nb_model * 80 / 100
    s2 = nb_model * 20 / 100

    res = pd.DataFrame(columns = ['species', 'IUCN', 'percentage'])
    le = joblib.load(str(Path(Path(__file__).parent, 'Label_Encoder', 'le_IUCN.joblib')))
    for i in range(len(res_all[1])):
            s = 0
            for j in range(len(res_all)):
                s = s + res_all[j][i]
            if s > s1 :
                res.loc[len(res)] = [df_to_predict.iloc[i].name, le.inverse_transform([1])[0], s*100/len(res_all)]
            elif s < s2 :
                res.loc[len(res)] = [df_to_predict.iloc[i].name, le.inverse_transform([0])[0], 100-(s*100/len(res_all))]
            else :
                res.loc[len(res)] = [df_to_predict.iloc[i].name, 'NaN',max(s*100/len(res_all),100-(s*100/len(res_all)))]

    result_path = Path(Path(__file__).parent.parent, 'outputs', 'IUCN_preds_deep.Rdata')
    pyreadr.write_rdata(result_path, res, df_name='IUCN_preds_deep')

file_path = Path(__file__)
data_path = Path(file_path.parent.parent, 'outputs', 'data_noNA.Rdata')
data = pyreadr.read_r(data_path)
for key, value in data.items(): 
    data = value
data_no_na = prepare_data(data)
splits = split(data_no_na)
acc = train(splits, 10)
print(acc)
predict(data, splits, 10)
