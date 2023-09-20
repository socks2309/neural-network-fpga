#FPGA Implementation of Simple Neural network
from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import train_test_split
# load the breast cancer dataset
X, y = load_breast_cancer(return_X_y=True)
# split the train and test dataset
X_train, X_test,\
	y_train, y_test = train_test_split(X, y,
									test_size=0.20,
									random_state=23)
#feature Scaling  
from sklearn.preprocessing import StandardScaler    
st_x= StandardScaler()    
X_train= st_x.fit_transform(X_train)    
X_test= st_x.transform(X_test)  
import keras
from keras.models import Sequential
from keras.layers import Dense, Dropout
# Initialising the ANN
classifier = Sequential()
# Adding the input layer and the first hidden layer
classifier.add(Dense(16, activation='relu', input_shape=(30,)))
# Adding dropout to prevent overfitting
#classifier.add(Dropout(0.1))
# Adding the second hidden layer
classifier.add(Dense(8, activation='relu'))
# Adding dropout to prevent overfitting
#classifier.add(Dropout(0.1))
# Adding the output layer
classifier.add(Dense(1, activation='sigmoid'))
# Compiling the ANN
classifier.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
# Fitting the ANN to the Training set
classifier.fit(X_train, y_train, batch_size=100, epochs=150)
# Predicting the Test set results
y_pred = classifier.predict(X_test)
y_keras=classifier.predict(X_test)
y_pred = (y_pred > 0.5)
# Making the Confusion Matrix
from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test, y_pred)
print(cm)
from sklearn.metrics import accuracy_score
print('Accuracy score is:',100*accuracy_score(y_test, y_pred),'%')
#print model parameters: weight and bias
first_layer_weights = classifier.layers[0].get_weights()[0]
first_layer_biases  = classifier.layers[0].get_weights()[1]
second_layer_weights = classifier.layers[1].get_weights()[0]
second_layer_biases  = classifier.layers[1].get_weights()[1]
third_layer_weights = classifier.layers[2].get_weights()[0]
third_layer_biases  = classifier.layers[2].get_weights()[1]
import numpy as np
#export model parameters for 1st layer
layer0_w = first_layer_weights
layer0_b = first_layer_biases
n_features = 30 #no of features as input
n_layer0_neuron = 16 #no of neuron in layer0
layer0_w = layer0_w.T
layer0_b =layer0_b
f=open('model_params_layer0.c','w')
#write weights of layer_0 into test file
f.write("double w0[16][30]= {")
for j in range(n_layer0_neuron):
    f.write("{")
    for i in range(n_features):
        f.write(str(layer0_w[j][i]))
        if(i<n_features - 1):
            f.write(",")
        if((i+1)%10==0):
            f.write("\n")
    f.write("}")
    if(j<n_layer0_neuron -1 ):
        #print(j)
        f.write(",")
f.write("};"+"\n\n")
#write bias values into file
f.write("double b0[16] = {")
for i in range(n_layer0_neuron):
    f.write(str(layer0_b[i]))
    if(i< n_layer0_neuron -1):
        f.write(",")
    if((i+1)%10==0):
        f.write("\n")
   
f.write("};"+"\n\n")
f.close()

#export model parameters for 2nd layer
layer1_w = second_layer_weights
layer1_b = second_layer_biases
n_features = n_layer0_neuron #no of features as input i.e o/p of prev layer
n_layer1_neuron = 8 #no of neuron in layer0
layer1_w = layer1_w.T
layer1_b = layer1_b
f=open('model_params_layer1.c','w')
#write weights of layer_1 into test file
f.write("double w1[8][16]= {")
for j in range(n_layer1_neuron):
    f.write("{")
    for i in range(n_features):
        f.write(str(layer1_w[j][i]))
        if(i<n_features - 1):
            f.write(",")
        if((i+1)%10==0):
            f.write("\n")
    f.write("}")
    if(j<n_layer1_neuron -1):
        #print(j)
        f.write(",")
f.write("};"+"\n\n")
#write bias values into file
f.write("double b1[8] = {")
for i in range(n_layer1_neuron):
    f.write(str(layer1_b[i]))
    if(i< n_layer1_neuron -1):
        f.write(",")
    if((i+1)%10==0):
        f.write("\n")
   
f.write("};"+"\n\n")
f.close()

#export model parameters for 3rd layer
layer2_w = third_layer_weights
layer2_b = third_layer_biases
n_features = n_layer1_neuron #no of features as input i.e o/p of prev layer
n_layer2_neuron = 1 #no of neuron in layer0
layer2_w = layer2_w.T
layer2_b = layer2_b
f=open('model_params_layer2.c','w')
#write weights of layer_2 into test file
f.write("double w2[8]= ")
for j in range(n_layer2_neuron):
    f.write("{")
    for i in range(n_features):
        f.write(str(layer2_w[j][i]))
        if(i<n_features - 1):
            f.write(",")
        if((i+1)%10==0):
            f.write("\n")
    
f.write("};"+"\n\n")
#write bias values into file
f.write("double b2 = ")
for i in range(n_layer2_neuron):
    f.write(str(layer2_b[i]))
    if(i< n_layer2_neuron -1):
        f.write(",")
    if((i+1)%10==0):
        f.write("\n")
   
f.write(";"+"\n\n")
f.close()
for jj in range(100):
	#Convert test features to fixed point for verification
	f = open("feature.c","w")
	f.write("double x[30] = {")
	for i in range(30):
    		f.write(str(X_test[jj][i]))
    		if(i<29):
            		f.write(",")
    		if((i+1)%10==0):
            		f.write("\n")
    
	f.write("};"+"\n\n")
	f.close()
	
	#call bash shell for doing additional task
	import os
	#Calling Bambu HLS tool
	run_HLS="bambu-0.9.7.AppImage simple_ann.c --top-fname=main --soft-float -lm"
	os.system(run_HLS)
	#calling ICARUS Verilog Simulator to simulate generated HDL code
	run_HDL="iverilog -o result main.v main_tb.v"
	os.system(run_HDL)
	show_result="vvp result"
	os.system(show_result)
	os.system("gcc simple_ann.c -lm")
	os.system("./a.out")
	print("Actual label for this input feature:",y_test[jj])
	print(" Keras label for this input feature:",y_pred[jj])
	print("\n***************************************")

