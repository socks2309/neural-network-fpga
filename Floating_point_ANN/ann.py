from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import confusion_matrix, accuracy_score
from keras.models import Sequential
from keras.layers import Dense, Dropout
import os

x,y = load_breast_cancer(return_X_y = True)

x_train, x_test, \
    y_train, y_test = train_test_split(x,y,
                                       test_size=0.20,
                                       random_state=23)

st_x = StandardScaler()
x_train = st_x.fit_transform(x_train)
x_test = st_x.transform(x_test)

# Initializing ANN
classifier = Sequential()
classifier.add(Dense(16, activation='relu', input_shape=(30,)))
#classifier.add(Dropout(0.1))
classifier.add(Dense(8,activation='relu'))
#classifier.add(Dropout(0.1))
#Output layer
classifier.add(Dense(1,activation='sigmoid'))

#Compiling ANN
classifier.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

#Fitting the ANN to training set
classifier.fit(x_train, y_train, batch_size=100, epochs=200)

#Predictions
print("\n")
y_pred = classifier.predict(x_test)
y_keras = classifier.predict(x_test)
y_pred = (y_pred > 0.5)

#Making confusion matrix
c_matrix = confusion_matrix(y_test, y_pred)
print("Confusion matrix: ")
print(c_matrix)
print("\n")

print('Accuracy score is:',100*accuracy_score(y_test, y_pred),'%\n\n')

#Print model parameters: weight and bias
first_layer_weights = classifier.layers[0].get_weights()[0]
first_layer_biases = classifier.layers[0].get_weights()[1]
second_layer_weights = classifier.layers[1].get_weights()[0]
second_layer_biases = classifier.layers[1].get_weights()[1]
output_layer_weights = classifier.layers[2].get_weights()[0]
output_layer_biases = classifier.layers[2].get_weights()[1]

# Model parameters exporting function
def export_model_params(layer, layer_w, layer_b, n_features, n_neurons):
    layer_w = layer_w.T
    layer_b = layer_b
    file_name = f"params_layer{layer}.c"
    f = open(file_name, 'w')

    #Writing weights
    f.write(f"double weights_layer{layer}[{n_neurons}][{n_features}] = " + "{")
    for j in range (n_neurons):
        f.write("{")
        for i in range(n_features):
            f.write(str(layer_w[j][i]))
            if (i < n_features - 1):
                f.write(",")
            """
            if ((i+1)%10 == 0):
                f.write("\n")
            """
        f.write("}")
        if(j < n_neurons - 1):
            f.write(",\n")
    f.write("};" + "\n\n")

    #Writing biases
    f.write(f"double biases_layer{layer}[{n_neurons}] = " + "{")
    for i in range(n_neurons):
        f.write(str(layer_b[i]))
        if (i < n_neurons - 1):
            f.write(",")
        """
        if((i+1)%10 == 0):
            f.write("\n")
        """
    f.write("};" + "\n\n")
    f.close()

#Exporting params
export_model_params(0,first_layer_weights, first_layer_biases, 30, 16) #Layer 1
export_model_params(1,second_layer_weights,second_layer_biases,16, 8) #Layer 2
export_model_params(2, output_layer_weights, output_layer_biases, 8, 1) #Output layer


#Testing for first test image x_test[0][]->
file = open("feature.c", "w")
file.write("double x[30] = {")
for i in range(30):
    file.write(str(x_test[1][i]))
    if(i < 29):
        file.write(",")
    if((i+1)%10 == 0):
        file.write("\n")
file.write("};" + "\n\n")
file.close()
print("Actual label for this input feature: ", y_test[1])



#Calling Bambu tool
run_HLS = "/home/socks/bambu-0.9.7.AppImage ann.c --top-fname=main --soft-float -lm"
os.system(run_HLS)

#Generating HDL code using Icarus Verilog
run_HDL = "iverilog -o result main.v main_tb.v"
os.system(run_HDL)
show_result = "vvp result"
os.system(show_result)
os.system("gcc ann.c -lm")
os.system("./a.out")

#Predictions for C file
print("Actual label for this input feature: ", y_test[0])
print("Keras label for this input feature: ", y_pred[0])
print("\n******************************************************\n")