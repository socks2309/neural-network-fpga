import os
from sklearn.datasets import load_breast_cancer
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import confusion_matrix, accuracy_score
from keras.models import Sequential
from keras.layers import Dense, Dropout

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
    file_name = f"{os.path.expanduser('~')}/neural-network-fpga/main-branch/src/c/params_layer{layer}.c"
    f = open(file_name, 'w')
    f.write("#include \"../../include/network.h\"\n\n")

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

# Exporting test labels -->
file_name = f"{os.path.expanduser('~')}/neural-network-fpga/main-branch/src/c/test_features.c"
test_feat = open(file_name, "w")
test_feat.write("#include \"../../include/network.h\"\n\n")
test_feat.write("short y_test[100] = {")
for i in range(100):
    test_feat.write(str(y_test[i]))
    if (i < 100):
        test_feat.write(",")
test_feat.write("};")
test_feat.close()

# Exporting keras predictions -->
file_name = f"{os.path.expanduser('~')}/neural-network-fpga/main-branch/src/c/keras_predicted_features.c"
pred_feat = open(file_name, "w")
pred_feat.write("#include <stdbool.h>\n")
pred_feat.write("#include \"../../include/network.h\"\n\n")

pred_feat.write("bool y_pred[100] = {")
for i in range(100):
    pred_feat.write(str(y_pred[i]).strip("[ ]").lower())
    if (i < 100):
        pred_feat.write(",")
pred_feat.write("};")
pred_feat.close()

#Exporting test images ->
file_name = f"{os.path.expanduser('~')}/neural-network-fpga/main-branch/src/c/features.c"
file = open(file_name, "w")
file.write("double x[100][30] = {")
for i in range(100):
    file.write("{")
    for j in range(30):
        file.write(str(x_test[i][j])) #type: ignore
        if (j < 29):
            file.write(",")
    file.write("}")
    if (i < 99):
        file.write(",")
file.write("};")
file.close()