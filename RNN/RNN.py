#César Alejandro Aceves Díaz
#Alejandro Becerra Ramírez
#Isaac Ulises Ascencio Padilla
#Alfonso Vidrio Lizaola
#Waldo Gómez Plascencia

import tensorflow as tf
from tensorflow.keras.layers import Embedding, LSTM, Dense
from tensorflow.keras.models import Sequential
from tensorflow.keras.preprocessing.text import Tokenizer
from nltk.tokenize import word_tokenize
import numpy as np
from sklearn.model_selection import train_test_split
from tensorflow.keras.preprocessing.sequence import pad_sequences
from tensorflow.keras.layers import Dropout, Bidirectional

max_len = 100  # Por ejemplo, puedes ajustar este valor según tus necesidades

# Cargar datos
with open("Consultas.txt", "r") as f:
    consultas = f.readlines()

# Inicializa el tokenizer y crea el vocabulario
tokenizer = Tokenizer()
tokenizer.fit_on_texts(consultas)
vocab_size = len(tokenizer.word_index) + 1 

# Preprocesamiento de texto sin padding
def preprocesar_texto(texto):
    tokens = word_tokenize(texto.lower())
    secuencias = tokenizer.texts_to_sequences([tokens])[0]
    if len(secuencias) == 0:
        secuencias = [0]  # Puedes asignar un índice especial para representar un token vacío
    return secuencias

# Convertir consultas a secuencias procesadas
consultas_procesadas = [preprocesar_texto(consulta) for consulta in consultas]

# Separar las últimas palabras de cada consulta como las etiquetas objetivo
X = [consulta[:-1] for consulta in consultas_procesadas]
y = [consulta[-1] for consulta in consultas_procesadas]

# Convertir y a formato one-hot encoding
y = tf.keras.utils.to_categorical(y, num_classes=vocab_size)

# Separar en conjuntos de entrenamiento y validación
X_train, X_val, y_train, y_val = train_test_split(X, y, test_size=0.2)

embedding_dim = 256  # Aumentar la dimensionalidad de la incrustación

# Crear el modelo
model = Sequential()
model.add(Embedding(vocab_size, embedding_dim, input_length=None))  # No se especifica input_length
model.add(Bidirectional(LSTM(units=128, return_sequences=True)))  # Cambiar a Bidireccional
model.add(Dropout(0.5))  # Añadir Dropout
model.add(Bidirectional(LSTM(units=128)))  # Cambiar a Bidireccional
model.add(Dropout(0.5))  # Añadir Dropout
model.add(Dense(vocab_size, activation='softmax'))  # Mantener 'softmax'
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])

# Convertir listas de listas a arrays NumPy con secuencias de longitud uniforme
X_train = pad_sequences(X_train, maxlen=max_len, padding='post')
X_val = pad_sequences(X_val, maxlen=max_len, padding='post')

# Entrenar el modelo
# model.fit(X_train, y_train, epochs=50, batch_size=32, validation_data=(X_val, y_val)) 

# guardar el modelo
modelo = model.save("modelo_rnn.h5")

# Visualizar la arquitectura de la red neuronal
from keras.utils import plot_model
plot_model(model, to_file='model.png', show_shapes=True, show_layer_activations=True, show_layer_names=True)


# leer el modelo
model = tf.keras.models.load_model("modelo_rnn.h5")

# Predicción
def predecir_siguiente_palabra(texto):
    secuencia = preprocesar_texto(texto)
    prediccion = model.predict(np.array([secuencia]))
    indice_prediccion = np.argmax(prediccion[0])
    palabra_prediccion = tokenizer.index_word[indice_prediccion]
    return palabra_prediccion

consulta_incompleta = "SELECT * FROM libros" 
palabra_predicha = predecir_siguiente_palabra(consulta_incompleta)


# Visualizar la arquitectura de la red neuronal
from keras.utils import plot_model
plot_model(modelo,to_file='model.png',show_shapes=True,show_layer_activations=True,show_layer_names=True) 

# palabra_predicha = tokenizer.index_word[18]
# print(palabra_predicha)
# Predicción

# def predecir_en_tiempo_real():
#   texto_actual = ""
#   while True:
#     caracter = input()
#     if caracter == "\n":  # Presionar Enter para terminar
#       break
#     texto_actual += caracter
#     secuencia = preprocesar_texto(texto_actual)
#     prediccion = model.predict(secuencia)
#     indice_prediccion = tf.argmax(prediccion[0]).numpy()
#     palabra_predicha = tokenizer.index_word[indice_prediccion]
#     print(f"Predicción: {palabra_predicha}")

# # Llamar a la función para iniciar la predicción en tiempo real
# predecir_en_tiempo_real()
