### Alexnet using keras

import matplotlib
matplotlib.use('Agg')
from matplotlib import pyplot as plt
import keras
from keras.models import Sequential
from keras.layers import Dense, Activation, Dropout, Flatten, Conv2D, MaxPooling2D
from keras.layers.normalization import BatchNormalization
import numpy as np
from keras.preprocessing.image import ImageDataGenerator
from keras.preprocessing.image import ImageDataGenerator, array_to_img, img_to_array, load_img
import tensorflow as tf
from keras.layers.convolutional import Conv2D, MaxPooling2D, ZeroPadding2D
from keras.regularizers import l2
from keras.models import load_model
from twilio.rest import Client
from keras.utils import multi_gpu_model
import time
import pickle

# Define callback time
class TimeHistory(keras.callbacks.Callback):
    def on_train_begin(self, logs={}):
        self.times = []

    def on_epoch_begin(self, batch, logs={}):
        self.epoch_time_start = time.time()

    def on_epoch_end(self, batch, logs={}):
        self.times.append(time.time() - self.epoch_time_start)

# verify GPU usage
sess = tf.Session(config=tf.ConfigProto(log_device_placement=True))


# training generators
batch_size = 16
input_size = (3,56,56)
nb_classes = 2
mean_flag = True # if False, then the mean subtraction layer is not prepended

#code ported from https://blog.keras.io/building-powerful-image-classification-models-using-very-little-data.html

# this is the augmentation configuration we will use for training
train_datagen = ImageDataGenerator(
        shear_range=0.2,
        zoom_range=0.2,
        horizontal_flip=True)

test_datagen = ImageDataGenerator()
                                  

train_generator = train_datagen.flow_from_directory(
    '/home/aleon/tiny-imagenet/tiny-imagenet-200/train/',  
    batch_size=batch_size,
    shuffle=True,
    target_size=input_size[1:],
    class_mode='categorical')  

validation_generator = test_datagen.flow_from_directory(
    '/home/aleon/tiny-imagenet/tiny-imagenet-200/val/test_img/', 
    batch_size=batch_size,
    target_size=input_size[1:],
    shuffle=True,
    class_mode='categorical')

# variant of alexnet

model = Sequential()

with tf.device('/cpu:0'):
    # 1st Convolutional Layer
    model.add(Conv2D(filters=96, input_shape=(56,56,3), kernel_size=(11,11),
                    strides=(4,4), padding='valid'))
    model.add(Activation('relu'))
    # Pooling 
    model.add(MaxPooling2D(pool_size=(3,3), strides=(2,2), padding='valid'))
    # Batch Normalisation before passing it to the next layer
    model.add(BatchNormalization())

    # 2nd Convolutional Layer
    # add padding
    model.add(ZeroPadding2D(padding=(2, 2)))
    model.add(Conv2D(filters=256, kernel_size=(5,5), strides=(1,1)))
    model.add(Activation('relu'))
    # Pooling
    model.add(MaxPooling2D(pool_size=(3,3), strides=(2,2)))
    # Batch Normalisation
    model.add(BatchNormalization())

    # 3rd Convolutional Layer
    model.add(ZeroPadding2D(padding=(1, 1)))
    model.add(Conv2D(filters=384, kernel_size=(3,3), strides=(1,1)))
    model.add(Activation('relu'))
    # Batch Normalisation
    model.add(BatchNormalization())

    # 4th Convolutional Layer
    model.add(ZeroPadding2D(padding=(1, 1)))
    model.add(Conv2D(filters=384, kernel_size=(3,3), strides=(1,1)))
    model.add(Activation('relu'))
    # Batch Normalisation
    model.add(BatchNormalization())

    # 5th Convolutional Layer
    model.add(ZeroPadding2D(padding=(1, 1)))
    model.add(Conv2D(filters=256, kernel_size=(3,3), strides=(1,1)))
    model.add(Activation('relu'))
    # Pooling
    model.add(ZeroPadding2D(padding=(1, 1)))
    model.add(MaxPooling2D(pool_size=(3,3), strides=(1,1), padding='valid'))
    # Batch Normalisation
    model.add(BatchNormalization())

with tf.device('/gpu:0'):
    # Passing it to a dense layer
    model.add(Flatten())
    # 1st Dense Layer
    model.add(Dense(4096, input_shape=(56*56*3,)))
    model.add(Activation('relu'))
    # Add Dropout to prevent overfitting
    model.add(Dropout(0.4))
    # Batch Normalisation
    model.add(BatchNormalization())


    # 2nd Dense Layer
    model.add(Dense(4096))
    model.add(Activation('relu'))
    # Add Dropout
    model.add(Dropout(0.4))
    # Batch Normalisation
    model.add(BatchNormalization())

    # 3rd Dense Layer
    model.add(Dense(1000))
    model.add(Activation('relu'))
    # Add Dropout
    model.add(Dropout(0.4))
    # Batch Normalisation
    model.add(BatchNormalization())

    # Output Layer
    model.add(Dense(200))
    model.add(Activation('softmax'))

    model.summary()


# define data parallelism
# (4) Compile 
#model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])




from keras.optimizers import SGD
#sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
#model.compile(loss='mse',
#             optimizer=sgd,
#             metrics=['accuracy'])


sgd = SGD(lr=0.01, decay=1e-6, momentum=0.9, nesterov=True)
model.compile(loss='mse',
            optimizer=sgd,
            metrics=['accuracy'])



time_callback = TimeHistory()
#model.compile(loss=keras.losses.categorical_crossentropy, optimizer='adam', metrics=["accuracy"])
history = model.fit_generator(train_generator,
                        steps_per_epoch=2000,
                        validation_data=validation_generator,
                        nb_val_samples=800,
                        nb_epoch=20,
                        verbose=1,
                        callbacks=[time_callback]
)
times = time_callback.times

with open('model-par_tinyImagenet_20epoch_times.txt', 'w') as f:
    for item in times:
        f.write("%s\n" % item)

# save model weights
model.save('model-par_tinyImagenet_20epochs_2nd.h5')  # creates a HDF5 file 'my_model.h5'


# save history object
with open('model-par_tinyImagnet_20_epochstrainHistoryDict', 'wb') as file_pi:
    pickle.dump(history.history, file_pi)


# sending text
# sending text
# Your Account Sid and Auth Token from twilio.com/console
account_sid = os.environ['TWILIO_ACT_ID']
auth_token = os.environ['TWILIO_AUTH_TOKEN']
client = Client(account_sid, auth_token)

message = client.messages .create(
        body="Model has completed training!",
        from_=os.environ['TWILIO_FROM_NUM'],
        to=os.environ['TWILIO_TO_NUM']
    )


print(message.sid)

plt.subplot(1,2,1)
plt.plot(history.history['acc'])
plt.plot(history.history['val_acc'])
plt.title('Accuracy v/s Epochs')
plt.ylabel('Accuracy')
plt.xlabel('Epoch')
plt.legend(['train', 'test'], loc='upper left') 

plt.subplot(1,2,2)
plt.plot(history.history['loss'])
plt.plot(history.history['val_loss'])
plt.title('Loss v/s Epochs')
plt.ylabel('M.S.E Loss')
plt.xlabel('Epoch')
plt.legend(['train', 'test'], loc='upper left') 

plt.tight_layout()
plt.savefig('model_par_tinyImagenet_20epochs_2nd.png')
