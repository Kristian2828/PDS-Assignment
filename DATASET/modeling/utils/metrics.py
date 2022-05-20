import pickle
import matplotlib.pyplot as plt
from sklearn.metrics import classification_report, ConfusionMatrixDisplay

y_test = None


def evaluate(y_pred):
    global y_test

    if y_test is None:
        y_test = load_test_labels()

    print("Classification Report:")
    print(classification_report(y_test, y_pred))

    print("Confusion Matrix:")
    ConfusionMatrixDisplay.from_predictions(y_test, y_pred)
    plt.show()


def load_test_labels():
    with open("../data/y_test.pickle", "rb") as f:
        return pickle.load(f)