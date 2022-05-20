import pickle


def load_file(file_name):
    path = "../data/" + file_name
    with open(path, "rb") as f:
        return pickle.load(f)
