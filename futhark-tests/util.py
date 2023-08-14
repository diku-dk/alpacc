import os
import shutil

class DeleteNew():

    def __init__(self):
        self.old_content = None

    def __enter__(self):
        self.old_content = set(os.listdir())

    def __exit__(self, *args, **kwargs):
        new_content = set(os.listdir()) - self.old_content
        for content in new_content:
            if os.path.isdir(content):
                shutil.rmtree(content)
            elif os.path.isfile(content):
                os.remove(content)
        self.old_content = None