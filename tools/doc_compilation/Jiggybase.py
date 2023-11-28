

import jiggybase
import os

api_key = os.getenv('JIGGYBASE_KEY')

jb = jiggybase.JiggyBase(api_key)

collection = jb.collection('SeaSondeR')

collection.delete_docs(delete_all=True)



folder_path = 'tools/doc_compilation/'


files = os.listdir(folder_path)


docs = [file for file in files if file.endswith('.txt')]


for file in docs:
    full_path = os.path.join(folder_path, file)
    if os.path.isfile(full_path):
      collection.upsert_file(full_path)



