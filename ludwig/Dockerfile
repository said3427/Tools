# Start from a core stack version
FROM jupyter/datascience-notebook
# Install from requirements.txt file
COPY requirements.txt /tmp/

RUN pip install --requirement /tmp/requirements.txt && \
    fix-permissions $CONDA_DIR && \
    fix-permissions /home/$NB_USER

RUN python -m spacy download en

WORKDIR /home/$NB_USER/work/
