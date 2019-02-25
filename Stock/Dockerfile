# Start from a core stack version
FROM jupyter/datascience-notebook
# Install from requirements.txt file
COPY requirements.txt /tmp/

RUN conda install -y quandl numpy pandas matplotlib pystan


RUN pip install --requirement /tmp/requirements.txt && \
    fix-permissions $CONDA_DIR && \
    fix-permissions /home/$NB_USER

