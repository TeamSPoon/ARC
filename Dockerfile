# Use basic Python 3 image as launching point
FROM python:3-slim
# Add script
ADD tutorial_code/tutorial_script.py /
# Install dependencies
RUN pip install numpy
RUN apt update 
RUN mkdir /data
RUN apt install -y swi-prolog
ADD prolog/kaggle_arc/kaggle_arc_exec /
# Execute the script
CMD ["python", "./tutorial_script.py"]
