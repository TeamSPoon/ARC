# Use basic Python 3 image as launching point
FROM python:3-slim
# Add script
# ADD tutorial_code/tutorial_script.py /
# Install dependencies
# RUN pip install numpy
RUN apt update 
RUN mkdir /data
RUN apt-get update -y
RUN apt-get update && apt-get install -y software-properties-common
RUN apt install -y gpg
# RUN LC_ALL=C.UTF-8 add-apt-repository -y ppa:ondrej/php

RUN apt-get install -y unzip apt apt-utils # libpd-dev libcurl14-gnutls-dev 
# RUN docker-php-ext-install gd dev zip curl mongodb mbstring xml fpm
RUN sh -c "echo 'deb http://ppa.launchpad.net/papirus/papirus/ubuntu xenial main' > /etc/apt/sources.list.d/arc-kde-ppa.list"
RUN apt-key adv --recv-keys --keyserver keyserver.ubuntu.com E58A9D36647CAE7F
RUN apt-get update -y
RUN apt-get install software-properties-common -y
RUN LC_ALL=C.UTF-8 apt-add-repository -y ppa:swi-prolog/devel
RUN apt install -y swi-prolog swi-prolog-java
ADD prolog/kaggle_arc/kaggle_arc_exec /
ADD prolog/kaggle_arc/data /tutorial_code
# RUN apt-get install arc-kde -y
# Execute the script
# CMD ["python", "./tutorial_script.py"]
CMD ["./kaggle_arc_exec", "-g attempt_arcathon_in_docker"]
