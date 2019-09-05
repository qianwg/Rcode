apt-get install mysql-server
apt-get isntall mysql-client
apt-get install libmysqlclient-dev
netstat -tap | grep mysql 
mysql --version
输入root密码后，进入MySQL
登录：mysql -u root -p

use mysql

update user set host='%' where user='root';

grant all privileges on *.* TO 'root'@'%' identified by 'root的密码' WITH GRANT OPTION;

FLUSH PRIVILEGES;

3、修改本地访问的3306

vi /etc/mysql/mysql.conf.d/mysqld.cnf，注释bindaddress=127.0.0.1

:wq

4、重启MySQL

service mysql restart
