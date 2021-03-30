library(DBI)
library(RMySQL)
myconn <- dbConnect(MySQL(), user='root', password='Sep032016!', dbname='employees')
dbListTables(myconn)

# Descriptions of tables
query <- "DESC employees"
(employeesTable.desc <- dbGetQuery(myconn, query))

query <- "DESC dept_manager"
(dept_managerTable.desc <- dbGetQuery(myconn, query))

query <- "DESC departments"
(departmentsTable.desc <- dbGetQuery(myconn, query))

query <- "DESC titles"
(titlesTable.desc <- dbGetQuery(myconn, query))

query <- "DESC salaries"
(salariesTable.desc <- dbGetQuery(myconn, query))

query <- "DESC dept_emp"
(dept_empTable.desc <- dbGetQuery(myconn, query))


# Return number of different deptartment numbers
query <- "SELECT COUNT(DISTINCT dept_no) FROM dept_emp;"
(q <- dbGetQuery(myconn, query))

# Return max and min of hire date
query <- "SELECT MIN(hire_date), MAX(hire_date) FROM employees;"
(q <- dbGetQuery(myconn, query))


# Return information about Managers
query <- "SELECT e.emp_no, e.first_name, e.last_name, dm.dept_no, e.hire_date, t.title 
        FROM employees e JOIN dept_manager dm ON e.emp_no = dm.emp_no
        JOIN titles t ON e.emp_no = t.emp_no
        WHERE t.title = 'Manager'
        ORDER BY e.emp_no;"
(q <- dbGetQuery(myconn, query))


# Return number of different titles
query <- "SELECT COUNT(DISTINCT title) FROM titles;"
(q <- dbGetQuery(myconn, query))

# Return list of different titles
query <- "SELECT DISTINCT title FROM titles;"
(titlesDistinct <- dbGetQuery(myconn, query))


# Return list of Senior Engineer salaries who were hired on or after Jan 1, 1997
query <- "SELECT s.salary, t.title, e.emp_no, e.gender, e.hire_date
          FROM salaries s JOIN titles t ON t.emp_no = s.emp_no
          JOIN employees e on e.emp_no = s.emp_no
          WHERE title = 'Senior Engineer'
          AND hire_date >= '1997-01-01';"
sen.eng <- dbGetQuery(myconn, query)
head(sen.eng)
nrow(sen.eng)


# Return list of Engineer salaries who were hired on or after Jan 1, 1997
query <- "SELECT s.salary, t.title, e.emp_no, e.gender, e.hire_date
          FROM salaries s JOIN titles t ON t.emp_no = s.emp_no
          JOIN employees e on e.emp_no = s.emp_no
          WHERE title = 'Engineer'
          AND hire_date >= '1997-01-01';"
mid.eng <- dbGetQuery(myconn, query)
head(mid.eng)
nrow(mid.eng)

# Combine Engineer employee information
mid_sen.eng <- rbind(mid.eng, sen.eng)
nrow(mid_sen.eng)

# linear model of Engineer employee information
lm.eng <- lm(salary~title+gender, data=mid_sen.eng)
summary(lm.eng) 
# gender is statistically significant
mean(mid.eng$salary)
mean(sen.eng$salary)
# non-senior engineers make slightly more than senior engineers on average, 
# however, these variables are not statistically significant in the above model



# Return list of Senior Staff and Staff salaries who were hired on or after Jan 1, 1997
query <- "SELECT s.salary, t.title, e.emp_no, e.gender, e.hire_date
          FROM salaries s JOIN titles t ON t.emp_no = s.emp_no
          JOIN employees e on e.emp_no = s.emp_no
          WHERE title = 'Staff' OR title = 'Senior Staff'
          AND hire_date >= '1997-01-01';"
staff <- dbGetQuery(myconn, query)
head(staff)
nrow(staff)

# linear model of staff employee information
lm.staff <- lm(salary~title+gender, data=staff)
summary(lm.staff) # gender is not statistically significant

dbDisconnect(myconn)

