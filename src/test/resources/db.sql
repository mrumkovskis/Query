      -- HSQL DB CREATE SCRIPT --

CREATE TABLE WORK
 (WDATE DATE NOT NULL,
  EMPNO INTEGER NOT NULL,
  HOURS INTEGER NOT NULL,
  EMPNO_MGR INTEGER)
//
COMMENT ON TABLE work IS 'work'
COMMENT ON COLUMN work.wdate IS 'work date'

CREATE TABLE EMP
 (EMPNO INTEGER NOT NULL,
  ENAME VARCHAR(50),
  JOB VARCHAR(9),
  MGR INTEGER,
  HIREDATE DATE,
  SAL DECIMAL(7, 2),
  COMM DECIMAL(7, 2),
  DEPTNO INTEGER NOT NULL)
//
CREATE TABLE DEPT
 (DEPTNO INTEGER NOT NULL,
  DNAME VARCHAR(14),
  LOC VARCHAR(13))
//
CREATE TABLE DEPT_ADDR
 (DEPTNR INTEGER NOT NULL,
  ADDR VARCHAR(50) NOT NULL,
  ZIP_CODE VARCHAR(10),
  ADDR_NR INTEGER)
//
CREATE TABLE DEPT_SUB_ADDR
  (DEPTNO INTEGER NOT NULL,
   ADDR VARCHAR(50) NOT NULL)
//
CREATE TABLE ADDRESS
 (NR INTEGER NOT NULL,
  ADDR VARCHAR(50) NOT NULL)
//
CREATE TABLE DEPT_EQUIPMENT
  (DEPT_NAME VARCHAR(14) NOT NULL,
   EQUIPMENT VARCHAR(50) NOT NULL)
//
CREATE TABLE CAR
 (NR VARCHAR(10) NOT NULL,
  NAME VARCHAR(20) NOT NULL,
  IS_ACTIVE BOOLEAN,
  DEPTNR INTEGER,
  TYRES_NR INTEGER)
//
CREATE TABLE CAR_USAGE
 (CAR_NR VARCHAR(10) NOT NULL,
  EMPNO INTEGER NOT NULL,
  DATE_FROM DATE)
//
CREATE TABLE CAR_IMAGE
 (CARNR INTEGER NOT NULL,
  IMAGE BLOB(1K))
//
CREATE TABLE TYRES
  (NR INTEGER NOT NULL,
   CARNR VARCHAR(10) NOT NULL,
   BRAND VARCHAR(20) NOT NULL,
   SEASON VARCHAR(1) NOT NULL)
//
CREATE TABLE TYRES_USAGE
  (TUNR INTEGER NOT NULL,
   CARNR VARCHAR(10) NOT NULL,
   DATE_FROM DATE)
//
CREATE TABLE SALGRADE
 (GRADE INTEGER,
  LOSAL INTEGER,
  HISAL INTEGER)
//
CREATE TABLE ABSTIME_TBL
 (F1 TIMESTAMP )
//
CREATE TABLE DUMMY (DUMMY INTEGER)
//
alter table dept add primary key (deptno)
//
alter table dept add unique (dname)
//
alter table emp add primary key (empno)
//
alter table emp add foreign key (deptno) references dept(deptno)
//
alter table emp add foreign key (mgr) references emp(empno)
//
alter table salgrade add primary key (grade)
//
alter table work add foreign key (empno) references emp(empno) on delete cascade
//
alter table work add foreign key (empno_mgr) references emp(empno)
//
alter table car add primary key (nr)
//
alter table car add foreign key (deptnr) references dept(deptno)
//
alter table dept_addr add primary key (deptnr)
//
alter table dept_addr add foreign key (deptnr) references dept(deptno)
//
alter table dept_sub_addr add foreign key (deptno) references dept_addr(deptnr)
//
alter table address add primary key (nr)
//
alter table dept_addr add foreign key (addr_nr) references address(nr)
//
alter table dept_equipment add foreign key (dept_name) references dept(dname)
//
alter table car_usage add primary key (car_nr, empno)
//
alter table car_usage add foreign key (car_nr) references car(nr)
//
alter table car_usage add foreign key (empno) references emp(empno)
//
alter table tyres add primary key (nr)
//
alter table tyres add foreign key (carnr) references car(nr)
//
alter table car add foreign key (tyres_nr) references tyres(nr)
//
alter table tyres add unique (nr, carnr)
//
alter table tyres_usage add foreign key (tunr) references tyres(nr) on delete cascade
//
alter table tyres_usage add foreign key (tunr, carnr) references tyres(nr, carnr)
//
alter table tyres_usage add foreign key (carnr) references car(nr)
//

CREATE FUNCTION inc_val_5 (x INTEGER)
  RETURNS INTEGER
  RETURN x + 5
//
CREATE PROCEDURE in_out(INOUT newid INTEGER, OUT outstring VARCHAR(100), IN instring VARCHAR(100))
BEGIN ATOMIC
  SET newid = newid + 5;
  SET outstring = instring;
END
//
CREATE SEQUENCE seq START WITH 10000
//
create schema accounts
    create table account(id integer not null,
        number varchar(20) not null, balance decimal(7, 2) not null, empno integer)
    create table transaction(id integer not null,
        originator_id integer not null, beneficiary_id integer not null,
        amount decimal(7, 2) not null, tr_date date not null)
//
alter table accounts.account add primary key (id)
//
alter table accounts.account add constraint emp_ref foreign key (empno) references public.emp(empno)
//
alter table accounts.transaction add primary key (id)
//
alter table accounts.transaction add constraint originator_ref foreign key (originator_id) references account(id)
//
alter table accounts.transaction add constraint beneficiary_ref foreign key (beneficiary_id) references account(id)
