-- Database: water_quality_preprocessing

-- DROP DATABASE IF EXISTS water_quality_preprocessing;

CREATE DATABASE water_quality_preprocessing
    WITH
    OWNER = postgres
    ENCODING = 'UTF8'
    LC_COLLATE = 'English_United States.1252'
    LC_CTYPE = 'English_United States.1252'
    TABLESPACE = pg_default
    CONNECTION LIMIT = -1
    IS_TEMPLATE = False;
	

/* Convert each date for all the years (2018-2023) 
using the method below 

Keep 2023 separated due to date being up to 20th July
whereas all the dates in other years are up to 19th of September consistently */
	
CREATE TABLE water_quality_2023(
				date VARCHAR,
				water_depth_avg DECIMAL,
				water_temp_avg DECIMAL,
				EC_25_avg DECIMAL
);



COPY water_quality_2023 FROM 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2022.csv'
DELIMITER ',' CSV HEADER;


select * from water_quality_2023;

ALTER TABLE water_quality_2023
	ADD COLUMN date_2023 DATE;
	
UPDATE water_quality_2023
	SET date_2023 = TO_DATE(date, 'DD-Mon-YY');

SELECT TO_CHAR(date_2023, 'DD/MM/YYYY') as date_2023 FROM water_quality_2022;

select * from water_quality_2023;

COPY (
SELECT
	date_2023,
	water_depth_avg,
	water_temp_avg,
	ec_25_avg
FROM water_quality_2023
)
TO 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2022_final.csv' WITH CSV HEADER;

/* 2022 */

CREATE TABLE water_quality_2022(
				date VARCHAR,
				water_depth_avg DECIMAL,
				water_temp_avg DECIMAL,
				EC_25_avg DECIMAL
);



COPY water_quality_2022 FROM 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2022.csv'
DELIMITER ',' CSV HEADER;

select * from water_quality_2022;

ALTER TABLE water_quality_2022
	ADD COLUMN date_2022 DATE;
	
UPDATE water_quality_2022
	SET date_2022 = TO_DATE(date, 'DD-Mon-YY');

SELECT TO_CHAR(date_2022, 'DD/MM/YYYY') as date_2022 FROM water_quality_2022;

select * from water_quality_2022;

COPY (
SELECT
	date_2022,
	water_depth_avg as water_depth_avg_2022,
	water_temp_avg as water_temp_avg_2022,
	ec_25_avg as ec_25_avg_2022
FROM water_quality_2022
)
TO 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2022_final1.csv' WITH CSV HEADER;

/* 2021 */

CREATE TABLE water_quality_2021(
				date VARCHAR,
				water_depth_avg DECIMAL,
				water_temp_avg DECIMAL,
				EC_25_avg DECIMAL
);



COPY water_quality_2021 FROM 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2021.csv'
DELIMITER ',' CSV HEADER;

select * from water_quality_2021;

ALTER TABLE water_quality_2021
	ADD COLUMN date_2021 DATE;
	
UPDATE water_quality_2021
	SET date_2021 = TO_DATE(date, 'DD-Mon-YY');

SELECT TO_CHAR(date_2021, 'DD/MM/YYYY') as date_2021 FROM water_quality_2021;

select * from water_quality_2021;

COPY (
SELECT
	date_2021,
	water_depth_avg,
	water_temp_avg,
	ec_25_avg
FROM water_quality_2021
)
TO 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2021_final.csv' WITH CSV HEADER;

/* 2020 */

CREATE TABLE water_quality_2020(
				date VARCHAR,
				water_depth_avg DECIMAL,
				water_temp_avg DECIMAL,
				EC_25_avg DECIMAL
);



COPY water_quality_2020 FROM 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2022.csv'
DELIMITER ',' CSV HEADER;

select * from water_quality_2020;

ALTER TABLE water_quality_2020
	ADD COLUMN date_2020 DATE;
	
UPDATE water_quality_2020
	SET date_2020 = TO_DATE(date, 'DD-Mon-YY');

SELECT TO_CHAR(date_2020, 'DD/MM/YYYY') as date_2020 FROM water_quality_2020;

select * from water_quality_2020;

COPY (
SELECT
	date_2020,
	water_depth_avg,
	water_temp_avg,
	ec_25_avg
FROM water_quality_2020
)
TO 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2020_final.csv' WITH CSV HEADER;

/* 2019 */

CREATE TABLE water_quality_2019(
				date VARCHAR,
				water_depth_avg DECIMAL,
				water_temp_avg DECIMAL,
				EC_25_avg DECIMAL
);



COPY water_quality_2019 FROM 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2019.csv'
DELIMITER ',' CSV HEADER;

select * from water_quality_2019;

ALTER TABLE water_quality_2019
	ADD COLUMN date_2019 DATE;
	
UPDATE water_quality_2019
	SET date_2019 = TO_DATE(date, 'DD-Mon-YY');

SELECT TO_CHAR(date_2019, 'DD/MM/YYYY') as date_2019 FROM water_quality_2019;

select * from water_quality_2019;

COPY (
SELECT
	date_2019,
	water_depth_avg,
	water_temp_avg,
	ec_25_avg
FROM water_quality_2019
)
TO 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2019_final.csv' WITH CSV HEADER;

/* 2018 */

CREATE TABLE water_quality_2018(
				date VARCHAR,
				water_depth_avg DECIMAL,
				water_temp_avg DECIMAL,
				EC_25_avg DECIMAL
);



COPY water_quality_2018 FROM 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2018.csv'
DELIMITER ',' CSV HEADER;

select * from water_quality_2018;

ALTER TABLE water_quality_2018
	ADD COLUMN date_2018 DATE;
	
UPDATE water_quality_2018
	SET date_2018 = TO_DATE(date, 'DD-Mon-YY');

SELECT TO_CHAR(date_2018, 'DD/MM/YYYY') as date_2018 FROM water_quality_2018;

select * from water_quality_2018;

COPY (
SELECT
	date_2018,
	water_depth_avg,
	water_temp_avg,
	ec_25_avg
FROM water_quality_2018
)
TO 'C:/Program Files/PostgreSQL/16/data/Data_copy/Cabrach_2018_final.csv' WITH CSV HEADER;


/* After saving the data output for each water quality year ONLY 2018 till 2022,
combine them together via Excel as normal. 

Keep 2023 separated due to data being available up to 20th July...

Keep the columns separated like date_2022 | date_2021 |...
...and water_temp_avg_2022 | water_temp_avg_2021 ...
you get the idea */