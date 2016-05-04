CREATE TABLE deficienciesQuery AS
SELECT state, STRFTIME('%Y', survey_date_output ) AS year, count (*) AS numDeficiencies FROM Deficiencies_Download
WHERE (year BETWEEN '2010' AND '2015')
GROUP BY state, year;

CREATE TABLE penaltiesQuery AS
SELECT state, STRFTIME('%Y', pnlty_date ) AS year, count (*) AS numPenalties FROM Penalties_Download
WHERE (year BETWEEN '2010' AND '2015')
GROUP BY state, year;

CREATE TABLE deficienciesPenaltiesLJ AS
SELECT * FROM deficienciesQuery LEFT OUTER JOIN penaltiesQuery ON deficienciesQuery.state = penaltiesQuery.state AND deficienciesQuery.year = penaltiesQuery.year;

CREATE TABLE deficienciesPenaltiesFinal AS
SELECT state, year, IFNULL(numDeficiencies,0) AS numDeficiencies, IFNULL(numPenalties,0) AS numPenalties FROM deficienciesPenaltiesLJ;

