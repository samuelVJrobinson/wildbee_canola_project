SELECT * FROM arthropod LIMIT 10;
SELECT * FROM arthropodJoinLandscape LIMIT 10;

SELECT BLID, genus, sum(Nbee) as N FROM readyAbundanceDitch group by BLID, genus order by N desc;
SELECT *FROM site ORDER BY distToWLID_km DESC;

SELECT * FROM trapSummary;

SELECT genus, species, count(*) AS N FROM arthropod GROUP BY genus, species HAVING N > 50 ORDER BY N DESC;