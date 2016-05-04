CREATE TABLE providersQuery AS
SELECT provnum, provname, state, zip,  bedcert, certification,  
participation_date, overall_rating, survey_rating, quality_rating, staffing_rating, RN_staffing_rating,
TOTHRD, exp_total, adj_total,  WEIGHTED_ALL_CYCLES_SCORE,  incident_cnt, cmplnt_cnt, FINE_CNT, TOT_PENLTY_CNT
FROM ProviderInfo_Download;
