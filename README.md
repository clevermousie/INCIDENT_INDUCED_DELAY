# INCIDENT_INDUCED_DELAY

PEMS data file (0.0)

STA_ID (1.0)

Incident_period (2.0)

incident_info.txt (3.0)

crash_input.txt (4.0)

Total_delay (9)

INCIDENT_FREE_DB (10)

Station_distribution (11)

distribution_estimator (15)

OCCUPANCY_DETECT (16)

Flowchart

1.0,4.0 --> 9

9 --> day_long.txt 9.1     output.txt 9.2

incident_period (2.0)--> 10

10--> incident_free_db.txt 10.1

10.1 --> 11

11 --> station_dis.txt 11.1

9.2--> 15

15 --> incident_impact.txt 15.1

15.1, 2.0, 3.0 --> 16

16 --> occupancy_station.txt 16.1

Input file formatting:

2: month1 dom1 year1 hour1 minute-of-hour1 month2 day-of-month2 year2 hour2 minute-of-hour2 availability ( yes -1, no -0)

4: incident-name, date1(m/d/yy), time1(h:m), date2(m/d/yy),time2(h:m),severity, priority,impact, mp, availability

output file formatting:
9.1: flag, incident_id, flag, day-of-weak

9.2: incident_id, day-of-year2, day-to-calculate, crash-hour2, crash_minute2, up-station, down-station, station-coef, delay-at-end-of-incident

10.1: day-of-week minute-of-day day-of-year availability

11.1: day-of-week minute-of-day station total_delay total_delay_squared_root day_count

15.1: flag, incident_id, up_station, dow_station

16.1: incident_id, station, minute-of-year(start), minute-of-year(finish), total_delay, severity, priority, impact
