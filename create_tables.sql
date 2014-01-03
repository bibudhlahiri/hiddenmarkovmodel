create table browsing_sessions
( 
  id varchar(100),
  ClientIPServerIP varchar(100),
  BrowsingSessionID numeric, --should be integer
  BrowsingsSessionStartTimeSec numeric,
  BrowsingSessionEndTimeSec numeric,
  Host varchar(500),
  BrowsingSessionDuration integer,
  UniqueURLs integer,
  TotalURLs integer,
  AvgUrlVisited numeric,
  TotalPages integer,
  SequenceOfRequest varchar(50000),
  MarkedCategory varchar(20)
);

copy browsing_sessions
from '/Users/blahiri/cleartrail_ddos/data/WebServerDDoSDataSet/browsing_sessions_selected_columns.csv' 
WITH CSV HEADER DELIMITER ',';

select count(*) 
from browsing_sessions;

alter table browsing_sessions
add column client_ip varchar(100);

update browsing_sessions
set client_ip = substring(ClientIPServerIP from 1 for position('_' in ClientIPServerIP) - 1)
where position('_' in ClientIPServerIP) > 0;

CREATE INDEX idx_browsing_sessions_clientIPserverIP on browsing_sessions (ClientIPServerIP);
CREATE INDEX idx_browsing_sessions_browsingsessionID on browsing_sessions (BrowsingSessionID);
----------------------------------------------
drop table if exists http_requests;
create table http_requests
( 
 ClientIP varchar(100),
 ServerIP varchar(100),
 ClientPort integer,
 ServerPort integer,
 TimeStamp numeric,
 CompleteURL varchar(500),
 QueryString varchar(5000),
 RequestType integer,
 Length integer,
 Host varchar(500),
 UserAgent varchar(500),
 Referer varchar(500),
 RequestSize integer,
 ResponseCode integer,
 ContentLength integer,
 ResponseSize integer,
 SessionID varchar(500),
 ClientIPServerIP varchar(500),
 BrowsingSessionID integer,
 PageViewingSessionID integer,
 URLIDs integer,
 UrlLevels integer,
 QueryParameterCount integer,
 IsImage boolean
);

copy http_requests
from '/Users/blahiri/cleartrail_ddos/data/WebServerDDoSDataSet/HttpRequestStats.csv' 
WITH CSV HEADER DELIMITER ','  ENCODING 'ISO_8859_5';

select count(*) 
from http_requests;

CREATE INDEX idx_http_requests_clientIPserverIP on http_requests (ClientIPServerIP);
CREATE INDEX idx_http_requests_browsingsessionID on http_requests (BrowsingSessionID);

/*create sequence http_request_id_seq;
ALTER TABLE http_requests add COLUMN id bigint;
ALTER TABLE http_requests ALTER COLUMN id SET DEFAULT nextval('http_request_id_seq');

delete from http_requests

select *
from http_requests
limit 5*/
--------------------------------------------------

drop table if exists transition_raw_user_sessions;
create table transition_raw_user_sessions
( 
  url1 integer,
  url2 integer,
  frequency integer
);

copy transition_raw_user_sessions
from '/Users/blahiri/cleartrail_ddos/data/transition_raw_user_sessions.csv' 
WITH CSV HEADER DELIMITER ','  ENCODING 'ISO_8859_5';

select count(*)
from transition_raw_user_sessions;
--------------------------------------------------

drop table if exists two_grams;
create table two_grams
(
  id SERIAL PRIMARY KEY,
  ClientIPServerIP varchar(500),
  BrowsingSessionID integer,
  gram_sequence varchar(50),
  frequency integer
);

copy two_grams (ClientIPServerIP, BrowsingSessionID, gram_sequence, frequency)
from '/Users/blahiri/cleartrail_ddos/data/2gram.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select count(*)
from two_grams

select *
from two_grams
limit 5
------------------------
drop table if exists three_grams;
create table three_grams
(
  id SERIAL PRIMARY KEY,
  ClientIPServerIP varchar(500),
  BrowsingSessionID integer,
  gram_sequence varchar(50),
  frequency integer
);

copy three_grams (ClientIPServerIP, BrowsingSessionID, gram_sequence, frequency)
from '/Users/blahiri/cleartrail_ddos/data/n_gram/3_gram.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select count(*)
from three_grams;

select *
from three_grams
limit 5
------------------------

drop table if exists four_grams;
create table four_grams
(
  id SERIAL PRIMARY KEY,
  ClientIPServerIP varchar(500),
  BrowsingSessionID integer,
  gram_sequence varchar(50),
  frequency integer
);

copy four_grams (ClientIPServerIP, BrowsingSessionID, gram_sequence, frequency)
from '/Users/blahiri/cleartrail_ddos/data/n_gram/4_gram.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select count(*)
from four_grams;

select *
from four_grams
limit 5;
----------------------------

drop table if exists five_grams;
create table five_grams
(
  id SERIAL PRIMARY KEY,
  ClientIPServerIP varchar(500),
  BrowsingSessionID integer,
  gram_sequence varchar(50),
  frequency integer
);

copy five_grams (ClientIPServerIP, BrowsingSessionID, gram_sequence, frequency)
from '/Users/blahiri/cleartrail_ddos/data/n_gram/5_gram.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select count(*)
from five_grams;

select *
from five_grams
limit 5;
------------------------

drop table if exists two_grams_with_pages;
create table two_grams_with_pages
(
  id SERIAL PRIMARY KEY,
  ClientIPServerIP varchar(500),
  BrowsingSessionID integer,
  gram_sequence varchar(50),
  frequency integer
);

copy two_grams_with_pages (ClientIPServerIP, BrowsingSessionID, gram_sequence, frequency)
from '/Users/blahiri/cleartrail_ddos/data/pages/n_gram/2gram_sequence.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select count(*)
from two_grams_with_pages;

select *
from two_grams_with_pages
limit 5
-----------------------
drop table if exists http_requests_pages;
create table http_requests_pages
( 
 ClientIP varchar(100),
 ServerIP varchar(100),
 ClientPort integer,
 ServerPort integer,
 TimeStamp numeric,
 CompleteURL varchar(500),
 QueryString varchar(5000),
 RequestType integer,
 Length integer,
 Host varchar(500),
 UserAgent varchar(500),
 Referer varchar(500),
 RequestSize integer,
 ResponseCode integer,
 ContentLength integer,
 ResponseSize integer,
 SessionID varchar(500),
 ClientIPServerIP varchar(500),
 BrowsingSessionID integer,
 PageViewingSessionID integer,
 URLIDs integer,
 UrlLevels integer,
 QueryParameterCount integer,
 IsImage boolean
);

copy http_requests_pages
from '/Users/blahiri/cleartrail_ddos/data/pages/page_data/page_data.csv' 
WITH CSV HEADER DELIMITER ','  ENCODING 'ISO_8859_5';

--96,947
select count(*) 
from http_requests_pages;

CREATE INDEX idx_http_requests_pages_clientIPserverIP on http_requests_pages (ClientIPServerIP);
CREATE INDEX idx_http_requests_pages_browsingsessionID on http_requests_pages (BrowsingSessionID);
--------------------------------------------------------------------------------------------------
drop table if exists http_requests_objects;
create table http_requests_objects
( 
 ClientIP varchar(100),
 ServerIP varchar(100),
 ClientPort integer,
 ServerPort integer,
 TimeStamp numeric,
 CompleteURL varchar(500),
 QueryString varchar(5000),
 RequestType integer,
 Length integer,
 Host varchar(500),
 UserAgent varchar(500),
 Referer varchar(500),
 RequestSize integer,
 ResponseCode integer,
 ContentLength integer,
 ResponseSize integer,
 SessionID varchar(500),
 ClientIPServerIP varchar(500),
 BrowsingSessionID integer,
 PageViewingSessionID integer,
 URLIDs integer,
 UrlLevels integer,
 QueryParameterCount integer,
 IsImage boolean
);

copy http_requests_objects
from '/Users/blahiri/cleartrail_ddos/data/pages/page_data/object_data.csv' 
WITH CSV HEADER DELIMITER ','  ENCODING 'ISO_8859_5';

--122,910
select count(*) 
from http_requests_objects;

CREATE INDEX idx_http_requests_objects_clientIPserverIP on http_requests_objects (ClientIPServerIP);
CREATE INDEX idx_http_requests_objects_browsingsessionID on http_requests_objects (BrowsingSessionID);
-------------------------------------------------------------------------------------------------------

drop table if exists three_grams_with_pages;
create table three_grams_with_pages
(
  id SERIAL PRIMARY KEY,
  ClientIPServerIP varchar(500),
  BrowsingSessionID integer,
  gram_sequence varchar(50),
  frequency integer
);

copy three_grams_with_pages (ClientIPServerIP, BrowsingSessionID, gram_sequence, frequency)
from '/Users/blahiri/cleartrail_ddos/data/pages/n_gram/3gram_sequence.csv' 
WITH CSV HEADER DELIMITER ',' ENCODING 'ISO_8859_5';

select count(*)
from three_grams_with_pages;

select *
from three_grams_with_pages
limit 5
