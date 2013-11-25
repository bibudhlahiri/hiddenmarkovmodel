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
add column client_ip  varchar(100);

update browsing_sessions
set client_ip = substring(ClientIPServerIP from 1 for position('_' in ClientIPServerIP) - 1)
where position('_' in ClientIPServerIP) > 0;
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

/*create sequence http_request_id_seq;
ALTER TABLE http_requests add COLUMN id bigint;
ALTER TABLE http_requests ALTER COLUMN id SET DEFAULT nextval('http_request_id_seq');

delete from http_requests

select *
from http_requests
limit 5*/
