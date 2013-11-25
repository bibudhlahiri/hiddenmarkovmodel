--are there client IPs which are sometimes bot and sometimes benign
select ClientIPServerIP, client_ip, substring(ClientIPServerIP from 1 for position('_' in ClientIPServerIP) - 1)
from browsing_sessions
where position('_' in ClientIPServerIP) > 0

--36 (36% if 99) client IPs switched between different categories
select client_ip, count(distinct MarkedCategory)
from browsing_sessions
where MarkedCategory in ('User', 'Bot')
group by client_ip
having count(distinct MarkedCategory) > 1

select position('x' in 'abc')

--99 distinct client IPs
select count(distinct client_ip)
from browsing_sessions
where client_ip is not null

select *
from browsing_sessions
where client_ip = '192.168.113.111'
order by browsingssessionstarttimesec

select client_ip
from browsing_sessions
where position('_' in ClientIPServerIP) = 0

--"192.168.112.66_timesofindia.indiatimes.com"
select ClientIPServerIP
from browsing_sessions
limit 1

--"192.168.112.66_timesofindia.indiatimes.com"
select ClientIPServerIP
from http_requests
limit 1

--"192.168.112.66_timesofindia.indiatimes.com_1"
select id
from browsing_sessions
limit 1

--"192.168.112.66_184.26.162.33_55908_80"
select sessionid
from http_requests
limit 1

select *
from http_requests
order by clientip, serverip, clientport, 
serverport, "timestamp", sessionid, 
browsingsessionid
limit 5

select clientip, serverip, clientport, 
serverport, sessionid, browsingsessionid, count(*)
from http_requests
group by clientip, serverip, clientport, 
serverport, sessionid, browsingsessionid
order by count(*) desc


select *
from http_requests
where clientip = '192.168.50.55' 
and serverip = '96.17.182.65'
and clientport = 56045
and serverport = 80
and sessionid = '192.168.50.55_96.17.182.65_56045_80'
and browsingsessionid = 1

select *
from browsing_sessions bs, http_requests hr
where bs.ClientIPServerIP = hr.ClientIPServerIP
and bs.ClientIPServerIP like '192.168.50.55%'
and to_char(bs.browsingssessionstarttimesec, '999999999999') like '1378%'

--Match two tables, 1762 rows. The last number in bs.id after the last '_' 
--matches the browsingsessionid in hr. Basically, a session is defined by 
--client IP, server IP, and session ID.
select bs.SequenceOfRequest, hr.browsingsessionid, hr.urlids, hr.timestamp
from browsing_sessions bs, http_requests hr
where bs.ClientIPServerIP = hr.ClientIPServerIP
and bs.ClientIPServerIP like '192.168.50.55%'
and substring(bs.id from (char_length(bs.id) - 1) for 2) = '_1'
and hr.browsingsessionid = 1
order by hr.timestamp



--1 row
select bs.*
---bs.SequenceOfRequest
from browsing_sessions bs
where bs.ClientIPServerIP like '192.168.50.55%'
and substring(bs.id from (char_length(bs.id) - 1) for 2) = '_1'

--1,762 rows
select hr.*
--hr.browsingsessionid, hr.urlids, hr.timestamp
from http_requests hr
where hr.ClientIPServerIP like '192.168.50.55%'
and hr.browsingsessionid = 1
order by hr.timestamp

--Two distinct values of serverip: "96.17.182.65" and "96.17.182.40"
select distinct(serverip)
--hr.browsingsessionid, hr.urlids, hr.timestamp
from http_requests hr
where hr.ClientIPServerIP like '192.168.50.55%'
and hr.browsingsessionid = 1

--99
select count(distinct clientip)
from http_requests

--14,369 distinct URLs
select distinct(urlids)
from http_requests
order by urlids

select completeurl, urlids, urllevels
from http_requests
limit 5

select bs.ClientIPServerIP, bs.SequenceOfRequest, bs.browsingsessionid, hr.browsingsessionid, hr.urlids, hr.timestamp
from browsing_sessions bs, http_requests hr
where bs.ClientIPServerIP = hr.ClientIPServerIP
and bs.browsingsessionid = hr.browsingsessionid
and bs.MarkedCategory = 'Bot'
order by hr.timestamp

--219,857
select count(*) from http_requests

--Relative frequencies of URLs from HTTP requests
select cast(a.n as real)/219857
from 
(select urlids, count(*) n
 from http_requests
 group by urlids        
 order by count(*) desc) a


select clientipserverip, completeurl, querystring, requesttype, urlids, urllevels
from http_requests
limit 5

select completeurl
from http_requests
where urlids = 3273

select client_ip, count(*)
from browsing_sessions
group by client_ip
order by count(*) desc

select *
from browsing_sessions
where client_ip = '192.168.50.219'
order by browsingsessionid, browsingssessionstarttimesec

select *
from browsing_sessions
where uniqueurls is null

--633
select count(distinct bs1.id)
from browsing_sessions bs1
where bs1.MarkedCategory = 'User'

--For how many user sessions, the immediate next sessions are either user or bot?
--571
select count(distinct bs1.id)
from browsing_sessions bs1, browsing_sessions bs2
where bs1.MarkedCategory = 'User'
and bs2.MarkedCategory in ('User', 'Bot')
and bs1.client_ip = bs2.client_ip
and bs2.browsingsessionid > bs1.browsingsessionid
and not exists (select 1 from browsing_sessions bs3
                where bs3.client_ip = bs1.client_ip
                and bs3.browsingsessionid > bs1.browsingsessionid
                and bs3.browsingsessionid < bs2.browsingsessionid
                and bs3.MarkedCategory in ('User', 'Bot'))
                
--For how many user sessions, the next session by the same client was a bot session?
--69 (12% of 571)
select count(distinct bs1.id)
from browsing_sessions bs1, browsing_sessions bs2
where bs1.MarkedCategory = 'User'
and bs2.MarkedCategory = 'Bot'
and bs1.client_ip = bs2.client_ip
and bs2.browsingsessionid > bs1.browsingsessionid
and not exists (select 1 from browsing_sessions bs3
                where bs3.client_ip = bs1.client_ip
                and bs3.browsingsessionid > bs1.browsingsessionid
                and bs3.browsingsessionid < bs2.browsingsessionid
                and bs3.MarkedCategory in ('User', 'Bot'))

--For how many user sessions, the next session by the same client was a user session?
--502 (88% of 571)
select count(distinct bs1.id)
from browsing_sessions bs1, browsing_sessions bs2
where bs1.MarkedCategory = 'User'
and bs2.MarkedCategory = 'User'
and bs1.client_ip = bs2.client_ip
and bs2.browsingsessionid > bs1.browsingsessionid
and not exists (select 1 from browsing_sessions bs3
                where bs3.client_ip = bs2.client_ip
                and bs3.browsingsessionid > bs1.browsingsessionid
                and bs3.browsingsessionid < bs2.browsingsessionid
                and bs3.MarkedCategory in ('User', 'Bot'))


--For how many bot sessions, the immediate next sessions are either user or bot?
--214
select count(distinct bs1.id)
from browsing_sessions bs1, browsing_sessions bs2
where bs1.MarkedCategory = 'Bot'
and bs2.MarkedCategory in ('User', 'Bot')
and bs1.client_ip = bs2.client_ip
and bs2.browsingsessionid > bs1.browsingsessionid
and not exists (select 1 from browsing_sessions bs3
                where bs3.client_ip = bs1.client_ip
                and bs3.browsingsessionid > bs1.browsingsessionid
                and bs3.browsingsessionid < bs2.browsingsessionid
                and bs3.MarkedCategory in ('User', 'Bot'))

--For how many bot sessions, the immediate next sessions is user session?
--66 (31% of 214)
select count(distinct bs1.id)
from browsing_sessions bs1, browsing_sessions bs2
where bs1.MarkedCategory = 'Bot'
and bs2.MarkedCategory = 'User'
and bs1.client_ip = bs2.client_ip
and bs2.browsingsessionid > bs1.browsingsessionid
and not exists (select 1 from browsing_sessions bs3
                where bs3.client_ip = bs1.client_ip
                and bs3.browsingsessionid > bs1.browsingsessionid
                and bs3.browsingsessionid < bs2.browsingsessionid
                and bs3.MarkedCategory in ('User', 'Bot'))

--For how many bot sessions, the immediate next sessions is bot session?
--148 (69% of 214)
select count(distinct bs1.id)
from browsing_sessions bs1, browsing_sessions bs2
where bs1.MarkedCategory = 'Bot'
and bs2.MarkedCategory = 'Bot'
and bs1.client_ip = bs2.client_ip
and bs2.browsingsessionid > bs1.browsingsessionid
and not exists (select 1 from browsing_sessions bs3
                where bs3.client_ip = bs1.client_ip
                and bs3.browsingsessionid > bs1.browsingsessionid
                and bs3.browsingsessionid < bs2.browsingsessionid
                and bs3.MarkedCategory in ('User', 'Bot'))

--What are the probabilities of the 11k distinct URLs, given the session is a bot session?
--28% of the URLs are ID 63 ("http://timesofindia.indiatimes.com/breakingnews/breakingnews.html")
--8233 distinct URLs appear.
select sum(b.freq)
from 
(select hr.urlids as urlid, cast(count(*) as real)/72313 as freq
from interesting_sessions i, http_requests hr
where i.ClientIPServerIP = hr.ClientIPServerIP
and i.browsingsessionid = hr.browsingsessionid
and i.MarkedCategory = 'Bot'
group by hr.urlids
order by hr.urlids) b

--72,313
select sum(a.n)
from 
(select hr.urlids, count(*) n
 from interesting_sessions i, http_requests hr
 where i.ClientIPServerIP = hr.ClientIPServerIP
 and i.browsingsessionid = hr.browsingsessionid
 and i.MarkedCategory = 'Bot'
 group by hr.urlids
 order by count(*) desc) a


--What are the probabilities of the 11k distinct URLs, given the session is a user session?
--9.4% of the URLs are 63; 5233 distinct URLs appear
select sum(b.freq)
from 
(select hr.urlids, cast(count(*) as real)/46857 as freq
from interesting_sessions i, http_requests hr
where i.ClientIPServerIP = hr.ClientIPServerIP
and i.browsingsessionid = hr.browsingsessionid
and i.MarkedCategory = 'User'
group by hr.urlids
order by count(*) desc) b

--46,857
select sum(a.n)
from 
(select hr.urlids, count(*) n
 from interesting_sessions i, http_requests hr
 where i.ClientIPServerIP = hr.ClientIPServerIP
 and i.browsingsessionid = hr.browsingsessionid
 and i.MarkedCategory = 'User'
 group by hr.urlids
 order by count(*) desc) a


select distinct completeurl
from http_requests
where urlids = 63

select distinct hr.urlids as urlid
from interesting_sessions i, http_requests hr
where i.ClientIPServerIP = hr.ClientIPServerIP
and i.browsingsessionid = hr.browsingsessionid
order by hr.urlids

--For how many user or bot sessions, the immediate next session is user or bot?
--785, which equals 571 + 214
create table interesting_sessions as
select bs1.*
from browsing_sessions bs1, browsing_sessions bs2
where bs1.MarkedCategory in ('User', 'Bot')
and bs2.MarkedCategory in ('User', 'Bot')
and bs1.client_ip = bs2.client_ip
and bs2.browsingsessionid > bs1.browsingsessionid
and not exists (select 1 from browsing_sessions bs3
                where bs3.client_ip = bs1.client_ip
                and bs3.browsingsessionid > bs1.browsingsessionid
                and bs3.browsingsessionid < bs2.browsingsessionid
                and bs3.MarkedCategory in ('User', 'Bot'))


select count(*) 
from interesting_sessions


select markedcategory, count(*) 
from interesting_sessions
group by markedcategory


select client_ip, count(*) 
from interesting_sessions
group by client_ip
order by count(*) desc

--Take the user with max sessions
select *
from interesting_sessions
where client_ip = '192.168.50.219'
and MarkedCategory = 'Bot'

select i.browsingsessionid, i.MarkedCategory, hr.urlids as urlid
from interesting_sessions i, http_requests hr
where i.ClientIPServerIP = hr.ClientIPServerIP
and i.browsingsessionid = hr.browsingsessionid
and client_ip = '192.168.50.219'
order by i.browsingsessionid, hr.timestamp


