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
 and hr.urlids in (1, 3501, 3498, 3499, 20, 38, 4297, 3496, 
 47, 44, 58, 24, 59, 61, 63, 65, 62, 4433, 4434, 4770, 4435, 
 4436, 4683, 4684, 4438, 4439, 4764, 4440, 4765, 4443, 4444, 
 4767, 4448, 4768)
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
and client_ip = '192.168.50.93'
order by i.browsingsessionid, hr.timestamp

select i.client_ip, i.browsingsessionid, i.MarkedCategory, hr.urlids as urlid
from interesting_sessions i, http_requests hr
where i.ClientIPServerIP = hr.ClientIPServerIP
and i.browsingsessionid = hr.browsingsessionid
order by i.client_ip, i.browsingsessionid, hr.timestamp

--62 distinct client IPs, 34 have some bot sessions
select distinct(client_ip)
from interesting_sessions
where markedcategory = 'Bot'

select distinct(client_ip)
from interesting_sessions
where markedcategory = 'User'

--IP with highest bot sessions
select client_ip, count(distinct browsingsessionid)
from interesting_sessions
where markedcategory = 'Bot'
group by client_ip
order by count(distinct browsingsessionid) desc

--37 sessions, 34 bot
select *
from interesting_sessions
where client_ip = '192.168.50.92'
and markedcategory = 'Bot'



select client_ip, 
count(distinct browsingsessionid) n_sessions, 
sum(case when markedcategory = 'User' then 1 else 0 end) as n_user_sessions,
sum(case when markedcategory = 'Bot' then 1 else 0 end) as n_bot_sessions
from interesting_sessions
--where client_ip = '192.168.50.93'
group by client_ip
having count(distinct browsingsessionid) >= 10
order by count(distinct browsingsessionid) desc


select clientip, count(*)
from http_requests
group by clientip
order by count(*)

--Markov transition matrix among URLs for user sessions
explain
select hr1.urlids, hr2.urlids, count(*)
--bs.ClientIPServerIP, bs.browsingsessionid, hr1.urlids, hr2.urlids
from browsing_sessions bs, http_requests hr1, http_requests hr2
where bs.ClientIPServerIP = hr1.ClientIPServerIP
and bs.browsingsessionid = hr1.browsingsessionid
and hr1.ClientIPServerIP = hr2.ClientIPServerIP
and hr1.browsingsessionid = hr2.browsingsessionid
and hr2.timestamp > hr1.timestamp
and bs.MarkedCategory = 'User'
and not exists (select 1 from http_requests hr3
                where hr3.ClientIPServerIP = hr2.ClientIPServerIP
                and hr3.browsingsessionid = hr2.browsingsessionid
                and hr3.timestamp > hr1.timestamp
                and hr3.timestamp < hr2.timestamp)
group by hr1.urlids, hr2.urlids
order by count(*) desc
--bs.ClientIPServerIP, bs.browsingsessionid, hr1.timestamp, hr2.timestamp

select hr1.urlids, hr2.urlids, count(*)
--bs.ClientIPServerIP, bs.browsingsessionid, hr1.urlids, hr2.urlids
from browsing_sessions bs, http_requests hr1, http_requests hr2
where bs.ClientIPServerIP = hr1.ClientIPServerIP
and bs.browsingsessionid = hr1.browsingsessionid
and hr1.ClientIPServerIP = hr2.ClientIPServerIP
and hr1.browsingsessionid = hr2.browsingsessionid
and hr2.timestamp > hr1.timestamp
and bs.MarkedCategory = 'Bot'
and not exists (select 1 from http_requests hr3
                where hr3.ClientIPServerIP = hr2.ClientIPServerIP
                and hr3.browsingsessionid = hr2.browsingsessionid
                and hr3.timestamp > hr1.timestamp
                and hr3.timestamp < hr2.timestamp)
group by hr1.urlids, hr2.urlids
order by count(*) desc


select *
from transition_raw_user_sessions
order by url1, frequency desc


select trus1.url1, trus1.url2, cast(trus1.frequency as real)/(select sum(trus2.frequency) from transition_raw_user_sessions trus2 where trus2.url1 = trus1.url1)
from transition_raw_user_sessions trus1
order by trus1.url1, trus1.frequency desc

                
