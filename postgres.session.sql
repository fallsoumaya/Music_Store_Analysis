SELECT title, first_name, last_name FROM employee
ORDER BY title DESC
LIMIT 1;

SELECT COUNT(*), billing_country 
FROM invoice
GROUP BY billing_country
ORDER BY COUNT(*) DESC;

SELECT total
FROM invoice
ORDER BY total DESC
LIMIT 3;

SELECT billing_city,SUM(total)
FROM invoice
GROUP BY billing_city
ORDER BY SUM(total) DESC
LIMIT 1;

SELECT customer.customer_id, first_name, last_name, SUM(total) AS total_spending
FROM customer
JOIN invoice ON customer.customer_id = invoice.customer_id
GROUP BY customer.customer_id
ORDER BY total_spending DESC
LIMIT 1;


SELECT customer.email, customer.first_name, customer.last_name, genre.name AS genre
FROM customer, genre
WHERE genre.name LIKE '%Rock%' and customer.email LIKE 'a%'
ORDER BY email ASC;

SELECT artist.name AS artist_name, COUNT(track.track_id) AS track_count
FROM artist
JOIN album ON artist.artist_id = album.artist_id
JOIN track ON album.album_id = track.album_id
JOIN genre ON track.genre_id = genre.genre_id
WHERE genre.name = 'Rock'
GROUP BY artist.name
ORDER BY track_count DESC
LIMIT 10;

SELECT track.name, track.milliseconds
FROM track
WHERE track.milliseconds > (SELECT AVG(milliseconds) FROM track)
ORDER BY track.milliseconds DESC;


WITH best_selling_artist AS (
    SELECT 
        artist.artist_id AS artist_id, 
        artist.name AS artist_name, 
        SUM(invoice_line.unit_price * invoice_line.quantity) AS total_sales
    FROM 
        invoice_line
    JOIN 
        track ON track.track_id = invoice_line.track_id
    JOIN 
        album ON album.album_id = track.album_id
    JOIN 
        artist ON artist.artist_id = album.artist_id
    GROUP BY 
        artist.artist_id, artist.name
    ORDER BY 
        total_sales DESC
)
SELECT 
    c.first_name || ' ' || c.last_name AS customer_name, 
    bsa.artist_name, 
    SUM(il.unit_price * il.quantity) AS total_spent
FROM 
    invoice i
JOIN 
    customer c ON c.customer_id = i.customer_id
JOIN 
    invoice_line il ON il.invoice_id = i.invoice_id
JOIN 
    track t ON t.track_id = il.track_id
JOIN 
    album alb ON alb.album_id = t.album_id
JOIN 
    best_selling_artist bsa ON bsa.artist_id = alb.artist_id
GROUP BY 
    customer_name, bsa.artist_name
ORDER BY 
    total_spent DESC;

WITH genre_purchases AS (
    SELECT 
        customer.country, 
        genre.name AS genre_name, 
        COUNT(invoice_line.quantity) AS purchases
    FROM 
        invoice_line
    JOIN 
        invoice ON invoice.invoice_id = invoice_line.invoice_id
    JOIN 
        customer ON customer.customer_id = invoice.customer_id
    JOIN 
        track ON track.track_id = invoice_line.track_id
    JOIN 
        genre ON genre.genre_id = track.genre_id
    GROUP BY 
        customer.country, genre.name
),
max_purchases AS (
    SELECT 
        country, 
        MAX(purchases) AS max_purchases
    FROM 
        genre_purchases
    GROUP BY 
        country
)
SELECT 
    gp.country, 
    gp.genre_name, 
    gp.purchases
FROM 
    genre_purchases gp
JOIN 
    max_purchases mp ON gp.country = mp.country AND gp.purchases = mp.max_purchases
ORDER BY 
    gp.country, gp.genre_name;

WITH Customer_with_country AS (
    SELECT 
        customer.customer_id,
        customer.first_name,
        customer.last_name,
        billing_country,
        SUM(invoice.total) AS total_spending
    FROM 
        invoice
    JOIN 
        customer ON customer.customer_id = invoice.customer_id
    GROUP BY 
        customer.customer_id, customer.first_name, customer.last_name, billing_country
),
Max_spending_per_country AS (
    SELECT 
        billing_country,
        MAX(total_spending) AS max_spending
    FROM 
        Customer_with_country
    GROUP BY 
        billing_country
)
SELECT 
    cwc.customer_id,
    cwc.first_name,
    cwc.last_name,
    cwc.billing_country,
    cwc.total_spending
FROM 
    Customer_with_country cwc
JOIN 
    Max_spending_per_country mspc ON cwc.billing_country = mspc.billing_country AND cwc.total_spending = mspc.max_spending
ORDER BY 
    cwc.billing_country, cwc.total_spending DESC;

