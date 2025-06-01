--- Creating tables for the database

CREATE TABLE Warehouse
(
  ZipCode CHAR(5) NOT NULL,
  WarehouseAddress VARCHAR(200) NOT NULL,
  WarehouseID CHAR(3) NOT NULL,
  PRIMARY KEY (WarehouseID)
);

CREATE TABLE UsedBook
(
  Title VARCHAR(100) NOT NULL,
  ISBN CHAR(13) NOT NULL,
  YearPublished CHAR(4) NOT NULL,
  Author VARCHAR(50) NOT NULL,
  Language VARCHAR(25) NOT NULL,
  Publisher VARCHAR(100) NOT NULL,
  Condition VARCHAR(25) NOT NULL,
  SellingPrice NUMERIC(4,2) NOT NULL,
  NumOfCopies INT NOT NULL,
  PurchasingPrice NUMERIC(4,2) NOT NULL,
  WarehouseID CHAR(3) NOT NULL,
  PRIMARY KEY (ISBN),
  FOREIGN KEY (WarehouseID) REFERENCES Warehouse(WarehouseID)
);

CREATE TABLE Advertisement
(
  URL VARCHAR(200) NOT NULL,
  SocialPlatform VARCHAR(25) NOT NULL,
  ISBN CHAR(13) NOT NULL,
  PRIMARY KEY (URL),
  FOREIGN KEY (ISBN) REFERENCES UsedBook(ISBN)
);

CREATE TABLE Customer
(
  FirstName VARCHAR(50) NOT NULL,
  LastName VARCHAR(50) NOT NULL,
  CustomerEmail VARCHAR(100) NOT NULL,
  CustomerPhone VARCHAR(50) NOT NULL,
  CustomerAddress VARCHAR(200) NOT NULL,
  CustomerID CHAR(3) NOT NULL,
  PRIMARY KEY (CustomerID)
);

CREATE TABLE Employee
(
  SSN VARCHAR(25) NOT NULL,
  FirstName VARCHAR(50) NOT NULL,
  LastName VARCHAR(50) NOT NULL,
  PRIMARY KEY (SSN)
);

CREATE TABLE Category
(
  CategoryName VARCHAR(50) NOT NULL,
  CategoryID CHAR(4) NOT NULL,
  PRIMARY KEY (CategoryID)
);

CREATE TABLE IsProvidedBy
(
  ISBN CHAR(13) NOT NULL,
  CustomerID CHAR(3) NOT NULL,
  PRIMARY KEY (ISBN, CustomerID),
  FOREIGN KEY (ISBN) REFERENCES UsedBook(ISBN),
  FOREIGN KEY (CustomerID) REFERENCES Customer(CustomerID)
);

CREATE TABLE IsSentTo
(
  URL VARCHAR(200) NOT NULL,
  CustomerID CHAR(3) NOT NULL,
  PRIMARY KEY (URL, CustomerID),
  FOREIGN KEY (URL) REFERENCES Advertisement(URL),
  FOREIGN KEY (CustomerID) REFERENCES Customer(CustomerID)
);

CREATE TABLE BelongsTo
(
  ISBN CHAR(13) NOT NULL,
  CategoryID CHAR(4) NOT NULL,
  PRIMARY KEY (ISBN, CategoryID),
  FOREIGN KEY (ISBN) REFERENCES UsedBook(ISBN),
  FOREIGN KEY (CategoryID) REFERENCES Category(CategoryID)
);

CREATE TABLE CheckoutCart
(
  ReceiptID CHAR(3) NOT NULL,
  Date DATE NOT NULL,
  CustomerID CHAR(3) NOT NULL,
  SSN VARCHAR(25) NOT NULL,
  PRIMARY KEY (ReceiptID),
  FOREIGN KEY (CustomerID) REFERENCES Customer(CustomerID),
  FOREIGN KEY (SSN) REFERENCES Employee(SSN)
);

CREATE TABLE IsBought
(
  ISBN CHAR(13) NOT NULL,
  ReceiptID CHAR(3) NOT NULL,
  PRIMARY KEY (ISBN, ReceiptID),
  FOREIGN KEY (ISBN) REFERENCES UsedBook(ISBN),
  FOREIGN KEY (ReceiptID) REFERENCES CheckoutCart(ReceiptID)
);

--- Inserting data

INSERT INTO category  VALUES ('Psychology','9990');
INSERT INTO category  VALUES ('Horror','9991');
INSERT INTO category  VALUES ('Mystery','9992');
INSERT INTO category  VALUES ('Crime','9993');
INSERT INTO category  VALUES ('Detective','9994');
INSERT INTO category  VALUES ('Romance','9995');
INSERT INTO category  VALUES ('Fantasy','9996');

INSERT INTO warehouse VALUES ('78238','23121 Strawberry Ave, San Antonio','001');
INSERT INTO warehouse VALUES ('78211','23811 Blueberry Ave, San Antonio','002');
INSERT INTO warehouse VALUES ('78246','89849 Orange Drive, San Antonio','003');
INSERT INTO warehouse VALUES ('78355','84783 Apple Ave, San Antonio','004');
INSERT INTO warehouse VALUES ('78210','23842 Pineapple Drive, San Antonio','005');
INSERT INTO warehouse VALUES ('78292','38947 Purple Road, San Antonio','006');
INSERT INTO warehouse VALUES ('78124','23232 Green Ave, San Antonio','007');

INSERT INTO usedbook VALUES ('The Shining','9780450040184','1982','Stephen King','English','Hodder Paperbacks','Like New','12.67','30','2.50','001');
INSERT INTO usedbook VALUES ('The Mystery of the Blue Train','9780425210789','2006','Agatha Christie','English','Penguin Publishing Group','Very Good','7.99','25','3.50','003');
INSERT INTO usedbook VALUES ('A Mercy','9780307472342','2009','Toni Morrison','English','Vintage Books USA','Excellent Condition','10.69','11','3.50','005');
INSERT INTO usedbook VALUES ('Birds of Britain','9780691199795','1967','John D. Green','English','The Bodley Head','Acceptable','50.00','1','25.00','001');
INSERT INTO usedbook VALUES ('Coloriages Mystères Disney Portraits','9782019452049','2020','Christophe-Alexis Perez','French','Hachette Heroes','Like New','25.55','16','12.50','002');
INSERT INTO usedbook VALUES ('The Handmaid Tale','9781328879943','2017','Margaret Atwood','English','Houghton Mifflin Harcourt','Very Good','10.99','55','2.50','002');
INSERT INTO usedbook VALUES ('Yogisha Ekkusu No Kenshin','9784167110123','2008', 'Keigo Higashino','Japanese','Tsai Fong Books','Like New','8.99','2','1.50','004');
INSERT INTO usedbook VALUES ('Roundelay of Strong Wind','9787514306538','2014','Keigo Higashino','Chinese','Modern Press','Excellent Condition','4.14','24','1.00','003');
INSERT INTO usedbook VALUES ('Kindred','9780807083697','2003','Octavia E. Butler','English','Beacon Press','Acceptable','10.60','8','2.50','007');
INSERT INTO usedbook VALUES ('The Farming of Bones','9780140280494','1999','Edwidge Danticat','English','Penguin Books','Very Good','6.79','3','2.00','006');

INSERT INTO customer VALUES ('Phoebe','Buffay','pbuffay@gmail.com','(575) 838-1858','1 Trinity Pl #1232, San Antonio, TX 78212','123');
INSERT INTO customer VALUES ('Rachel','Green','rgreen@gmail.com','(210) 257-9215','2132 San Pedro Ave, San Antonio, TX 72831','124');
INSERT INTO customer VALUES ('Monica','Lewis','mlewis@gmail.com','(210) 681-2840','77 Pierce Street Bryan, San Antonio, TX 77803','125');
INSERT INTO customer VALUES ('Ross','Geller','rgeller@gmail.com','(210) 437-3943','8810 Old Sky Hbr, San Antonio, TX 78242','126');
INSERT INTO customer VALUES ('Chandler','Bing','chanbing@gmail.com','(210) 599-2098','8100 Pinebrook Dr, San Antonio, TX 78230','127');
INSERT INTO customer VALUES ('Joey','Tribbiani','jtribbiani@gmail.com','(432) 398-7745','6725 Walzem Rd, San Antonio, TX 78239','128');
INSERT INTO customer VALUES ('Ted','Mosby','tmosby@gmail.com','(281) 884-8358','6623 Suncliff Crst, San Antonio, TX 78238','129');
INSERT INTO customer VALUES ('Barney','Stinson','bstinson@gmail.com','(432) 664-5739','2938 Mulberry Ave, San Antonio, TX 72321','130');
INSERT INTO customer VALUES ('Robin','Smulders','rsmulders@gmail.com','(682) 438-1124','9914 W Military Dr, San Antonio, TX 78242','131');
INSERT INTO customer VALUES ('Lily','Adrin','ladrin@gmail.com','(713) 518-3045','3212 Mulberry Ave, San Antonio, TX 72321','132');
INSERT INTO customer VALUES ('Victoria','Lee','viclee@gmail.com','(210) 802-2872','4222 Mulberry Ave, San Antonio, TX 72321','133');
INSERT INTO customer VALUES ('Lucas','Graham','lgraham@gmail.com','(210) 838-2312','1211 Brown Ave, San Antonio, TX 72812','134');
INSERT INTO customer VALUES ('Pipe','Mila','pmila@gmail.com','(232) 821-3821','1211 Brown Ave, San Antonio, TX 72812','135');
INSERT INTO customer VALUES ('Wen','Kim','wkim@gmail.com','(228) 892-3921','2378 Baby Ave, San Antonio, TX 72381','136');

INSERT INTO employee VALUES ('531-58-7090','Eleanor','Shellstrop');
INSERT INTO employee VALUES ('041-36-6964','Emily','Bach');
INSERT INTO employee VALUES ('212-52-2444','John','Brown');
INSERT INTO employee VALUES ('522-59-8036','Lacey','Whitney');
INSERT INTO employee VALUES ('518-12-5731','Tahani','Max');

INSERT INTO belongsto VALUES ('9780450040184','9990');
INSERT INTO belongsto VALUES ('9780425210789','9990');
INSERT INTO belongsto VALUES ('9787514306538','9990');
INSERT INTO belongsto VALUES ('9780425210789','9991');
INSERT INTO belongsto VALUES ('9780307472342','9992');
INSERT INTO belongsto VALUES ('9780691199795','9993');
INSERT INTO belongsto VALUES ('9782019452049','9994');
INSERT INTO belongsto VALUES ('9781328879943','9994');
INSERT INTO belongsto VALUES ('9784167110123','9995');
INSERT INTO belongsto VALUES ('9787514306538','9996');

INSERT INTO isprovidedby VALUES ('9780450040184','123');
INSERT INTO isprovidedby VALUES ('9780450040184','124');
INSERT INTO isprovidedby VALUES ('9780450040184','125');
INSERT INTO isprovidedby VALUES ('9780425210789','126');
INSERT INTO isprovidedby VALUES ('9780425210789','127');
INSERT INTO isprovidedby VALUES ('9780691199795','129');
INSERT INTO isprovidedby VALUES ('9782019452049','130');
INSERT INTO isprovidedby VALUES ('9781328879943','132');
INSERT INTO isprovidedby VALUES ('9784167110123','132');
INSERT INTO isprovidedby VALUES ('9787514306538','133');

INSERT INTO advertisement VALUES ('https://msng.link/o/?theshining=ig','Instagram','9780450040184');
INSERT INTO advertisement VALUES ('https://msng.link/o/?theshining=fb','Facebook','9780450040184');
INSERT INTO advertisement VALUES ('https://msng.link/o/?agathachristie=ig','Instagram','9780425210789');
INSERT INTO advertisement VALUES ('https://msng.link/o/?agathachristie=fb','Facebook','9780425210789');
INSERT INTO advertisement VALUES ('https://msng.link/o/?amercytonimorrison=ig','Instagram','9780307472342');
INSERT INTO advertisement VALUES ('https://msng.link/o/?amercytonimorrison=fb','Facebook','9780307472342');
INSERT INTO advertisement VALUES ('https://msng.link/o/?birdsofbritain=ig','Instagram','9780691199795');


INSERT INTO issentto VALUES ('https://msng.link/o/?theshining=ig','123');
INSERT INTO issentto VALUES ('https://msng.link/o/?theshining=ig','124');
INSERT INTO issentto VALUES ('https://msng.link/o/?agathachristie=ig','123');
INSERT INTO issentto VALUES ('https://msng.link/o/?agathachristie=ig','126');
INSERT INTO issentto VALUES ('https://msng.link/o/?agathachristie=ig','127');
INSERT INTO issentto VALUES ('https://msng.link/o/?agathachristie=ig','128');
INSERT INTO issentto VALUES ('https://msng.link/o/?amercytonimorrison=fb','123');
INSERT INTO issentto VALUES ('https://msng.link/o/?amercytonimorrison=fb','124');
INSERT INTO issentto VALUES ('https://msng.link/o/?birdsofbritain=ig','125');
INSERT INTO issentto VALUES ('https://msng.link/o/?birdsofbritain=ig','123');


INSERT INTO checkoutcart VALUES ('454','9-16-2022','128','531-58-7090');
INSERT INTO checkoutcart VALUES ('455','9-16-2022','131','041-36-6964');
INSERT INTO checkoutcart VALUES ('456','9-16-2022','134','212-52-2444');
INSERT INTO checkoutcart VALUES ('457','9-16-2022','135','522-59-8036');
INSERT INTO checkoutcart VALUES ('458','9-17-2022','136','518-12-5731');



INSERT INTO isbought VALUES ('9780450040184','454');
INSERT INTO isbought VALUES ('9780450040184','455');
INSERT INTO isbought VALUES ('9781328879943','455');
INSERT INTO isbought VALUES ('9787514306538','456');
INSERT INTO isbought VALUES ('9787514306538','457');
INSERT INTO isbought VALUES ('9780140280494','457');
INSERT INTO isbought VALUES ('9784167110123','458');
INSERT INTO isbought VALUES ('9780807083697','458');

--- A listing of the different book categories
--- and the corresponding revenue generated
--- for each category

with temp_1 as (
select u.isbn, (u.sellingprice - u.purchasingprice)*count(*) as revenue
from usedbook u, isbought i
where u.isbn = i.isbn
group by u.isbn),
temp_2 as (
select revenue, b.categoryid, c.categoryname
from belongsto b inner join temp_1 using (isbn) inner join category c using (categoryid))
select categoryname, sum(revenue) as revenue_based_on_category
from temp_2
group by categoryname;


--- A listing of top ten book names in terms of units sold. 

select * from (
with temp_top as (
select u.title, count(*) as unit_sold
from usedbook u, isbought i
where u.isbn = i.isbn
group by u.title)
select title, unit_sold,
rank() over (order by unit_sold desc) as ranking
from temp_top) as A
where ranking <= 10;


--- A listing of each customer name,
--- a count of the advertisements they have received,
--- a count of books sold and a count of books purchased.  

select c.firstname || ' ' || c.lastname as customer_name, count(i.receiptid) as num_books_purchased, count(i2.isbn) as num_books_sold, count(i3.url) as num_advertisements
from customer c
left join checkoutcart c2 using (customerid)
left join isbought i using (receiptid)
left join isprovidedby i2 using (customerid)
left join issentto i3 using (customerid)
group by c.customerid
order by c.customerid;
