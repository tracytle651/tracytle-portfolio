# Bookstore Operational Database Design and Analysis

This project was completed for the BAT-3306 Databases course and showcases both database design and practical business queries. All sample data used in this project is anonymized and synthetically generated for academic purposes.


The bookstore — a secondhand retailer offering a wide range of used and rare books — requires an operational database to support its inventory management, promotional strategies, sales tracking, and customer relationship efforts. 

This project involved:
- Designing a normalized relational schema from scratch
- Defining entity relationships and constraints
- Populating the database with realistic sample data
- Writing SQL queries to extract operational insights


## Objectives

- Model core business functions such as book sales, customer activity, promotions, inventory, and employee management
- Demonstrate how SQL can answer business questions around profit, customer engagement, and sales trends
- Use entity-relationship modeling to define a robust schema aligned with business requirements


## Database Features

The schema supports:
- **Book Inventory**: Tracks titles, conditions, pricing, languages, and storage across multiple warehouses
- **Customer Records**: Maintains purchase and contact information
- **Employee Transactions**: Tracks which employees process checkouts
- **Marketing & Promotions**: Stores advertisement URLs, platforms, and targeted recipients
- **Sales Tracking**: Captures cart checkouts, receipts, and units sold per book
- **Supplier Activity**: Captures which customers sold used books to the store


## Research Questions

1. What is the total revenue generated from each book category?
2. What are the top 10 best-selling books by units sold?
3. For each customer:
   - How many books did they purchase?
   - How many books did they sell to the store?
   - How many promotional advertisements did they receive?


> All research questions are answered using SQL queries written for clarity and efficiency.


## Deliverables

| File | Description |
|------|-------------|
| `bookstore_analysis.sql` | SQL script to create tables, insert data, and run business queries |
| `bookstore_report.pdf` | Full project documentation with schema design and sample data |



[Back to portfolio homepage](../README.md)
