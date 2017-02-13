IMPORT FGL fglquerydlg

TYPE t_cust RECORD
               cust_id   INTEGER,
               cust_name VARCHAR(50),
               cust_addr VARCHAR(100),
               cust_city INTEGER,
               cust_lord DATE
           END RECORD
DEFINE cust_arr DYNAMIC ARRAY OF t_cust

MAIN
    DEFINE qx SMALLINT

    DATABASE ":memory:+driver='dbmsqt'"
    CALL create_db_tables()

    OPEN FORM f1 FROM "query_dialog_test"
    DISPLAY FORM f1

    CALL fglquerydlg_init()

    CASE fgl_db_driver_type()
      WHEN "sqt"
        CALL fglquerydlg_set_date_format("DATE('%1-%2-%3')") -- SQLite
      OTHERWISE
        CALL fglquerydlg_set_date_format("MDY(%2,%3,%1)") -- Informix
    END CASE

    LET qx = query_create()

    CALL fill_cust_arr("customer", "1=1", NULL)
    DISPLAY ARRAY cust_arr TO sr.* ATTRIBUTES(UNBUFFERED)
        ON ACTION query ATTRIBUTES(TEXT="Query")
           CALL query_input_and_execute(qx)
    END DISPLAY

    CALL fglquerydlg_free(qx)

    CALL fglquerydlg_fini()

END MAIN

FUNCTION query_create()
    DEFINE qx, tx, lx1, r SMALLINT
    DEFINE r1 RECORD
                pk INTEGER,
                lb VARCHAR(50)
           END RECORD

    LET qx = fglquerydlg_new("query_1")

    CALL fglquerydlg_set_select_list(qx,"customer.cust_id, customer.cust_name")

    LET lx1 = fglquerydlg_add_pick_list(qx, "city" )
    DECLARE c11 CURSOR FOR SELECT * FROM city ORDER BY city_name
    FOREACH c11 INTO r1.*
        CALL fglquerydlg_add_pick_list_item(qx, lx1, r1.pk, r1.lb )
    END FOREACH

    LET tx = fglquerydlg_add_table(qx, "customer", "Customer")
    CALL fglquerydlg_add_column(qx,tx, "cust_id",   "INTEGER",      FGLQD_IT_SIMPLE,    NULL, "ID")
    CALL fglquerydlg_add_column(qx,tx, "cust_name", "VARCHAR(50)",  FGLQD_IT_SIMPLE,    NULL, "Name")
    CALL fglquerydlg_add_column(qx,tx, "cust_addr", "VARCHAR(100)", FGLQD_IT_SIMPLE,    NULL, "Address")
    CALL fglquerydlg_add_column(qx,tx, "cust_city", "INTEGER",      FGLQD_IT_PICK_LIST, lx1,  "City")
    CALL fglquerydlg_add_column(qx,tx, "cust_lord", "DATE",         FGLQD_IT_FROM_TYPE, NULL, "Last order")

    LET tx = fglquerydlg_add_table(qx, "city", "City")
    CALL fglquerydlg_add_column(qx,tx, "city_name", "VARCHAR(50)",  FGLQD_IT_SIMPLE,    NULL, "Name")
    CALL fglquerydlg_set_relation(qx,tx,"customer","customer.cust_city=city.city_id")

    LET tx = fglquerydlg_add_table(qx, "orders", "Order")
    CALL fglquerydlg_add_column(qx,tx, "ord_id",    "INTEGER", FGLQD_IT_SIMPLE,    NULL, "ID")
    CALL fglquerydlg_add_column(qx,tx, "ord_date",  "DATE",    FGLQD_IT_FROM_TYPE, NULL, "Ship Date")
    CALL fglquerydlg_add_column(qx,tx, "ord_total", "DECIMAL", FGLQD_IT_SIMPLE,    NULL, "Total")
    CALL fglquerydlg_set_relation(qx,tx,"customer","orders.ord_cust=customer.cust_id")

    RETURN qx

END FUNCTION

FUNCTION query_input_and_execute(qx)
    DEFINE qx SMALLINT
    DEFINE r SMALLINT
    LET r = fglquerydlg_execute(qx,FALSE)
    IF r==0 THEN
       CALL fill_cust_arr( fglquerydlg_get_from_part(qx),
                           fglquerydlg_get_where_part(qx),
                           NULL
                         )
    END IF
END FUNCTION

FUNCTION create_db_tables()
    DEFINE x SMALLINT,
           c t_cust

    WHENEVER ERROR CONTINUE
    DROP TABLE city
    DROP TABLE orders
    DROP TABLE customer
    WHENEVER ERROR STOP

    CREATE TABLE city (
                   city_id INTEGER NOT NULL PRIMARY KEY,
                   city_name VARCHAR(50)
           )
    INSERT INTO city VALUES ( 1, "Paris" )
    INSERT INTO city VALUES ( 2, "London" )
    INSERT INTO city VALUES ( 3, "Berlin" )
    INSERT INTO city VALUES ( 4, "Madrid" )
    INSERT INTO city VALUES ( 5, "Rome" )
    INSERT INTO city VALUES ( 6, "Budapest" )
    INSERT INTO city VALUES ( 7, "Vien" )
    INSERT INTO city VALUES ( 8, "Dublin" )
    INSERT INTO city VALUES ( 9, "Amsterdam" )
    INSERT INTO city VALUES (10, "Berne" )

    CREATE TABLE customer (
                   cust_id INTEGER NOT NULL PRIMARY KEY,
                   cust_name VARCHAR(50) NOT NULL,
                   cust_addr VARCHAR(100),
                   cust_city INTEGER,
                   cust_lord DATE,
                   FOREIGN KEY (cust_city) REFERENCES city(city_id)
                   
           )

    LET c.cust_id = 1
    LET c.cust_name = "Mark Stanberg"
    LET c.cust_addr = "5 River Place"
    LET c.cust_city = 1
    LET c.cust_lord = MDY(12,2,2016)
    INSERT INTO customer VALUES ( c.* )

    LET c.cust_id = 2
    LET c.cust_name = "Paul Kliwer"
    LET c.cust_addr = "10 Market St."
    LET c.cust_city = 2
    LET c.cust_lord = MDY(23,12,2014)
    INSERT INTO customer VALUES ( c.* )
    
    LET c.cust_id = 3
    LET c.cust_name = "Philip Butter"
    LET c.cust_addr = "3 Cristal St."
    LET c.cust_city = 3
    LET c.cust_lord = MDY(04,15,2015)
    INSERT INTO customer VALUES ( c.* )

    FOR x=100 TO 200
        LET c.cust_id = x
        LET c.cust_name = SFMT("Customer #%1", x)
        LET c.cust_addr = NULL
        LET c.cust_city = 1
        LET c.cust_lord = TODAY + x
        INSERT INTO customer VALUES ( c.* )
    END FOR

    CREATE TABLE orders (
                   ord_id INTEGER NOT NULL PRIMARY KEY,
                   ord_cust INTEGER NOT NULL,
                   ord_date DATE NOT NULL,
                   ord_total DECIMAL(10,2),
                   FOREIGN KEY (ord_cust) REFERENCES customer(cust_id)
           )

    INSERT INTO orders VALUES ( 1001, 1, TODAY, 2000.00 )
    INSERT INTO orders VALUES ( 1002, 2, TODAY,  451.50 )
    INSERT INTO orders VALUES ( 1004, 3, TODAY,  222.50 )

END FUNCTION

FUNCTION fill_cust_arr(from_part, where_part, order_by_part)
    DEFINE from_part, where_part, order_by_part STRING
    DEFINE x SMALLINT,
           sql STRING
    LET sql = "SELECT * FROM ", from_part, " WHERE ", where_part
    IF order_by_part IS NOT NULL THEN
       LET sql = sql, " ORDER BY ", order_by_part
    END IF
display sql
    DECLARE c12 CURSOR FROM sql
    CALL cust_arr.clear()
    LET x = 1
    FOREACH c12 INTO cust_arr[x].*
        LET x = x+1
    END FOREACH
    CALL cust_arr.deleteElement(x)
END FUNCTION

