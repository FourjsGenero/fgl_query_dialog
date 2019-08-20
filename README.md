# Genero SQL query dialog

## Description

This Genero BDL demo implements a generic SQL query dialog that builds the
FROM and WHERE part of a SELECT statement.

Callers can configure the query dialog with DB table and column description,
providing the database object names, the SQL types and a display label.

![Genero SQL query dialog (GDC)](https://github.com/FourjsGenero/fgl_query_dialog/raw/master/docs/screen-001.png)

## Prerequisites

* Genero BDL 3.20+
* Genero Browser Client 1.00.52+
* Genero Desktop Client 3.20+
* Genero Studio 3.20+
* GNU Make

## Compilation from command line

1. make clean all

## Compilation in Genero Studio

1. Load the query_dialog.4pw project
2. Build the project

## Usage

1. Start the program
2. Click on Query action to open the query dialog
3. Enter search criterion
5. Try the preview to see if the SQL runs
4. Validate to run the SELECT statement in the caller

## Programmer's reference

### APIs

* fglquerydlg_init() : Initializes the library module
* fglquerydlg_fini() : Terminates the library module
* fglquerydlg_new(name) : Creates a new query set
* fglquerydlg_free(qx) : Frees the query set x
* fglquerydlg_set_select_list(qx, sl) : Defines the SELECT list for preview
* fglquerydlg_add_table(qx, tn, dn) : Add a DB table, returns the table id
* fglquerydlg_add_column(qx, tx, cn, ct, it, lx, dn) : Add a DB column for a table, returns the table id
* fglquerydlg_set_relation(qx, tx, rtn, sql) : Add a foreign key relation for a table.
* fglquerydlg_add_pick_list(qx, nm) : Creates a pick list
* fglquerydlg_add_pick_list_item(qx, lx, vk, vl) : Add an element to a pick list
* fglquerydlg_execute(qx,new) : Open the query dialog and wait for user input
* fglquerydlg_set_date_format(fmt) : Define the DATE format for date literals
* fglquerydlg_get_from_part(qx) : Returns the FROM part
* fglquerydlg_get_where_part(qx) : Returns the WHERE part


## Bug fixes:

