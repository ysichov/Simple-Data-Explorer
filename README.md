# Simple Data Explorer (SDE)

Multi-window explorer for SAP tables, views and CDS views — with per-window select-options that
re-read the data as you type, drag-and-drop links between windows, foreign-key navigation, a join
builder, pivot tables and a generated Open SQL statement you can edit and run.

Version 2 (**SelecTor**) lives in [`src/`](src) and is the version documented here. The older
single-file viewer is still shipped for old releases — see
[History and older documentation](#history-and-older-documentation).

<img width="595" alt="SelecTor selection screen" src="https://github.com/user-attachments/assets/f038ec54-a24e-40e6-8564-e015ef91d2fb" />

---

## Table of contents

- [Overview](#overview)
- [Requirements](#requirements)
- [Installation](#installation)
  - [abapGit](#abapgit)
  - [Standalone single-file program](#standalone-single-file-program)
  - [Legacy one-file versions](#legacy-one-file-versions)
- [Quick start](#quick-start)
- [Selection screen](#selection-screen)
- [The table window](#the-table-window)
  - [Toolbar](#toolbar)
  - [Select-options panel](#select-options-panel)
  - [Filter shortcuts](#filter-shortcuts)
  - [Empty columns](#empty-columns)
  - [Languages and technical names](#languages-and-technical-names)
  - [Sorting](#sorting)
- [Links between windows](#links-between-windows)
  - [Double-click navigation](#double-click-navigation)
  - [Drag and drop](#drag-and-drop)
  - [Table link graph](#table-link-graph)
- [HR and HCM plugins](#hr-and-hcm-plugins)
  - [Payroll clusters](#payroll-clusters)
- [Tools: joins, pivot tables, SQL](#tools-joins-pivot-tables-sql)
  - [Join builder](#join-builder)
  - [Pivot tables](#pivot-tables)
  - [Generated SQL](#generated-sql)
  - [Saving and loading layouts](#saving-and-loading-layouts)
- [Repository layout](#repository-layout)
- [Architecture](#architecture)
- [Building the standalone program](#building-the-standalone-program)
- [History and older documentation](#history-and-older-documentation)
- [Author](#author)
- [License](#license)

---

## Overview

- **Multiple windows.** Open as many tables as your screen allows, including the same table several
  times. Every window is an independent dialog box with its own data and filters.
- **A selection panel per window.** It opens and closes with the arrow icon. All standard search
  helps are connected, including the special ones for personnel numbers (`PAxxxx-PERNR`) and
  organizational management objects (`HRPxxxx`).
- **Reactivity.** Data is re-read as soon as a filter changes — no round-trip through a separate
  selection screen, no loss of context.
- **Empty columns are hidden** by default, because most SAP tables are mostly empty. One menu entry
  brings them back.
- **Multilingualism.** All languages installed in the system are supported: column and table names
  are translated on the fly, and technical field names are one of the options.
- **Navigation.** Double-click a value to open the checked table behind the foreign key; drag values
  from one window into the filters of another; look at the link graph as in SE11.
- **HR/HCM plugins.** Jumps to PA20/PP01, infotype long texts, wage type texts, organizational
  management links, payroll cluster viewer.
- **Tools.** Build joins from the foreign keys the dictionary already knows, build pivot tables,
  read the generated Open SQL, edit it, run it, and save the whole layout to a file.
- **Tables, views and CDS.** Transparent and cluster tables, DDIC views (including the maintenance
  views of SM30) and the SQL views generated for CDS entities.

![Multiple windows](https://ysychov.files.wordpress.com/2020/03/first-2.png)

## Requirements

- **SAP GUI for Windows.** The whole UI is built on `CL_GUI_*` controls (ALV grid, HTML viewer,
  splitter, dialog boxes); layouts are saved through frontend services.
- **SAP_BASIS 7.50 or higher** for the sources in `src/`. For older systems use the
  [legacy one-file versions](#legacy-one-file-versions).
- **Display rights on the data you open.** SDE reads with plain Open SQL and performs no
  `S_TABU_*` authority checks of its own — treat it as a developer/support tool and install it
  accordingly.

## Installation

### abapGit

1. In abapGit, create an online repository for `https://github.com/ysichov/Simple-Data-Explorer.git`
   and assign it to a package (for example `$ZSDE` for a local one).
2. Pull, then activate all objects.
3. Run program **`ZSDE2`** (SE38 / SA38).

### Standalone single-file program

No package, no abapGit, nothing to activate except one report:

1. Create a report (for example `Z_SDE_STANDALONE`) in SE38.
2. Paste the contents of [`src/z_sde_standalone.prog.abap`](src/z_sde_standalone.prog.abap).
3. Activate and run.

The file is generated from `src/` — see
[Building the standalone program](#building-the-standalone-program). Do not edit it by hand.

### Legacy one-file versions

Generation 1 of the viewer, kept for old systems and for the HR flavour:

| File | Target | Notes |
| --- | --- | --- |
| [`SDE_702.abap`](SDE_702.abap) | SAP_BASIS 7.02 | Report `YS_SDE` |
| [`SDE_750_HR.abap`](SDE_750_HR.abap) | SAP_BASIS 7.50 | HR/HCM build, infotype texts (`PAxxxx-ITXEX`) |

## Quick start

1. Run `ZSDE2`.
2. Pick the object type with the buttons in the toolbar of the selection screen: **Tables**,
   **Views**, **CDS**.
3. Type a name and press **Enter** — a window opens. Type the next name and press Enter again to
   open another window; the selection screen stays where it is.
4. Filter in the left panel of a window; the grid follows every change.
5. Press the **Tools** icon in a window to build joins and pivot tables on top of it.

## Selection screen

| Field | Meaning |
| --- | --- |
| **Tables / Views / CDS** | Switches the input field. Tables accepts transparent and cluster tables, Views any DDIC view (F4 lists the maintenance views of SM30), CDS the SQL view generated for a CDS entity (application class `SDGV`). |
| **Table / View / CDS name** | F4 is available in all three modes. The name is checked before the window opens. |
| **Max rows** | Row limit of every read, `500` by default. It also caps the statements the Tools area runs. |
| **Folder for saved joins** | Frontend folder for `.sdj` layout files, `C:\temp\sde\` by default. F4 opens a directory browser. |

## The table window

A window is a dialog box split into the **selection panel** (left, collapsible) and the **data grid**
(right). When the Tools area is opened, it is docked below the data.

### Toolbar

| Button | Description |
| --- | --- |
| Show/hide select-options | Collapses the selection panel and gives the width to the grid. |
| Refresh | Re-reads the data with the current filters. |
| Load join layout | Reads a `.sdj` file; opens the Tools area if needed. |
| Save join layout | Writes the current tables, fields, mode, filters and SQL to a `.sdj` file. |
| Languages | Menu: **Technical name** plus every language installed in the system. |
| Empty columns options | Menu: **Hide empty columns** / **Show empty columns**. |
| Table links | Opens the graphical link visualization. |
| Tools | Docks the join and pivot builder below the data (click again to hide). |
| Sort ascending / descending | Builds `ORDER BY` on the marked columns — see [Sorting](#sorting). |
| Standard ALV functions | Shows or hides the standard ALV toolbar (export, layouts, find, …). |

### Select-options panel

![Selection panel](https://ysychov.files.wordpress.com/2020/03/arrows.png)

One row per field, with the dictionary information next to it — data element, domain, data type and
length. This is meant to save time while writing technical specifications.

![Dictionary information](https://ysychov.files.wordpress.com/2020/03/dictionary.png)

- **F4** on a row opens the standard search help of the field. Values already entered in other rows
  are passed to the help, so the proposals stay in the context of the current filter.
- **Double-click** opens the dictionary: on the `ELEMENT` column SE11 with the data element, on the
  `DOMAIN` column SE11 with the domain, on any other column the documentation of the data element.
- The **option icon** opens the standard sign/option dialog (`EQ`, `BT`, `NE`, `CP`, …).
- The **more icon** opens the complex selections dialog — an arbitrary number of ranges per field,
  exactly as on a normal selection screen. The icon shows whether ranges exist.
- Fields whose search help is a short value list get a **drop-down** in the input cell.
- The **context menu** offers *Clear Select-Options* and *Delete receiver* (see
  [Drag and drop](#drag-and-drop)).

![Search help](https://ysychov.files.wordpress.com/2020/03/help.png)

### Filter shortcuts

Typing is interpreted the way an experienced user expects:

| Input | Result |
| --- | --- |
| a value in **low** | `I EQ value` |
| values in **low** and **high** | `I BT low high` |
| a value containing `*`, `%`, `+` or `&` | `I CP pattern` |
| clearing sign and option | the row is cleared |
| a date or time | converted from the user format into the internal one |

### Empty columns

Empty columns are hidden after every read, so a 200-column table shows only what actually carries
data. **Show empty columns** brings them all back.

![Same table with and without empty columns](https://ysychov.files.wordpress.com/2020/03/empty.png)

### Languages and technical names

The **Languages** menu lists every language installed in the system plus **Technical name**. The
choice re-translates the grid headers *and* the labels of the selection panel.

![The same table in three languages and with technical names](https://ysychov.files.wordpress.com/2020/03/lang.png)

### Sorting

The sort buttons do **not** sort the fetched rows on the frontend — that would sort the wrong 500
rows. They build an `ORDER BY` and read again from the database. Mark one or more columns (or just
place the cursor in one) and press the sort icon. Because the statement is built by the Tools area,
sorting requires **Tools** to be open; the program says so if it is not.

## Links between windows

### Double-click navigation

The colour of a column tells what a double-click will do:

| Colour | Meaning |
| --- | --- |
| **Blue** | The field has a check table: it opens in a new window, filtered by the value you clicked. |
| **Green** | A [plugin](#hr-and-hcm-plugins) handles the field — a transaction, or a related table that no foreign key describes. |
| **Grey** | A column of the text table, joined to the data automatically so the language-dependent text stands next to the code. |
| White | A plain field. |

![Blue and green links](https://ysychov.files.wordpress.com/2020/03/links.png)

### Drag and drop

Select one or more cells or a whole column in a data grid and drag them onto the selection panel of
another window. The dragged column turns into **buttons** in the source window, an **inheritance
indicator** appears next to the receiving field, and clicking a button in the source re-reads the
target window with that value. Dragging between two selection panels works the same way. A data
model of several tables comes alive in about a minute.

![Drag and drop between windows](https://ysychov.files.wordpress.com/2020/03/dynamic.png)

*Delete receiver* in the context menu of the selection panel breaks the link again.

### Table link graph

The **Table links** button opens the graphical visualization of the foreign key relations of the
current table, the same picture SE11 draws.

![Table link graph](https://ysychov.files.wordpress.com/2020/03/graph.png)

## HR and HCM plugins

Links that no foreign key describes are hard-coded as plugins.

By data element:

| Data element | Action |
| --- | --- |
| `PERSNO` | PA20 for the personnel number, with infotype, subtype and validity dates prefilled |
| `HROBJID` | PP01 for the OM object |
| `LGART` | Wage type text table `T512W` |
| `ITXEX` | Long text of the infotype record, in a text viewer window |
| `SUBTY` | Subtype table of the infotype |

By table and field:

| From | To |
| --- | --- |
| `PA0001-PLANS` / `ORGEH` / `STELL` | `HRP1000`, object type `S` / `O` / `C` |
| `HRP1001-SCLAS` / `SOBID` | The related OM object — it is put into the search parameters of the same window, so you can walk the structure link by link |
| `HRP1001-ADATANR` | The additional data table of the relation |
| `HRP1002` / `HRP1035` / `HRP1222`, field `TABNR` | `HRT1002` / `HRT1035` / `HRT1222` |
| `PA2006-QUONR` | `PTQUODED`, and from there back to `PA2006` / `PA2001` |
| `HRPY_RGDIR-SEQNR` | [Payroll cluster viewer](#payroll-clusters) |

![Organizational management links](https://ysychov.files.wordpress.com/2020/03/bu.png)
![HRP1002 and HRT1002](https://ysychov.files.wordpress.com/2020/03/hrp1002.png)

### Payroll clusters

A double-click on a sequence number in `HRPY_RGDIR` opens the payroll result of that personnel
number and sequence number as a **tree**: every cluster table of the result is a node, and each node
opens in a normal SDE window with filters, empty-column handling and everything else.

## Tools: joins, pivot tables, SQL

The **Tools** button docks a builder below the data of the window: an HTML panel on top (tables and
zones), the field list, the generated SQL, and the result grid.

<img width="1800" alt="Tools area" src="https://github.com/user-attachments/assets/fa269282-022a-4f01-b3cf-d7cb1088ba19" />

Two cards switch the mode — **Join** and **Pivot table**. Clicking the active card again turns the
mode off and leaves the plain table behind.

### Join builder

<img width="1624" alt="Join example" src="https://github.com/user-attachments/assets/2434f1d5-11f1-4bd3-915a-69a7f1eb614c" />

- **Candidates** are discovered from the dictionary around the base table: outgoing foreign keys,
  incoming ones and text tables. Any other table can be added by name.
- Selecting a candidate gives it an **alias** (`T0`, `T1`, `T2`, …) that never changes afterwards,
  so filters and sorts stay valid while you rearrange the join.
- Each joined table has a **join type** (`INNER` / `LEFT OUTER`) and an **ON condition**, filled from
  the key pairs and editable by hand.
- Tables can be reordered; fields can be picked with *select all* / *clear* / *keys*, per table or
  one by one, reordered by dragging or by click-to-move, and selected with a lasso. The field texts
  follow a language of your choice.
- The **selection panel of the window** gains every field of every joined table under its alias
  (`T1_CONNID`), so filters can be set on any table of the join. Filters of tables that leave the
  join are dropped instead of breaking the statement.

### Pivot tables

<img width="1382" alt="Pivot example" src="https://github.com/user-attachments/assets/22b7fd73-2284-49e5-8a78-be66e8e9732f" />

Drop fields into the three zones — **Rows**, **Columns**, **Values**. Every value chip carries an
aggregation: `SUM`, `COUNT`, `MIN`, `MAX`, `AVG` for numeric fields, `COUNT` / `MIN` / `MAX`
otherwise. The whole pivot is executed as **one statement**, with a `CASE` bucket per column value,
so the database does the work. Pivot works on the base table and on a join alike.

### Generated SQL

The statement is always visible below the panel. The **edit** link turns it into a text area: change
anything you like and run it. `UP TO n ROWS`, `ORDER BY` and `GROUP BY` are recognized and applied
separately, and the row limit of the selection screen still applies when the statement does not
carry one. The result appears in the grid next to the field list.

Filters set in the window feed the `WHERE` clause, so the SQL text and the result follow the
selection panel while you type.

### Saving and loading layouts

**Save join layout** / **Load join layout** in the toolbar write and read `.sdj` files in the folder
from the selection screen (`C:\temp\sde\` by default). A layout is tab-separated plain text —
readable in any editor — and contains the base table, the joined tables with their conditions, the
field list and its order, the mode, the filters, the `ORDER BY` and a manually edited statement if
there is one. Loading a layout restores all of it, even before the selection panel has been opened.

## Repository layout

| Path | Contents |
| --- | --- |
| [`src/`](src) | The program: one report, the global classes, one interface, plus the generated standalone report |
| [`SDE_702.abap`](SDE_702.abap), [`SDE_750_HR.abap`](SDE_750_HR.abap) | Legacy one-file versions (not part of the abapGit package) |
| [`generate_standalone.sh`](generate_standalone.sh), [`generate_standalone.bat`](generate_standalone.bat) | Build of the standalone report |
| [`fix_event_order.py`](fix_event_order.py) | Post-processing of the merged report (see below) |
| [`AGENTS.md`](AGENTS.md) | Rules for automated contributors |

## Architecture

| Object | Responsibility |
| --- | --- |
| `zsde2` (report) | Selection screen, object type switch, entry point |
| `zcl_sde_appl` | Global state: list of open windows, languages, icons, parameters |
| `zcl_sde_table_viewer` | One window — grid, toolbar, data read, navigation, docked tools |
| `zcl_sde_sel_opt` | Selection panel: rows, options, ranges, F4, drop-downs, dictionary jumps |
| `zcl_sde_transmitter`, `zcl_sde_receiver` | Event link that makes one window drive another |
| `zcl_sde_dragdrop`, `zcl_sde_dd_data` | Drag and drop between grids and selection panels |
| `zcl_sde_tools` | Join and pivot builder: HTML UI, SQL generation, result grid, layout files |
| `zcl_sde_pivot`, `zif_sde_pivot_types` | Pivot model and the `CASE`-based statement |
| `zcl_sde_plugins` | Dictionary and HR navigation rules |
| `zcl_sde_py_cluster_viewer` | Payroll result as a tree |
| `zcl_sde_text_viewer` | Infotype long texts |
| `zcl_sde_rtti` | Dynamic types, field catalogs, drop-down lists |
| `zcl_sde_sql` | Dynamic reads and existence checks |
| `zcl_sde_ddic`, `zcl_sde_common` | Dictionary and ALV helpers |
| `zcl_sde_popup` | Base class of every dialog window |
| `z_sde_standalone` (report) | Everything above merged into one file — generated, never edited |

## Building the standalone program

Prerequisites: [abapmerge](https://github.com/larshp/abapMerge) (`npm i -g abapmerge`), bash and
Python 3.

```bash
./generate_standalone.sh
```

On Windows, `generate_standalone.bat` calls the same script through Git bash. The paths at the top
of the script are absolute — adjust `SDE_SRC` / `TARGET_DIR` to your machine before the first run.

The script copies `src/` to a work directory, renames the entry point to `z_sde.prog.abap` (abapmerge
derives the report name from the file name and only injects the classes once it finds a matching
`REPORT` statement), runs abapmerge, and then calls `fix_event_order.py`: abapmerge orders class
definitions by `INHERITING FROM` only, while `METHODS ... FOR EVENT ... OF <class>` also requires the
other class to be defined earlier in a flattened program. Finally the header comment is restored.

`src/z_sde_standalone.prog.abap` is the output of this build. Change the classes under `src/` and
regenerate — never edit the standalone file directly.

## History and older documentation

Generation 1 of SDE was a single report for viewing tables and the links between them; this
repository is its successor and the description above covers both generations. The
[wiki of this repository](../../wiki) documented generation 1 and is superseded by this file.

The repository used to be called `SDE`; links of the form `github.com/ysichov/SDE/...` still
redirect here.

- [Simple Data Explorer](https://ysychov.wordpress.com/2020/02/10/simple-data-explorer/) — feature
  description in Russian

## Author

Yurii Sychov — [e-mail](mailto:ysichov@gmail.com) ·
[blog](https://ysychov.wordpress.com/blog/) ·
[LinkedIn](https://www.linkedin.com/in/ysychov/)

ALV listbox handling is based on [bizhuka/eui](https://github.com/bizhuka/eui).

## License

[MIT](LICENSE)
