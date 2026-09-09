# SDE as an ADT REST resource

Status: **reads real data**. A custom application is registered under `/sap/bc/adt/zsde/` and
serves three resources as JSON: table rows with their field catalogue, the code metrics of an
object, and its version history.

```
GET /sap/bc/adt/zsde/table/T001?rows=5
Content-Type: application/json

{
  "table": "t001",
  "count": 5,
  "fields": [
    { "name": "mandt", "position": 1, "key": true,  "datatype": "CLNT",
      "length": 3, "decimals": 0, "text": "Client" },
    { "name": "bukrs", "position": 2, "key": true,  "datatype": "CHAR",
      "length": 4, "decimals": 0, "text": "Company Code" }
  ],
  "rows": [ { "mandt": "100", "bukrs": "0001" } ]
}
```

Rows and catalogue use the same lowercased DDIC field names, so a client maps
`field.name` straight onto `row[field.name]`.

## Why ADT REST and not a plain ICF service

`CL_ADT_RES_APP_BASE` inherits from `CL_REST_HTTP_HANDLER` and its `get_static_uri_path( )`
returns `/sap/bc/adt`, so the resource attaches to the ICF node that ADT already owns. That
removes three things a standalone ICF service would need: its own SICF node, its own
authentication, and its own CSRF handling. An Eclipse or VS Code plugin reuses the ADT session
it already has.

The cost is the release floor: this requires an ADT backend (SAP_BASIS 7.31 SP04+). On 7.02,
which `SDE_702.abap` still targets, none of this exists and only a bare `IF_HTTP_EXTENSION`
handler is possible.

Verified on: S/4HANA 2023, `S4CORE 108`, `SAP_BASIS 758`.

## Objects

| Object | Type | Role |
|---|---|---|
| `ZCL_SDE_ADT_RES_TABLE` | CLAS | Resource. Inherits `CL_ADT_REST_RESOURCE`, redefines `get`. |
| `ZCL_SDE_ADT_RES_METRICS` | CLAS | Resource. Code metrics of an object, computed by ACE. |
| `ZCL_SDE_ADT_RES_VERSIONS` | CLAS | Resource. Versionable parts of an object and their versions, read by AVE. |
| `ZCL_SDE_ADT_RES_APP` | CLAS | Application. Inherits `CL_ADT_RES_APP_BASE`, redefines `fill_router`. |
| `ZSDE_ADT_RES_APP` | ENHO | BAdI implementation that registers the application. |

The classes live in [`src/`](src) and travel through abapGit. The ENHO was created in SAP and
is picked up by abapGit as well (all three objects are in package `Z_SDE`).

Routing is one line per service:

```abap
router->attach( iv_template      = '/zsde/table/{name}'
                iv_handler_class = 'ZCL_SDE_ADT_RES_TABLE' ).
router->attach( iv_template      = '/zsde/metrics/{name}'
                iv_handler_class = 'ZCL_SDE_ADT_RES_METRICS' ).
router->attach( iv_template      = '/zsde/versions/{name}'
                iv_handler_class = 'ZCL_SDE_ADT_RES_VERSIONS' ).
```

Every service of the VERTEX front end registers here rather than under a prefix of its own. A
second prefix means a second BAdI implementation and a second `STATIC_URI_PATH` filter — the
part of this page that costs an hour to get wrong. The prefix is where the ADT node is claimed,
not the identity of the service.

Templates are relative to `get_static_uri_path( )`, so `/zsde/table/{name}` serves
`/sap/bc/adt/zsde/table/{name}`. Read the path variable with
`request->get_uri_attribute( name = 'name' )` — the name is passed through verbatim, no case
conversion, so it must match the `{name}` in the template exactly.

## Registration

There is **no code inside the BAdI**. The implementing class *is* the application class.

1. SE18 → enhancement spot `SADT_REST_RFC_APPLICATION` → tab *Enh. Spot Element Definitions*
2. Pick the BAdI **`BADI_ADT_REST_RFC_APPLICATION`** ("REST application class registration")
   and open its `Implementations` node.
   The spot also contains `BADI_ADT_DISCOVERY_PROVIDER`; implementing that one instead is the
   easy misclick — it activates cleanly and the route still 404s.
3. Create → Enhancement Implementation `ZSDE_ADT_RES_APP`, BAdI Implementation
   `ZSDE_ADT_REST_RFC_APP`, Implementing Class `ZCL_SDE_ADT_RES_APP`.
4. Fill the filter (see below), then activate.

### The filter is mandatory and easy to get wrong

Filter field `STATIC_URI_PATH`, and the value must be the **full** path with `CP`:

```
STATIC_URI_PATH   CP   /sap/bc/adt/zsde/*
```

Two traps here.

**The value is the full path, not the router-relative one.** `SADT_CREATE_APPL_REST_RESOURCE`
does `GET BADI ... FILTERS static_uri_path = i_uri` where `i_uri` is the complete request path
(`/sap/bc/adt/zsde/table/T001`). A filter of `/zsde/*` never matches, and the miss surfaces as
`ExceptionResourceNotFound` / message `SADT_RESOURCE 002` — "Resource ... does not exist".

Reading `CL_ADT_REST_REGISTRATIONS=>get_all_adt_registrations` suggests filters are irrelevant,
because that method ignores them and calls `fill_router` on every implementation. It is a
dev-time listing that feeds URI proposals, not the dispatcher. The dispatcher is
`CL_ADT_WB_RES_APP` (ICF handler of `/sap/bc/adt`) → `CL_ADT_RES_APP_ACCESS=>get_application_resource`
→ that function module.

**`CP` looks missing in SE19.** The filter dialog is laid out as an interval,
`Value1 <Comparator1> Filter <Comparator2> Value2`, and Comparator 1 offers only relational
operators. Enter a single condition in the right half — leave Value 1 and Comparator 1 empty,
set Comparator 2 and Value 2 — and it is stored back into `VALUE1` with `COMPARE = CP`.

Leaving the filter empty is not an option either: the activation check then reports one
"active simultaneously" conflict per existing implementation (134 on this system), because an
empty filter means "responsible for every URI".

SAP's own applications typically register two disjunctions — `=` on the bare base path for the
base URL, and `CP` on `base/*` for everything under it. The `CP` row alone is enough to serve
sub-paths.

## Selecting rows

Filters are passed as indexed query parameters, one part per parameter. Nothing is packed into a
separator-delimited string, so a colon inside a `TIMS` value or a quote inside a text field
cannot break the request.

| Parameter | Meaning | Default |
|---|---|---|
| `f{i}` | field name | — |
| `s{i}` | sign, `I` or `E` | `I` |
| `o{i}` | option | `EQ` |
| `l{i}` | low value | — |
| `h{i}` | high value, for `BT` and `NB` | — |

Options are `EQ NE GT GE LT LE CP NP BT NB`; at most 20 lines are read. In `CP` and `NP` a `*`
becomes `%` and a `+` becomes `_`. Single quotes inside values are doubled before the literal is
built.

```
?rows=100&f1=BUKRS&o1=EQ&l1=0001
?rows=100&f1=LAND1&o1=CP&l1=D*
?f1=BUKRS&o1=BT&l1=0001&h1=1000
?f1=LAND1&o1=EQ&l1=DE&f2=LAND1&o2=EQ&l2=UA
?f1=LAND1&s1=E&o1=EQ&l1=DE
```

These are select-option semantics, not a naive chain of ANDs: **lines for the same field are
ORed**, an excluding line becomes `AND NOT`, and **different fields are ANDed**. The fourth
example above returns Germany or Ukraine; the fifth returns everything except Germany. That
matches `ZCL_SDE_SEL_OPT`, so moving real select-options across later needs no rework.

### Why not a raw WHERE string

`ZCL_SDE_SQL=>READ_ANY_TABLE` catches `CX_SY_DYNAMIC_OSQL_SYNTAX` with `#EC NO_HANDLER`. A
malformed condition would therefore return an empty table and no complaint at all, and a user
who mistyped a field name would conclude there is no data. Passing the parts separately lets the
resource check each one and answer:

| Situation | Response |
|---|---|
| Field not in the table | **400** `Table T001 has no field BUKRSX.` |
| Unknown option | **400**, listing the allowed ones |
| Field is `STRG` or `RSTR` | **400** — a LOB cannot appear in a WHERE clause |
| `BT` or `NB` without `h{i}` | **400**, naming the missing parameter |

## Code metrics

```
GET /sap/bc/adt/zsde/metrics/ZCL_SDE_SQL?type=CLAS
Content-Type: application/json

{
  "object": "zcl_sde_sql",
  "type": "clas",
  "program": "zcl_sde_sql===================cp",
  "totals": { "units": 4, "cyclomatic": 12, "avg_cyclomatic": 3.00, "loc": 62,
              "lloc": 38, "cloc": 9, "volume": 1841.55, "effort": 48310.22, "bugs": 0.613 },
  "units": [
    { "include": "zcl_sde_sql===================cm001", "unit_type": "method",
      "unit_name": "read_any_table", "cyclomatic": 5, "mi": 61.24, "loc": 28, "lloc": 17,
      "cloc": 4, "volume": 902.31, "difficulty": 18.40, "effort": 16602.50, "bugs": 0.301,
      "n1": 94, "n2": 71, "big_n1": 21, "big_n2": 39, "vocabulary": 60, "prog_length": 165 }
  ]
}
```

The numbers are ACE's — McCabe cyclomatic complexity, the Halstead set and the maintainability
index — so [ACE](https://github.com/ysichov/ACE) must be installed in the same system. Without
it the resource does not activate.

`type` accepts `CLAS`, `INTF`, `PROG` and `INCL`, with or without the ADT subtype: the caller may
send `CLAS/OC` as it comes from the object tree, and only the part in front of the slash is read.
Anything else answers 400 naming what is supported. A name that does not exist answers 404.

### Reaching ACE without SAP GUI

`ZCL_ACE_METRICS=>CALCULATE` takes a parse result, not an object name, and in ACE that result
lives inside a window object built on `CL_GUI` controls. None of that can run in an HTTP request.
The way in is `ZCL_ACE_PARSER=>PARSE`, which fills the parse structure through a CHANGING
parameter and touches no control at all — including `TT_CALLS_LINE`, the table of unit boundaries
the metrics read to find methods.

```abap
DATA ls_source TYPE zif_ace_parse_data=>ts_parse_data.
zcl_ace_parser=>parse( EXPORTING i_program = lv_program i_include = lv_program
                       CHANGING  cs_source = ls_source ).
DATA(ls_result) = zcl_ace_metrics=>calculate( is_parse_data = ls_source
                                              i_program     = lv_program ).
```

Two things decide whether the answer is complete.

**A class pool holds no code.** The method bodies are in its `CM` includes, so every include of
the pool is parsed as well — passing the *pool* as `i_program` and the *include* as `i_include`,
because `CALCULATE` aggregates `tt_progs WHERE program = i_program`. Passing the include as both,
which is what every caller inside ACE does, would leave each method in an object of its own.

**`D010INC` also returns the system includes.** `<SYSINI>` and its kin come back alongside the
real ones, and their units — `SYSTEM-EXIT`, `%_CTL_END` — then appear as code nobody wrote and
are counted into the totals. They are skipped by the angle bracket, which cannot occur in a
repository object name.

### Deliberate gaps here too

- **No package mode.** One object per request. ACE can walk a package; parsing one in an HTTP
  request would need a progress channel this resource does not have.
- **Test includes are included.** `CCAU` is parsed like any other include, so a class with unit
  tests reports them among its methods. The include of every unit is in the payload, so a client
  can group or drop them; nothing is filtered away silently here.
- **No token detail.** `ZCL_ACE_METRICS` also returns the classified token list behind the
  Halstead counts. It is debugging material and larger than everything else together, so it is
  left out of the payload.

## Object versions

Two shapes on one path. Without `part`, the versionable parts of the object:

```
GET /sap/bc/adt/zsde/versions/ZCL_X?type=CLAS

{
  "object": "zcl_x",
  "type": "clas",
  "parts": [
    { "class": "ZCL_X", "unit": "Public section", "name": "ZCL_X", "part_type": "CPUB" },
    { "class": "ZCL_X", "unit": "DO_WORK", "name": "ZCL_X                         DO_WORK",
      "part_type": "METH" }
  ]
}
```

With `part` and `ptype`, the versions of that one part:

```
GET /sap/bc/adt/zsde/versions/ZCL_X?type=CLAS&part=ZCL_X%20...%20DO_WORK&ptype=METH

{
  "object": "zcl_x", "type": "clas", "part": "...", "part_type": "meth",
  "versions": [
    { "version": "00003", "date": "20260714", "time": "104512", "author": "SYCHOV",
      "author_name": "Yurii Sychov", "request": "ALCK900593", "task": "ALCK900614" }
  ]
}
```

Dates and times are passed as the dictionary holds them, `YYYYMMDD` and `HHMMSS`. Formatting is
the reader's job, because only the reader knows the locale.

**Why two requests.** The parts of a class are its sections, its local includes and one entry per
method. Answering both in one call would read the version directory once per part just to draw a
list of names — eighty reads for eighty rows. The parts list is cheap; the versions are asked for
one part at a time, which is also how AVE's own two grids work.

The numbers are AVE's, so [AVE](https://github.com/ysichov/AVE) must be installed in the same
system. Its whole version layer is free of `CL_GUI`, so nothing had to be worked around:

```abap
DATA(lo_object) = NEW zcl_ave_object_factory( )->get_instance(
                      object_type = 'CLAS' object_name = lv_name ).
DATA(lt_parts)  = lo_object->get_parts( ).
DATA(lo_vrsd)   = NEW zcl_ave_vrsd( type = ls_part-type name = ls_part-object_name ).
DATA(lo_ver)    = NEW zcl_ave_version( ls_vrsd ).
```

`type` accepts `CLAS`, `INTF`, `PROG`, `INCL`, `FUGR`, `FUNC`, `DDLS`, `TABL`, `DOMA` and `DTEL`,
with or without the ADT subtype. The DDIC three are mapped to the VRSD part types AVE expects
(`TABD`, `DOMD`, `DTED`).

### A transport and a package are refused

Both answer 400 naming the reason. AVE reads them, and reading them is what AVE is for — but a
request is dozens of objects, and AVE reports progress with an estimate and asks whether to
continue when it grows. One blocking HTTP call has nowhere to put that, so it is refused rather
than left to time out.

### ZCX_AVE carries no message

Its constructor passes only `previous` to the superclass, so `get_text( )` on the exception
itself is the generic class text. The resource walks the `previous` chain and joins what it finds,
because "AVE cannot list the parts of ZCL_X" with no reason after it is a silent failure wearing
an error message.

## Verifying the registration without guessing at screens

```sql
-- must show IF_ADT_REST_RFC_APPLICATION, not IF_ADT_DISCOVERY_PROVIDER
SELECT * FROM enhobj WHERE enhname = 'ZSDE_ADT_RES_APP'

-- the actual stored filter (TYPE 'S' filters live here, BADI_CHAR_COND stays empty)
SELECT enhname, badi_impl, filter_name, value1, compare, is_interval, conj_idx, disj_idx
  FROM badi_string_cond WHERE enhname = 'ZSDE_ADT_RES_APP'
```

Confirmed working state:

| FILTER_NAME | VALUE1 | COMPARE | IS_INTERVAL | CONJ_IDX | DISJ_IDX |
|---|---|---|---|---|---|
| STATIC_URI_PATH | `/sap/bc/adt/zsde/*` | CP | | 1 | 1 |

To find the host and port for a browser test: SICF → service path `/sap/bc/adt` → right-click
the `adt` node → *Test Service*, then edit the path in the address bar.

Reading the response: a 404 means the registration is not matched, 500 means the route resolved
and `get( )` raised, 403 means authorization stopped the request before our code ran.

## ADT safe mode

`CL_ADT_SAFE_MODE_BADI_ACCESS` can switch the framework to a second factory that loads BAdI
implementations differently. It is driven by table `SADT_BADI_ACCESS`; the table is empty here,
so safe mode is off. Both factories read the same `STATIC_URI_PATH` filter, so the value above
is correct either way.

## Known leftovers

- SE19 added a stray `interfaces IF_ADT_DISCOVERY_PROVIDER .` to `ZCL_SDE_ADT_RES_APP` during
  the first, wrong registration, and staging back from SAP carried it into the repository.
  `REGISTER_WORKSPACES` is unimplemented, which ABAP reports only as a warning, so the class
  activates either way, and nothing calls it now that the discovery implementation is gone.
  The line has been removed here; the system still has it until the next abapGit pull and
  activation.

## How the payload is built

`CL_ADT_REST_JSON_HANDLER` is not usable here: it serializes through a Simple Transformation
named at construction time, and an ST is statically typed, so it cannot describe a table
structure only known at runtime. Instead `/UI2/CL_JSON=>serialize( )` builds the JSON from RTTI,
and the result is returned through `CL_ADT_REST_PLAIN_TEXT_HANDLER` constructed with
`content_type = if_rest_media_type=>gc_appl_json` — that handler takes the content type as a
constructor parameter, so the correct `Content-Type` is set without adding an ST object.

The row structure comes from `cl_abap_typedescr=>describe_by_name( )` →
`cl_abap_tabledescr=>create( )` → `CREATE DATA ... TYPE HANDLE`. Note that `TYPE HANDLE` does
not accept an inline functional call; the descriptor has to be in a variable first.

The catalogue is one call, `cl_abap_structdescr->get_ddic_field_list( )`, which returns key
flags, DDIC data types, lengths and field texts together.

### Deliberate gaps

- **No conversion exits.** Values are serialized as stored, so `ALPHA`-padded keys arrive
  padded and no unit or currency formatting is applied. This is lossless and reversible;
  formatting is a decision for the client or a later flag, not something to bake in silently.
- **Transparent and cluster tables only.** `zcl_sde_sql=>exist_table( )` matches `TRANSP` and
  `CLUSTER`, so views and CDS entities return 404. `zcl_sde_sql` has `exist_view( )` and
  `exist_cds( )` ready for when that is wanted.
- **No paging.** `rows` caps the read, there is no offset or cursor.

A missing table produces a real 404 (`CX_ADT_RES_NOT_FOUND`) rather than an empty result —
`zcl_sde_sql=>read_any_table( )` silently returns nothing for an unknown table, so the resource
checks existence itself first.

## Next step

Two questions are still open and should be answered before this goes anywhere near a productive
system:

- **Authorization.** SDE reads arbitrary tables. `BADI_ADT_REST_AUTHORIZATION` in package
  `SADT_REST` is the intended hook; `S_TABU_DIS` / `S_TABU_NAM` checks belong there or in the
  resource itself.
- ~~**Volume.**~~ Measured on ALC: `TADIR?rows=10000` returns in 2–3 seconds end to end.
  Accepted as adequate, so the array-of-objects payload stays and no paging was added. Worth
  keeping in mind that roughly half of that payload is field names repeated on every row; if it
  ever needs to shrink, `rows` can become positional arrays aligned with `fields`.

  The rate does constrain one thing: SDE's select-options re-read as you type, and 2–3 seconds
  per 10 000 rows is a page-load budget, not a keystroke budget. Reactive filtering needs a
  small default page size, not a faster serializer.
