# SDE as an ADT REST resource

Status: **reads real data**. A custom resource is registered under `/sap/bc/adt/zsde/` and
returns table rows plus a field catalogue as JSON.

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
| `ZCL_SDE_ADT_RES_APP` | CLAS | Application. Inherits `CL_ADT_RES_APP_BASE`, redefines `fill_router`. |
| `ZSDE_ADT_RES_APP` | ENHO | BAdI implementation that registers the application. |

The classes live in [`src/`](src) and travel through abapGit. The ENHO was created in SAP and
is picked up by abapGit as well (all three objects are in package `Z_SDE`).

Routing is one line:

```abap
router->attach( iv_template      = '/zsde/table/{name}'
                iv_handler_class = 'ZCL_SDE_ADT_RES_TABLE' ).
```

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
