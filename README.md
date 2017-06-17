# json2xlsx

```
writexlsx {\"Sheet1\":{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"name\":\"Courier\",\"color\":\"red\"}}}}} -o output.xlsx
```

```
Set jsonCells={\"Sheet1\":{\"A1\":{\"value\":2,\"format\":{\"numberFormat\":\"2Decimal\",\"font\":{\"bold\":true}}},\"B2\":{\"value\":1000,\"format\":{\"numberFormat\":\"yyyy-mm-dd;@\"}},\"A3\":{\"value\":\"abc\",\"format\":{\"font\":{\"name\":\"Courier\",\"color\":\"red\"}}}}}
writexlsx -c %jsonCells% -o output.xlsx
```

-   Include images:

```
Set jsonImages={\"Sheet1\":[{\"file\":\"image.png\",\"left\":3,\"top\":3,\"width\":200,\"height\":300}],\"Sheet2\":[{\"file\":\"image2.png\",\"left\":2,\"top\":3,\"width\":200,\"height\":300}]}
writexlsx -c %jsonCells% -i %jsonImages% -o output.xlsx
```
