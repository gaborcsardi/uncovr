## 📊 {{ package }} test coverage

{{ total_emoji }} **{{ total_percent }}** lines \
`{{ total_lines_covered }}/{{ total_lines }}` &emsp; \
{{ total_funcs_emoji }} **{{ total_funcs_percent}}** functions \
`{{total_funcs_hit }}/{{ total_funcs }}`

| File | Lines | Functions | Coverage |
|:-----|------:|----------:|---------:|
{{ data }}

## 📊 {{ package }} test results

{{ emoji$fail }} **{{ n$fail }}** failed &nbsp; \
{{ emoji$warn }} **{{ n$warn }}** warnings &nbsp; \
{{ emoji$skip }} **{{ n$skip }}** skips \
{{ emoji$ok }} **{{ n$ok }}** passed &nbsp;

| File | {{ emoji$fail }} Fail | {{ emoji$warn }} Warn | {{ emoji$skip }} Skip | {{ emoji$ok }} Pass |
|:-----|-----:|-----:|-----:|-----:|
{{ test_files }}

## 🧪 Test details

<details><summary>See here.</summary>
| File | Test | Fail | Warn | Skip | Pass |
|:-----|:-----|-----:|-----:|-----:|-----:|
{{ test_details }}
</details>
