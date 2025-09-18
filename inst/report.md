## 📊 {{ package }} test coverage

{{ total_emoji }} **{{ total_percent }}** lines \
`{{ total_lines_covered }}/{{ total_lines }}` &emsp; \
{{ total_funcs_emoji }} **{{ total_funcs_percent}}** functions \
`{{total_funcs_hit }}/{{ total_funcs }}`

| File | Lines | Functions | Coverage |
|:-----|------:|----------:|---------:|
{{ data }}

## 📊 {{ package }} test results

{{ emo_fail }} **{{ n$fail }}** failed &nbsp; \
{{ emo_warn }} **{{ n$warn }}** warnings &nbsp; \
{{ emo_skip }} **{{ n$skip }}** skips \
{{ emo_ok }} **{{ n$ok }}** passed &nbsp;

| File | {{ emo_fail }} Fail | {{ emo_warn }} Warn | {{ emo_skip }} Skip | {{ emo_ok }} Pass |
|:-----|-----:|-----:|-----:|-----:|
{{ test_files }}

## 🧪 Test details

<details><summary>See here.</summary>
| File | Test | Fail | Warn | Skip | Pass |
|:-----|:-----|-----:|-----:|-----:|-----:|
{{ test_details }}
</details>
