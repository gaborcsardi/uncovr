## 🔬 {{ package }} test coverage

{{ total_emoji }} **{{ total_percent }}** lines \
`{{ total_lines_covered }}/{{ total_lines }}` &emsp; \
{{ total_funcs_emoji }} **{{ total_funcs_percent}}** functions \
`{{total_funcs_hit }}/{{ total_funcs }}`

| File | Lines | Functions | Coverage |
|:-----|------:|----------:|---------:|
{{ data }}

## 📊 {{ package }} test results

{{ emo_fail }} **{{ n$fail }}** failed &emsp; \
{{ emo_warn }} **{{ n$warn }}** warnings &emsp; \
{{ emo_skip }} **{{ n$skip }}** skips &emsp; \
{{ emo_ok }} **{{ n$ok }}** passed

| File | {{ emo_fail }} Fail | {{ emo_warn }} Warn | {{ emo_skip }} Skip | {{ emo_ok }} Pass |
|:-----|-----:|-----:|-----:|-----:|
{{ test_files }}

<details>
<summary>

## 🧪 Test details (click to reveal)

</summary>

| File | Test | {{ emo_fail }} Fail | {{ emo_warn }} Warn | {{ emo_skip }} Skip | {{ emo_ok }} Pass |
|:-----|:-----|-----:|-----:|-----:|-----:|
{{ test_details }}

</details>
