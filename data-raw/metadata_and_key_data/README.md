# README `data-raw/metadata_and_key_data`

# Intake Survey Metadata

# Round 1 Survey Metadata

`round_1_question_key.csv`
The final data key containing the unique question ID's `qname`, a text description of the corresponding question presented to analysts in the Qualtrics survey `question`, and the corresponding `variable_name` assigned to the question. 
- Are there repeated values? repeated surveys? repeated values? #TODO?
- The question key is used in preprocessing the round 1 survey data with `preprocess_survey_r1()`.

# Round 2 Survey Metadata

- Qualtrics question key: Contains the HTML formatted questions presented to the analysts, does not include duplicated question ID's per Qualtrics output `"data-raw/metadata_and_key_data/round_2_qualtrics_question_key.csv"`
- question key: The final data key containing unique question ID's `qname`, a text description of the corresponding question presented to the analysts `question` and the corresponding `variable_name`. Values of `qname`, `variable_name` are repeated because analysts were able to submit responses for multiple analyses in a single Qualtrics survey response.
- `round_2_question_key.csv` is generated by `round_2_question_key.R`


TODO explain difference between these two key files

# Review Survey Metadata

