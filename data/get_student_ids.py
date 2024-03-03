import pandas as pd

# Load the CSV files
df1 = pd.read_csv('current_academic_student_att_less_than_5%.csv')
df2 = pd.read_csv('student_att_less_than_10%.csv')
df3 = pd.read_csv('system_bts_0_previous_academic_year.csv')
df4 = pd.read_csv('dropped_out_school_students.csv')

# Extract the 'id' column from each DataFrame
id_df1 = df1[['id']]
id_df2 = df2[['id']]
id_df3 = df3[['id']]
id_df4 = df4[['id']]

# Concatenate the 'id' columns
all_ids = pd.concat([id_df1, id_df2, id_df3, id_df4])

# Drop duplicate 'id' values
distinct_ids = all_ids.drop_duplicates()

# Save the distinct ids to a new CSV file
distinct_ids.to_csv('distinct_ids.csv', index=False)

