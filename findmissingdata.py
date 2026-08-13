import os
import csv

# --- SETUP: Update these variables ---
csv_file_path = r'C:\Users\rmdon\Box\Roitman Data Repository\Projects\2026_FPSignalCharacterizationAutomation_RD\Metadata\FileKey_FPSignalCharacterizationAutomation.csv'
parent_folder_path = r'C:\Users\rmdon\Box\RoitmanTeamPhotometryFolder\Rachel\Rig Backups\Rizzo 260720\SignalChecking-251111-121406'
column_name = 'BlockFolder' 
output_csv_path = r'C:\Users\rmdon\Box\Roitman Data Repository\Projects\2026_FPSignalCharacterizationAutomation_RD\Metadata\Missing_Rizzo_SignalChecking-251111-121406_FPSignalCharacterizationAutomation.csv' # Where to save the results
# ------------------------------------------

# 1. Read the CSV and store names in a Set for instant lookup
csv_folders = set()
with open(csv_file_path, mode='r', encoding='utf-8-sig') as f:
    reader = csv.DictReader(f)
    for row in reader:
        # .strip() removes accidental spaces around the name
        csv_folders.add(row[column_name].strip()) 

# 2. Get actual folders from your computer
actual_folders = {
    entry.name for entry in os.scandir(parent_folder_path) if entry.is_dir()
}

# 3. Find the difference (In actual_folders, but NOT in csv_folders)
missing_from_csv = actual_folders - csv_folders

# 4. Output the results to a new CSV file
with open(output_csv_path, mode='w', newline='', encoding='utf-8') as f:
    writer = csv.writer(f)
    
    # Write a header row for the new file
    writer.writerow(['BlockFolder']) 
    
    # Write each missing folder name into its own row
    for folder in missing_from_csv:
        writer.writerow([folder])

print(f"Success! Found {len(missing_from_csv)} missing folders.")
print(f"Results saved to: {output_csv_path}")