"""DEPRECATED: This script is no longer needed for the JSON workflow.

The JSON workflow names files by constraint_id directly (via
story_generator_base.save_story), eliminating the timestamp-rename step.
This script remains useful only for renaming legacy timestamp-named .pl
files produced by the old Prolog-template generators.
"""

import os
import re

def rename_story_files():
    """
    Renames story files in the 'constraint_stories' directory from a timestamp-based
    name to a name based on the IntervalID found within the file.
    """
    story_dir = 'constraint_stories'
    if not os.path.isdir(story_dir):
        print(f"Error: Directory '{story_dir}' not found.")
        return

    for filename in os.listdir(story_dir):
        if filename.endswith('.pl'):
            old_path = os.path.join(story_dir, filename)
            try:
                with open(old_path, 'r') as f:
                    content = f.read()

                match = re.search(r"constraint_id:\s*([a-zA-Z0-9_]+)", content)

                if match:
                    interval_id = match.group(1)
                    new_filename = f"{interval_id}.pl"
                    new_path = os.path.join(story_dir, new_filename)

                    if old_path == new_path:
                        print(f"Skipping '{filename}', already named correctly.")
                        continue

                    if os.path.exists(new_path):
                        print(f"Warning: A file named '{new_filename}' already exists. Skipping rename for '{filename}'.")
                        continue

                    os.rename(old_path, new_path)
                    print(f"Renamed '{filename}' to '{new_filename}'")
                else:
                    print(f"Warning: No IntervalID found in '{filename}'. Skipping.")
            except Exception as e:
                print(f"Error processing file '{filename}': {e}")

if __name__ == '__main__':
    rename_story_files()
