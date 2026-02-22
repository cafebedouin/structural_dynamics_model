import re
import time
import random
from agent.story_generator_base import PROLOG_DIR, generate_story, load_processed_log

TOPIC_FILE = PROLOG_DIR / "topics.txt"
PROCESSED_LOG = PROLOG_DIR / "processed_topics.txt"


def sanitize_topic(filename):
    """Converts 'ai_auditability_gap.pl' to 'AI auditability gap'."""
    return filename.replace(".pl", "").replace("_", " ").title()


def get_topics_to_process():
    if not TOPIC_FILE.exists():
        print(f"Error: {TOPIC_FILE} not found.")
        return []

    all_topics = [line.strip() for line in TOPIC_FILE.read_text().splitlines()
                  if line.strip()]
    seen = load_processed_log(PROCESSED_LOG)
    return [t for t in all_topics if t not in seen]


def run_workflow():
    topics = get_topics_to_process()
    if not topics:
        print("No new topics to process.")
        return

    print(f"Starting batch for {len(topics)} topics.")
    for i, filename in enumerate(topics):
        topic_query = sanitize_topic(filename)
        print(f"\n--- [{i+1}/{len(topics)}] Generating: {topic_query} ---")
        generate_story(
            source_description=f"TOPIC: {topic_query}",
            processed_log_path=PROCESSED_LOG,
            log_key=filename,
            model="gemini-2.0-flash",
        )
        if i < len(topics) - 1:
            delay = random.uniform(30, 60)
            print(f"Cooling down for {delay:.2f}s...")
            time.sleep(delay)


if __name__ == "__main__":
    run_workflow()
