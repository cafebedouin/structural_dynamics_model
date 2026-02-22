import re
import time
import random
from datetime import datetime
from agent.story_generator_base import (
    EXCLUSION_PATTERNS, PROLOG_DIR, generate_story, load_processed_log,
)

URL_CAP = 500
URL_FILE = PROLOG_DIR / "urls.txt"
PROCESSED_LOG = PROLOG_DIR / "processed_links.txt"


def get_urls_to_process():
    """Read URLs from file, filter against seen links and exclusions."""
    if not URL_FILE.exists():
        print(f"Error: URL file not found at {URL_FILE}. Please create it.")
        return []

    all_urls = [line.strip() for line in URL_FILE.read_text().splitlines()
                if line.strip().startswith('http')]
    seen_links = load_processed_log(PROCESSED_LOG)
    exclusion_regex = re.compile('|'.join(EXCLUSION_PATTERNS), re.IGNORECASE)

    new_links = []
    for url in all_urls:
        if url not in seen_links and not exclusion_regex.search(url):
            new_links.append(url)
            seen_links.add(url)

    return list(dict.fromkeys(new_links))[:URL_CAP]


def run_workflow():
    links = get_urls_to_process()
    if not links:
        print(f"[{datetime.now()}] No new stories to process from {URL_FILE}.")
        return

    print(f"Found {len(links)} new stories to process. Starting batch...")
    for i, link in enumerate(links):
        print(f"\n[{i+1}/{len(links)}] Processing: {link}")
        generate_story(
            source_description=f"URL: {link}",
            processed_log_path=PROCESSED_LOG,
            log_key=link,
        )
        if i < len(links) - 1:
            sleep_gap = random.uniform(30, 90)
            print(f"Sleeping for {sleep_gap:.2f}s before next story...")
            time.sleep(sleep_gap)


if __name__ == "__main__":
    run_workflow()
