import re
import time
import random
import feedparser
from datetime import datetime
from agent.story_generator_base import (
    EXCLUSION_PATTERNS, PROLOG_DIR, generate_story, load_processed_log,
)

RSS_URLS = [
    "https://rss.beehiiv.com/feeds/4aF2pGVAEN.xml",
    "https://www.numlock.com/feed",
]
URL_CAP = 100
PROCESSED_LOG = PROLOG_DIR / "processed_links.txt"


def get_new_links():
    """Fetch links from RSS feeds, filter against seen links and exclusions."""
    seen_links = load_processed_log(PROCESSED_LOG)
    exclusion_regex = re.compile('|'.join(EXCLUSION_PATTERNS), re.IGNORECASE)
    all_new_links = []

    for url in RSS_URLS:
        print(f"Fetching feed: {url}")
        feed = feedparser.parse(url)
        for entry in feed.entries:
            content = entry.get('summary', '')
            if 'content' in entry:
                for item in entry.content:
                    content += item.get('value', '')
            for link in re.findall(r'href="(https?://[^"]+)"', content):
                if link not in seen_links and not exclusion_regex.search(link):
                    all_new_links.append(link)
                    seen_links.add(link)

    return list(dict.fromkeys(all_new_links))[:URL_CAP]


def run_workflow():
    links = get_new_links()
    if not links:
        print(f"[{datetime.now()}] No new stories to process.")
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
