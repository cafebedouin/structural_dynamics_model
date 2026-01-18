import re

class NarrativeContextHandler:
    def __init__(self, raw_text):
        self.raw_text = raw_text
        self.intervals = []

    def chunk_by_narrative(self, anchor_pattern=r"(Chapter \d+|Section \d+|ARTICLE [IVX]+)"):
        """
        Chunks the document based on structural headers to preserve
        ontological coherence within each interval.
        """
        # Identify start positions of narrative headers
        splits = [m.start() for m in re.finditer(anchor_pattern, self.raw_text)]
        if not splits:
            # Fallback to a mid-point narrative break if no headers found
            splits = [0, len(self.raw_text) // 2]

        splits.append(len(self.raw_text))

        for i in range(len(splits) - 1):
            chunk = self.raw_text[splits[i]:splits[i+1]]
            interval_id = f"interval_{i}"
            # Define T0 and Tn as relative integers for Prolog temporal integrity
            self.intervals.append({
                "id": interval_id,
                "text": chunk.strip(),
                "t_range": (i * 10, (i + 1) * 10) # Maps to integer requirements
            })
        return self.intervals

    def map_to_prolog_intervals(self):
        """Generates the interval/3 predicates for the ontology."""
        predicates = []
        for item in self.intervals:
            t0, tn = item["t_range"]
            predicates.append(f"interval({item['id']}, {t0}, {tn}).")
        return "\n".join(predicates)
