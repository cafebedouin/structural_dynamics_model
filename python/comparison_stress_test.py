#!/usr/bin/env python3
"""
Comparison Stress Test: Pre-Rebuild vs Current Classification

Extracts corpus_data.json from commit 8150d65 (last pre-rebuild snapshot,
Jan 26) and compares it against the current HEAD output. Flags every
constraint whose classification type changed, surfacing the migration
from the old 3-type system (mountain, rope, noose + unclassified) to
the current 6-type system (mountain, rope, tangled_rope, snare, scaffold, piton).
"""

import json
import subprocess
import sys
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

BASELINE_COMMIT = '8150d65'

# Prolog variable artifacts that leak into old classification arrays
ARTIFACT_TYPES = {
    'Type', 'Type1', 'Type2', 'Type3',
    'T1', 'T2', 'T3',
    'Mountain', 'Rope', 'Noose',
}

METRIC_DRIFT_THRESHOLD = 0.1

SCRIPT_DIR = Path(__file__).resolve().parent
PROJECT_ROOT = SCRIPT_DIR.parent
CORPUS_PATH = PROJECT_ROOT / 'outputs' / 'corpus_data.json'
REPORT_PATH = PROJECT_ROOT / 'outputs' / 'stress_test_report.md'


def normalize_type(t):
    """Normalize claimed_type for display: None -> 'NONE'."""
    if t is None:
        return 'NONE'
    return str(t)


def is_artifact_classification(cl):
    """Return True if a classification entry is a Prolog variable artifact."""
    t = cl.get('type', '')
    if t in ARTIFACT_TYPES:
        return True
    return False


def is_null_context(cl):
    """Return True if context is [null, null, null, null]."""
    ctx = cl.get('context')
    if isinstance(ctx, list) and ctx == [None, None, None, None]:
        return True
    return False


def clean_classifications(classifications):
    """Filter out artifact types and null-context entries."""
    return [
        cl for cl in classifications
        if not is_artifact_classification(cl) and not is_null_context(cl)
    ]


class ComparisonEngine:
    """Compares two corpus snapshots and computes diffs."""

    def __init__(self, old_data, new_data):
        self.old = old_data
        self.new = new_data

        self.old_keys = set(self.old.keys())
        self.new_keys = set(self.new.keys())
        self.shared = self.old_keys & self.new_keys
        self.added = self.new_keys - self.old_keys
        self.removed = self.old_keys - self.new_keys

    def run(self):
        """Run all comparisons and return results dict."""
        return {
            'summary': self._summary_stats(),
            'type_changes': self._claimed_type_changes(),
            'classification_changes': self._classification_changes(),
            'metric_drift': self._metric_drift(),
            'migration_matrix': self._migration_matrix(),
        }

    def _summary_stats(self):
        """Section 1: counts and type distributions."""
        old_types = Counter(
            normalize_type(c.get('claimed_type'))
            for c in self.old.values()
        )
        new_types = Counter(
            normalize_type(c.get('claimed_type'))
            for c in self.new.values()
        )

        # Type changes within shared set
        changed = 0
        unchanged = 0
        for k in self.shared:
            ot = normalize_type(self.old[k].get('claimed_type'))
            nt = normalize_type(self.new[k].get('claimed_type'))
            if ot != nt:
                changed += 1
            else:
                unchanged += 1

        return {
            'old_total': len(self.old),
            'new_total': len(self.new),
            'shared': len(self.shared),
            'added': len(self.added),
            'removed': len(self.removed),
            'changed': changed,
            'unchanged': unchanged,
            'old_type_dist': old_types,
            'new_type_dist': new_types,
        }

    def _claimed_type_changes(self):
        """Section 2: every constraint that changed claimed_type, grouped by path."""
        changes_by_path = defaultdict(list)

        for k in sorted(self.shared):
            old_c = self.old[k]
            new_c = self.new[k]
            ot = normalize_type(old_c.get('claimed_type'))
            nt = normalize_type(new_c.get('claimed_type'))

            if ot == nt:
                continue

            old_metrics = old_c.get('metrics', {})
            new_metrics = new_c.get('metrics', {})

            record = {
                'constraint_id': k,
                'old_type': ot,
                'new_type': nt,
                'domain': new_c.get('domain', old_c.get('domain', 'unknown')),
                'old_extractiveness': old_metrics.get('extractiveness'),
                'new_extractiveness': new_metrics.get('extractiveness'),
                'old_suppression': old_metrics.get('suppression'),
                'new_suppression': new_metrics.get('suppression'),
            }
            path = f"{ot} -> {nt}"
            changes_by_path[path].append(record)

        return changes_by_path

    def _classification_changes(self):
        """Section 3: constraints where classification arrays changed (even if claimed_type didn't)."""
        changes = []

        for k in sorted(self.shared):
            old_c = self.old[k]
            new_c = self.new[k]

            old_cls = clean_classifications(old_c.get('classifications', []))
            new_cls = clean_classifications(new_c.get('classifications', []))

            # Compare type sequences after cleaning
            old_types_seq = tuple(cl.get('type') for cl in old_cls)
            new_types_seq = tuple(cl.get('type') for cl in new_cls)

            if old_types_seq != new_types_seq:
                old_type_counts = Counter(old_types_seq)
                new_type_counts = Counter(new_types_seq)

                changes.append({
                    'constraint_id': k,
                    'claimed_type_changed': normalize_type(old_c.get('claimed_type')) != normalize_type(new_c.get('claimed_type')),
                    'old_claimed_type': normalize_type(old_c.get('claimed_type')),
                    'new_claimed_type': normalize_type(new_c.get('claimed_type')),
                    'old_perspective_types': dict(old_type_counts),
                    'new_perspective_types': dict(new_type_counts),
                    'old_count': len(old_cls),
                    'new_count': len(new_cls),
                })

        return changes

    def _metric_drift(self):
        """Section 4: constraints where extractiveness or suppression moved > threshold."""
        drifts = []

        for k in sorted(self.shared):
            old_m = self.old[k].get('metrics', {})
            new_m = self.new[k].get('metrics', {})

            old_ext = old_m.get('extractiveness')
            new_ext = new_m.get('extractiveness')
            old_sup = old_m.get('suppression')
            new_sup = new_m.get('suppression')

            ext_delta = None
            sup_delta = None

            if old_ext is not None and new_ext is not None:
                ext_delta = new_ext - old_ext
            if old_sup is not None and new_sup is not None:
                sup_delta = new_sup - old_sup

            if ((ext_delta is not None and abs(ext_delta) > METRIC_DRIFT_THRESHOLD) or
                    (sup_delta is not None and abs(sup_delta) > METRIC_DRIFT_THRESHOLD)):
                drifts.append({
                    'constraint_id': k,
                    'old_extractiveness': old_ext,
                    'new_extractiveness': new_ext,
                    'ext_delta': ext_delta,
                    'old_suppression': old_sup,
                    'new_suppression': new_sup,
                    'sup_delta': sup_delta,
                    'domain': self.new[k].get('domain', 'unknown'),
                    'old_type': normalize_type(self.old[k].get('claimed_type')),
                    'new_type': normalize_type(self.new[k].get('claimed_type')),
                })

        # Sort by largest absolute delta
        drifts.sort(key=lambda d: max(
            abs(d['ext_delta']) if d['ext_delta'] is not None else 0,
            abs(d['sup_delta']) if d['sup_delta'] is not None else 0
        ), reverse=True)

        return drifts

    def _migration_matrix(self):
        """Section 5: cross-tab of old_type x new_type."""
        matrix = Counter()
        for k in self.shared:
            ot = normalize_type(self.old[k].get('claimed_type'))
            nt = normalize_type(self.new[k].get('claimed_type'))
            matrix[(ot, nt)] += 1
        return matrix


class ReportGenerator:
    """Generates the 5-section markdown report."""

    def __init__(self, results, old_commit, new_commit):
        self.results = results
        self.old_commit = old_commit
        self.new_commit = new_commit

    def generate(self):
        """Return complete markdown report string."""
        lines = []
        lines.append(f"# Stress Test: Pre-Rebuild vs Current Classification Comparison")
        lines.append(f"")
        lines.append(f"**Generated:** {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        lines.append(f"**Baseline commit:** `{self.old_commit}` (pre-rebuild)")
        lines.append(f"**Current:** HEAD")
        lines.append(f"")

        lines.extend(self._section_summary())
        lines.extend(self._section_type_changes())
        lines.extend(self._section_classification_changes())
        lines.extend(self._section_metric_drift())
        lines.extend(self._section_migration_matrix())

        return '\n'.join(lines)

    def _section_summary(self):
        """Section 1: Summary."""
        s = self.results['summary']
        lines = []
        lines.append("## 1. Summary")
        lines.append("")
        lines.append("### Corpus Comparison")
        lines.append("")
        lines.append(f"| Metric | Count |")
        lines.append(f"|--------|-------|")
        lines.append(f"| Old corpus constraints | {s['old_total']} |")
        lines.append(f"| Current corpus constraints | {s['new_total']} |")
        lines.append(f"| Shared (intersection) | {s['shared']} |")
        lines.append(f"| Added in current | {s['added']} |")
        lines.append(f"| Removed from old | {s['removed']} |")
        lines.append(f"| Type changed (shared) | {s['changed']} |")
        lines.append(f"| Type unchanged (shared) | {s['unchanged']} |")
        lines.append("")

        lines.append("### Type Distribution Side-by-Side")
        lines.append("")
        all_types = sorted(set(list(s['old_type_dist'].keys()) + list(s['new_type_dist'].keys())))
        lines.append("| Type | Old Count | Old % | New Count | New % |")
        lines.append("|------|-----------|-------|-----------|-------|")
        for t in all_types:
            old_c = s['old_type_dist'].get(t, 0)
            new_c = s['new_type_dist'].get(t, 0)
            old_pct = (old_c / s['old_total'] * 100) if s['old_total'] > 0 else 0
            new_pct = (new_c / s['new_total'] * 100) if s['new_total'] > 0 else 0
            lines.append(f"| {t} | {old_c} | {old_pct:.1f}% | {new_c} | {new_pct:.1f}% |")
        lines.append("")
        return lines

    def _section_type_changes(self):
        """Section 2: Claimed Type Changes."""
        changes = self.results['type_changes']
        lines = []
        lines.append("## 2. Claimed Type Changes")
        lines.append("")

        total_changed = sum(len(v) for v in changes.values())
        lines.append(f"**{total_changed} constraints** changed `claimed_type` across **{len(changes)} migration paths**.")
        lines.append("")

        for path in sorted(changes.keys(), key=lambda p: len(changes[p]), reverse=True):
            records = changes[path]
            lines.append(f"### {path} ({len(records)} constraints)")
            lines.append("")
            lines.append("| Constraint | Domain | Old Ext | New Ext | Old Sup | New Sup |")
            lines.append("|------------|--------|---------|---------|---------|---------|")

            for r in records:
                old_ext = f"{r['old_extractiveness']:.2f}" if r['old_extractiveness'] is not None else "N/A"
                new_ext = f"{r['new_extractiveness']:.2f}" if r['new_extractiveness'] is not None else "N/A"
                old_sup = f"{r['old_suppression']:.2f}" if r['old_suppression'] is not None else "N/A"
                new_sup = f"{r['new_suppression']:.2f}" if r['new_suppression'] is not None else "N/A"
                lines.append(f"| {r['constraint_id']} | {r['domain']} | {old_ext} | {new_ext} | {old_sup} | {new_sup} |")

            lines.append("")

        return lines

    def _section_classification_changes(self):
        """Section 3: Perspective Changes."""
        changes = self.results['classification_changes']
        lines = []
        lines.append("## 3. Perspective Changes")
        lines.append("")
        lines.append(f"**{len(changes)} constraints** had perspective-level classification changes")
        lines.append(f"(after filtering Prolog artifacts and null-context entries).")
        lines.append("")

        # Split into those where claimed_type also changed vs silent reclassifications
        silent = [c for c in changes if not c['claimed_type_changed']]
        overt = [c for c in changes if c['claimed_type_changed']]

        lines.append(f"- Overt (claimed_type also changed): {len(overt)}")
        lines.append(f"- Silent (claimed_type unchanged, perspectives differ): {len(silent)}")
        lines.append("")

        if silent:
            lines.append("### Silent Reclassifications")
            lines.append("")
            lines.append("These constraints kept the same `claimed_type` but their perspective arrays changed:")
            lines.append("")
            lines.append("| Constraint | Claimed Type | Old Perspectives | New Perspectives |")
            lines.append("|------------|-------------|------------------|------------------|")

            for c in silent[:50]:  # cap at 50 for readability
                old_p = ', '.join(f"{k}:{v}" for k, v in sorted(c['old_perspective_types'].items()))
                new_p = ', '.join(f"{k}:{v}" for k, v in sorted(c['new_perspective_types'].items()))
                lines.append(f"| {c['constraint_id']} | {c['new_claimed_type']} | {old_p} | {new_p} |")

            if len(silent) > 50:
                lines.append(f"| ... | ... | ... | ... |")
                lines.append(f"")
                lines.append(f"*({len(silent) - 50} additional silent reclassifications omitted)*")

            lines.append("")

        return lines

    def _section_metric_drift(self):
        """Section 4: Metric Drift."""
        drifts = self.results['metric_drift']
        lines = []
        lines.append("## 4. Metric Drift")
        lines.append("")
        lines.append(f"**{len(drifts)} constraints** had extractiveness or suppression shift >{METRIC_DRIFT_THRESHOLD}.")
        lines.append("")

        if drifts:
            lines.append("| Constraint | Domain | Old Type | New Type | Ext Delta | Sup Delta |")
            lines.append("|------------|--------|----------|----------|-----------|-----------|")

            for d in drifts[:50]:
                ext_d = f"{d['ext_delta']:+.2f}" if d['ext_delta'] is not None else "N/A"
                sup_d = f"{d['sup_delta']:+.2f}" if d['sup_delta'] is not None else "N/A"
                lines.append(f"| {d['constraint_id']} | {d['domain']} | {d['old_type']} | {d['new_type']} | {ext_d} | {sup_d} |")

            if len(drifts) > 50:
                lines.append(f"| ... | ... | ... | ... | ... | ... |")
                lines.append(f"")
                lines.append(f"*({len(drifts) - 50} additional metric drift entries omitted)*")

            lines.append("")

        return lines

    def _section_migration_matrix(self):
        """Section 5: Migration Matrix."""
        matrix = self.results['migration_matrix']
        lines = []
        lines.append("## 5. Migration Matrix")
        lines.append("")
        lines.append("Rows = old type, Columns = new type. Cells = number of constraints taking that path.")
        lines.append("")

        # Collect all types
        old_types = sorted(set(k[0] for k in matrix.keys()))
        new_types = sorted(set(k[1] for k in matrix.keys()))

        # Header
        header = "| Old \\ New | " + " | ".join(new_types) + " | **Total** |"
        separator = "|-----------|" + "|".join("-" * (max(len(t), 5) + 2) for t in new_types) + "|-----------|"
        lines.append(header)
        lines.append(separator)

        for ot in old_types:
            row_total = sum(matrix.get((ot, nt), 0) for nt in new_types)
            cells = " | ".join(
                str(matrix.get((ot, nt), 0)) if matrix.get((ot, nt), 0) > 0 else "."
                for nt in new_types
            )
            lines.append(f"| **{ot}** | {cells} | **{row_total}** |")

        # Column totals
        col_totals = [sum(matrix.get((ot, nt), 0) for ot in old_types) for nt in new_types]
        grand_total = sum(col_totals)
        totals_row = " | ".join(f"**{t}**" for t in col_totals)
        lines.append(f"| **Total** | {totals_row} | **{grand_total}** |")
        lines.append("")

        return lines


def load_old_corpus(commit):
    """Extract corpus_data.json from a git commit."""
    # Write to temp file to avoid pipe buffering issues
    import tempfile
    tmp = tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False)
    tmp_path = tmp.name
    tmp.close()

    result = subprocess.run(
        ['git', 'show', f'{commit}:outputs/corpus_data.json'],
        capture_output=True,
        cwd=str(PROJECT_ROOT),
    )

    if result.returncode != 0:
        print(f"ERROR: Failed to extract corpus_data.json from commit {commit}")
        print(result.stderr.decode())
        sys.exit(1)

    raw = result.stdout
    if not raw:
        print(f"ERROR: corpus_data.json is empty at commit {commit}")
        sys.exit(1)

    data = json.loads(raw)
    if 'constraints' in data:
        return data['constraints']
    return data


def load_current_corpus():
    """Load current corpus_data.json from disk."""
    with open(CORPUS_PATH, 'r') as f:
        data = json.load(f)
    if 'constraints' in data:
        return data['constraints']
    return data


def main():
    print(f"Comparison Stress Test: {BASELINE_COMMIT} vs HEAD")
    print(f"=" * 55)
    print()

    # Load both snapshots
    print(f"Loading baseline from commit {BASELINE_COMMIT}...")
    old_data = load_old_corpus(BASELINE_COMMIT)
    print(f"  -> {len(old_data)} constraints")

    print(f"Loading current corpus from {CORPUS_PATH}...")
    new_data = load_current_corpus()
    print(f"  -> {len(new_data)} constraints")
    print()

    # Run comparison
    engine = ComparisonEngine(old_data, new_data)
    results = engine.run()

    # Print stdout summary
    s = results['summary']
    print(f"Shared constraints: {s['shared']}")
    print(f"Added:   {s['added']}")
    print(f"Removed: {s['removed']}")
    print(f"Type changed:   {s['changed']}")
    print(f"Type unchanged: {s['unchanged']}")
    print()

    print("Old type distribution:")
    for t, c in s['old_type_dist'].most_common():
        print(f"  {t:20s} {c:4d}")
    print()

    print("New type distribution:")
    for t, c in s['new_type_dist'].most_common():
        print(f"  {t:20s} {c:4d}")
    print()

    print(f"Migration paths: {len(results['type_changes'])}")
    print(f"Perspective changes: {len(results['classification_changes'])}")
    print(f"Metric drifts (>{METRIC_DRIFT_THRESHOLD}): {len(results['metric_drift'])}")
    print()

    # Generate report
    report = ReportGenerator(results, BASELINE_COMMIT, 'HEAD')
    markdown = report.generate()

    REPORT_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(REPORT_PATH, 'w') as f:
        f.write(markdown)

    print(f"Report written to {REPORT_PATH}")


if __name__ == '__main__':
    main()
