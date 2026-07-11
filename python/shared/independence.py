"""OQ-186 common-cause discriminator — single canonical implementation.

Consumers: enhanced_report.py (Independence column in the contamination-network
neighbor table + legend), tensions_ledger.py (edge-line marker),
evaluative_convergence.py (members_common_cause_clique evidence boolean).

The discriminator (pre-registered: audits/2026-07-11_oq186_oq188_readsite/
PREREG.md Block 4, commit 57159a36; A/B probe outcome (a) — the network machinery
forms identical-looking edges for co-authored slices and for genuinely distinct
constraints, so independence must be carried at the read site): two constraints
are a COMMON-CAUSE pair iff they share >=1 beneficiary AND >=1 victim AND their
base_extractiveness differ by <= EPS_MARGIN. Such a pair is consistent with
co-authored slices of one underlying fact — convergence across the pair is
re-description, not independent corroboration.

EPS_MARGIN = 0.02 is AUTHORED (owned as such in PREREG): chosen below the
smallest eps rail spacing (0.04, the .x8/.x2 grid) so distinct rail values never
read as near-duplicates; not derived. The Phase-1 discrimination census KEPT the
clause (9/21 = 42.9% of non-both-sides live agent-edge pairs inside the margin —
not a majority).

Domain: corpus-derived agent edges (shared_beneficiary / shared_victim).
Authored 'explicit' edges and inferred_coupling are out of domain — an asserted
link is the story's own claim, not corpus topology. Callers render out-of-domain
and not-computable as 'n/a', never as a silent False (Pattern 6).
"""

EPS_MARGIN = 0.02
AGENT_EDGE_TYPES = ("shared_beneficiary", "shared_victim")


def is_common_cause_pair(entry_a, entry_b, eps_margin=EPS_MARGIN):
    """True iff the two per_constraint entries form a common-cause pair.
    None (not False) when not computable: either entry missing, or either
    base_extractiveness null — callers must surface 'n/a', never treat as
    'checked and independent'."""
    if not entry_a or not entry_b:
        return None
    eps_a = entry_a.get("base_extractiveness")
    eps_b = entry_b.get("base_extractiveness")
    if eps_a is None or eps_b is None:
        return None
    bens_a = set(entry_a.get("beneficiaries") or [])
    bens_b = set(entry_b.get("beneficiaries") or [])
    vics_a = set(entry_a.get("victims") or [])
    vics_b = set(entry_b.get("victims") or [])
    return bool(bens_a & bens_b) and bool(vics_a & vics_b) \
        and abs(eps_a - eps_b) <= eps_margin
