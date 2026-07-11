"""OQ-188 flip-under-role-change predicate — single canonical implementation.

Consumers: enhanced_report.py (glyph on per-seat type lines + legend sentence),
tensions_ledger.py (per-position glyph + header legend line),
evaluative_convergence.py (all_members_knife_edge evidence boolean).

The predicate (pre-registered: audits/2026-07-11_oq186_oq188_readsite/PREREG.md
Block 1, commit 57159a36; fire-rate census 98.1% of matched institutional seats
-> standing form): a seat FIRES iff its serialized perspective_chi d matches a
stakeholder_role_d_* constant (tolerance 1e-6, against the SERIALIZED config of
the same pipeline output — never a hardcoded copy) AND f(d) changes sign between
the authored role's d and the nearest alternative role constant. Zero free
parameters: the sigmoid, the role ladder, and hence the sign root all come from
the serialized config section.

Bucket semantics (Pattern 6 — absence never reads as "checked and robust"):
null d, canonical-fallback d (0.0), and unmatched d are OUT OF DOMAIN of the
predicate and simply do not fire; the fire/silent distinction is meaningful only
for matched seats. The flag is COMMENTARY-GRADE: it annotates a rendered verdict,
it never overrides classification (OQ-01 bypass untouched).
"""

import math

SEATS = ("powerless", "moderate", "institutional", "analytical")
GLYPH = "‡"  # ‡ — defined once; every render site imports it
_TOL = 1e-6
_SIGMOID_KEYS = ("sigmoid_lower", "sigmoid_upper", "sigmoid_midpoint",
                 "sigmoid_steepness")
_ROLE_PREFIX = "stakeholder_role_d_"


def _sigmoid_f(d, config):
    lower = config["sigmoid_lower"]
    upper = config["sigmoid_upper"]
    return lower + (upper - lower) / (
        1 + math.exp(-config["sigmoid_steepness"] * (d - config["sigmoid_midpoint"]))
    )


def seat_fires(d, config):
    """The predicate for one serialized d value. True iff d matches a role
    constant and f flips sign to the nearest alternative role constant.
    None (not False) when the predicate is out of domain: null d, canonical
    fallback (0.0), unmatched d, or config lacking the role/sigmoid params —
    callers must not collapse None into 'checked and robust'."""
    if d is None:
        return None
    roles = {k: v for k, v in config.items()
             if k.startswith(_ROLE_PREFIX) and isinstance(v, (int, float))}
    if len(roles) < 2 or any(not isinstance(config.get(k), (int, float))
                             for k in _SIGMOID_KEYS):
        return None
    if abs(d) <= _TOL:
        return None  # canonical fallback, no authored role selected this d
    match = next((k for k, rd in roles.items() if abs(d - rd) <= _TOL), None)
    if match is None:
        return None  # unmatched: d not on the role ladder
    alt_d = min((rd for k, rd in roles.items() if k != match),
                key=lambda rd: abs(rd - d))
    return (_sigmoid_f(d, config) > 0) != (_sigmoid_f(alt_d, config) > 0)


def role_flip_fired_seats(entry, config):
    """Seats of one per_constraint entry whose serialized perspective_chi d
    fires the predicate. Returns a frozenset of seat names; empty when nothing
    fires or nothing is computable (the standing legend sentence, not this
    return value, carries the 'this is computed from serialized config' fact)."""
    if not entry or not config:
        return frozenset()
    pchi = entry.get("perspective_chi") or {}
    return frozenset(
        seat for seat in SEATS
        if seat_fires((pchi.get(seat) or {}).get("d"), config) is True
    )
