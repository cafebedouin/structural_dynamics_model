"""Omega reporter — query and JSON functions."""


def _format_source_gap(gap):
    """Format gap dict to Prolog-style source_gap string."""
    gap_type = gap.get('gap_type', 'unknown')
    powerless = gap.get('powerless_type', 'unknown')
    institutional = gap.get('institutional_type', 'unknown')
    return f"gap({gap_type},{powerless},{institutional})"


def extract_omegas(constraints):
    """Build omega-centric records from per-constraint data."""
    omegas = []
    for c in constraints:
        gaps = c.get('gaps') or []
        for omega in (c.get('omegas') or []):
            record = {
                'name': omega['id'],
                'severity': omega.get('severity') or 'N/A',
                'associated_constraint': c['id'],
                'source_gap': _format_source_gap(gaps[0]) if gaps else 'N/A',
                'question': omega.get('question') or 'N/A',
                'resolution_strategy': c.get('resolution_strategy') or 'N/A',
            }
            omegas.append(record)
    return omegas


def dedup_omegas(omegas):
    """Remove duplicate omegas by name, keeping first occurrence."""
    seen = set()
    unique = []
    for o in omegas:
        if o['name'] not in seen:
            seen.add(o['name'])
            unique.append(o)
    return unique


def query(data: dict) -> dict:
    """Pipeline data -> template context for omega report."""
    pipeline = data["pipeline"]
    constraints = pipeline['per_constraint']

    omega_data = extract_omegas(constraints)
    omega_data = dedup_omegas(omega_data)

    sorted_omegas = sorted(omega_data, key=lambda x: (x['severity'] != 'critical', x['name']))

    return {
        "sorted_omegas": sorted_omegas,
        "total": len(sorted_omegas),
    }


def json_fn(data: dict):
    """Pipeline data -> JSON-serializable omega list."""
    pipeline = data["pipeline"]
    constraints = pipeline['per_constraint']

    omega_data = extract_omegas(constraints)
    omega_data = dedup_omegas(omega_data)

    return omega_data
