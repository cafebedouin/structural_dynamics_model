"""Report registry — definitions, data loading, and execution engine."""

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable

import jinja2

from shared.loader import (
    load_json, read_config, PIPELINE_JSON, CORPUS_JSON, ORBIT_JSON,
    ENRICHED_PIPELINE_JSON, OUTPUT_DIR,
)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

TEMPLATES_DIR = Path(__file__).resolve().parent / "templates"

# ---------------------------------------------------------------------------
# Sentinel for distinguishing "not provided" from None
# ---------------------------------------------------------------------------

_SENTINEL = object()

# ---------------------------------------------------------------------------
# Data sources
# ---------------------------------------------------------------------------

DATA_SOURCES = {
    "pipeline": lambda: load_json(PIPELINE_JSON, label="pipeline"),
    "corpus": lambda: load_json(CORPUS_JSON, label="corpus"),
    "orbit": lambda: load_json(ORBIT_JSON, label="orbit"),
    "enriched": lambda: load_json(ENRICHED_PIPELINE_JSON, label="enriched"),
    "fingerprint": lambda: load_json(OUTPUT_DIR / "fingerprint_data.json", label="fingerprint"),
    "false_mountain": lambda: load_json(OUTPUT_DIR / "false_mountain_data.json", label="false_mountain"),
    "config": lambda: read_config(),
    "omega": lambda: load_json(OUTPUT_DIR / "omega_data.json", label="omega"),
}

# ---------------------------------------------------------------------------
# ReportDefinition
# ---------------------------------------------------------------------------

@dataclass
class ReportDefinition:
    name: str                                          # e.g. "variance_analysis"
    query_fn: Callable[[dict], dict]                   # loaded data -> template context
    template: str | None                               # template filename (None = no markdown)
    output_path: Path | None                           # markdown output (None = stdout)
    json_output_path: Path | None = None               # optional JSON sidecar
    json_fn: Callable[[dict], Any] | None = None       # data -> JSON-serializable
    data_sources: list[str] = field(default_factory=lambda: ["pipeline"])

# ---------------------------------------------------------------------------
# Jinja2 environment
# ---------------------------------------------------------------------------

_env = jinja2.Environment(
    loader=jinja2.FileSystemLoader(str(TEMPLATES_DIR)),
    keep_trailing_newline=True,
    trim_blocks=False,
    lstrip_blocks=False,
)

# ---------------------------------------------------------------------------
# run_report()
# ---------------------------------------------------------------------------

def run_report(
    report: ReportDefinition,
    *,
    data_override: dict | None = None,
    output_override: str | Path | None = _SENTINEL,
    json_output_override: str | Path | None = _SENTINEL,
) -> bool:
    """Execute a single report definition.

    Returns True on success, False on failure.
    """
    # 1. Load data sources (or use override)
    if data_override is not None:
        data = data_override
    else:
        data = {}
        for source_name in report.data_sources:
            loader = DATA_SOURCES.get(source_name)
            if loader is None:
                print(f"Unknown data source: {source_name}")
                return False
            data[source_name] = loader()
            if not data[source_name]:
                print(f"Warning: {source_name} data is empty")

    # 2. Call query_fn -> context dict
    try:
        context = report.query_fn(data)
    except Exception as e:
        print(f"Error in query for {report.name}: {e}")
        return False

    # 3. Determine output paths (allow overrides)
    md_path = report.output_path if output_override is _SENTINEL else output_override
    json_path = report.json_output_path if json_output_override is _SENTINEL else json_output_override

    # 4. Render template if present
    if report.template is not None:
        try:
            tmpl = _env.get_template(report.template)
            rendered = tmpl.render(**context)
        except Exception as e:
            print(f"Error rendering template for {report.name}: {e}")
            return False

        if md_path is not None:
            md_path = Path(md_path)
            md_path.parent.mkdir(parents=True, exist_ok=True)
            with open(md_path, "w", encoding="utf-8") as f:
                f.write(rendered)
        else:
            print(rendered, end="")

    # 5. Write JSON sidecar if json_fn is defined
    if report.json_fn is not None:
        try:
            json_data = report.json_fn(data)
        except Exception as e:
            print(f"Error in json_fn for {report.name}: {e}")
            return False

        if json_path is not None:
            json_path = Path(json_path)
            json_path.parent.mkdir(parents=True, exist_ok=True)
            with open(json_path, "w", encoding="utf-8") as f:
                json.dump(json_data, f, indent=2)
        else:
            print(json.dumps(json_data, indent=2))

    return True

# ---------------------------------------------------------------------------
# Report registrations (lazy imports to avoid circular deps)
# ---------------------------------------------------------------------------

def _build_reports() -> dict[str, ReportDefinition]:
    from reports.queries.red_spot_check import query as rsc_query
    from reports.queries.omega_reporter import query as omega_query, json_fn as omega_json
    from reports.queries.corpus_profile import query as cp_query, json_fn as cp_json
    from reports.queries.variance_analysis import query as va_query
    from reports.queries.pattern_mining import query as pm_query
    from reports.queries.sufficiency_test import query as st_query, json_fn as st_json
    from reports.queries.conflict_map import query as cm_query
    from reports.queries.reform_threshold_report import query as rtr_query
    from reports.queries.powerless_blind_diagnostic import query as pbd_query
    from reports.queries.classification_audit import query as ca_query
    from reports.queries.omega_enricher import query as oe_query, json_fn as oe_json
    from reports.queries.institutional_dissent import query as id_query, json_fn as id_json
    from reports.queries.meta_reporter import query as mr_query
    from reports.queries.type_reporter import query as tr_query

    defs = [
        ReportDefinition(
            name="red_spot_check",
            query_fn=rsc_query,
            template="red_spot_check.md.j2",
            output_path=None,  # stdout
            data_sources=["pipeline"],
        ),
        ReportDefinition(
            name="corpus_profile",
            query_fn=cp_query,
            template=None,  # JSON-only
            output_path=None,
            json_output_path=OUTPUT_DIR / "corpus_profile.json",
            json_fn=cp_json,
            data_sources=["pipeline"],
        ),
        ReportDefinition(
            name="omega_report",
            query_fn=omega_query,
            template="omega_report.md.j2",
            output_path=OUTPUT_DIR / "omega_report.md",
            json_output_path=OUTPUT_DIR / "omega_data.json",
            json_fn=omega_json,
            data_sources=["pipeline"],
        ),
        ReportDefinition(
            name="variance_analysis",
            query_fn=va_query,
            template="variance_analysis.md.j2",
            output_path=OUTPUT_DIR / "variance_analysis.md",
            data_sources=["corpus"],
        ),
        ReportDefinition(
            name="pattern_mining",
            query_fn=pm_query,
            template="pattern_mining.md.j2",
            output_path=OUTPUT_DIR / "pattern_mining.md",
            data_sources=["corpus"],
        ),
        ReportDefinition(
            name="sufficiency_test",
            query_fn=st_query,
            template="sufficiency_test.md.j2",
            output_path=OUTPUT_DIR / "index_sufficiency.md",
            json_output_path=OUTPUT_DIR / "index_sufficiency.json",
            json_fn=st_json,
            data_sources=["corpus", "pipeline"],
        ),
        # Group 4 reports (fingerprint/false_mountain JSON sidecars)
        ReportDefinition(
            name="conflict_map",
            query_fn=cm_query,
            template="conflict_map.md.j2",
            output_path=OUTPUT_DIR / "conflict_map.md",
            data_sources=["fingerprint", "corpus"],
        ),
        ReportDefinition(
            name="reform_threshold_report",
            query_fn=rtr_query,
            template="reform_threshold_report.md.j2",
            output_path=None,  # stdout
            data_sources=["fingerprint", "corpus", "config"],
        ),
        ReportDefinition(
            name="powerless_blind_diagnostic",
            query_fn=pbd_query,
            template="powerless_blind_diagnostic.md.j2",
            output_path=None,  # stdout
            data_sources=["fingerprint", "corpus", "config"],
        ),
        ReportDefinition(
            name="classification_audit",
            query_fn=ca_query,
            template="classification_audit.md.j2",
            output_path=OUTPUT_DIR / "classification_audit_report.md",
            data_sources=["false_mountain", "corpus", "config"],
        ),
        # Group 6 reports (complex reporters)
        ReportDefinition(
            name="omega_enricher",
            query_fn=oe_query,
            template="enriched_omega_report.md.j2",
            output_path=OUTPUT_DIR / "enriched_omega_report.md",
            json_output_path=OUTPUT_DIR / "enriched_omega_data.json",
            json_fn=oe_json,
            data_sources=["omega", "corpus", "orbit"],
        ),
        ReportDefinition(
            name="institutional_dissent",
            query_fn=id_query,
            template="institutional_dissent.md.j2",
            output_path=OUTPUT_DIR / "institutional_dissent_report.md",
            json_output_path=OUTPUT_DIR / "institutional_dissent_data.json",
            json_fn=id_json,
            data_sources=["enriched", "corpus", "orbit"],
        ),
        ReportDefinition(
            name="meta_report",
            query_fn=mr_query,
            template="meta_report.txt.j2",
            output_path=None,  # stdout — run_pipeline.py captures via redirect_stdout
            data_sources=["pipeline", "orbit"],
        ),
        ReportDefinition(
            name="type_report",
            query_fn=tr_query,
            template=None,    # query handles all file writing internally
            output_path=None,
            data_sources=["pipeline", "orbit"],
        ),
    ]
    return {d.name: d for d in defs}


# Module-level REPORTS dict (populated on first access)
class _ReportsProxy(dict):
    """Lazy-loading dict that builds on first access."""
    _loaded = False

    def _ensure(self):
        if not self._loaded:
            self.update(_build_reports())
            self._loaded = True

    def __getitem__(self, key):
        self._ensure()
        return super().__getitem__(key)

    def __contains__(self, key):
        self._ensure()
        return super().__contains__(key)

    def __iter__(self):
        self._ensure()
        return super().__iter__()

    def keys(self):
        self._ensure()
        return super().keys()

    def values(self):
        self._ensure()
        return super().values()

    def items(self):
        self._ensure()
        return super().items()

    def __len__(self):
        self._ensure()
        return super().__len__()


REPORTS = _ReportsProxy()
