"""DR-Audit Studio — Streamlit wrapper around DRAuditOrchestrator.

Usage:
    streamlit run agent/app.py
"""

import json
import os
import re
import streamlit as st
import sys
from pathlib import Path

# Bridge API key from Streamlit secrets to env before orchestrator import
if "GEMINI_API_KEY" in st.secrets:
    os.environ["GEMINI_API_KEY"] = st.secrets["GEMINI_API_KEY"]
elif "GOOGLE_API_KEY" in st.secrets:
    os.environ["GOOGLE_API_KEY"] = st.secrets["GOOGLE_API_KEY"]

# This adds "/mount/src/structural_dynamics_model" to your path
root_path = Path(__file__).resolve().parent.parent
if str(root_path) not in sys.path:
    sys.path.insert(0, str(root_path))

from agent.orchestrator import DRAuditOrchestrator, PipelineResult  # noqa: E402

st.set_page_config(page_title="Deferential Realism: Process Illustrator", layout="wide")
st.title("Deferential Realism: Process Illustrator")

# ---------------------------------------------------------------------------
# Sidebar controls
# ---------------------------------------------------------------------------
with st.sidebar:
    st.header("Pipeline Settings")
    axes_count = st.number_input("Axes to select", min_value=1, max_value=6, value=3)
    skip_search = st.checkbox("Skip search grounding")
    skip_corpus = True  # Corpus pipeline runs locally only
#    st.info("Corpus pipeline runs via CLI (`python3 python/run_pipeline.py`). "
#            "Per-constraint reports still run here.")
    skip_essay = st.checkbox("Skip essay synthesis")
    dry_run = st.checkbox("Dry run (SCOPE only)")

# ---------------------------------------------------------------------------
# Main input
# ---------------------------------------------------------------------------
topic = st.text_area("Paste research or scenario idea:", height=200)

if st.button("Run DR Audit", type="primary"):
    if not topic.strip():
        st.warning("Please provide an input before running the audit.")
        st.stop()

    # Progress callback wired to st.status
    status_container = st.status("Starting pipeline...", expanded=True)

    def progress_cb(step, message):
        status_container.update(label=f"{step}: {message}")
        status_container.write(f"**{step}**: {message}")

    orch = DRAuditOrchestrator(
        axes=axes_count,
        skip_corpus_update=skip_corpus,
        skip_search=skip_search,
        skip_essay=skip_essay,
        dry_run=dry_run,
        progress_callback=progress_cb,
    )

    result = orch.run(topic.strip())
    status_container.update(label="Pipeline complete", state="complete")

    # -------------------------------------------------------------------
    # SCOPE Manifest
    # -------------------------------------------------------------------
    if result.scope_manifest:
        st.divider()
        st.subheader("SCOPE Manifest")

        manifest = result.scope_manifest
        st.markdown(f"**Domain:** {manifest.get('domain', 'N/A')}")
        st.markdown(f"**Family:** `{manifest.get('family_id', 'N/A')}`")

        if manifest.get("topic_summary"):
            st.markdown(f"**Summary:** {manifest['topic_summary']}")

        # Selected axes table
        selected = [a for a in manifest.get("axes", []) if a.get("selected")]
        if selected:
            st.markdown("**Selected axes:**")
            for ax in selected:
                label = ax.get("human_readable", ax["claim_id"])
                st.markdown(
                    f"- **{label}** — {ax['hypothesis']} "
                    f"(ε={ax['epsilon_bin']}) — {ax['structural_delta']}"
                )

        with st.expander("Full manifest JSON"):
            st.json(manifest)

    # -------------------------------------------------------------------
    # Enhanced Reports
    # -------------------------------------------------------------------
    if result.report_paths:
        st.divider()
        st.subheader("Enhanced Reports")

        for rpath in result.report_paths:
            try:
                text = rpath.read_text(encoding="utf-8")
            except Exception:
                continue

            # Prefer JSON sidecar for verdict; fall back to regex on markdown
            label = rpath.stem.replace("_report", "")
            sidecar_path = rpath.with_suffix(".json")
            verdict = ""
            if sidecar_path.exists():
                try:
                    sidecar = json.loads(sidecar_path.read_text(encoding="utf-8"))
                    verdict = sidecar.get("verdict", "")
                except (json.JSONDecodeError, OSError):
                    pass
            if not verdict:
                verdict_match = re.search(r"VERDICT:\s*(.+)", text)
                verdict = verdict_match.group(1).strip() if verdict_match else ""

            if "GREEN" in verdict.upper():
                st.success(f"{label}: {verdict}")
            elif "YELLOW" in verdict.upper():
                st.warning(f"{label}: {verdict}")
            elif "RED" in verdict.upper():
                st.error(f"{label}: {verdict}")

            with st.expander(f"Report: {label}"):
                st.markdown(text)

    # -------------------------------------------------------------------
    # Iteration Summary
    # -------------------------------------------------------------------
    iterate_step = next((s for s in result.steps if s.step == "iterate"), None)
    if iterate_step and iterate_step.data:
        stats = iterate_step.data.get("iteration_stats", {})
        iterated = {cid: v for cid, v in stats.items() if v["iterations"] > 0}
        if iterated:
            st.divider()
            st.subheader("Verdict Iteration")
            for cid, info in iterated.items():
                verdict = info["final_verdict"]
                iters = info["iterations"]
                tokens = info["tokens_in"] + info["tokens_out"]
                if verdict == "GREEN":
                    st.success(f"{cid}: {verdict} after {iters} iteration(s) ({tokens:,} tokens)")
                elif verdict == "YELLOW":
                    st.warning(f"{cid}: {verdict} after {iters} iteration(s) ({tokens:,} tokens)")
                else:
                    st.error(f"{cid}: {verdict} after {iters} iteration(s) ({tokens:,} tokens)")
            total_iter_tokens = iterate_step.tokens_in + iterate_step.tokens_out
            st.caption(f"Total iteration cost: {total_iter_tokens:,} tokens in {iterate_step.duration_s:.1f}s")

    # -------------------------------------------------------------------
    # Essay
    # -------------------------------------------------------------------
    if result.essay:
        st.divider()
        st.subheader("Essay")
        st.markdown(result.essay)

    # -------------------------------------------------------------------
    # Sidebar: token usage
    # -------------------------------------------------------------------
    with st.sidebar:
        st.divider()
        st.subheader("Token Usage")
        st.metric("Tokens In", f"{result.total_tokens_in:,}")
        st.metric("Tokens Out", f"{result.total_tokens_out:,}")
        st.metric("Duration", f"{result.total_duration_s:.1f}s")

        st.subheader("Step Details")
        for s in result.steps:
            status_icon = {"success": "OK", "error": "ERR", "skipped": "SKIP"}.get(s.status, "?")
            st.text(f"{s.step}: {status_icon} ({s.duration_s:.1f}s)")
