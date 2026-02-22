"""DR-Audit Studio — Streamlit wrapper around DRAuditOrchestrator.

Usage:
    streamlit run agent/app.py
"""

import json
import os
import re

import streamlit as st

# Bridge API key from Streamlit secrets to env before orchestrator import
if "GEMINI_API_KEY" in st.secrets:
    os.environ["GEMINI_API_KEY"] = st.secrets["GEMINI_API_KEY"]
elif "GOOGLE_API_KEY" in st.secrets:
    os.environ["GOOGLE_API_KEY"] = st.secrets["GOOGLE_API_KEY"]

from orchestrator import DRAuditOrchestrator, PipelineResult  # noqa: E402

st.set_page_config(page_title="DR-Audit Studio", layout="wide")
st.title("DR-Audit Studio")

# ---------------------------------------------------------------------------
# Sidebar controls
# ---------------------------------------------------------------------------
with st.sidebar:
    st.header("Pipeline Settings")
    axes_count = st.number_input("Axes to select", min_value=1, max_value=6, value=3)
    skip_search = st.checkbox("Skip search grounding")
    skip_corpus = st.checkbox("Skip corpus update")
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

            # Parse verdict for color banner
            verdict_match = re.search(r"VERDICT:\s*(.+)", text)
            verdict = verdict_match.group(1).strip() if verdict_match else ""

            label = rpath.stem.replace("_report", "")
            if "GREEN" in verdict.upper():
                st.success(f"{label}: {verdict}")
            elif "YELLOW" in verdict.upper():
                st.warning(f"{label}: {verdict}")
            elif "RED" in verdict.upper():
                st.error(f"{label}: {verdict}")

            with st.expander(f"Report: {label}"):
                st.markdown(text)

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
