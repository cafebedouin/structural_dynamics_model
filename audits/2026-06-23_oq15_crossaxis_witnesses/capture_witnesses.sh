#!/usr/bin/env bash
# Phase 0a raw-witness capture for OQ-15 cross-axis surface.
# Read-only: greps + reads, asserts nothing, writes only this audit's raw log.
# Run from prolog/:  bash ../audits/2026-06-23_oq15_crossaxis_witnesses/capture_witnesses.sh
set -u
cd "$(dirname "$0")/../../prolog" || exit 1

echo "######## W1 — form of each cross-axis call site ########"
echo "==== cs_drift_mismatch.pl: transitive via cs_is_metric_stable -> network_dynamics ===="
grep -nE "cs_is_metric_stable|network_dynamics:detect_network_drift|network_drift_velocity" cs_drift_mismatch.pl
echo
echo "==== drl_composition.pl: detect_necessity_inheritance STATIC (dr_type + cs_reading_relation) ===="
grep -nE "detect_necessity_inheritance|drl_core:dr_type|cs_reading_relation" drl_composition.pl | head
echo
echo "==== cs_kernel_registry.pl: cs_kernel_divergence/compare_kernel_readings STATIC (classify_at_time) ===="
grep -nE "cs_kernel_divergence|compare_kernel_readings|classify_at_time" cs_kernel_registry.pl | head
echo
echo "==== drl_purity_network.pl: constraint_neighbors cs_kernel_id exclusion STATIC ===="
grep -nE "cs_kernel_id" drl_purity_network.pl

echo
echo "######## W2 — direction of detect_necessity_inheritance vs bucket-1 comparisons ########"
echo "detect_necessity_inheritance: committer->observer (influences edge -> entailment); bucket-1: observer->committer."
echo "(see W1 line-witnesses above)"

echo
echo "######## BC — back-channel (runtime assert) surface ########"
echo "==== cs_ modules asserting runtime facts (expect none) ===="
grep -nE "assert[az]" cs_*.pl | grep -vE "^\s*%|dynamic" || echo "  (none)"
echo "==== drl_ modules asserting runtime facts (expect only fpn_* caches) ===="
grep -nE "assert[az]" drl_*.pl | grep -vE "^\s*%|dynamic"

echo
echo "######## reverse-read claim — constraint_bridge.pl ########"
echo "==== ANY cs_ committer read in constraint_bridge.pl (expect none) ===="
grep -nE "\bcs_[a-z]" constraint_bridge.pl || echo "  (none — compute_veto_actors is observer-internal: dr_type + authored constraint_beneficiary)"

echo
echo "######## json_report.pl — both-axis aggregator (mediator-output role) ########"
grep -nE "drl_core:dr_type|cs_axiom_foreclosed|cs_kernel_divergence|cs_drift_mismatch|cs_reading_relation" json_report.pl | head
