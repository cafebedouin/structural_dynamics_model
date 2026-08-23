% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__consensus_safeguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__consensus_safeguard_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: supermajority_threshold__consensus_safeguard_reading
 *   human_readable: Amendment Supermajority Threshold — Deep-Consensus Filter Reading
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   Under the consensus-safeguard reading, the supermajority amendment
 *   threshold (of the Article V type: concurrent supermajorities in proposing
 *   chambers plus a broad subnational ratification requirement) is a standing
 *   quality filter on constitutional change: it holds every proposed revision
 *   until support for it is broad enough and persistent enough to count as
 *   deep democratic consensus rather than transient majoritarian passion. The
 *   arrangement solves a credible-commitment problem — letting a polity bind
 *   itself across time — and its costs are opportunity costs (forgone
 *   adaptability) spread across everyone rather than concentrated on anyone.
 *   This story instantiates one reading of the amendment-threshold kernel;
 *   see kernel_context for the reading structure. The claim/metric gap is
 *   deliberate: the constraint is CLAIMED as rope (pure coordination with
 *   diffuse beneficiaries) while the authored metrics describe
 *   low-but-nonzero extraction drifting slowly upward across the interval —
 *   the engine measures that divergence; do not reconcile the claim to the
 *   metrics.
 *
 * KEY AGENTS:
 *   - national_legislature: agenda-setting seat (institutional/constrained) — proposes amendments, records the votes, cannot relax its own gate
 *   - state_ratifying_bodies: agenda-setting seat with beneficiary position (institutional/constrained) — ratify or deny; decisive blocking power resides here
 *   - political_minorities: primary beneficiary (organized/constrained) — charter protections shielded from transient revision
 *   - future_citizen_generations: beneficiary with no seat (powerless/trapped) — inherit whatever is locked in
 *   - ordinary_citizens: dual-positioned seat (moderate/constrained) — stability gained, adaptability paid
 *   - persistent_majorities_seeking_change: excluded seat (powerful/trapped) — sustained support that cannot convert into ratification
 *   - constitutional_scholars_and_comparativists: analytical observer (analytical/analytical) — measures entrenchment cross-nationally
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__consensus_safeguard_reading, 0.32).
domain_priors:suppression_score(supermajority_threshold__consensus_safeguard_reading, 0.28).
domain_priors:theater_ratio(supermajority_threshold__consensus_safeguard_reading, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(supermajority_threshold__consensus_safeguard_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__consensus_safeguard_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__consensus_safeguard_reading, "Amendment Supermajority Threshold — Deep-Consensus Filter Reading").
narrative_ontology:topic_domain(supermajority_threshold__consensus_safeguard_reading, "constitutional_theory/political_economy/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__consensus_safeguard_reading, '191b05cb-e40b-4521-88b5-6ec597832ea0').
narrative_ontology:cs_kernel_codification('191b05cb-e40b-4521-88b5-6ec597832ea0', formalized).
narrative_ontology:cs_authority_grounding('191b05cb-e40b-4521-88b5-6ec597832ea0', lineage).
narrative_ontology:cs_interpretation_layer_present('191b05cb-e40b-4521-88b5-6ec597832ea0').
narrative_ontology:cs_reading_relation('191b05cb-e40b-4521-88b5-6ec597832ea0', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_reading_relation('191b05cb-e40b-4521-88b5-6ec597832ea0', supermajority_threshold__adaptive_gradient_reading, influences).
narrative_ontology:cs_axiom('191b05cb-e40b-4521-88b5-6ec597832ea0', foundational, transient_majorities_lack_constitutional_authority).
narrative_ontology:cs_axiom_status(transient_majorities_lack_constitutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('191b05cb-e40b-4521-88b5-6ec597832ea0', transient_majorities_lack_constitutional_authority, deontological).
narrative_ontology:cs_axiom('191b05cb-e40b-4521-88b5-6ec597832ea0', foundational, supermajority_barrier_filters_passion_from_consensus).
narrative_ontology:cs_axiom_status(supermajority_barrier_filters_passion_from_consensus, holdable).
narrative_ontology:cs_axiom_grounding('191b05cb-e40b-4521-88b5-6ec597832ea0', supermajority_barrier_filters_passion_from_consensus, empirically_contingent).
narrative_ontology:cs_reference_frame('191b05cb-e40b-4521-88b5-6ec597832ea0', deep_consensus_quality_filter).
narrative_ontology:cs_drift_state('191b05cb-e40b-4521-88b5-6ec597832ea0', contemporary_polarization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('191b05cb-e40b-4521-88b5-6ec597832ea0', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, political_minorities).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, future_citizen_generations).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, ordinary_citizens).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, constitutional_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__consensus_safeguard_reading, state_ratifying_bodies).
narrative_ontology:constraint_victim(supermajority_threshold__consensus_safeguard_reading, ordinary_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proposes constitutional amendments and controls whether proposals reach the ratification stage; its chambers record the votes that must clear the supermajority line. It cannot waive or lower the threshold for its own convenience — altering the rule requires running the same gauntlet it administers. When proposals fall short, its members absorb the delay and the political cost of failed reform.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, national_legislature, agenda_setter,
    institutional, generational, constrained, national).

% Subnational legislatures must each affirm an amendment before it enters the charter; any bloc large enough to deny ratification holds decisive power over that proposal's fate. They receive the stability the rule produces and pay nothing to administer it beyond conducting their own voting procedures.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, state_ratifying_bodies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__consensus_safeguard_reading, state_ratifying_bodies, beneficiary).

% Groups whose legal, civil, and religious protections are written into the charter and would be exposed if bare majorities could rewrite fundamental law each session. They lobby, litigate, and organize, but cannot leave the charter's jurisdiction; their security depends on the difficulty of revision.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, political_minorities, beneficiary,
    organized, generational, constrained, national).

% People not yet born or not yet enfranchised who will inherit whichever charter the present leaves behind. They hold no vote in any ratifying body and no way to revise the terms they receive; whatever the rule locks in, they receive as settled fact.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, future_citizen_generations, beneficiary,
    powerless, generational, trapped, national).

% Experience the rule from both sides: they gain fundamental law that does not swing with each election cycle, and they pay in reduced responsiveness whenever they want the charter itself changed quickly. Leaving means emigration, which few can exercise; their leverage runs through elections that do not directly touch the amendment rule.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, ordinary_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(supermajority_threshold__consensus_safeguard_reading, ordinary_citizens, payer).

% Broad coalitions that sustain strong public support for a specific constitutional change across multiple years and election cycles yet cannot assemble the required supermajority inside the ratifying bodies. They hold no seat in the amendment forum beyond representatives who cannot deliver ratification alone; their realistic options are repeated failed attempts, litigation around the charter's edges, or acceptance.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, persistent_majorities_seeking_change, excluded,
    powerful, biographical, trapped, national).

% Study amendment difficulty across constitutions and eras, publish entrenchment indices, and testify in redesign debates. They bear none of the rule's costs and collect none of its benefits; their seat is analytical.
narrative_ontology:constraint_stakeholder(supermajority_threshold__consensus_safeguard_reading, constitutional_scholars_and_comparativists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__consensus_safeguard_reading, diffuse).
narrative_ontology:fixing_cost_class(supermajority_threshold__consensus_safeguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of protecting long-horizon constitutional commitments — rights guarantees, institutional design, sovereign creditworthiness — from short-horizon majoritarian revision, and provides a focal commitment device that lets a polity make durable promises to minorities, creditors, and future members.
% TRANSFER_FUNCTION: Moves constitutional-revision authority from sitting legislative majorities to broad cross-sectional supermajorities, and temporally from the present toward the inherited settlement; the payment side is forgone adaptability, borne by any coalition seeking rapid fundamental change.
% ABSENT_VOICES: Persistent majorities whose preferred amendments repeatedly stall below the line would object that the filter misclassifies their sustained support as transient passion; residents of jurisdictions with no ratifying vote bear the charter while holding no seat in its revision. Both stand outside the amendment forum, represented only indirectly.
% DISAPPEARANCE_RATIONALE: If the threshold vanished overnight, constitutional revision would follow simple-majority dynamics: fundamental law would churn with electoral cycles, long-term commitments to minorities and creditors would lose credibility, and institutional actors whose authority rests on continuity would face serial renegotiation of their own foundations.
% FOUNDING_PROBLEM: How can a republic make credible long-term commitments — protecting minorities, creditors, and institutional continuity — when bare majorities control ordinary law and could otherwise rewrite the rules in their own favor each session?
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties by comparative constitutional scholarship (cross-national amendment-difficulty datasets showing the rule's near-universal adoption for precisely this purpose), by the political-economy literature on credible commitment linking constitutional rigidity to sovereign credit and investor protection, and by founding-era ratification debates recorded in contemporaneous documents independent of any current beneficiary seat. Current beneficiary seats also attest the problem, but the corroborating sources named here stand outside that set.
narrative_ontology:disappearance_verdict(supermajority_threshold__consensus_safeguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__consensus_safeguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__consensus_safeguard_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__consensus_safeguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__consensus_safeguard_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__consensus_safeguard_reading_tests).
:- end_tests(supermajority_threshold__consensus_safeguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.32 at interval end) because the barrier's costs are diffuse opportunity costs rather than transfers to a capturing seat; suppression is moderate-low (0.28) because the rule gates procedurally at amendment moments only — ordinary-law alternatives remain fully available and no standing coercive apparatus enforces the gate between attempts. Theater is low (0.16): the rule genuinely blocks proposals; little activity is performative maintenance. Accessibility collapse is moderate-low (0.38) because statutory, judicial-interpretive, and compact-based routes around formal amendment remain open. Resistance is moderate (0.52): amendment movements and threshold-lowering campaigns actively contest the gate. The measurement series run on one shared time grid (points 0,4,8,12,16,20,24) with both tracked metrics authored at every point; the slow upward drift in both series reflects accumulating blocking episodes, not enforcement change. A suppression_requirement series is deliberately omitted: the enforcement picture is static across the interval (the gate is a counting rule, not a growing apparatus), so the scalar covers it per the static-enforcement rule.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes per-seat classifications from the structural data, and the seats diverge sharply. From the ratifying-body seats the rule reads as self-binding procedure they administer at negligible cost. From the political-minority and future-generation seats it reads as shelter. From the ordinary-citizen seat it reads as a near-even trade. From the persistent-majority seat — powerful, trapped, excluded from the forum — the same rule reads as costly obstruction of sustained, demonstrated support. Same structure, different experienced types; the divergence is computed, not authored.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for political_minorities, future_citizen_generations, and constitutional_institutions. Ordinary_citizens carry a dual declaration (beneficiary with payer secondary); because the derivation weights the primary role, an override sets the moderate power atom to 0.45 — near-symmetric — reflecting that stability gains roughly offset flexibility losses for this seat; no other stakeholder occupies the moderate atom. Persistent_majorities_seeking_change hold the excluded role with no beneficiary/victim declaration, so derivation would fall back to the canonical powerful-atom default; an override sets the powerful atom to 0.85 because, when blocked, this seat bears the arrangement's principal cost. National scope amplifies effective extraction modestly per the engine's scope handling; suppression remains unscaled by design.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — credible long-horizon commitment against short-horizon majorities — is live, so no mandatrophy is declared and the arrangement is not a scaffold (its justification is the steady state, not a transition; no sunset clause exists). The classification guards against two mislabels: reading episodic blocking as mandate death (blocking is the rule operating as designed, and the blocking_incident_victim_status omega tracks whether accumulation ever converts it into standing extraction), and reading the barrier as a natural feature (emerges_naturally is false — it is a constructed rule, however self-entrenched). The prohibitive fixing cost is structural self-entrenchment, not theatrical maintenance; theater_ratio stays low, which is what separates this profile from a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates the consensus_safeguard_reading of the supermajority_threshold kernel; what structural facts would change under the sibling readings (minoritarian_veto_reading, adaptive_gradient_reading), and where exactly is the disagreement located?',
    'Corpus-level comparison across the three sibling story files; no in-story resolution exists because each reading is a separate epsilon-invariant constraint. The disagreement is located in the causal characterization of the threshold''s operation: quality filter versus minority veto versus calibratable tool.',
    'Sibling readings re-declare the party structure: the veto reading adds blocked majorities as a victim set and scrutinizes status-quo-protected incumbents for gain capture; the gradient reading reframes threshold height as a tunable parameter evaluated against calibration evidence rather than principle. Extractiveness and claimed_type are reading-indexed and differ across the sibling files over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one kernel, three readings, three files.').

omega_variable(
    blocking_incident_victim_status,
    'When a coalition sustains supermajority-grade public support for a specific amendment across many years and election cycles yet remains blocked, does the blocked cohort constitute a victim set — shifting this arrangement toward hybrid coordination/extraction — or evidence that its support lacked the depth the filter exists to detect?',
    'Longitudinal opinion series matched to ratification margins: if support stays above the threshold-equivalent share of the public for a full ratification cycle and the amendment still fails, the blocked-cohort account gains force; if support oscillates around the line, the filtration account holds.',
    'Confirmation of a standing victim set shifts the computed type toward tangled_rope, raises effective extraction for the blocked seat, and places gain_flow under scrutiny regarding who benefits from the protected status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(blocking_incident_victim_status, empirical, 'Whether episodic blocking accumulates into a standing victim set.').

omega_variable(
    diffuse_beneficiary_attribution,
    'Are the declared beneficiaries (charter-sheltered minorities, future generations, the citizenry at large) the real net beneficiaries, or does the barrier primarily shield specific entrenched arrangements — apportionment formulas, indirect-election structures — whose present-day holders capture the gains?',
    'Distributional audit of which provisions the barrier has actually protected from revision attempts over the interval, and which actors occupy positions under those provisions.',
    'If capture is found, gain_flow moves from diffuse to a named seat, excess extraction rises above the coordination floor, and the classification trends from pure coordination toward captured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_beneficiary_attribution, conceptual, 'Whether diffuse-beneficiary framing conceals concentrated capture.').

omega_variable(
    filter_premise_empirical_warrant,
    'Does the barrier actually separate deep from transient consensus — do amendments that clear the threshold show durability and breadth-of-support advantages over ordinary statute, as this reading''s empirical axiom asserts?',
    'Cross-national panel linking amendment-difficulty indices to constitutional turnover, amendment repeal rates, and post-adoption support trajectories.',
    'Negative results strip the empirical axiom of warrant; the reading then survives only as deontological aspiration or migrates toward the calibration framing instantiated in the sibling file, with classification consequences riding there.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filter_premise_empirical_warrant, empirical, 'Empirical testability of the filtration premise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__consensus_safeguard_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(supe_tr_t4, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement(supe_tr_t8, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(supe_tr_t12, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(supe_tr_t16, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(supe_tr_t24, supermajority_threshold__consensus_safeguard_reading, theater_ratio, 24, 0.16).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(supe_be_t4, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 4, 0.21).
narrative_ontology:measurement(supe_be_t8, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(supe_be_t12, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(supe_be_t16, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 16, 0.29).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(supe_be_t24, supermajority_threshold__consensus_safeguard_reading, base_extractiveness, 24, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(supermajority_threshold__consensus_safeguard_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__consensus_safeguard_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__minoritarian_veto_reading).
narrative_ontology:affects_constraint(supermajority_threshold__consensus_safeguard_reading, supermajority_threshold__adaptive_gradient_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'supermajority threshold': one kernel (the amendment barrier as a standing arrangement), three readings, three files. This file instantiates the consensus_safeguard_reading, which authors low reading-indexed extractiveness over the shared referent (the standing amendment-barrier arrangement). The minoritarian_veto_reading file authors high extractiveness over the same referent with a declared victim set; the adaptive_gradient_reading file reframes threshold height as a tunable parameter. Each file is separately epsilon-invariant per DP-001; the readings differ in their indexed values, not in the referent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__consensus_safeguard_reading, moderate, 0.45).
constraint_indexing:directionality_override(supermajority_threshold__consensus_safeguard_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
