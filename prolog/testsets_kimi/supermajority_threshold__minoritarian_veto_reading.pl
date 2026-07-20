% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold: Minoritarian Veto Reading
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the minoritarian_veto_reading of the
 *   contested supermajority_threshold kernel. Under this reading, a
 *   constitutional supermajority requirement for amendment functions not as a
 *   consensus safeguard but as a snare: it empowers blocking minorities to
 *   entrench the status quo against majoritarian will, converting historical
 *   procedural privilege into a permanent veto. The constraint is claimed as
 *   a snare; the metrics are authored independently to describe high
 *   extraction, high suppression, and significant theater as the consensus
 *   rationale decouples from actual operation.
 *
 * KEY AGENTS:
 *   - entrenched_elites: Primary beneficiary (powerful/mobile) â collect policy stability and blocked redistribution
 *   - status_quo_beneficiaries: Secondary beneficiary (moderate/constrained) â retain embedded legal advantages
 *   - blocking_minorities: Operational beneficiary (organized/mobile) â wield asymmetric veto power
 *   - contemporary_majorities: Primary target (organized/constrained) â bear extraction of blocked sovereignty
 *   - reform_movements: Secondary target (moderate/constrained) â expend resources against locked barrier
 *   - constitutional_arbiter: Institutional administrator (institutional/constrained) â legitimizes threshold
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.83).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.78).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.83).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold: Minoritarian Veto Reading").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, '55406fb7-e203-4f6d-8b20-71b5269d26de').
narrative_ontology:cs_kernel_codification('55406fb7-e203-4f6d-8b20-71b5269d26de', fixed_text).
narrative_ontology:cs_authority_grounding('55406fb7-e203-4f6d-8b20-71b5269d26de', lineage).
narrative_ontology:cs_interpretation_layer_present('55406fb7-e203-4f6d-8b20-71b5269d26de').
narrative_ontology:cs_reading_relation('55406fb7-e203-4f6d-8b20-71b5269d26de', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('55406fb7-e203-4f6d-8b20-71b5269d26de', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('55406fb7-e203-4f6d-8b20-71b5269d26de', foundational, supermajority_constitutes_minoritarian_veto).
narrative_ontology:cs_axiom_status(supermajority_constitutes_minoritarian_veto, holdable).
narrative_ontology:cs_axiom_grounding('55406fb7-e203-4f6d-8b20-71b5269d26de', supermajority_constitutes_minoritarian_veto, empirically_contingent).
narrative_ontology:cs_axiom('55406fb7-e203-4f6d-8b20-71b5269d26de', foundational, majority_sovereignty_principle).
narrative_ontology:cs_axiom_status(majority_sovereignty_principle, holdable).
narrative_ontology:cs_axiom_grounding('55406fb7-e203-4f6d-8b20-71b5269d26de', majority_sovereignty_principle, deontological).
narrative_ontology:cs_reference_frame('55406fb7-e203-4f6d-8b20-71b5269d26de', majoritarian_sovereignty).
narrative_ontology:cs_drift_state('55406fb7-e203-4f6d-8b20-71b5269d26de', contemporary_polarized_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('55406fb7-e203-4f6d-8b20-71b5269d26de', '').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, blocking_minorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold wealth and influence embedded in current constitutional and regulatory arrangements; the supermajority threshold prevents amendments that would redistribute power or resources away from them.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    powerful, generational, mobile, national).

% Specific sectors, regions, or demographic groups whose advantages are codified in existing constitutional structures and who would lose those advantages if majorities could amend more easily.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    moderate, biographical, constrained, national).

% Cohesive political factions that, by withholding consent, can prevent amendments from reaching the supermajority threshold; they gain disproportionate policy influence relative to their electoral size.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, blocking_minorities, beneficiary,
    organized, biographical, mobile, national).

% Electoral majorities that support constitutional reforms but cannot assemble the required supermajority; they experience unresponsive government and policy outcomes that diverge from their preferences.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, biographical, constrained, national).

% Organized campaigns advocating constitutional change; they expend political capital against a barrier that does not require comparable energy to maintain.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_movements, payer,
    moderate, biographical, constrained, national).

% Courts and constitutional officers that interpret amendment clauses, certify procedural compliance, and legitimate the threshold by embedding it in constitutional jurisprudence.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_arbiter, agenda_setter,
    institutional, generational, constrained, national).

% Academic observers who study amendment rates, democratic responsiveness, and institutional design across polities; they document the gap between majoritarian preference and constitutional output.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, comparative_democracy_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:fixing_cost_class(supermajority_threshold__minoritarian_veto_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates constitutional stability by requiring more than a simple majority to alter fundamental law, theoretically ensuring that amendments reflect broad and persistent agreement rather than transient electoral outcomes.
% TRANSFER_FUNCTION: Moves effective amendment sovereignty from contemporary electoral majorities to blocking minorities and entrenched status quo interests; transfers the fruits of policy stability to existing beneficiaries at the cost of democratic adaptability.
% ABSENT_VOICES: Future generations bound by unamendable structures; citizens in disenfranchised territories whose inclusion would alter majorities but who lack voice in the amendment calculus; comparative constitutional scholars from polities with flexible amendment rules who would note the anomaly.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold disappeared, electoral majorities could constitutionalize their preferences, entrenched policy advantages would become contestable by simple majority, and the current distribution of veto power would collapse; constitutional orders would reorganize around majoritarian amendment procedures.
% FOUNDING_PROBLEM: To prevent hasty, ill-considered constitutional change and ensure amendments enjoy deep, persistent democratic support before altering fundamental law.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional historians and democratic theorists outside the benefiting parties attest that while the threshold was historically justified as a deliberation mechanism, contemporary empirical analysis of amendment blockage and democratic backsliding supports the claim that the original problem no longer explains the arrangement's persistence.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.83, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.83) because the threshold systematically transfers amendment capacity from contemporary majorities to status quo defenders without proportional coordination benefit. Suppression (0.78) reflects the active structural exclusion of simple-majority alternatives and the delegitimation of majoritarian reform paths. Theater ratio (0.45) captures the growing decoupling between the consensus rhetoric used to justify the threshold and its actual function as a minority veto. Accessibility collapse (0.80) is high because once the threshold is understood, the alternative of simple majority amendment is structurally foreclosed. Resistance (0.70) reflects active majoritarian and reformist opposition to the barrier.
 *
 * PERSPECTIVAL GAP:
 *   The constitutional arbiter seat experiences the constraint as a neutral procedural rule embedded in constitutional text, with directionality near symmetric or mildly beneficiary because its authority depends on constitutional stability. The contemporary majorities and reform movements experience it as a locked gate with directionality near full target. The engine computes this divergence from the same structural data: the arbiter's institutional role and constrained exit differ from the majority's organized-but-blocked position.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched elites, status quo beneficiaries, and blocking minorities are structural beneficiaries of the veto (low d, subsidized by stability). Contemporary majorities and reform movements are structural targets (high d, extraction of sovereign capacity). The constitutional arbiter sits between, deriving institutional legitimacy from the same structure it administers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing hasty amendment â is treated as dead because the threshold now blocks even deeply considered, persistent majoritarian reform. Without the R5 genealogy, the constraint might be misclassified as a rope (consensus coordination) or tangled rope (coordination with some extraction). The R5 status=dead combined with disappearance_verdict=world_rearranges flags the mandatrophy and supports the snare classification: the arrangement persists not because its founding problem is live, but because identifiable beneficiaries capture its veto function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the supermajority threshold a genuine consensus safeguard, a functional tool requiring calibration, or a snare empowering blocking minorities?',
    'Comparative historical analysis of amendment success rates under varying thresholds, correlated with polarization metrics and elite privilege persistence.',
    'Resolution would determine whether this constraint belongs to the consensus_safeguard, adaptive_gradient, or minoritarian_veto reading â shifting its classification across the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of the supermajority kernel is structurally accurate').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the blockage of reform by supermajority thresholds sustained primarily by institutional structure or by internalized beliefs in constitutional sacredness?',
    'Public opinion research measuring willingness to amend amendment rules; observation of reform behavior in constitutional conventions.',
    'If internalized, effective suppression exceeds structural measure because majorities police their own constitutional bounds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    fixing_path_feasibility,
    'Can the supermajority threshold be reformed through existing constitutional mechanisms, or does its self-entrenchment require extra-constitutional action?',
    'Jurisprudential analysis of whether the threshold applies to its own revision; historical cases of threshold modification.',
    'If self-entrenching, the constraint''s resistance to change amplifies its extractiveness and confirms the snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fixing_path_feasibility, empirical, 'Whether the threshold is self-entrenching').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(supe_tr_t60, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 60, 0.37).
narrative_ontology:measurement(supe_tr_t80, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 80, 0.41).
narrative_ontology:measurement(supe_tr_t100, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 100, 0.45).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(supe_be_t60, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 60, 0.7).
narrative_ontology:measurement(supe_be_t80, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 80, 0.77).
narrative_ontology:measurement(supe_be_t100, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 100, 0.83).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(supe_su_t60, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(supe_su_t80, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(supe_su_t100, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
