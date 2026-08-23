% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Preparedness Persistence â Hybrid Reading
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_reading of the
 *   preparedness_persistence kernel. The natural-language concept
 *   'preparedness' conflates structurally distinct subsystems: engineering
 *   inspection retains live operational competence (mountain-like stability),
 *   while evacuation drills have atrophied into ritualized performance
 *   (piton-like inertia). The hybrid reading resolves this by stratifying the
 *   label: the constraint is neither uniformly functional nor uniformly
 *   hollow, but a mixed system where extraction is localized to the
 *   ritualized subsystems. Sibling readings â competence_reading (all live)
 *   and husk_reading (all atrophied) â are modeled as separate constraints
 *   linked in the network.
 *
 * KEY AGENTS:
 *   - emergency_management_agency: Agenda-setter (institutional/constrained) â maintains the bundled preparedness program
 *   - engineering_inspectors: Observer (organized/constrained) â conducts competent inspections that retain genuine safety function
 *   - drill_administrators: Observer (moderate/constrained) â manages ritualized evacuation rehearsals
 *   - legislative_oversight: Observer (institutional/constrained) â funds and audits the mixed program
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.42).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.35).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Preparedness Persistence â Hybrid Reading").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, '265d372e-2df5-42cb-bb61-a90889ed577b').
narrative_ontology:cs_kernel_codification('265d372e-2df5-42cb-bb61-a90889ed577b', implicit).
narrative_ontology:cs_authority_grounding('265d372e-2df5-42cb-bb61-a90889ed577b', practice).
narrative_ontology:cs_interpretation_layer_present('265d372e-2df5-42cb-bb61-a90889ed577b').
narrative_ontology:cs_reading_relation('265d372e-2df5-42cb-bb61-a90889ed577b', preparedness_persistence__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('265d372e-2df5-42cb-bb61-a90889ed577b', preparedness_persistence__husk_reading, coexists_with).
narrative_ontology:cs_axiom('265d372e-2df5-42cb-bb61-a90889ed577b', foundational, selective_maintenance_mandate).
narrative_ontology:cs_axiom_status(selective_maintenance_mandate, holdable).
narrative_ontology:cs_axiom_grounding('265d372e-2df5-42cb-bb61-a90889ed577b', selective_maintenance_mandate, instrumental).
narrative_ontology:cs_axiom('265d372e-2df5-42cb-bb61-a90889ed577b', secondary, subsystem_autonomy_principle).
narrative_ontology:cs_axiom_status(subsystem_autonomy_principle, holdable).
narrative_ontology:cs_axiom_grounding('265d372e-2df5-42cb-bb61-a90889ed577b', subsystem_autonomy_principle, conventional).
narrative_ontology:cs_reference_frame('265d372e-2df5-42cb-bb61-a90889ed577b', comprehensive_operational_readiness).
narrative_ontology:cs_drift_state('265d372e-2df5-42cb-bb61-a90889ed577b', contemporary_mixed_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('265d372e-2df5-42cb-bb61-a90889ed577b', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the overall disaster preparedness framework, administering budgets for both engineering inspection and evacuation drill programs. Politically constrained from abolishing either component even where operational returns have diverged, because cutting any preparedness activity invites liability exposure.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_agency, agenda_setter,
    institutional, generational, constrained, national).

% Conduct rigorous structural and safety inspections that retain genuine operational competence. They observe the divergence between inspected physical risk and the rehearsed response protocols, but their professional mandate is limited to the inspection subsystem.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspectors, observer,
    organized, biographical, constrained, regional).

% Manage scheduled evacuation drills, documentation, and compliance reporting. Their work is procedurally specified and repeated regardless of evolving risk profiles or building-specific conditions; they maintain the ritualized subsystem without capturing concentrated extraction.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, drill_administrators, observer,
    moderate, biographical, constrained, regional).

% Funds and audits preparedness programs through appropriations and post-audit reviews. They receive reports showing mixed competence but lack political incentive to surgically prune ritualized components, preferring to maintain the bundled institutional appearance of readiness.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, legislative_oversight, observer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Engineering inspection coordinates genuine risk identification and mitigation; evacuation drills once coordinated collective response behavior but now primarily satisfy procedural compliance requirements.
% TRANSFER_FUNCTION: Moves institutional budget, staff time, and regulatory attention from risk-calibrated activities toward documented drill completion and inspection regimes, with ritualized components absorbing resources disproportionate to operational return.
% ABSENT_VOICES: Frontline building occupants and facilities maintenance staff who experience drills as scheduled interruptions and know which safety protocols are structurally sound versus performatively maintained; their operational knowledge is rarely solicited in formal audits.
% DISAPPEARANCE_RATIONALE: Genuine engineering inspection would need immediate replacement to prevent structural failures; ritualized drill programs would cease, freeing resources and altering compliance officer roles. The mixed nature means some functions are load-bearing while others are inertial.
% FOUNDING_PROBLEM: Catastrophic events revealed that uncoordinated emergency response and unmaintained infrastructure caused preventable loss of life and property.
% FOUNDING_PROBLEM_CORROBORATION: Engineering safety boards and post-disaster forensic reports corroborate that structural inspection prevents collapse. Independent efficiency audits and academic studies contest whether scheduled evacuation rehearsals continue to provide marginal safety returns, asserting they have become performative compliance exercises.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).
:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 because only the ritualized drill subsystem extracts operational resources disproportionate to safety returns; the competent inspection subsystem dampens the whole-system Îµ. Theater_ratio is high (0.70) because the visible face of preparedness â scheduled drills, documented compliance â is predominantly performative. Suppression is low (0.35) because persistence is driven by institutional inertia and budget continuity rather than active coercion. Accessibility_collapse is moderate (0.45): alternatives to ritualized drills are thinkable but institutionally costly to implement; alternatives to engineering inspection are not viable. Resistance is low (0.25) because no party is hurt enough by the diffuse costs to mobilize reform.
 *
 * PERSPECTIVAL GAP:
 *   Engineering inspectors experience the constraint as necessary and functional; drill administrators experience it as procedural obligation; oversight bodies see a budget line they cannot politically untangle. The engine will compute different seat types: inspectors near mountain, drill administrators near piton, oversight near rope or piton depending on how they value the compliance documentation.
 *
 * DIRECTIONALITY LOGIC:
 *   No concentrated beneficiary is declared; extraction is diffuse across the ritualized subsystem (budgets, staff time, compliance documentation). Costs are borne diffusely by taxpayers and building occupants. Without beneficiary or victim declarations, directionality defaults to canonical fallback per power atom, but the structural picture is symmetrically diffuse â no agent is heavily subsidized or targeted by the constraint as a whole.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing catastrophic loss â is live for structural inspection but dead or shifted for ritualized drills. The constraint persists as a bundled institution because the live and dead components share a budget and administrative structure. This is classic mandatrophy: the mandate has outlived its function for a significant fraction of the system. The hybrid reading captures this by declaring the founding_problem_status contested, preventing misclassification as pure coordination (rope) or pure extraction (snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the hybrid_reading of kernel preparedness_persistence. Does stratification into competent and ritualized subsystems resolve the kernel contest, or does it presuppose a decomposition that the sibling readings reject?',
    'Cross-reading empirical comparison: measure Îµ and theater_ratio independently for engineering inspection and evacuation drill subsystems under each reading''s framing.',
    'If the decomposition is robust across readings, the hybrid reading provides a synthesis; if the subsystem boundaries collapse under measurement, the kernel contest remains unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Whether hybrid stratification resolves the preparedness kernel contest').

omega_variable(
    sibling_structural_delta,
    'The competence_reading treats all preparedness as live knowledge (low Îµ, low theater), while the husk_reading treats all as memorial performance (high Îµ, high theater). How much of the variance in this reading''s metrics is driven by subsystem selection versus framing?',
    'Subsystem-isolated measurement: author separate constraint stories for engineering inspection and evacuation drills, then compare their metrics to the hybrid whole.',
    'High framing-dependency would suggest the kernel is observer-relative; high subsystem-dependency would support Îµ-invariance and justify decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_structural_delta, empirical, 'Framing versus subsystem contribution to metric variance across kernel readings').

omega_variable(
    inertia_vs_active_defense,
    'Is the persistence of ritualized drill programs driven by passive institutional inertia (budget continuity, role preservation) or by active political defense from concentrated beneficiaries?',
    'Budget-trace analysis: if drill budgets survive zero-based review without targeted lobbying, inertia dominates; if cuts are systematically blocked by identifiable actors, active defense dominates.',
    'If inertia dominates, the piton classification holds; if active defense and concentrated beneficiaries are found, the constraint reclassifies toward snare or tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inertia_vs_active_defense, empirical, 'Institutional inertia versus active beneficiary defense in ritualized subsystem persistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(preparedness_persistence_hybrid_tr_t0, preparedness_persistence__hybrid_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(preparedness_persistence_hybrid_tr_t8, preparedness_persistence__hybrid_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(preparedness_persistence_hybrid_tr_t16, preparedness_persistence__hybrid_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(preparedness_persistence_hybrid_tr_t24, preparedness_persistence__hybrid_reading, theater_ratio, 24, 0.52).
narrative_ontology:measurement(preparedness_persistence_hybrid_tr_t32, preparedness_persistence__hybrid_reading, theater_ratio, 32, 0.62).
narrative_ontology:measurement(preparedness_persistence_hybrid_tr_t40, preparedness_persistence__hybrid_reading, theater_ratio, 40, 0.7).

% Extraction over time
narrative_ontology:measurement(preparedness_persistence_hybrid_be_t0, preparedness_persistence__hybrid_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(preparedness_persistence_hybrid_be_t8, preparedness_persistence__hybrid_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(preparedness_persistence_hybrid_be_t16, preparedness_persistence__hybrid_reading, base_extractiveness, 16, 0.28).
narrative_ontology:measurement(preparedness_persistence_hybrid_be_t24, preparedness_persistence__hybrid_reading, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(preparedness_persistence_hybrid_be_t32, preparedness_persistence__hybrid_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement(preparedness_persistence_hybrid_be_t40, preparedness_persistence__hybrid_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(preparedness_persistence_hybrid_su_t0, preparedness_persistence__hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(preparedness_persistence_hybrid_su_t8, preparedness_persistence__hybrid_reading, suppression_requirement, 8, 0.27).
narrative_ontology:measurement(preparedness_persistence_hybrid_su_t16, preparedness_persistence__hybrid_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(preparedness_persistence_hybrid_su_t24, preparedness_persistence__hybrid_reading, suppression_requirement, 24, 0.32).
narrative_ontology:measurement(preparedness_persistence_hybrid_su_t32, preparedness_persistence__hybrid_reading, suppression_requirement, 32, 0.34).
narrative_ontology:measurement(preparedness_persistence_hybrid_su_t40, preparedness_persistence__hybrid_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).

% DUAL FORMULATION NOTE:
% The kernel 'preparedness persistence' decomposes into three readings because the natural-language label conflates subsystems with divergent Îµ profiles. The hybrid reading stratifies the system, linking to both the competence and husk readings as boundary cases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
