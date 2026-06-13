% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__grievance_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__grievance_threshold_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: secession_legitimacy_boundary__grievance_threshold_reading
 *   human_readable: Secession Legitimacy via Grievance Threshold
 *   domain: political_economy/federalism/resource_politics
 *
 * SUMMARY:
 *   This constraint defines the conditions under which secession from a
 *   federal state becomes legitimate, specifically when federal actions cross
 *   a threshold of structural injustice, irrespective of the constitutional
 *   text. It is a reading of the broader 'secession_legitimacy_boundary'
 *   kernel, emphasizing a grievance-based justification for exit. The
 *   constraint operates as a Tangled Rope: it offers a theoretical
 *   coordination mechanism (a path to legitimate exit for the aggrieved) but
 *   in practice, it enables extraction by setting a high,
 *   federally-controlled bar for 'structural injustice,' thus suppressing
 *   legitimate grievances and maintaining the status quo.
 *
 * KEY AGENTS:
 *   - federal_government: Agenda setter (institutional/constrained) — defines and enforces the threshold.
 *   - aggrieved_states_or_regions: Payer (organized/identity_locked) — bears the costs of injustice and the burden of proof.
 *   - majority_states: Beneficiary (institutional/mobile) — benefits from union stability and current resource distribution.
 *   - international_observers: Observer (analytical/analytical) — provides external validation or critique.
 *   - resource_producing_regions: Payer (moderate/constrained) — primary source of grievance claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, 0.6).
domain_priors:suppression_score(secession_legitimacy_boundary__grievance_threshold_reading, 0.7).
domain_priors:theater_ratio(secession_legitimacy_boundary__grievance_threshold_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__grievance_threshold_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__grievance_threshold_reading, tangled_rope).
narrative_ontology:human_readable(secession_legitimacy_boundary__grievance_threshold_reading, "Secession Legitimacy via Grievance Threshold").
narrative_ontology:topic_domain(secession_legitimacy_boundary__grievance_threshold_reading, "political_economy/federalism/resource_politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__grievance_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__grievance_threshold_reading, 'f9d517da-35ec-4568-a737-640c41439111').
narrative_ontology:cs_kernel_codification('f9d517da-35ec-4568-a737-640c41439111', distributed).
narrative_ontology:cs_authority_grounding('f9d517da-35ec-4568-a737-640c41439111', extraction).
narrative_ontology:cs_interpretation_layer_present('f9d517da-35ec-4568-a737-640c41439111').
narrative_ontology:cs_reading_relation('f9d517da-35ec-4568-a737-640c41439111', secession_legitimacy_boundary__constitutional_impossibility_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9d517da-35ec-4568-a737-640c41439111', secession_legitimacy_boundary__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('f9d517da-35ec-4568-a737-640c41439111', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('f9d517da-35ec-4568-a737-640c41439111', foundational, legitimacy_from_justice_not_text).
narrative_ontology:cs_axiom_status(legitimacy_from_justice_not_text, holdable).
narrative_ontology:cs_axiom_grounding('f9d517da-35ec-4568-a737-640c41439111', legitimacy_from_justice_not_text, deontological).
narrative_ontology:cs_axiom('f9d517da-35ec-4568-a737-640c41439111', foundational, federal_overreach_justifies_exit).
narrative_ontology:cs_axiom_status(federal_overreach_justifies_exit, holdable).
narrative_ontology:cs_axiom_grounding('f9d517da-35ec-4568-a737-640c41439111', federal_overreach_justifies_exit, empirically_contingent).
narrative_ontology:cs_reference_frame('f9d517da-35ec-4568-a737-640c41439111', justice_based_federal_compact).
narrative_ontology:cs_drift_state('f9d517da-35ec-4568-a737-640c41439111', contemporary_global_federalism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f9d517da-35ec-4568-a737-640c41439111', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__grievance_threshold_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__grievance_threshold_reading, majority_states).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_states_or_regions).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__grievance_threshold_reading, resource_producing_regions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers federal law and resource allocation, often benefiting from the current arrangement. It defines what constitutes 'structural injustice' in a way that rarely meets the threshold for legitimate secession, thus maintaining the union.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, federal_government, agenda_setter,
    institutional, generational, constrained, national).

% Bear the costs of perceived structural injustice (e.g., disproportionate resource extraction, cultural marginalization). They seek to define and prove that the federal actions have crossed the threshold, legitimizing their claim to secession. Their identity is often tied to their regional distinctiveness and historical grievances.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, aggrieved_states_or_regions, payer,
    organized, generational, identity_locked, regional).

% Benefit from the stability of the union and the current distribution of resources and power. They generally oppose secession claims and support the federal government's interpretation of 'structural injustice' as a very high bar.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, majority_states, beneficiary,
    institutional, generational, mobile, national).

% Monitor human rights, self-determination, and conflict resolution. They provide external validation or critique of claims of structural injustice and the legitimacy of secession, influencing international opinion and potential intervention.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, international_observers, observer,
    analytical, generational, analytical, global).

% Often bear the direct environmental and social costs of resource extraction while seeing a disproportionate share of the economic benefits flow to the federal or majority state governments. They are a primary source of grievance claims.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__grievance_threshold_reading, resource_producing_regions, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for evaluating the legitimacy of secession claims based on objective criteria of structural injustice, aiming to prevent arbitrary dissolution of the union while acknowledging extreme grievances.
% TRANSFER_FUNCTION: Transfers the burden of proof for secession legitimacy onto aggrieved parties, requiring them to demonstrate a threshold of structural injustice, thereby preserving the existing federal power and resource distribution unless this high bar is met.
% ABSENT_VOICES: Indigenous nations and other self-determining groups whose sovereignty claims predate the federal state are often excluded from the debate, as their legitimacy for secession is not framed by federal 'structural injustice' but by inherent rights.
% DISAPPEARANCE_RATIONALE: If this reading of secession legitimacy vanished, the criteria for union dissolution would become either purely constitutional (making unilateral secession impossible) or purely popular (making it a simple majority vote). This would fundamentally alter the power dynamics between federal and regional entities, leading to either greater federal entrenchment or increased regional instability.
% FOUNDING_PROBLEM: The problem of balancing federal unity with regional autonomy and preventing both arbitrary secession and indefinite subjugation of aggrieved populations.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political theorists attest to the ongoing tension between federal power and regional self-determination. International legal scholars and human rights organizations corroborate the need for mechanisms to address structural injustice within federal systems, even if they dispute the specific threshold or remedy.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__grievance_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__grievance_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__grievance_threshold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(secession_legitimacy_boundary__grievance_threshold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__grievance_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__grievance_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the federal government's ability to define and control the 'grievance threshold,' making it difficult for aggrieved parties to meet. Suppression (0.7) is high because the federal state actively enforces its interpretation, often through legal and political means, to prevent secession. The theater ratio (0.2) is relatively low, as the debate over structural injustice is often genuine, but the 'threshold' itself can become a performative barrier. The rising extractiveness and suppression over time reflect an increasing centralization of power and a hardening of federal resistance to regional autonomy claims.
 *
 * PERSPECTIVAL GAP:
 *   From the federal government's perspective, this constraint is a Rope, providing a legitimate, albeit high-bar, path for grievances to be addressed, ensuring stability. From the perspective of aggrieved states or regions, it functions as a Snare, where the 'threshold' is an impossible standard designed to trap them within an extractive system. The engine's classification as Tangled Rope captures this hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government and majority states are beneficiaries (low d) as they benefit from the stability and the high bar for secession. Aggrieved states and resource-producing regions are targets (high d) as they bear the costs of structural injustice and the burden of proving the threshold has been crossed. International observers are analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (as the federal government might claim) by highlighting the active enforcement and identifiable victims. It also prevents mislabeling it as a pure Snare by acknowledging the genuine coordination function of providing a theoretical, albeit difficult, path for legitimate secession. The 'grievance threshold' itself is the mechanism that allows for both coordination (a shared standard for dispute) and extraction (control over that standard).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_of_injustice_threshold,
    'Can ''structural injustice'' be objectively defined and measured to establish a clear, non-arbitrary threshold for legitimate secession, or is its definition inherently political and subject to federal control?',
    'Establishment of an independent, internationally recognized body with authority to adjudicate claims of structural injustice against agreed-upon criteria, or a clear legal precedent from a neutral arbiter.',
    'If objective, the constraint moves closer to a Rope, as the threshold provides a genuine, fair path for coordination. If inherently political, it reinforces the Snare-like qualities, as the threshold becomes a tool for federal extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_of_injustice_threshold, conceptual, 'Ambiguity in the objectivity of the ''structural injustice'' threshold.').

omega_variable(
    federal_interpretation_bias,
    'To what extent does the federal government''s interpretation and enforcement of the ''grievance threshold'' systematically bias outcomes against aggrieved regions, regardless of the merits of their claims?',
    'Comparative analysis of secession attempts across different federal systems, examining the success rates of grievance-based claims versus constitutional or popular sovereignty claims, controlling for objective measures of injustice.',
    'Strong evidence of systematic bias would shift the constraint closer to a Snare, indicating the coordination function is largely a cover for extraction. Weak or no bias would support the Tangled Rope classification, acknowledging a genuine, albeit imperfect, coordination attempt.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(federal_interpretation_bias, empirical, 'Bias in federal interpretation of the grievance threshold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__grievance_threshold_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t1950, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(sece_tr_t1970, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(sece_tr_t1990, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(sece_tr_t2010, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(sece_tr_t2024, secession_legitimacy_boundary__grievance_threshold_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sece_be_t1950, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(sece_be_t1970, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(sece_be_t1990, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(sece_be_t2010, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(sece_be_t2024, secession_legitimacy_boundary__grievance_threshold_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t1950, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(sece_su_t1970, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(sece_su_t1990, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(sece_su_t2010, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(sece_su_t2024, secession_legitimacy_boundary__grievance_threshold_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
