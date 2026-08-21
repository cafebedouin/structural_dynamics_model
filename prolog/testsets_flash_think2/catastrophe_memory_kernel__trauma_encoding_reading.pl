% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritualized Intergenerational Trauma Encoding
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes a ritual practice that functions as a mechanism
 *   for encoding and transmitting intergenerational trauma within a
 *   community, serving as a warning system against past catastrophes. While
 *   it provides a perceived benefit of threat vigilance and collective
 *   memory, it imposes a psychological burden on future generations. This
 *   story instantiates the 'trauma_encoding_reading' of the
 *   'catastrophe_memory_kernel'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.4).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritualized Intergenerational Trauma Encoding").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'b1235a52-2af7-4a66-a8ae-c9bdf1b667a8').
narrative_ontology:cs_kernel_codification('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', implicit).
narrative_ontology:cs_authority_grounding('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', practice).
narrative_ontology:cs_interpretation_layer_present('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8').
narrative_ontology:cs_reading_relation('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', foundational, memory_as_preventative_burden).
narrative_ontology:cs_axiom_status(memory_as_preventative_burden, holdable).
narrative_ontology:cs_axiom_grounding('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', memory_as_preventative_burden, deontological).
narrative_ontology:cs_axiom('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', secondary, collective_vigilance_justifies_individual_cost).
narrative_ontology:cs_axiom_status(collective_vigilance_justifies_individual_cost, holdable).
narrative_ontology:cs_axiom_grounding('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', collective_vigilance_justifies_individual_cost, instrumental).
narrative_ontology:cs_reference_frame('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', unbroken_memory_transmission).
narrative_ontology:cs_drift_state('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', contemporary_therapeutic_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('b1235a52-2af7-4a66-a8ae-c9bdf1b667a8', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, ancestral_trauma_survivors).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, ritual_leaders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the ritual practices, believing they are essential for collective survival and threat vigilance. Benefits from the perceived safety and continuity, but also bears the collective burden of trauma transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community, beneficiary).

% Inherits the psychological burden of past trauma through ritual and collective memory, experiencing heightened anxiety or a sense of perpetual threat. Has little agency to opt out of these inherited practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, future_generations, payer,
    powerless, biographical, trapped, local).

% The original generation that experienced the catastrophe and instituted the rituals. Their legacy and experience are validated and preserved, ensuring their suffering is not forgotten and serves a protective function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ancestral_trauma_survivors, agenda_setter,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, ancestral_trauma_survivors, beneficiary).

% Administer and interpret the rituals, ensuring their continuity and proper execution. They gain status and authority within the community by upholding these traditions, and benefit from the stability they provide.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_leaders, agenda_setter,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, ritual_leaders, beneficiary).

% Study the ritual practices and their effects on collective memory and psychological well-being. They analyze the mechanisms of trauma transmission and the social functions of such rituals without direct participation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, cultural_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, descendant_community).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits critical survival information, threat vigilance, and collective identity across generations, ensuring the memory of a catastrophic event and its lessons are preserved.
% TRANSFER_FUNCTION: Transfers a psychological burden (trauma memory, vigilance, anxiety) from past generations to future ones, in exchange for perceived collective safety and preparedness against recurrence.
% ABSENT_VOICES: Descendants who might wish to be free of the inherited psychological burden, or those who question the efficacy and necessity of trauma transmission versus alternative healing and resilience-building approaches.
% DISAPPEARANCE_RATIONALE: If the ritual and its trauma-encoding function vanished overnight, the community's collective memory, identity, and perceived threat-detection capacity would be profoundly altered. It would likely lead to new, potentially less structured, forms of trauma processing or a loss of vigilance against historical threats.
% FOUNDING_PROBLEM: To ensure the collective memory of a catastrophic event (e.g., persecution, genocide, natural disaster) and the lessons learned from it are never forgotten, thereby preventing its recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Historians, sociologists, and psychologists studying collective memory and trauma transmission often corroborate the persistence of the original threat or similar threats, and the community's continued vulnerability, supporting the claim that the founding problem remains live.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the significant psychological cost imposed on descendants, which is disproportionate to the direct, tangible benefits of vigilance. Suppression (0.40) is moderate, stemming from strong social norms and identity-locked participation rather than overt coercion. Theater ratio (0.20) is low, as the ritual's function is genuinely believed to be vital, even if its efficacy is debated. The claimed type is 'tangled_rope' because it genuinely coordinates (threat vigilance, collective memory) but also extracts (psychological burden) through the same structure, requiring social enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'ancestral_trauma_survivors' and 'ritual_leaders', the constraint is a necessary 'rope' for survival and identity. From the 'future_generations' seat, it is experienced as a 'snare' or 'tangled_rope' due to the inherited psychological burden and limited exit options. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'descendant_community' (as a collective) and 'ancestral_trauma_survivors' are beneficiaries, gaining threat vigilance and validation of their experience. 'Future_generations' are the primary victims, bearing the psychological cost. 'Ritual_leaders' benefit from status and continuity. The 'requires_active_enforcement' is true due to the strong social pressure and identity-based adherence to the ritual, which enforces participation and belief in its necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a 'tangled_rope' prevents mislabeling this as a 'snare' by acknowledging the genuine coordination function (threat vigilance) that the community believes it provides. However, it also prevents mislabeling it as a pure 'rope' by highlighting the significant and often unacknowledged psychological extraction from future generations. The 'founding_problem_status' being 'live' suggests the mandate is not fully atrophied, but the 'contested' corroboration points to a potential shift in function or an overestimation of the original problem's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burden_necessity_vs_avoidability,
    'Is the psychological burden on future generations a necessary cost for effective threat vigilance, or an avoidable side effect of the trauma transmission mechanism?',
    'Empirical studies comparing communities with similar historical traumas but different ritual practices, assessing both vigilance levels and psychological well-being across generations. Longitudinal studies of therapeutic interventions aimed at processing trauma without losing historical memory.',
    'If avoidable, the constraint''s extractiveness is higher than necessary, suggesting potential for ''scaffold''-like reform or ''rope''-like alternatives. If necessary, the extraction is an inherent cost of the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_necessity_vs_avoidability, empirical, 'Whether the psychological cost is an inherent or contingent feature of the warning system.').

omega_variable(
    threat_relevance_decay,
    'To what extent is the ''threat vigilance'' encoded by the ritual still relevant to contemporary risks, or has the original threat diminished or evolved beyond the ritual''s capacity to address?',
    'Historical and sociological analysis of the original catastrophe''s recurrence patterns, combined with contemporary risk assessments and community vulnerability studies. Comparison of ritual-derived warnings with actual threats faced by the community.',
    'If the threat has diminished or changed, the ''threat vigilance'' function may be atrophied, increasing the effective extractiveness and pushing the constraint towards a ''piton'' or ''snare'' classification, as the cost outweighs the diminishing benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_relevance_decay, empirical, 'Relevance of the encoded threat vigilance to current realities.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative trauma processing structural (lack of communal support for new methods) or internalized (descendants believe the inherited method is the only legitimate way)?',
    'Post-exit suppression trajectory: if individuals who leave the community or adopt alternative practices still experience internal barriers to processing trauma, it suggests internalized suppression. Community-level studies on the acceptance of new therapeutic approaches.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the ''trapped'' exit option more severe. If structural, external support for alternatives could reduce suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for trauma processing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.66).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.4).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.41).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'catastrophe_memory_kernel', each representing a distinct structural claim about the function of collective memory rituals. They are linked to capture their interdependencies within the broader kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
