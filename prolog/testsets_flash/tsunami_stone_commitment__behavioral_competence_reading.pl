% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Commitment (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/institutional_memory
 *
 * SUMMARY:
 *   This constraint describes the tsunami stone inscriptions as retaining
 *   live behavioral force, actively enforced through intergenerational
 *   transmission, prior to the 2011 tsunami. It represents a successful,
 *   low-extraction commitment system where the 'cost' of compliance is
 *   outweighed by the benefit of survival. The claimed type is 'piton'
 *   because its function is so deeply integrated and beneficial that it
 *   persists by inertia of its own success, with minimal active maintenance
 *   cost relative to its value, and no concentrated beneficiary capturing
 *   extraction. This is one reading of the 'tsunami_stone_commitment' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.05).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Commitment (Behavioral Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/institutional_memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '3cfc7e06-8b68-4ef5-ac83-4e8f824564ac').
narrative_ontology:cs_kernel_codification('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', fixed_text).
narrative_ontology:cs_authority_grounding('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', lineage).
narrative_ontology:cs_interpretation_layer_present('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac').
narrative_ontology:cs_reading_relation('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', foundational, inscription_as_active_behavioral_directive).
narrative_ontology:cs_axiom_status(inscription_as_active_behavioral_directive, holdable).
narrative_ontology:cs_axiom_grounding('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', inscription_as_active_behavioral_directive, conventional).
narrative_ontology:cs_axiom('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', foundational, intergenerational_transmission_fidelity).
narrative_ontology:cs_axiom_status(intergenerational_transmission_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', intergenerational_transmission_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', ancestral_survival_mandate).
narrative_ontology:cs_drift_state('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', pre_2011_tsunami_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3cfc7e06-8b68-4ef5-ac83-4e8f824564ac', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, future_generations).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The communities living in coastal areas, whose ancestors inscribed the warnings. They bear the 'cost' of adhering to the inscription's directive (e.g., building homes above a certain elevation, evacuating when specific natural signals occur), but this cost is perceived as a life-saving investment. Their identity is deeply tied to the land and its history, making 'exit' from the commitment unthinkable.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities, payer,
    organized, generational, identity_locked, local).

% The custodians of oral tradition and local history, responsible for transmitting the stone inscription's meaning and behavioral directives across generations. They actively enforce the norm through storytelling, education, and social pressure, ensuring the inscription retains its live behavioral force. Their authority is grounded in lineage and expertise.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, elders_and_storytellers, agenda_setter,
    powerful, generational, identity_locked, local).

% The primary beneficiaries of the inscription's continued behavioral force, as their lives are protected from future tsunami events. They are 'trapped' in the sense that their survival depends on the intergenerational transmission of this knowledge, but it is a beneficial trap.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, local).

% Researchers who study the long-term effectiveness of indigenous disaster mitigation strategies. They analyze the inscription's role in shaping community behavior and resilience, providing an external, analytical perspective on its function.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, disaster_anthropologists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates intergenerational knowledge transfer and collective behavioral responses to rare but catastrophic natural events, ensuring communities maintain safety protocols over centuries.
% TRANSFER_FUNCTION: Transfers life-saving knowledge and behavioral norms from past generations to future ones, effectively transferring safety and resilience across time.
% ABSENT_VOICES: Those who might prioritize short-term economic gain (e.g., building closer to the coast for tourism) over long-term safety are implicitly excluded by the strong social norms and historical memory. Their voices are suppressed by the collective commitment to survival.
% DISAPPEARANCE_RATIONALE: If the behavioral force of the stone inscriptions vanished, communities would gradually lose the memory of safe practices, leading to increased vulnerability to future tsunamis and potential catastrophic loss of life. The long-term survival strategy would collapse.
% FOUNDING_PROBLEM: The problem of preserving life-saving knowledge and ensuring behavioral compliance across many generations in the face of rare, devastating tsunami events, where direct experience is lost to time.
% FOUNDING_PROBLEM_CORROBORATION: The continued existence of tsunami threats and the observed adherence to the stone's warnings by coastal communities, corroborated by disaster anthropologists who study the efficacy of these long-term mitigation strategies. The 2011 tsunami event provided a stark, tragic corroboration of the need for such warnings.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the 'cost' of adherence (e.g., building higher, evacuating) is a direct investment in survival, not a transfer to an extractive party. Suppression is low (0.1) as compliance is driven by strong social norms and a shared understanding of existential risk, rather than overt coercion. Theater ratio is also very low (0.05) because the inscription's function is direct and effective, with little performative overhead. The constraint is a piton because its primary function (saving lives) is fully operational and deeply embedded, but it doesn't generate concentrated benefits for any single party to actively 'maintain' it beyond the intergenerational transmission itself; its persistence is due to its proven efficacy and the collective memory of catastrophe.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal perspectival gap in this reading, as all stakeholders (except perhaps those excluded for prioritizing short-term gain) perceive the constraint as beneficial for collective survival. The 'cost' is understood as an investment, not an extraction. The analytical observer (disaster anthropologists) would confirm its efficacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal communities are payers, bearing the 'cost' of compliance, but their directionality is strongly towards beneficiary due to the direct survival benefit. Elders and storytellers are agenda-setters, actively transmitting the norm, but they also benefit from the community's survival. Future generations are clear beneficiaries. No party extracts from this arrangement; the 'extraction' is the collective effort of maintaining the norm, which directly benefits all participants.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_force_vs_commemoration,
    'Did the tsunami stone inscriptions retain live behavioral force, or had they largely decayed to symbolic artifacts by 2010?',
    'Ethnographic studies and historical records detailing community adherence to the warnings prior to the 2011 tsunami, including building practices and evacuation drills.',
    'If behavioral force was live, this ''piton'' classification holds. If they were largely symbolic, the constraint would be reclassified as a ''piton'' with a higher theater ratio, or even a ''snare'' if the symbolic value masked a lack of preparedness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_force_vs_commemoration, empirical, 'Ambiguity between active behavioral guidance and mere symbolic commemoration.').

omega_variable(
    intergenerational_transmission_efficacy,
    'How effective was the intergenerational transmission mechanism in ensuring accurate and actionable knowledge transfer over centuries?',
    'Linguistic analysis of oral traditions, archaeological evidence of consistent settlement patterns, and comparative studies with communities lacking such inscriptions.',
    'Lower efficacy would suggest a higher ''theater_ratio'' or ''suppression'' (if enforcement became more coercive to compensate for knowledge decay), potentially shifting the classification towards a ''tangled_rope'' or ''snare'' if the ''agenda_setters'' were extracting status from a failing system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_efficacy, empirical, 'Uncertainty regarding the fidelity and robustness of knowledge transmission.').

omega_variable(
    reading_framing_underdetermination,
    'Is this ''behavioral_competence_reading'' the most defensible framing, or does the ''commemorative_husk_reading'' offer a more accurate structural account of the stone''s function prior to 2011?',
    'Further ethnographic data on pre-2011 community practices and beliefs, particularly regarding the direct causal link between the stones and their actions. The 2011 tsunami''s outcome (survival vs. loss) also provides empirical validation for one reading over the other.',
    'If the ''commemorative_husk_reading'' were adopted, the constraint''s extractiveness and theater_ratio would likely be higher, as the stones would be performing a symbolic function without direct behavioral impact, potentially shifting the classification to a ''piton'' with higher theater or even a ''snare'' if the symbolic value was exploited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Under-determination between the ''behavioral_competence_reading'' and the ''commemorative_husk_reading'' of the tsunami stone commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 1000, 2010).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t1000, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1000, 0.05).
narrative_ontology:measurement(tsun_tr_t1200, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(tsun_tr_t1400, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1400, 0.05).
narrative_ontology:measurement(tsun_tr_t1600, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(tsun_tr_t1800, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(tsun_tr_t2010, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 2010, 0.05).

% Extraction over time
narrative_ontology:measurement(tsun_be_t1000, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(tsun_be_t1200, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1200, 0.05).
narrative_ontology:measurement(tsun_be_t1400, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1400, 0.05).
narrative_ontology:measurement(tsun_be_t1600, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(tsun_be_t1800, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(tsun_be_t2010, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 2010, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t1000, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1000, 0.1).
narrative_ontology:measurement(tsun_su_t1200, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1200, 0.1).
narrative_ontology:measurement(tsun_su_t1400, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1400, 0.1).
narrative_ontology:measurement(tsun_su_t1600, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1600, 0.1).
narrative_ontology:measurement(tsun_su_t1800, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 1800, 0.1).
narrative_ontology:measurement(tsun_su_t2010, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 2010, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'behavioral_competence_reading' of the 'tsunami_stone_commitment' kernel, focusing on the active, life-saving function of the inscriptions. It is linked to the 'catastrophe_validation_axis' (which treats the 2011 tsunami as an empirical test) and the 'commemorative_husk_reading' (which views the stones as symbolic artifacts).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
