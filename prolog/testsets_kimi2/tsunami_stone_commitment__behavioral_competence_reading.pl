% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone as Live Behavioral Commitment
 *   domain: disaster_anthropology/commitment_system/institutional_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the behavioral_competence_reading of
 *   the Japanese tsunami stone (hinan-ishi) kernel. Under this reading, the
 *   inscribed stone and its accompanying intergenerational oral tradition
 *   operated as a live commitment system: coastal communities evacuated to
 *   high ground and restricted low-elevation settlement because the stone
 *   retained normative force through elder maintenance and ritual repetition.
 *   The constraint is authored as a piton: its founding coordination
 *   functionâpreserving disaster memory without state infrastructureâhas
 *   been superseded by modern early-warning systems, yet the stone and its
 *   practices persist by institutional inertia and theatrical maintenance.
 *   There is no concentrated extractive beneficiary; community elders do not
 *   capture rents, and households bear only diffuse opportunity costs. The
 *   metrics and claim are authored independently: the reading asserts
 *   behavioral competence, while the metrics describe negligible extraction
 *   coupled with high performative maintenance.
 *
 * KEY AGENTS:
 *   - community_elders (agenda_setter / organized / identity_locked) â maintain the stone and transmit the norm intergenerationally
 *   - coastal_households (payer / moderate / constrained) â bear diffuse costs of restricted land use and evacuation compliance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.2).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone as Live Behavioral Commitment").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_system/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '353be6cb-7847-4b6e-920c-91884a16bd89').
narrative_ontology:cs_kernel_codification('353be6cb-7847-4b6e-920c-91884a16bd89', fixed_text).
narrative_ontology:cs_authority_grounding('353be6cb-7847-4b6e-920c-91884a16bd89', lineage).
narrative_ontology:cs_interpretation_layer_present('353be6cb-7847-4b6e-920c-91884a16bd89').
narrative_ontology:cs_reading_relation('353be6cb-7847-4b6e-920c-91884a16bd89', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('353be6cb-7847-4b6e-920c-91884a16bd89', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('353be6cb-7847-4b6e-920c-91884a16bd89', foundational, inscribed_norm_causally_effective).
narrative_ontology:cs_axiom_status(inscribed_norm_causally_effective, holdable).
narrative_ontology:cs_axiom_grounding('353be6cb-7847-4b6e-920c-91884a16bd89', inscribed_norm_causally_effective, empirically_contingent).
narrative_ontology:cs_axiom('353be6cb-7847-4b6e-920c-91884a16bd89', foundational, oral_transmission_authority_legitimate).
narrative_ontology:cs_axiom_status(oral_transmission_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('353be6cb-7847-4b6e-920c-91884a16bd89', oral_transmission_authority_legitimate, conventional).
narrative_ontology:cs_reference_frame('353be6cb-7847-4b6e-920c-91884a16bd89', intergenerational_memory_state).
narrative_ontology:cs_drift_state('353be6cb-7847-4b6e-920c-91884a16bd89', modern_early_warning_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('353be6cb-7847-4b6e-920c-91884a16bd89', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, coastal_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain the physical stone monument, lead periodic rituals of remembrance, and orally transmit the specific evacuation rule and land-use boundary to each generation. Their standing in the village is bound to this custodial role; stepping away from it would mean relinquishing a core source of local authority and identity.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, community_elders, agenda_setter,
    organized, generational, identity_locked, local).

% Live with a customary prohibition on building homes below the elevation line marked by the stone, and participate in evacuation drills tied to the oral tradition. They bear the diffuse opportunity cost of restricted coastal land use and the social friction of noncompliance. Leaving the village is possible but economically and socially costly.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, coastal_households, payer,
    moderate, biographical, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves actionable tsunami memory across intervals longer than a human lifetime in coastal communities without state-run early warning systems, converting a catastrophic event into a durable behavioral prescription.
% TRANSFER_FUNCTION: Moves intergenerational deference and land-use restraint from younger community members to the memory of past victims encoded in the stone; no material rent is transferred, but opportunity costs of restricted settlement are borne by households.
% ABSENT_VOICES: Modern municipal planners, real-estate developers, and state disaster bureaucrats are absent from the traditional transmission frame; they would favor coastal development and centralized warning infrastructure over elder-maintained local memory.
% DISAPPEARANCE_RATIONALE: If the stone and its intergenerational transmission vanished, the localized behavioral prescription and elevation boundary would be lost to new generations; coastal land use would likely shift toward the shore, and the community's self-contained preparedness posture would degrade even if state systems remained.
% FOUNDING_PROBLEM: Tsunami recurrence in a region lacking literacy, state infrastructure, and rapid communication, where the interval between disasters exceeds a single lifetime and actionable memory therefore decays without artificial aids.
% FOUNDING_PROBLEM_CORROBORATION: Historical mortality records and geological tsunami traces corroborate the recurrent hazard. Modern disaster sociologists attest that pre-modern communities faced exactly this memory-behavior gap. Community elders assert the problem is still live, but this is self-asserted; external corroboration from modern infrastructure planners and historical geologists supports the view that the founding problem has been superseded by state early-warning systems.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.1, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is very low (0.10) because the constraint transfers no material rent; it exacts only land-use opportunity and social compliance. Suppression is low (0.20) because enforcement is normative and identity-based rather than coercive. Theater_ratio is high (0.65) because, in the modern era, much of the stone's maintenance (cleaning, ritual retelling, processional remembrance) serves communal identity rather than functional warning: state early-warning infrastructure has absorbed the coordination load. Accessibility_collapse is moderate-low (0.30): modern alternatives are visible once known, but within the traditional frame the stone's prescription appears unique. Resistance is low (0.15) because the norm is prosocial and safety-oriented. The temporal series trace a single shared grid: extraction stays negligible while theater accumulates as the coordination function atrophies.
 *
 * PERSPECTIVAL GAP:
 *   The elder seat and the household seat compute similarly: both are embedded in the same local identity structure and neither experiences concentrated extraction. The meaningful divergence is between this reading and the commemorative_husk_reading, which would register a much higher theater_ratio and classify the constraint as almost pure performance. The engine will compute low effective extraction for both seated agents because the base epsilon is negligible.
 *
 * DIRECTIONALITY LOGIC:
 *   No directionality override is required. The elders are agenda_setters with identity_locked exit; their structural directionality is near-symmetric because they do not collect rents. Households are payers in the sense that they bear diffuse opportunity costs, but with negligible base extractiveness, effective extraction chi is negligible for all seats. The engine's derivation chain will produce low chi across the board, consistent with the piton profile.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreserving actionable tsunami memory across generations without state infrastructureâis dead in the contemporary Japanese context. The constraint persists not because it solves an unsolved coordination problem, but because the intergenerational transmission mechanism has become self-maintaining cultural inertia. This prevents misclassification as a rope (active coordination) or snare (active extraction): the arrangement is a stabilized residue of a once-functional norm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this stone inscription a live behavioral commitment or a commemorative husk?',
    'Ethnographic and historical analysis correlating stone content with settlement patterns and evacuation behavior across multiple generations.',
    'If the husk reading is correct, theater_ratio rises and the constraint is pure piton/performance; if this reading is correct, the constraint retains functional coordination despite its aged form.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Core ambiguity between live norm and symbolic artifact').

omega_variable(
    piton_function_boundary,
    'Does the stone''s persistence reflect ongoing functional necessity or purely inertial cultural maintenance?',
    'Comparative mortality and evacuation data from the 2011 tsunami in communities with and without active stone traditions.',
    'If the tradition had no measurable protective effect, the constraint is a pure piton; if it did, the classification edges toward rope or degraded rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_function_boundary, empirical, 'Whether the constraint is functionally necessary or inertial').

omega_variable(
    empirical_validation_influence,
    'Does the 2011 tsunami event validate the stone as an empirical prediction device, or does it merely test a post-hoc interpretation?',
    'Examine whether 2011 outcomes caused communities to update the stone''s content, authority, or transmission intensity.',
    'If communities updated the stone, the validation axis influences the competence reading; if no update occurred, the event was absorbed into existing frames without altering the constraint''s structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_validation_influence, conceptual, 'Relationship between catastrophe validation and reading structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(tsun_tr_t40, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(tsun_tr_t80, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 80, 0.6).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 100, 0.65).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(tsun_be_t40, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 40, 0.07).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.08).
narrative_ontology:measurement(tsun_be_t80, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 80, 0.09).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 100, 0.1).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(tsunami_stone_commitment__behavioral_competence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
