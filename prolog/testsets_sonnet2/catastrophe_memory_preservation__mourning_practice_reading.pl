% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__mourning_practice_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Commemoration Ritual as Symbolic Continuity Practice
 *   domain: religious/cultural
 *
 * SUMMARY:
 *   This story authors the mourning-practice reading of the
 *   catastrophe-memory-preservation kernel: the ritual commemorating a
 *   historical catastrophe is read here as a voluntary symbolic-continuity
 *   practice — it binds a dispersed community's identity across generations
 *   but transfers no operational threat-recognition or survival competence.
 *   Participation is elective, exit is unobstructed, and no identifiable
 *   victim set bears costs of nonparticipation. The rising theater_ratio over
 *   the interval reflects a genuine drift within THIS reading's own terms: as
 *   literal survival memory recedes from living participants, the proportion
 *   of ritual activity that is purely symbolic (as opposed to any residual
 *   instructional content) increases — this is the reading's own account of
 *   gradual purification toward pure identity-marking, not evidence against
 *   the reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.1).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Commemoration Ritual as Symbolic Continuity Practice").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious/cultural").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, 'a1a5a50f-ea98-4652-b01b-0cf5758fc090').
narrative_ontology:cs_kernel_codification('a1a5a50f-ea98-4652-b01b-0cf5758fc090', implicit).
narrative_ontology:cs_authority_grounding('a1a5a50f-ea98-4652-b01b-0cf5758fc090', practice).
narrative_ontology:cs_interpretation_layer_present('a1a5a50f-ea98-4652-b01b-0cf5758fc090').
narrative_ontology:cs_reading_relation('a1a5a50f-ea98-4652-b01b-0cf5758fc090', catastrophe_memory_preservation__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1a5a50f-ea98-4652-b01b-0cf5758fc090', catastrophe_memory_preservation__hybrid_atrophy_reading, influences).
narrative_ontology:cs_axiom('a1a5a50f-ea98-4652-b01b-0cf5758fc090', foundational, ritual_function_is_symbolic_not_operational).
narrative_ontology:cs_axiom_status(ritual_function_is_symbolic_not_operational, holdable).
narrative_ontology:cs_axiom_grounding('a1a5a50f-ea98-4652-b01b-0cf5758fc090', ritual_function_is_symbolic_not_operational, conventional).
narrative_ontology:cs_axiom('a1a5a50f-ea98-4652-b01b-0cf5758fc090', secondary, identity_continuity_requires_no_instrumental_justification).
narrative_ontology:cs_axiom_status(identity_continuity_requires_no_instrumental_justification, holdable).
narrative_ontology:cs_axiom_grounding('a1a5a50f-ea98-4652-b01b-0cf5758fc090', identity_continuity_requires_no_instrumental_justification, deontological).
narrative_ontology:cs_reference_frame('a1a5a50f-ea98-4652-b01b-0cf5758fc090', post_catastrophe_communal_reconstitution).
narrative_ontology:cs_drift_state('a1a5a50f-ea98-4652-b01b-0cf5758fc090', contemporary_diaspora_dispersal, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a1a5a50f-ea98-4652-b01b-0cf5758fc090', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, diaspora_descendants).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, ritual_officiants).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__mourning_practice_reading, collective_identity_persistence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gathers annually or on a fixed calendar to observe the commemorative rite — lighting, naming, fasting, recitation. Members choose to attend; nonattendance carries social but not material cost. The rite reaffirms who 'we' are relative to the catastrophe, not how to survive a recurrence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community, beneficiary,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, commemorating_community, agenda_setter).

% Scattered geographically from the community of origin, they use the ritual calendar to maintain a felt connection to ancestry and group identity. Participation is elective and often partial (attending some years, some observances); the ritual gives them a symbolic anchor with no operational content required for their actual daily lives.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, diaspora_descendants, beneficiary,
    moderate, generational, mobile, global).

% Clergy, elders, or designated memory-keepers who set the liturgical calendar, texts, and forms observed. They derive communal standing and modest material support from officiating, but the position is not economically coercive — others could and sometimes do decline to seek the role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_officiants, agenda_setter,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__mourning_practice_reading, ritual_officiants, beneficiary).

% Younger members drifting from ritual participation entirely, finding the commemorative forms emotionally distant from their lived concerns. They are not suppressed from leaving — no barrier holds them — but their perspective (that the ritual has become disconnected from anything actionable) rarely reaches the officiants who set the calendar.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, unaffiliated_youth, excluded,
    powerless, biographical, mobile, regional).

% Study the ritual's persistence and transformation across generations, comparing it to catastrophe-memory practices in other traditions. They document whether the rite functions as pure identity marker or retains any operational residue, without themselves participating.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__mourning_practice_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__mourning_practice_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes a dispersed community's calendar around a shared act of remembrance, allowing geographically and generationally separated members to affirm shared identity and continuity with ancestors who experienced the catastrophe, at a fixed, low, predictable cost of participation.
% TRANSFER_FUNCTION: Moves attention, ritual labor, and modest material support (candles, texts, officiant honoraria) from participants to the commemorative occasion itself; no operational knowledge, survival skill, or resource hedge is transferred to participants as a condition of the ritual's function.
% ABSENT_VOICES: Unaffiliated youth who find the ritual emotionally distant and disconnected from present concerns would, if consulted, ask why the calendar has not evolved to address contemporary threats or concerns; they are simply drifting away rather than being heard in ritual design.
% DISAPPEARANCE_RATIONALE: Officiants and older community members would say the world rearranges substantially — a core marker of collective identity vanishes, diaspora cohesion weakens measurably. Comparative-religion observers and drifting youth would say the world is largely unchanged in operational terms, since no survival-relevant capacity depends on the rite continuing; the dispute is exactly the dispute this reading takes a position in.
% FOUNDING_PROBLEM: A historical catastrophe fractured the community's continuity — dispersing survivors, breaking transmission of collective memory, and threatening the erasure of a shared identity across generations.
% FOUNDING_PROBLEM_CORROBORATION: Officiants and the commemorating community attest the identity-continuity problem remains live — assimilation and dispersal pressures persist. Comparative-religion scholars, observing from outside the beneficiary set, corroborate that the identity-continuity function is empirically active (participation correlates with self-reported group-identity strength in survey data) even as they note the rite carries no operational survival content; unaffiliated youth, also outside the beneficiary set, do not corroborate that the current ritual form still serves even the identity function for their own generation.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).
:- end_tests(catastrophe_memory_preservation__mourning_practice_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because the ritual's transfer is modest — attention, minor material contribution, ritual labor from officiants — and nothing coerced is extracted from a defined victim class. Suppression is authored very low (0.10): nonparticipation carries social cost but no material or legal barrier. Accessibility_collapse is low-moderate (0.25): alternatives to participation (secular commemoration, non-participation, alternative diaspora identity practices) remain visibly available and are exercised by unaffiliated youth. Resistance is low (0.15) because there is little active opposition to the ritual itself, mostly quiet attrition rather than contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The commemorating community and diaspora descendants are declared beneficiaries — the ritual subsidizes their sense of continuity and belonging at low personal cost, placing them near the beneficiary end of directionality. Ritual officiants hold a dual role: they set the ritual agenda but also benefit from the standing it confers, so their directionality sits closer to symmetric despite formally being the agenda-setter seat. No victim group is declared under this reading, consistent with the expected structural delta: participation is opt-in and no one is coerced into bearing the ritual's costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling the ritual as pure extraction or as a failed survival mechanism by keeping its founding-problem status honestly contested: the identity-continuity problem the ritual was built to solve is still live for older cohorts and diaspora members, per comparative-religion corroboration, even though drifting youth report the current form no longer serves them. Because the mourning-practice reading never claims operational transfer as the ritual's function, its 'success' or 'failure' is evaluated only against identity-continuity — the sibling survival_competence_reading is a different claim with a different success criterion and should not be judged by this reading's metrics or vice versa.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_content_residue_ambiguity,
    'Does the commemorative ritual, despite this reading''s claim of pure symbolic function, retain any residual operational content (e.g., embedded warning narratives, geographic hazard knowledge, or crisis-response scripts) that the mourning-practice reading is structurally motivated to discount?',
    'Ethnographic content analysis of ritual texts and oral transmission across at least two generations, coded for operationally actionable content versus purely symbolic/affective content, cross-checked against the survival_competence_reading''s own coding of the same texts.',
    'If substantial operational residue is found, the mourning_practice_reading''s claim of ''no operational transfer'' would be partially falsified for this specific ritual instance, strengthening the sibling hybrid_atrophy_reading''s account of partial-but-declining operational content over the pure mourning-practice account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_content_residue_ambiguity, empirical, 'Whether the ritual secretly retains survival-relevant content this reading denies.').

omega_variable(
    identity_function_universality_ambiguity,
    'Is the identity-continuity function this reading claims genuinely still operative across the whole community, or only for older/more committed cohorts — with unaffiliated youth''s drift indicating the function itself, not just participation, has begun to fail?',
    'Longitudinal survey of self-reported group-identity strength correlated with ritual participation across age cohorts, tracked over at least two decades.',
    'If the identity function is measurably failing even for engaged cohorts, the founding_problem_status should shift from contested toward dead, which would strengthen a piton-adjacent reading of the same practice rather than a live rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_function_universality_ambiguity, empirical, 'Whether the claimed coordination benefit is still real or is itself eroding.').

omega_variable(
    kernel_framing_choice_ambiguity,
    'Is the choice to treat ''mourning practice'' and ''survival competence'' as separable claims about the SAME ritual correct, or does the ritual''s own self-understanding fuse them such that decomposing it into three constraint stories imposes an analytic distinction the community itself does not draw?',
    'Compare emic (community self-description) accounts of the ritual''s purpose against the etic decomposition used across the three sibling stories; where emic accounts explicitly deny the separability, that is evidence the decomposition itself, not just the reading choice within it, needs revisiting.',
    'If emic accounts consistently fuse mourning and survival-competence purposes, this reading''s clean-separation framing (per Rule 1 of the committer discipline) may understate a real hybrid function that the hybrid_atrophy_reading captures better structurally, independent of any diachronic atrophy claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_ambiguity, conceptual, 'Whether decomposing the kernel into three readings matches the community''s own conceptual structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.33).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.38).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.18).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 60, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__mourning_practice_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__mourning_practice_reading, 0.06).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation__hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the catastrophe_memory_preservation kernel, each authored as a separate, ε-invariant constraint per the ε-invariance principle. mourning_practice_reading (this file) claims low-to-moderate extraction, rope classification, no victim set, and pure identity-coordination function. survival_competence_reading claims the ritual transfers real operational threat-recognition capacity — a stronger empirical claim with a potentially different beneficiary/victim structure if the competence claim is false and survival stakes are real. hybrid_atrophy_reading claims a diachronic transition from the survival-competence function to the mourning-practice function under modernity, treating the two synchronic readings as successive historical states of the same underlying practice rather than as competing contemporaneous claims. All three are linked via affects_constraints; none averages or hedges across the others' ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
