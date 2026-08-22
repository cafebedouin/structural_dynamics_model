% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__mourning_practice_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__mourning_practice_reading
 *   human_readable: Catastrophe Memory Preservation as Mourning Practice
 *   domain: religious studies / collective memory / ritual practice
 *
 * SUMMARY:
 *   This constraint instantiates the mourning_practice_reading of the
 *   catastrophe_memory_preservation kernel. It treats ritual practice after
 *   catastrophe as a coordination mechanism that preserves symbolic
 *   continuity and collective identity without transferring operational
 *   survival competence. The reading is contested by sibling readings:
 *   survival_competence_reading claims ritual preserves operational
 *   threat-recognition, and hybrid_atrophy_reading claims ritual has
 *   atrophied from operational to symbolic function under modernity. This
 *   story authors the constraint as a rope: voluntary, low-extraction,
 *   producing in-group cohesion as a beneficiary with no victim set.
 *
 * KEY AGENTS:
 *   - in_group_members (beneficiary, moderate power, mobile exit): derive collective identity from voluntary mourning ritual participation
 *   - ritual_practitioners (agenda_setter, moderate power, mobile exit): maintain and transmit the commemorative practice without extracting from participants
 *   - memory_studies_scholars (observer, analytical power, analytical exit): document the symbolic function from outside the ritual commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__mourning_practice_reading, 0.25).
domain_priors:suppression_score(catastrophe_memory_preservation__mourning_practice_reading, 0.15).
domain_priors:theater_ratio(catastrophe_memory_preservation__mourning_practice_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__mourning_practice_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__mourning_practice_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_preservation__mourning_practice_reading, "Catastrophe Memory Preservation as Mourning Practice").
narrative_ontology:topic_domain(catastrophe_memory_preservation__mourning_practice_reading, "religious studies / collective memory / ritual practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__mourning_practice_reading, '41079fa9-e9c1-4b9a-9e09-5b5fa4afac08').
narrative_ontology:cs_kernel_codification('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', implicit).
narrative_ontology:cs_authority_grounding('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', practice).
narrative_ontology:cs_interpretation_layer_present('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08').
narrative_ontology:cs_reading_relation('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', catastrophe_memory_preservation__hybrid_atrophy_reading, coexists_with).
narrative_ontology:cs_axiom('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', foundational, symbolic_continuity_over_operational_utility).
narrative_ontology:cs_axiom_status(symbolic_continuity_over_operational_utility, holdable).
narrative_ontology:cs_axiom_grounding('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', symbolic_continuity_over_operational_utility, conventional).
narrative_ontology:cs_axiom('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', foundational, voluntary_ritual_participation_suffices).
narrative_ontology:cs_axiom_status(voluntary_ritual_participation_suffices, holdable).
narrative_ontology:cs_axiom_grounding('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', voluntary_ritual_participation_suffices, conventional).
narrative_ontology:cs_reference_frame('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', living_mourning_practice).
narrative_ontology:cs_drift_state('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('41079fa9-e9c1-4b9a-9e09-5b5fa4afac08', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__mourning_practice_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__mourning_practice_reading, in_group_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in post-catastrophe mourning rituals to maintain collective identity and symbolic continuity. Participation is voluntary and opt-in; members derive cohesion, shared narrative, and belonging from the practice. Exit means leaving the ritual community but does not typically incur material penalty or broader social exclusion.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, in_group_members, beneficiary,
    moderate, generational, mobile, regional).

% Maintain, schedule, and transmit ritual protocols for catastrophe commemoration. They hold custodial authority over the practice's form but do not extract material rents from other participants; their authority derives from enactment and recognition rather than enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, ritual_practitioners, agenda_setter,
    moderate, generational, mobile, regional).

% Study the social function of catastrophe rituals from outside the ritual commitment, documenting how symbolic continuity and collective identity operate independently of operational survival training. They observe the coordination mechanism without being bound by its norms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__mourning_practice_reading, memory_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective identity and symbolic continuity after catastrophe by providing a shared, repeatable practice through which group members acknowledge loss and affirm membership without requiring operational threat-response training.
% TRANSFER_FUNCTION: Moves attention, emotional labor, and commemorative effort from individual grief processing into a collective, standardized symbolic framework that produces group cohesion and intergenerational narrative stability.
% ABSENT_VOICES: Secular critics who view ritual mourning as cognitively costly or substitutionally inadequate; trauma-processing professionals who advocate individualized therapeutic models over collective ritual; and out-group members who do not share the symbolic vocabulary and whose exclusion from the ritual space is unremarked.
% DISAPPEARANCE_RATIONALE: If the mourning practice vanished, the group would lose a primary vehicle for post-catastrophe identity reaffiliation; individual grief would remain but the collective frame for integrating it would dissolve, requiring alternative social technologies for cohesion.
% FOUNDING_PROBLEM: Catastrophe disrupts collective narrative continuity; without a repeated practice, group memory fragments and identity coherence weakens across generations.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and memory studies scholars outside the benefiting ritual community attest that social groups experiencing catastrophe exhibit memory fragmentation and identity dissolution in the absence of shared commemorative practice.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__mourning_practice_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__mourning_practice_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__mourning_practice_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_preservation__mourning_practice_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__mourning_practice_reading, 0.25, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.25) because the constraint moves emotional labor into a collective frame but does not subtract material resources or suppress alternatives; participation is opt-in. Suppression is very low (0.15) because enforcement is absent and exit is socially available. Theater ratio is low (0.10) because the ritual's symbolic function is largely identical with its performed activityâthere is little gap between display and purpose. Accessibility collapse is moderate (0.40) because while alternatives to ritual exist, the specific symbolic vocabulary is non-substitutable for identity holders. Resistance is negligible (0.05) because beneficiaries are also the participants and the arrangement is voluntary.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (ritual practitioners) and beneficiary seat (in_group_members) should compute similarly as low-directionality positions because both participate in and sustain the coordination. The observer seat (scholars) has analytical exit and sees the structure from outside, but its classification should also trend toward rope because the structural data shows no extraction or enforcement. Divergence from the sibling readings occurs because those readings import functional claims (operational transfer, historical atrophy) that this reading brackets.
 *
 * DIRECTIONALITY LOGIC:
 *   In_group_members are the structural beneficiaries: they receive group cohesion and symbolic continuity (d near 0.0). Ritual_practitioners are agenda_setters but not extractors; their role is custodial rather than rent-collecting, placing them near symmetric or slightly toward beneficiary (d ~ 0.3). No victim set is declared because participation is opt-in and no agent bears a cost that constitutes extraction. Scholars are analytical observers with no stake in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope prevents mislabeling this voluntary commemorative practice as a snare or tangled rope. If the same ritual were enforced by a hierarchy that punished non-participation, or if it were shown to secretly extract labor or resources under the cover of mourning, the metrics would shift toward extraction and suppression. The absence of a victim set and the low suppression score are the mandatrophy guards: they keep the coordination story from collapsing into extraction when the coordination is genuine but emotionally weighty.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the current ritual function genuinely purely symbolic mourning, or does latent operational threat-recognition capacity persist beneath the symbolic surface?',
    'Ethnographic observation of ritual content for embedded threat-cues; empirical measurement of participant preparedness versus non-participant controls; historical comparison of ritual form before and after the putative atrophy point.',
    'If operational capacity is found to persist, this reading''s epsilon is too low and the constraint shifts toward tangled_rope or snare; if purely symbolic, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether this reading correctly identifies the constraint as purely symbolic or conflates it with operational survival functions.').

omega_variable(
    voluntary_participation_boundary,
    'To what extent is participation in catastrophe mourning ritual genuinely voluntary versus socially compulsory within the in-group?',
    'Participant exit interviews and ethnographic observation of social sanction for non-participation; measurement of identity-cost for opting out.',
    'If exit carries high identity-cost, the rope framing understates extraction and the constraint may function as tangled_rope with identity-locked exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_boundary, empirical, 'Whether opt-in participation is structurally voluntary or identity-coerced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__mourning_practice_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_preservation__mourning_practice_reading, theater_ratio, 50, 0.12).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 20, 0.22).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 30, 0.23).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_preservation__mourning_practice_reading, base_extractiveness, 50, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 20, 0.12).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 30, 0.13).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 40, 0.14).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_preservation__mourning_practice_reading, suppression_requirement, 50, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__mourning_practice_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__mourning_practice_reading, hybrid_atrophy_reading).

% DUAL FORMULATION NOTE:
% This constraint is the mourning_practice_reading of the catastrophe_memory_preservation kernel, decomposed from the colloquial label per the epsilon-invariance principle. Sibling constraints handle survival_competence and hybrid_atrophy readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
