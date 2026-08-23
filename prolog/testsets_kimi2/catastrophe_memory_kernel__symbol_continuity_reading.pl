% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__symbol_continuity_reading
 *   human_readable: Ritual as Symbolic Continuity of Collective Identity
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates the symbol_continuity_reading of the
 *   catastrophe_memory_kernel: ritual practice is read as a mechanism for
 *   preserving symbolic continuity and collective identity across time,
 *   particularly after catastrophic rupture. The reading treats
 *   mourning-practice as an identity-marker whose primary yield is
 *   transmission rather than operational survival. The constraint coordinates
 *   intergenerational belonging but imposes rigidity costs on adaptive
 *   modification.
 *
 * KEY AGENTS:
 *   - Communal elders (agenda_setter / organized / identity_locked): administer ritual norms and judge acceptable practice.
 *   - Traditionalist communities (beneficiary / moderate / identity_locked): participate in ritual and derive collective identity from continuity.
 *   - Reform communities (payer / moderate / constrained): seek adaptive modification and bear the cost of ritual rigidity.
 *   - Secularized descendants (excluded / moderate / mobile): have exited and are absent from authoritative discourse.
 *   - Secular anthropologists (observer / analytical / analytical): study the memory-function from outside.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__symbol_continuity_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_kernel__symbol_continuity_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_kernel__symbol_continuity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__symbol_continuity_reading, resistance, 0.32).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__symbol_continuity_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__symbol_continuity_reading, "Ritual as Symbolic Continuity of Collective Identity").
narrative_ontology:topic_domain(catastrophe_memory_kernel__symbol_continuity_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__symbol_continuity_reading, 'ae335e4c-8bac-4188-b743-21bdc26b33b8').
narrative_ontology:cs_kernel_codification('ae335e4c-8bac-4188-b743-21bdc26b33b8', distributed).
narrative_ontology:cs_authority_grounding('ae335e4c-8bac-4188-b743-21bdc26b33b8', practice).
narrative_ontology:cs_interpretation_layer_present('ae335e4c-8bac-4188-b743-21bdc26b33b8').
narrative_ontology:cs_reading_relation('ae335e4c-8bac-4188-b743-21bdc26b33b8', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae335e4c-8bac-4188-b743-21bdc26b33b8', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae335e4c-8bac-4188-b743-21bdc26b33b8', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('ae335e4c-8bac-4188-b743-21bdc26b33b8', foundational, symbolic_continuity_primary).
narrative_ontology:cs_axiom_status(symbolic_continuity_primary, holdable).
narrative_ontology:cs_axiom_grounding('ae335e4c-8bac-4188-b743-21bdc26b33b8', symbolic_continuity_primary, conventional).
narrative_ontology:cs_axiom('ae335e4c-8bac-4188-b743-21bdc26b33b8', secondary, form_integrity_over_adaptive_function).
narrative_ontology:cs_axiom_status(form_integrity_over_adaptive_function, holdable).
narrative_ontology:cs_axiom_grounding('ae335e4c-8bac-4188-b743-21bdc26b33b8', form_integrity_over_adaptive_function, conventional).
narrative_ontology:cs_reference_frame('ae335e4c-8bac-4188-b743-21bdc26b33b8', unbroken_symbolic_chain).
narrative_ontology:cs_drift_state('ae335e4c-8bac-4188-b743-21bdc26b33b8', contemporary_secular_modernity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae335e4c-8bac-4188-b743-21bdc26b33b8', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__symbol_continuity_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, traditionalist_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__symbol_continuity_reading, communal_elders).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__symbol_continuity_reading, reform_communities).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__symbol_continuity_reading, collective_identity_transmission_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer ritual norms and judge acceptable practice. Their authority rests on being guardians of the unbroken chain; adaptive modifications threaten their legitimacy and structural role as interpreters of tradition.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, communal_elders, agenda_setter,
    organized, generational, identity_locked, regional).

% Participate in ritual as the primary vehicle of collective identity across catastrophe and diaspora. Experience continuity as a benefit that binds them to ancestors and descendants, even when semantic content has thinned.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, traditionalist_communities, beneficiary,
    moderate, generational, identity_locked, regional).

% Seek adaptive modifications to ritual practice to meet contemporary needs or secular contexts. Their alternatives are suppressed by communal pressure to preserve symbolic purity; they bear the cost of rigidity without capturing compensating benefits.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, reform_communities, payer,
    moderate, biographical, constrained, regional).

% Have exited the ritual community and no longer participate in its normative framework. Would argue for radical adaptation or abandonment but are structurally absent from authoritative discourse about legitimacy and memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, secularized_descendants, excluded,
    moderate, biographical, mobile, regional).

% Study the ritual's memory-function from outside its normative demands. Observe the tension between continuity and adaptation without being bound to either seat.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__symbol_continuity_reading, secular_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves collective identity across generational rupture by providing a repeated, embodied practice that carries symbolic meaning even when semantic content has thinned or territorial stability is lost.
% TRANSFER_FUNCTION: Moves obligation to maintain ritual form from the present generation to the past and future, binding current practitioners to a chain of continuity; the cost is borne by those seeking adaptive modification whose alternatives are excluded by communal pressure.
% ABSENT_VOICES: Secularized descendants and assimilationist reformers who would abandon or radically adapt the ritual are present in the broader population but structurally excluded from authoritative discourse about legitimacy.
% DISAPPEARANCE_RATIONALE: Without the ritual, the collective identity marker dissolves; traditionalist communities lose their primary mechanism of intergenerational binding, and the boundary between in-group and assimilated out-group blurs.
% FOUNDING_PROBLEM: Catastrophic rupture threatened to sever the chain of collective memory and identity, requiring a mechanism to transmit belonging across time without depending on stable territory or political sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Historical records and diaspora narratives attest to the rupture; secular historians corroborate the catastrophic event. The claim that ritual continuity is the necessary or best response is asserted primarily by traditionalist authorities and contested by reform voices.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__symbol_continuity_reading, 0.22, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).
:- end_tests(catastrophe_memory_kernel__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because the constraint's primary yield is symbolic and non-monetary; the cost is opportunity cost borne by reformers rather than active resource transfer. Suppression is moderate-low (0.28) because exit from the ritual community is socially costly but not structurally barred. Theater_ratio rises over the interval (0.20 to 0.40) as semantic content thins and performance of continuity becomes more pronounced relative to operational function. Accessibility_collapse is moderate (0.45) because secular alternatives are visible but socially expensive once the ritual is understood as identity-binding.
 *
 * PERSPECTIVAL GAP:
 *   The traditionalist seat experiences the constraint as pure coordination (identity preserved, ancestors honored), while the reformer seat experiences the same structure as an obstacle to necessary adaptation. The engine computes this divergence from identical structural data via directionality: the beneficiary seat sees subsidy, the payer seat sees extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditionalist communities and communal elders are positioned near the beneficiary end: the constraint subsidizes their identity and authority. Reform communities are positioned near the target end: they bear the rigidity cost without receiving compensating coordination. Secularized descendants have exited and are outside the directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The low extractiveness and genuine coordination function (intergenerational identity transmission) prevent misclassification as a snare. The presence of a payer seat (reform communities) and the absence of a concentrated capturer of extraction prevent misclassification as pure rope with zero asymmetry. The classification captures that the coordination is real but not costless to all parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the symbol_continuity_reading of catastrophe_memory_kernel; would adopting the survival_competence_reading or trauma_encoding_reading change the beneficiary/victim structure?',
    'Compare the four readings as separate constraint stories; if survival_competence assigns operational survival value, its beneficiary set shifts to the whole community as net beneficiaries and its victim set may empty.',
    'If resolved toward survival_competence, classification shifts toward rope with no victims; if symbol_continuity is correct, rigidity costs remain as diffuse suppression of adaptation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Sibling reading structural delta').

omega_variable(
    ritual_naturalness_vs_construction,
    'Is the ritual continuity constraint a naturally emergent feature of collective memory, or a constructed enforcement of traditional authority?',
    'Cross-cultural comparison: do catastrophe-communities without centralized elders develop equivalent ritual continuity mechanisms spontaneously?',
    'If naturally emergent, directionality shifts toward symmetric; if constructed enforcement by elders, extractiveness rises and classification may shift toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_naturalness_vs_construction, empirical, 'Natural vs constructed basis of ritual continuity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (enforced by communal sanctions) or internalized (practitioners believe adaptive modification is betrayal)?',
    'Post-exit suppression trajectory: if individuals who leave the community continue to enforce ritual norms on themselves or feel guilt, suppression is partially internalized.',
    'If internalized, effective suppression exceeds structural measure and the constraint functions as identity-locked extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__symbol_continuity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__symbol_continuity_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 30, 0.21).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__symbol_continuity_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_kernel__symbol_continuity_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__symbol_continuity_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_kernel, which decomposes into four structurally distinct claims: symbol_continuity_reading (this file), survival_competence_reading, trauma_encoding_reading, and boundary_maintenance_reading. Each reading carries a different epsilon, beneficiary structure, and coordination/extraction balance. They are not the same constraint viewed from different angles; they are competing functional ascriptions to the same ritual practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
