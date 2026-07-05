% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__exogenous_imposition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__exogenous_imposition_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__exogenous_imposition_reading
 *   human_readable: Top-Down Decree Installation of New State Commitments (Exogenous Imposition Reading)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates the exogenous-imposition reading of the
 *   state-commitment-installation kernel: legitimacy for a new normative
 *   commitment is manufactured by an authority claiming a transformation
 *   mandate, which installs the commitment by decree without antecedent
 *   grassroots advocacy or fringe-to-center climbing. The signature
 *   structural markers are abrupt adoption, the state as direct beneficiary
 *   of the reordering, absence of organic constituency-building, and
 *   resistance concentrated at the base (customary authorities, rural
 *   populations, displaced practitioners) rather than dissolved by
 *   demonstrated superiority. This is a distinct constraint from the
 *   endogenous_climb_reading (where the same nominal commitment gains
 *   legitimacy through incremental fringe validation and voluntary uptake —
 *   negligible suppression, no decree apparatus) and from the
 *   hybrid_cascade_reading (apex-installed but requiring subsequent fringe
 *   validation to stabilize — a mixed suppression/consent profile). Each
 *   reading has its own epsilon; do not average across them.
 *
 * KEY AGENTS:
 *   - modernizing_state_apparatus: agenda_setter/beneficiary (institutional/arbitrage) — decrees the new commitment and captures legitimacy premium
 *   - reform_aligned_bureaucratic_elite: beneficiary (powerful/mobile) — staffs the apparatus administering displacement
 *   - customary_local_authorities: payer (moderate/trapped) — lose standing constituted by the superseded order
 *   - rural_populations_subject_to_decree: payer (powerless/trapped) — must reorganize practice without consent
 *   - displaced_traditional_practitioners: payer (powerless/identity_locked) — professional identity delegitimized wholesale
 *   - urban_reform_sympathizers: excluded (moderate/constrained) — favored phased adoption, given no institutional voice
 *   - historical_sociologists: observer (analytical/analytical) — compare fragility across installation modes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68).
domain_priors:suppression_score(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.79).
domain_priors:theater_ratio(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__exogenous_imposition_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__exogenous_imposition_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__exogenous_imposition_reading, "Top-Down Decree Installation of New State Commitments (Exogenous Imposition Reading)").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__exogenous_imposition_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__exogenous_imposition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__exogenous_imposition_reading, '735f5928-0007-48f1-bdd5-6a55c5567ad0').
narrative_ontology:cs_kernel_codification('735f5928-0007-48f1-bdd5-6a55c5567ad0', formalized).
narrative_ontology:cs_authority_grounding('735f5928-0007-48f1-bdd5-6a55c5567ad0', extraction).
narrative_ontology:cs_interpretation_layer_present('735f5928-0007-48f1-bdd5-6a55c5567ad0').
narrative_ontology:cs_reading_relation('735f5928-0007-48f1-bdd5-6a55c5567ad0', state_commitment_installation_mechanism__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('735f5928-0007-48f1-bdd5-6a55c5567ad0', state_commitment_installation_mechanism__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('735f5928-0007-48f1-bdd5-6a55c5567ad0', foundational, transformation_mandate_confers_installation_authority).
narrative_ontology:cs_axiom_status(transformation_mandate_confers_installation_authority, holdable).
narrative_ontology:cs_axiom_grounding('735f5928-0007-48f1-bdd5-6a55c5567ad0', transformation_mandate_confers_installation_authority, conventional).
narrative_ontology:cs_axiom('735f5928-0007-48f1-bdd5-6a55c5567ad0', foundational, base_validation_unnecessary_for_legitimacy).
narrative_ontology:cs_axiom_status(base_validation_unnecessary_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('735f5928-0007-48f1-bdd5-6a55c5567ad0', base_validation_unnecessary_for_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('735f5928-0007-48f1-bdd5-6a55c5567ad0', revolutionary_transformation_mandate).
narrative_ontology:cs_drift_state('735f5928-0007-48f1-bdd5-6a55c5567ad0', post_rupture_consolidation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('735f5928-0007-48f1-bdd5-6a55c5567ad0', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, modernizing_state_apparatus).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__exogenous_imposition_reading, reform_aligned_bureaucratic_elite).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, customary_local_authorities).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, rural_populations_subject_to_decree).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_traditional_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a self-declared transformation mandate — often forged in revolution, conquest, or crisis legitimacy — and issues the new commitment by decree, bypassing existing institutional intermediaries. Consolidates administrative and symbolic authority by replacing the prior normative order with one it controls from installation onward. Bears little cost from the disruption it authors and captures the legitimacy gains of appearing modern, decisive, and historically necessary.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, modernizing_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__exogenous_imposition_reading, modernizing_state_apparatus, beneficiary).

% Staffs the new apparatus that administers the imposed commitment — new courts, new schools, new registries — and gains career advancement, status, and resource control proportional to how thoroughly the old order is displaced. Can exit into other administrative postings if the reform falters, unlike those beneath them.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, reform_aligned_bureaucratic_elite, beneficiary,
    powerful, biographical, mobile, national).

% Held legitimacy under the prior normative order — elders, guild heads, religious officiants, customary judges — and lose standing overnight when the decree supersedes the framework their authority was embedded in. Cannot relocate their authority elsewhere; it was constituted by the very order being replaced.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, customary_local_authorities, payer,
    moderate, biographical, trapped, local).

% Must reorganize daily practice — land tenure, marriage registration, dispute resolution, schooling — around institutions installed without their participation or consent. Migration or informal noncompliance are the only available responses, both costly and precarious; formal exit from the new commitment's jurisdiction does not exist.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, rural_populations_subject_to_decree, payer,
    powerless, biographical, trapped, regional).

% Their professional and often personal identity was constituted by practicing the superseded norm (customary law specialists, traditional healers integrated into now-defunct dispute systems). The decree does not merely reduce their income; it delegitimizes the framework their competence was built on, making retraining and re-identification, not mere relocation, the only path forward.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, displaced_traditional_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Would have argued for a slower, demonstration-based adoption process building legitimacy from below, but the decree model forecloses any comparative period — installation is abrupt and total, so there is no interval in which alternative advocacy could be heard or tested.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, urban_reform_sympathizers, excluded,
    moderate, biographical, constrained, national).

% Study whether decree-installed commitments that lack grassroots validation exhibit characteristic fragility — reversal on regime change, persistent low compliance, shadow-persistence of the displaced norm — compared to commitments that climbed from institutional fringes.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__exogenous_imposition_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides rapid, uniform replacement of a fragmented or delegitimized normative order with a single administratively legible standard, useful when the prior order has collapsed (post-conquest, post-revolution, post-crisis) and waiting for organic consensus would leave a governance vacuum.
% TRANSFER_FUNCTION: Moves normative authority, administrative jurisdiction, and the legitimacy premium of defining 'the new order' from customary local authorities and practitioners to the installing state apparatus and the bureaucratic elite it deputizes.
% ABSENT_VOICES: Customary authorities and rural populations were not consulted on the content or pace of the new commitment; urban reform sympathizers who might have pushed for a phased, demonstration-based rollout were excluded from the decision because the decree model has no institutional slot for pre-installation debate.
% DISAPPEARANCE_RATIONALE: If the decree and its enforcement apparatus were withdrawn, customary authorities and displaced practitioners would attempt to reconstitute the prior normative order in the vacuum, and rural populations would revert to informal practice wherever compliance had been performative rather than substantive — the abruptness of installation without base validation means the new order has shallow roots that a genuinely climbed commitment would not have.
% FOUNDING_PROBLEM: A rupture in governing legitimacy (conquest, revolution, state collapse, or externally imposed crisis) left no functioning normative order capable of coordinating administration, and the new authority needed an immediate, unambiguous replacement it could point to as evidence of transformation.
% FOUNDING_PROBLEM_CORROBORATION: The installing state and its bureaucratic beneficiaries attest the founding rupture was real and required immediate top-down resolution. Independent historians studying comparable episodes (post-revolutionary legal codification, colonial and post-colonial administrative reform) attest that in many cases the 'rupture' was itself partly manufactured or exaggerated to justify bypassing existing institutions that would have resisted displacement — corroboration from outside the beneficiary set is mixed rather than unanimous.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__exogenous_imposition_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__exogenous_imposition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__exogenous_imposition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__exogenous_imposition_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__exogenous_imposition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__exogenous_imposition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the concentrated transfer of normative authority and administrative jurisdiction to the installing state and its bureaucratic beneficiaries, extracted from customary authorities and populations who had no part in authoring the change. Suppression (0.79, high and front-loaded per the measurement series) is the defining signature of this reading: because there is no antecedent constituency, the decree can only take hold through active enforcement — courts, registries, penalties for noncompliance — rather than through the voluntary uptake that would characterize the endogenous_climb_reading. Theater ratio rises over the interval (0.20 to 0.42) as initial coercive installation gradually gives way to performative compliance rituals (ceremonial registration, symbolic incorporation of displaced practitioners into token advisory roles) once the base's active resistance has been worn down but genuine internalization has not occurred — the state settles for compliance theater rather than continuing to expend full coercive capacity indefinitely. Accessibility collapse (0.61) is moderate-high: the decree closes off the formal old order but does not fully erase informal, shadow persistence of prior practice, which is why it is not mountain-like.
 *
 * PERSPECTIVAL GAP:
 *   From the modernizing_state_apparatus seat, the constraint likely computes closer to a coordination-flavored structure — it resolved a genuine post-rupture governance vacuum and the state bore the risk of acting first. From the customary_local_authorities and rural_populations seats, the same structure computes as extraction backed by force — a transfer of jurisdiction they never consented to, sustained only by continuing enforcement capacity. The engine's per-seat computation should surface this divergence directly from the differential power/exit data authored above; the story does not assert a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and its bureaucratic beneficiaries sit near the full-beneficiary end: they set the agenda, capture the legitimacy and administrative gains, and hold mobile/arbitrage exit options if the reform is later reversed or reformed elsewhere. Customary authorities, rural populations, and displaced practitioners sit near the full-target end: trapped or identity-locked exit, no voice in the decree's formation, and their prior standing or livelihood is precisely what the installation displaces. Urban reform sympathizers are excluded rather than coordinated or extracted from directly — their absence from the process is itself part of the constraint's structural signature (no grassroots advocacy channel exists under this reading, by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (governance vacuum after rupture) is genuinely contested as live vs. dead: in many historical cases the rupture was real and transitory, meaning the founding problem is now dead but the decree apparatus and its enforcement infrastructure persist as entrenched administrative power (mandatrophy candidate). The tangled_rope classification, rather than pure snare, is appropriate because there typically was a real coordination problem at the moment of installation — the vacuum was not always fabricated — even though the ongoing extraction has since outlived that problem in many documented cases. Where a story would show the founding problem as clearly dead with continuing enforcement, that is exactly the signature this reading is built to expose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_authenticity_ambiguity,
    'Was the transformation mandate the installing authority claims genuinely necessitated by a governance vacuum, or was the rupture narrative constructed or exaggerated to justify bypassing institutions that would have resisted displacement?',
    'Comparative historical analysis of pre-decree institutional functionality: if customary authorities and prior normative order were still substantially functional at the moment of decree, the rupture claim is weaker; if a genuine vacuum existed, it is stronger.',
    'A constructed rupture narrative shifts this constraint from tangled_rope toward snare (no genuine coordination problem, pure extraction under cover of crisis legitimacy); a genuine rupture supports the tangled_rope reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rupture_authenticity_ambiguity, empirical, 'Whether the founding rupture justifying top-down installation was real or manufactured.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is exogenous imposition the correct reading of how THIS particular commitment gained legitimacy, or would the endogenous_climb_reading or hybrid_cascade_reading better fit the same historical episode viewed with different evidence?',
    'Detailed process-tracing of the adoption timeline: presence of a genuine pre-decree advocacy network favors endogenous_climb; evidence of post-decree fringe validation efforts (local elites gradually internalizing rather than merely complying) favors hybrid_cascade; documented abrupt decree with no antecedent or subsequent validation effort supports exogenous_imposition as authored here.',
    'Selecting the wrong reading for a given historical episode would misattribute the suppression and beneficiary structure — each reading is ε-invariant on its own terms but the readings are not interchangeable descriptions of one event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of the three kernel readings actually describes a given historical installation episode.').

omega_variable(
    base_resistance_durability,
    'Does resistance at the base (customary authorities, rural populations) dissipate through genuine internalization over the interval, or does it merely go underground as the theater ratio rises?',
    'Longitudinal compliance auditing distinguishing substantive behavior change from formal registration compliance; shadow-practice surveys in rural populations.',
    'If resistance goes underground rather than dissipating, the declining suppression_requirement trajectory after time_point 12 is misleading — the state has reduced overt enforcement not because the commitment succeeded but because it has shifted to lower-cost theatrical maintenance, which would support reclassifying later-period operation toward piton-adjacent dynamics for the specific enforcement apparatus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(base_resistance_durability, empirical, 'Whether declining measured suppression reflects genuine legitimation or theater substitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__exogenous_imposition_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(stat_tr_t4, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(stat_tr_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(stat_tr_t12, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(stat_tr_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(stat_tr_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(stat_be_t4, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(stat_be_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(stat_be_t12, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(stat_be_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(stat_be_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(stat_su_t4, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 4, 0.88).
narrative_ontology:measurement(stat_su_t8, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 8, 0.83).
narrative_ontology:measurement(stat_su_t12, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 12, 0.79).
narrative_ontology:measurement(stat_su_t16, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(stat_su_t24, state_commitment_installation_mechanism__exogenous_imposition_reading, suppression_requirement, 24, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__exogenous_imposition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__exogenous_imposition_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language concept 'how new commitments gain legitimacy' (the state_commitment_installation_mechanism kernel). The endogenous_climb_reading describes bottom-up legitimation with negligible suppression; the hybrid_cascade_reading describes apex installation requiring subsequent fringe validation with a declining-suppression profile as validation accrues; this exogenous_imposition_reading describes pure top-down decree with sustained high suppression and no grassroots advocacy channel. Each has its own stable ε and is linked here via affects_constraints rather than represented as one constraint with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
