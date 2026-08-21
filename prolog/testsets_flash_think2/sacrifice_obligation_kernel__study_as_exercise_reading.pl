% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Study of Sacrifice Law as Mitzvah Fulfillment
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'study as exercise' reading of the
 *   sacrifice obligation kernel, a foundational interpretation within
 *   post-Temple Judaism. It asserts that intellectual engagement with the
 *   laws of sacrifice constitutes a genuine fulfillment of the mitzvah
 *   (divine commandment). This reading emerged as a response to the
 *   destruction of the Temple, which rendered physical sacrifices impossible.
 *   It is a widely accepted and beneficial interpretation, coordinating
 *   religious practice and identity for centuries. The low extractiveness and
 *   suppression reflect its role as a legitimate and widely embraced solution
 *   to a profound religious challenge.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.2).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Study of Sacrifice Law as Mitzvah Fulfillment").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority/commitment_system").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__study_as_exercise_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, '137b43c7-9fdc-466d-988c-910dea2ecc5e').
narrative_ontology:cs_kernel_codification('137b43c7-9fdc-466d-988c-910dea2ecc5e', fixed_text).
narrative_ontology:cs_authority_grounding('137b43c7-9fdc-466d-988c-910dea2ecc5e', lineage).
narrative_ontology:cs_interpretation_layer_present('137b43c7-9fdc-466d-988c-910dea2ecc5e').
narrative_ontology:cs_reading_relation('137b43c7-9fdc-466d-988c-910dea2ecc5e', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('137b43c7-9fdc-466d-988c-910dea2ecc5e', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('137b43c7-9fdc-466d-988c-910dea2ecc5e', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('137b43c7-9fdc-466d-988c-910dea2ecc5e', foundational, intellectual_engagement_is_spiritual_fulfillment).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_spiritual_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('137b43c7-9fdc-466d-988c-910dea2ecc5e', intellectual_engagement_is_spiritual_fulfillment, theological).
narrative_ontology:cs_reference_frame('137b43c7-9fdc-466d-988c-910dea2ecc5e', halakhic_continuity_through_study).
narrative_ontology:cs_drift_state('137b43c7-9fdc-466d-988c-910dea2ecc5e', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('137b43c7-9fdc-466d-988c-910dea2ecc5e', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, talmudic_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, lay_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__study_as_exercise_reading, traditional_ritualists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and enforcers of Halakha. This reading solidifies their interpretive monopoly on how to fulfill the sacrifice obligation in the absence of a Temple, elevating intellectual engagement as the legitimate path.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_authority, agenda_setter,
    institutional, generational, identity_locked, global).

% Their core activity of studying sacred texts is elevated to a direct fulfillment of a central divine commandment, providing profound spiritual meaning and justification for their scholarly pursuits.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, talmudic_scholars, beneficiary,
    organized, biographical, constrained, global).

% Are provided with an accessible and legitimate means to fulfill the sacrifice obligation, even without a physical Temple. This allows for continuity of religious practice and identity in challenging circumstances.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, lay_adherents, beneficiary,
    moderate, biographical, mobile, global).

% Those who believe that only physical performance of sacrifices can truly fulfill the mitzvah find their preferred mode of worship de-emphasized or deemed impossible under this interpretation. While not 'extracted from' financially, they bear the cost of non-recognition for their literalist approach.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, traditional_ritualists, payer,
    powerless, generational, identity_locked, local).

% Groups focused on the immediate physical rebuilding of the Temple and restoration of sacrifices find their urgency and literal interpretation sidelined by this reading, which offers an alternative, non-physical path to fulfillment. They are not directly targeted but are excluded from the dominant discourse on current fulfillment.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, messianic_activists, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates how Jewish adherents fulfill the divine commandment of sacrifice in the absence of a functioning Temple, providing a universally accessible and legitimate path through intellectual engagement.
% TRANSFER_FUNCTION: Transfers the primary locus of religious obligation fulfillment from physical ritual performance to intellectual engagement with sacred texts, and implicitly transfers authority for defining this fulfillment to rabbinic interpretive tradition.
% ABSENT_VOICES: Traditional ritualists who insist on physical performance and messianic activists who prioritize immediate restoration of the Temple would object, arguing that study is preparatory but not equivalent to actual sacrifice. Their voices are marginalized by the dominant rabbinic consensus that upholds study as fulfillment.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished overnight, it would create a profound crisis of religious practice and identity for millions of adherents. The central commandment of sacrifice would become unfulfillable, leading to widespread spiritual distress and a collapse of current modes of religious observance. The entire structure of post-Temple Judaism would be destabilized.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE rendered the physical performance of divine sacrifice commandments impossible, creating a profound existential and halakhic crisis for Jewish communities.
% FOUNDING_PROBLEM_CORROBORATION: This problem is universally attested across Jewish historical and religious texts, from the Talmud to contemporary rabbinic literature. The ongoing absence of a Temple means the problem remains live, and the interpretation of study as fulfillment is a foundational response to this enduring challenge, corroborated by centuries of continuous practice and scholarship.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because this reading provides a legitimate and accessible path to fulfilling a central religious obligation, rather than extracting from adherents. The 'cost' is primarily the intellectual rigor required for study. Suppression is low (0.2) as the interpretation is largely consensual, though rabbinic authority actively maintains its legitimacy against alternative views. Theater ratio is minimal (0.05) because the study is a genuine, functional activity with profound spiritual meaning. Accessibility collapse is high (0.85) because this reading effectively collapses the physical performance alternative, making study the primary accessible path to fulfillment in the current era. Resistance is low (0.1) due to the widespread acceptance and benefits of this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   While this reading is largely consensual within mainstream Judaism, the 'payer' seat (traditional ritualists) experiences it differently, viewing it as an accommodation rather than a full fulfillment. The 'excluded' seat (messianic activists) sees it as diverting attention from the ultimate goal of physical restoration. The engine's per-seat classification would reflect these divergences, even though the overall constraint is a Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authority and Talmudic scholars are clear beneficiaries, as their intellectual pursuits are elevated to direct mitzvah fulfillment, and their interpretive authority is reinforced. Lay adherents also benefit by having a clear path to religious observance. Traditional ritualists are positioned as payers, as their literalist approach to physical sacrifice is sidelined, though this is a cost of non-recognition rather than direct extraction. Messianic activists are excluded, as their focus on physical restoration is deemed less urgent by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_equivalence_ambiguity,
    'Is intellectual engagement truly equivalent to physical sacrifice in the eyes of divine law, or is it a rabbinic accommodation for an impossible situation?',
    'Theological discourse and ongoing textual interpretation; ultimately, a matter of faith and interpretive tradition rather than empirical data.',
    'If deemed a mere accommodation, the ''fulfillment'' aspect might be seen as less complete, potentially increasing the perceived ''cost'' for adherents and shifting the constraint towards a more extractive (though still low) profile for those who seek absolute fulfillment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_equivalence_ambiguity, conceptual, 'Ambiguity regarding the ultimate divine status of study as sacrifice fulfillment.').

omega_variable(
    impact_on_future_ritual,
    'Does the widespread acceptance of study as fulfillment diminish the communal will or readiness for actual physical sacrifices if the Temple were to be rebuilt?',
    'Sociological and anthropological studies of religious communities'' responses to opportunities for ritual restoration; analysis of internal debates within rabbinic and lay communities regarding future ritual practice.',
    'If it significantly diminishes readiness, this reading, while beneficial now, could be seen as creating a long-term ''cost'' by eroding the capacity for future ritual, potentially shifting its classification towards a more complex (e.g., Tangled Rope) dynamic if future generations feel constrained by past interpretive choices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_future_ritual, empirical, 'Potential long-term impact of study-as-fulfillment on future ritual readiness.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t390, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 390, 0.05).
narrative_ontology:measurement(sacr_tr_t781, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 781, 0.05).
narrative_ontology:measurement(sacr_tr_t1172, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1172, 0.05).
narrative_ontology:measurement(sacr_tr_t1563, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1563, 0.05).
narrative_ontology:measurement(sacr_tr_t1954, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1954, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t390, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 390, 0.05).
narrative_ontology:measurement(sacr_be_t781, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 781, 0.05).
narrative_ontology:measurement(sacr_be_t1172, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1172, 0.05).
narrative_ontology:measurement(sacr_be_t1563, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1563, 0.05).
narrative_ontology:measurement(sacr_be_t1954, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1954, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(sacr_su_t390, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 390, 0.2).
narrative_ontology:measurement(sacr_su_t781, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 781, 0.2).
narrative_ontology:measurement(sacr_su_t1172, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1172, 0.2).
narrative_ontology:measurement(sacr_su_t1563, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1563, 0.2).
narrative_ontology:measurement(sacr_su_t1954, sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 1954, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__study_as_exercise_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
