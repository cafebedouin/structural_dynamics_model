% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Messianic Suspension of Sacrificial Obligation
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint instantiates the messianic suspension reading of the
 *   sacrifice obligation kernel: the biblical commandment to offer sacrifices
 *   is understood not as annulled but as divinely suspended since the
 *   Temple's destruction, with study of sacrificial law functioning as
 *   instrumental maintenance of operational readiness rather than as
 *   substitutive fulfillment. The beneficiary is future generations who will
 *   inherit the preserved competence; there is no victim set during the
 *   suspension period because no party is coerced into performance or
 *   extraction. The reading is claimed as scaffold â a transitional
 *   coordination structure whose justification is the future restoration, not
 *   the permanent steady state of study.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Agenda-setter (institutional/identity_locked) â administers suspension doctrine and directs study curriculum
 *   - future_generations: Beneficiary (powerless/trapped) â receives preserved operational knowledge for restoration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, scaffold).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Messianic Suspension of Sacrificial Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

narrative_ontology:has_sunset_clause(sacrifice_obligation_kernel__messianic_suspension_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '83962b1f-f99b-4874-a990-6aa87ae2ae31').
narrative_ontology:cs_kernel_codification('83962b1f-f99b-4874-a990-6aa87ae2ae31', fixed_text).
narrative_ontology:cs_authority_grounding('83962b1f-f99b-4874-a990-6aa87ae2ae31', lineage).
narrative_ontology:cs_interpretation_layer_present('83962b1f-f99b-4874-a990-6aa87ae2ae31').
narrative_ontology:cs_reading_relation('83962b1f-f99b-4874-a990-6aa87ae2ae31', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('83962b1f-f99b-4874-a990-6aa87ae2ae31', sacrifice_obligation_kernel__performance_only_reading, influences).
narrative_ontology:cs_reading_relation('83962b1f-f99b-4874-a990-6aa87ae2ae31', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('83962b1f-f99b-4874-a990-6aa87ae2ae31', foundational, divine_suspension_active).
narrative_ontology:cs_axiom_status(divine_suspension_active, holdable).
narrative_ontology:cs_axiom_grounding('83962b1f-f99b-4874-a990-6aa87ae2ae31', divine_suspension_active, theological).
narrative_ontology:cs_axiom('83962b1f-f99b-4874-a990-6aa87ae2ae31', foundational, study_as_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_as_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('83962b1f-f99b-4874-a990-6aa87ae2ae31', study_as_readiness_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('83962b1f-f99b-4874-a990-6aa87ae2ae31', temple_cult_active).
narrative_ontology:cs_drift_state('83962b1f-f99b-4874-a990-6aa87ae2ae31', rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('83962b1f-f99b-4874-a990-6aa87ae2ae31', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_temporal_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares and administers the halakhic ruling that the biblical sacrificial obligation is divinely suspended until messianic restoration; directs curriculum, jurisprudence, and communal pedagogy toward the study of sacrificial law as operational maintenance rather than fulfillment; authority derives from continuity with Talmudic lineage and interpretive tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, rabbinic_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Will inherit the preserved technical, textual, and procedural competence necessary to resume Temple sacrifices upon messianic restoration; the suspension reading stores operational knowledge across the interregnum for their eventual use.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the technical, textual, and procedural knowledge necessary to resume biblical sacrificial worship upon messianic restoration, preventing total loss of operational competence during the interregnum.
% TRANSFER_FUNCTION: Moves rabbinic interpretive labor and student cognitive effort into the maintenance and transmission of sacrificial law texts and protocols, storing that capacity across generations for delivery at the point of restoration.
% ABSENT_VOICES: Temple Mount activist movements and halakhic minorities who argue for immediate physical performance of sacrifices in the present era are excluded from the consensus; they would reject the suspension doctrine as an illegitimate deferral of a live obligation.
% DISAPPEARANCE_RATIONALE: If the suspension reading vanished, the rabbinic curriculum would need to either treat sacrifice law as a currently fulfillable obligation (requiring performance or a substitutive framework like study-as-exercise) or abandon its halakhic centrality; the present arrangement of waiting, study, and messianic deferral would collapse.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE created a crisis of continuity for a central biblical commandment that required an altar and priesthood.
% FOUNDING_PROBLEM_CORROBORATION: Historical attestation in Mishnah Avot and Talmudic tractates corroborates the destruction as the triggering crisis; secular historians confirm the Temple's destruction. The suspension reading itself is contested by sibling readings within the halakhic corpus, but the founding event is uncontested outside the beneficiary framework.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the constraint does not extract labor or resources from a victim class; it mobilizes study toward a deferred collective good. Suppression is low (0.12) because the suspension is not maintained by coercion against alternatives but by halakhic consensus and the practical impossibility of Temple worship. Theater ratio is low (0.10) because study serves a genuine preservation function; it is not performative maintenance of a dead practice. Accessibility collapse is moderate (0.45) because accepting the suspension reading makes immediate sacrifice unthinkable within the framework, though alternative readings (study-as-exercise, performance activism) persist in the broader discourse. Resistance is low (0.20) because movements rejecting the suspension are marginal, though they exist. The temporal series shows stable, mildly rising extraction as the rabbinic curriculum institutionalizes sacrificial study over centuries, but the level remains low.
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic agenda-setter seat experiences the constraint as custodial authority preserving a commandment for future realization; the future-generations beneficiary seat experiences it as inherited infrastructure they did not choose but will depend upon. The engine will compute a near-symmetric or lightly beneficiary-tilted directionality for the rabbinic seat (they bear the labor of maintenance but gain institutional legitimacy) and a beneficiary directionality for future generations. No payer seat exists in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Future generations are the declared beneficiary (d near 0.0). Rabbinic authority is the agenda-setter: they are not victims (no extraction) but they are identity-locked into the interpretive tradition (d moderate, near 0.4-0.5). There is no concentrated victim seat. The engine will derive low effective extraction overall because no agent is declared as bearing asymmetric cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling this as a snare or piton: the constraint has a declared sunset (messianic restoration), a genuine coordination function (preservation of operational knowledge), and no identifiable victim set. If the messianic horizon were treated as indefinitely deferred with no sunset, the constraint would drift toward piton (theatrical maintenance). The authored metrics and the founding_problem_status (live) support the scaffold claim against mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_sunset_deferral,
    'Is the messianic restoration horizon a determinate sunset condition for this scaffold, or an indefinitely deferred endpoint that renders the arrangement functionally permanent?',
    'Comparative analysis across Jewish history of messianic expectation half-lives; if restoration remains unrealized across multiple civilizational cycles, the scaffold classification yields to piton detection.',
    'If indefinitely deferred, the constraint drifts from scaffold toward piton â the study function becomes theatrical maintenance of a permanently deferred practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_sunset_deferral, conceptual, 'Whether the messianic sunset is a real transition or a permanent deferral').

omega_variable(
    custodial_authority_benefit,
    'Does the suspension reading concentrate juridical and pedagogical authority over an unperformable commandment in the present rabbinic establishment, creating extraction disguised as custodianship?',
    'Analysis of resource flows: whether rabbinic institutions capture status, enrollment, or material support specifically attributable to their monopoly on sacrificial law interpretation.',
    'If present authority captures significant current benefit, the low-extraction reading is partial; a tangled_rope or false-summit classification would be indicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custodial_authority_benefit, empirical, 'Whether present rabbinic authority extracts from the suspension doctrine').

omega_variable(
    suspension_vs_transformation,
    'Is the divine suspension a genuine pause with automatic resumption, or has the obligation been normatively transformed such that study becomes its permanent form?',
    'Internal halakhic analysis of whether the suspension mechanism includes automatic triggers or requires new juridical activation at restoration.',
    'If transformed, the reading collapses into study_as_exercise or symbolic_archive; the scaffold claim requires a genuine resumption mechanics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_transformation, conceptual, 'Whether suspension entails pure pause or normative transformation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(sacr_tr_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1000, 0.12).
narrative_ontology:measurement(sacr_be_t2000, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 2000, 0.15).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__messianic_suspension_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__messianic_suspension_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the sacrifice_obligation_kernel. It decomposes the colloquial 'sacrifice obligation' into structurally distinct claims: whether the obligation is suspended, substitutively fulfilled by study, still requiring physical performance, or merely archival. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
