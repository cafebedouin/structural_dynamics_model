% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: exercise_as_competence_maintenance__lived_catastrophe_necessity_reading
 *   human_readable: Lived Catastrophe Necessity: Competence Only Maintained by Real-Stakes Activation
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   This constraint is the lived-catastrophe-necessity reading of the kernel
 *   'exercise_as_competence_maintenance.' It asserts that simulation, while
 *   necessary for procedural fluency, is structurally insufficient for
 *   maintaining judgment-under-stakes — the competence kernel that prevents
 *   catastrophes. Competence decays covertly without real-stakes activation;
 *   the certification regime accepts simulation compliance as proof of
 *   competence while externalizing the real test (catastrophe) to operators
 *   and the public. The coordination function is real (simulation builds
 *   procedural discipline); the extraction function is that this coordination
 *   substitutes for the only thing that actually validates the deeper
 *   competence layer, transferring catastrophe risk to those with no voice in
 *   the regime.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68).
domain_priors:suppression_score(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.62).
domain_priors:theater_ratio(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "Lived Catastrophe Necessity: Competence Only Maintained by Real-Stakes Activation").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, '0dde59d0-99fd-41c3-814a-4f66dff76b35').
narrative_ontology:cs_kernel_codification('0dde59d0-99fd-41c3-814a-4f66dff76b35', distributed).
narrative_ontology:cs_authority_grounding('0dde59d0-99fd-41c3-814a-4f66dff76b35', practice).
narrative_ontology:cs_interpretation_layer_present('0dde59d0-99fd-41c3-814a-4f66dff76b35').
narrative_ontology:cs_reading_relation('0dde59d0-99fd-41c3-814a-4f66dff76b35', exercise_as_competence_maintenance__simulation_sufficiency_reading, forecloses).
narrative_ontology:cs_reading_relation('0dde59d0-99fd-41c3-814a-4f66dff76b35', exercise_as_competence_maintenance__hybrid_decay_reading, influences).
narrative_ontology:cs_axiom('0dde59d0-99fd-41c3-814a-4f66dff76b35', foundational, judgment_under_stakes_requires_real_consequence).
narrative_ontology:cs_axiom_status(judgment_under_stakes_requires_real_consequence, holdable).
narrative_ontology:cs_axiom_grounding('0dde59d0-99fd-41c3-814a-4f66dff76b35', judgment_under_stakes_requires_real_consequence, empirically_contingent).
narrative_ontology:cs_axiom('0dde59d0-99fd-41c3-814a-4f66dff76b35', foundational, simulation_cannot_replicate_stake_structure).
narrative_ontology:cs_axiom_status(simulation_cannot_replicate_stake_structure, holdable).
narrative_ontology:cs_axiom_grounding('0dde59d0-99fd-41c3-814a-4f66dff76b35', simulation_cannot_replicate_stake_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('0dde59d0-99fd-41c3-814a-4f66dff76b35', post_bhopal_competence_crisis).
narrative_ontology:cs_drift_state('0dde59d0-99fd-41c3-814a-4f66dff76b35', contemporary_simulation_dominance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0dde59d0-99fd-41c3-814a-4f66dff76b35', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, regulatory_bodies).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, insurance_underwriters).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_operators_with_catastrophe_experience).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators_without_catastrophe_exposure).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_exposed_to_untested_systems).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_personnel_in_high_hazard_sectors).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizations_relying_on_simulation_alone).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, competence_requires_real_stakes_activation).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_is_insufficient_for_judgment_retention).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, covert_competence_decay_without_catastrophe_exercise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set competence standards and certification regimes that accept simulation hours as proxies for competence while structurally depending on real catastrophes to validate those regimes. They benefit from the legitimacy simulation-based certification provides while the actual competence test — catastrophe — remains externalized to operators and the public.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, regulatory_bodies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, regulatory_bodies, beneficiary).

% Price risk based on certification records that reflect simulation compliance, not demonstrated catastrophe competence. They benefit from the information asymmetry: premiums reflect paper competence while actual competence (which determines loss) is only revealed by catastrophe. Their exit is mobile — they can reprice or withdraw after losses materialize.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, insurance_underwriters, beneficiary,
    organized, biographical, mobile, global).

% Hold the only validated competence — having survived real catastrophes. They command premium positions, consulting fees, and authority precisely because their competence has been tested. Their exit is arbitrage-grade: they can sell their validated competence across sectors and geographies.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, senior_operators_with_catastrophe_experience, beneficiary,
    powerful, biographical, arbitrage, regional).

% Carry certification and simulation compliance but have never faced real-stakes activation. They bear the risk of covert competence decay — their judgment under pressure is untested. Their exit is constrained: leaving the role means career disruption; staying means bearing unquantified risk of failure when catastrophe arrives.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, frontline_operators_without_catastrophe_exposure, payer,
    moderate, biographical, constrained, local).

% Live downstream of systems operated by personnel whose competence has only been simulated. They bear the consequences of competence decay — chemical releases, transport disasters, medical errors — with no voice in certification standards and no exit from the exposure. Their situation is structurally trapped: they cannot opt out of the infrastructure, healthcare, or transport systems they depend on.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_exposed_to_untested_systems, payer,
    powerless, immediate, trapped, local).

% Enter professions believing simulation builds real competence, then discover the gap only when catastrophe strikes or when they observe senior operators' judgment that simulation never taught. Their professional identity is fused to the certification pathway — leaving means abandoning their vocational self-concept. They are excluded from the competence validation conversation: the system certifies them as competent while withholding the only test that would validate it.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_personnel_in_high_hazard_sectors, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, junior_personnel_in_high_hazard_sectors, excluded).

% Invest heavily in simulation infrastructure, training hours, and compliance documentation — believing this maintains competence. They bear the financial cost of the simulation regime AND the latent risk of competence decay. Their exit is constrained: regulatory and insurance regimes require simulation compliance; abandoning it invites sanction while continuing it masks the competence gap.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organizations_relying_on_simulation_alone, payer,
    organized, biographical, constrained, regional).

% Study the simulation-competence gap, the decay curves of judgment under non-activation, and the structural incentives that maintain simulation-as-proxy. They see the full structure: the coordination function (simulation builds procedural fluency) and the extraction function (simulation compliance substitutes for catastrophe-tested competence, externalizing the real test to operators and the public).
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, safety_science_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Simulation provides necessary procedural rehearsal — checklist discipline, communication protocols, system familiarity — that prevents elementary failures and creates a shared operational language across teams and shifts.
% TRANSFER_FUNCTION: Moves the burden of competence validation from the certification regime (which accepts simulation as proof) onto frontline operators and the exposed public. The certification regime collects legitimacy and compliance revenue; operators and public pay with untested judgment and catastrophe consequences.
% ABSENT_VOICES: The public exposed to untested systems has no seat at certification standard-setting. Junior personnel who will discover the competence gap only at catastrophe are excluded from the simulation-sufficiency debate. Victims of past catastrophes whose deaths revealed the simulation gap are structurally silenced — their testimony is treated as 'lessons learned' rather than falsification of the simulation-sufficiency claim.
% DISAPPEARANCE_RATIONALE: If the lived-catastrophe-necessity reading disappeared — i.e., if simulation were genuinely accepted as sufficient — certification regimes would stop requiring any real-stakes exposure, simulation hours would fully substitute for operational experience, and the covert decay of judgment-under-stakes would accelerate without even the periodic correction of actual catastrophes. The world would rearrange: more frequent catastrophes with worse outcomes because the last residual competence anchor (catastrophe as teacher) would be formally severed.
% FOUNDING_PROBLEM: After early industrial catastrophes (e.g., Texas City 1947, Flixborough 1974, Bhopal 1984), it became clear that procedural compliance alone did not prevent disasters. The founding problem was: how to build and maintain the judgment-under-pressure that only real emergencies teach, without waiting for the next catastrophe to teach it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by multiple independent sources outside the benefiting parties: accident investigation boards (CSB, AAIB, NTSB) consistently find 'normalization of deviance' and 'drift into failure' in certified organizations; resilience engineering researchers (Hollnagel, Woods, Dekker) document the simulation-competence gap empirically; frontline operator unions and professional associations have testified that simulation does not replicate the cognitive load of real stakes. The simulation-sufficiency reading is primarily advanced by regulatory bodies and training vendors who benefit from the certification regime.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, world_rearranges).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the transfer of competence-validation burden from the certifying institutions to operators/public. Suppression (0.62) is the active maintenance of simulation-as-sufficient: certification audits check simulation hours, not judgment quality; regulators cite simulation compliance as due diligence; insurers price on paper records. Theater ratio (0.55) is high because an increasing share of simulation activity is performative — scenario scripting for audit trails, 'no-fault' debriefs that avoid judgment critique, fidelity investments that mimic sensory inputs but not stake-structure. Accessibility collapse (0.42) is moderate: alternatives exist (apprenticeship to catastrophe-experienced operators, controlled real-stakes exercises, red-teaming with genuine consequences) but are structurally marginalized by the certification regime. Resistance (0.35) is low because the extracted parties (frontline operators, public, junior personnel) are fragmented, powerless, or identity-locked into the system.
 *
 * PERSPECTIVAL GAP:
 *   From the regulatory/agenda-setter seat, the constraint appears as a rope: simulation coordinates procedural competence across a distributed workforce. From the frontline operator and public seats, it computes as a snare: the coordination story is cover for extracting catastrophe-risk-bearing from those with no say. The engine computes this divergence from the structural data — the claimed_type (tangled_rope) names the hybrid honestly.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory bodies and insurers are structural beneficiaries: they collect legitimacy, compliance revenue, and premium income while externalizing the competence test. Senior operators with catastrophe experience are beneficiaries with arbitrage exit — they sell validated competence. Frontline operators without exposure, junior personnel, and the public are payers: they bear the covert decay risk and catastrophe consequences with constrained or trapped exit. Organizations relying on simulation alone are payers bearing both simulation costs and latent catastrophe risk. The directionality gradient runs from institutional beneficiaries (d ≈ 0.1) through organized beneficiaries (d ≈ 0.25) to moderate/powerless payers (d ≈ 0.7–0.95).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (building judgment without catastrophe) was real and live. The simulation regime was a genuine scaffold — but it lacked a sunset clause and mutated into a tangled rope: it coordinates procedural fluency (genuine function) while extracting judgment-validation from those it certifies (asymmetric extraction). The mandatrophy is unresolved: the simulation regime persists because it solves the coordination problem for institutions (certification, insurance, compliance) while the judgment-competence problem it was built for remains unsolved — and the regime actively suppresses alternatives that would test judgment under real stakes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    simulation_fidelity_threshold,
    'Is there a fidelity threshold above which simulation genuinely exercises judgment-under-stakes, or is the stake-structure itself irreducible (no simulation can replicate the neurocognitive state of real consequence)?',
    'Controlled studies comparing judgment quality after high-fidelity simulation vs. controlled real-stakes exercises (e.g., military live-fire, medical simulation with patient actors vs. supervised live procedures); neurocognitive markers of stake-processing (cortisol, dopamine, amygdala activation) in simulated vs. real emergencies.',
    'If a fidelity threshold exists, the constraint''s extraction is partially reducible by engineering; if stake-structure is irreducible, the extraction is structural — the constraint is a fundamental limit on simulation-based certification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(simulation_fidelity_threshold, empirical, 'Whether simulation can ever cross the stake-structure threshold for judgment exercise.').

omega_variable(
    covert_decay_measurement,
    'Can covert competence decay (judgment atrophy without procedural degradation) be measured before catastrophe reveals it?',
    'Longitudinal tracking of operator judgment quality in organizations with known catastrophe-experience vs. simulation-only cohorts; development of leading indicators (decision latency under ambiguity, error recognition speed, deviation detection rate) that correlate with later catastrophe performance.',
    'If measurable, the constraint''s suppression could be reduced by making decay visible to certification; if inherently latent until catastrophe, the suppression is structural — the regime cannot audit what only catastrophe reveals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(covert_decay_measurement, empirical, 'Whether the extraction''s hiddenness is contingent or structural.').

omega_variable(
    reading_framing_underdetermination,
    'Does the kernel ''exercise_as_competence_maintenance'' refer to a unitary competence that either is or isn''t exercised by simulation, or does it inherently contain distinct components (procedural fluency vs. judgment-under-stakes) with different exercise requirements?',
    'Genealogical analysis of the kernel''s formation: did the founding problem (post-catastrophe competence building) conceive of a unitary competence, or was the procedural/judgment distinction present from the start? Cross-reading comparison of how each reading operationalizes ''competence.''',
    'If the kernel is inherently bipartite, the hybrid_decay_reading is the structurally faithful description and this reading''s claim of unitary ''competence kernel'' is a framing artifact. If unitary, this reading and simulation_sufficiency_reading are genuine forecloses pairs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the kernel''s internal structure resolves or sustains the reading contest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t1970, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(exer_tr_t1985, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(exer_tr_t1995, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(exer_tr_t2005, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(exer_tr_t2015, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(exer_tr_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, theater_ratio, 2025, 0.55).

% Extraction over time
narrative_ontology:measurement(exer_be_t1970, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(exer_be_t1985, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(exer_be_t1995, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(exer_be_t2005, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(exer_be_t2015, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2015, 0.63).
narrative_ontology:measurement(exer_be_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t1970, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(exer_su_t1985, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1985, 0.48).
narrative_ontology:measurement(exer_su_t1995, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(exer_su_t2005, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement(exer_su_t2015, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(exer_su_t2025, exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, 0.08).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, simulation_certification_regime).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, high_hazard_operator_licensing).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, public_protection_liability_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the exercise_as_competence_maintenance kernel. The simulation_sufficiency_reading claims simulation fidelity constitutes genuine exercise; the hybrid_decay_reading splits the kernel into procedural (simulation-exercisable) and judgment (catastrophe-only) components. All three readings share the kernel_id but instantiate different constraints with different ε, different victim sets, and different structural dynamics. This decomposition follows the ε-invariance principle: the label 'competence maintenance through exercise' covers structurally distinct claims about what exercise means and what competence is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, organized, 0.25).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, moderate, 0.72).
constraint_indexing:directionality_override(exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
