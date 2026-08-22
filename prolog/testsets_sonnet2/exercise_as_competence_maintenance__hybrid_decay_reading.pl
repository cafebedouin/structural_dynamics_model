% ============================================================================
% CONSTRAINT STORY: exercise_as_competence_maintenance__hybrid_decay_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_exercise_as_competence_maintenance__hybrid_decay_reading, []).

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
 *   constraint_id: exercise_as_competence_maintenance__hybrid_decay_reading
 *   human_readable: Simulation-Based Competence Maintenance (Hybrid Decay Reading)
 *   domain: safety_engineering/organizational_learning/crisis_preparedness
 *
 * SUMMARY:
 *   Responder organizations (emergency management, aviation, nuclear
 *   operations, military, hospital crisis teams) maintain competence through
 *   recurring scripted simulation exercises, which are then used to satisfy
 *   regulatory and organizational readiness requirements. This reading holds
 *   that the underlying competence kernel is not a single thing but two: a
 *   proceduralized component (checklists, equipment operation, communication
 *   protocols) that scripted simulation genuinely exercises and maintains,
 *   and a judgment/improvisation component (recognizing when the script does
 *   not apply, adapting under genuine uncertainty and stakes) that scripted
 *   simulation does not exercise and that consequently decays even as
 *   certified 'readiness' scores remain high or improve. Over time the
 *   exercise regime drifts toward certifying and rewarding the proceduralized
 *   component more heavily — because it is the component that is easy to
 *   script, schedule, and pass/fail-grade — while the judgment component,
 *   harder to design for and riskier to expose failure on, is progressively
 *   under-exercised. The gap surfaces specifically when real incidents
 *   diverge from any rehearsed script, and the people who pay for that gap
 *   (frontline responders professionally, the public physically) are not the
 *   people whose institutional interests the exercise program serves.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58).
domain_priors:suppression_score(exercise_as_competence_maintenance__hybrid_decay_reading, 0.52).
domain_priors:theater_ratio(exercise_as_competence_maintenance__hybrid_decay_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(exercise_as_competence_maintenance__hybrid_decay_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(exercise_as_competence_maintenance__hybrid_decay_reading, tangled_rope).
narrative_ontology:human_readable(exercise_as_competence_maintenance__hybrid_decay_reading, "Simulation-Based Competence Maintenance (Hybrid Decay Reading)").
narrative_ontology:topic_domain(exercise_as_competence_maintenance__hybrid_decay_reading, "safety_engineering/organizational_learning/crisis_preparedness").

domain_priors:requires_active_enforcement(exercise_as_competence_maintenance__hybrid_decay_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(exercise_as_competence_maintenance__hybrid_decay_reading, '2361d95a-ad0a-49fd-b6a8-2ce0cf55f209').
narrative_ontology:cs_kernel_codification('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', distributed).
narrative_ontology:cs_authority_grounding('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', practice).
narrative_ontology:cs_interpretation_layer_present('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209').
narrative_ontology:cs_reading_relation('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', exercise_as_competence_maintenance__simulation_sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', exercise_as_competence_maintenance__lived_catastrophe_necessity_reading, influences).
narrative_ontology:cs_axiom('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', foundational, competence_kernel_is_dual_component).
narrative_ontology:cs_axiom_status(competence_kernel_is_dual_component, holdable).
narrative_ontology:cs_axiom_grounding('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', competence_kernel_is_dual_component, empirically_contingent).
narrative_ontology:cs_axiom('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', foundational, judgment_under_stakes_requires_unscripted_exercise).
narrative_ontology:cs_axiom_status(judgment_under_stakes_requires_unscripted_exercise, holdable).
narrative_ontology:cs_axiom_grounding('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', judgment_under_stakes_requires_unscripted_exercise, empirically_contingent).
narrative_ontology:cs_reference_frame('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', procedural_and_judgment_dual_competence_standard).
narrative_ontology:cs_drift_state('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', contemporary_certification_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2361d95a-ad0a-49fd-b6a8-2ce0cf55f209', '').
narrative_ontology:cs_kernel_id(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_as_competence_maintenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_program_administrators).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_compliance_officers).
narrative_ontology:constraint_beneficiary(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders_facing_novel_crises).
narrative_ontology:constraint_victim(exercise_as_competence_maintenance__hybrid_decay_reading, public_harmed_by_improvisation_failures).
narrative_ontology:constraint_vindicates(exercise_as_competence_maintenance__hybrid_decay_reading, procedural_readiness_is_measurable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design, schedule, and certify the tabletop and simulation exercises that satisfy regulatory and organizational readiness mandates. They select scenarios, set pass criteria, and issue certifications. Their institutional standing and budget depend on exercises running smoothly and generating documented 'readiness' — not on whether responders handle a genuinely unscripted event well, since that outcome is rarely attributable back to the exercise program specifically.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_program_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Require documented exercise completion as the legally sufficient proof of organizational preparedness. Checking the exercise box discharges their oversight obligation regardless of whether the exercise built procedural competence, judgment, or neither. They bear no cost when exercises fail to transfer to real crises, since liability attaches to exercise non-completion, not exercise inadequacy.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, regulatory_compliance_officers, beneficiary,
    institutional, biographical, arbitrage, national).

% Point to completed exercises as evidence of due diligence in budget hearings, insurance renewals, and post-incident reviews. They benefit from the exercise regime's legitimating function even when they privately suspect it does not prepare responders for genuinely novel scenarios; if a real crisis goes badly, exercise completion provides a defensible paper trail.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(exercise_as_competence_maintenance__hybrid_decay_reading, organizational_leadership, agenda_setter).

% Drill scripted scenarios repeatedly, which sharpens procedural recall (checklists, equipment handling, communication protocols) but leaves them under-practiced at the improvisational judgment a real, unscripted, high-stakes event demands. When a crisis diverges from any rehearsed script, they must exercise judgment they have had few genuine opportunities to develop, and they personally bear the professional and psychological cost of failures that follow.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, frontline_responders_facing_novel_crises, payer,
    moderate, immediate, trapped, local).

% Are the people on the receiving end when responders, well-drilled in procedure but under-exercised in judgment, mishandle the improvisational component of a real disaster. They have no visibility into the exercise regime's design and no voice in whether it targets the right competence — they experience only the downstream consequence.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, public_harmed_by_improvisation_failures, payer,
    powerless, immediate, trapped, regional).

% Specialists in adaptive, free-play, and red-team exercise design who argue that judgment-under-stakes requires deliberately unscripted, adversarial, high-ambiguity exercise formats rather than scripted tabletop drills. Their recommendations are frequently overridden because free-play exercises are harder to schedule, harder to certify pass/fail, and more likely to produce visible failures during the exercise itself — a reputational risk administrators prefer to avoid.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, exercise_design_experts, excluded,
    moderate, biographical, constrained, national).

% Conduct after-action analysis following real crises, comparing what the exercise regime certified against what the incident actually demanded. They are among the few parties positioned to observe the gap between procedural competence and judgment-under-stakes directly, though their findings are advisory and rarely restructure the exercise regime itself.
narrative_ontology:constraint_stakeholder(exercise_as_competence_maintenance__hybrid_decay_reading, post_incident_review_boards, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(exercise_as_competence_maintenance__hybrid_decay_reading, diffuse).
narrative_ontology:fixing_cost_class(exercise_as_competence_maintenance__hybrid_decay_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, schedulable, certifiable mechanism for maintaining procedural competence across a responder workforce — checklists, equipment familiarity, communication chains — which genuinely does decay without periodic rehearsal and genuinely is exercised by scripted simulation.
% TRANSFER_FUNCTION: Moves organizational liability and regulatory risk from leadership and compliance officers onto a paper record of exercise completion, while the residual risk from unexercised judgment-under-stakes is moved onto frontline responders (who bear professional consequences) and the public (who bear physical/safety consequences) when a real crisis diverges from any rehearsed script.
% ABSENT_VOICES: Exercise design experts advocating free-play and adversarial formats are structurally excluded from setting program parameters because their recommended formats are harder to certify and carry visible failure risk during the exercise; the public harmed by downstream improvisation failures has no representation in exercise design at all.
% DISAPPEARANCE_RATIONALE: If the exercise regime vanished, procedural competence (the genuinely-exercised component) would measurably decay — the coordination function is real and its removal would be felt. But if only the compliance-certification function vanished while genuine adaptive training replaced it, administrators and compliance officers would lose their liability shield while frontline responders and the public might be substantially better served. Whether the world 'rearranges' or 'stays the same' depends on which component of the kernel is asked about, which is exactly this reading's structural claim.
% FOUNDING_PROBLEM: Responder organizations needed a way to prevent procedural skill decay between real incidents and to demonstrate preparedness to regulators, insurers, and the public without waiting for an actual disaster to test readiness.
% FOUNDING_PROBLEM_CORROBORATION: Post-incident review boards, operating outside the beneficiary set, repeatedly document (in after-action reports across multiple domains) that certified-ready organizations perform well on procedural tasks during real incidents but poorly on improvisational tasks that fell outside any rehearsed script — corroborating that the founding problem is only partially solved by the current exercise format, and that the compliance-certification function has drifted from the readiness function it was built to signal.
narrative_ontology:disappearance_verdict(exercise_as_competence_maintenance__hybrid_decay_reading, contested).
narrative_ontology:founding_problem_status(exercise_as_competence_maintenance__hybrid_decay_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(exercise_as_competence_maintenance__hybrid_decay_reading, 'none', 1).
narrative_ontology:epsilon_provenance(exercise_as_competence_maintenance__hybrid_decay_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(exercise_as_competence_maintenance__hybrid_decay_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(exercise_as_competence_maintenance__hybrid_decay_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and theater ratio (0.61) are both mid-to-high and rising because the metrics track the compliance-certification half of the kernel drifting away from the readiness-production half: exercises increasingly generate paper compliance without proportionally increasing improvisational capacity, and the gap widens as the exercise design that is 'easy to certify' consistently wins over the exercise design that 'produces judgment.' Suppression (0.52) is moderate rather than severe — it operates less through coercion of responders and more through the structural bias of what gets scheduled, funded, and counted as a pass, which forecloses adaptive/free-play formats as viable defaults. Accessibility collapse (0.45) is moderate because alternative exercise designs (free-play, red-team, adversarial scenario) are known and advocated by experts but are institutionally disfavored, not physically unavailable.
 *
 * DIRECTIONALITY LOGIC:
 *   Exercise program administrators and compliance officers sit near the full-beneficiary end: they collect institutional legitimacy and discharged liability from certified exercise completion regardless of whether judgment-under-stakes actually transfers. Organizational leadership benefits similarly and has mobile exit (can relocate liability narrative even if outcomes are poor). Frontline responders are structurally trapped — their exit from the exercise regime is not a real option if they wish to remain employed in the role — and they personally absorb the professional consequences when the unexercised component of the kernel is the one a real crisis calls on. The public is powerless and trapped by definition: they cannot select which responders show up or what exercise regime prepared them.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists collapsing the exercise regime into either 'pure fraud' (which would ignore that procedural competence genuinely decays without simulation and genuinely is maintained by it) or 'pure legitimate coordination' (which would ignore that the judgment component is structurally under-served by the same mechanism claimed to maintain 'readiness' wholesale). Classifying it as tangled_rope rather than snare or rope preserves both halves: there IS a genuine coordination function (procedural competence maintenance) that would be lost if the regime vanished, AND there is asymmetric extraction (compliance/liability benefit concentrated on administrators and leadership, improvisation-failure cost concentrated on responders and the public) sustained by active enforcement (mandatory exercise completion, certification requirements). Treating the two kernel components as separable is exactly the analytical move that prevents mislabeling the whole arrangement as either innocent maintenance or pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_decomposability,
    'Is the competence kernel genuinely decomposable into a proceduralized component and a judgment component with different exercise requirements, or is this decomposition itself an artifact of what is easy versus hard to design exercises for?',
    'Controlled comparison of responder performance in scripted-only versus scripted-plus-adaptive-exercise cohorts, tracked against real-incident outcomes that specifically required deviation from rehearsed scripts; convergent evidence across multiple domains (aviation, emergency medicine, military) would support decomposability as a structural fact rather than a design artifact.',
    'If the kernel is not genuinely decomposable — if judgment is simply procedural competence at higher complexity, fully reachable by sufficiently complex simulation — this reading collapses toward simulation_sufficiency_reading and the tangled_rope classification loses its victim-generating mechanism. If decomposable, the hybrid reading and its distinct victim set stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_decomposability, conceptual, 'Whether the two-component kernel structure this reading depends on is real or a design-availability artifact.').

omega_variable(
    sibling_reading_disagreement_locus,
    'Where exactly do the three kernel readings (hybrid_decay, simulation_sufficiency, lived_catastrophe_necessity) locate their disagreement — is it about what simulation CAN achieve in principle (a technology/fidelity question) or about what judgment-under-stakes IS (a conceptual question about whether it can exist without genuine stakes)?',
    'Adjudicate via cases where simulation fidelity has been pushed to its practical maximum (full-motion, high-consequence, deceptive-injection exercises) and check whether judgment gaps persist even there; persistence would locate the disagreement in the conceptual question rather than the technology question.',
    'If the disagreement is purely technological (fidelity-bound), the simulation_sufficiency_reading is the correct long-run resting point and this hybrid reading describes a transitional state, not a stable structural fact. If conceptual (stakes are constitutive of judgment, not merely a fidelity parameter), the hybrid reading''s decay claim is durable regardless of simulation technology improvements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_disagreement_locus, conceptual, 'Whether the three-way kernel dispute is resolvable by better simulation technology or is conceptually irreducible.').

omega_variable(
    compliance_certification_capture,
    'Has the compliance-certification function of the exercise regime been actively captured by administrators to serve liability-shielding interests, or has it drifted there as an unintended side effect of what is measurable?',
    'Internal program records showing whether administrators actively selected against adaptive/free-play formats when advised of the judgment gap by exercise design experts, versus records showing the drift occurred without any such advisory moment.',
    'Active capture would push the classification toward snare (deliberate suppression of the coordination-improving alternative); unintended drift keeps tangled_rope as the more accurate reading (genuine coordination function persists, extraction is structural rather than deliberately engineered).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_certification_capture, empirical, 'Whether the compliance/readiness gap is the product of deliberate capture or unintended measurability-driven drift.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(exercise_as_competence_maintenance__hybrid_decay_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exer_tr_t0, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(exer_tr_t4, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement(exer_tr_t8, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement(exer_tr_t12, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 12, 0.49).
narrative_ontology:measurement(exer_tr_t16, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 16, 0.54).
narrative_ontology:measurement(exer_tr_t20, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(exer_tr_t24, exercise_as_competence_maintenance__hybrid_decay_reading, theater_ratio, 24, 0.61).

% Extraction over time
narrative_ontology:measurement(exer_be_t0, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(exer_be_t4, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 4, 0.43).
narrative_ontology:measurement(exer_be_t8, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(exer_be_t12, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(exer_be_t16, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(exer_be_t20, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(exer_be_t24, exercise_as_competence_maintenance__hybrid_decay_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(exer_su_t0, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(exer_su_t4, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement(exer_su_t8, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 8, 0.45).
narrative_ontology:measurement(exer_su_t12, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 12, 0.47).
narrative_ontology:measurement(exer_su_t16, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(exer_su_t20, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(exer_su_t24, exercise_as_competence_maintenance__hybrid_decay_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(exercise_as_competence_maintenance__hybrid_decay_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(exercise_as_competence_maintenance__hybrid_decay_reading, 0.1).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, simulation_sufficiency_reading).
narrative_ontology:affects_constraint(exercise_as_competence_maintenance__hybrid_decay_reading, lived_catastrophe_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the exercise_as_competence_maintenance kernel. simulation_sufficiency_reading claims the whole competence kernel is reachable by sufficiently high-fidelity simulation (no structurally distinct judgment component); lived_catastrophe_necessity_reading claims simulation never constitutes genuine exercise of any part of the kernel and only real catastrophe does. This reading (hybrid_decay_reading) occupies the structural middle: it splits the kernel into a proceduralized component simulation genuinely exercises and a judgment component simulation does not reach, producing a distinct victim set (those harmed specifically by judgment-component failures) that neither sibling reading generates in the same way. ε for this reading (0.58) reflects genuine partial coordination plus genuine partial extraction, structurally different from what either sibling would author for the same standing exercise regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
