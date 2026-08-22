% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__state_enforced_creation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: quran_ontological_status__state_enforced_creation_reading
 *   human_readable: State-Enforced Created Qur'an Doctrine (Mihna)
 *   domain: theological/political
 *
 * SUMMARY:
 *   This constraint is the state_enforced_creation reading of the
 *   quran_ontological_status kernel. It instantiates the historical episode
 *   (833â848 CE) in which the Abbasid caliphate adopted the Mu'tazilite
 *   theological position that the Qur'an is created (makhlÅ«q) and enforced
 *   it through the mihna inquisition tribunals. Unlike the pure theological
 *   created_reading (a metaphysical claim advanced by rationalist scholars)
 *   and the uncreated_reading (the traditionalist antipode), this reading is
 *   defined by the superimposition of state coercive power onto the
 *   theological claim. The result is a snare: the createdness doctrine
 *   functions as a loyalty test and purge mechanism, extracting compliance
 *   from traditionalist scholars and literalist communities while
 *   consolidating caliphal authority. The claim/metric independence is
 *   maintained: the constraint is structurally a snare (high extraction, high
 *   suppression, active enforcement, identifiable victims) even though the
 *   underlying theological proposition may have been held sincerely by some
 *   Mu'tazilites.
 *
 * KEY AGENTS:
 *   - Caliphal authority (agenda_setter/institutional/mobile): enforces created-Qur'an doctrine via mihna tribunals to consolidate theological-political control.
 *   - Mu'tazilite school (beneficiary/organized/constrained): receives temporary state backing as doctrinal legitimizers; loses institutional position when state favor withdraws.
 *   - Traditionalist scholars (payer/moderate/identity_locked): bear imprisonment and torture for refusing doctrinal affirmation; identity fused with uncreated-Qur'an theology.
 *   - Literalist communities (payer/powerless/identity_locked): devotional communities subordinated to state-imposed doctrine; religious identity makes exit unthinkable.
 *   - Independent jurists (excluded/moderate/constrained): sidelined by the collapse of theological dispute into a binary loyalty test.
 *   - Later historical observers (observer/analytical/analytical): analyze the mihna as a case of political instrumentalization of theology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.88).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.92).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Created Qur'an Doctrine (Mihna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, '2fe28882-cb2f-4bd8-a0e5-1b591e2da980').
narrative_ontology:cs_kernel_codification('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', fixed_text).
narrative_ontology:cs_authority_grounding('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', extraction).
narrative_ontology:cs_interpretation_layer_present('2fe28882-cb2f-4bd8-a0e5-1b591e2da980').
narrative_ontology:cs_reading_relation('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', foundational, caliphal_theological_jurisdiction_over_revelation_status).
narrative_ontology:cs_axiom_status(caliphal_theological_jurisdiction_over_revelation_status, overridden).
narrative_ontology:cs_axiom_grounding('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', caliphal_theological_jurisdiction_over_revelation_status, conventional).
narrative_ontology:cs_axiom('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', secondary, coerced_affirmation_as_legitimate_orthodoxy_test).
narrative_ontology:cs_axiom_status(coerced_affirmation_as_legitimate_orthodoxy_test, overridden).
narrative_ontology:cs_axiom_grounding('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', coerced_affirmation_as_legitimate_orthodoxy_test, conventional).
narrative_ontology:cs_reference_frame('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', caliphal_theological_supremacy_framework).
narrative_ontology:cs_drift_state('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', post_mihna_restoration, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2fe28882-cb2f-4bd8-a0e5-1b591e2da980', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_school).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the theological line that the Qur'an is created and enforces it through the mihna tribunals. Uses the doctrine to consolidate religious authority under caliphal control, purging scholars who refuse public affirmation. Can abandon the doctrine by decree when political utility expires.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, mobile, national).

% Receives temporary state backing as the doctrinal legitimizers of the created-Qur'an position; their scholars serve as interrogators and theological authorities in the mihna. Their institutional dominance is entirely dependent on caliphal favor and collapses when the state withdraws support.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_school, beneficiary,
    organized, biographical, constrained, national).

% Bear the direct costs of the mihna: interrogated, imprisoned, and tortured for refusing to affirm the createdness of the Qur'an. Their scholarly and religious identity is fused with the uncreated-Qur'an doctrine, making public capitulation structurally equivalent to self-annihilation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    moderate, biographical, identity_locked, national).

% Devotional communities whose religious practice and self-understanding depend on the Qur'an as uncreated divine speech. State enforcement severs their access to traditional scholarly leadership and forces subordination to a doctrine they experience as heretical imposition.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    powerless, generational, identity_locked, regional).

% Jurists and mediators who might otherwise adjudicate theological plurality are sidelined because the state has collapsed the ontological dispute into a binary loyalty test. Their exclusion is necessary to maintain the illusion of unanimous compliance.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, independent_jurists, excluded,
    moderate, biographical, constrained, national).

% Subsequent historians and theologians who analyze the mihna as a canonical case of political instrumentalization of theological doctrine. They observe the structural divergence between the Mu'tazilite metaphysical claim and its state enforcement function.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, later_historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly coordinates a unified imperial theology around divine transcendence by resolving the Qur'an's ontological status, protecting tawhid through a single state-backed doctrine.
% TRANSFER_FUNCTION: Moves compliance, loyalty, and doctrinal control from traditionalist scholars and literalist communities to the caliphal state and its allied rationalist school; transfers physical security, scholarly standing, and communal autonomy away from dissenters.
% ABSENT_VOICES: Traditionalist scholars imprisoned or silenced under interrogation; independent jurists who would advocate for theological pluralism rather than binary enforcement; the uncreated-reading scholarly community whose position is criminalized rather than debated.
% DISAPPEARANCE_RATIONALE: If the state-enforced created-Qur'an doctrine and its mihna machinery vanished overnight, traditionalist scholars would resume public teaching and judicial posts, the caliphate would lose its theological enforcement instrument, and the Mu'tazilite school would revert to intellectual contestation rather than state-backed dominance. The scholarly and political landscape of the Abbasid empire would reorganize around de facto pluralism.
% FOUNDING_PROBLEM: The Mu'tazilite theological problem of divine transcendence â if the Qur'an is uncreated, it risks co-eternity with God, threatening tawhid â coupled with the Abbasid caliphal problem of consolidating religious authority under centralized state control.
% FOUNDING_PROBLEM_CORROBORATION: Later Sunni chroniclers and traditionalist historians (e.g., al-Tabari) attest the mihna's termination and the restoration of the uncreated-reading as dominant; their testimony comes from outside the caliphal and Mu'tazilite beneficiary circle. No independent corroboration from the benefiting parties exists for the claim that the problem required state inquisition to solve.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.88, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__state_enforced_creation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__state_enforced_creation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) because the constraint extracts physical safety, scholarly standing, communal autonomy, and doctrinal freedom from its targets. Suppression is higher still (0.92) because the arrangement requires active inquisitorial enforcement, imprisonment, and flogging to persist; it cannot survive without coercion. Theater is moderate-high (0.55) because while the Mu'tazilite metaphysical claim may be sincerely held, its function in the mihna is increasingly performative â a public orthodoxy test masking political purification. Accessibility collapse is high (0.80) because the state criminalizes the alternative (uncreated-reading) and excludes mediators. Resistance is high (0.78) due to Ahmad ibn Hanbal's sustained refusal and the broader traditionalist resilience. The temporal series show extraction and suppression rising as the mihna matures, with theater accumulating as the political function eclipses the theological rationale.
 *
 * PERSPECTIVAL GAP:
 *   The caliphal seat computes the constraint as necessary coordination of imperial theological unity; the traditionalist seat computes it as violent extraction and heretical overreach. The Mu'tazilite seat experiences temporary ascendancy that turns into historical liability by association. The engine computes this divergence from the structural data â the same tribunals and texts produce opposite directionalities depending on whether the agent is a beneficiary of political control or a target of purgation.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority sits at the beneficiary end (low d): the constraint subsidizes its political and theological control. Traditionalist scholars sit at the full-target end (high d): the constraint extracts their compliance, physical security, and scholarly standing. Literalist communities are also high-d targets. The Mu'tazilite school sits between beneficiary and symmetric: they gain institutional standing but are constrained by state dependency, making their effective extraction low but their exit brittle. Independent jurists are excluded from the derivation set. Historical observers are analytical with no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mihna's founding problem was twofold: the Mu'tazilite theological concern for divine transcendence and the Abbasid caliphal need to centralize religious authority. The state-enforced reading collapsed these into a single coercive mechanism. Mandatrophy is resolved because the caliphate eventually abandoned the mihna (al-Mutawakkil, c. 848 CE), acknowledging that the political cost of enforcement exceeded its benefit and that the mechanism had outlived its function. This prevents misclassifying the underlying Mu'tazilite metaphysics â which in the pure created_reading might function as a rope or tangled_rope in scholarly debate â as inherently a snare. The snare is specifically the state enforcement layer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_doctrine_vs_political_instrument,
    'Is the created-Qur''an doctrine enforced by the mihna a theological position that was politically instrumentalized, or was it always primarily a political instrument using theological vocabulary?',
    'Historical analysis of pre-Mihna Mu''tazilite texts versus state documents from the Mihna period; compare doctrinal content before and during state enforcement to isolate the political layer.',
    'If primarily a political instrument, classification as snare is reinforced; if a sincere doctrine hijacked by the state, the underlying constraint family includes a less extractive created_reading that is contaminated by this snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_doctrine_vs_political_instrument, conceptual, 'Whether the enforced doctrine is theology or political cover.').

omega_variable(
    kernel_reading_separation,
    'Does the state_enforced_creation reading represent a structurally distinct constraint from the theological created_reading, or are they the same constraint with added enforcement?',
    'Epsilon-invariance test: evaluate the theological created_reading without state enforcement for epsilon, suppression, and victim set. If epsilon changes substantially with the addition of state power, they are distinct constraints.',
    'If distinct, the kernel decomposition is validated; if the same, the extraction is inherent to the doctrine itself and the created_reading computes as more extractive than its theological proponents claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_separation, conceptual, 'Whether state enforcement creates a structurally distinct constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(qura_tr_t3, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(qura_tr_t6, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 6, 0.45).
narrative_ontology:measurement(qura_tr_t9, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 9, 0.55).
narrative_ontology:measurement(qura_tr_t12, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 12, 0.58).
narrative_ontology:measurement(qura_tr_t15, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(qura_be_t3, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(qura_be_t6, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(qura_be_t9, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 9, 0.88).
narrative_ontology:measurement(qura_be_t12, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 12, 0.85).
narrative_ontology:measurement(qura_be_t15, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 15, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(qura_su_t3, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 3, 0.72).
narrative_ontology:measurement(qura_su_t6, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 6, 0.85).
narrative_ontology:measurement(qura_su_t9, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 9, 0.92).
narrative_ontology:measurement(qura_su_t12, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 12, 0.9).
narrative_ontology:measurement(qura_su_t15, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 15, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, created_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the quran_ontological_status kernel family. It decomposes from the pure theological created_reading by the addition of state enforcement, which substantially changes epsilon (suppression rises, victim set appears, theater accumulates). The uncreated_reading is the theological antipode. These are structurally distinct constraints; do not merge them into a single story because epsilon varies by observable (theological dispute vs. state inquisition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
