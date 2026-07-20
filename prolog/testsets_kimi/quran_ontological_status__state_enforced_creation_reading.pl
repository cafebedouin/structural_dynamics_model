% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   This constraint instantiates the state_enforced_creation_reading of the
 *   quran_ontological_status kernel. During the Abbasid mihna (c. 833â851
 *   CE), the caliphate enforced the Mu'tazilite theological position that the
 *   Qur'an is created (makhlÅ«q) through inquisition tribunals, demanding
 *   public affirmation and punishing refusal. The constraint transforms a
 *   metaphysical dispute into an apparatus of political control. Its sibling
 *   readings are the uncreated_reading (later Sunni orthodoxy) and the
 *   created_reading (pure Mu'tazilite theology without state coercion).
 *
 * KEY AGENTS:
 *   - Caliphal authority (agenda_setter): Institutional power, arbitrage-grade exit; enforces doctrine via mihna tribunals.
 *   - Mu'tazilite rationalists (beneficiary): Organized scholars receiving state patronage; exit constrained by dependence on caliphal favor.
 *   - Traditionalist scholars (payer): Organized victims, identity_locked; bear imprisonment and torture for doctrinal refusal.
 *   - Literalist communities (payer): Moderate power, trapped; compelled to accept state-imposed theology.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.85).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.92).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Created Qur'an Doctrine (Mihna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "theological/political").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'b3e21517-70fb-4b35-b9a7-77689d4df237').
narrative_ontology:cs_kernel_codification('b3e21517-70fb-4b35-b9a7-77689d4df237', fixed_text).
narrative_ontology:cs_authority_grounding('b3e21517-70fb-4b35-b9a7-77689d4df237', extraction).
narrative_ontology:cs_interpretation_layer_present('b3e21517-70fb-4b35-b9a7-77689d4df237').
narrative_ontology:cs_reading_relation('b3e21517-70fb-4b35-b9a7-77689d4df237', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('b3e21517-70fb-4b35-b9a7-77689d4df237', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('b3e21517-70fb-4b35-b9a7-77689d4df237', foundational, created_quran_doctrine).
narrative_ontology:cs_axiom_status(created_quran_doctrine, overridden).
narrative_ontology:cs_axiom_grounding('b3e21517-70fb-4b35-b9a7-77689d4df237', created_quran_doctrine, theological).
narrative_ontology:cs_axiom('b3e21517-70fb-4b35-b9a7-77689d4df237', foundational, caliphal_doctrinal_authority).
narrative_ontology:cs_axiom_status(caliphal_doctrinal_authority, overridden).
narrative_ontology:cs_axiom_grounding('b3e21517-70fb-4b35-b9a7-77689d4df237', caliphal_doctrinal_authority, conventional).
narrative_ontology:cs_reference_frame('b3e21517-70fb-4b35-b9a7-77689d4df237', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('b3e21517-70fb-4b35-b9a7-77689d4df237', post_mihna_abolition, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('b3e21517-70fb-4b35-b9a7-77689d4df237', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_rationalists).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims the right to define and enforce correct theological belief regarding the Qur'an's ontological status. Operates the mihna tribunals, demands public recantation from scholars, and uses doctrinal conformity as a test of political loyalty. Derives legitimacy from the assertion that the caliph is the guardian of religious correctness.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, continental).

% Provide the theological argumentation that the Qur'an is created in order to safeguard divine transcendence. Enjoy state patronage, appointments to judiciary and teaching positions, and protection from theological rivals while the mihna lasts. Their institutional fortunes are tied to continued caliphal favor.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_rationalists, beneficiary,
    organized, biographical, constrained, continental).

% Uphold the doctrine that the Qur'an is the uncreated eternal speech of God. Refuse to affirm the createdness doctrine under interrogation by state tribunals. Subject to imprisonment, flogging, and banishment; Ahmad ibn Hanbal is imprisoned and tortured for persistent refusal.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    organized, biographical, identity_locked, continental).

% Lay believers and local communities adhering to literalist and traditionalist interpretations of scripture. Their scholars are persecuted, and they are required to accept state-imposed theological positions that contradict their established beliefs. They lack institutional channels to resist the inquisition.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination function at the political level. The doctrinal claim originally aimed to defend divine transcendence against anthropomorphism, but under state enforcement the arrangement coordinates only submission to caliphal doctrinal authority, not theological consensus.
% TRANSFER_FUNCTION: Moves legitimacy, public doctrinal assent, and political submission from traditionalist scholars and pluralist theological discourse to the caliph and the state-backed rationalist school. Extracts compliance via threat of imprisonment, flogging, or exclusion from public life.
% ABSENT_VOICES: Traditionalist scholars who refused recantation and were imprisoned or silenced; the broader populace whose theological commitments were overridden by state fiat; later Sunni orthodoxy which repudiated the mihna and memorialized its victims rather than its beneficiaries.
% DISAPPEARANCE_RATIONALE: If the state-enforced created doctrine and its tribunals vanished overnight, imprisoned scholars would be released, public theological profession would no longer be coerced, the caliphate would lose a direct instrument of ideological control, and scholarly pluralism would re-emerge in the public sphere.
% FOUNDING_PROBLEM: Theological controversy over the nature of divine speech and the ontological status of the Qur'an, fused with the Abbasid caliphate's desire to centralize religious authority and test political loyalty through doctrinal conformity.
% FOUNDING_PROBLEM_CORROBORATION: Later Abbasid chroniclers and Sunni historiographers writing outside the benefiting rationalist circle attest that the mihna was abandoned due to political failure and popular resistance; al-Mutawakkil's abolition edict and subsequent Sunni historiography corroborate that the inquisition outlived its function.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.85) is high because the constraint extracts compliance through judicial violence and imprisonment, decoupled from any genuine coordination benefit. Suppression (0.92) is higher still because the mechanism is an inquisition: alternatives are not merely disadvantaged but actively punished. Theater ratio (0.65) reflects that the theological debate serves as a performative cover for political loyalty testing; the measurement arc shows theater rising as genuine intellectual support erodes and the constraint persists by raw coercion. Accessibility collapse (0.80) is high because public profession of the uncreated doctrine became lethal to scholarly careers; resistance (0.78) is high due to Ahmad ibn Hanbal's sustained refusal and traditionalist martyrology. The cyclical arcârise under al-Ma'mun and al-Wathiq, abrupt collapse under al-Mutawakkilâis authored explicitly: the constraint was never self-sustaining and dissolved the moment state enforcement withdrew.
 *
 * PERSPECTIVAL GAP:
 *   The caliphal seat experiences the constraint as necessary doctrinal discipline that preserves theological correctness and political unity; the traditionalist seat experiences the identical structure as heretical tyranny that extracts assent through torture. The engine computes this divergence from beneficiary/victim declarations and exit options: the caliph has arbitrage mobility to switch doctrines, while traditionalists are identity_locked to the uncreated thesis.
 *
 * DIRECTIONALITY LOGIC:
 *   Caliphal authority is the concentrated beneficiary (d near 0.0), subsidized by the political submission of the scholarly class. Mu'tazilite rationalists are secondary beneficiaries (low d), though their gains are contingent. Traditionalist scholars are primary targets (d near 1.0) because the constraint is designed specifically to overcome their theological identity; their identity_locked exit amplifies effective extraction. Literalist communities are also high-d targets, trapped within an empire that criminalizes their literalist commitments.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problemâcentralizing religious authority and enforcing doctrinal unityâwas contested at inception and dead by the time of its abolition. The mihna persisted for nearly two decades not because it solved a live coordination problem, but because state prestige had been invested in it. This is a classic mandatrophy pattern: the arrangement outlived its function, yet the caliphal administration maintained it until the cost of resistance outweighed the benefit of doctrinal conformity. Classifying it as a snare prevents misreading the theological vocabulary as evidence of genuine coordination; the classification captures that the coordination story is cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_claim_vs_political_instrument,
    'Is the createdness doctrine intrinsically extractive, or does extraction emerge only from its fusion with state inquisition machinery?',
    'Comparative analysis with the non-state created_reading: if the purely theological thesis generates negligible coercion without state enforcement, the extraction is attributable to the state instrument rather than the doctrine.',
    'If extraction vanishes without state power, this reading is cleanly classified as a snare; if social coercion persists independently, the constraint family requires re-evaluation of the created_reading''s Îµ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_claim_vs_political_instrument, conceptual, 'Whether the extraction is in the doctrine or the enforcement.').

omega_variable(
    rationalist_benefit_or_captivity,
    'Did the Mu''tazilite school genuinely benefit from state backing, or were they captured and ultimately discredited by association with the mihna?',
    'Historical analysis of Mu''tazilite institutional fortunes before, during, and after the mihna; measure whether state patronage translated into lasting scholarly dominance or into post-abolition marginalization.',
    'If discredited and marginalized, their beneficiary status was temporary and illusory, sharpening the classification toward pure caliphal extraction; if they retained gains, the constraint carries a stronger tangled-rope signal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalist_benefit_or_captivity, empirical, 'Whether the rationalist beneficiary seat was substantive or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(qura_tr_t3, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 3, 0.45).
narrative_ontology:measurement(qura_tr_t6, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(qura_tr_t9, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 9, 0.65).
narrative_ontology:measurement(qura_tr_t12, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 12, 0.72).
narrative_ontology:measurement(qura_tr_t15, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 15, 0.78).
narrative_ontology:measurement(qura_tr_t18, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 18, 0.9).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(qura_be_t3, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement(qura_be_t6, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 6, 0.73).
narrative_ontology:measurement(qura_be_t9, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 9, 0.82).
narrative_ontology:measurement(qura_be_t12, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 12, 0.88).
narrative_ontology:measurement(qura_be_t15, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement(qura_be_t18, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 18, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(qura_su_t3, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 3, 0.75).
narrative_ontology:measurement(qura_su_t6, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 6, 0.85).
narrative_ontology:measurement(qura_su_t9, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 9, 0.92).
narrative_ontology:measurement(qura_su_t12, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 12, 0.95).
narrative_ontology:measurement(qura_su_t15, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 15, 0.9).
narrative_ontology:measurement(qura_su_t18, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 18, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).

% DUAL FORMULATION NOTE:
% This story is one reading of the quran_ontological_status kernel. The state-enforced creation reading is distinct from the pure theological created_reading and the uncreated_reading because its Îµ is driven by state coercion rather than metaphysical argument. Decomposition follows Îµ-invariance: the same doctrinal label ('created Qur'an') covers both a theological thesis and a political instrument, which have different extraction profiles and different stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
