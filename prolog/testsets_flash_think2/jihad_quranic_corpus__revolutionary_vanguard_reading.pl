% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'revolutionary vanguard' reading of Jihad,
 *   which interprets it as an immediate individual obligation (fard 'ayn)
 *   against apostate rulers and foreign occupiers. This reading bypasses
 *   traditional state authority through the declaration of 'takfir'
 *   (excommunication) and the invocation of emergency jurisprudence. It is
 *   one of several contested readings of the Quranic corpus on Jihad. The
 *   high extractiveness reflects the demand for individual sacrifice and the
 *   severe costs imposed on targeted populations and existing authorities.
 *   The high suppression reflects the active delegitimization of alternative
 *   interpretations and the coercion required to maintain this ideology
 *   against state and mainstream religious opposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'a41531f3-ade2-4b99-b76b-fd737b67dafb').
narrative_ontology:cs_kernel_codification('a41531f3-ade2-4b99-b76b-fd737b67dafb', fixed_text).
narrative_ontology:cs_authority_grounding('a41531f3-ade2-4b99-b76b-fd737b67dafb', extraction).
narrative_ontology:cs_interpretation_layer_present('a41531f3-ade2-4b99-b76b-fd737b67dafb').
narrative_ontology:cs_reading_relation('a41531f3-ade2-4b99-b76b-fd737b67dafb', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('a41531f3-ade2-4b99-b76b-fd737b67dafb', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('a41531f3-ade2-4b99-b76b-fd737b67dafb', foundational, takfir_of_apostate_rulers_justifies_rebellion).
narrative_ontology:cs_axiom_status(takfir_of_apostate_rulers_justifies_rebellion, holdable).
narrative_ontology:cs_axiom_grounding('a41531f3-ade2-4b99-b76b-fd737b67dafb', takfir_of_apostate_rulers_justifies_rebellion, theological).
narrative_ontology:cs_axiom('a41531f3-ade2-4b99-b76b-fd737b67dafb', foundational, jihad_fard_ayn_bypasses_state_authority).
narrative_ontology:cs_axiom_status(jihad_fard_ayn_bypasses_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('a41531f3-ade2-4b99-b76b-fd737b67dafb', jihad_fard_ayn_bypasses_state_authority, conventional).
narrative_ontology:cs_reference_frame('a41531f3-ade2-4b99-b76b-fd737b67dafb', early_islamic_community_ideal).
narrative_ontology:cs_drift_state('a41531f3-ade2-4b99-b76b-fd737b67dafb', contemporary_global_jihad_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a41531f3-ade2-4b99-b76b-fd737b67dafb', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, adherents_seeking_eschatological_reward).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_targeted_areas).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_ulama).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulate the ideology of immediate individual jihad, issue fatwas of takfir against rulers, and direct adherents. They gain authority and influence by presenting themselves as the true interpreters of divine will and the only path to liberation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders, agenda_setter,
    institutional, generational, identity_locked, global).

% Are mobilized to participate in armed struggle, often sacrificing their lives and resources. They perceive themselves as fulfilling a divine obligation and expect spiritual rewards, including martyrdom, which is a powerful identity-locking mechanism.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, adherents_seeking_eschatological_reward, beneficiary,
    moderate, biographical, identity_locked, global).

% Are declared illegitimate and apostate by the vanguard, making them primary targets for overthrow. They face direct violence and delegitimization campaigns, with no easy exit from the ideological condemnation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, immediate, trapped, national).

% Are designated as legitimate targets due to their presence in Muslim lands. They face armed resistance and are subject to the vanguard's reinterpretation of warfare, which often disregards traditional rules of engagement.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, immediate, constrained, regional).

% Are often caught in the crossfire, displaced, or reclassified as legitimate targets due to collective guilt or association with apostate regimes/occupiers. They bear the direct human cost of the conflict.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_targeted_areas, payer,
    powerless, immediate, trapped, local).

% Are bypassed and often condemned by the revolutionary vanguard for their adherence to classical jurisprudence and state authority. Their traditional role in interpreting Islamic law is actively undermined, and their voices are excluded from the vanguard's discourse.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_ulama, excluded,
    institutional, generational, constrained, global).

% Observe and document violations of international law, including the targeting of civilians and non-state actors' disregard for established rules of armed conflict. Their analysis is external to the constraint's internal logic.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, international_humanitarian_law_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes individuals and small groups for immediate armed struggle against perceived enemies of Islam, bypassing traditional state and scholarly authority to achieve a divinely mandated revolutionary goal.
% TRANSFER_FUNCTION: Transfers individual lives, wealth, and agency to the revolutionary cause; transfers political and religious authority from existing state structures and mainstream scholars to the vanguard; transfers immense suffering and instability to targeted populations.
% ABSENT_VOICES: Mainstream Islamic scholars (ulama) who uphold classical jurisprudence on jihad and non-combatant immunity are actively excluded and delegitimized. Civilians in targeted areas, whose lives are directly impacted, have no voice in the vanguard's decision-making.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, a significant ideological justification for decentralized, non-state armed groups would be removed. This would lead to a re-evaluation of legitimate targets and authority in armed conflict within the Islamic world, likely reducing violence and re-empowering traditional religious and state institutions.
% FOUNDING_PROBLEM: The perceived corruption and apostasy of existing Muslim rulers, coupled with the occupation of Muslim lands by foreign powers, leading to a belief that traditional Islamic governance has failed and immediate, individual action is required to restore true Islam.
% FOUNDING_PROBLEM_CORROBORATION: Adherents and sympathetic ideologues within the revolutionary vanguard attest to the problem's live status, citing ongoing political and military grievances. Mainstream scholars and state authorities dispute the 'takfir' and 'apostasy' claims, arguing the problem is manufactured or misdiagnosed, and that the vanguard's methods are illegitimate. Independent observers note the persistence of grievances but question the vanguard's proposed solutions.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.88) due to the demand for adherents' lives and resources, and the severe disruption and violence inflicted upon targeted societies. Suppression is also very high (0.92) because this reading actively delegitimizes and seeks to overthrow existing authorities (both political and religious), requiring constant ideological enforcement and suppression of dissent. The theater ratio is low (0.10) as the constraint is primarily about direct, often violent, action rather than performative maintenance. Accessibility collapse is high (0.85) for adherents, as the ideology presents immediate armed struggle as the only legitimate path. Resistance is high (0.75) from targeted states, mainstream religious authorities, and affected populations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the revolutionary vanguard, this reading is a necessary and righteous path to restore true Islam, offering liberation and divine reward. From the perspective of apostate rulers, occupying forces, and mainstream ulama, it is a destructive, illegitimate, and highly extractive interpretation that destabilizes societies and violates established Islamic law. The engine's classification as a Snare captures this divergence, highlighting the coercive and extractive nature despite the internal justification.
 *
 * DIRECTIONALITY LOGIC:
 *   Revolutionary vanguard leaders are clear beneficiaries, gaining authority and influence. Adherents are also beneficiaries in their own frame, expecting spiritual rewards. Apostate rulers, occupying forces, and civilians in targeted areas are direct victims, bearing the brunt of violence and delegitimization. Mainstream ulama are excluded, their authority undermined. The identity-locked exit option for adherents reflects the profound ideological commitment and the promise of eschatological reward.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a valid reading of the ''jihad_quranic_corpus'' kernel, or a radical departure?',
    'Comparative textual analysis by independent Islamic scholars and historical contextualization of the Quranic verses and early Islamic practice.',
    'If deemed a radical departure, its legitimacy within Islamic discourse would be severely undermined, potentially reducing its ability to mobilize adherents. If deemed a valid, albeit minority, reading, its persistence would be understood as an internal contestation of Islamic law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Identifies this constraint as one reading of the ''jihad_quranic_corpus'' kernel.').

omega_variable(
    legitimacy_of_takfir_against_rulers,
    'Is the declaration of ''takfir'' (excommunication) against Muslim rulers legitimate under classical Islamic jurisprudence, or an innovation used to justify rebellion?',
    'Consensus (ijma) of mainstream Islamic scholars across different schools of thought, and historical precedent for such declarations.',
    'If illegitimate, the foundational premise for bypassing state authority collapses, reclassifying the constraint as a pure Snare with no legitimate coordination function. If legitimate, it retains a contested, but jurisprudentially grounded, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_takfir_against_rulers, conceptual, 'Ambiguity regarding the jurisprudential legitimacy of takfir against rulers.').

omega_variable(
    scope_of_emergency_jurisprudence,
    'Is the application of ''darura'' (emergency jurisprudence) to bypass state authority and classical rules of warfare legitimate in the contexts invoked by the vanguard, or an overreach?',
    'Review by independent Islamic legal experts on the conditions for invoking darura and its historical application in Islamic law.',
    'If an overreach, the constraint''s claims to religious legitimacy are weakened, further exposing its extractive nature. If legitimate, it implies a genuine, albeit extreme, coordination function under specific conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_emergency_jurisprudence, empirical, 'Ambiguity regarding the legitimate scope of emergency jurisprudence.').

omega_variable(
    civilian_combatant_reclassification,
    'Is the reclassification of civilians as combatants via collective guilt or association with targeted regimes/occupiers legitimate under Islamic ethical warfare, or a violation?',
    'Analysis of classical Islamic texts on non-combatant immunity and contemporary fatwas from widely recognized Islamic legal bodies.',
    'If a violation, the constraint''s ethical claims are severely undermined, increasing its perceived extractiveness and moral illegitimacy. If legitimate (even under extreme conditions), it implies a different, albeit controversial, ethical framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civilian_combatant_reclassification, conceptual, 'Ambiguity regarding the reclassification of civilians as combatants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 1979, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1979, 0.15).
narrative_ontology:measurement(jiha_tr_t1989, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1989, 0.12).
narrative_ontology:measurement(jiha_tr_t1999, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1999, 0.1).
narrative_ontology:measurement(jiha_tr_t2009, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(jiha_tr_t2019, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1979, 0.75).
narrative_ontology:measurement(jiha_be_t1989, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1989, 0.8).
narrative_ontology:measurement(jiha_be_t1999, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1999, 0.85).
narrative_ontology:measurement(jiha_be_t2009, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2009, 0.87).
narrative_ontology:measurement(jiha_be_t2019, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2019, 0.88).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2024, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1979, 0.7).
narrative_ontology:measurement(jiha_su_t1989, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1989, 0.8).
narrative_ontology:measurement(jiha_su_t1999, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1999, 0.88).
narrative_ontology:measurement(jiha_su_t2009, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2009, 0.9).
narrative_ontology:measurement(jiha_su_t2019, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2019, 0.92).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, state_monopoly_on_violence).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jihad_jurisprudence).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'jihad_quranic_corpus' kernel. Its structural properties and metrics differ significantly from the 'defensive_spiritual_reading' and 'expansionist_legalist_reading', necessitating separate constraint stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
