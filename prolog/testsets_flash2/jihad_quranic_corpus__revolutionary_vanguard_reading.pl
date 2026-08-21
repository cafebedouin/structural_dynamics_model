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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'revolutionary vanguard' reading of Jihad,
 *   which interprets it as an immediate individual obligation (fard 'ayn)
 *   against rulers deemed apostate and foreign occupiers. This reading
 *   bypasses traditional state authority through the declaration of takfir
 *   (excommunication) and emergency jurisprudence, leading to decentralized,
 *   often indiscriminate, violence. It is a highly extractive and suppressive
 *   constraint, creating identifiable victims among target populations and
 *   mainstream Muslim communities. This story is one reading of the
 *   'jihad_quranic_corpus' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.92).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, 'ae7d4e3d-06fe-4035-ac34-f84368bb834b').
narrative_ontology:cs_kernel_codification('ae7d4e3d-06fe-4035-ac34-f84368bb834b', fixed_text).
narrative_ontology:cs_authority_grounding('ae7d4e3d-06fe-4035-ac34-f84368bb834b', extraction).
narrative_ontology:cs_interpretation_layer_present('ae7d4e3d-06fe-4035-ac34-f84368bb834b').
narrative_ontology:cs_reading_relation('ae7d4e3d-06fe-4035-ac34-f84368bb834b', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('ae7d4e3d-06fe-4035-ac34-f84368bb834b', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('ae7d4e3d-06fe-4035-ac34-f84368bb834b', foundational, takfir_justifies_rebellion).
narrative_ontology:cs_axiom_status(takfir_justifies_rebellion, holdable).
narrative_ontology:cs_axiom_grounding('ae7d4e3d-06fe-4035-ac34-f84368bb834b', takfir_justifies_rebellion, theological).
narrative_ontology:cs_axiom('ae7d4e3d-06fe-4035-ac34-f84368bb834b', foundational, emergency_overrides_state_monopoly).
narrative_ontology:cs_axiom_status(emergency_overrides_state_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('ae7d4e3d-06fe-4035-ac34-f84368bb834b', emergency_overrides_state_monopoly, theological).
narrative_ontology:cs_reference_frame('ae7d4e3d-06fe-4035-ac34-f84368bb834b', early_islamic_revolutionary_precedent).
narrative_ontology:cs_drift_state('ae7d4e3d-06fe-4035-ac34-f84368bb834b', contemporary_global_jihad_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ae7d4e3d-06fe-4035-ac34-f84368bb834b', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_muslim_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret religious texts to declare rulers apostate and call for immediate, decentralized armed struggle. They gain authority and followers by presenting themselves as the true defenders of Islam against corrupt regimes and foreign influence. Their identity is fused with this revolutionary interpretation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders, agenda_setter,
    organized, generational, identity_locked, global).

% Are motivated by a sense of religious duty and perceived injustice to participate in violent actions. They gain a sense of purpose, belonging, and divine reward, but often pay with their lives or freedom. Their identity is deeply intertwined with the cause.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals, beneficiary,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals, payer).

% Are declared illegitimate and targeted for overthrow by the revolutionary vanguard. They bear the direct costs of insurgency, political instability, and loss of legitimacy. Their only 'exit' is to conform to the vanguard's interpretation or be violently removed.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    institutional, biographical, trapped, national).

% Are targeted as invaders of Muslim lands, facing constant attacks and resistance. They bear military and political costs, and their presence is used to justify the fard 'ayn declaration. Their exit is withdrawal, which is politically costly.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, biographical, constrained, regional).

% Are caught in the conflict, suffering violence, displacement, and disruption of daily life. They are often deemed legitimate targets due to collective guilt or proximity to 'apostate' or 'occupying' entities. Their options are flight or enduring the conflict.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas, payer,
    powerless, immediate, trapped, local).

% Are pressured to support the revolutionary vanguard's interpretation, facing social ostracism or violence if they resist. They bear the reputational cost of association with extremism and the internal strife caused by the division. Their exit is to publicly denounce the interpretation, risking retaliation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_muslim_communities, payer,
    organized, generational, constrained, global).

% Are bypassed and often condemned by the revolutionary vanguard for their adherence to traditional jurisprudence, which typically requires state authority for jihad and protects non-combatants. They would argue against the legitimacy of the vanguard's interpretation but are excluded from its decision-making.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_islamic_scholars, excluded,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates decentralized, immediate armed action against perceived enemies of Islam, bypassing traditional state and scholarly authority. It mobilizes individuals and small groups for direct action.
% TRANSFER_FUNCTION: Transfers legitimacy for violence from established state or scholarly institutions to self-appointed vanguard leaders and individuals. It transfers resources (lives, property, social capital) from target populations to the revolutionary cause.
% ABSENT_VOICES: Classical Islamic scholars and mainstream religious institutions are systematically excluded; they would argue for the necessity of state authority for jihad, the protection of non-combatants, and the dangers of takfir (excommunication). Their voices are suppressed by the vanguard's narrative of apostasy and emergency.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the justification for decentralized violence against 'apostate' rulers and 'occupiers' would collapse. Insurgencies would lose their ideological basis, radicalized individuals would lose their immediate religious imperative, and the political landscape in affected regions would shift dramatically, likely reducing conflict and allowing traditional authorities to reassert control over religious matters.
% FOUNDING_PROBLEM: The perceived corruption and apostasy of Muslim rulers, coupled with foreign occupation of Muslim lands, leading to a state of emergency where traditional jurisprudential safeguards are deemed insufficient.
% FOUNDING_PROBLEM_CORROBORATION: The revolutionary vanguard and its followers attest that the problem is acutely live, citing ongoing political oppression and foreign military presence. Mainstream Muslim communities and classical scholars acknowledge the existence of political grievances and foreign intervention but dispute the 'apostasy' and 'emergency' declarations, arguing that the proposed solution exacerbates the problem. No corroboration from outside the benefiting parties for the 'apostasy' and 'emergency' claims as interpreted by the vanguard.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.92, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.92) because this reading justifies the seizure of lives, property, and political authority from its targets, often without traditional legal or ethical safeguards. Suppression (0.88) is also very high, as it actively delegitimizes and silences dissenting religious authorities and coerces participation or acquiescence from affected populations. Resistance is high (0.95) due to the violent opposition it generates from targeted states, occupiers, and mainstream religious bodies. Accessibility collapse (0.75) is substantial because the declaration of fard 'ayn and takfir severely limits alternatives for individuals and communities, making non-participation a religious transgression or a physical danger. Theater ratio is low (0.15) as the constraint is primarily functional in mobilizing violence, with little performative overhead.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the revolutionary vanguard, this is a necessary, divinely mandated response to existential threats, a 'rope' for collective salvation. From the perspective of its victims and mainstream scholars, it is a 'snare' that perverts religious texts to justify extreme violence and consolidate power. The engine's classification will reflect the latter, given the high extractiveness and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Revolutionary vanguard leaders and radicalized individuals are beneficiaries (d near 0.0-0.2) as they gain authority, purpose, and perceived divine reward, despite personal risks. Apostate rulers, occupying forces, and civilians in target areas are clear victims (d near 0.9-1.0), bearing the direct costs of violence and instability. Mainstream Muslim communities are also victims (d near 0.8) as they face pressure to conform and suffer the reputational damage and internal strife caused by this interpretation. Classical Islamic scholars are excluded, their authority bypassed and their traditional interpretations suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the classical sense, as its mandate is continuously re-declared by its proponents based on evolving political conditions. The 'founding problem' (apostate rulers, occupation) is kept 'live' through ongoing interpretation, preventing the mandate from atrophying. The classification as a snare prevents mislabeling it as a legitimate coordination mechanism, highlighting its coercive and extractive nature despite its proponents' claims of religious necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takfir_legitimacy,
    'Is the declaration of takfir (excommunication) against Muslim rulers and populations legitimate under classical Islamic jurisprudence, or is it an innovation used to justify violence?',
    'Consensus of independent, globally recognized Islamic jurisprudential bodies and historical analysis of fatwas (religious edicts) on takfir.',
    'If illegitimate, the entire basis for the ''fard ''ayn'' declaration against Muslim rulers collapses, reclassifying the constraint as a pure snare with no religious coordination function. If deemed legitimate by a significant body, it would retain a contested ''tangled rope'' aspect for its adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(takfir_legitimacy, conceptual, 'Legitimacy of takfir as a basis for revolutionary jihad.').

omega_variable(
    emergency_jurisprudence_scope,
    'Is the current political and military situation a genuine ''state of emergency'' (darura) that overrides classical jurisprudential safeguards for jihad, or is it a manufactured justification for violence?',
    'Independent assessment by international legal bodies and non-partisan conflict analysts, comparing the claimed ''emergency'' to historical precedents and established criteria for darura.',
    'If the emergency is not genuine, the justification for bypassing state authority and non-combatant immunity collapses, further solidifying the ''snare'' classification. If a genuine emergency is widely acknowledged, it might lend a thin ''scaffold'' aspect to the constraint, though its extractive nature would remain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_jurisprudence_scope, empirical, 'Validity of emergency jurisprudence claims.').

omega_variable(
    identity_fusion_mechanism,
    'To what extent is individual participation driven by genuine religious conviction versus social pressure, charismatic leadership, or a desire for belonging and purpose in marginalized communities?',
    'Sociological studies, ethnographic research, and psychological profiles of individuals joining such movements, focusing on pre-radicalization factors and post-disengagement narratives.',
    'If identity fusion is primarily driven by social/psychological factors rather than pure religious conviction, the ''identity_locked'' exit option for radicalized individuals becomes more a function of social engineering than theological commitment, potentially increasing the effective suppression metric for those individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, empirical, 'Mechanisms of identity fusion in radicalized individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 1979, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1979, 0.1).
narrative_ontology:measurement(jiha_tr_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(jiha_tr_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(jiha_tr_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(jiha_be_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1979, 0.75).
narrative_ontology:measurement(jiha_be_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1990, 0.82).
narrative_ontology:measurement(jiha_be_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2001, 0.9).
narrative_ontology:measurement(jiha_be_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2010, 0.93).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2024, 0.92).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1979, 0.7).
narrative_ontology:measurement(jiha_su_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1990, 0.78).
narrative_ontology:measurement(jiha_su_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2001, 0.85).
narrative_ontology:measurement(jiha_su_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2010, 0.9).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2024, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jihad_quranic_corpus' kernel. This 'revolutionary vanguard' reading directly challenges and seeks to displace the 'defensive spiritual' and 'expansionist legalist' readings by reinterpreting core concepts like fard 'ayn, takfir, and emergency jurisprudence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
