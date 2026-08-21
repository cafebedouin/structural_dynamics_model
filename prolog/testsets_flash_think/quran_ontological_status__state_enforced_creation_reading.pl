% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__state_enforced_creation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   domain: islamic_theology/political_authority/philosophy_of_language
 *
 * SUMMARY:
 *   This constraint describes the historical period of the Mihna (833-848 CE)
 *   in the Abbasid Caliphate, where the Mu'tazilite doctrine of the created
 *   Qur'an was enforced by state power. This involved an inquisition
 *   demanding public affirmation of the doctrine, with severe consequences
 *   for dissenters. The constraint is a Snare because the coordination story
 *   (doctrinal unity, caliphal authority) served as a cover for the
 *   extraction of compliance and the suppression of theological and political
 *   opposition.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__state_enforced_creation_reading, 0.85).
domain_priors:suppression_score(quran_ontological_status__state_enforced_creation_reading, 0.9).
domain_priors:theater_ratio(quran_ontological_status__state_enforced_creation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__state_enforced_creation_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__state_enforced_creation_reading, snare).
narrative_ontology:human_readable(quran_ontological_status__state_enforced_creation_reading, "State-Enforced Created Qur'an Doctrine (Mihna)").
narrative_ontology:topic_domain(quran_ontological_status__state_enforced_creation_reading, "islamic_theology/political_authority/philosophy_of_language").

domain_priors:requires_active_enforcement(quran_ontological_status__state_enforced_creation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__state_enforced_creation_reading, 'e400b49a-8dee-41fa-8ef1-5e834212f38b').
narrative_ontology:cs_kernel_codification('e400b49a-8dee-41fa-8ef1-5e834212f38b', formalized).
narrative_ontology:cs_authority_grounding('e400b49a-8dee-41fa-8ef1-5e834212f38b', extraction).
narrative_ontology:cs_interpretation_layer_present('e400b49a-8dee-41fa-8ef1-5e834212f38b').
narrative_ontology:cs_reading_relation('e400b49a-8dee-41fa-8ef1-5e834212f38b', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('e400b49a-8dee-41fa-8ef1-5e834212f38b', quran_ontological_status__created_reading, influences).
narrative_ontology:cs_axiom('e400b49a-8dee-41fa-8ef1-5e834212f38b', foundational, quran_is_created_divine_speech).
narrative_ontology:cs_axiom_status(quran_is_created_divine_speech, holdable).
narrative_ontology:cs_axiom_grounding('e400b49a-8dee-41fa-8ef1-5e834212f38b', quran_is_created_divine_speech, theological).
narrative_ontology:cs_axiom('e400b49a-8dee-41fa-8ef1-5e834212f38b', foundational, caliph_as_doctrinal_arbiter).
narrative_ontology:cs_axiom_status(caliph_as_doctrinal_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('e400b49a-8dee-41fa-8ef1-5e834212f38b', caliph_as_doctrinal_arbiter, conventional).
narrative_ontology:cs_reference_frame('e400b49a-8dee-41fa-8ef1-5e834212f38b', caliphal_doctrinal_supremacy).
narrative_ontology:cs_drift_state('e400b49a-8dee-41fa-8ef1-5e834212f38b', post_mihna_abandonment, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e400b49a-8dee-41fa-8ef1-5e834212f38b', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:constraint_beneficiary(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, literalist_communities).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).
narrative_ontology:constraint_victim(quran_ontological_status__state_enforced_creation_reading, general_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Abbasid Caliphate, which initiated and enforced the Mihna. It sought to consolidate its religious and political authority by imposing a unified theological doctrine, benefiting from increased control over scholarly discourse and state legitimacy.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, caliphal_authority, agenda_setter,
    institutional, generational, arbitrage, regional).

% The rationalist theological school whose doctrine of the created Qur'an was adopted and enforced by the state. They temporarily gained significant influence, state patronage, and saw their theological rivals suppressed, though their position was precarious.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, mu_tazilite_scholars, beneficiary,
    organized, biographical, mobile, regional).

% Scholars who adhered to the doctrine of the uncreated Qur'an, such as Ahmad ibn Hanbal. They were subjected to interrogation, imprisonment, torture, and public humiliation for refusing to affirm the state-sanctioned doctrine. Their careers and lives were at risk.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, traditionalist_scholars, payer,
    powerless, biographical, trapped, regional).

% Communities and segments of the populace who held traditionalist views regarding the Qur'an's uncreated nature. They faced pressure to conform, saw their religious leaders persecuted, and experienced a suppression of their theological expression.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, literalist_communities, payer,
    moderate, generational, constrained, regional).

% The broader intellectual environment that allowed for diverse theological interpretations and open debate. This was actively suppressed by the state's imposition of a single, enforced doctrine, leading to a chilling effect on independent thought.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(quran_ontological_status__state_enforced_creation_reading, scholarly_pluralism).

% The ordinary people who were subject to the doctrinal shifts and the fear generated by the inquisition. While not directly targeted like scholars, they lived under a regime that enforced theological conformity, impacting their religious practice and social cohesion.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, general_populace, payer,
    moderate, immediate, constrained, local).

% The collective body of later Islamic scholars and historians who would analyze the Mihna's impact on theological development, political legitimacy, and the trajectory of Islamic thought. They observe the long-term consequences of this period of enforced conformity.
narrative_ontology:constraint_stakeholder(quran_ontological_status__state_enforced_creation_reading, future_islamic_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__state_enforced_creation_reading, caliphal_authority).
narrative_ontology:fixing_cost_class(quran_ontological_status__state_enforced_creation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified theological doctrine regarding the Qur'an's nature, thereby consolidating caliphal religious and political authority and presenting a cohesive intellectual front.
% TRANSFER_FUNCTION: Transfers doctrinal control and legitimacy from independent scholarly consensus to caliphal decree, and transfers intellectual freedom and safety from traditionalist scholars to the state, which then grants it to favored rationalist schools.
% ABSENT_VOICES: Scholars advocating for the uncreated Qur'an doctrine, those seeking theological pluralism, and communities who held traditionalist views without state interference. They were actively silenced, imprisoned, or purged, preventing any counter-narrative or alternative theological development from gaining public traction.
% DISAPPEARANCE_RATIONALE: If the Mihna and its underlying doctrine of state-enforced theological conformity vanished overnight, the immediate effect would be a resurgence of traditionalist views, a re-evaluation of caliphal authority's role in religious matters, and a significant shift in the balance of power within Islamic scholarship, leading to a more pluralistic theological landscape.
% FOUNDING_PROBLEM: The Abbasid Caliphate faced challenges to its religious authority and sought to unify theological discourse under a state-sanctioned doctrine to bolster its legitimacy and control over a diverse and sometimes fractious scholarly class.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts from both pro- and anti-Mihna sources, including Abbasid chronicles and biographies of scholars like Ahmad ibn Hanbal, corroborate the caliphate's political motivations and the widespread resistance. Later caliphs abandoned the Mihna, implicitly acknowledging its failure to achieve lasting doctrinal unity or enhance caliphal religious authority, and historians widely view it as a political rather than purely theological endeavor.
narrative_ontology:disappearance_verdict(quran_ontological_status__state_enforced_creation_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__state_enforced_creation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__state_enforced_creation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_ontological_status__state_enforced_creation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__state_enforced_creation_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high due to the forced ideological conformity and the severe penalties for non-compliance, which extracted intellectual freedom and personal safety from traditionalist scholars. Suppression is very high, reflecting the direct state coercion, imprisonment, and torture used to enforce the doctrine. The theater ratio is low to moderate; while public affirmations had a performative aspect, the underlying threat and actual violence were very real, making it far from a mere theatrical display. Resistance was significant, notably from figures like Ahmad ibn Hanbal, indicating that the constraint was not passively accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Caliphal Authority and Mu'tazilite scholars, the Mihna was a necessary measure to establish theological truth and consolidate legitimate rule. From the perspective of traditionalist scholars and affected communities, it was a brutal imposition of power, suppressing genuine religious belief and intellectual freedom. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Caliphal Authority is the primary beneficiary, gaining enhanced religious and political control. Mu'tazilite scholars are also beneficiaries, as their doctrine received state backing and their rivals were suppressed. Traditionalist scholars, literalist communities, and the general populace are victims, bearing the costs of persecution, forced conformity, and fear. Scholarly pluralism is an excluded non-agent, its very existence undermined by the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mihna's mandate was to unify the Umma under a single theological doctrine and strengthen caliphal authority. While it temporarily achieved some doctrinal conformity, it ultimately failed to secure lasting legitimacy for the caliphate's theological role and was abandoned. The constraint persisted through coercion, not through its ability to solve a genuine, enduring coordination problem. Its persistence beyond its initial political utility, despite widespread resistance, indicates a snare-like function where the coordination story became a cover for power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_motivation,
    'Was the Mihna primarily driven by a genuine theological conviction on the part of the caliphs, or was it predominantly a political maneuver to consolidate authority?',
    'Detailed historical analysis of caliphal correspondence, court records, and the broader political context of the Abbasid era, weighing theological arguments against power struggles and succession crises.',
    'If primarily theological, the constraint might lean more towards a misguided Rope or Scaffold; if primarily political, it strongly reinforces the Snare classification by highlighting the extractive nature of the enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_motivation, conceptual, 'Ambiguity regarding the primary motivation behind the state''s enforcement of the created Qur''an doctrine.').

omega_variable(
    long_term_impact_on_pluralism,
    'What was the long-term impact of the Mihna on the diversity and freedom of theological inquiry within Islamic scholarship?',
    'Comparative study of theological output and institutional structures before, during, and after the Mihna, assessing the range of accepted doctrines and the prevalence of independent scholarly thought.',
    'If scholarly pluralism was severely and permanently curtailed, it underscores the high suppression and extractive nature of the constraint. If it eventually rebounded or found new forms, it suggests the constraint''s long-term suppressive power was limited.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_impact_on_pluralism, empirical, 'The lasting effect of state-enforced doctrine on intellectual freedom and theological diversity.').

omega_variable(
    suppression_mechanism_nature,
    'Was the suppression primarily structural (state violence, legal decree) or did it also induce significant internalized conformity (fear, ideological self-censorship)?',
    'Analysis of personal accounts, fatwas, and scholarly writings from the period for evidence of self-censorship or shifts in public discourse that outlasted direct state enforcement.',
    'If internalized conformity was a significant factor, the effective suppression of the constraint was even higher than the structural measures suggest, as it shaped thought beyond direct coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_nature, empirical, 'The balance between overt state coercion and internalized ideological conformity as mechanisms of suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__state_enforced_creation_reading, 833, 848).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t833, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 833, 0.15).
narrative_ontology:measurement(qura_tr_t836, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 836, 0.17).
narrative_ontology:measurement(qura_tr_t839, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 839, 0.19).
narrative_ontology:measurement(qura_tr_t842, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 842, 0.21).
narrative_ontology:measurement(qura_tr_t845, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 845, 0.23).
narrative_ontology:measurement(qura_tr_t848, quran_ontological_status__state_enforced_creation_reading, theater_ratio, 848, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t833, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 833, 0.8).
narrative_ontology:measurement(qura_be_t836, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 836, 0.82).
narrative_ontology:measurement(qura_be_t839, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 839, 0.84).
narrative_ontology:measurement(qura_be_t842, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 842, 0.86).
narrative_ontology:measurement(qura_be_t845, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 845, 0.87).
narrative_ontology:measurement(qura_be_t848, quran_ontological_status__state_enforced_creation_reading, base_extractiveness, 848, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t833, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 833, 0.88).
narrative_ontology:measurement(qura_su_t836, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 836, 0.89).
narrative_ontology:measurement(qura_su_t839, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 839, 0.9).
narrative_ontology:measurement(qura_su_t842, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 842, 0.91).
narrative_ontology:measurement(qura_su_t845, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 845, 0.92).
narrative_ontology:measurement(qura_su_t848, quran_ontological_status__state_enforced_creation_reading, suppression_requirement, 848, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__state_enforced_creation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__state_enforced_creation_reading, quran_ontological_status__created_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
