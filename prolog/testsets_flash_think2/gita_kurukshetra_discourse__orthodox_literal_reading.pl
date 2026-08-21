% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__orthodox_literal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__orthodox_literal_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__orthodox_literal_reading
 *   human_readable: Bhagavad Gita: Orthodox Literal Reading of Caste Duty and Righteous War
 *   domain: religious/ethical/social
 *
 * SUMMARY:
 *   This constraint represents the orthodox literal reading of the Bhagavad
 *   Gita's Kurukshetra discourse, which mandates caste-based duty
 *   (varnashrama dharma) and legitimates righteous violence (dharmayuddha).
 *   It is one specific interpretation of a foundational text, distinct from
 *   allegorical or universalist readings. The interpretation serves to uphold
 *   a rigid social hierarchy and justify actions, including warfare, deemed
 *   necessary for maintaining cosmic and social order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85).
domain_priors:suppression_score(gita_kurukshetra_discourse__orthodox_literal_reading, 0.9).
domain_priors:theater_ratio(gita_kurukshetra_discourse__orthodox_literal_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__orthodox_literal_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__orthodox_literal_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__orthodox_literal_reading, "Bhagavad Gita: Orthodox Literal Reading of Caste Duty and Righteous War").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__orthodox_literal_reading, "religious/ethical/social").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__orthodox_literal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__orthodox_literal_reading, '7da53766-0e05-44b4-ad96-ceb835b76395').
narrative_ontology:cs_kernel_codification('7da53766-0e05-44b4-ad96-ceb835b76395', fixed_text).
narrative_ontology:cs_authority_grounding('7da53766-0e05-44b4-ad96-ceb835b76395', lineage).
narrative_ontology:cs_interpretation_layer_present('7da53766-0e05-44b4-ad96-ceb835b76395').
narrative_ontology:cs_reading_relation('7da53766-0e05-44b4-ad96-ceb835b76395', gita_kurukshetra_discourse__gandhian_allegorical_reading, forecloses).
narrative_ontology:cs_reading_relation('7da53766-0e05-44b4-ad96-ceb835b76395', gita_kurukshetra_discourse__universalist_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('7da53766-0e05-44b4-ad96-ceb835b76395', foundational, varnashrama_dharma_divine_mandate).
narrative_ontology:cs_axiom_status(varnashrama_dharma_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('7da53766-0e05-44b4-ad96-ceb835b76395', varnashrama_dharma_divine_mandate, theological).
narrative_ontology:cs_axiom('7da53766-0e05-44b4-ad96-ceb835b76395', foundational, kshatriya_dharma_righteous_violence).
narrative_ontology:cs_axiom_status(kshatriya_dharma_righteous_violence, holdable).
narrative_ontology:cs_axiom_grounding('7da53766-0e05-44b4-ad96-ceb835b76395', kshatriya_dharma_righteous_violence, deontological).
narrative_ontology:cs_reference_frame('7da53766-0e05-44b4-ad96-ceb835b76395', classical_dharmic_order).
narrative_ontology:cs_drift_state('7da53766-0e05-44b4-ad96-ceb835b76395', contemporary_global_ethics_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7da53766-0e05-44b4-ad96-ceb835b76395', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__orthodox_literal_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, brahmins_interpretive_authority).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, upper_castes_social_order).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__orthodox_literal_reading, enemies_in_dharmic_war).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__orthodox_literal_reading, devotees_seeking_moksha).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, varnashrama_dharma_doctrine).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__orthodox_literal_reading, karma_yoga_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the exclusive right to interpret sacred texts, including the Gita, and define 'dharma'. Benefits from the perpetuation of the caste system and the justification of actions that maintain social order, as their authority is derived from this framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, brahmins_interpretive_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefits from the legitimation of their social role, including the use of violence in 'righteous' war, as a divinely ordained duty leading to spiritual merit. Their power and status are reinforced by this interpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, kshatriya_warrior_class, beneficiary,
    powerful, generational, constrained, national).

% Benefits from the stability and privileges afforded by the caste hierarchy, which is presented as a natural and divinely sanctioned order. Their social position is secured by adherence to this interpretation.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, upper_castes_social_order, beneficiary,
    organized, generational, constrained, national).

% Bears the costs of a rigid social hierarchy, including limited social mobility, prescribed duties, and potential discrimination, all justified as their 'dharma' or karmic destiny. Exit is structurally and ideologically suppressed.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, lower_castes, payer,
    powerless, generational, trapped, national).

% Are the direct targets of violence justified by the concept of 'dharmic war'. Their lives and well-being are sacrificed for the maintenance of a divinely ordained order, with no recourse or legitimate exit from their position as 'enemies'.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, enemies_in_dharmic_war, payer,
    powerless, immediate, trapped, local).

% Challenges the literal interpretation of caste and violence, advocating for social equality and non-violence. They are often marginalized or suppressed by orthodox institutions, their voices deemed illegitimate or heretical within the dominant framework.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, reform_movements, excluded,
    organized, biographical, constrained, national).

% Find spiritual guidance and a path to liberation (moksha) within the framework of prescribed duties and devotion. Their spiritual identity is often fused with adherence to the orthodox interpretation, making alternative paths difficult to conceive or pursue.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, devotees_seeking_moksha, beneficiary,
    moderate, biographical, identity_locked, global).

% Analyzes the text and its interpretations from an academic, external perspective, comparing it with other religious and ethical systems. They are not bound by the internal logic of the orthodox reading but can identify its structural implications.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__orthodox_literal_reading, scholars_of_comparative_religion, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a divinely sanctioned social order (varnashrama dharma) and provides ethical justification for action (karma yoga), including violence, to maintain cosmic balance and individual spiritual progress within one's prescribed role.
% TRANSFER_FUNCTION: Transfers social status, interpretive authority, and material benefits to upper castes, and legitimizes violence against those who disrupt the 'dharmic' order. It demands duty, obedience, and acceptance of one's social position from lower castes.
% ABSENT_VOICES: Lower castes, those targeted by 'dharmic' violence, and reformist thinkers who challenge the literal interpretation. They would argue for equality, non-violence, and a more inclusive understanding of dharma, but are structurally excluded from the interpretive discourse.
% DISAPPEARANCE_RATIONALE: If this orthodox literal reading and its enforcement vanished overnight, the divine sanction for caste hierarchy and 'righteous' violence would collapse. This would lead to profound challenges to existing social structures, re-evaluation of ethical norms, and a reordering of religious authority, fundamentally reorganizing the social and spiritual landscape.
% FOUNDING_PROBLEM: To provide ethical guidance and spiritual solace to Arjuna in a moment of moral crisis, specifically regarding his duty as a warrior (Kshatriya) to fight in a righteous war against kinsmen, and to establish the path to liberation (moksha) through action (karma yoga) within one's prescribed social role.
% FOUNDING_PROBLEM_CORROBORATION: Orthodox religious institutions and traditional scholars corroborate the founding problem's live status, emphasizing the timeless relevance of dharma and duty. Reformist movements and critical scholars (outside the benefiting parties) attest that the original context is largely superseded, and the interpretation now primarily serves to maintain social power structures, supporting a 'dead' or 'misinterpreted' status.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__orthodox_literal_reading, world_rearranges).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__orthodox_literal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gita_kurukshetra_discourse__orthodox_literal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__orthodox_literal_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__orthodox_literal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gita_kurukshetra_discourse__orthodox_literal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to the significant social and material benefits accrued by upper castes and the interpretive authority, at the direct expense of lower castes. Suppression is very high (0.90) because the constraint is presented as divinely mandated, making dissent or alternative social arrangements ideologically and structurally difficult to conceive or implement. Accessibility collapse is also very high (0.92) for the same reason. Resistance is moderate (0.30) as challenges have historically existed but faced strong institutional and ideological opposition. Theater ratio is moderate (0.40) as there's a genuine belief in upholding 'dharma', but also a performative aspect that masks the underlying extraction and suppression.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Brahminical interpretive authority and upper castes, this reading provides a coherent, divinely ordained framework for social and spiritual order. From the perspective of lower castes and reform movements, it is a highly extractive and suppressive system that uses religious authority to maintain power imbalances. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Brahminical interpretive authority, Kshatriya warrior class, and upper castes are structural beneficiaries, gaining status, power, and material advantage. Lower castes and those designated as 'enemies' in dharmic war are clear targets, bearing the costs of social rigidity and violence. Devotees seeking moksha are beneficiaries of spiritual guidance but are identity-locked into the framework. Reform movements are excluded, as their challenge to the literal interpretation is suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    caste_divine_mandate_ambiguity,
    'Is the caste system (varnashrama dharma) truly a divinely mandated social order, or is its interpretation as such a social construct serving specific power interests?',
    'Historical and sociological analysis of the evolution of caste, comparative religious studies on social stratification, and theological re-interpretations from within the tradition that challenge the divine origin of rigid caste hierarchy.',
    'If found to be a social construct, the constraint''s ''emerges_naturally'' aspect would be reclassified as false, significantly increasing its effective extractiveness and suppression, and shifting its classification further towards a Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(caste_divine_mandate_ambiguity, conceptual, 'Ambiguity regarding the divine vs. social construction of caste hierarchy.').

omega_variable(
    violence_righteousness_ambiguity,
    'Is the violence described in the Kurukshetra war literally righteous and applicable to contemporary conflict, or is it an allegorical representation of internal struggle, or a historical context-specific event not to be universalized?',
    'Textual hermeneutics comparing different interpretive traditions, ethical philosophical analysis of violence in religious texts, and the impact of such interpretations on real-world conflicts.',
    'If the violence is re-interpreted as allegorical or context-specific, the justification for ''righteous war'' would collapse, reducing the constraint''s suppression and extractiveness related to conflict, and potentially shifting its classification towards a more benign form of identity coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_righteousness_ambiguity, conceptual, 'Ambiguity regarding the literal vs. allegorical nature of righteous violence.').

omega_variable(
    interpretive_monopoly_legitimacy,
    'Is the Brahminical interpretive monopoly on the Bhagavad Gita legitimate and necessary for preserving its sacred meaning, or does it serve to control narrative and maintain social power?',
    'Analysis of historical power dynamics within religious institutions, examination of alternative interpretive traditions (e.g., Bhakti movements, reformist readings), and the impact of open vs. closed interpretive communities on social equity.',
    'If the monopoly is found to be primarily for power maintenance, the ''authority_grounding'' of the cs_structure would shift from ''lineage'' to ''extraction'', and the constraint''s effective suppression would increase, highlighting its coercive aspects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_monopoly_legitimacy, empirical, 'Legitimacy of the Brahminical interpretive monopoly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__orthodox_literal_reading, 500, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t500, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 500, 0.3).
narrative_ontology:measurement(gita_tr_t800, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 800, 0.33).
narrative_ontology:measurement(gita_tr_t1100, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1100, 0.36).
narrative_ontology:measurement(gita_tr_t1400, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1400, 0.39).
narrative_ontology:measurement(gita_tr_t1700, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 1700, 0.42).
narrative_ontology:measurement(gita_tr_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, theater_ratio, 2000, 0.4).

% Extraction over time
narrative_ontology:measurement(gita_be_t500, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 500, 0.8).
narrative_ontology:measurement(gita_be_t800, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 800, 0.83).
narrative_ontology:measurement(gita_be_t1100, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1100, 0.86).
narrative_ontology:measurement(gita_be_t1400, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1400, 0.88).
narrative_ontology:measurement(gita_be_t1700, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 1700, 0.87).
narrative_ontology:measurement(gita_be_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, base_extractiveness, 2000, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(gita_su_t500, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 500, 0.85).
narrative_ontology:measurement(gita_su_t800, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 800, 0.88).
narrative_ontology:measurement(gita_su_t1100, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1100, 0.91).
narrative_ontology:measurement(gita_su_t1400, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1400, 0.93).
narrative_ontology:measurement(gita_su_t1700, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 1700, 0.92).
narrative_ontology:measurement(gita_su_t2000, gita_kurukshetra_discourse__orthodox_literal_reading, suppression_requirement, 2000, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__orthodox_literal_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, hindu_social_norms_caste_system).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__orthodox_literal_reading, hindu_ethical_frameworks_duty_ethics).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Bhagavad Gita's Kurukshetra discourse kernel. Its ε value differs significantly from the 'gandhian_allegorical_reading' and 'universalist_devotional_reading' due to its literal interpretation of caste and violence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
