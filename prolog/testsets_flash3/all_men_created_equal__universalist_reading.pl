% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__universalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__universalist_reading, []).

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
 *   constraint_id: all_men_created_equal__universalist_reading
 *   human_readable: Universalist Reading of 'All Men Are Created Equal'
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universalist reading' of the phrase 'all
 *   men are created equal,' interpreting it as an evolving, expansive
 *   principle that demands iterative application to all individuals and
 *   groups, regardless of the founders' original intent. It is a reading that
 *   actively seeks to resolve the historical paradox of the founding by
 *   expanding the scope of equality. The claimed type is 'rope' because, from
 *   this perspective, the principle serves as a coordination mechanism for
 *   social progress and inclusion, despite the inherent costs and resistance
 *   to its expansion. The metrics reflect the ongoing effort and friction
 *   involved in this expansion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__universalist_reading, 0.45).
domain_priors:suppression_score(all_men_created_equal__universalist_reading, 0.3).
domain_priors:theater_ratio(all_men_created_equal__universalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(all_men_created_equal__universalist_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__universalist_reading, rope).
narrative_ontology:human_readable(all_men_created_equal__universalist_reading, "Universalist Reading of 'All Men Are Created Equal'").
narrative_ontology:topic_domain(all_men_created_equal__universalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__universalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__universalist_reading, 'c690fc9f-9d65-48ca-8e59-d0d30c0528b4').
narrative_ontology:cs_kernel_codification('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', fixed_text).
narrative_ontology:cs_authority_grounding('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', lineage).
narrative_ontology:cs_interpretation_layer_present('c690fc9f-9d65-48ca-8e59-d0d30c0528b4').
narrative_ontology:cs_reading_relation('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', all_men_created_equal__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', foundational, equality_as_evolving_principle).
narrative_ontology:cs_axiom_status(equality_as_evolving_principle, holdable).
narrative_ontology:cs_axiom_grounding('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', equality_as_evolving_principle, deontological).
narrative_ontology:cs_axiom('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', foundational, founder_intent_subordinate_to_universal_values).
narrative_ontology:cs_axiom_status(founder_intent_subordinate_to_universal_values, holdable).
narrative_ontology:cs_axiom_grounding('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', founder_intent_subordinate_to_universal_values, deontological).
narrative_ontology:cs_reference_frame('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', aspirational_founding_ideal).
narrative_ontology:cs_drift_state('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c690fc9f-9d65-48ca-8e59-d0d30c0528b4', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__universalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion).
narrative_ontology:constraint_beneficiary(all_men_created_equal__universalist_reading, civil_rights_advocates).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, institutions_resisting_expansion).
narrative_ontology:constraint_victim(all_men_created_equal__universalist_reading, groups_losing_exclusive_privilege).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups (e.g., women, racial minorities, LGBTQ+ individuals) invoke the principle to demand equal rights and protections, benefiting from its expansive interpretation. Their exit options are constrained by the ongoing struggle for recognition within the existing legal framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, marginalized_groups_claiming_inclusion, beneficiary,
    organized, generational, constrained, national).

% Legal scholars, activists, and organizations who actively champion the universalist interpretation, pushing for its application to new contexts and groups. They shape legal discourse and public opinion, benefiting from the principle's legitimizing power.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, civil_rights_advocates, agenda_setter,
    institutional, generational, mobile, national).

% Government bodies, legal systems, or social structures that historically upheld discriminatory practices and now face legal and social pressure to conform to an expanding definition of equality. They bear the costs of legal challenges, policy changes, and social upheaval.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, institutions_resisting_expansion, payer,
    institutional, biographical, constrained, national).

% Groups that previously enjoyed unearned advantages or exclusive status based on a narrower understanding of equality. They experience a loss of privilege as the principle expands, leading to resistance and attempts to reassert traditional hierarchies.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, groups_losing_exclusive_privilege, payer,
    powerful, biographical, constrained, national).

% Academics and legal practitioners who adhere to an originalist interpretation, arguing that the principle's meaning is fixed by the founders' intent. While their arguments are part of the public discourse, their specific reading is actively resisted by the universalist framework.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, originalist_scholars, excluded,
    analytical, generational, analytical, national).

% The ultimate arbiter of constitutional meaning, whose rulings iteratively expand or contract the scope of equality. Its decisions are a primary mechanism through which the universalist reading is enforced or challenged.
narrative_ontology:constraint_stakeholder(all_men_created_equal__universalist_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a foundational, aspirational principle around which diverse groups can coordinate their demands for justice and inclusion, and around which legal and social reforms can be legitimized.
% TRANSFER_FUNCTION: Transfers moral and legal authority from historical, restrictive interpretations of equality to an evolving, inclusive understanding, leading to a redistribution of rights, resources, and recognition towards previously marginalized groups.
% ABSENT_VOICES: Future generations and currently unrecognized groups (e.g., sentient AI, non-human animals in some philosophical frameworks) are implicitly absent, as their claims to equality are not yet fully articulated or recognized within the current legal and social discourse, but the universalist reading provides a framework for their eventual inclusion.
% DISAPPEARANCE_RATIONALE: If the universalist reading of equality vanished, the legal and social landscape would fundamentally rearrange. Civil rights movements would lose a core legitimizing principle, legal precedents built on expansion would be undermined, and the ongoing struggle for inclusion would lose its aspirational anchor, likely leading to a resurgence of exclusionary practices and a fragmentation of social justice efforts.
% FOUNDING_PROBLEM: The American founding documents articulated a principle of equality ('all men are created equal') that was immediately contradicted by the existence of slavery and other forms of systemic discrimination, creating a foundational tension between ideal and reality.
% FOUNDING_PROBLEM_CORROBORATION: Historians, political philosophers, and civil rights organizations universally corroborate the founding problem's existence and its ongoing relevance. The tension between the aspirational ideal and the historical reality of exclusion remains a central theme in American constitutional and social discourse, attested by ongoing legal battles and social movements.
narrative_ontology:disappearance_verdict(all_men_created_equal__universalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__universalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__universalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(all_men_created_equal__universalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__universalist_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__universalist_reading_tests).
:- end_tests(all_men_created_equal__universalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the 'coordination costs' of expanding equality – the effort, legal battles, and social friction required to overcome resistance and integrate new groups. It's not 'pure extraction' because the expansion genuinely benefits marginalized groups. Suppression (0.30) is low, as the universalist reading actively challenges existing suppressive structures rather than enforcing them. Theater ratio (0.10) is low and decreasing over time, indicating that this reading is primarily functional in driving social change, with minimal performative maintenance. The historical trend shows increasing extractiveness as more groups demand inclusion, and decreasing theater as the principle's functional role in social change becomes more pronounced.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of marginalized groups, this reading is a powerful tool for liberation and justice. From the perspective of those losing privilege, it can feel like an extractive force dismantling established order. The engine's per-seat classification will capture this divergence, showing the universalist reading as a beneficial rope for some and a costly tangled rope or snare for others, depending on their structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Marginalized groups and civil rights advocates are clear beneficiaries (low d), as the constraint's expansion directly serves their interests. Institutions resisting expansion and groups losing exclusive privilege are targets (high d), as they bear the costs of adapting to new norms and losing historical advantages. The Supreme Court acts as an agenda-setter, mediating the expansion and enforcement of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The universalist reading actively prevents mandatrophy by continuously re-evaluating and expanding the mandate of equality. It ensures the principle remains 'live' by applying it to contemporary challenges, rather than allowing it to become a vestigial declaration. The ongoing contestation with other readings (originalist, textualist) is precisely what keeps the principle from atrophying into a mere historical artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_personhood_ambiguity,
    'What are the ultimate boundaries of ''all men'' in the universalist reading? Does it extend beyond human beings to other sentient entities or even AI?',
    'Future philosophical and legal developments, potentially triggered by advancements in AI or increased understanding of animal consciousness, leading to new social movements and legal challenges.',
    'If the scope expands, the constraint''s beneficiaries and victims would broaden significantly, potentially increasing extractiveness as more entities demand inclusion and more institutions resist. If it remains human-centric, the current structural dynamics would persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_personhood_ambiguity, conceptual, 'Uncertainty regarding the ultimate reach of the universalist principle.').

omega_variable(
    pace_of_expansion_vs_social_cohesion,
    'At what pace can the universalist expansion of equality proceed without undermining social cohesion or triggering severe backlash that stalls progress?',
    'Empirical sociological studies on social change, political science analysis of backlash phenomena, and historical comparisons of civil rights movements across different eras and cultures.',
    'If the pace is too rapid, it could lead to increased resistance and suppression, potentially reclassifying the constraint as a tangled rope or snare for those resisting. If too slow, it could lead to increased resistance from marginalized groups and a perception of the principle as performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pace_of_expansion_vs_social_cohesion, empirical, 'The optimal rate of equality expansion to balance progress and social stability.').

omega_variable(
    universalist_vs_originalist_legitimacy,
    'Does the universalist reading derive its legitimacy primarily from the aspirational force of the text itself, or from its demonstrated capacity to foster a more just society, independent of founder intent?',
    'Analysis of judicial reasoning in landmark equality cases, public discourse on constitutional interpretation, and philosophical arguments regarding the sources of legal authority.',
    'If legitimacy is purely aspirational, it might be more vulnerable to challenges from originalist interpretations. If it''s grounded in societal outcomes, it gains a pragmatic defense, potentially strengthening its position against competing readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universalist_vs_originalist_legitimacy, conceptual, 'The primary source of legitimacy for the universalist interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__universalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__universalist_reading, theater_ratio, 1776, 0.5).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__universalist_reading, theater_ratio, 1865, 0.3).
narrative_ontology:measurement(all__tr_t1920, all_men_created_equal__universalist_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement(all__tr_t1964, all_men_created_equal__universalist_reading, theater_ratio, 1964, 0.15).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__universalist_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__universalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__universalist_reading, base_extractiveness, 1776, 0.1).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__universalist_reading, base_extractiveness, 1865, 0.25).
narrative_ontology:measurement(all__be_t1920, all_men_created_equal__universalist_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(all__be_t1964, all_men_created_equal__universalist_reading, base_extractiveness, 1964, 0.4).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__universalist_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__universalist_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__universalist_reading, suppression_requirement, 1776, 0.1).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__universalist_reading, suppression_requirement, 1865, 0.2).
narrative_ontology:measurement(all__su_t1920, all_men_created_equal__universalist_reading, suppression_requirement, 1920, 0.25).
narrative_ontology:measurement(all__su_t1964, all_men_created_equal__universalist_reading, suppression_requirement, 1964, 0.3).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__universalist_reading, suppression_requirement, 2000, 0.3).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__universalist_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__universalist_reading, identity_coordination).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, equal_protection_doctrine).
narrative_ontology:affects_constraint(all_men_created_equal__universalist_reading, voting_rights_act).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel. It is linked to the 'originalist_reading' and 'textualist_paradox_reading' through shared textual origin and ongoing interpretive contestation. Each reading presents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
