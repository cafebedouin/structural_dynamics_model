% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__communal_autonomy_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Communal Autonomy in Marriage Authority (Communal Autonomy Reading)
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint represents the 'communal autonomy' reading of the
 *   'marriage authority' kernel, where marriage and family law are primarily
 *   governed by community religious traditions, with the state acting as an
 *   enforcer rather than an author of these norms. This institutionalizes
 *   personal law variation, benefiting religious leadership and community
 *   members by preserving distinct identities, but potentially extracting
 *   from intra-community dissenters. The claimed type is 'rope' reflecting
 *   the coordination function for the community, but the metrics acknowledge
 *   the suppressive and extractive aspects for those who do not conform.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.35).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.6).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy in Marriage Authority (Communal Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '48c839d1-5155-4ebb-afd9-3bc300e81683').
narrative_ontology:cs_kernel_codification('48c839d1-5155-4ebb-afd9-3bc300e81683', formalized).
narrative_ontology:cs_authority_grounding('48c839d1-5155-4ebb-afd9-3bc300e81683', lineage).
narrative_ontology:cs_interpretation_layer_present('48c839d1-5155-4ebb-afd9-3bc300e81683').
narrative_ontology:cs_reading_relation('48c839d1-5155-4ebb-afd9-3bc300e81683', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('48c839d1-5155-4ebb-afd9-3bc300e81683', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('48c839d1-5155-4ebb-afd9-3bc300e81683', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('48c839d1-5155-4ebb-afd9-3bc300e81683', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('48c839d1-5155-4ebb-afd9-3bc300e81683', foundational, community_self_governance_is_fundamental).
narrative_ontology:cs_axiom_status(community_self_governance_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('48c839d1-5155-4ebb-afd9-3bc300e81683', community_self_governance_is_fundamental, deontological).
narrative_ontology:cs_axiom('48c839d1-5155-4ebb-afd9-3bc300e81683', foundational, state_respects_religious_personal_law).
narrative_ontology:cs_axiom_status(state_respects_religious_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('48c839d1-5155-4ebb-afd9-3bc300e81683', state_respects_religious_personal_law, conventional).
narrative_ontology:cs_reference_frame('48c839d1-5155-4ebb-afd9-3bc300e81683', traditional_community_governance).
narrative_ontology:cs_drift_state('48c839d1-5155-4ebb-afd9-3bc300e81683', contemporary_individual_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('48c839d1-5155-4ebb-afd9-3bc300e81683', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_members).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and interprets marriage norms according to religious tradition. Benefits from the state's enforcement of these norms, which solidifies their authority and the community's distinct identity. Faces pressure from both internal dissent and external secular forces.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_community_leadership, agenda_setter,
    institutional, generational, constrained, local).

% Benefits from the stability and clarity of community-specific marriage norms, which reinforce cultural identity and social cohesion. May experience social pressure to conform, but generally aligns with the traditional framework.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Bears the costs of non-conformity to community marriage norms, which can include social ostracism, legal disadvantages (e.g., regarding inheritance or child custody), or difficulty accessing community services. Exit is difficult due to deep social and identity ties.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, local).

% Enforces the marriage and family law norms derived from religious community traditions, without actively authoring them. Benefits from maintaining social order and avoiding direct conflict with powerful religious groups. Its enforcement role lends legal weight to community traditions.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, the_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Analyzes the legal and social implications of legal pluralism in family law, often highlighting tensions between communal autonomy and universal human rights or secular legal principles. Does not directly participate in the constraint's operation but influences public and judicial discourse.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, secular_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__communal_autonomy_reading, religious_community_leadership).
narrative_ontology:fixing_cost_class(marriage_authority__communal_autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable and culturally resonant framework for marriage and family life within specific religious communities, reducing internal disputes over norms and preserving distinct cultural identities.
% TRANSFER_FUNCTION: Transfers primary authority over family law from the general democratic legislature to specific religious community leaders, and imposes social and legal conformity on dissenters within those communities.
% ABSENT_VOICES: Secularists advocating for a uniform civil code and gender rights advocates challenging discriminatory aspects of personal laws are often excluded from the direct decision-making processes within these communities, though their arguments influence broader legal and political debates.
% DISAPPEARANCE_RATIONALE: If state enforcement of community-derived marriage norms vanished overnight, the legal validity of religiously sanctioned marriages would become uncertain, leading to widespread legal and social instability within and across communities. Family structures, inheritance, and child custody would be thrown into disarray, forcing a rapid reorganization of legal frameworks.
% FOUNDING_PROBLEM: To preserve the distinct religious and cultural identities of minority communities by allowing them to govern their own personal affairs, thereby preventing assimilation into a dominant secular or majoritarian legal system and protecting their way of life.
% FOUNDING_PROBLEM_CORROBORATION: Religious community leaders and some legal pluralism advocates attest that the problem of preserving distinct identities and traditions remains live. Critics, including secularists and gender rights advocates, contest the fairness and contemporary relevance of this arrangement, arguing that it often entrenches inequality, but acknowledge the historical impetus for cultural preservation.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).
:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is moderate (0.35) because while the system provides coordination for the majority, it imposes costs on dissenters who may face social and legal disadvantages. Suppression is higher (0.60) due to both state enforcement and strong community social pressure, making exit or non-conformity difficult. The theater ratio is low (0.20) as the state's enforcement role is genuine, though its 'neutrality' in not authoring the norms can be seen as performative when those norms are challenged. The slight fluctuations in metrics over time reflect ongoing societal debates and judicial interventions that subtly shift the balance without fundamentally altering the structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious community leadership, this arrangement is a legitimate 'rope' that preserves cultural and religious identity. From the perspective of intra-community dissenters, it can feel more like a 'snare' due to the high social and legal costs of non-conformity and limited exit options. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious community leadership and members are beneficiaries, gaining stability and identity preservation. The state, by enforcing these norms, also benefits from social order and avoiding direct conflict. Intra-community dissenters are the primary targets, bearing the costs of non-conformity due to social and legal pressures. Secular legal scholars act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_for_dissenters,
    'Is the constraint''s primary function coordination for the community, or extraction from intra-community dissenters?',
    'Analysis of exit costs and available alternatives for dissenters: if exit costs are prohibitively high and alternatives are suppressed, it leans towards extraction.',
    'If primarily extraction, the constraint''s effective type for dissenters would shift from a modified rope towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_for_dissenters, conceptual, 'Ambiguity in the constraint''s function for different internal groups.').

omega_variable(
    state_neutrality_vs_endorsement,
    'Is the state''s enforcement of community religious traditions truly neutral, or does it implicitly endorse and subsidize specific religious authorities?',
    'Legal analysis of state funding or preferential treatment for religious institutions involved in family law, or judicial rulings on the state''s ''neutrality'' in practice.',
    'If the state implicitly endorses, its role shifts from a neutral enforcer to an active supporter, increasing the constraint''s legitimacy and suppressive power for the community, but also its entanglement with religious authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_neutrality_vs_endorsement, empirical, 'The true nature of the state''s role in legal pluralism.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression for intra-community dissenters primarily structural (state enforcement, community ostracism) or internalized (belief in tradition, fear of social/divine punishment)?',
    'Post-exit trajectory analysis: if dissenters continue to self-regulate or face internal conflict even after leaving the community or jurisdiction, it suggests a significant internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as dissenters carry the suppression with them after any formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenters.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1990, marriage_authority__communal_autonomy_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(marr_tr_t1996, marriage_authority__communal_autonomy_reading, theater_ratio, 1996, 0.19).
narrative_ontology:measurement(marr_tr_t2002, marriage_authority__communal_autonomy_reading, theater_ratio, 2002, 0.2).
narrative_ontology:measurement(marr_tr_t2008, marriage_authority__communal_autonomy_reading, theater_ratio, 2008, 0.21).
narrative_ontology:measurement(marr_tr_t2014, marriage_authority__communal_autonomy_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(marr_tr_t2020, marriage_authority__communal_autonomy_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(marr_be_t1990, marriage_authority__communal_autonomy_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(marr_be_t1996, marriage_authority__communal_autonomy_reading, base_extractiveness, 1996, 0.32).
narrative_ontology:measurement(marr_be_t2002, marriage_authority__communal_autonomy_reading, base_extractiveness, 2002, 0.34).
narrative_ontology:measurement(marr_be_t2008, marriage_authority__communal_autonomy_reading, base_extractiveness, 2008, 0.35).
narrative_ontology:measurement(marr_be_t2014, marriage_authority__communal_autonomy_reading, base_extractiveness, 2014, 0.36).
narrative_ontology:measurement(marr_be_t2020, marriage_authority__communal_autonomy_reading, base_extractiveness, 2020, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1990, marriage_authority__communal_autonomy_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(marr_su_t1996, marriage_authority__communal_autonomy_reading, suppression_requirement, 1996, 0.57).
narrative_ontology:measurement(marr_su_t2002, marriage_authority__communal_autonomy_reading, suppression_requirement, 2002, 0.59).
narrative_ontology:measurement(marr_su_t2008, marriage_authority__communal_autonomy_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(marr_su_t2014, marriage_authority__communal_autonomy_reading, suppression_requirement, 2014, 0.61).
narrative_ontology:measurement(marr_su_t2020, marriage_authority__communal_autonomy_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
