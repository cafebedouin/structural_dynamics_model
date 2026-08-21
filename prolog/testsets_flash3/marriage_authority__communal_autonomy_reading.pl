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
 *   human_readable: Marriage Authority (Communal Autonomy Reading)
 *   domain: legal_pluralism/constitutional_law/family_law
 *
 * SUMMARY:
 *   This constraint describes the operation of marriage authority when it is
 *   primarily grounded in community religious tradition, with the state
 *   acting as an enforcer of these norms rather than their author. This
 *   'communal autonomy' reading emphasizes the right of religious groups to
 *   self-govern their personal law, leading to institutionalized personal law
 *   variation. The constraint is claimed as a Rope, reflecting its
 *   coordination function for community members, but its metrics show
 *   moderate extraction and substantial suppression, particularly for
 *   intra-community dissenters and women seeking equality. Legislative
 *   amendments to personal law typically require the consent of the affected
 *   community, reinforcing the autonomy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.35).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.6).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Marriage Authority (Communal Autonomy Reading)").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, 'ba14008d-c2ba-484d-bb06-b2962e983513').
narrative_ontology:cs_kernel_codification('ba14008d-c2ba-484d-bb06-b2962e983513', formalized).
narrative_ontology:cs_authority_grounding('ba14008d-c2ba-484d-bb06-b2962e983513', lineage).
narrative_ontology:cs_interpretation_layer_present('ba14008d-c2ba-484d-bb06-b2962e983513').
narrative_ontology:cs_reading_relation('ba14008d-c2ba-484d-bb06-b2962e983513', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba14008d-c2ba-484d-bb06-b2962e983513', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('ba14008d-c2ba-484d-bb06-b2962e983513', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba14008d-c2ba-484d-bb06-b2962e983513', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('ba14008d-c2ba-484d-bb06-b2962e983513', foundational, communal_religious_autonomy_is_foundational).
narrative_ontology:cs_axiom_status(communal_religious_autonomy_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('ba14008d-c2ba-484d-bb06-b2962e983513', communal_religious_autonomy_is_foundational, deontological).
narrative_ontology:cs_axiom('ba14008d-c2ba-484d-bb06-b2962e983513', secondary, state_should_not_legislate_personal_law).
narrative_ontology:cs_axiom_status(state_should_not_legislate_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('ba14008d-c2ba-484d-bb06-b2962e983513', state_should_not_legislate_personal_law, conventional).
narrative_ontology:cs_reference_frame('ba14008d-c2ba-484d-bb06-b2962e983513', traditional_communal_governance).
narrative_ontology:cs_drift_state('ba14008d-c2ba-484d-bb06-b2962e983513', contemporary_individual_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('ba14008d-c2ba-484d-bb06-b2962e983513', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_community_members).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, women_seeking_equality_in_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and interprets religious personal law for marriage, divorce, and inheritance. Benefits from the state's recognition and enforcement of these norms, which solidifies its authority within the community. Resists state legislative interference in communal religious matters.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_community_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the stability and cultural continuity provided by religiously sanctioned marriage norms. Their social identity and family structures are often deeply intertwined with these traditions. Exit means social ostracization or loss of community support.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_community_members, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of rigid adherence to traditional norms, particularly when seeking divorce or challenging inheritance rules that may be inequitable. Their identity is often tied to the community, making formal exit costly or unthinkable, and internal dissent is often suppressed.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, immediate, identity_locked, local).

% Often face discriminatory provisions within personal laws regarding divorce, maintenance, and property rights. They seek reform to align personal laws with constitutional equality guarantees, but their efforts are often met with resistance from community leadership.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, women_seeking_equality_in_marriage, payer,
    powerless, biographical, constrained, national).

% Enforces personal laws as interpreted by community leadership, but also adjudicates challenges based on constitutional principles. Operates within a framework that recognizes communal autonomy while also upholding fundamental rights. Its role is to harmonize, not to legislate personal law.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, the_state_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for a Uniform Civil Code to eliminate personal law pluralism, arguing it undermines national unity and gender equality. They are excluded from the direct administration of personal law but exert political and legal pressure for reform.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, secularist_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable framework for marriage, family, and inheritance within specific religious communities, allowing members to live according to their traditions without state interference in norm-setting.
% TRANSFER_FUNCTION: Transfers authority over family law matters from the general democratic legislature to specific religious community leadership, in exchange for social cohesion and cultural preservation within those communities.
% ABSENT_VOICES: Secularist advocates and individual rights groups are often marginalized in discussions about personal law reform, as the discourse is framed around communal religious freedom. Their arguments for a uniform civil code are often dismissed as an attack on religious identity.
% DISAPPEARANCE_RATIONALE: If state enforcement of communal personal laws vanished, religious communities would face immediate legal uncertainty regarding marriage, divorce, and inheritance. This would force communities to either self-enforce (with limited legal standing) or push for new state recognition, fundamentally altering the current legal pluralism.
% FOUNDING_PROBLEM: To allow diverse religious communities to govern their internal family matters according to their own traditions, preventing majoritarian imposition of a single family law code and preserving cultural and religious identity.
% FOUNDING_PROBLEM_CORROBORATION: Religious community leadership universally attests the problem is live, citing the importance of religious freedom and cultural preservation. Some constitutional scholars and historians corroborate the historical intent to protect minority religious practices, though they may contest the contemporary application regarding individual rights.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The extractiveness (0.35) is moderate, reflecting the costs borne by those within the community who dissent from traditional norms or seek reforms. Suppression (0.6) is substantial, as community pressure and state enforcement combine to limit alternatives and exits for those bound by personal law. The theater ratio is low (0.1) because the system is genuinely functional in maintaining communal order, not merely performative. The slight increase in extractiveness and suppression over time reflects growing tensions between traditional norms and evolving individual rights claims.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of religious community leadership, this is a legitimate Rope that preserves religious freedom and cultural identity. From the perspective of intra-community dissenters, it operates as a Snare, trapping them in inequitable arrangements with limited exit options. The engine's per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim status of each stakeholder.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious community leadership and many community members are beneficiaries, as the system preserves their traditions and authority. Intra-community dissenters and women seeking equality are victims, bearing the costs of rigid norms and limited recourse. The state judiciary acts as an agenda-setter, enforcing the norms while also mediating constitutional challenges, but does not author the primary norms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_consent_vs_individual_rights,
    'To what extent does ''communal consent'' for legislative amendments genuinely represent the will of all community members, particularly those with less power, versus reflecting the preferences of community leadership?',
    'Independent, anonymous surveys of community members, disaggregated by gender and socio-economic status, on proposed reforms to personal law. Analysis of internal community dispute resolution mechanisms for fairness and accessibility.',
    'If ''communal consent'' is found to be primarily top-down, the constraint''s effective suppression and extractiveness for dissenters would be higher than currently measured, potentially reclassifying it as a Tangled Rope or Snare for those seats. If genuine, it reinforces the Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_consent_vs_individual_rights, empirical, 'Assesses the authenticity and representativeness of ''communal consent'' in personal law amendments.').

omega_variable(
    state_enforcement_vs_authoring_boundary,
    'Is the state''s role truly limited to ''enforcing but not authoring'' personal law norms, or does its selective enforcement and judicial review implicitly shape or ''author'' the effective norms over time?',
    'Comparative legal analysis of judicial precedents and legislative non-action across different personal law codes. Examination of how judicial interpretations of constitutional equality principles interact with and modify traditional norms.',
    'If the state''s judicial review is found to significantly shape or override traditional norms, the ''communal autonomy'' framing would be weakened, and the constraint would lean more towards a ''judicial harmonization'' reading, potentially increasing the perceived extractiveness from community leadership''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_vs_authoring_boundary, conceptual, 'Clarifies the boundary between state enforcement and implicit state authorship in legal pluralism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__communal_autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t10, marriage_authority__communal_autonomy_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(marr_tr_t20, marriage_authority__communal_autonomy_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(marr_tr_t30, marriage_authority__communal_autonomy_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__communal_autonomy_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(marr_tr_t50, marriage_authority__communal_autonomy_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__communal_autonomy_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(marr_be_t10, marriage_authority__communal_autonomy_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(marr_be_t20, marriage_authority__communal_autonomy_reading, base_extractiveness, 20, 0.31).
narrative_ontology:measurement(marr_be_t30, marriage_authority__communal_autonomy_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(marr_be_t40, marriage_authority__communal_autonomy_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(marr_be_t50, marriage_authority__communal_autonomy_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__communal_autonomy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(marr_su_t10, marriage_authority__communal_autonomy_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(marr_su_t20, marriage_authority__communal_autonomy_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(marr_su_t30, marriage_authority__communal_autonomy_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(marr_su_t40, marriage_authority__communal_autonomy_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(marr_su_t50, marriage_authority__communal_autonomy_reading, suppression_requirement, 50, 0.6).


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
