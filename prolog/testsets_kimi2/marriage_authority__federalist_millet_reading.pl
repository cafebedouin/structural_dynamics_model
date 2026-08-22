% ============================================================================
% CONSTRAINT STORY: marriage_authority__federalist_millet_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__federalist_millet_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: marriage_authority__federalist_millet_reading
 *   human_readable: Federalist Millet Reading of Marriage Authority
 *   domain: legal/constitutional/comparative_family_law
 *
 * SUMMARY:
 *   This constraint is the federalist_millet_reading of the
 *   marriage_authority kernel. It treats the fragmentation of marriage
 *   authority across religious or communal personal law systems as a
 *   deliberate consociational device â an elite constitutional bargain
 *   designed to prevent majoritarian tyranny in family law and secure
 *   minority consent to the regime. The reading shares structural overlap
 *   with communal_autonomy_reading but frames the arrangement as a federalist
 *   power-splitting mechanism rather than a recognition of pre-political
 *   community sovereignty. Sibling readings include communal_autonomy_reading
 *   (religious tradition grounding), secularist_reading (UCC transition),
 *   gender_rights_reading (equality-based reform), and
 *   judicial_harmonization_reading (case-by-case constitutional floor).
 *
 * KEY AGENTS:
 *   - Political elites: Constitutional architects and grand coalition managers who set the agenda of fragmented authority.
 *   - Minority communities: Organized beneficiaries of autonomous personal law regimes shielded from majoritarian imposition.
 *   - Majority community: Powerful but constrained beneficiary of inter-group peace, checked from imposing uniform marriage law.
 *   - Community religious authorities: Organized beneficiaries who receive state-delegated jurisdiction over family law.
 *   - Gender rights advocates: Excluded voices pressing intra-community equality against consociational stability prioritization.
 *   - Secularist reformers: Excluded voices pressing for uniform civil code and the elimination of religion-based pluralism.
 *   - Comparative constitutionalists: Analytical observers evaluating the stability effects of consociational fragmentation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__federalist_millet_reading, 0.25).
domain_priors:suppression_score(marriage_authority__federalist_millet_reading, 0.2).
domain_priors:theater_ratio(marriage_authority__federalist_millet_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__federalist_millet_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__federalist_millet_reading, rope).
narrative_ontology:human_readable(marriage_authority__federalist_millet_reading, "Federalist Millet Reading of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__federalist_millet_reading, "legal/constitutional/comparative_family_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__federalist_millet_reading, 'e9cd125b-3fea-42d3-b406-06f87f6d3f94').
narrative_ontology:cs_kernel_codification('e9cd125b-3fea-42d3-b406-06f87f6d3f94', formalized).
narrative_ontology:cs_authority_grounding('e9cd125b-3fea-42d3-b406-06f87f6d3f94', lineage).
narrative_ontology:cs_interpretation_layer_present('e9cd125b-3fea-42d3-b406-06f87f6d3f94').
narrative_ontology:cs_reading_relation('e9cd125b-3fea-42d3-b406-06f87f6d3f94', marriage_authority__communal_autonomy_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9cd125b-3fea-42d3-b406-06f87f6d3f94', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('e9cd125b-3fea-42d3-b406-06f87f6d3f94', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9cd125b-3fea-42d3-b406-06f87f6d3f94', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('e9cd125b-3fea-42d3-b406-06f87f6d3f94', foundational, fragmentation_prevents_tyranny).
narrative_ontology:cs_axiom_status(fragmentation_prevents_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('e9cd125b-3fea-42d3-b406-06f87f6d3f94', fragmentation_prevents_tyranny, instrumental).
narrative_ontology:cs_axiom('e9cd125b-3fea-42d3-b406-06f87f6d3f94', foundational, elite_bargain_legitimacy).
narrative_ontology:cs_axiom_status(elite_bargain_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e9cd125b-3fea-42d3-b406-06f87f6d3f94', elite_bargain_legitimacy, conventional).
narrative_ontology:cs_reference_frame('e9cd125b-3fea-42d3-b406-06f87f6d3f94', consociational_federalism).
narrative_ontology:cs_drift_state('e9cd125b-3fea-42d3-b406-06f87f6d3f94', contemporary_constitutional_politics, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e9cd125b-3fea-42d3-b406-06f87f6d3f94', '').
narrative_ontology:cs_kernel_id(marriage_authority__federalist_millet_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, minority_communities).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, majority_community).
narrative_ontology:constraint_beneficiary(marriage_authority__federalist_millet_reading, community_religious_authorities).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, consociationalism_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, legal_pluralism_legitimacy).
narrative_ontology:constraint_vindicates(marriage_authority__federalist_millet_reading, elite_bargain_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Crafted the constitutional bargain that fragments marriage authority across communities. They maintain the system through grand coalition governance and resist moves toward a uniform civil code that would unravel minority consent. Can alter the constitutional structure only at high risk of regime destabilization.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, political_elites, agenda_setter,
    institutional, generational, arbitrage, national).

% Receive constitutionally protected autonomy over personal law and marriage regulation, shielding them from majoritarian cultural imposition. Their community identity is reinforced by state recognition, but exit from the community's legal regime into a secular uniform framework is politically and socially difficult.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, minority_communities, beneficiary,
    organized, generational, constrained, national).

% Surrender the capacity to impose uniform family law in exchange for minority consent to the broader constitutional regime and inter-group peace. While culturally dominant, their legislative preferences are checked by the consociational pact.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, majority_community, beneficiary,
    powerful, generational, constrained, national).

% Exercise delegated state-recognized jurisdiction over marriage and family law within their communities. Their authority depends on the constitutional fragmentation continuing; they resist both state harmonization and internal reform that would bypass their institutional role.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, community_religious_authorities, beneficiary,
    organized, generational, constrained, regional).

% Advocate for gender equality norms within marriage law but are structurally marginalized in elite consociational bargaining, which treats inter-group stability as prior to intra-group equality reform.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, gender_rights_advocates, excluded,
    moderate, biographical, mobile, national).

% Press for a uniform civil code and the elimination of religion-based personal law, arguing that fragmented marriage authority violates secular constitutional principles. They are outside the elite bargain that sustains pluralism.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, secularist_reformers, excluded,
    moderate, biographical, mobile, national).

% Analyze consociational arrangements and federalist fragmentation of family law across divided societies, evaluating their stability effects without being bound to any one community's legal regime.
narrative_ontology:constraint_stakeholder(marriage_authority__federalist_millet_reading, comparative_constitutionalists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents majoritarian domination in family law by fragmenting marriage authority across recognized communities, thereby securing minority consent to the constitutional regime and averting zero-sum inter-group conflict over uniform norms.
% TRANSFER_FUNCTION: Moves jurisdictional authority over marriage from a centralized democratic legislature to community-specific personal law systems, and moves political stability from the elite bargain to all communities.
% ABSENT_VOICES: Women's rights advocates within minority communities and secularist reformers pressing for a uniform civil code are structurally sidelined in the elite bargain; their objections are treated as secondary to inter-group peace.
% DISAPPEARANCE_RATIONALE: If the fragmentation vanished and a single majoritarian marriage authority were imposed, minority communities would face cultural homogenization pressures, the elite bargain would unravel, and the consociational stability mechanism would collapse â the political order would rearrange around majoritarian versus minority bloc conflict.
% FOUNDING_PROBLEM: How to secure a stable constitutional democracy in a deeply divided society where a single marriage law would be seen as majoritarian domination and provoke minority alienation or secession.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional scholars and political scientists studying divided societies attest to the instability risk of majoritarian uniform law from outside the immediate beneficiary communities; historical evidence of civil conflict in comparable jurisdictions corroborates the founding problem.
narrative_ontology:disappearance_verdict(marriage_authority__federalist_millet_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__federalist_millet_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__federalist_millet_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__federalist_millet_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__federalist_millet_reading, 0.25, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__federalist_millet_reading_tests).
:- end_tests(marriage_authority__federalist_millet_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.25) because the constraint moves jurisdictional authority rather than material rents; no agent captures a concentrated revenue stream from the fragmentation itself. Suppression is low (0.20) because persistence relies on self-enforcing elite consensus and constitutional entrenchment rather than active coercion against daily behavior. Theater ratio is low (0.10) because the consociational function is genuine and not primarily performative. Accessibility collapse is moderate (0.45): the uniform civil code alternative is constitutionally and politically distant under this arrangement, though it remains rhetorically present. Resistance is moderate (0.35) because secularist and gender-equality challengers generate sustained contestation without destabilizing the core bargain. The measurement series share one time grid.
 *
 * PERSPECTIVAL GAP:
 *   The consociational elite seat experiences the constraint as necessary coordination that purchases regime stability at a low extraction cost. The minority community seat experiences it as protective autonomy. The excluded secularist and gender-rights seats, if they were incorporated as payers, would experience the same structure as a barrier to democratic legislation and gender equality â the engine computes this divergence from role and exit data. The majority community seat is structurally constrained but classified here as a stability beneficiary, producing a lower directionality than a pure target.
 *
 * DIRECTIONALITY LOGIC:
 *   Political elites sit near the symmetric-to-moderate beneficiary range: they designed the constraint and maintain it, but are themselves bound by the need to preserve the elite bargain. Minority communities and community religious authorities are clear beneficiaries (low d), receiving protected jurisdictional space. The majority community is coded as beneficiary because the consociational frame treats their legislative constraint as the price of stability and minority consent, not as extraction. Gender rights and secularist reformers are excluded rather than incorporated payers; if they were seated as payers their d would approach the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â deep societal division threatening constitutional collapse â remains live in divided societies. The constraint is therefore not a piton: its persistence is tied to the ongoing problem it was built to solve. Should the society's cleavages dissolve or the consociational bargain break, the fragmentation might atrophy into theatrical maintenance; at present, it retains a genuine coordination function. The classification as rope rather than scaffold reflects the absence of a sunset clause and the open-ended framing of the elite bargain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    majority_consent_ambiguity,
    'Is the majority community''s acceptance of fragmented marriage authority willing coordination for stability, or suppressed preference held in check by constitutional entrenchment?',
    'Electoral and survey evidence of majority preference for uniform civil code versus actual compliance with the consociational regime.',
    'If unwilling, effective suppression is higher and the constraint edges toward tangled rope; if willing, it remains a low-extraction rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_consent_ambiguity, empirical, 'Whether majority compliance is genuine or suppressed').

omega_variable(
    federalist_vs_communal_framing,
    'Does the legitimacy of fragmented marriage authority derive from the consociational state elite bargain (federalist) or from pre-political community religious tradition (communal autonomy)?',
    'Comparative historical analysis of constitutional founding moments: was the system imposed as a federalist device or recognized as existing communal autonomy?',
    'Federalist framing makes the state the authoritative interpreter; communal framing makes religious authorities the authoritative interpreters, shifting cs_structure authority_grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalist_vs_communal_framing, conceptual, 'Alternative framing of authority source for the fragmentation').

omega_variable(
    stability_equality_tradeoff,
    'Does the consociational stability produced by marriage fragmentation require permanent sacrifice of gender equality within personal laws?',
    'Cross-national comparison of consociational systems with and without internal personal-law reform.',
    'If tradeoff is rigid, the coordination function carries a distributive cost that gender-rights readings classify as extraction; if reform is possible within fragmentation, the rope framing is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stability_equality_tradeoff, empirical, 'Whether stability and gender equality are structurally incompatible under this constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__federalist_millet_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(famr_tr_t0, marriage_authority__federalist_millet_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(famr_tr_t6, marriage_authority__federalist_millet_reading, theater_ratio, 6, 0.06).
narrative_ontology:measurement(famr_tr_t12, marriage_authority__federalist_millet_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(famr_tr_t18, marriage_authority__federalist_millet_reading, theater_ratio, 18, 0.08).
narrative_ontology:measurement(famr_tr_t24, marriage_authority__federalist_millet_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(famr_tr_t30, marriage_authority__federalist_millet_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(famr_be_t0, marriage_authority__federalist_millet_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(famr_be_t6, marriage_authority__federalist_millet_reading, base_extractiveness, 6, 0.21).
narrative_ontology:measurement(famr_be_t12, marriage_authority__federalist_millet_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement(famr_be_t18, marriage_authority__federalist_millet_reading, base_extractiveness, 18, 0.23).
narrative_ontology:measurement(famr_be_t24, marriage_authority__federalist_millet_reading, base_extractiveness, 24, 0.24).
narrative_ontology:measurement(famr_be_t30, marriage_authority__federalist_millet_reading, base_extractiveness, 30, 0.25).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(marriage_authority__federalist_millet_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__federalist_millet_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, judicial_harmonization_reading).
narrative_ontology:affects_constraint(marriage_authority__federalist_millet_reading, gender_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_authority kernel. The kernel decomposes into multiple structurally distinct claims (communal autonomy, federalist elite bargain, secularist UCC, gender equality, judicial harmonization) because each reading assigns different authority grounding, beneficiary structure, and epsilon. This reading focuses on the consociational federalist framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
