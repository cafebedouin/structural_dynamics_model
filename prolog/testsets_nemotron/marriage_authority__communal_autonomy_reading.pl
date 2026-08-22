% ============================================================================
% CONSTRAINT STORY: marriage_authority__communal_autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: marriage_authority__communal_autonomy_reading
 *   human_readable: Communal Autonomy Reading of Marriage Authority
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This constraint story captures the communal autonomy reading of marriage
 *   authority: the state recognizes and enforces community-specific personal
 *   laws for marriage, divorce, and inheritance, but does not author them.
 *   Legislative amendments require consent from religious leadership. The
 *   arrangement presents as coordination (managing pluralism, preventing
 *   majoritarian domination) but operates with asymmetric extraction:
 *   religious leadership and communal institutions benefit from gatekeeping
 *   authority and resource control, while intra-community dissenters, gender
 *   rights advocates, and interfaith couples bear the costs of patriarchal
 *   interpretations and jurisdictional gaps. The state legislature holds
 *   formal enactment power but cannot exercise it without communal consent —
 *   a captured agenda-setter. Constitutional courts see the full structure
 *   but are doctrinally constrained from systemic intervention.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, 0.55).
domain_priors:suppression_score(marriage_authority__communal_autonomy_reading, 0.65).
domain_priors:theater_ratio(marriage_authority__communal_autonomy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority__communal_autonomy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__communal_autonomy_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__communal_autonomy_reading, "Communal Autonomy Reading of Marriage Authority").
narrative_ontology:topic_domain(marriage_authority__communal_autonomy_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__communal_autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__communal_autonomy_reading, '62c6947c-a623-4b42-8b4d-fab626f714a5').
narrative_ontology:cs_kernel_codification('62c6947c-a623-4b42-8b4d-fab626f714a5', formalized).
narrative_ontology:cs_authority_grounding('62c6947c-a623-4b42-8b4d-fab626f714a5', lineage).
narrative_ontology:cs_interpretation_layer_present('62c6947c-a623-4b42-8b4d-fab626f714a5').
narrative_ontology:cs_reading_relation('62c6947c-a623-4b42-8b4d-fab626f714a5', marriage_authority__secularist_reading, forecloses).
narrative_ontology:cs_reading_relation('62c6947c-a623-4b42-8b4d-fab626f714a5', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('62c6947c-a623-4b42-8b4d-fab626f714a5', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_reading_relation('62c6947c-a623-4b42-8b4d-fab626f714a5', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('62c6947c-a623-4b42-8b4d-fab626f714a5', foundational, communal_consent_gate_for_personal_law_amendments).
narrative_ontology:cs_axiom_status(communal_consent_gate_for_personal_law_amendments, holdable).
narrative_ontology:cs_axiom_grounding('62c6947c-a623-4b42-8b4d-fab626f714a5', communal_consent_gate_for_personal_law_amendments, conventional).
narrative_ontology:cs_axiom('62c6947c-a623-4b42-8b4d-fab626f714a5', foundational, religious_tradition_as_legitimate_source_of_family_law).
narrative_ontology:cs_axiom_status(religious_tradition_as_legitimate_source_of_family_law, holdable).
narrative_ontology:cs_axiom_grounding('62c6947c-a623-4b42-8b4d-fab626f714a5', religious_tradition_as_legitimate_source_of_family_law, theological).
narrative_ontology:cs_axiom('62c6947c-a623-4b42-8b4d-fab626f714a5', secondary, state_enforcement_without_state_authorship_of_family_norms).
narrative_ontology:cs_axiom_status(state_enforcement_without_state_authorship_of_family_norms, holdable).
narrative_ontology:cs_axiom_grounding('62c6947c-a623-4b42-8b4d-fab626f714a5', state_enforcement_without_state_authorship_of_family_norms, conventional).
narrative_ontology:cs_reference_frame('62c6947c-a623-4b42-8b4d-fab626f714a5', constitutional_settlement_1950_communal_autonomy_bargain).
narrative_ontology:cs_drift_state('62c6947c-a623-4b42-8b4d-fab626f714a5', contemporary_constitutional_equality_jurisprudence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('62c6947c-a623-4b42-8b4d-fab626f714a5', '').
narrative_ontology:cs_kernel_id(marriage_authority__communal_autonomy_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, religious_leadership).
narrative_ontology:constraint_beneficiary(marriage_authority__communal_autonomy_reading, communal_institutions).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, intra_community_dissenters).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, gender_rights_advocates).
narrative_ontology:constraint_victim(marriage_authority__communal_autonomy_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and administer personal law codes for marriage, divorce, inheritance within their community. Derive authority from religious tradition and state recognition. Control the process for legislative amendments to personal law, which require their consent. Benefit from institutional legitimacy, resource control over community institutions, and gatekeeping authority over family formation.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, religious_leadership, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, religious_leadership, agenda_setter).

% Religious courts, waqf boards, community councils that implement personal law. Receive state funding and legal recognition. Depend on the communal autonomy framework for their institutional survival and authority. Collect fees, manage endowments, and exercise social control through family law adjudication.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, communal_institutions, beneficiary,
    organized, generational, constrained, regional).

% Community members who contest patriarchal interpretations, seek divorce rights, challenge inheritance inequalities, or reject religious authority over personal status. Face social ostracism, loss of community support, and legal barriers to exit. Their identity is fused with community membership; leaving the community means losing kinship, economic, and spiritual networks. State courts rarely intervene in 'internal' matters.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer,
    powerless, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, intra_community_dissenters, payer).

% Feminist organizations, women's rights lawyers, and reformist scholars who challenge gender-discriminatory provisions in personal laws. Excluded from the communal consent process for legislative amendments. Pursue litigation in constitutional courts but face doctrinal barriers (essential religious practice test, non-interference in personal law). Their reforms are blocked by the communal veto.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, gender_rights_advocates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, gender_rights_advocates, excluded).

% Couples crossing community boundaries who fall into jurisdictional gaps. Neither community's personal law fully governs them; state marriage law (Special Marriage Act) exists but requires public notice periods that expose them to family/community violence. No community claims them; the state refuses to author a unified framework. They bear the cost of legal pluralism's boundary failures.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, interfaith_couples, payer,
    powerless, immediate, trapped, local).

% Formally enacts personal law amendments but only with communal consent. Cannot unilaterally reform family law. Competes with religious leadership for legitimacy. Enforces communal courts' decrees through state machinery. Holds the monopoly on coercion but has ceded normative authority over family law to communities.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, state_legislature, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__communal_autonomy_reading, state_legislature, observer).

% Adjudicate challenges to personal law provisions on equality grounds. Apply the essential religious practice test and often defer to communal autonomy. Their judgments create a patchwork of reforms without systemic change. See the full structural picture but are constrained by separation-of-powers doctrine from legislative action.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, constitutional_courts, observer,
    institutional, generational, analytical, national).

% Advocates for Uniform Civil Code who view communal autonomy as a transitional anomaly. Excluded from the communal consent process. Their political project requires majoritarian legislative action, which they have not achieved. Would eliminate the constraint entirely if politically viable.
narrative_ontology:constraint_stakeholder(marriage_authority__communal_autonomy_reading, secularist_reformers, excluded,
    moderate, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages deep diversity in a plural society by institutionalizing community self-governance over family law, preventing majoritarian imposition of a single code, and providing recognized forums for dispute resolution within each community's normative universe.
% TRANSFER_FUNCTION: Transfers normative authority over marriage, divorce, inheritance, and child custody from the democratic legislature to religious leadership, in exchange for state enforcement of communal court decrees. Transfers the costs of non-conformity (social exclusion, legal disadvantage) to intra-community dissenters and boundary-crossing individuals.
% ABSENT_VOICES: Intra-community dissenters (especially women, LGBTQ+ members, reformist minorities) are structurally excluded from the communal consent process that gates legislative amendments. Interfaith couples fall into jurisdictional voids with no community to represent them. Their objections are treated as internal community matters or individual choices, not structural flaws.
% DISAPPEARANCE_RATIONALE: If communal autonomy over marriage authority vanished overnight, the state would face immediate pressure to legislate a unified family law code (Uniform Civil Code). Religious leadership would lose institutional control over family law adjudication and associated resources. Intra-community dissenters would gain access to state courts for rights claims but lose community-based dispute resolution. Interfaith couples would have a single legal framework but lose community recognition. The legal pluralism infrastructure (religious courts, waqf boards, community councils) would face existential crisis.
% FOUNDING_PROBLEM: Post-colonial constitutional settlement needed to accommodate deep religious diversity while maintaining state unity. Communal autonomy over personal law was the bargain: communities accept state sovereignty in exchange for self-governance in family law. The alternative was partition or endless communal conflict.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional Assembly debates and the Constituent Assembly's decision to place personal law in the Concurrent List (not Union List) corroborate the founding bargain. However, feminist historians (e.g., Flavia Agnes, Nivedita Menon) and the Shah Bano judgment dissenting opinions attest that the bargain entrenched patriarchal authority within communities. The 'founding problem' of communal peace is attested by the state; the 'founding problem' of gender justice is attested by excluded voices — neither fully corroborates the other.
narrative_ontology:disappearance_verdict(marriage_authority__communal_autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__communal_autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__communal_autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(marriage_authority__communal_autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__communal_autonomy_reading, 0.55, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__communal_autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__communal_autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__communal_autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate: the constraint transfers normative authority and material benefits to religious leadership, but the coordination function (preventing communal conflict, providing dispute resolution) is genuine and valued by many community members. Suppression (0.65) is higher: exit is identity-locked for dissenters, and the communal veto blocks legislative reform. Theater ratio (0.25) is low-moderate: the coordination function is real but increasingly performs as cover for extraction. Accessibility collapse (0.6) reflects that alternatives (uniform civil code, individual opt-out) are structurally blocked. Resistance (0.45) is moderate: litigation and advocacy persist but face doctrinal barriers. The claimed type is tangled_rope because both coordination and extraction are structurally present and require active enforcement (state enforcement of communal decrees, communal veto on amendments).
 *
 * PERSPECTIVAL GAP:
 *   From the religious leadership seat: the constraint is a rope — genuine coordination managing pluralism, protecting community survival. From the intra-community dissenter seat: it is a snare — extraction enforced through identity lock and communal veto. From the constitutional court seat: it is a tangled rope — both coordination and extraction visible, but doctrinal tools (essential religious practice test) force a binary choice that misses the hybrid. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious leadership and communal institutions are structural beneficiaries (d near 0.0): they collect authority, resources, and legitimacy from the arrangement. Intra-community dissenters are full targets (d near 1.0): identity-locked, bear extraction costs, cannot exit without losing everything. Gender rights advocates are constrained targets (d ~0.7): excluded from consent process, some mobility through litigation. Interfaith couples are trapped targets (d ~0.9): no community claims them, state refuses unified framework. State legislature is a captured agenda-setter (d ~0.3): formal power but constrained by communal consent. Constitutional courts are analytical observers (d=0.5): see structure but cannot act systemically. Secularist reformers are mobile excluded (d~0.2): not subject to extraction but cannot access the constraint's benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communal peace via autonomy bargain) is contested: the state attests it remains live; excluded voices attest it has shifted to gender justice. The constraint persists because the communal veto blocks reform, and no political coalition has formed for Uniform Civil Code. Mandatrophy is unresolved: the coordination function (communal peace) may be live, but the extraction function (patriarchal authority) has expanded beyond the founding bargain. The theater ratio rise tracks this divergence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_consent_gate_authenticity,
    'Does the communal consent requirement for legislative amendments reflect genuine community self-determination, or is it a capture mechanism by religious elites?',
    'Empirical study of consent processes: who is consulted, what dissent is recorded, whether women''s organizations within communities have voice. Compare amendment histories across communities.',
    'If capture mechanism, the coordination function is substantially extractive; if genuine self-determination, the rope component is stronger. Affects ε and the tangled_rope vs snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_consent_gate_authenticity, empirical, 'Whether the communal veto is democratic or elitist within communities.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit option for intra-community dissenters structural (legal barriers, economic dependency) or internalized (religious identity as self-concept, fear of spiritual consequences)?',
    'Post-exit trajectory studies: do dissenters who leave communities maintain suppression internally? Qualitative work on religious identity formation and exit costs.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent after exit. If structural, state intervention could reduce exit costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Structural vs internalized suppression mechanism for identity-locked agents.').

omega_variable(
    coordination_extraction_separability,
    'Can the pluralism-coordination function (preventing communal conflict, providing dispute forums) be separated from the extraction function (patriarchal authority, communal resource control)?',
    'Counterfactual: jurisdictions that have implemented partial reforms (e.g., optional civil marriage alongside personal law) — does communal conflict increase? Do dispute forums survive without extraction?',
    'If separable, the constraint is a tangled rope with a removable extraction layer. If inseparable, the extraction is the price of coordination — true tangled rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether coordination and extraction components are structurally separable.').

omega_variable(
    kernel_reading_foreclosure_relations,
    'Which sibling readings does this reading logically foreclose vs coexist with vs influence?',
    'Structural analysis of each reading''s core premises: communal autonomy''s core premise (community consent gate) vs secularist (legislative supremacy) vs gender_rights (constitutional equality as override) vs federalist_millet (pluralism as anti-tyranny) vs judicial_harmonization (case-by-case floor).',
    'Determines reading_relations in cs_structure. Foreclosure would mean this reading''s framework cannot accommodate the sibling''s premise. Coexistence means different parties hold both. Influence means structural pressure without logical elimination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_relations, conceptual, 'Structural relationships between this reading and its siblings in the marriage_authority kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__communal_autonomy_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__communal_autonomy_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(marr_tr_t1973, marriage_authority__communal_autonomy_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority__communal_autonomy_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority__communal_autonomy_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(marr_tr_t2005, marriage_authority__communal_autonomy_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement(marr_tr_t2015, marriage_authority__communal_autonomy_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority__communal_autonomy_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__communal_autonomy_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(marr_be_t1973, marriage_authority__communal_autonomy_reading, base_extractiveness, 1973, 0.42).
narrative_ontology:measurement(marr_be_t1985, marriage_authority__communal_autonomy_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(marr_be_t1995, marriage_authority__communal_autonomy_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(marr_be_t2005, marriage_authority__communal_autonomy_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(marr_be_t2015, marriage_authority__communal_autonomy_reading, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement(marr_be_t2024, marriage_authority__communal_autonomy_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__communal_autonomy_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(marr_su_t1973, marriage_authority__communal_autonomy_reading, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement(marr_su_t1985, marriage_authority__communal_autonomy_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(marr_su_t1995, marriage_authority__communal_autonomy_reading, suppression_requirement, 1995, 0.62).
narrative_ontology:measurement(marr_su_t2005, marriage_authority__communal_autonomy_reading, suppression_requirement, 2005, 0.64).
narrative_ontology:measurement(marr_su_t2015, marriage_authority__communal_autonomy_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(marr_su_t2024, marriage_authority__communal_autonomy_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__communal_autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority__communal_autonomy_reading, 0.08).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__communal_autonomy_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% This reading and its four siblings decompose the 'marriage authority' label into structurally distinct constraints with different ε, beneficiaries, victims, and coordination types. The communal_autonomy_reading has ε_mod rope with religious_leadership beneficiary and intra-community dissenters as victim. The secularist_reading would have ε near 0 for UCC but high transition cost. The gender_rights_reading has judicial override as coordination with gender_rights_advocates as beneficiary. The federalist_millet_reading has consociational coordination with all communities as beneficiaries. The judicial_harmonization_reading has case-by-case coordination with constitutional_courts as agenda_setter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, institutional, 0.3).
constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, powerless, 0.95).
constraint_indexing:directionality_override(marriage_authority__communal_autonomy_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
