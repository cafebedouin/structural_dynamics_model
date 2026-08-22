% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__parsi_communal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__parsi_communal_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_authority_kernel__parsi_communal_reading
 *   human_readable: Parsi Communal Marriage Authority (Parsi Marriage and Divorce Act 1936)
 *   domain: comparative_law/religious_governance/constitutional_pluralism
 *
 * SUMMARY:
 *   The Parsi Marriage and Divorce Act 1936 codifies Parsi community custom
 *   into statute, granting the community its own matrimonial courts staffed
 *   partly by community delegates and vesting authority over marriage
 *   validity, divorce, and (through linked custom rather than the Act itself)
 *   fire-temple and trust access in community institutions. This reading
 *   treats the arrangement as it operates for the community under contest: a
 *   genuine coordination mechanism for a small, geographically dispersed
 *   minority to maintain a coherent family-law and ritual-status system,
 *   layered with an asymmetric extraction mechanism — a patrilineal
 *   membership rule that treats women who marry out, and their children,
 *   differently from men who do so. The coordination function (specialized
 *   tribunals suited to community custom) and the extraction function
 *   (gendered exclusion enforced through the same institutions) are both
 *   real, which is why this reading claims tangled_rope rather than pure rope
 *   or pure snare.
 *
 * KEY AGENTS:
 *   - parsi_community_institutions: administers the customary framework and the matrimonial court delegate system
 *   - parsi_matrimonial_court_delegates: benefit from continuation of a separate tribunal system
 *   - endogamous_lineage_families: retain full ritual and trust access by marrying within the community
 *   - parsi_women_marrying_outside_community: bear loss of ritual standing under the patrilineal rule
 *   - children_of_intermarried_parsi_women: bear exclusion from membership regardless of personal choice
 *   - indian_constitutional_courts: adjudicate equal-protection challenges while generally deferring to personal-law pluralism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__parsi_communal_reading, 0.42).
domain_priors:suppression_score(marriage_authority_kernel__parsi_communal_reading, 0.55).
domain_priors:theater_ratio(marriage_authority_kernel__parsi_communal_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Communal Marriage Authority (Parsi Marriage and Divorce Act 1936)").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/religious_governance/constitutional_pluralism").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '490f761a-d39d-4d78-9188-e9a8b151e595').
narrative_ontology:cs_kernel_codification('490f761a-d39d-4d78-9188-e9a8b151e595', formalized).
narrative_ontology:cs_authority_grounding('490f761a-d39d-4d78-9188-e9a8b151e595', lineage).
narrative_ontology:cs_interpretation_layer_present('490f761a-d39d-4d78-9188-e9a8b151e595').
narrative_ontology:cs_reading_relation('490f761a-d39d-4d78-9188-e9a8b151e595', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('490f761a-d39d-4d78-9188-e9a8b151e595', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('490f761a-d39d-4d78-9188-e9a8b151e595', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('490f761a-d39d-4d78-9188-e9a8b151e595', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('490f761a-d39d-4d78-9188-e9a8b151e595', foundational, descent_based_membership_defines_community_boundary).
narrative_ontology:cs_axiom_status(descent_based_membership_defines_community_boundary, holdable).
narrative_ontology:cs_axiom_grounding('490f761a-d39d-4d78-9188-e9a8b151e595', descent_based_membership_defines_community_boundary, conventional).
narrative_ontology:cs_axiom('490f761a-d39d-4d78-9188-e9a8b151e595', foundational, community_self_administered_tribunals_required_for_customary_fidelity).
narrative_ontology:cs_axiom_status(community_self_administered_tribunals_required_for_customary_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('490f761a-d39d-4d78-9188-e9a8b151e595', community_self_administered_tribunals_required_for_customary_fidelity, instrumental).
narrative_ontology:cs_axiom('490f761a-d39d-4d78-9188-e9a8b151e595', secondary, patrilineal_descent_governs_ritual_eligibility).
narrative_ontology:cs_axiom_status(patrilineal_descent_governs_ritual_eligibility, holdable).
narrative_ontology:cs_axiom_grounding('490f761a-d39d-4d78-9188-e9a8b151e595', patrilineal_descent_governs_ritual_eligibility, conventional).
narrative_ontology:cs_reference_frame('490f761a-d39d-4d78-9188-e9a8b151e595', zoroastrian_priestly_customary_authority).
narrative_ontology:cs_drift_state('490f761a-d39d-4d78-9188-e9a8b151e595', contemporary_demographic_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('490f761a-d39d-4d78-9188-e9a8b151e595', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_court_delegates).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, endogamous_lineage_families).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_outside_community).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, children_of_intermarried_parsi_women).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, intermarried_men_seeking_community_recognition).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, personal_law_pluralism_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trustees, panchayats, and the special Parsi Matrimonial Courts administer marriage, divorce, and community membership questions under the 1936 Act. They set who counts as a valid Parsi marriage, adjudicate disputes through community-appointed delegates rather than ordinary civil judges, and control access to fire temples and community trusts that flow from recognized marital status. Their institutional survival is bound to maintaining a shrinking, closed membership base.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_institutions, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Serve as the lay delegates who sit alongside a judge on matrimonial court panels unique to Parsis. Their role, status, and small honoraria exist only because the community-specific tribunal exists; they have a direct stake in the continuation of separate community adjudication rather than merger into ordinary family courts.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_court_delegates, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_court_delegates, agenda_setter).

% Families whose children marry within the community retain full access to fire temple rites, communal charitable trusts, and inheritance certainty under the community's own rules. They benefit from the clarity and continuity the customary framework provides and from the demographic premium placed on in-group marriage (matrimonial subsidies, housing trust preference).
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, endogamous_lineage_families, beneficiary,
    moderate, generational, mobile, national).

% A Parsi woman who marries a non-Parsi man is treated by community custom (upheld in practice though contested in courts) as having exited the faith for ritual and trust-access purposes, even though a Parsi man marrying outside retains full standing. She loses access to fire temple entry and Tower of Silence rites and often to communal trust benefits for herself and her children. Her only routes out are litigation in civil courts against the community's own institutions, or acceptance of diminished status.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_outside_community, payer,
    powerless, biographical, trapped, national).

% Children born to a Parsi mother and non-Parsi father are frequently denied recognition as Parsi under the community's patrilineal custom, even where children of a Parsi father and non-Parsi mother are accepted. They cannot alter their parentage and have no standing within the community structures that decide the rule.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, children_of_intermarried_parsi_women, payer,
    powerless, biographical, trapped, national).

% Non-Parsi men who marry Parsi women and seek conversion or recognition for religious participation are generally refused, since the community does not accept converts. They bear the cost of exclusion from their spouse's religious and social life without any pathway the community itself will open.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, intermarried_men_seeking_community_recognition, payer,
    powerless, biographical, constrained, national).

% Hear equal-protection and gender-discrimination challenges to the community's patrilineal membership rule and the special matrimonial court structure. Their rulings can compel reform of the endogamy rule but have historically deferred to the community's right to define its own religious and cultural membership under personal-law pluralism.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_constitutional_courts, observer,
    institutional, generational, analytical, national).

% The community as a whole faces population collapse driven partly by low marriage rates and the exclusion of intermarried women and their children from full membership. Demographers and diaspora reformers who would argue for opening membership to arrest decline are not represented in the bodies that set marriage and membership rules.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, demographically_declining_parsi_population, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, diffuse).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, internally administered forum for Parsi marriage, divorce, and inheritance-adjacent status questions, allowing a small, geographically dispersed community to maintain a coherent body of customary family law and specialized tribunals (the Parsi Matrimonial Courts) rather than being absorbed wholesale into general civil family law.
% TRANSFER_FUNCTION: Moves ritual standing, trust and charitable-fund access, and marital recognition toward those who marry within the community and along patrilineal descent, and away from women who marry out and their children, regardless of the individual's personal conduct or belief.
% ABSENT_VOICES: Parsi women who married outside the community, their children, and diaspora reform advocates who favor opening membership on gender-neutral terms are not seated on the panchayats or matrimonial court delegate panels that make and re-affirm the endogamy and patrilineal rules; they can only reach the question via external civil litigation the community resists.
% DISAPPEARANCE_RATIONALE: If the 1936 Act's community-specific authority vanished and Parsi marriage/divorce fell under the ordinary civil framework, community trusts would need new eligibility rules, the delegate-panel tribunal system would cease to exist, and excluded women, their children, and intermarried spouses would gain equal access to fire temple and trust benefits overnight — a materially different distribution of ritual and economic standing.
% FOUNDING_PROBLEM: In the 19th century, Parsis in Bombay sought a marriage and divorce framework suited to their own customs and priestly authority rather than being governed by British ecclesiastical courts designed for Christian marriage, and wanted formal legal recognition of their community's distinct family-law norms.
% FOUNDING_PROBLEM_CORROBORATION: Community trustees and delegate panels attest the founding problem remains live — that only community-administered tribunals can correctly apply Zoroastrian ritual and lineage concepts. Independent legal scholars, Indian constitutional courts in obiter commentary on personal-law reform, and diaspora demographers attest from outside the benefiting institutions that the original problem of jurisdictional recognition was resolved decades ago and that the patrilineal exclusion rule now functions primarily to police community boundaries amid demographic anxiety, not to serve any live doctrinal necessity.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_authority_kernel__parsi_communal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__parsi_communal_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__parsi_communal_reading_tests).
:- end_tests(marriage_authority_kernel__parsi_communal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate rather than severe: most Parsis marrying within the community experience the arrangement as pure coordination with no extraction at all, so the story's ε reflects the asymmetric burden concentrated on the smaller group of intermarried women and their children, not a uniform community-wide extraction. Suppression (0.55) reflects that the rule is enforced through control of ritual spaces (fire temples, Towers of Silence) and community trusts rather than through state coercion — exit from the rule requires either community reform (slow, contested internally) or civil litigation against community bodies that community members are reluctant to bring against their own institutions. Theater ratio rises modestly over the interval (0.10 to 0.28) as the original 19th-century rationale (need for community-specific ecclesiastical-style courts distinct from British Christian courts) has become progressively less load-bearing relative to the rule's present function of policing community boundaries amid demographic decline.
 *
 * DIRECTIONALITY LOGIC:
 *   Community institutions and matrimonial court delegates sit at the beneficiary end: they administer the system and derive institutional standing and modest material benefit from its continuation. Endogamous lineage families are moderate beneficiaries — they gain ritual and trust access but do not administer the rule. Women who marry outside the community, their children, and intermarried men seeking recognition sit at the target end: they bear a cost (loss of ritual standing, trust access, or recognition) determined by a rule they had no role in setting and that treats their situation asymmetrically relative to men in an equivalent position. This gendered asymmetry — not merely 'insider vs outsider' status — is what pushes several payer seats toward trapped exit options despite formal legal personhood and civil-court access, because civil litigation against one's own religious community carries social costs the formal option does not capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (need for a Parsi-specific marriage and divorce framework distinct from British ecclesiastical courts) was substantially resolved by the mid-20th century once the Act secured formal recognition and the matrimonial court structure stabilized. What persists past that resolution is the patrilineal membership rule, which does no work toward the original coordination problem (recognition of Parsi customary marriage law) and instead performs boundary-maintenance against demographic dilution. Classifying this as tangled_rope rather than snare preserves the genuine coordination value of community-specific tribunals for the majority of Parsi marriages while flagging the asymmetric extraction visited on a minority within the community — collapsing the two into a single verdict in either direction would either excuse the gendered exclusion as coordination cost, or delegitimize the entire matrimonial court system that most community members experience as unproblematic self-governance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    communal_reading_vs_sibling_readings_location,
    'Where exactly does the parsi_communal_reading''s structural profile diverge from the other four readings of the marriage_authority_kernel, and is the divergence located in the tribunal structure, the membership rule, or the demographic stakes?',
    'Comparative structural analysis across the five sibling constraint stories, examining whether the tribunal-delegate structure (present here, absent in secular_civil_reading), the patrilineal membership rule (present here and in muslim_shariat_reading''s community-board structure but absent in secular_civil_reading), or demographic-decline pressure (unique to this reading among the five) drives the classification difference.',
    'If the divergence is located primarily in the membership rule rather than the tribunal structure, reform proposals that preserve the tribunals but remove the patrilineal exclusion would resolve most of the tangled_rope extraction while leaving the coordination function intact — informing which sibling reading (if any) the community should move toward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communal_reading_vs_sibling_readings_location, conceptual, 'Locating the structural delta between this reading and its four siblings on the shared kernel.').

omega_variable(
    endogamy_rule_natural_or_constructed,
    'Is the patrilineal membership rule an authentic feature of ancient Zoroastrian custom, or a colonial-era or 20th-century institutional construction adopted to manage demographic anxiety?',
    'Historical and textual analysis of pre-1936 Zoroastrian practice across Persian and Indian Parsi communities, comparing documented historical treatment of intermarriage against the codified 20th-century rule.',
    'If the rule is a modern construction rather than ancient custom, the community institutions'' claim that the rule is required by tradition weakens substantially, supporting reclassification of the membership-exclusion component as closer to pure extraction (snare-like) rather than an inherited coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(endogamy_rule_natural_or_constructed, empirical, 'Whether the patrilineal exclusion rule is genuine ancient custom or a more recent institutional construction.').

omega_variable(
    gender_asymmetry_persistence_mechanism,
    'Does the asymmetric treatment of women who marry out (versus men who marry out) persist because of genuine theological distinction, or because the bodies that interpret custom have historically been male-dominated and never revisited the rule from the excluded women''s standpoint?',
    'Track the composition of panchayats and matrimonial court delegate panels over time and correlate with periods when the rule was reaffirmed versus challenged; interview excluded women and their descendants about whether an equivalent all-female interpretive body would plausibly have reached the same rule.',
    'If the asymmetry tracks panel composition rather than doctrine, the tangled_rope classification''s victim declaration is strongly corroborated as extraction riding on an unexamined structural default rather than considered theology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_persistence_mechanism, empirical, 'Whether the gendered membership asymmetry reflects theology or unexamined institutional composition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.1).
narrative_ontology:measurement(marr_tr_t1955, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1955, 0.13).
narrative_ontology:measurement(marr_tr_t1975, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1975, 0.18).
narrative_ontology:measurement(marr_tr_t1995, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.3).
narrative_ontology:measurement(marr_be_t1955, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1955, 0.33).
narrative_ontology:measurement(marr_be_t1975, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1975, 0.37).
narrative_ontology:measurement(marr_be_t1995, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(marr_be_t2010, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2010, 0.41).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(marr_su_t1955, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1955, 0.5).
narrative_ontology:measurement(marr_su_t1975, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1975, 0.52).
narrative_ontology:measurement(marr_su_t1995, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1995, 0.53).
narrative_ontology:measurement(marr_su_t2010, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__parsi_communal_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five readings of the shared marriage_authority_kernel, each authored as an independent ε-invariant constraint with its own stakeholders and classification per the ε-invariance principle. This reading's distinguishing structural features are the community-specific matrimonial court delegate system, the patrilineal membership rule creating gender asymmetry in ritual and trust access, and demographic decline that makes the community's continued viability an explicit stake in how the membership rule is resolved. The secular_civil_reading is the natural terminus reformers point to as an alternative available exit path (hence 'influences' rather than 'coexists_with' — the availability of secular civil marriage under the Special Marriage Act changes the leverage of excluded members without foreclosing the communal reading itself, since many members continue to prefer community-administered marriage even while a civil alternative exists).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
