% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_law_authority__christian_canonical_reading, []).

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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Marriage as Sacrament / Denominational Governance under Christian Ecclesiastical Authority
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This story authors the Christian canonical reading of the
 *   family_law_authority kernel: marriage governed as a sacrament under
 *   Catholic ecclesiastical hierarchy, or as a denominationally-varying rite
 *   under Protestant church governance. The reading's distinguishing
 *   structural feature is the coexistence of two regimes under one broad
 *   tradition — Catholic sacramental indissolubility adjudicated through
 *   tribunals, versus Protestant denominational variance permitting divorce
 *   and remarriage to differing degrees — both grounded in a shared claim
 *   that ecclesiastical/denominational authority, not the couple or the state
 *   alone, determines the religious validity of the marriage. This is not a
 *   story about civil marriage generally; the civil regime is present only as
 *   the parallel, increasingly load-bearing system that has absorbed much of
 *   the founding problem's practical substance. Sibling readings of the same
 *   kernel (Hindu dharmashastra, Muslim shariat, Parsi Zoroastrian, secular
 *   contractual) are authored as separate constraint stories, each with its
 *   own epsilon; this story's epsilon (0.58) is authored for the Christian
 *   canonical arrangement alone and must not be read as representative of the
 *   kernel as a whole.
 *
 * KEY AGENTS:
 *   - catholic_hierarchy: primary agenda-setter (institutional/arbitrage) — administers tribunals, sets doctrine, denies sacramental access to non-conforming unions
 *   - protestant_denominational_bodies: parallel agenda-setter (institutional/arbitrage) — set denomination-specific rules, compete for adherents partly on marriage governance
 *   - remarried_catholics and lgbtq_couples: primary targets (powerless/trapped) — bear exclusion or sanction under doctrine they have no voice in setting
 *   - civil_courts: analytical observer (institutional/analytical) — operate the parallel secular regime that increasingly does the practical work the ecclesiastical regime once did alone
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.58).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.62).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Marriage as Sacrament / Denominational Governance under Christian Ecclesiastical Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, '6484ba78-44d7-4e70-9c08-ed012d561ef3').
narrative_ontology:cs_kernel_codification('6484ba78-44d7-4e70-9c08-ed012d561ef3', formalized).
narrative_ontology:cs_authority_grounding('6484ba78-44d7-4e70-9c08-ed012d561ef3', lineage).
narrative_ontology:cs_interpretation_layer_present('6484ba78-44d7-4e70-9c08-ed012d561ef3').
narrative_ontology:cs_reading_relation('6484ba78-44d7-4e70-9c08-ed012d561ef3', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('6484ba78-44d7-4e70-9c08-ed012d561ef3', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6484ba78-44d7-4e70-9c08-ed012d561ef3', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('6484ba78-44d7-4e70-9c08-ed012d561ef3', family_law_authority__secular_contractual_reading, influences).
narrative_ontology:cs_axiom('6484ba78-44d7-4e70-9c08-ed012d561ef3', foundational, marriage_as_indissoluble_sacrament).
narrative_ontology:cs_axiom_status(marriage_as_indissoluble_sacrament, holdable).
narrative_ontology:cs_axiom_grounding('6484ba78-44d7-4e70-9c08-ed012d561ef3', marriage_as_indissoluble_sacrament, theological).
narrative_ontology:cs_axiom('6484ba78-44d7-4e70-9c08-ed012d561ef3', foundational, ecclesiastical_authority_over_marital_validity).
narrative_ontology:cs_axiom_status(ecclesiastical_authority_over_marital_validity, holdable).
narrative_ontology:cs_axiom_grounding('6484ba78-44d7-4e70-9c08-ed012d561ef3', ecclesiastical_authority_over_marital_validity, conventional).
narrative_ontology:cs_axiom('6484ba78-44d7-4e70-9c08-ed012d561ef3', secondary, denominational_discretion_over_divorce).
narrative_ontology:cs_axiom_status(denominational_discretion_over_divorce, holdable).
narrative_ontology:cs_axiom_grounding('6484ba78-44d7-4e70-9c08-ed012d561ef3', denominational_discretion_over_divorce, conventional).
narrative_ontology:cs_reference_frame('6484ba78-44d7-4e70-9c08-ed012d561ef3', tridentine_sacramental_marriage_doctrine).
narrative_ontology:cs_drift_state('6484ba78-44d7-4e70-9c08-ed012d561ef3', contemporary_pluralist_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6484ba78-44d7-4e70-9c08-ed012d561ef3', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, catholic_hierarchy).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, protestant_denominational_bodies).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, spouses_seeking_recognized_status).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, children_of_marriage).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, spouses_seeking_annulment_or_divorce).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, remarried_catholics).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, lgbtq_couples).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, interfaith_couples).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines marriage as an indissoluble sacrament, adjudicates validity through tribunals (annulment process), and denies remarried-without-annulment Catholics access to communion. Sets doctrine, controls canon law courts, and derives institutional authority and continued relevance from being the exclusive arbiter of what counts as a valid marriage in the eyes of the Church.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_hierarchy, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Set their own, denomination-specific rules on marriage validity and divorce, ranging from near-Catholic strictness to full acceptance of remarriage after divorce. Compete with each other and with the Catholic Church for adherents partly on the basis of how they govern marriage and family life; their governance authority is sustained by member allegiance rather than a unified hierarchy.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_denominational_bodies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, protestant_denominational_bodies, beneficiary).

% Married couples whose union is validated within their faith community gain social standing, sacramental access (communion, other rites), family and community integration, and often continuity with extended family expectations. They benefit from the coordination the arrangement provides — a shared, legible standard for what marriage is within the community.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, spouses_seeking_recognized_status, beneficiary,
    moderate, biographical, constrained, national).

% Benefit in the arrangement's own terms from stability, legitimacy, and inheritance/status clarity that flows from a recognized marital union; have no voice in how the governing rules are set and bear the consequences (stigma, contested legitimacy, custody complications) when a marriage's validity is disputed.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, children_of_marriage, beneficiary,
    powerless, biographical, trapped, national).

% In Catholic contexts, must petition an ecclesiastical tribunal to have a marriage declared null (not dissolved) in order to remarry within the Church; process is lengthy, costly, and outcome-uncertain even where the civil marriage has already ended. In stricter Protestant bodies, face social and religious sanction for divorce even where doctrine formally permits it. Exit from the civil marriage does not free them from ecclesiastical consequences.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, spouses_seeking_annulment_or_divorce, payer,
    moderate, biographical, constrained, national).

% Having remarried civilly without an annulment, are barred from receiving communion and treated as in an irregular canonical state, despite full participation in parish and community life otherwise. Their only path back to full standing runs through the same tribunal system that produced the original impasse.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, remarried_catholics, payer,
    powerless, biographical, trapped, national).

% Categorically excluded from sacramental marriage in Catholic doctrine and in many Protestant denominations; even where civil marriage is available to them, the ecclesiastical governance structure denies them the sacramental/denominational status that confers full religious-community standing. They have no seat in the doctrinal process that excludes them.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, lgbtq_couples, excluded,
    powerless, biographical, trapped, national).

% Face dispensation requirements, mixed-rite complications, or outright non-recognition depending on which partner's tradition and which denomination is asked to validate the union; often must negotiate between two governing authorities with no shared adjudicating body, absorbing the coordination cost personally.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, interfaith_couples, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(family_law_authority__christian_canonical_reading, interfaith_couples, excluded).

% Operate a parallel, secular marriage/divorce regime; increasingly the substantive legal consequences of marriage and divorce (property, custody, support) are settled civilly regardless of ecclesiastical status, which narrows but does not eliminate the practical stakes of the Church's own validity determinations for believers who value sacramental standing.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, civil_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, legible standard within a faith community for what counts as a valid, recognized marriage — coordinating expectations around fidelity, inheritance, family formation, religious rite access, and communal standing without requiring each couple or family to negotiate these terms from scratch.
% TRANSFER_FUNCTION: Moves authority over the legitimacy of intimate and family life from the individuals in the relationship to the ecclesiastical or denominational body; moves social and sacramental standing away from those whose unions fall outside the doctrinal categories (divorced-without-annulment, same-sex, certain interfaith unions) toward those whose unions are validated, and channels tribunal fees, institutional loyalty, and continued relevance to the adjudicating body.
% ABSENT_VOICES: LGBTQ couples and, in the Catholic case, spouses seeking annulment have no vote in the doctrinal or tribunal process that governs them; interfaith couples negotiate between two authorities neither of which was built with their situation as a design input.
% DISAPPEARANCE_RATIONALE: If ecclesiastical authority over marriage validity vanished overnight, civil marriage and divorce law would become the sole operative framework for the couples currently governed by canon law or denominational rules; annulment tribunals would lose their function, communion-based sanctions on remarried Catholics would lose their basis, and religious communities would need new (or no) mechanisms to mark marital status for participation in sacramental life.
% FOUNDING_PROBLEM: Early and medieval Christian communities needed a stable, communally legible way to distinguish legitimate unions from concubinage or informal cohabitation, to secure inheritance and paternity, to bind marriage to covenantal/theological meaning, and to give the Church (rather than feudal lords or purely civil authorities) jurisdiction over an institution central to salvation and communal order.
% FOUNDING_PROBLEM_CORROBORATION: The Catholic hierarchy and many Protestant bodies attest the founding problem is still live: marriage's sacramental/covenantal meaning requires continued doctrinal stewardship. Historians of canon law and sociologists of religion, along with civil-law scholars, attest that the practical problems (paternity certainty, property, communal legitimacy) are now substantially handled by civil registries and courts, and that ecclesiastical jurisdiction increasingly persists for reasons of institutional authority and doctrinal identity rather than unmet coordination need — this reading treats the corroboration as genuinely contested rather than resolved either way.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_law_authority__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(family_law_authority__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects a hybrid: genuine coordination value (shared legibility of marital status within a faith community, real for the majority of couples whose unions are validated without friction) alongside real, asymmetric cost borne by remarried-without-annulment Catholics, LGBTQ couples, and contested interfaith unions, who are denied standing or forced through costly adjudication for reasons unrelated to any harm they cause. Suppression (0.62, declining slightly over the interval) captures the declining but still real social and sacramental sanction attached to noncompliance — declining because civil courts have absorbed most of the practical consequences (property, custody) that once depended entirely on ecclesiastical determination, which is why suppression_requirement trends downward across the grid even as base_extractiveness ticks up (doctrinal firmness intensifying rhetorically even as practical enforcement capacity erodes). Theater ratio (0.3, rising) reflects that an increasing share of the tribunal and doctrinal apparatus operates as performative reaffirmation of authority rather than functionally necessary adjudication, as the civil system has taken over the coordination problem's substantive stakes. Accessibility collapse (0.5) and resistance (0.55) sit mid-range because alternatives (civil marriage, denomination-switching, leaving the faith) exist and are increasingly exercised, but real communal, familial, and salvific stakes still constrain many from treating exit as costless.
 *
 * DIRECTIONALITY LOGIC:
 *   Catholic hierarchy and Protestant denominational bodies are structural agenda-setters and incidental beneficiaries: they administer the arrangement, and their institutional authority and continued relevance are sustained by remaining the exclusive interpreters of marital validity — d sits near the beneficiary end for them. Spouses whose marriages are validated without friction, and their children, are genuine beneficiaries of the coordination function — real subsidy, not merely nominal. Spouses seeking annulment/divorce, remarried Catholics, LGBTQ couples, and interfaith couples are targets: they bear the cost of a standard built without their situation as an input, with constrained or trapped exit options, pushing d toward the target end. Civil courts are analytical/institutional observers whose growing practical jurisdiction is what has been eroding (not increasing) the ecclesiastical regime's suppression capacity over the interval.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than pure snare) preserves the fact that for the majority of couples the arrangement performs real, wanted coordination — legible marital status, communal integration, sacramental participation — while the classification's victim declarations and enforcement requirement prevent that real coordination function from laundering the asymmetric cost borne by remarried Catholics, LGBTQ couples, and contested annulment petitioners. Calling this a mountain (natural, inevitable) would hide the constructed, contestable nature of the doctrinal boundaries; calling it a pure snare would erase the genuine subsidy most participating couples experience. The founding_problem interview registers this tension directly: the problem (legible legitimate unions, inheritance clarity, communal order) is contested as live versus dead precisely because civil registries now do much of that work, leaving doctrinal stewardship of sacramental meaning as the residual, less externally corroborated justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacramental_versus_constructed_authority,
    'Is ecclesiastical jurisdiction over marriage validity a theologically necessary feature of the sacrament itself, or a historically contingent institutional arrangement that could be reformed or dissolved without loss to the sacrament''s religious meaning?',
    'Comparative theological and historical analysis of how marriage was governed in the early Church prior to formal canon-law jurisdiction, and comparison with Protestant traditions that retain sacramental or covenantal meaning while distributing validity authority differently (or not at all) among denominational bodies.',
    'If jurisdiction is theologically necessary, the enforcement and adjudication apparatus is closer to an inherent feature of the coordination function; if contingent, the apparatus is more clearly a constructed extraction layer riding on top of a coordination function that could be preserved under lighter governance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_versus_constructed_authority, conceptual, 'Whether ecclesiastical jurisdiction over marital validity is theologically inherent or a contingent institutional construction.').

omega_variable(
    civil_absorption_of_founding_problem,
    'Has the founding problem (legitimacy, inheritance, paternity certainty, communal order) been substantially absorbed by secular civil registries and courts, leaving ecclesiastical jurisdiction to serve primarily an identity/authority-maintenance function rather than a load-bearing coordination function?',
    'Comparative analysis of the practical legal consequences that now depend on civil registration/divorce versus those that still depend on ecclesiastical determination, across jurisdictions with strong versus weak church-state separation.',
    'If substantially absorbed, the case for classifying the residual ecclesiastical apparatus as increasingly theatrical (rising theater_ratio) strengthens and the founding_problem_status of ''dead, persisting by inertia'' becomes more defensible than ''contested.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_absorption_of_founding_problem, empirical, 'Whether civil legal systems have absorbed the substantive stakes the ecclesiastical marriage regime was originally built to manage.').

omega_variable(
    denominational_reading_boundary,
    'Should Catholic sacramental-indissolubility governance and Protestant denominational-variance governance be treated as one reading (as authored here) or decomposed into two separate readings, given that Catholic no-divorce and Protestant divorce-permitted produce materially different victim sets (annulment petitioners versus none, in the strictest Protestant sense)?',
    'Test whether epsilon and victim declarations remain stable across the Catholic and Protestant sub-cases; if they diverge substantially (as the annulment-tribunal victim class suggests they might), the ε-invariance principle requires decomposition into catholic_sacramental_reading and protestant_denominational_reading as separate stories.',
    'If decomposed, this story''s epsilon would likely bifurcate: a higher-epsilon Catholic sacramental sub-reading (tribunal costs, no-divorce rigidity) and a lower-epsilon Protestant denominational sub-reading (more accommodation, more internal variance). Kept combined, this story''s 0.58 is a blended estimate across both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denominational_reading_boundary, conceptual, 'Whether the Catholic and Protestant sub-traditions within the christian_canonical_reading should themselves be decomposed per the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fami_tr_t10, family_law_authority__christian_canonical_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(fami_tr_t30, family_law_authority__christian_canonical_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(fami_tr_t50, family_law_authority__christian_canonical_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__christian_canonical_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fami_be_t10, family_law_authority__christian_canonical_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(fami_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(fami_be_t30, family_law_authority__christian_canonical_reading, base_extractiveness, 30, 0.53).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(fami_be_t50, family_law_authority__christian_canonical_reading, base_extractiveness, 50, 0.57).
narrative_ontology:measurement(fami_be_t60, family_law_authority__christian_canonical_reading, base_extractiveness, 60, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(fami_su_t10, family_law_authority__christian_canonical_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(fami_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(fami_su_t30, family_law_authority__christian_canonical_reading, suppression_requirement, 30, 0.66).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.64).
narrative_ontology:measurement(fami_su_t50, family_law_authority__christian_canonical_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement(fami_su_t60, family_law_authority__christian_canonical_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(family_law_authority__christian_canonical_reading, 0.1).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the family_law_authority kernel, each authored as a separate constraint story with its own epsilon, beneficiary/victim structure, and classification: christian_canonical_reading (this file), hindu_dharmashastra_reading, muslim_shariat_reading, parsi_zoroastrian_reading, and secular_contractual_reading. The readings are not decompositions of a single measurement disagreement but genuinely distinct governance structures over the same underlying social institution (marriage); each community's own framework determines validity, exit conditions, and victim classes differently. Network edges here are declared for cross-reading contamination and influence analysis (e.g., civil-law pressure on ecclesiastical jurisdiction, interfaith friction where two readings meet at the same couple), not because the readings share one epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
