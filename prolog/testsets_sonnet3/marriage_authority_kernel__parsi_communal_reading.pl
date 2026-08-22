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
 *   human_readable: Parsi Marriage and Divorce Act 1936 — Communal Custom as Family Law Authority
 *   domain: comparative_law/religious_governance
 *
 * SUMMARY:
 *   This constraint instantiates the Parsi-communal reading of the
 *   marriage_authority_kernel: the claim that marriage and family law
 *   authority for Parsis derives from Zoroastrian community custom as
 *   codified in the Parsi Marriage and Divorce Act 1936, administered by
 *   community panchayats and dedicated matrimonial courts. Within the
 *   community, the arrangement functions largely as genuine coordination — a
 *   small, demographically fragile minority preserving a doctrinally
 *   appropriate, culturally attuned adjudicative forum outside general civil
 *   courts. But the same custom-derived authority enforces a
 *   gender-asymmetric endogamy rule: a Parsi woman marrying outside the
 *   community risks loss of ritual and social standing, and her children are
 *   typically denied recognition as Parsi, while a Parsi man's intermarriage
 *   and mixed-parentage children face no equivalent exclusion. This asymmetry
 *   is not required by Zoroastrian doctrine itself (a live internal reform
 *   dispute) but is enforced through the same custom-derived authority
 *   structure that the coordination function relies on, which is why this
 *   reading classifies as tangled_rope rather than a clean rope: coordination
 *   and extraction run through the identical mechanism.
 *
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
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_authority_kernel__parsi_communal_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__parsi_communal_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__parsi_communal_reading, "Parsi Marriage and Divorce Act 1936 — Communal Custom as Family Law Authority").
narrative_ontology:topic_domain(marriage_authority_kernel__parsi_communal_reading, "comparative_law/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__parsi_communal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__parsi_communal_reading, '7787bf93-6e45-420d-8375-a58dce5a642f').
narrative_ontology:cs_kernel_codification('7787bf93-6e45-420d-8375-a58dce5a642f', formalized).
narrative_ontology:cs_authority_grounding('7787bf93-6e45-420d-8375-a58dce5a642f', lineage).
narrative_ontology:cs_interpretation_layer_present('7787bf93-6e45-420d-8375-a58dce5a642f').
narrative_ontology:cs_reading_relation('7787bf93-6e45-420d-8375-a58dce5a642f', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('7787bf93-6e45-420d-8375-a58dce5a642f', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('7787bf93-6e45-420d-8375-a58dce5a642f', marriage_authority_kernel__christian_canonical_reading, coexists_with).
narrative_ontology:cs_reading_relation('7787bf93-6e45-420d-8375-a58dce5a642f', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('7787bf93-6e45-420d-8375-a58dce5a642f', foundational, communal_endogamy_as_survival_imperative).
narrative_ontology:cs_axiom_status(communal_endogamy_as_survival_imperative, holdable).
narrative_ontology:cs_axiom_grounding('7787bf93-6e45-420d-8375-a58dce5a642f', communal_endogamy_as_survival_imperative, empirically_contingent).
narrative_ontology:cs_axiom('7787bf93-6e45-420d-8375-a58dce5a642f', secondary, patrilineal_descent_determines_communal_membership).
narrative_ontology:cs_axiom_status(patrilineal_descent_determines_communal_membership, overridden).
narrative_ontology:cs_axiom_grounding('7787bf93-6e45-420d-8375-a58dce5a642f', patrilineal_descent_determines_communal_membership, conventional).
narrative_ontology:cs_reference_frame('7787bf93-6e45-420d-8375-a58dce5a642f', zoroastrian_communal_custom_1936_codification).
narrative_ontology:cs_drift_state('7787bf93-6e45-420d-8375-a58dce5a642f', contemporary_demographic_crisis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7787bf93-6e45-420d-8375-a58dce5a642f', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__parsi_communal_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_community_institutions).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, endogamously_married_parsi_men).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_court_officers).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_outside_community).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, children_of_mixed_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__parsi_communal_reading, intermarried_parsi_men_partners).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, communal_self_governance_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__parsi_communal_reading, parsi_ethnic_continuity_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Panchayats and the community-elected Parsi Matrimonial Courts administer the 1936 Act, adjudicating marriage validity, divorce, and inheritance-adjacent status exclusively for Parsis. They set eligibility criteria for who counts as Parsi for legal purposes and enforce endogamy norms through excommunication-adjacent social and religious consequences (denial of Tower of Silence rites, fire temple access) that operate alongside, not through, the statute itself.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_community_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Access a dedicated, community-run adjudication system with judges drawn from their own community, procedural norms attuned to Zoroastrian custom, and continued full religious and social standing. Their marriages are never questioned for community membership; their children are automatically recognized as Parsi regardless of the mother's origin under prevailing patrilineal practice.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, endogamously_married_parsi_men, beneficiary,
    moderate, biographical, mobile, national).

% Serve as delegates and judges within the special matrimonial courts constituted under the Act in cities with historically large Parsi populations. Their institutional relevance and communal standing depend on the continued existence of a separate personal-law track; they administer endogamy-adjacent exclusions as an extension of their adjudicatory role.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_court_officers, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__parsi_communal_reading, parsi_matrimonial_court_officers, agenda_setter).

% Historically and in prevailing community practice, a Parsi woman who marries a non-Parsi risks being treated as having exited the community for religious and ritual purposes, while a Parsi man doing the same does not lose standing and his children remain recognized as Parsi. She can seek recourse in civil courts and community reform bodies, but doing so means contesting the community's own custom-derived authority, at real social cost within a small, close-knit population.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_women_marrying_outside_community, payer,
    moderate, biographical, constrained, national).

% Children of a Parsi mother and non-Parsi father face contested or denied recognition as Parsi in matters of religious initiation (navjote), fire temple access, and Tower of Silence rites, based entirely on the parent's gender rather than any doctrinal necessity. They have no voice in the custom that determines their status and cannot exit a classification imposed at birth.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, children_of_mixed_marriages, payer,
    powerless, biographical, trapped, national).

% Non-Parsi spouses, disproportionately wives of Parsi men in practice treated more leniently than the reverse, still cannot themselves convert into full communal recognition in most Parsi communities, and their presence indirectly fuels the demographic-decline narrative used to justify tightening endogamy enforcement further.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, intermarried_parsi_men_partners, payer,
    powerless, biographical, constrained, national).

% Groups and individuals within the community who argue the gender-asymmetric endogamy rule is a custom-drift rather than doctrinal requirement, and who litigate in civil courts (e.g. the Goolrukh Gupta case) for equal treatment of women's children. Their arguments are heard in civil forums but carry no binding weight inside the community's own tribunal structure, which remains governed by the panchayats and matrimonial courts.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, parsi_reformist_advocates, excluded,
    moderate, generational, constrained, national).

% Adjudicate individual rights claims (Article 14 equality, Article 25 religious freedom) that arise when community custom conflicts with constitutional guarantees, without directly displacing the 1936 Act's jurisdiction over Parsi matrimonial matters. Their rulings create pressure on the community's self-governance without formally abolishing it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__parsi_communal_reading, indian_constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__parsi_communal_reading, parsi_community_institutions).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__parsi_communal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Parsis, a small and demographically shrinking community, with an internally administered, doctrinally attuned forum for marriage, divorce, and inheritance-adjacent adjudication, preserving a distinct religious-legal identity within a pluralistic national legal system and coordinating community members around shared endogamy-linked continuity concerns.
% TRANSFER_FUNCTION: Moves religious and social standing, ritual access, and recognized communal membership from women who marry outside the community and their children to men who do so and their children, transferring the cost of maintaining strict community boundaries disproportionately onto female members and mixed-marriage offspring while communal institutions retain adjudicatory authority and social capital.
% ABSENT_VOICES: Parsi reformist advocates and the children of mixed marriages themselves have no seat inside the panchayat/matrimonial court structure; their objections are litigated externally in constitutional courts, which can pressure but not directly rewrite the community's internally administered custom.
% DISAPPEARANCE_RATIONALE: If the Act and its custom-derived authority disappeared overnight, Parsi marriage and divorce matters would fall under the Special Marriage Act or general civil jurisdiction, matrimonial court officers would lose their adjudicatory role, the gender-asymmetric endogamy consequences would lose their institutional enforcement mechanism (though social/ritual consequences administered by temple trusts could persist independently), and the community's claim to a distinct personal-law identity within India's pluralistic legal order would be substantially altered.
% FOUNDING_PROBLEM: A tiny, geographically concentrated religious minority migrating from Persia sought a legally recognized, doctrinally appropriate forum for marriage and divorce that would not require submitting intimate community matters to British colonial courts unfamiliar with Zoroastrian custom, and that would help preserve a numerically small community's distinct identity against assimilation.
% FOUNDING_PROBLEM_CORROBORATION: Community institutions and endogamously-married members attest the founding problem (preserving communal identity amid demographic fragility) remains live and urgent, citing a Parsi population that has fallen from over 100,000 to under 60,000 in India. Reformist advocates within the community and constitutional court rulings (including in the Goolrukh Gupta litigation) attest that the gender-asymmetric enforcement mechanism has drifted from any doctrinal necessity into an extraction of standing from women and their children, and that population decline is better addressed by inclusive membership rules than exclusionary ones — corroboration here comes from within the community's own reform wing and from courts external to the panchayat structure, not solely from beneficiaries.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__parsi_communal_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__parsi_communal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__parsi_communal_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.42) reflects that the core adjudicative function — resolving Parsi marriage and divorce matters — is not itself predatory, but the endogamy-enforcement layer riding on top of it imposes real, asymmetric costs concentrated on women and mixed-parentage children. Suppression (0.55) is moderate-high: exit from the custom's jurisdiction requires either accepting loss of communal/ritual standing or litigating externally in civil courts against one's own community, a real but not absolute barrier. Theater ratio (0.28) is low-moderate: the matrimonial courts perform genuine adjudicative work, though an increasing share of enforcement activity concerns boundary-maintenance (who counts as Parsi) rather than dispute resolution proper, and that share has grown as demographic anxiety intensified. Accessibility collapse (0.6) is moderate: alternatives (civil marriage under the Special Marriage Act) exist and are used, but choosing them typically means exiting the specific communal-recognition benefits, so the collapse is meaningful without being total. Resistance (0.5) reflects active internal reform pressure (Goolrukh Gupta litigation, reformist advocacy) balanced against strong communal attachment to the custom among those it favors.
 *
 * DIRECTIONALITY LOGIC:
 *   Community institutions and matrimonial court officers sit at the beneficiary end: they administer the system, derive institutional relevance and social capital from its continuation, and bear none of its costs. Endogamously-married Parsi men are structural beneficiaries by birth-position: the custom never tests their standing. Parsi women marrying outside the community, and especially the resulting children (powerless, trapped — their status is fixed at birth by a rule they had no part in and cannot exit without repudiating their own parentage), sit at the target end. Intermarried partners of Parsi men occupy an intermediate position: they are excluded from full recognition but do not bear the sharper social sanction directed at Parsi women who marry out.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing a doctrinally appropriate forum and preserving a fragile minority's distinct identity — remains genuinely live given continuing demographic decline, which is why this is not simply relabeled as a snare. But the specific mechanism used to address that problem, gender-asymmetric endogamy enforcement, has drifted from the founding purpose: it does not doctrinally follow from Zoroastrian tradition (contested even within the community) and demonstrably accelerates the demographic decline it purports to prevent by driving intermarrying women and their children out of the community rather than retaining them. Classifying this as tangled_rope rather than snare or rope prevents two mislabeling errors: treating the entire custom-derived authority as pure extraction (it performs real adjudicative coordination work that the community values and that has no clean substitute within civil courts alone) and treating it as pure benign coordination (which would erase the real, gender-differential costs borne by specific identifiable victims through the same mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    custom_versus_drift_gender_asymmetry,
    'Is the gender-asymmetric endogamy rule (Parsi women lose standing on intermarriage; Parsi men and their mixed-parentage children do not) a doctrinal requirement of Zoroastrian tradition, or a patrilineal social custom that has attached itself to the 1936 Act''s authority without theological necessity?',
    'Comparative textual and historical analysis of Zoroastrian scripture and pre-colonial community practice, cross-referenced against the trajectory of the rule''s application since 1936; testimony already on record in the Goolrukh Gupta litigation from religious scholars on both sides.',
    'If the asymmetry is doctrinally required, the tangled_rope classification understates how load-bearing the extraction is to the coordination function itself (harder to sever without dissolving the custom). If it is a non-doctrinal patrilineal accretion, the coordination function can in principle be preserved while removing the extraction, making the tangled_rope classification a snapshot of a separable, reformable defect rather than an intrinsic feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custom_versus_drift_gender_asymmetry, conceptual, 'Whether gender-asymmetric endogamy enforcement is doctrinally load-bearing or a severable custom-drift.').

omega_variable(
    kernel_reading_boundary_location,
    'This story treats the Parsi communal reading as a self-contained constraint distinct from the hindu_codified, muslim_shariat, christian_canonical, and secular_civil readings of the same marriage_authority_kernel — but where exactly does the disagreement between readings live: in who counts as the legitimate source of authority (custom vs. codified statute vs. constitutional right), or in what counts as the relevant community (religious community vs. national citizenry)?',
    'Structural comparison across all five sibling constraint files: identify whether the beneficiary/victim sets, enforcement mechanisms, and exit-option profiles cluster around the authority-source axis or the community-definition axis.',
    'If the disagreement is primarily about authority-source, this reading''s tangled_rope classification is largely independent of the sibling readings'' classifications (each kernel-reading pair is doctrinally self-contained). If it is primarily about community-definition, the readings may share structural pressure points (e.g. all endogamy-linked personal-law systems facing similar demographic or constitutional-equality challenges) that should be reflected via network.affects_constraints links rather than treated as fully independent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Where the kernel-level disagreement among the five readings is structurally located.').

omega_variable(
    demographic_decline_causal_direction,
    'Does strict endogamy enforcement causally accelerate the Parsi population decline it is invoked to prevent (by excluding women who marry out and their children from the community''s reproductive base), or is the population decline driven by independent factors (late marriage age, low fertility, urban migration) such that endogamy enforcement is a response to decline rather than a contributor to it?',
    'Demographic modeling comparing community population trajectories under current exclusionary rules versus counterfactual inclusive-membership rules, informed by comparable minority communities that have relaxed matrilineal exclusion.',
    'If enforcement measurably accelerates decline, the mandatrophy analysis strengthens: the mechanism actively undermines its own founding purpose, sharpening the case that this is drifted extraction rather than functional coordination. If decline is independent of the rule, the founding-problem status remains genuinely contested rather than trending toward dead-function-persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_decline_causal_direction, empirical, 'Whether endogamy enforcement causally worsens the demographic decline used to justify it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__parsi_communal_reading, 1936, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1936, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1936, 0.12).
narrative_ontology:measurement(marr_tr_t1954, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(marr_tr_t1980, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 1980, 0.19).
narrative_ontology:measurement(marr_tr_t2000, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2000, 0.23).
narrative_ontology:measurement(marr_tr_t2012, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2012, 0.26).
narrative_ontology:measurement(marr_tr_t2024, marriage_authority_kernel__parsi_communal_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(marr_be_t1936, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1936, 0.3).
narrative_ontology:measurement(marr_be_t1954, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1954, 0.32).
narrative_ontology:measurement(marr_be_t1980, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 1980, 0.36).
narrative_ontology:measurement(marr_be_t2000, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2000, 0.39).
narrative_ontology:measurement(marr_be_t2012, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2012, 0.41).
narrative_ontology:measurement(marr_be_t2024, marriage_authority_kernel__parsi_communal_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1936, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1936, 0.4).
narrative_ontology:measurement(marr_su_t1954, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1954, 0.44).
narrative_ontology:measurement(marr_su_t1980, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 1980, 0.48).
narrative_ontology:measurement(marr_su_t2000, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2000, 0.51).
narrative_ontology:measurement(marr_su_t2012, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2012, 0.53).
narrative_ontology:measurement(marr_su_t2024, marriage_authority_kernel__parsi_communal_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__parsi_communal_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_authority_kernel__parsi_communal_reading, 0.1).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, christian_canonical_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__parsi_communal_reading, secular_civil_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language concept 'marriage/family law authority in India' along the kernel marriage_authority_kernel. Each sibling reading (hindu_codified, muslim_shariat, christian_canonical, parsi_communal, secular_civil) authors its own ε, beneficiary/victim structure, and classification, reflecting that the underlying legal-pluralism arrangement is not one constraint measured five ways but five structurally distinct authority claims that happen to share a jurisdictional domain (Indian personal law). The parsi_communal_reading is distinguished from its siblings by dedicated community tribunals (panchayats/matrimonial courts rather than ordinary civil courts), a demographic-survival justification largely absent from the larger-population sibling readings, and a gender-asymmetric endogamy mechanism whose severability from the coordination function is the story's central open question.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
