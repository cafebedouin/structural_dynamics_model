% ============================================================================
% CONSTRAINT STORY: family_law_authority__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: family_law_authority__christian_canonical_reading
 *   human_readable: Christian Canonical Reading of Marriage Authority
 *   domain: comparative_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   This constraint story captures the Christian canonical reading of the
 *   family_law_authority kernel, in which marriage is treated as a sacrament
 *   (Catholic) or as an institution under denominational governance
 *   (Protestant), with ecclesiastical authorities retaining jurisdiction over
 *   validity, entry, and exit. The structural delta within the reading is
 *   sharp: Catholic indissolubility imposes high extraction on Catholic
 *   spouses, while Protestant denominational variance permits divorce under
 *   assembly rules, lowering extraction for Protestant laity. Both
 *   traditions, however, exclude civil-state authorities from adjudicating
 *   validity. The story claims Tangled Rope because the arrangement
 *   simultaneously coordinates communal religious identity and asymmetrically
 *   extracts marital autonomy, especially from Catholic parties.
 *
 * KEY AGENTS:
 *   - Catholic Magisterium: Primary agenda-setter and beneficiary (institutional/global/constrained) â sets canon law and annulment standards.
 *   - Protestant Denominational Assemblies: Secondary agenda-setter and beneficiary (institutional/national/constrained) â govern divorce and remarriage within denominational canons.
 *   - Catholic Married Laicity: Primary payer/target (powerless/local/trapped) â bears the cost of sacramental indissolubility.
 *   - Protestant Married Laicity: Secondary payer (moderate/local/constrained) â subject to denominational authority but with viable divorce pathways.
 *   - Secular State Authorities: Excluded seat (institutional/national/analytical) â civil family law is subordinated to ecclesiastical jurisdiction.
 *   - Comparative Family Law Scholars: Analytical observer (analytical/global/analytical) â tracks divergence across canonical and civil regimes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_law_authority__christian_canonical_reading, 0.62).
domain_priors:suppression_score(family_law_authority__christian_canonical_reading, 0.72).
domain_priors:theater_ratio(family_law_authority__christian_canonical_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(family_law_authority__christian_canonical_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_law_authority__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(family_law_authority__christian_canonical_reading, "Christian Canonical Reading of Marriage Authority").
narrative_ontology:topic_domain(family_law_authority__christian_canonical_reading, "comparative_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(family_law_authority__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_law_authority__christian_canonical_reading, 'dc65d1b9-79e2-4a19-b5d3-813d01ac73ee').
narrative_ontology:cs_kernel_codification('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', fixed_text).
narrative_ontology:cs_authority_grounding('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', lineage).
narrative_ontology:cs_interpretation_layer_present('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee').
narrative_ontology:cs_reading_relation('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', family_law_authority__hindu_dharmashastra_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', family_law_authority__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', family_law_authority__parsi_zoroastrian_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', family_law_authority__secular_contractual_reading, forecloses).
narrative_ontology:cs_axiom('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', foundational, sacramental_ecclesiastical_jurisdiction).
narrative_ontology:cs_axiom_status(sacramental_ecclesiastical_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', sacramental_ecclesiastical_jurisdiction, theological).
narrative_ontology:cs_axiom('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', foundational, grace_constitutes_marriage).
narrative_ontology:cs_axiom_status(grace_constitutes_marriage, holdable).
narrative_ontology:cs_axiom_grounding('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', grace_constitutes_marriage, theological).
narrative_ontology:cs_reference_frame('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', sacramental_order_framework).
narrative_ontology:cs_drift_state('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', contemporary_secular_family_law_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('dc65d1b9-79e2-4a19-b5d3-813d01ac73ee', '').
narrative_ontology:cs_kernel_id(family_law_authority__christian_canonical_reading, family_law_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, catholic_magisterium).
narrative_ontology:constraint_beneficiary(family_law_authority__christian_canonical_reading, protestant_denominational_assemblies).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, catholic_married_laicity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(family_law_authority__christian_canonical_reading, protestant_married_laicity).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, sacramental_permanence_doctrine).
narrative_ontology:constraint_vindicates(family_law_authority__christian_canonical_reading, ecclesiastical_jurisdiction_over_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims exclusive sacramental jurisdiction over marriage through canon law and the annulment tribunal system; enforces indissolubility for Catholic parties. Derives institutional legitimacy, global moral authority, and communal boundary maintenance from this control.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_magisterium, agenda_setter,
    institutional, civilizational, constrained, global).

% Adjudicate marriage validity, divorce, and remarriage within denominational canons and synodal governance. While permitting divorce under denominational rules, they retain authority to exclude from communion or leadership, thereby preserving institutional control over marital boundaries.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_denominational_assemblies, agenda_setter,
    institutional, generational, constrained, national).

% Enter marriage under sacramental indissolubility with no right to divorce; exit is possible only through rare annulment or apostasy. Bears the full biographical cost of marital permanence enforced by ecclesiastical authority.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, catholic_married_laicity, payer,
    powerless, biographical, trapped, local).

% Subject to denominational authority over marriage validity and remarriage. While divorce is permitted under assembly rules, the terms, recognition, and spiritual consequences remain under church control rather than individual or civil discretion.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, protestant_married_laicity, payer,
    moderate, biographical, constrained, local).

% Civil courts and legislatures that would otherwise regulate marriage and divorce are structurally subordinated or bypassed in this reading; their family-law authority is excluded from questions of sacramental validity.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, secular_state_authorities, excluded,
    institutional, generational, analytical, national).

% Analyze the divergence between ecclesiastical and civil marriage regimes, documenting how canonical authority allocates marital exit rights differently across Christian traditions without being bound to any single confessional framework.
narrative_ontology:constraint_stakeholder(family_law_authority__christian_canonical_reading, comparative_family_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious community boundaries, kinship legitimacy, and intergenerational transmission of faith by sacralizing marriage and placing its validity under ecclesiastical rather than individual or state control.
% TRANSFER_FUNCTION: Transfers authority over marital entry, validity, and exit from individual spouses and civil states to ecclesiastical tribunals and denominational assemblies; concentrates the cost of indissolubility on Catholic laity while distributing communal cohesion benefits across the faithful.
% ABSENT_VOICES: Secular family-law scholars, feminist critics of patriarchal marriage structures, civil-state authorities asserting exclusive jurisdiction, and non-Christian religious jurists are excluded from canonical adjudication; their objections are audible only in rival readings of the kernel.
% DISAPPEARANCE_RATIONALE: If sacramental ecclesiastical authority over marriage vanished, Catholic spouses would gain access to civil divorce, Protestant communities would lose denominational control over remarriage eligibility, and the boundary between religious and civil family law would collapse â the global family-law landscape would reorganize around civil-contract principles.
% FOUNDING_PROBLEM: The early Christian community needed to distinguish its marital ethics from Roman contractual and patrilineal customs, stabilize kinship for missionary expansion, and secure ecclesiastical authority over domestic life against competing civil and familial jurisdictions.
% FOUNDING_PROBLEM_CORROBORATION: Secular legal historians corroborate the historical instability of Roman-era kinship and the strategic value of ecclesiastical marriage regulation; however, sociologists and civil rights advocates outside the beneficiary ecclesiastical seats attest that the arrangement now persists primarily as institutional authority maintenance rather than communal stabilization.
narrative_ontology:disappearance_verdict(family_law_authority__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(family_law_authority__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(family_law_authority__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(family_law_authority__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(family_law_authority__christian_canonical_reading, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.62) reflects the moderate-to-high extraction of marital autonomy, weighted heavily toward Catholic spouses under indissolubility and lightened by Protestant variance. Suppression (0.72) is high because civil divorce alternatives are actively foreclosed as spiritually invalid and penalized through exclusion from sacraments. Theater ratio (0.30) captures the performative dimension of annulment proceedings and doctrinal distinctions that maintain the appearance of permanence while managing practical exit. Accessibility collapse (0.75) is high: once inside the sacramental framework, civil alternatives are treated as ontologically invalid. Resistance (0.50) reflects sustained secularization, internal dissent, and competing civil marriage movements. The measurement series share a single time grid and show a medieval peak in extraction and enforcement, a post-Reformation and modern decline, and a contemporary partial resurgence of enforcement demand amid culture-war politics.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (Catholic Magisterium and Protestant assemblies) experience the constraint as necessary coordination of sacramental order and communal boundary maintenance; the payer seats (especially Catholic laity) experience it as asymmetric extraction of biographical autonomy. Protestant spouses occupy a middle position: coordinated into denominational community but with lower effective extraction because their traditions permit ecclesiastically recognized divorce. The engine computes this divergence from the structural asymmetry in exit options and power.
 *
 * DIRECTIONALITY LOGIC:
 *   The Catholic Magisterium and Protestant denominational assemblies are declared beneficiaries, deriving institutional authority and communal control; their directionality sits near the beneficiary end, though constrained exit options prevent full arbitrage. Catholic married laity are declared victims/payers with trapped exit, placing their directionality near the full-target end. Protestant married laity are payers but not declared victims, reflecting their moderated extraction; their directionality is intermediate. Secular state authorities are excluded and bear no directionality weight in extraction computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â distinguishing Christian marriage from Roman kinship instability â is contested as to whether it persists in modernity. The constraint is not a pure Snare because it supplies genuine coordination (sacramental community, intergenerational religious transmission). It is not a pure Rope because the coordination function is inseparable from asymmetric authority and the denial of exit to Catholic spouses. The R5 genealogy flags a mandatrophy tension: the arrangement persists partly because of theological identity-lock rather than because the original kinship-instability problem remains live for contemporary believers. This prevents classification as either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    catholic_protestant_extraction_asymmetry,
    'Does the Christian canonical reading represent a single constraint with heterogeneous extraction across Catholic and Protestant sub-traditions, or should it decompose into two separate constraints with different epsilon profiles?',
    'Comparative canon-law analysis measuring the differential cost of marital exit (annulment difficulty vs. denominational divorce permission) and the degree of autonomous individual choice permitted in each tradition.',
    'If extraction is structurally divergent, the reading should split into separate Catholic and Protestant canonical constraints; if the variance is adjudicable within one authority structure, the unified reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(catholic_protestant_extraction_asymmetry, conceptual, 'Internal denominational variance in extraction and coordination').

omega_variable(
    sacramental_naturalness_or_construction,
    'Is sacramental marriage a natural-law feature of reality that the Church discovers and guards, or a constructed jurisdictional rule that benefits identifiable ecclesiastical authorities?',
    'Historical-theological analysis of doctrinal development (e.g., pre-Tridentine practice, Eastern Catholic variance, and the formalization of the 1917 and 1983 Codes) to determine the degree of institutional construction versus claimed natural-law status.',
    'If genuine natural law, the constraint would trend toward Mountain classification; if substantially constructed, the current Tangled Rope classification is confirmed and the beneficiary declarations are structurally grounded.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sacramental_naturalness_or_construction, conceptual, 'Whether sacramental marriage is natural law or constructed authority').

omega_variable(
    suppression_mechanism_ambiguity,
    'For Catholic spouses subject to indissolubility, is the measured suppression structural (external canon-law barriers and sacramental penalties) or internalized (belief that divorce is metaphysically impossible regardless of external barriers)?',
    'Post-exit suppression trajectory study: observe whether individuals who leave the Catholic Church continue to experience psychological or social barriers to civil divorce and remarriage.',
    'If internalized, effective suppression exceeds the structural measure and the payer seat''s extraction is higher than the canon-law metric suggests; if purely structural, suppression tracks external enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for sacramental indissolubility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_law_authority__christian_canonical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fami_tr_t0, family_law_authority__christian_canonical_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(fami_tr_t20, family_law_authority__christian_canonical_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fami_tr_t40, family_law_authority__christian_canonical_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(fami_tr_t60, family_law_authority__christian_canonical_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(fami_tr_t80, family_law_authority__christian_canonical_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(fami_tr_t100, family_law_authority__christian_canonical_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(fami_be_t0, family_law_authority__christian_canonical_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fami_be_t20, family_law_authority__christian_canonical_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement(fami_be_t40, family_law_authority__christian_canonical_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(fami_be_t60, family_law_authority__christian_canonical_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(fami_be_t80, family_law_authority__christian_canonical_reading, base_extractiveness, 80, 0.5).
narrative_ontology:measurement(fami_be_t100, family_law_authority__christian_canonical_reading, base_extractiveness, 100, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fami_su_t0, family_law_authority__christian_canonical_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fami_su_t20, family_law_authority__christian_canonical_reading, suppression_requirement, 20, 0.85).
narrative_ontology:measurement(fami_su_t40, family_law_authority__christian_canonical_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(fami_su_t60, family_law_authority__christian_canonical_reading, suppression_requirement, 60, 0.6).
narrative_ontology:measurement(fami_su_t80, family_law_authority__christian_canonical_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement(fami_su_t100, family_law_authority__christian_canonical_reading, suppression_requirement, 100, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_law_authority__christian_canonical_reading, identity_coordination).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, hindu_dharmashastra_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, muslim_shariat_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, parsi_zoroastrian_reading).
narrative_ontology:affects_constraint(family_law_authority__christian_canonical_reading, secular_contractual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel family_law_authority. The Christian canonical reading treats marriage as a sacrament under ecclesiastical jurisdiction, structurally distinct from dharmic, shariat, Zoroastrian, and secular contractual readings. Each reading carries a different epsilon because the standing arrangement under contest differs: sacramental validity, dharmic samskara, Quranic nikah, community preservation, and autonomous civil contract are not the same constraint observed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
