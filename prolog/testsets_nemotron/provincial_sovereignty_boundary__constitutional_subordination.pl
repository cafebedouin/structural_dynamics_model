% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__constitutional_subordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__constitutional_subordination, []).

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
 *   constraint_id: provincial_sovereignty_boundary__constitutional_subordination
 *   human_readable: Provincial Constitutional Subordination — Federal Veto Over Exit
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'constitutional subordination' reading
 *   of the contested kernel 'provincial_sovereignty_boundary'. It asserts
 *   that provinces are creatures of the federal Constitution Act 1867/1982
 *   with no inherent sovereignty; exit (secession) requires federal consent
 *   via the Clarity Act; equalization and federal climate policy (GGPPA) are
 *   legitimate exercises of federal authority. The sibling readings —
 *   compact_federalism (provinces retain residual sovereignty, exit
 *   negotiable) and resource_sovereignty_primacy (s.92A resource ownership
 *   grounds absolute sovereignty) — are separate constraints with different
 *   ε, different victim/beneficiary structures, and different
 *   classifications. This reading's structural delta: federal veto over exit;
 *   equalization and climate policy as legitimate federal authority;
 *   separatism as constitutional nullity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, 0.68).
domain_priors:suppression_score(provincial_sovereignty_boundary__constitutional_subordination, 0.72).
domain_priors:theater_ratio(provincial_sovereignty_boundary__constitutional_subordination, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, extractiveness, 0.68).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__constitutional_subordination, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__constitutional_subordination, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__constitutional_subordination, "Provincial Constitutional Subordination — Federal Veto Over Exit").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__constitutional_subordination, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__constitutional_subordination).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__constitutional_subordination, '1d57e4a7-e4f0-40e4-9b63-f183b0c5564f').
narrative_ontology:cs_kernel_codification('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', formalized).
narrative_ontology:cs_authority_grounding('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', extraction).
narrative_ontology:cs_interpretation_layer_present('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f').
narrative_ontology:cs_reading_relation('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', provincial_sovereignty_boundary__compact_federalism, forecloses).
narrative_ontology:cs_reading_relation('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', provincial_sovereignty_boundary__resource_sovereignty_primacy, forecloses).
narrative_ontology:cs_axiom('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', foundational, federal_constitutional_paramountcy).
narrative_ontology:cs_axiom_status(federal_constitutional_paramountcy, holdable).
narrative_ontology:cs_axiom_grounding('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', federal_constitutional_paramountcy, conventional).
narrative_ontology:cs_axiom('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', foundational, secession_requires_federal_consent).
narrative_ontology:cs_axiom_status(secession_requires_federal_consent, holdable).
narrative_ontology:cs_axiom_grounding('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', secession_requires_federal_consent, conventional).
narrative_ontology:cs_reference_frame('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', confederation_1867_federal_union).
narrative_ontology:cs_drift_state('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', post_ggppa_scc_2021, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1d57e4a7-e4f0-40e4-9b63-f183b0c5564f', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__constitutional_subordination, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, federal_government).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_apparatus).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__constitutional_subordination, provincial_resource_revenue_authorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, federal_paramountcy_doctrine).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, equalization_as_national_cohesion).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__constitutional_subordination, climate_policy_federal_jurisdiction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds constitutional veto over provincial exit via Reference re Secession of Quebec (1998) and Clarity Act (2000). Sets equalization formula and national carbon pricing framework. Collects legitimacy and fiscal coordination benefits; bears enforcement costs of maintaining union.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Alberta, Saskatchewan, Newfoundland — net contributors to equalization, subject to federal carbon pricing. Cannot exit without federal consent; have challenged federal jurisdiction in court (Reference re Greenhouse Gas Pollution Pricing Act). Bear disproportionate fiscal transfer outflows; receive national market access and federal disaster relief in return.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, payer,
    powerful, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__constitutional_subordination, resource_rich_provinces, beneficiary).

% Quebec, Maritimes, Manitoba — net recipients of equalization payments. Benefit from federal redistribution framework they cannot unilaterally alter. Their fiscal capacity depends on federal transfer machinery; exit would sever the revenue stream.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, equalization_receiving_provinces, beneficiary,
    moderate, biographical, constrained, regional).

% Federal carbon pricing regime (GGPPA) and net-zero accountability legislation. Gains regulatory reach over provincial resource decisions; its legitimacy depends on federal constitutional authority being upheld. Would lose coherence if provinces could opt out of climate policy.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, national_climate_policy_apparatus, beneficiary,
    organized, generational, mobile, national).

% Quebec sovereigntists, Alberta independence advocates, Western alienation movements. Would object to federal veto over exit; structurally excluded from constitutional amendment formula (7/50 rule). Their exit claims are ruled unconstitutional by the very framework they seek to leave.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, separatist_movements, excluded,
    organized, biographical, identity_locked, regional).

% Alberta Energy Regulator, Saskatchewan Ministry of Energy and Resources, Newfoundland Petroleum Directorate. Administer provincial resource royalty regimes but operate within federal climate policy constraints (methane regulations, carbon pricing). Their regulatory autonomy is bounded by federal jurisdiction upheld by courts.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, provincial_resource_revenue_authorities, payer,
    moderate, biographical, constrained, regional).

% Neither federal nor provincial order fully recognizes inherent sovereignty. Section 35 rights exist within a constitutional framework that treats provinces as creatures of federal statute. Their consent is not required for federal-provincial deals affecting their territories (e.g., Trans Mountain, equalization).
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, indigenous_nations, excluded,
    moderate, generational, identity_locked, national).

% Interpret the constitutional architecture; their readings (compact vs. subordination vs. resource primacy) structure the legitimacy contest. Do not bear fiscal costs or collect rents; their authority derives from institutional position, not extraction.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__constitutional_subordination, constitutional_scholars_courts, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single fiscal and regulatory union across a geographically vast, resource-asymmetric federation: equalization prevents fiscal fragmentation; federal climate policy prevents free-riding on emissions; federal veto over exit prevents unilateral secession that would dissolve the federation.
% TRANSFER_FUNCTION: Moves resource revenues from net-contributing provinces (Alberta, Saskatchewan, Newfoundland) to net-receiving provinces via equalization; moves regulatory authority over emissions from provinces to federal level via GGPPA; moves exit authority from provincial legislatures to federal Parliament via Clarity Act.
% ABSENT_VOICES: Indigenous nations whose territorial sovereignty predates both federal and provincial orders; separatist movements structurally excluded by the 7/50 amending formula; would-be provincial exit referenda that cannot proceed without federal consent. The Clarity Act gives the federal House of Commons the power to decide whether a referendum question is 'clear' — the excluded parties have no veto over that judgment.
% DISAPPEARANCE_RATIONALE: If the federal veto over exit, equalization framework, and federal climate policy authority vanished overnight: resource-rich provinces would likely pursue greater fiscal autonomy or exit; equalization-receiving provinces would face immediate fiscal crisis; national carbon pricing would fragment into patchwork provincial regimes; the federation would either reorganize into a looser confederation or dissolve. The fiscal and regulatory architecture depends on this constraint.
% FOUNDING_PROBLEM: The 1867 Confederation was designed to prevent American-style fragmentation and create a viable economic union across British North American colonies with divergent resource endowments. The federal government was given residual power (POGG), disallowance, and declaratory power to prevent provincial policies from undermining the national project.
% FOUNDING_PROBLEM_CORROBORATION: Federal government and equalization-receiving provinces attest the founding problem (preventing fragmentation, managing asymmetry) remains live — citing ongoing separatist pressures and climate free-rider risks. Resource-rich provinces and separatist movements attest the founding problem is dead or transformed: the original threat was US annexation, not internal resource politics; the current constraint serves federal extraction, not federation survival. Academic historians (e.g., Janet Ajzenstat, Peter Russell) corroborate the compact-federalism reading as a live interpretive tradition, not merely a cover story.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__constitutional_subordination, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__constitutional_subordination, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__constitutional_subordination, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(provincial_sovereignty_boundary__constitutional_subordination, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__constitutional_subordination, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__constitutional_subordination, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__constitutional_subordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects substantial net fiscal transfers from resource-rich to equalization-receiving provinces, plus regulatory extraction via federal climate policy overriding provincial resource jurisdiction. Suppression (0.72) reflects the Clarity Act's federal veto over exit, the 7/50 amending formula that structurally excludes separatist movements, and court enforcement of federal paramountcy. Theater ratio (0.45) reflects genuine coordination functions (equalization prevents fiscal fragmentation; carbon pricing prevents free-riding) coexisting with extractive elements (federal veto over exit serves federal unity more than provincial welfare). The measurement series tracks the post-1982 Charter/Constitution Act era through Reference re Secession (1998), Clarity Act (2000), GGPPA enactment (2018), and Supreme Court upholding (2021).
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, this constraint is genuine coordination (rope-like): it solves collective action problems (fiscal equalization, climate free-riding, secession prevention) that would otherwise fragment the federation. From resource-rich provincial seats, the same structure operates as extraction enforced by a federal veto they cannot overcome — a tangled rope where coordination benefits others and costs fall on them. From separatist seats, it is a snare: the exit gate is locked by the very authority they seek to leave. The engine will compute this divergence; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government (agenda_setter, institutional power, arbitrage exit) is the structural beneficiary — collects fiscal coordination rents and regulatory authority, controls the exit gate. Resource-rich provinces (payer, powerful, constrained exit) are primary targets — bear disproportionate fiscal outflows and regulatory override; cannot exit without federal consent. Equalization-receiving provinces (beneficiary, moderate, constrained exit) gain net transfers but lose exit leverage. Separatist movements (excluded, identity_locked) are the most extraction-targeted — their core demand (exit) is structurally nullified. Indigenous nations (excluded, identity_locked) are doubly excluded — from both federal and provincial orders. The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing US-style fragmentation, managing resource asymmetry) was live in 1867. By 2025, the original threat (annexation) is gone, but new collective action problems (climate coordination, fiscal equalization in a decarbonizing economy) have emerged. The constraint has not atrophied — it has been repurposed. The mandatrophy question is whether the current extraction (federal veto over exit, GGPPA override of provincial resource jurisdiction) is proportional to the current coordination function, or whether the constraint now primarily serves federal institutional self-preservation. The 'contested' founding_problem_status captures this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_veto_legitimacy,
    'Is the federal veto over provincial exit (Clarity Act) a genuine coordination mechanism preventing destructive fragmentation, or an extractive lock-in that prevents provinces from escaping unfavorable fiscal terms?',
    'Counterfactual analysis: if a province held a clear referendum on a clear question and the federal House of Commons refused to negotiate, would the resulting crisis strengthen or weaken the federation''s legitimacy? Historical precedent: 1995 Quebec referendum (49.4% Yes) did not trigger Clarity Act because question deemed unclear.',
    'If the veto is extractive lock-in, the constraint''s suppression is higher than measured — the exit barrier is not coordination but capture. If coordinative, the suppression is the price of preventing a cascade of secessions that would impoverish all parties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_veto_legitimacy, conceptual, 'Whether the federal exit veto is coordination or capture.').

omega_variable(
    equalization_extraction_boundary,
    'At what point does equalization shift from fiscal coordination (preventing provincial insolvency) to extraction (permanent transfer from resource-rich to resource-poor provinces regardless of fiscal capacity)?',
    'Longitudinal analysis of equalization formula changes (1982, 2004, 2007, 2009, 2013, 2019) against provincial fiscal capacity indices. Does the formula track fiscal need or political equilibrium?',
    'If equalization has become extraction, the constraint''s extractiveness is understated for resource-rich provinces and the ''tangled_rope'' classification should lean toward snare for those seats. If coordinative, the current ε is fair.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(equalization_extraction_boundary, empirical, 'Whether equalization has crossed from coordination into extraction.').

omega_variable(
    climate_policy_pretext,
    'Is federal climate policy (GGPPA) a genuine coordination response to interprovincial emissions free-riding, or a pretext for federal intrusion into provincial resource jurisdiction (s.92A)?',
    'Compare emissions outcomes in provinces with federal backstop vs. provincial systems (BC, Quebec). If outcomes are equivalent, the federal backstop is extractive — it asserts jurisdiction without marginal benefit. If federal backstop achieves lower emissions, coordination function is real.',
    'If pretext, the constraint''s theater_ratio is higher — climate policy is performative cover for jurisdictional expansion. If genuine, theater_ratio is lower and the tangled_rope coordination function is substantiated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_policy_pretext, empirical, 'Whether federal climate policy is coordination or jurisdictional capture.').

omega_variable(
    indigenous_exclusion_structural,
    'Does the constitutional subordination reading structurally require Indigenous exclusion, or is Indigenous exclusion a contingent feature that could be resolved within this reading?',
    'Analyze whether Section 35 ''existing aboriginal and treaty rights'' can be reconciled with federal paramountcy over provincial exit and resource jurisdiction. If reconciliation requires abandoning the subordination reading, the exclusion is structural.',
    'If structural, the constraint''s victim set is larger than authored — Indigenous nations are systematic excluded parties whose consent is not required for federal-provincial deals affecting their territories. This would increase suppression and extraction metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_exclusion_structural, conceptual, 'Whether Indigenous exclusion is structural to this reading or contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__constitutional_subordination, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t1982, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1982, 0.25).
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t1990, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t1998, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t2000, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t2010, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t2018, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2018, 0.42).
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t2021, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2021, 0.44).
narrative_ontology:measurement(prov_sov_boundary_const_sub_tr_t2025, provincial_sovereignty_boundary__constitutional_subordination, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t1982, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1982, 0.45).
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t1990, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t1998, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 1998, 0.58).
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t2000, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t2010, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t2018, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t2021, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2021, 0.67).
narrative_ontology:measurement(prov_sov_boundary_const_sub_be_t2025, provincial_sovereignty_boundary__constitutional_subordination, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t1982, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1982, 0.5).
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t1990, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t1998, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 1998, 0.62).
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t2000, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t2010, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t2018, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2018, 0.69).
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t2021, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2021, 0.71).
narrative_ontology:measurement(prov_sov_boundary_const_sub_su_t2025, provincial_sovereignty_boundary__constitutional_subordination, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__constitutional_subordination, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__constitutional_subordination, 0.12).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, equalization_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, federal_carbon_pricing_backstop).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, clarity_act_secession_framework).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, provincial_resource_jurisdiction_s92a).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, indigenous_consent_duty_haida).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, compact_federalism_reading).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__constitutional_subordination, resource_sovereignty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint (constitutional_subordination) is one of three readings of the provincial_sovereignty_boundary kernel. The compact_federalism reading treats provinces as sovereign compact partners (lower extraction, higher provincial exit rights). The resource_sovereignty_primacy reading treats s.92A resource ownership as grounding absolute provincial sovereignty (highest provincial extraction, federal veto nullified). All three share the same constitutional text but instantiate different constraints with different ε. This reading is the one currently enforced by courts and federal institutions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__constitutional_subordination, institutional, 0.15).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__constitutional_subordination, powerful, 0.75).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__constitutional_subordination, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
