% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__compact_federalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__compact_federalism, []).

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
 *   constraint_id: provincial_sovereignty_boundary__compact_federalism
 *   human_readable: Compact Federalism: Provincial Sovereignty Boundary
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   The compact_federalism reading of the provincial sovereignty boundary
 *   treats Confederation as a treaty-like compact among pre-existing
 *   sovereign provinces. Provinces retain residual sovereignty; federal
 *   authority is delegated and conditional on provincial consent.
 *   Equalization is a negotiable term, not a constitutional entitlement.
 *   Climate policy falls under provincial jurisdiction (natural resources,
 *   property rights) subject to federal override only with provincial
 *   agreement. Exit is a negotiated right under duress — not unilateral
 *   secession, not federal permission. This reading is instantiated by
 *   Alberta's Sovereignty Act, Quebec's distinct society claims, and the 1998
 *   Secession Reference's 'negotiated departure' framework. The constraint is
 *   the division of powers itself (ss. 91/92 Constitution Act 1867, s.92A
 *   1982) as interpreted through this compact lens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, 0.65).
domain_priors:suppression_score(provincial_sovereignty_boundary__compact_federalism, 0.45).
domain_priors:theater_ratio(provincial_sovereignty_boundary__compact_federalism, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, extractiveness, 0.65).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__compact_federalism, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__compact_federalism, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__compact_federalism, "Compact Federalism: Provincial Sovereignty Boundary").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__compact_federalism, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__compact_federalism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__compact_federalism, 'a66392f1-8c08-458f-bf9e-317496bd582a').
narrative_ontology:cs_kernel_codification('a66392f1-8c08-458f-bf9e-317496bd582a', formalized).
narrative_ontology:cs_authority_grounding('a66392f1-8c08-458f-bf9e-317496bd582a', lineage).
narrative_ontology:cs_interpretation_layer_present('a66392f1-8c08-458f-bf9e-317496bd582a').
narrative_ontology:cs_reading_relation('a66392f1-8c08-458f-bf9e-317496bd582a', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('a66392f1-8c08-458f-bf9e-317496bd582a', provincial_sovereignty_boundary__resource_sovereignty_primacy, coexists_with).
narrative_ontology:cs_axiom('a66392f1-8c08-458f-bf9e-317496bd582a', foundational, provincial_sovereignty_derives_from_compact).
narrative_ontology:cs_axiom_status(provincial_sovereignty_derives_from_compact, holdable).
narrative_ontology:cs_axiom_grounding('a66392f1-8c08-458f-bf9e-317496bd582a', provincial_sovereignty_derives_from_compact, conventional).
narrative_ontology:cs_axiom('a66392f1-8c08-458f-bf9e-317496bd582a', foundational, federal_authority_conditional_on_provincial_consent).
narrative_ontology:cs_axiom_status(federal_authority_conditional_on_provincial_consent, holdable).
narrative_ontology:cs_axiom_grounding('a66392f1-8c08-458f-bf9e-317496bd582a', federal_authority_conditional_on_provincial_consent, conventional).
narrative_ontology:cs_axiom('a66392f1-8c08-458f-bf9e-317496bd582a', secondary, equalization_as_negotiable_term_not_entitlement).
narrative_ontology:cs_axiom_status(equalization_as_negotiable_term_not_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('a66392f1-8c08-458f-bf9e-317496bd582a', equalization_as_negotiable_term_not_entitlement, instrumental).
narrative_ontology:cs_reference_frame('a66392f1-8c08-458f-bf9e-317496bd582a', id_1867_compact_of_sovereign_provinces).
narrative_ontology:cs_drift_state('a66392f1-8c08-458f-bf9e-317496bd582a', contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a66392f1-8c08-458f-bf9e-317496bd582a', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__compact_federalism, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, have_not_provinces).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, have_provinces).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__compact_federalism, citizens).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, quebec).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__compact_federalism, citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets federal legislative agenda, controls spending power and equalization formula, appoints Supreme Court justices. Claims authority to address national concerns (climate, health care) under POGG clause. Collects GST and equalization contributions; distributes transfers. Can credibly threaten to withhold transfers or use declaratory power.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Alberta, Saskatchewan, Newfoundland & Labrador. Net contributors to equalization (approx $600B cumulative since 1957). Resource economies subject to federal climate policy (carbon price, emissions cap, tanker ban). Constitutional amendment formula requires 7/50 for most changes — effectively blocks exit. Threaten separation referendums (Alberta 2022 sovereignty act) but face economic integration costs.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, have_provinces, payer,
    powerful, generational, constrained, regional).

% Quebec, Manitoba, New Brunswick, Nova Scotia, PEI. Net recipients of equalization (Quebec receives ~$13B/yr, 50% of total). Depend on federal transfers for health, education, social services. Support federal climate policy when it brings green transition funding. Exit not seriously contemplated — federation delivers net fiscal benefit. Quebec adds distinct society claim and veto expectation.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, have_not_provinces, beneficiary,
    organized, generational, constrained, regional).

% Unique dual position: largest equalization recipient yet claims most extensive provincial sovereignty. Never signed 1982 Constitution Act; demands distinct society recognition, immigration control, cultural veto. Identity-locked exit: sovereignty referendums (1980, 1995) failed but movement persists as core political identity. Bill 96 (2022) asserts Quebec as nation within Canada — exit negotiated through identity, not economics.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, quebec, agenda_setter,
    powerful, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, quebec, payer).

% Inherent rights holders under s.35 Constitution Act 1982; not parties to 1867 compact. Federal-provincial division of powers ignores nation-to-nation relationship. Resource development on traditional territories approved by provinces without consent (e.g., TMX pipeline, Ring of Fire). Duty to consult is procedural, not veto. Exit from Canadian sovereignty not on offer — trapped in federal/provincial jurisdictional gap.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, indigenous_nations, excluded,
    moderate, generational, trapped, national).

% Bear tax burden of federal-provincial transfers; receive portable health care, equalization-funded services. Interprovincial mobility high (Charter s.6) — can exit provincial policy regimes by moving. But cannot exit federal climate policy or equalization structure. Benefit from national risk-pooling; pay for interregional redistribution.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, citizens, payer,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(provincial_sovereignty_boundary__compact_federalism, citizens, beneficiary).

% Supreme Court adjudicates division of powers (pith and substance, double aspect, POGG). Reference cases (Secession 1998, Carbon Pricing 2021) define boundary. Appointed by federal government; security of tenure. Neither collect nor pay — interpret the compact. Their readings shift the constraint's effective extraction over time.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__compact_federalism, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages shared currency, common market, national defense, interprovincial trade, and redistributive equalization among economically diverse provinces; provides dispute resolution via courts instead of armed conflict.
% TRANSFER_FUNCTION: Moves fiscal capacity from resource-rich provinces (AB, SK, NL) to resource-poor provinces (QC, MB, NB, NS, PE) via equalization (~$24B/yr); moves policy autonomy from provinces to federal government via spending power (health transfers with conditions) and POGG doctrine (carbon pricing, emergencies).
% ABSENT_VOICES: Indigenous nations with inherent title and self-government rights (excluded from 1867 compact, s.35 added 1982 without consent); territories (YT, NT, NU) without provincial status but subject to federal control; future generations bearing climate policy costs locked in by current jurisdictional stalemate.
% DISAPPEARANCE_RATIONALE: Without the division of powers, either a unitary state emerges (federal government absorbs all jurisdiction) or the confederation dissolves into sovereign provinces — equalization ends, common market fragments, climate policy becomes 13 uncoordinated regimes. The 1998 Secession Reference confirmed negotiated exit, not unilateral, but negotiation requires federal participation.
% FOUNDING_PROBLEM: Uniting British North American colonies for mutual defense, transcontinental railway, and economic development while preserving distinct French-Canadian society in Quebec and regional autonomy for Maritime and Western colonies.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars outside beneficiary set (Peter Russell, Jeremy Webber, Brian Slattery) attest founding compact included Quebec's distinct status and provincial sovereignty as core terms; Royal Commission on Aboriginal Peoples (1996) attests founding problem excluded Indigenous nations entirely; Alberta Sovereignty Act (2022) and Quebec Bill 96 (2022) demonstrate founding problem remains live for provinces but with divergent readings.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__compact_federalism, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__compact_federalism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__compact_federalism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__compact_federalism, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__compact_federalism, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__compact_federalism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__compact_federalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__compact_federalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects net fiscal transfer from have to have-not provinces plus federal policy intrusion into provincial jurisdiction (carbon pricing, health conditions). Suppression (0.45) is moderate — courts enforce boundaries but provinces retain constitutional tools (notwithstanding clause, s.92A, reference cases). Theater (0.40) rising: first ministers' conferences perform collaboration while bilateral deals replace multilateralism. Accessibility collapse (0.52): secession legally possible but economically prohibitive; constitutional amendment formula (7/50) effectively blocks structural change. Resistance (0.62) high: constant provincial litigation, sovereignty legislation, carbon tax opposition. Claimed type tangled_rope: genuine coordination (common market, defense, risk pooling) coexists with asymmetric extraction (have provinces subsidize have-not; federal spending power penetrates provincial jurisdiction).
 *
 * PERSPECTIVAL GAP:
 *   From federal seat: constraint is rope — coordination mechanism enabling national projects. From have-province seat: constraint is snare — extraction mechanism locking resource wealth into federal redistribution. From Quebec seat: constraint is tangled rope — coordination on fiscal terms but snare on identity/sovereignty terms. From Indigenous seat: constraint is mountain of colonial law — immovable structure denying nationhood. Engine computes per-seat classification from these structural asymmetries; authored claim does not adjudicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal government and have-not provinces are structural beneficiaries (d ~0.15-0.25): federal collects GST, sets national standards, distributes transfers; have-not provinces receive net equalization. Have provinces are structural targets (d ~0.75): pay disproportionate equalization, face federal climate policy on their resource economies, constrained exit. Quebec occupies dual position — beneficiary of transfers (d ~0.2) but identity-locked target on sovereignty (d ~0.8). Indigenous nations are excluded targets (d ~0.9) — bear resource development costs without jurisdictional authority. Citizens are mobile payers/beneficiaries (d ~0.5). Courts are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   Founding problem (defence, railway, Quebec distinct society) is contested: defence/railway rationales obsolete; Quebec distinct society and regional autonomy remain live. Equalization has outlived 1957 design (resource revenue volatility not addressed; per-capita GDP formula creates perverse incentives). Federal spending power has accumulated mandates (health, climate, childcare) without provincial consent — mandatrophy unresolved. The constraint persists because no party can unilaterally rewrite it (amendment formula) and no coalition can agree on replacement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compact_vs_creature_ambiguity,
    'Is the Canadian federation a compact among sovereign provinces (compact_federalism) or a hierarchical constitution where provinces are creatures of statute (constitutional_subordination)?',
    'Supreme Court reference on whether provinces can unilaterally initiate secession negotiation without federal consent; or constitutional amendment attempting to entrench provincial veto.',
    'If compact_federalism prevails, federal spending power and POGG are narrowly construed; provinces gain veto on national projects. If constitutional_subordination prevails, federal authority expands; equalization becomes non-negotiable entitlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compact_vs_creature_ambiguity, conceptual, 'Foundational ambiguity: treaty-like compact vs. hierarchical statute.').

omega_variable(
    exit_mechanism_ambiguity,
    'Is provincial exit a negotiated right (compact_federalism), a unilateral right (resource_sovereignty_primacy), or a federally-permitted process (constitutional_subordination)?',
    'Clarity Act (2000) vs. Alberta Sovereignty Act (2022) confrontation; or a province holding a clear referendum question on independence and demanding negotiation.',
    'Negotiated exit (compact) gives federal government veto leverage. Unilateral exit (resource primacy) makes federation voluntary association. Federal-permitted exit (subordination) makes federation indissoluble without federal consent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_mechanism_ambiguity, conceptual, 'Whether exit is a provincial right, federal gift, or negotiated process.').

omega_variable(
    equalization_coordination_or_extraction,
    'Is equalization a genuine coordination function (risk pooling, national cohesion) or asymmetric extraction from have provinces to have-not provinces?',
    'Economic analysis of whether equalization improves national GDP vs. merely redistributes; or political negotiation of formula renewal (next 2029).',
    'If coordination, equalization is rope-like and non-negotiable floor. If extraction, it is snare-like and subject to provincial veto. Tangled rope classification hinges on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equalization_coordination_or_extraction, empirical, 'Whether fiscal federalism coordinates or extracts.').

omega_variable(
    climate_policy_jurisdictional_boundary,
    'Does federal carbon pricing/emissions cap under POGG constitute legitimate national concern or ultra vires intrusion into provincial s.92A resource jurisdiction?',
    'Supreme Court carbon pricing reference (2021) upheld federal law but left boundary contested; future reference on emissions cap or oil/gas production cap will test limit.',
    'If federal authority confirmed, provincial resource sovereignty erodes (extraction increases). If provincial jurisdiction confirmed, national climate coordination fails (coordination collapses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_policy_jurisdictional_boundary, empirical, 'Whether climate policy is federal coordination or provincial extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__compact_federalism, 0, 157).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(psb_cf_tr_t0, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(psb_cf_tr_t30, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 30, 0.15).
narrative_ontology:measurement(psb_cf_tr_t60, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 60, 0.22).
narrative_ontology:measurement(psb_cf_tr_t90, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 90, 0.28).
narrative_ontology:measurement(psb_cf_tr_t120, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 120, 0.34).
narrative_ontology:measurement(psb_cf_tr_t157, provincial_sovereignty_boundary__compact_federalism, theater_ratio, 157, 0.4).

% Extraction over time
narrative_ontology:measurement(psb_cf_be_t0, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(psb_cf_be_t30, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(psb_cf_be_t60, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 60, 0.42).
narrative_ontology:measurement(psb_cf_be_t90, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 90, 0.51).
narrative_ontology:measurement(psb_cf_be_t120, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 120, 0.58).
narrative_ontology:measurement(psb_cf_be_t157, provincial_sovereignty_boundary__compact_federalism, base_extractiveness, 157, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(psb_cf_su_t0, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(psb_cf_su_t30, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 30, 0.4).
narrative_ontology:measurement(psb_cf_su_t60, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 60, 0.42).
narrative_ontology:measurement(psb_cf_su_t90, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 90, 0.44).
narrative_ontology:measurement(psb_cf_su_t120, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 120, 0.45).
narrative_ontology:measurement(psb_cf_su_t157, provincial_sovereignty_boundary__compact_federalism, suppression_requirement, 157, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(provincial_sovereignty_boundary__compact_federalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(provincial_sovereignty_boundary__compact_federalism, 0.15).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, equalization_formula).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, federal_spending_power).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, carbon_pricing_backstop).
narrative_ontology:affects_constraint(provincial_sovereignty_boundary__compact_federalism, constitutional_amendment_formula).

% DUAL FORMULATION NOTE:
% This constraint (compact_federalism) is one reading of the provincial_sovereignty_boundary kernel. It forecloses constitutional_subordination (mutually exclusive sovereignty premises) and coexists_with resource_sovereignty_primacy (different grounding for provincial sovereignty — compact vs. resource title). All three readings share the same constitutional text (ss.91/92/92A) but instantiate different constraints with different ε, beneficiaries, and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, powerful, 0.78).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, institutional, 0.18).
constraint_indexing:directionality_override(provincial_sovereignty_boundary__compact_federalism, moderate, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
