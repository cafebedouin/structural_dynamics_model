% ============================================================================
% CONSTRAINT STORY: provincial_sovereignty_boundary__resource_sovereignty_primacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_provincial_sovereignty_boundary__resource_sovereignty_primacy, []).

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
 *   constraint_id: provincial_sovereignty_boundary__resource_sovereignty_primacy
 *   human_readable: Provincial Resource Sovereignty as Absolute Constitutional Right (s.92A Reading)
 *   domain: political_economy/federalism/resource_governance
 *
 * SUMMARY:
 *   This constraint instantiates the resource_sovereignty_primacy reading of
 *   the provincial_sovereignty_boundary kernel. The standing arrangement
 *   under contest is the constitutional and political claim that Section 92A
 *   of the Constitution Act 1982 grounds absolute provincial sovereignty over
 *   natural resources, with the structural implication that federal climate
 *   and fiscal policy in resource territory constitutes illegitimate
 *   extraction and that provinces possess a unilateral constitutional right
 *   to resist or exit such policy. This reading is structurally contested by
 *   constitutional_subordination (provinces as creatures of the constitution)
 *   and coexists uneasily with compact_federalism (confederation as
 *   inter-provincial compact).
 *
 * KEY AGENTS:
 *   - resource_rich_provincial_governments: Primary agenda-setter (institutional/constrained) â asserts and enforces the absolute sovereignty reading through legislation and litigation
 *   - fossil_fuel_industry: Primary beneficiary (powerful/mobile) â captures regulatory forbearance and decentralized permitting
 *   - federal_government: Primary payer (institutional/constrained) â bears the cost of blocked national climate and fiscal coordination
 *   - climate_vulnerable_citizens: Secondary payer (powerless/trapped) â bears the downstream costs of fragmented environmental governance
 *   - indigenous_treaty_nations: Excluded voice (moderate/constrained) â holds prior title that the provincial-federal binary systematically ignores
 *   - constitutional_courts: Analytical observer (institutional/analytical) â adjudicates the boundary but does not set the constitutional agenda
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.68).
domain_priors:suppression_score(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.62).
domain_priors:theater_ratio(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, extractiveness, 0.68).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(provincial_sovereignty_boundary__resource_sovereignty_primacy, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(provincial_sovereignty_boundary__resource_sovereignty_primacy, tangled_rope).
narrative_ontology:human_readable(provincial_sovereignty_boundary__resource_sovereignty_primacy, "Provincial Resource Sovereignty as Absolute Constitutional Right (s.92A Reading)").
narrative_ontology:topic_domain(provincial_sovereignty_boundary__resource_sovereignty_primacy, "political_economy/federalism/resource_governance").

domain_priors:requires_active_enforcement(provincial_sovereignty_boundary__resource_sovereignty_primacy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'ee078417-0366-4855-af00-b1d7984e2cbe').
narrative_ontology:cs_kernel_codification('ee078417-0366-4855-af00-b1d7984e2cbe', formalized).
narrative_ontology:cs_authority_grounding('ee078417-0366-4855-af00-b1d7984e2cbe', lineage).
narrative_ontology:cs_interpretation_layer_present('ee078417-0366-4855-af00-b1d7984e2cbe').
narrative_ontology:cs_reading_relation('ee078417-0366-4855-af00-b1d7984e2cbe', provincial_sovereignty_boundary__constitutional_subordination, forecloses).
narrative_ontology:cs_reading_relation('ee078417-0366-4855-af00-b1d7984e2cbe', provincial_sovereignty_boundary__compact_federalism, coexists_with).
narrative_ontology:cs_axiom('ee078417-0366-4855-af00-b1d7984e2cbe', foundational, provincial_resource_ownership_grounds_absolute_sovereignty).
narrative_ontology:cs_axiom_status(provincial_resource_ownership_grounds_absolute_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('ee078417-0366-4855-af00-b1d7984e2cbe', provincial_resource_ownership_grounds_absolute_sovereignty, conventional).
narrative_ontology:cs_axiom('ee078417-0366-4855-af00-b1d7984e2cbe', foundational, unilateral_exit_is_constitutional_right).
narrative_ontology:cs_axiom_status(unilateral_exit_is_constitutional_right, holdable).
narrative_ontology:cs_axiom_grounding('ee078417-0366-4855-af00-b1d7984e2cbe', unilateral_exit_is_constitutional_right, conventional).
narrative_ontology:cs_reference_frame('ee078417-0366-4855-af00-b1d7984e2cbe', provincial_resource_supremacy_framework).
narrative_ontology:cs_drift_state('ee078417-0366-4855-af00-b1d7984e2cbe', contemporary_climate_federalism, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ee078417-0366-4855-af00-b1d7984e2cbe', '').
narrative_ontology:cs_kernel_id(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provincial_governments).
narrative_ontology:constraint_beneficiary(provincial_sovereignty_boundary__resource_sovereignty_primacy, fossil_fuel_industry).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government).
narrative_ontology:constraint_victim(provincial_sovereignty_boundary__resource_sovereignty_primacy, climate_vulnerable_citizens).
narrative_ontology:constraint_vindicates(provincial_sovereignty_boundary__resource_sovereignty_primacy, provincial_resource_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert that provincial ownership of natural resources under s.92A Constitution Act 1982 grounds absolute sovereignty over resource development. Use constitutional litigation, sovereignty legislation, and political mobilization to block federal climate and fiscal policy in resource territory. Frame federal policy as unconstitutional extraction.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, resource_rich_provincial_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from provincial autonomy that blocks federal environmental regulation and carbon pricing. Supports constitutional and political claims that keep regulatory authority at the provincial level. Infrastructure is fixed but capital can shift jurisdictions.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, fossil_fuel_industry, beneficiary,
    powerful, biographical, mobile, national).

% Bears the cost of blocked climate and fiscal policy when provinces claim resource sovereignty. Must litigate constitutional challenges or negotiate with provinces to implement national standards. Its policy alternatives are constrained by constitutional division of powers and provincial resistance.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, federal_government, payer,
    institutional, generational, constrained, national).

% Bear the downstream costs of fragmented resource and climate policy when provincial sovereignty blocks national coordination. Cannot exit the atmospheric commons or the federation. Their interests are systematically underweighted when provinces and industry coordinate around resource extraction.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, climate_vulnerable_citizens, payer,
    powerless, civilizational, trapped, national).

% Hold treaty and title rights to lands and resources that precede and complicate provincial ownership claims. Are often structurally excluded from the provincial-federal sovereignty debate, which treats resource jurisdiction as exclusively between those two orders of government.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, indigenous_treaty_nations, excluded,
    moderate, generational, constrained, national).

% Adjudicate disputes between federal and provincial governments over resource and environmental jurisdiction. Have generally rejected absolute sovereignty claims but operate within a framework that recognizes strong provincial resource ownership rights.
narrative_ontology:constraint_stakeholder(provincial_sovereignty_boundary__resource_sovereignty_primacy, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns exclusive jurisdiction over natural resources to provinces, preventing inter-jurisdictional conflict over resource revenues and development approvals.
% TRANSFER_FUNCTION: Moves regulatory authority and resource rents from federal climate and fiscal policy space to provincial governments and industry, while transferring the costs of fragmented environmental governance to populations requiring national coordination.
% ABSENT_VOICES: Indigenous nations with treaty and title rights are structurally excluded from the provincial-federal sovereignty binary; climate scientists and intergenerational equity advocates are marginalized when the frame is strictly constitutional rather than ecological.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, federal climate and fiscal policy would operate in resource space without constitutional challenge; national carbon pricing and environmental assessment would apply uniformly; provincial resource revenues would be subject to shared standards; the federation would rearrange around concurrent jurisdiction rather than absolute provincial sovereignty.
% FOUNDING_PROBLEM: The 1982 constitutional patriation needed to resolve resource ownership disputes by constitutionally entrenching provincial ownership and management of natural resources, particularly for Alberta and Saskatchewan.
% FOUNDING_PROBLEM_CORROBORATION: Federal constitutional historians and the federal government attest the provision was a negotiated settlement to prevent expropriation, not a grant of absolute sovereignty that blocks federal climate policy. Indigenous legal scholars attest the founding problem excluded their title entirely. Resource-rich provinces attest the problem remains live because federal climate policy constitutes expropriation by other means.
narrative_ontology:disappearance_verdict(provincial_sovereignty_boundary__resource_sovereignty_primacy, world_rearranges).
narrative_ontology:founding_problem_status(provincial_sovereignty_boundary__resource_sovereignty_primacy, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(provincial_sovereignty_boundary__resource_sovereignty_primacy, 'none', 1).
narrative_ontology:epsilon_provenance(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(provincial_sovereignty_boundary__resource_sovereignty_primacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(provincial_sovereignty_boundary__resource_sovereignty_primacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint moves substantial regulatory and fiscal authority from the federal order to provinces and industry, blocking national climate coordination. Suppression (0.62) reflects the active constitutional litigation and sovereignty legislation that suppress federal policy alternatives. Theater ratio (0.45) captures the growing performative dimension â sovereignty acts that assert powers courts are unlikely to uphold. Accessibility collapse (0.52) is moderate: federal alternatives are legally difficult but not impossible. Resistance (0.70) is high because the federal government and climate coalitions actively contest the reading. The temporal series show monotonic intensification from 1982 to 2024 as climate federalism sharpened the conflict.
 *
 * PERSPECTIVAL GAP:
 *   From the provincial agenda-setter seat, the constraint is constitutional self-defense against federal overreach into sovereign territory. From the federal and citizen seats, the same arrangement is obstruction of necessary national coordination on climate and fiscal policy. The engine computes this divergence from the asymmetry in declared roles and exit options: provinces can mobilize constitutional lawyers and sovereignty legislation, while citizens lack exit from atmospheric commons and the federation.
 *
 * DIRECTIONALITY LOGIC:
 *   Resource-rich provincial governments and the fossil fuel sector sit at the beneficiary end: the constraint subsidizes their autonomy and shields them from federal regulation. The federal government and climate-vulnerable citizens sit at the target end: they pay through blocked policy capacity and unpriced externalities. Indigenous nations are structurally excluded from the binary, carrying high directionality without being addressed by the constraint's logic at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â securing provincial resource ownership against federal expropriation â is contested. Some argue it is dead because federal expropriation is no longer a credible threat; others argue it remains live because federal climate policy functions as de facto expropriation. The classification as tangled_rope prevents mislabeling: the arrangement retains genuine coordination value in resource governance (clear jurisdictional assignment prevents inter-provincial conflict), but it has accumulated substantial extractive function as it blocks evolving federal policy needs. It is neither pure mountain (constitutional truth) nor pure snare (coordination story as pure cover), but a hybrid that coordinates some actors while extracting from others.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_text_vs_political_instrument,
    'Does s.92A inherently ground absolute provincial sovereignty, or is the absolute sovereignty reading a political construction layered onto a resource-management provision?',
    'Comparative constitutional analysis of how other federations manage resource ownership; historical records of the 1982 negotiation intent; systematic review of court rulings on whether s.92A operates as a shield or a sword.',
    'If the provision was intended as ordinary resource management, the reading is extractive political construction; if it inherently reserves absolute sovereignty, it moves toward constitutional mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_text_vs_political_instrument, conceptual, 'Whether the absolute sovereignty claim is textually grounded or politically constructed.').

omega_variable(
    indigenous_title_as_third_order,
    'Does the persistence of unceded indigenous title render the provincial-federal sovereignty binary over resources structurally incomplete?',
    'Supreme Court rulings on duty to consult and consent; treaty negotiations that reallocate resource authority; recognition of indigenous jurisdiction in resource governance.',
    'If indigenous title is recognized as a third sovereignty order, the resource_sovereignty_primacy reading collapses into an exclusionary structure that suppresses a foundational party, shifting classification toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_title_as_third_order, empirical, 'Whether indigenous title undermines the provincial-federal binary.').

omega_variable(
    federalism_coordination_cost,
    'Is provincial resource ownership separable from the broader constitutional order, or does the absolute sovereignty claim necessarily erode the coordination function of Canadian federalism?',
    'Natural experiment from policy domains where federal and provincial coordination succeeded despite s.92A; comparative analysis of federations with stronger concurrent jurisdiction over resources.',
    'If inseparable, part of the measured extraction is the necessary price of the coordination; if separable, the absolute sovereignty claim is pure extraction riding on a real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federalism_coordination_cost, conceptual, 'Whether coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(provincial_sovereignty_boundary__resource_sovereignty_primacy, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prov_tr_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 0, 0.18).
narrative_ontology:measurement(prov_tr_t8, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 8, 0.22).
narrative_ontology:measurement(prov_tr_t16, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 16, 0.28).
narrative_ontology:measurement(prov_tr_t24, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 24, 0.34).
narrative_ontology:measurement(prov_tr_t32, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 32, 0.4).
narrative_ontology:measurement(prov_tr_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(prov_be_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(prov_be_t8, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(prov_be_t16, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(prov_be_t24, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(prov_be_t32, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(prov_be_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prov_su_t0, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(prov_su_t8, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(prov_su_t16, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(prov_su_t24, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(prov_su_t32, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(prov_su_t40, provincial_sovereignty_boundary__resource_sovereignty_primacy, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
