% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint is the adaptation_priority reading of the contested
 *   climate_response_action kernel. It treats temperature rise as inevitable
 *   and prioritizes capital-intensive resilience and adaptive capacity,
 *   nominally protecting vulnerable populations. The structural arrangement
 *   requires approximately $540B annually for universal protection, but the
 *   standing architecture generates a $350B North-South financing gap,
 *   burdens developing nations with limited fiscal capacity, perpetuates
 *   inequality through protection disparities, and accepts higher future
 *   warming costs by deferring mitigation. Sibling readings include
 *   mitigation_priority (emissions reduction through technological
 *   innovation) and degrowth_transformation (structural economic
 *   transformation rejecting GDP growth). This story authors only the
 *   adaptation_priority reading as a clean epsilon-invariant constraint.
 *
 * KEY AGENTS:
 *   - developed_nation_governments: Primary agenda-setter and beneficiary (institutional/arbitrage) â set finance terms, defer mitigation liability
 *   - multilateral_development_banks: Secondary agenda-setter and beneficiary (institutional/arbitrage) â administer funds, expand mandates, collect fees
 *   - resilience_infrastructure_sector: Primary concentrated beneficiary (organized/mobile) â captures contracts from adaptation capital flows
 *   - developing_nation_governments: Primary payer (moderate/constrained) â bear debt and conditionality for incomplete protection
 *   - unprotected_vulnerable_populations: Target population and payer (powerless/trapped/local) â bear climate costs and protection disparities
 *   - future_generations: Diffuse payer (powerless/trapped/global) â inherit deferred warming costs
 *   - climate_justice_movements: Excluded voice (organized/constrained) â advocate for reparative alternatives but lack decision-making power
 *   - climate_finance_researchers: Analytical observer (analytical/analytical) â independently measure pledge gaps and distributional effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.72).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.62).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.72).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '0c67bc21-4e96-4b7c-b1f0-45782bacc1ce').
narrative_ontology:cs_kernel_codification('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', distributed).
narrative_ontology:cs_authority_grounding('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', distributed).
narrative_ontology:cs_reading_relation('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', foundational, temperature_rise_inevitable).
narrative_ontology:cs_axiom_status(temperature_rise_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', temperature_rise_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', foundational, capitalized_resilience_over_mitigation).
narrative_ontology:cs_axiom_status(capitalized_resilience_over_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', capitalized_resilience_over_mitigation, instrumental).
narrative_ontology:cs_reference_frame('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', capitalized_resilience_governance).
narrative_ontology:cs_drift_state('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', post_paris_finance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c67bc21-4e96-4b7c-b1f0-45782bacc1ce', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, developed_nation_governments).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, multilateral_development_banks).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, resilience_infrastructure_sector).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nation_governments).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, unprotected_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, climate_inevitability_thesis).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, adaptation_finance_architecture).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the terms of international climate finance in UNFCCC and GCF governance, pledging adaptation funds while avoiding binding mitigation liability and reparative obligations. Benefit from deferred decarbonization costs and expanded export markets for resilience technologies. Can renegotiate or abandon pledges without bearing the direct costs of adaptation failure in the Global South.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developed_nation_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, developed_nation_governments, beneficiary).

% Administer adaptation finance vehicles, define loan conditionality, and expand institutional mandates through climate-resilience programming. Collect administrative fees and interest while determining eligible project categories and disbursement timelines. Exit from unsuccessful portfolios is cushioned by preferred creditor status and callable capital from shareholder governments.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, multilateral_development_banks, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, multilateral_development_banks, beneficiary).

% Engineering, construction, and consultancy firms primarily based in developed nations that compete for contracts funded by multilateral and bilateral adaptation finance in the Global South. Revenue depends on the continuous flow of capital through the adaptation-priority architecture. Can redeploy expertise across jurisdictions as funding priorities shift.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, resilience_infrastructure_sector, beneficiary,
    organized, biographical, mobile, global).

% Must access international capital to protect populations from locked-in climate impacts that they did not primarily cause. Face loan conditionality, co-financing requirements, and slow disbursement that leaves projects incomplete. Rejecting the finance architecture means abandoning vulnerable populations to unmitigated harm; accepting it means sovereign debt accumulation and externally determined project priorities.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nation_governments, payer,
    moderate, generational, constrained, national).

% Rural, indigenous, and informal-settlement populations in climate-vulnerable regions who are nominally the priority beneficiaries of adaptation finance but are often bypassed by large infrastructure projects targeting urban economic assets. Bear the direct costs of climate impacts while receiving partial, delayed, or culturally inappropriate protection. Cannot exit their geographic or economic circumstances.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, unprotected_vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Inherit the higher warming costs accepted by the adaptation-priority framing, as deferred mitigation leads to greater cumulative emissions and irreversible tipping points. Have no voice in current finance negotiations and cannot exit the climate trajectory chosen by present decision-makers.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, global).

% Advocate for grant-based unconditional adaptation support, climate reparations, and concurrent mitigation reductions. Are accorded procedural speaking roles in COP consultations but are structurally excluded from decision-making on finance architecture, conditionality design, and disbursement priorities.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_justice_movements, excluded,
    organized, generational, constrained, global).

% Measure the gap between adaptation finance pledges and actual disbursements, document protection disparities across regions and income levels, and model the warming implications of deferring mitigation. Neither collect rents from nor pay into the arrangement; provide independent empirical assessment of the constraint's distributional effects.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_finance_researchers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes capital to build physical resilience against climate impacts that are already locked in, protecting populations and infrastructure from current and near-term climate harms that cannot be avoided by mitigation alone.
% TRANSFER_FUNCTION: Moves capital and debt obligation from developing nation public balance sheets and vulnerable populations to multilateral finance institutions and developed nation-based resilience contractors, while deferring mitigation costs and warming consequences to future periods.
% ABSENT_VOICES: Climate justice movements advocating for mitigation-with-reparations, degrowth scholars arguing for reduced resource throughput, and fossil-fuel-dependent developing nations resisting adaptation conditionality are structurally underrepresented in adaptation-priority finance fora.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority financing architecture vanished overnight, developing nations would lose both the incomplete protection and the accumulating debt; multilateral banks would lose mandate expansion and fee streams; resilience contractors would lose a primary revenue channel; vulnerable populations would face unmitigated harm without the current partial shield. The political compact deferring mitigation would collapse and climate finance would reorganize around a different burden-sharing logic.
% FOUNDING_PROBLEM: Climate impacts are already causing loss and damage that vulnerable populations cannot adapt to without external capital; mitigation alone leaves current and near-term victims unprotected.
% FOUNDING_PROBLEM_CORROBORATION: The IPCC Working Group II attests to adaptation needs from an analytical seat outside the finance beneficiary set. Developing nation negotiators corroborate that the financing gap is real but dispute that the current arrangement solves it; climate justice movements attest the problem is live while the solution form is capture. No party outside the benefiting set unambiguously corroborates the current architecture as the necessary solution.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.72) because the arrangement moves substantial resources from developing nations and future generations to developed-nation institutions and contractors while delivering incomplete and unequal protection. Suppression (0.62) reflects the structural suppression of grant-based and reparative alternatives through loan conditionality and creditor power. Theater ratio (0.48) captures the widening gap between pledged and disbursed finance, where performative announcements at COP meetings substitute for delivered protection. Accessibility collapse (0.58) indicates that once the MDB-led architecture is entrenched, unconditional or mitigation-first alternatives become harder to institutionalize. Resistance (0.55) reflects organized but structurally outgunned opposition from the G77 and climate justice movements. The temporal series run on one shared grid; all three tracked metrics show monotonic increase as the finance gap widened and conditionality hardened from 2009-2024.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (developed nation governments, MDBs) experience the arrangement as necessary coordination under capital scarcity and geopolitical constraint. The payer seats (developing nation governments, vulnerable populations, future generations) experience the same arrangement as asymmetric extraction that perpetuates North-South inequality. The engine computes this divergence from the structural data: same constraint, opposed directionality, different computed types per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (developed governments, MDBs, contractors) have low directionality â the constraint subsidizes their fiscal position, institutional mandates, or revenue streams. Payers (developing governments, vulnerable populations, future generations) have high directionality â the constraint extracts fiscal capacity, livelihood stability, and climatic security from them. Developing nation governments are moderate power with constrained exit; their directionality is structurally high but damped slightly by their organized voice in UNFCCC. Future generations are powerless, trapped, and global in scope; their directionality sits near the full-target end with severe scope amplification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not a pure snare because the coordination function is genuine: vulnerable populations do receive partial protection from locked-in climate impacts, and the capital scarcity problem is real. It is not a pure rope because the extraction is asymmetric and substantial: the same architecture that coordinates resilience also generates creditor power, protection disparities, and deferred warming costs. The tangled_rope classification captures the hybrid nature without adjudicating the coordination story as cover or as primary function. A snare classification would require the protection story to be purely cover; the temporal measurements show theater rising but not dominating, indicating residual real function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_kernel_adaptation,
    'This constraint is the adaptation_priority reading of the climate_response_action kernel. How would classification change if the degrowth_transformation reading (structural economic transformation rejecting GDP growth) were adopted as the governing framework?',
    'Compare the extractiveness and beneficiary/victim structure of the sibling reading''s constraint story; assess whether the same seats (MDBs, infrastructure sector) retain beneficiary status or become payers under a sufficiency-based allocation.',
    'If the degrowth reading reallocates beneficiary status away from capital-intensive contractors and toward sufficiency communities, the current arrangement would likely reclassify as snare or tangled_rope with inverted directionality for several seats; if the same seats remain beneficiaries, the kernel exhibits reading-invariant extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_adaptation, conceptual, 'Sibling reading comparison for kernel decomposition').

omega_variable(
    north_south_finance_gap_ambiguity,
    'Is the $350B North-South adaptation financing gap a structural feature of genuine coordination under global capital scarcity, or an artifact of extractive conditionality that could be closed by alternative governance?',
    'Counterfactual analysis of grant-based versus loan-based adaptation finance; sovereign debt relief outcomes where adaptation debt was cancelled; comparison of protection delivery rates under unconditional versus conditional finance.',
    'If grant-based or reparative finance closes the protection gap without extractive conditionality, the current arrangement is extractive overhead on a real coordination problem; if capital scarcity is genuinely binding at the global level, the measured extraction is largely coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_south_finance_gap_ambiguity, empirical, 'Whether the finance gap is structural or artifactual').

omega_variable(
    conditionality_as_enforcement,
    'Does the constraint''s persistence depend on voluntary participation by developing nations, or on structural coercion through debt conditionality and implied market-access threats?',
    'Examine developing nation opt-out or default rates and subsequent capital market consequences; compare disbursement patterns of fully voluntary climate funds versus MDB loan windows.',
    'If exit is punished by capital market exclusion or loss of other development finance, suppression is higher and the constraint leans toward snare; if participation is genuinely voluntary with viable alternatives, it leans toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_as_enforcement, empirical, 'Voluntary versus coerced participation in adaptation finance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_response_action__adaptation_priority, theater_ratio, 0, 0.2).
narrative_ontology:measurement(clim_tr_t3, climate_response_action__adaptation_priority, theater_ratio, 3, 0.28).
narrative_ontology:measurement(clim_tr_t6, climate_response_action__adaptation_priority, theater_ratio, 6, 0.35).
narrative_ontology:measurement(clim_tr_t9, climate_response_action__adaptation_priority, theater_ratio, 9, 0.4).
narrative_ontology:measurement(clim_tr_t12, climate_response_action__adaptation_priority, theater_ratio, 12, 0.44).
narrative_ontology:measurement(clim_tr_t15, climate_response_action__adaptation_priority, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_response_action__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_be_t3, climate_response_action__adaptation_priority, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(clim_be_t6, climate_response_action__adaptation_priority, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(clim_be_t9, climate_response_action__adaptation_priority, base_extractiveness, 9, 0.64).
narrative_ontology:measurement(clim_be_t12, climate_response_action__adaptation_priority, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(clim_be_t15, climate_response_action__adaptation_priority, base_extractiveness, 15, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_response_action__adaptation_priority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(clim_su_t3, climate_response_action__adaptation_priority, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(clim_su_t6, climate_response_action__adaptation_priority, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(clim_su_t9, climate_response_action__adaptation_priority, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(clim_su_t12, climate_response_action__adaptation_priority, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(clim_su_t15, climate_response_action__adaptation_priority, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).

% DUAL FORMULATION NOTE:
% The climate_response_action kernel decomposes into three structurally distinct constraints because the label 'climate response' conflates three different claims with different epsilon values, beneficiary/victim structures, and coordination functions. Adaptation_priority coordinates capital for resilience but extracts through debt and deferred warming; mitigation_priority coordinates emissions reduction but extracts through carbon market asymmetries; degrowth_transformations coordinates sufficiency but extracts through growth-dependent sector displacement. Each is a separate story linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
