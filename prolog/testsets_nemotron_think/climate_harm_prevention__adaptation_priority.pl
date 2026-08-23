% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_harm_prevention__adaptation_priority, []).

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
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation Priority as Legitimate Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the 'adaptation priority' reading of the
 *   contested kernel 'climate harm prevention.' It asserts that legitimate
 *   climate response must prioritize near-term resilience building for
 *   currently vulnerable populations because rapid mitigation is politically
 *   and economically infeasible within existing growth frameworks, thereby
 *   accepting a higher warming trajectory (2.5-3.5°C). The constraint
 *   structures global climate finance, UNFCCC negotiation priorities,
 *   national adaptation planning, and the moral architecture of climate
 *   obligation. It presents itself as pragmatic humanitarianism — protecting
 *   the living — while functionally transferring the costs of continued
 *   emissions to future generations and the geographically disadvantaged.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.45).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation Priority as Legitimate Climate Response").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '74f841e5-6113-4357-95f9-6a9a804ec609').
narrative_ontology:cs_kernel_codification('74f841e5-6113-4357-95f9-6a9a804ec609', distributed).
narrative_ontology:cs_authority_grounding('74f841e5-6113-4357-95f9-6a9a804ec609', practice).
narrative_ontology:cs_interpretation_layer_present('74f841e5-6113-4357-95f9-6a9a804ec609').
narrative_ontology:cs_reading_relation('74f841e5-6113-4357-95f9-6a9a804ec609', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('74f841e5-6113-4357-95f9-6a9a804ec609', climate_harm_prevention__degrowth_reading, coexists_with).
narrative_ontology:cs_axiom('74f841e5-6113-4357-95f9-6a9a804ec609', foundational, adaptation_primacy_over_mitigation).
narrative_ontology:cs_axiom_status(adaptation_primacy_over_mitigation, holdable).
narrative_ontology:cs_axiom_grounding('74f841e5-6113-4357-95f9-6a9a804ec609', adaptation_primacy_over_mitigation, conventional).
narrative_ontology:cs_axiom('74f841e5-6113-4357-95f9-6a9a804ec609', foundational, political_feasibility_as_normative_constraint).
narrative_ontology:cs_axiom_status(political_feasibility_as_normative_constraint, holdable).
narrative_ontology:cs_axiom_grounding('74f841e5-6113-4357-95f9-6a9a804ec609', political_feasibility_as_normative_constraint, conventional).
narrative_ontology:cs_reference_frame('74f841e5-6113-4357-95f9-6a9a804ec609', unfccc_original_mitigation_primacy).
narrative_ontology:cs_drift_state('74f841e5-6113-4357-95f9-6a9a804ec609', post_paris_agreement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('74f841e5-6113-4357-95f9-6a9a804ec609', '2026-07-25T14:30:00Z').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, present_vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, fossil_fuel_interests).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, national_governments).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, adaptation_primacy_doctrine).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, political_realism_in_climate_policy).
narrative_ontology:constraint_vindicates(climate_harm_prevention__adaptation_priority, mitigation_infeasibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations currently experiencing severe climate impacts (flooding, heat mortality, crop failure, displacement) who receive prioritized adaptation funding, infrastructure investment, and disaster response. Their vulnerability is immediate and documented; they cannot wait for mitigation effects that materialize decades later. Exit from vulnerability requires adaptation resources that this constraint directs toward them.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, present_vulnerable_populations, beneficiary,
    moderate, biographical, constrained, global).

% All people born after the policy commitment period who inherit a higher warming trajectory (2.5-3.5°C vs 1.5-2°C) with compounding climate damages, reduced adaptation options, and irreversible ecosystem losses. They bear the residual costs of accepted higher warming: more extreme events, sea-level rise commitment, biodiversity loss, and the mitigation burden deferred to them. They have no voice in current negotiations and no exit from the climatic trajectory locked in by present decisions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, future_generations, payer,
    powerless, generational, trapped, global).

% Primarily Global South nations and regions with limited fiscal space, weak institutions, and high climate exposure that receive insufficient adaptation finance under the prioritization framework. They bear disproportionate residual climate costs because adaptation funding is both inadequate and skewed toward higher-capacity recipients, while the higher warming trajectory hits them hardest. Their exit options are blocked by structural dependency on international finance and technology transfer that the constraint does not guarantee.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions, payer,
    powerless, generational, trapped, continental).

% Sovereign states that set climate policy priorities through NDCs, national adaptation plans, and budget allocations. They benefit from the political feasibility of adaptation (visible, local, credit-claimable) versus mitigation (diffuse, long-term, politically costly). They can arbitrage between adaptation and mitigation framing in international negotiations to maximize domestic political survival and access to climate finance. Their enforcement of the constraint occurs through domestic policy and international bargaining positions.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, national_governments, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_harm_prevention__adaptation_priority, national_governments, beneficiary).

% Oil, gas, and coal incumbents and their financial backers who benefit structurally from the constraint's acceptance of higher warming and deprioritization of rapid decarbonization. The adaptation-priority framing legitimizes continued extraction by presenting mitigation as infeasible, reducing regulatory pressure and stranded-asset risk. They exercise exit through asset diversification and political influence, but their core business model depends on the constraint persisting.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, fossil_fuel_interests, beneficiary,
    powerful, biographical, arbitrage, global).

% Multilateral development banks, climate funds (GCF, Adaptation Fund), bilateral agencies, and private adaptation finance vehicles that channel resources to adaptation projects. They collect management fees, expand institutional mandates, and gain political relevance from the front-loaded adaptation expenditure. Their exit is mobile — they can pivot portfolios — but their growth trajectory depends on the constraint's prioritization of adaptation over mitigation.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, adaptation_finance_institutions, beneficiary,
    organized, biographical, mobile, global).

% Transnational civil society networks (especially Global South-led) that frame climate action as reparative justice requiring mitigation primacy, historical responsibility, and equitable burden-sharing. They are structurally excluded from the adaptation-priority consensus because their demand for mitigation-centered response contradicts the political feasibility premise. Their exit is constrained — they can protest, litigate, and mobilize but cannot set the negotiation agenda.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, climate_justice_movements, excluded,
    organized, generational, constrained, global).

% The scientific assessment body that documents both the adaptation limits at higher warming levels and the mitigation pathways foreclosed by delay. They observe the constraint's operation through the divergence between WGII (impacts/adaptation) and WGIII (mitigation) findings, and through the carbon budget accounting that makes the trade-off visible. Their analytical exit is unrestricted but their influence on the constraint is mediated through the policy-science interface.
narrative_ontology:constraint_stakeholder(climate_harm_prevention__adaptation_priority, ipcc_scientists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Building near-term resilience for populations currently vulnerable to climate impacts, solving the immediate protection coordination problem that mitigation cannot address on relevant timescales — coordinating finance, technology, governance, and local knowledge for disaster risk reduction, infrastructure adaptation, and livelihood protection.
% TRANSFER_FUNCTION: Moves financial, technical, and political resources from long-term mitigation investment and future generations' carbon budget to present-day adaptation infrastructure and disaster response, concentrating benefits on currently vulnerable populations while distributing the costs of higher warming to future generations and low-adaptation-capacity regions.
% ABSENT_VOICES: Future generations (temporally excluded by definition), low-adaptation-capacity regions in the Global South (geopolitically marginalized in UNFCCC consensus processes), species and ecosystems with no adaptive capacity (ontologically excluded from human-centered resilience framing), and frontline communities who reject adaptation-only framing as surrender (politically excluded from legitimate discourse).
% DISAPPEARANCE_RATIONALE: If the adaptation-priority constraint vanished overnight, the UNFCCC architecture would reorient toward mitigation primacy: climate finance would shift from adaptation to decarbonization, NDCs would be benchmarked against 1.5°C pathways, carbon budgets would regain normative force, and the political economy of fossil fuel phase-out would become the central organizing problem. Global climate governance would reorganize around the mitigation timeline rather than the adaptation timeline.
% FOUNDING_PROBLEM: The political impossibility of binding global mitigation agreements after the Kyoto Protocol's failure and Copenhagen collapse (2009), combined with the immediate and escalating climate mortality and morbidity burden on vulnerable populations who cannot wait for mitigation effects that materialize only after decades of emissions reduction.
% FOUNDING_PROBLEM_CORROBORATION: IPCC WGII AR6 (outside beneficiary set) documents current vulnerability and adaptation limits; UNFCCC negotiation records (1995-2015) document repeated mitigation agreement failures; Global South climate justice analyses (e.g., LDC Group submissions, Climate Vulnerable Forum declarations) attest the political infeasibility narrative is constructed by major emitter inaction, not structural necessity; IEA and IPCC WGIII scenarios demonstrate technically feasible mitigation pathways that were politically rejected.
narrative_ontology:disappearance_verdict(climate_harm_prevention__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_harm_prevention__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_harm_prevention__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_harm_prevention__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_harm_prevention__adaptation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the constraint legitimizes a carbon budget overspend that transfers climate damages intergenerationally and geographically. Suppression (0.45) is moderate: the 'political infeasibility' framing actively marginalizes mitigation-centered alternatives in negotiation spaces and finance criteria, but does not legally ban them. Theater ratio (0.38) reflects genuine adaptation delivery alongside performative projects that serve donor visibility over vulnerability reduction. Accessibility collapse (0.55) captures how the feasibility premise closes off mitigation pathways discursively. Resistance (0.48) comes from climate justice movements, vulnerable nation blocs, and scientific assessments documenting adaptation limits. The measurement grid shows extractiveness and theater rising together post-Paris (2015), while suppression requirement plateaus as the constraint becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (national governments), the constraint appears as necessary triage — the only feasible way to discharge duty of care to current citizens. From the payer seats (future generations, low-capacity regions), it appears as intergenerational and interregional theft — a political choice disguised as physical necessity. From the analytical seat (IPCC), it appears as a trajectory choice with documented consequences: adaptation limits breach at 2°C+, residual damages escalate nonlinearly, and the mitigation window narrows. The engine computes this divergence from the structural data; the claimed type (tangled_rope) acknowledges both the genuine coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Present vulnerable populations are structural beneficiaries (d ~ 0.15) — they receive prioritized resources and the constraint's moral justification centers them. Future generations are full targets (d ~ 0.95) — trapped, no voice, bear compounding damages. Low-adaptation-capacity regions are near-full targets (d ~ 0.85) — trapped by structural dependency, marginalized in governance. National governments are agenda-setters with arbitrage exit (d ~ 0.25) — they set the rules and can pivot framing. Fossil fuel interests are incidental beneficiaries (d ~ 0.10) — they gain regulatory relief without administering the constraint. Adaptation finance institutions are beneficiaries with mobile exit (d ~ 0.20). Climate justice movements are excluded (d ~ 0.70) — they bear epistemic suppression costs. IPCC scientists are analytical observers (d ~ 0.50).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by acknowledging its genuine coordination function (protecting the currently vulnerable) while exposing its extraction structure (accepting higher warming that disproportionately harms those with least adaptive capacity). The mandatrophy risk is that the 'political infeasibility' premise becomes self-fulfilling: by treating mitigation as infeasible, the constraint reduces mitigation investment, which makes mitigation harder, which reinforces the premise. The founding problem (mitigation negotiation failure) is contested — corroborated by negotiation records but disputed by feasibility studies showing technical/economic viability. The constraint persists not because the founding problem is solved, but because the premise itself suppresses the alternatives that would solve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mitigation_feasibility_boundary,
    'Is rapid mitigation truly politically/economically infeasible, or is the infeasibility claim a self-fulfilling prophecy produced by the constraint''s own suppression of mitigation alternatives?',
    'Counterfactual assessment of mitigation pathways not pursued: compare actual post-2015 investment flows with IPCC WGIII feasible pathways; test whether political feasibility changes when mitigation is framed as adaptation-enabling (avoided damages) rather than adaptation-competing.',
    'If infeasibility is endogenous to the constraint, the extraction is manufactured — the constraint creates the victims it claims to protect from an unstoppable future. Reclassification toward snare. If exogenous, the tangled_rope classification holds: genuine coordination under hard constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mitigation_feasibility_boundary, empirical, 'Whether the political feasibility premise is a structural fact or a constraint-produced artifact.').

omega_variable(
    adaptation_mitigation_substitutability,
    'Can adaptation substantively substitute for mitigation at 2.5-3.5°C warming, or does the constraint assume substitutability that biophysics contradicts?',
    'IPCC WGII AR7 assessment of adaptation limits at higher warming levels; empirical tracking of adaptation effectiveness decay as warming exceeds 2°C; analysis of residual damages after maximum feasible adaptation.',
    'If adaptation limits are hard and near-term, the constraint''s coordination function collapses — it cannot deliver the protection it promises. The extraction becomes pure (snare). If substitutability holds partially, tangled_rope stands with updated extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_mitigation_substitutability, empirical, 'Biophysical substitutability between adaptation and mitigation at the warming trajectory this constraint accepts.').

omega_variable(
    intergenerational_discount_rate,
    'What implicit discount rate on future welfare justifies front-loading adaptation expenditure while accepting higher warming damages?',
    'Formal intergenerational welfare analysis comparing the constraint''s implied discount rate with ethical benchmarks (Ramsey rule, zero pure time preference, equity-weighting); sensitivity of optimal policy to discount rate assumptions in integrated assessment models.',
    'If the implied rate is ethically indefensible (high pure time preference), the constraint''s moral justification fails — it is extraction masked as pragmatism. If defensible under plausible ethics, the tangled_rope trade-off is normatively grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, conceptual, 'The normative weight the constraint assigns to present vs. future welfare through its expenditure and warming choices.').

omega_variable(
    adaptation_finance_capture,
    'Does the adaptation finance architecture deliver resources to the most vulnerable, or does it capture adaptation funding for middle-income implementers and consultant networks?',
    'Tracking of adaptation finance flows by recipient vulnerability quintile, implementing entity type, and project evaluative rigor; comparison with climate vulnerability indices.',
    'If finance capture is systematic, the beneficiary structure shifts: present_vulnerable_populations become nominal beneficiaries while adaptation_finance_institutions and national_governments become real beneficiaries. Extraction profile worsens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(adaptation_finance_capture, empirical, 'Whether the adaptation priority''s resource allocation matches its stated beneficiary logic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 1990, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_adapt_priority_tr_t1990, climate_harm_prevention__adaptation_priority, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(climate_adapt_priority_tr_t2000, climate_harm_prevention__adaptation_priority, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(climate_adapt_priority_tr_t2010, climate_harm_prevention__adaptation_priority, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(climate_adapt_priority_tr_t2015, climate_harm_prevention__adaptation_priority, theater_ratio, 2015, 0.33).
narrative_ontology:measurement(climate_adapt_priority_tr_t2020, climate_harm_prevention__adaptation_priority, theater_ratio, 2020, 0.36).
narrative_ontology:measurement(climate_adapt_priority_tr_t2025, climate_harm_prevention__adaptation_priority, theater_ratio, 2025, 0.38).
narrative_ontology:measurement(climate_adapt_priority_tr_t2030, climate_harm_prevention__adaptation_priority, theater_ratio, 2030, 0.38).

% Extraction over time
narrative_ontology:measurement(climate_adapt_priority_be_t1990, climate_harm_prevention__adaptation_priority, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(climate_adapt_priority_be_t2000, climate_harm_prevention__adaptation_priority, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(climate_adapt_priority_be_t2010, climate_harm_prevention__adaptation_priority, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(climate_adapt_priority_be_t2015, climate_harm_prevention__adaptation_priority, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(climate_adapt_priority_be_t2020, climate_harm_prevention__adaptation_priority, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(climate_adapt_priority_be_t2025, climate_harm_prevention__adaptation_priority, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement(climate_adapt_priority_be_t2030, climate_harm_prevention__adaptation_priority, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_adapt_priority_su_t1990, climate_harm_prevention__adaptation_priority, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(climate_adapt_priority_su_t2000, climate_harm_prevention__adaptation_priority, suppression_requirement, 2000, 0.32).
narrative_ontology:measurement(climate_adapt_priority_su_t2010, climate_harm_prevention__adaptation_priority, suppression_requirement, 2010, 0.38).
narrative_ontology:measurement(climate_adapt_priority_su_t2015, climate_harm_prevention__adaptation_priority, suppression_requirement, 2015, 0.42).
narrative_ontology:measurement(climate_adapt_priority_su_t2020, climate_harm_prevention__adaptation_priority, suppression_requirement, 2020, 0.44).
narrative_ontology:measurement(climate_adapt_priority_su_t2025, climate_harm_prevention__adaptation_priority, suppression_requirement, 2025, 0.45).
narrative_ontology:measurement(climate_adapt_priority_su_t2030, climate_harm_prevention__adaptation_priority, suppression_requirement, 2030, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_harm_prevention__adaptation_priority, 0.18).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'climate_harm_prevention' into three structurally distinct readings with divergent ε values. adaptation_priority (ε≈0.68) accepts higher warming to fund present adaptation; mitigation_priority (ε≈0.25) invests in decarbonization to limit warming; degrowth_reading (ε≈0.45) contracts Northern throughput to enable Southern development within carbon budget. The adaptation_priority reading influences the others by dominating climate finance architecture and UNFCCC agenda-setting, making mitigation_priority resource-starved and degrowth_reading politically marginalized.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, powerful, 0.1).
constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
