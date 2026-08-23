% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate response frames protection
 *   infrastructure as the immediate moral and practical imperative, accepting
 *   that temperature rise beyond 1.5°C is baked in and that mitigation alone
 *   cannot prevent near-term harm to vulnerable populations. This reading
 *   structures the global climate finance architecture around a $540B/year
 *   adaptation target, but the burden falls disproportionately on developing
 *   nations with limited fiscal capacity, creating a $350B annual financing
 *   gap. Wealthy nations benefit by substituting adaptation finance for
 *   deeper domestic emissions cuts while retaining capital mobility. The
 *   constraint is a tangled rope: it coordinates genuine protection for the
 *   exposed (coordination function) while extracting fiscal capacity from the
 *   Global South and locking in higher future warming costs for coming
 *   generations (asymmetric extraction). Active enforcement through MDB
 *   conditionality and UNFCCC reporting requirements sustains the
 *   arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.42).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, 'ee5f37cc-1764-45ca-8635-e93ee7d88542').
narrative_ontology:cs_kernel_codification('ee5f37cc-1764-45ca-8635-e93ee7d88542', formalized).
narrative_ontology:cs_authority_grounding('ee5f37cc-1764-45ca-8635-e93ee7d88542', lineage).
narrative_ontology:cs_interpretation_layer_present('ee5f37cc-1764-45ca-8635-e93ee7d88542').
narrative_ontology:cs_reading_relation('ee5f37cc-1764-45ca-8635-e93ee7d88542', climate_response_action__mitigation_priority, forecloses).
narrative_ontology:cs_reading_relation('ee5f37cc-1764-45ca-8635-e93ee7d88542', climate_response_action__degrowth_transformation, coexists_with).
narrative_ontology:cs_axiom('ee5f37cc-1764-45ca-8635-e93ee7d88542', foundational, temperature_rise_above_2c_inevitable).
narrative_ontology:cs_axiom_status(temperature_rise_above_2c_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('ee5f37cc-1764-45ca-8635-e93ee7d88542', temperature_rise_above_2c_inevitable, empirically_contingent).
narrative_ontology:cs_axiom('ee5f37cc-1764-45ca-8635-e93ee7d88542', foundational, protection_of_vulnerable_justifies_adaptation_investment).
narrative_ontology:cs_axiom_status(protection_of_vulnerable_justifies_adaptation_investment, holdable).
narrative_ontology:cs_axiom_grounding('ee5f37cc-1764-45ca-8635-e93ee7d88542', protection_of_vulnerable_justifies_adaptation_investment, deontological).
narrative_ontology:cs_reference_frame('ee5f37cc-1764-45ca-8635-e93ee7d88542', paris_agreement_adaptation_goal).
narrative_ontology:cs_drift_state('ee5f37cc-1764-45ca-8635-e93ee7d88542', contemporary_finance_gap_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ee5f37cc-1764-45ca-8635-e93ee7d88542', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, vulnerable_populations).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, wealthy_nations_avoiding_mitigation).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, climate_resilience_imperative).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, common_but_differentiated_responsibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Populations in low-lying coastal zones, arid regions, and heat-vulnerable urban centers who receive adaptation investments (sea walls, cooling centers, drought-resistant agriculture) but have no structural power to demand them and cannot exit the climate exposure.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, global).

% Governments of the Global South facing $540B annual adaptation need with limited fiscal capacity, forced to divert development budgets to resilience or borrow at premium rates, creating a $350B financing gap that deepens debt distress and crowds out health, education, and mitigation investment.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nations, payer,
    moderate, generational, constrained, national).

% Industrialized nations that shape adaptation finance rules through UNFCCC and MDB governance, benefit by substituting adaptation payments for deeper domestic emissions cuts, and retain capital mobility to avoid climate costs — their mitigation pledges remain unmet while adaptation commitments are also underfunded.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, wealthy_nations_avoiding_mitigation, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, wealthy_nations_avoiding_mitigation, beneficiary).

% Generations who inherit the higher warming trajectory accepted by this reading (2.5–3°C+), bearing compounding adaptation costs, irreversible ecosystem loss, and reduced habitable land — they have no voice in current negotiations and cannot exit the climatic trajectory locked in by present choices.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations, payer,
    powerless, civilizational, trapped, universal).

% World Bank, IMF, regional development banks that design and administer adaptation finance facilities, set loan terms, and enforce conditionality — they profit from lending flows and maintain institutional relevance through the adaptation agenda while bearing no climate risk themselves.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Civil society networks demanding reparative finance, debt cancellation, and mitigation-first pathways — they are formally admitted as observers to COP negotiations but structurally excluded from finance facility design rooms where allocation rules are written.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_justice_movements, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes capital and engineering capacity to protect human settlements and ecosystems from climate impacts that are already locked in by historical emissions, solving the immediate physical protection problem for exposed populations.
% TRANSFER_FUNCTION: Moves $540B annually from public budgets and concessional finance (disproportionately extracted from Global South fiscal space via debt service and opportunity cost) into physical resilience infrastructure, with a persistent $350B gap leaving the most exposed unprotected.
% ABSENT_VOICES: Climate justice movements, Indigenous peoples in frontline territories, and future generations are structurally excluded from the finance architecture design process; they would demand grant-based reparative finance, technology transfer without IP barriers, and mitigation prioritization but have no seat at the MDB/UNFCCC finance facility negotiating tables.
% DISAPPEARANCE_RATIONALE: If the adaptation-priority framework vanished overnight, the $540B/year protection architecture would dissolve — sea walls wouldn't be built, early warning systems would degrade, agricultural adaptation would stall — and vulnerable populations would face unmanaged climate impacts. The financing gap would cease to be a policy metric but the physical exposure would remain.
% FOUNDING_PROBLEM: By 2015 it was clear that mitigation pledges were insufficient to hold warming below 2°C, near-term impacts (extreme heat, flooding, crop failure) were already devastating vulnerable populations, and a dedicated adaptation finance architecture was needed to protect lives in the warming already committed.
% FOUNDING_PROBLEM_CORROBORATION: IPCC AR6 WGII (2022) confirms accelerating impacts and adaptation gap; UNFCCC Standing Committee on Finance reports document the persistent $350B shortfall; OECD climate finance tracking shows only 21% of finance reaches adaptation and mostly as loans. These sources sit outside the wealthy-nation beneficiary set.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) reflects the structural transfer: developing nations pay via debt service and diverted development budgets for protection that wealthy nations also benefit from (avoided mitigation costs, continued market access). Suppression (0.42) is moderate — not direct coercion but structural: developing nations cannot exit the climate exposure, cannot access capital on fair terms, and face conditionality that enforces the adaptation-first framing. Theater ratio (0.38) captures the gap between pledged finance ($100B/year goal, adaptation doubling promise) and delivered grants; the performative commitments sustain legitimacy while the financing gap widens. Accessibility collapse (0.45) is partial — mitigation and degrowth alternatives exist politically but are marginalized in finance architecture. Resistance (0.52) reflects growing climate justice demands, debt reform movements (Bridgetown Initiative), and loss-and-damage negotiations.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (developing_nations, future_generations) experience this as a snare-like extraction: they pay for protection they cannot afford while the coordination benefit is real but incomplete. The agenda_setter seats (wealthy_nations, IFIs) experience it as a rope: a coordination mechanism they designed that solves a genuine problem (protection) while conveniently substituting for harder mitigation. The engine will compute this divergence from the power/exit/beneficiary structure — the claimed tangled_rope reflects the author's structural assessment that both coordination and extraction are real and inseparable in this arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations are beneficiaries (d ~0.15) — they receive protection but have no power to shape it. Developing nations are payers (d ~0.85) — they bear fiscal burden, cannot exit exposure, and face identity_locked dynamics (sovereignty narrative prevents exit from UNFCCC process). Wealthy nations are agenda_setters who also benefit (d ~0.10) — they write the rules, avoid mitigation costs, and arbitrage capital. Future generations are trapped payers (d ~1.0) — zero exit, zero voice, full intergenerational transfer. IFIs are agenda_setters with arbitrage exit (d ~0.10). Climate justice movements are excluded — their structural position is observer with constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting populations from locked-in warming) remains live — impacts are accelerating. But the arrangement has developed mandatrophic drift: the adaptation finance architecture now serves to legitimize continued emissions by wealthy nations (substitution effect) and to maintain IFI lending portfolios, while the financing gap ensures the coordination function is only partially delivered. The theater ratio rise (0.22→0.38) tracks this drift: more performative pledging, less delivery. The constraint is not yet a piton — the coordination function is still actively needed — but the extraction-to-coordination ratio is worsening.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_mitigation_substitution,
    'Does adaptation finance structurally substitute for mitigation ambition in wealthy nations, or are they independently determined?',
    'Counterfactual analysis: if adaptation finance were fully delivered as grants, would wealthy nation NDCs strengthen? Econometric study of mitigation policy conditional on adaptation finance flows.',
    'If substitution is structural, the adaptation-priority reading functions as a mitigation-delay mechanism — its extraction includes the climate damages from forgone mitigation. This would increase effective extractiveness for future_generations and developing_nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_mitigation_substitution, empirical, 'Whether the adaptation-priority arrangement crowds out mitigation in practice.').

omega_variable(
    financing_gap_mechanism,
    'Is the $350B adaptation financing gap a temporary shortfall or a structural feature of the North-South fiscal architecture?',
    'Historical analysis of climate finance delivery vs. pledges since 2009; assessment of MDB capital adequacy and shareholder willingness for grant-equivalent scaling.',
    'If structural, the gap is not a bug but a feature — the arrangement coordinates protection rhetoric while extracting fiscal space from the Global South. This would confirm tangled_rope classification and increase extractiveness for developing_nations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(financing_gap_mechanism, empirical, 'Whether the adaptation finance gap is structural or contingent.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the adaptation_priority reading''s core premise (temperature rise inevitable) logically foreclose the mitigation_priority reading within a single policy framework?',
    'Formal analysis of policy framework compatibility: can a single national strategy simultaneously treat 2°C as achievable (mitigation_priority) and treat >2°C as the planning baseline (adaptation_priority)?',
    'If forecloses, the readings cannot coexist in one government''s strategy — the kernel would fracture into mutually exclusive policy regimes. If coexists_with, they operate as parallel tracks (current reality). This determines the reading_relation type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical compatibility of adaptation_priority and mitigation_priority within one framework.').

omega_variable(
    protection_disparity_permanence,
    'Will adaptation investment under this reading reduce or entrench protection disparities between wealthy and vulnerable populations?',
    'Longitudinal tracking of adaptation benefit distribution: do resilience investments reach the most exposed, or do they flow to asset-rich areas (coastal property defense, urban cooling for affluent districts)?',
    'If disparities widen, the coordination function is captured — the constraint becomes snare-like for vulnerable_populations. If disparities narrow, the coordination function is genuine. This affects the beneficiary/victim boundary for vulnerable_populations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protection_disparity_permanence, empirical, 'Whether adaptation finance reduces or reproduces vulnerability gradients.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 2015, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cra_ap_tr_t2015, climate_response_action__adaptation_priority, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(cra_ap_tr_t2018, climate_response_action__adaptation_priority, theater_ratio, 2018, 0.28).
narrative_ontology:measurement(cra_ap_tr_t2021, climate_response_action__adaptation_priority, theater_ratio, 2021, 0.32).
narrative_ontology:measurement(cra_ap_tr_t2024, climate_response_action__adaptation_priority, theater_ratio, 2024, 0.35).
narrative_ontology:measurement(cra_ap_tr_t2027, climate_response_action__adaptation_priority, theater_ratio, 2027, 0.37).
narrative_ontology:measurement(cra_ap_tr_t2030, climate_response_action__adaptation_priority, theater_ratio, 2030, 0.38).

% Extraction over time
narrative_ontology:measurement(cra_ap_be_t2015, climate_response_action__adaptation_priority, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement(cra_ap_be_t2018, climate_response_action__adaptation_priority, base_extractiveness, 2018, 0.52).
narrative_ontology:measurement(cra_ap_be_t2021, climate_response_action__adaptation_priority, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(cra_ap_be_t2024, climate_response_action__adaptation_priority, base_extractiveness, 2024, 0.63).
narrative_ontology:measurement(cra_ap_be_t2027, climate_response_action__adaptation_priority, base_extractiveness, 2027, 0.66).
narrative_ontology:measurement(cra_ap_be_t2030, climate_response_action__adaptation_priority, base_extractiveness, 2030, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cra_ap_su_t2015, climate_response_action__adaptation_priority, suppression_requirement, 2015, 0.25).
narrative_ontology:measurement(cra_ap_su_t2018, climate_response_action__adaptation_priority, suppression_requirement, 2018, 0.31).
narrative_ontology:measurement(cra_ap_su_t2021, climate_response_action__adaptation_priority, suppression_requirement, 2021, 0.36).
narrative_ontology:measurement(cra_ap_su_t2024, climate_response_action__adaptation_priority, suppression_requirement, 2024, 0.39).
narrative_ontology:measurement(cra_ap_su_t2027, climate_response_action__adaptation_priority, suppression_requirement, 2027, 0.41).
narrative_ontology:measurement(cra_ap_su_t2030, climate_response_action__adaptation_priority, suppression_requirement, 2030, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.12).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, global_climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, sovereign_debt_climate_clause).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_response_action kernel. The mitigation_priority reading (emissions-first, carbon markets, growth-compatible) and degrowth_transformation reading (structural transformation, sufficiency, reduced throughput) are sibling constraints with different ε, different beneficiary/victim structures, and different claimed types. All three share the kernel 'climate response requires...' but instantiate different constraints. The adaptation_priority reading influences mitigation_priority by diverting political capital and finance toward adaptation; it coexists_with degrowth_transformation as competing policy imaginaries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__adaptation_priority, institutional, 0.1).
constraint_indexing:directionality_override(climate_response_action__adaptation_priority, moderate, 0.85).
constraint_indexing:directionality_override(climate_response_action__adaptation_priority, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
