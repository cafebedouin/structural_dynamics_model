% ============================================================================
% CONSTRAINT STORY: climate_mitigation_legitimacy__renewable_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_mitigation_legitimacy__renewable_primacy_reading, []).

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
 *   constraint_id: climate_mitigation_legitimacy__renewable_primacy_reading
 *   human_readable: Renewable Primacy in Decarbonization Policy
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint represents the policy and investment framework driven by
 *   the belief that 'renewables plus storage can achieve full decarbonization
 *   faster and cheaper than nuclear.' It is a reading of the broader
 *   'climate_mitigation_legitimacy' kernel, which encompasses various
 *   approaches to decarbonization. This reading actively shapes energy
 *   policy, directing resources and regulatory support towards renewables and
 *   storage, while simultaneously marginalizing nuclear power as an
 *   undesirable alternative due to perceived cost and speed disadvantages.
 *   The claim is presented as a coordination solution for climate action, but
 *   its implementation involves significant extraction from and suppression
 *   of competing technologies.
 *
 * KEY AGENTS:
 *   - policymakers_renewable_focus: Agenda setter (institutional/constrained)
 *   - renewable_energy_developers: Primary beneficiary (organized/arbitrage)
 *   - storage_technology_providers: Primary beneficiary (organized/arbitrage)
 *   - climate_advocates_renewable_path: Beneficiary/Advocate (organized/mobile)
 *   - nuclear_industry: Primary payer/victim (powerful/identity_locked)
 *   - fossil_fuel_industry: Payer/victim (institutional/trapped)
 *   - grid_operators_baseload_focus: Payer/victim (institutional/constrained)
 *   - general_public: Beneficiary/Payer (moderate/constrained)
 *   - baseload_necessity_advocates: Excluded (organized/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68).
domain_priors:suppression_score(climate_mitigation_legitimacy__renewable_primacy_reading, 0.75).
domain_priors:theater_ratio(climate_mitigation_legitimacy__renewable_primacy_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(climate_mitigation_legitimacy__renewable_primacy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_mitigation_legitimacy__renewable_primacy_reading, tangled_rope).
narrative_ontology:human_readable(climate_mitigation_legitimacy__renewable_primacy_reading, "Renewable Primacy in Decarbonization Policy").
narrative_ontology:topic_domain(climate_mitigation_legitimacy__renewable_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(climate_mitigation_legitimacy__renewable_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_mitigation_legitimacy__renewable_primacy_reading, 'ab380853-6848-4cba-ba1b-4420576ddcd1').
narrative_ontology:cs_kernel_codification('ab380853-6848-4cba-ba1b-4420576ddcd1', formalized).
narrative_ontology:cs_authority_grounding('ab380853-6848-4cba-ba1b-4420576ddcd1', expertise).
narrative_ontology:cs_interpretation_layer_present('ab380853-6848-4cba-ba1b-4420576ddcd1').
narrative_ontology:cs_reading_relation('ab380853-6848-4cba-ba1b-4420576ddcd1', climate_mitigation_legitimacy__baseload_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('ab380853-6848-4cba-ba1b-4420576ddcd1', climate_mitigation_legitimacy__portfolio_pragmatism_reading, forecloses).
narrative_ontology:cs_reading_relation('ab380853-6848-4cba-ba1b-4420576ddcd1', climate_mitigation_legitimacy__degrowth_sufficiency_reading, coexists_with).
narrative_ontology:cs_axiom('ab380853-6848-4cba-ba1b-4420576ddcd1', foundational, renewable_cost_declines_indefinitely).
narrative_ontology:cs_axiom_status(renewable_cost_declines_indefinitely, holdable).
narrative_ontology:cs_axiom_grounding('ab380853-6848-4cba-ba1b-4420576ddcd1', renewable_cost_declines_indefinitely, empirically_contingent).
narrative_ontology:cs_axiom('ab380853-6848-4cba-ba1b-4420576ddcd1', foundational, nuclear_inherently_slow_expensive).
narrative_ontology:cs_axiom_status(nuclear_inherently_slow_expensive, holdable).
narrative_ontology:cs_axiom_grounding('ab380853-6848-4cba-ba1b-4420576ddcd1', nuclear_inherently_slow_expensive, empirically_contingent).
narrative_ontology:cs_reference_frame('ab380853-6848-4cba-ba1b-4420576ddcd1', rapid_cost_effective_decarbonization).
narrative_ontology:cs_drift_state('ab380853-6848-4cba-ba1b-4420576ddcd1', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ab380853-6848-4cba-ba1b-4420576ddcd1', '').
narrative_ontology:cs_kernel_id(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_providers).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocates_renewable_path).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_industry).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_baseload_focus).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(climate_mitigation_legitimacy__renewable_primacy_reading, general_public).
narrative_ontology:constraint_victim(climate_mitigation_legitimacy__renewable_primacy_reading, general_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promote and legislate policies that prioritize renewable energy and storage, directing subsidies and regulatory frameworks away from nuclear and fossil fuels. Their legitimacy is tied to achieving rapid, cost-effective decarbonization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, policymakers_renewable_focus, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from favorable policy, subsidies, and public funding directed towards renewable projects. They advocate for the 'faster and cheaper' narrative to secure continued investment and market dominance.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, renewable_energy_developers, beneficiary,
    organized, biographical, arbitrage, global).

% Experience high demand and investment due to the emphasis on integrating intermittent renewables. They are key enablers of the 'renewables plus storage' vision and benefit directly from its policy adoption.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, storage_technology_providers, beneficiary,
    organized, biographical, arbitrage, global).

% Champion the narrative as the most effective and ethical path to climate mitigation, aligning with their ideological commitments. They exert political pressure to maintain and strengthen policies favoring renewables.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, climate_advocates_renewable_path, beneficiary,
    organized, generational, mobile, global).

% Faces declining investment, regulatory hurdles, and public skepticism due to the prioritization of renewables. Their long-term, capital-intensive projects are seen as 'too slow and too expensive,' making them a capital sink that delays decarbonization.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, nuclear_industry, payer,
    powerful, generational, identity_locked, global).

% Is directly targeted for phase-out by decarbonization policies, but also indirectly by the 'faster and cheaper' narrative that positions renewables as the sole viable alternative, further eroding their social license and investment prospects.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, fossil_fuel_industry, payer,
    institutional, generational, trapped, global).

% Bear the costs and technical challenges of integrating high penetrations of intermittent renewables, often without sufficient dispatchable baseload. They face pressure to maintain grid stability while adhering to renewable-first mandates.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, grid_operators_baseload_focus, payer,
    institutional, immediate, constrained, national).

% Benefits from reduced emissions and potentially lower energy costs if the 'cheaper' claim holds. However, they may also bear costs through subsidies, grid instability, or higher prices if the 'faster and cheaper' promise is not fully realized.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(climate_mitigation_legitimacy__renewable_primacy_reading, general_public, payer).

% Argue that reliable decarbonization requires dispatchable baseload power (including nuclear) that renewables alone cannot provide at scale. Their concerns are often marginalized or dismissed within the dominant 'renewable primacy' discourse.
narrative_ontology:constraint_stakeholder(climate_mitigation_legitimacy__renewable_primacy_reading, baseload_necessity_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate global and national investment, policy, and technological development towards a specific decarbonization pathway centered on renewable energy and energy storage, aiming for rapid and cost-effective climate mitigation.
% TRANSFER_FUNCTION: Shifts significant capital, research funding, and political support from nuclear and fossil fuel industries towards renewable energy developers and storage technology providers. It also transfers the burden of grid stability and reliability onto grid operators, while promising long-term environmental benefits to the general public.
% ABSENT_VOICES: Advocates for nuclear power, proponents of technology-neutral energy portfolios, and those prioritizing grid resilience over speed of decarbonization are often excluded or marginalized in policy discussions dominated by the renewable primacy narrative.
% DISAPPEARANCE_RATIONALE: If the belief that 'renewables plus storage can achieve full decarbonization faster and cheaper than nuclear' vanished overnight, energy policy would immediately revert to a more technology-neutral approach, nuclear projects would regain investment, and the pace and cost of decarbonization would be re-evaluated, leading to a significant reorganization of energy markets and policy priorities.
% FOUNDING_PROBLEM: The urgent need for rapid, large-scale, and economically viable decarbonization to address the existential threat of climate change, coupled with a desire to avoid the perceived risks and costs associated with nuclear power.
% FOUNDING_PROBLEM_CORROBORATION: The scientific consensus on climate change (IPCC reports) and the economic imperative to transition away from fossil fuels are widely corroborated by independent scientific bodies and international organizations. The 'faster and cheaper' aspect is corroborated by some energy economists and renewable industry reports, though contested by others.
narrative_ontology:disappearance_verdict(climate_mitigation_legitimacy__renewable_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(climate_mitigation_legitimacy__renewable_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(climate_mitigation_legitimacy__renewable_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(climate_mitigation_legitimacy__renewable_primacy_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_mitigation_legitimacy__renewable_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_mitigation_legitimacy__renewable_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.68, rising to 0.75) because the policy framework effectively reallocates massive capital and market share from established energy sectors (nuclear, fossil fuels) to emerging renewable and storage technologies, often through subsidies and preferential market rules. `Suppression` is also high (0.75, rising to 0.80) as the constraint actively discourages or prohibits investment in nuclear, and fossil fuels, through regulatory barriers, funding cuts, and public discourse that frames them as inferior or obsolete. `Theater_ratio` is moderate (0.20, rising to 0.30) because while the core goal of decarbonization is genuine, some of the 'faster and cheaper' claims may become performative if real-world implementation faces unforeseen costs or technical hurdles, requiring continued rhetorical defense despite accumulating evidence. `Accessibility_collapse` is 0.60 as alternatives like nuclear are not entirely eliminated but are severely constrained. `Resistance` is 0.55, reflecting ongoing pushback from the nuclear and fossil fuel industries, as well as grid operators concerned about reliability.
 *
 * PERSPECTIVAL GAP:
 *   Policymakers and renewable advocates perceive this as a necessary and efficient coordination mechanism to address climate change, with benefits outweighing costs. The nuclear industry and grid operators, however, experience it as an extractive and suppressive force that unfairly disadvantages their technologies and creates grid stability challenges. The engine's per-seat classification will highlight this divergence, showing a 'rope' or 'scaffold' for beneficiaries and a 'snare' or 'tangled_rope' for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   The `policymakers_renewable_focus`, `renewable_energy_developers`, `storage_technology_providers`, and `climate_advocates_renewable_path` are clear beneficiaries, receiving policy support, market share, and ideological validation. The `nuclear_industry`, `fossil_fuel_industry`, and `grid_operators_baseload_focus` are victims, bearing the costs of disfavor, reduced investment, and operational challenges. The `general_public` is a mixed seat, benefiting from decarbonization but potentially paying through subsidies or grid instability. The `baseload_necessity_advocates` are excluded, their arguments suppressed by the dominant narrative.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the policy as a pure 'rope' (simple coordination) by highlighting the active suppression of alternatives and the significant extraction from disfavored sectors. It also avoids mislabeling it as a pure 'snare' by acknowledging the genuine coordination function towards decarbonization. The 'tangled_rope' classification captures the hybrid nature: coordinating towards a climate goal while extracting from and suppressing specific technological paths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    faster_cheaper_empirical_validity,
    'Is the claim that ''renewables plus storage can achieve full decarbonization faster and cheaper than nuclear'' empirically robust across all relevant scales and geographies, or does it rely on specific assumptions about cost declines and grid integration?',
    'Long-term, large-scale empirical data on grid integration costs, system-level LCOE (Levelized Cost of Energy), and actual deployment timelines in diverse regions, compared against nuclear deployment data.',
    'If the claim proves less robust, the constraint''s `extractiveness` and `suppression` of nuclear would be re-evaluated as less justified by efficiency, potentially shifting its classification towards a ''snare'' for nuclear, or a ''piton'' if the policy persists without its core justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(faster_cheaper_empirical_validity, empirical, 'Empirical validity of the ''faster and cheaper'' claim for renewables vs. nuclear.').

omega_variable(
    grid_stability_baseload_necessity,
    'To what extent is dispatchable baseload power (e.g., nuclear) structurally necessary for grid stability and resilience at high decarbonization levels, given current and projected storage capabilities?',
    'Advanced grid modeling, real-world operational data from highly decarbonized grids, and independent engineering assessments of system reliability under various extreme weather and demand scenarios.',
    'If baseload is found to be structurally necessary, the `suppression` of nuclear would be re-evaluated as detrimental to the overall decarbonization goal''s reliability, potentially shifting the constraint''s classification towards a ''snare'' for grid operators or a ''tangled_rope'' with higher effective extraction from the public.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grid_stability_baseload_necessity, empirical, 'Structural necessity of baseload power for grid stability in a decarbonized system.').

omega_variable(
    decarbonization_framing_priority,
    'Does the ''decarbonization'' goal, as framed by this reading, prioritize speed and cost above other values like energy security, grid resilience, and technological diversity?',
    'Policy analysis comparing stated objectives with actual resource allocation and risk assessments, and public discourse analysis of how trade-offs are presented and justified.',
    'If speed and cost are found to systematically override other values, the constraint''s `extractiveness` and `suppression` would be seen as a consequence of a narrow framing, potentially leading to a reclassification towards a ''snare'' if the suppressed values are critical for long-term societal well-being.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decarbonization_framing_priority, conceptual, 'Conceptual framing of decarbonization priorities within this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_mitigation_legitimacy__renewable_primacy_reading, 2000, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t2000, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(clim_tr_t2008, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2008, 0.12).
narrative_ontology:measurement(clim_tr_t2016, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement(clim_tr_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement(clim_tr_t2032, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2032, 0.25).
narrative_ontology:measurement(clim_tr_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, theater_ratio, 2040, 0.3).

% Extraction over time
narrative_ontology:measurement(clim_be_t2000, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(clim_be_t2008, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(clim_be_t2016, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2016, 0.6).
narrative_ontology:measurement(clim_be_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement(clim_be_t2032, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2032, 0.72).
narrative_ontology:measurement(clim_be_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, base_extractiveness, 2040, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t2000, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(clim_su_t2008, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(clim_su_t2016, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement(clim_su_t2024, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2024, 0.75).
narrative_ontology:measurement(clim_su_t2032, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2032, 0.78).
narrative_ontology:measurement(clim_su_t2040, climate_mitigation_legitimacy__renewable_primacy_reading, suppression_requirement, 2040, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_mitigation_legitimacy__renewable_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__baseload_necessity_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__portfolio_pragmatism_reading).
narrative_ontology:affects_constraint(climate_mitigation_legitimacy__renewable_primacy_reading, climate_mitigation_legitimacy__degrowth_sufficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'climate_mitigation_legitimacy' kernel, each representing a distinct policy approach to decarbonization. This reading focuses on the primacy of renewables and storage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
