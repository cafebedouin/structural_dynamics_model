% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__option_value_preserving, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Acceptable Risk: Option Value Preservation in Energy Policy
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   Energy policy under deep uncertainty faces a choice between commitment to
 *   narrow decarbonization pathways and preservation of optionality across
 *   multiple technologies and strategies. The option-value-preserving reading
 *   interprets 'acceptable risk' as maintaining both fossil, nuclear, and
 *   renewable pathways as live options, justifying continued investment and
 *   operation across the entire fuel mix. This reading structures energy
 *   policy to defer hard prioritization decisions, reserves grid capacity and
 *   capital for 'flexibility,' and suppresses advocacy for rapid pathway
 *   closure. The claim/metric separation is deliberate: the reading is
 *   CLAIMED as tangled_rope (coordination benefit of preserving optionality,
 *   active enforcement against pathway closure) while metrics show
 *   moderate-to-substantial extraction and theater (the cost to
 *   rapid-transition advocates, the growing performance of
 *   optionality-maintenance as justification rather than function). The
 *   engine computes the divergence per-seat; this narrative grounds it
 *   structurally.
 *
 * KEY AGENTS:
 *   - Incumbent fossil operators: benefit from extended operation justified as optionality reserve; actively shape the framing.
 *   - Nuclear industry advocates: benefit from protection against closure under the optionality logic; moderate organizational power.
 *   - Policy planners: benefit from deferral of hard choices; institutional power to enforce the multi-pathway framework.
 *   - Climate rapid-transition advocates: pay the cost of delayed closure and suppressed pathway prioritization.
 *   - Renewable advocates: pay the cost of deprioritization relative to incumbent pathways.
 *   - Affected communities: pay externality costs from extended operation of fossil and nuclear infrastructure.
 *   - Energy modelers: analytical seat documenting tension between optionality and emissions constraints; not binding on policy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.62).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.48).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Acceptable Risk: Option Value Preservation in Energy Policy").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '90df9731-0f23-4225-a4e9-a402c1209898').
narrative_ontology:cs_kernel_codification('90df9731-0f23-4225-a4e9-a402c1209898', distributed).
narrative_ontology:cs_authority_grounding('90df9731-0f23-4225-a4e9-a402c1209898', extraction).
narrative_ontology:cs_reading_relation('90df9731-0f23-4225-a4e9-a402c1209898', acceptable_risk_energy__acceptable_risk_energy_catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('90df9731-0f23-4225-a4e9-a402c1209898', acceptable_risk_energy__acceptable_risk_energy_expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('90df9731-0f23-4225-a4e9-a402c1209898', foundational, deep_uncertainty_demands_pathway_flexibility).
narrative_ontology:cs_axiom_status(deep_uncertainty_demands_pathway_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('90df9731-0f23-4225-a4e9-a402c1209898', deep_uncertainty_demands_pathway_flexibility, empirically_contingent).
narrative_ontology:cs_axiom('90df9731-0f23-4225-a4e9-a402c1209898', foundational, commitment_to_single_pathway_concentrates_technology_risk).
narrative_ontology:cs_axiom_status(commitment_to_single_pathway_concentrates_technology_risk, holdable).
narrative_ontology:cs_axiom_grounding('90df9731-0f23-4225-a4e9-a402c1209898', commitment_to_single_pathway_concentrates_technology_risk, empirically_contingent).
narrative_ontology:cs_reference_frame('90df9731-0f23-4225-a4e9-a402c1209898', multi_pathway_optionality_default).
narrative_ontology:cs_drift_state('90df9731-0f23-4225-a4e9-a402c1209898', contemporary_renewable_cost_collapse, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('90df9731-0f23-4225-a4e9-a402c1209898', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, nuclear_industry_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, policy_uncertainty_beneficiaries).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, climate_rapid_transition_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, renewable_acceleration_advocates).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, communities_bearing_fuel_mix_externalities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fossil energy operators benefit from the option-value framing because it justifies retaining coal and gas pathways as 'flexibility reserves' rather than planning for managed phase-out. They actively shape policy discourse to present rapid fossil retirement as excessive risk concentration. They collect rents by maintaining investment in infrastructure justified as 'optionality insurance.'
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators, agenda_setter).

% Nuclear advocates benefit from the option-value logic, which frames keeping nuclear viable as prudent risk management against both fossil and renewable failure scenarios. The framing protects long-development-cycle nuclear projects from closure and justifies sustained subsidies or regulatory exemptions. They do not set the agenda unilaterally but amplify the optionality argument in policy contexts.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, nuclear_industry_advocates, beneficiary,
    organized, generational, constrained, global).

% Policy makers and energy planners benefit from the option-value framing because it legitimizes delay and multi-pathway investment as prudent rather than indecisive. It permits deferring hard choices about energy priorities, which shifts the decision burden to future administrations and avoids near-term political cost.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, policy_uncertainty_beneficiaries, beneficiary,
    institutional, biographical, arbitrage, global).

% Climate advocates who argue for rapid fossil elimination bear the cost of the option-value framing: their priority (aggressive emissions reduction) is subordinated to flexibility logic. They cannot freely advocate for pathway closure without being framed as concentrating risk. Their resources are absorbed defending against the optionality-based delays their constituencies oppose.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_rapid_transition_advocates, payer,
    organized, civilizational, constrained, global).

% Renewable energy advocates face suppression of their core agenda: they argue for rapid scaling of renewables and storage, but the option-value framing reserves grid space and investment capital for incumbent pathways 'to preserve flexibility.' They cannot credibly advocate for renewable-exclusive grids without being accused of excessive risk concentration; their policy influence is modulated downward by the optionality frame.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, renewable_acceleration_advocates, payer,
    moderate, biographical, constrained, national).

% Communities living near coal plants, gas infrastructure, and nuclear facilities bear the externalities (air quality, water risk, decommissioning costs) of the extended operational life that option-value logic justifies. They cannot exit the geography and have no direct representation in energy policy discourse. The optionality framing extends their exposure duration.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, communities_bearing_fuel_mix_externalities, payer,
    powerless, biographical, trapped, local).

% The climate science consensus on carbon budgets and warming trajectories suggests that optionality logic — keeping all pathways open — is in structural tension with cumulative emissions constraints. The consensus would argue for rapid decarbonization, not flexible multi-pathway investment. It is excluded from the policy framing that treats optionality as a first-order principle.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_science_consensus, excluded,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_energy__option_value_preserving, climate_science_consensus).

% Future generations inherit both the climate outcomes and the locked-in infrastructure that option-value delayed closure creates. They would bear the cost of postponed hard choices but have no seat in present energy policy discourse. The option-value framing externalizes temporal costs.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_generations, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(acceptable_risk_energy__option_value_preserving, future_generations).

% Energy economists and integrated assessment modelers analyze energy transition pathways. They document the structural tension between optionality preservation and emissions constraints, but their findings are competing in a discourse where optionality is already institutionalized as a risk principle. Their analytical seat is not binding on policy.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, energy_modelers_and_economists, observer,
    institutional, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, incumbent_fossil_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces decision risk under deep uncertainty about energy technology performance, climate damages, and social demand: by maintaining multiple viable pathways, the arrangement avoids locking society into a single technology solution that might fail catastrophically.
% TRANSFER_FUNCTION: Delays closure costs (asset write-downs, employment transition, capital reallocation) away from incumbent operators and planners toward future administrations and communities bearing extended externalities. Transfers opportunity cost (the benefit of rapid decarbonization under certain climate timelines) from climate advocates to beneficiaries of extended incumbent infrastructure operation.
% ABSENT_VOICES: Climate scientists who argue the carbon budget is incompatible with extended multi-pathway optionality; rapid-transition advocates who are excluded from framing the risk conversation; affected communities near extended fossil and nuclear infrastructure who cannot object effectively; future generations who inherit locked-in emissions.
% DISAPPEARANCE_RATIONALE: If the option-value-preservation constraint disappeared, energy policy would reframe around explicit prioritization: either aggressive decarbonization timelines (narrow the pathway set) or explicit tail-risk avoidance (abandon certain options like fossil baseload). Capital flows would shift away from 'flexibility maintenance' toward committed pathways. Incumbent operators would face accelerated closure timelines; renewable scaling would accelerate or nuclear would be formally eliminated, depending on which reading replaced the optionality frame.
% FOUNDING_PROBLEM: Energy technology futures are deeply uncertain: renewable storage may fail to scale, nuclear may remain economically uncompetitive, climate damages may exceed all projections or fall short. Committing society to a single pathway before uncertainty resolves concentrates risk on that pathway's success.
% FOUNDING_PROBLEM_CORROBORATION: Option-value advocates attest the founding problem is live: deep uncertainties in climate, technology, and social demand justify multi-pathway investment. Climate scientists and rapid-transition advocates attest the founding problem is a rationalization for delay: the relevant uncertainty has narrowed (solar/wind/storage costs have collapsed, carbon budgets are tight), and the real problem is institutional inertia, not genuine option value. Independent energy transition analyses note the widening gap between optionality timelines and emissions budgets.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__option_value_preserving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures the transfer away from rapid decarbonization advocates toward incumbent operators and policy planners. It rises from 0.48 to 0.62 over the first 25 years (the framing hardens as renewable costs collapse and the optionality justification for fossil retention becomes threadbare — higher extraction as the cover story thins). It plateaus at 0.62 after year 25 because by then the constraint has either calcified into institutional practice (piton trajectory) or will have failed (if climate damage becomes undeniable). Theater rises modestly from 0.18 to 0.31 (the performance of optionality-maintenance as a real risk principle grows relative to functional energy coordination). Suppression is moderate-low (0.41–0.48) because the constraint does NOT heavily repress rapid-transition advocates in overt, visible ways — instead it moderates their framing and shifts the burden of advocacy upward (they must argue against institutionalized optionality). Both extremes (fossil-elimination and nuclear-elimination) are suppressed equally; neither can command policy without addressing the optionality frame.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (incumbent operators, planners) should perceive genuine coordination benefit — they see optionality preservation as sophisticated risk management. The payer seats (rapid-transition, renewable advocates, affected communities) should perceive extraction — they see optionality as a pretext for delaying their priorities. The engine computes these divergences from power, exit options, and beneficiary/victim declarations. The fossil operators have arbitrage exit (they can move capital across geographies and timelines) and institutional power; they compute as mild targets or even mild beneficiaries depending on the scope modifier. The affected communities have trapped exit and powerless status; they compute as high-target, high-extraction seats. The policy planners sit near symmetric on the extractiveness axis because optionality benefits them (deferral) but also constrains them (they must maintain the fiction of genuine flexibility).
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent fossil operators accumulate d near the beneficiary end because the constraint protects their asset base and revenue streams, and they have the power and exit options to avoid harm. Nuclear advocates accumulate at moderate-beneficiary because they benefit but are less mobile and more dependent on the constraint's persistence. Policy planners accumulate near symmetric (slight beneficiary bias from deferral option, slight target bias from commitment to multi-pathway investment). Climate advocates accumulate high-target (suppressed advocacy, delayed timeline priority, constrained exit). Affected communities accumulate highest-target (trapped exit, no voice, extended externality exposure). The directionality_overrides array is empty because the structural derivation from beneficiary/victim + exit captures the relationships: the constraint's primary effect is to retain incumbent pathways and defer hard choice, which maps cleanly to the power/exit/role data.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is a tangled_rope because it performs genuine coordination (optionality preservation under uncertainty is a real risk reduction function) AND demands active enforcement (suppression of rapid-closure advocacy, capital reservation for multi-pathway investment, institutional rules that block pathway-elimination decisions). The victim set is not incidental: climate advocates and affected communities must be actively suppressed to keep the optionality frame alive, because their priority (rapid decarbonization) is logically incompatible with flexible multi-pathway operation. The classification prevents mis-reading this as a rope (where victims would be absent or incidental) or a snare (where the coordination framing would be mere cover). It is genuinely both: the coordination function is real (optionality does reduce tail risk under true uncertainty), and the extraction is real (the benefit of deferral to incumbents and planners is decoupled from the cost, which falls on climate advocates and affected communities). The classification holds as long as the uncertainty genuinely persists; if the uncertainty collapses (e.g., if renewable storage becomes certain to scale and fossil is certain to be uncompetitive), the constraint should reclassify toward snare, because the coordination justification would no longer hold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genuine_uncertainty_vs_rationalization,
    'Is the deep uncertainty that optionality addresses structurally genuine, or is it a rationalization for institutional inertia?',
    'Comparison of energy technology cost trajectories, renewable storage scaling data, and climate damage models against projections made 10+ years prior. High prediction accuracy would suggest uncertainty is lower than the framing claims; wide divergence from old projections would suggest persistent genuine uncertainty.',
    'If uncertainty is lower than claimed, the constraint should reclassify from tangled_rope toward snare (coordination justification erodes, extraction becomes primary). If uncertainty remains high, the tangled_rope classification holds because the coordination function is genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_uncertainty_vs_rationalization, empirical, 'Whether the founding uncertainty persists or has been substantially resolved.').

omega_variable(
    optionality_as_cover_vs_real_risk_reduction,
    'Does maintaining all pathways genuinely reduce societal risk under uncertainty, or does it concentrate risk on future generations who inherit locked-in infrastructure?',
    'Long-run analysis comparing outcomes under committed-pathway scenarios vs. optionality-preservation scenarios, conditioned on different future climate and technology realizations. If optionality scenarios perform better across a wide range of futures, the coordination claim holds; if they consistently leave higher-cost infrastructure and greater emissions, the extraction claim holds.',
    'If optionality is risk-reducing, the tangled_rope classification is justified. If it concentrates temporal and climate risk on future generations, the constraint should reclassify toward snare (the coordination is cover for intergenerational extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optionality_as_cover_vs_real_risk_reduction, conceptual, 'Whether optionality preservation is risk-reducing or risk-concentrating across time horizons.').

omega_variable(
    suppression_mechanism_institutional_vs_internalized,
    'Is the suppression of rapid-transition advocacy structural (policy rules, capital gatekeeping, framing exclusion) or internalized (advocates have internalized optionality logic and suppress themselves)?',
    'Qualitative analysis of advocacy discourse and policy venue access: if advocates are excluded by formal rules or resource constraints, suppression is structural; if they actively adopt optionality language and self-moderate, suppression is internalized. Post-exit analysis: if advocacy intensity increases when advocates move to jurisdictions without the optionality frame, suppression was partly internalized.',
    'If structural, the constraint''s effective suppression is higher than the authored scalar suggests (targets carry the suppression with them only when exiting the policy venue). If internalized, the escape routes are fewer and exit costs are higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_institutional_vs_internalized, empirical, 'Whether suppression of rapid-transition advocacy is structural or internalized.').

omega_variable(
    kernel_reading_contention,
    'Is this reading (option-value-preserving) genuinely distinct from the expected-value-dominant reading, or do they collapse into the same constraint under different valuation schemes?',
    'Analysis of policy recommendations each reading generates: option-value recommends maintaining all pathways; expected-value recommends closing pathways that have high expected harm. If the readings generate incompatible action sets, they are distinct; if they differ only in how harm is weighted, they may be the same constraint under different observer frames.',
    'If distinct, two constraint stories are justified (this one + the expected-value one). If they collapse, the kernel decomposition is incomplete and may require reframing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contention, conceptual, 'Whether the option-value and expected-value readings are genuinely distinct constraints or artifacts of different aggregation schemes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__option_value_preserving, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__option_value_preserving, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__option_value_preserving, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.29).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__option_value_preserving, theater_ratio, 25, 0.3).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__option_value_preserving, theater_ratio, 30, 0.31).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__option_value_preserving, theater_ratio, 40, 0.31).
narrative_ontology:measurement_basis(acce_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__option_value_preserving, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__option_value_preserving, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__option_value_preserving, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__option_value_preserving, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__option_value_preserving, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__option_value_preserving, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(acce_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.41).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__option_value_preserving, suppression_requirement, 5, 0.43).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__option_value_preserving, suppression_requirement, 10, 0.45).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__option_value_preserving, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__option_value_preserving, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__option_value_preserving, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__option_value_preserving, suppression_requirement, 40, 0.48).
narrative_ontology:measurement_basis(acce_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy_catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy_expected_value_dominant).

% DUAL FORMULATION NOTE:
% The acceptable_risk_energy kernel decomposes into three structurally distinct readings: option_value_preserving (maintains multiple pathways for flexibility), catastrophic_tail_dominant (prioritizes tail-risk avoidance), and expected_value_dominant (minimizes aggregate expected harm). Each reading instantiates different beneficiary/victim sets, different ε values, and different classifications. The option-value reading influences both siblings by institutionalizing optionality as a default principle; both siblings must argue against or within the optionality frame to gain traction. The three readings are linked in network.affects_constraints for tracking constraint family relationships.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
