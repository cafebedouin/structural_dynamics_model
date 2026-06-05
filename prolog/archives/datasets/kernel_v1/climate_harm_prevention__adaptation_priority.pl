% ============================================================================
% CONSTRAINT STORY: climate_harm_prevention__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: climate_harm_prevention__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response: Present Resilience vs. Future Mitigation
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate harm prevention represents a
 *   specific institutional choice: to prioritize building resilience
 *   infrastructure and adaptive capacity for populations that can afford it,
 *   while accepting a higher atmospheric CO2 trajectory (2.7°C+ warming) on
 *   the grounds that mitigation at the necessary scale is 'politically
 *   infeasible.' This reading exists in structural tension with the
 *   mitigation-priority reading (which posits that transformation is possible
 *   and necessary) and the degrowth reading (which rejects the
 *   growth-compatible framing entirely). The adaptation-priority reading
 *   instantiates a kernel claim — that present generations must choose
 *   between near-term resilience for the vulnerable and future avoidance of
 *   warming — that is contested across political, economic, and ethical
 *   divides. The constraint exhibits the structural signature of tangled_rope
 *   because it contains genuine coordination elements (adaptation funding
 *   does protect vulnerable populations in the near term) layered with
 *   extraction (the 'political feasibility' framing justifies foregoing
 *   mitigation, offloading future costs to those without negotiating power).
 *   The theater ratio (0.68) reflects that adaptation governance relies
 *   heavily on scenario modeling, cost-benefit frameworks, and feasibility
 *   narratives that perform calculation of political constraints rather than
 *   testing their actuality. Measurements show increasing extractiveness,
 *   theater, and suppression over the 30-year interval, indicating that as
 *   adaptation-priority policy becomes institutionalized, its performative
 *   character increases (new adaptation institutions require legitimation
 *   through scenario studies) and the suppression of alternatives hardens
 *   (mitigation pathways are closed off by lock-in to adaptation-only
 *   investments).
 *
 * KEY AGENTS:
 *   - Near-term Wealthy Populations (Mid-Latitude): Primary beneficiary (institutional/arbitrage) — receive adaptation infrastructure, water security, cooling systems, health system resilience during the critical 2030-2070 period
 *   - Low-Adaptation-Capacity Regions: Primary victim (powerless/trapped) — face 70% of climate damages while receiving minimal adaptation finance; no exit from trajectory chosen by others
 *   - Future Generations: Primary victim (powerless/trapped) — inherit sunk CO2 and forced adaptation to residual 2.7°C warming; no voice in choice
 *   - Incumbent Carbon Industries: Beneficiary (institutional/arbitrage) — adaptation-priority framing legitimizes continued extraction; simultaneously benefits from adaptation infrastructure markets
 *   - Climate Policy Establishment (IPCC, UNFCCC, World Bank): Institutional actor (institutional/arbitrage) — maintains adaptation-priority framing through institutionalized cost-benefit analysis; sees own reasoning as degraded but unavoidable
 *   - Climate Justice Coalition: Constrained participant (organized/constrained) — can redirect adaptation funds toward justice but constrained by the adaptation-priority framing that preempts mitigation discussion
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent political choices as immutable constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, 0.58).
domain_priors:suppression_score(climate_harm_prevention__adaptation_priority, 0.72).
domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(climate_harm_prevention__adaptation_priority, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_harm_prevention__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_harm_prevention__adaptation_priority, "Adaptation-Priority Climate Response: Present Resilience vs. Future Mitigation").
narrative_ontology:topic_domain(climate_harm_prevention__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_harm_prevention__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_harm_prevention__adaptation_priority, '19f999ac-a6af-43f3-a5d2-1efa8c5dadbd').
narrative_ontology:cs_kernel_codification('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', formalized).
narrative_ontology:cs_authority_grounding('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', extraction).
narrative_ontology:cs_interpretation_layer_present('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd').
narrative_ontology:cs_reading_relation('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', climate_harm_prevention__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', climate_harm_prevention__degrowth_reading, influences).
narrative_ontology:cs_axiom('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', foundational, mitigation_infeasibility_at_necessary_scale).
narrative_ontology:cs_axiom_status(mitigation_infeasibility_at_necessary_scale, holdable).
narrative_ontology:cs_axiom_grounding('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', mitigation_infeasibility_at_necessary_scale, empirically_contingent).
narrative_ontology:cs_axiom('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', secondary, near_term_vulnerable_prioritization_justified).
narrative_ontology:cs_axiom_status(near_term_vulnerable_prioritization_justified, holdable).
narrative_ontology:cs_axiom_grounding('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', near_term_vulnerable_prioritization_justified, deontological).
narrative_ontology:cs_reference_frame('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', political_economy_constraints_binding).
narrative_ontology:cs_drift_state('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', contemporary_climate_emergency, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('19f999ac-a6af-43f3-a5d2-1efa8c5dadbd', '').
narrative_ontology:cs_kernel_id(climate_harm_prevention__adaptation_priority, climate_harm_prevention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, near_term_wealthy_populations).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, adaptation_infrastructure_vendors).
narrative_ontology:constraint_beneficiary(climate_harm_prevention__adaptation_priority, incumbent_carbon_industries).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, future_generations).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, low_adaptation_capacity_regions).
narrative_ontology:constraint_victim(climate_harm_prevention__adaptation_priority, ecological_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-ADAPTATION-CAPACITY REGIONS (SNARE) — No exit from warming trajectory chosen by wealthy nations. Cannot participate in resource allocation decisions. Face maximum extraction: bear 70% of climate costs while receiving minimal adaptation finance. Suppression is total — geographic/economic barriers prevent exit or voice.
constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — No exit from atmospheric CO2 trajectory locked in by present adaptation-priority policy. Cannot negotiate terms or exit from constraint. Face sunk costs of unavoided warming. Temporal barrier replaces spatial one: the choice is made before they exist, and they inherit the resulting climate state.
constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: NEAR-TERM VULNERABLE POPULATIONS — MID-LATITUDE DEVELOPED NATIONS (TANGLED ROPE) — Constrained by political feasibility but also primary beneficiaries of adaptation investment. Experience genuine coordination: adaptation funding builds resilience infrastructure, health systems, water security that protects them. Also experience extraction: the adaptation-priority framing justifies not reducing emissions, locking them into a higher-warming future. Moderate power, significant costs at biographical scale, also significant benefits.
constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INCUMBENT CARBON INDUSTRIES & ADAPTATION INFRASTRUCTURE VENDORS (ROPE) — Primary beneficiaries. Adaptation-priority framing legitimizes continued fossil fuel extraction ('we're adapting to warming, not preventing it') while simultaneously creating massive markets for seawalls, cooling systems, water infrastructure. Pure coordination with asymmetric benefits: the constraint solves their political problem (avoiding emission reduction) while opening new extraction pathways.
constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE JUSTICE COALITION & MITIGATION ADVOCATES (TANGLED ROPE) — Organized agents with constrained exit. Participate in adaptation governance but constrained by political economy of carbon lock-in. Benefit from adaptation funding (can direct it toward justice outcomes) but extracted from through the constraint itself (adaptation-priority framing preempts emission reduction). Experience the constraint as enforced but contestable.
constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE POLICY ESTABLISHMENT (PITON) — Institutional actors that have internalized adaptation-priority framing as inevitable. Theater is high: extensive scenarios modeling 2.7°C warming with 'adaptation', cost-benefit analysis justifying adaptation over mitigation, green growth narratives. Primary function (identify politically feasible climate policy) has largely been achieved; the residual institutional process continues through inertia. The policy apparatus sees its own reasoning as degraded — acknowledges mitigation would be preferable but frames it as politically impossible.
constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — POLITICAL FEASIBILITY AS NATURAL LAW (MOUNTAIN) — Frames adaptation-priority as a natural constraint: mitigation is 'politically infeasible' at the scale required, therefore adaptation is the only realistic option. Treats political economy of carbon lock-in as immutable physical law. However, structural data reveals this as false summit: political feasibility is contingent on institutional choice, not a natural law. The mountain classification naturalizes what is actually a tangled_rope with high suppression.
constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_harm_prevention__adaptation_priority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_harm_prevention__adaptation_priority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_harm_prevention__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_harm_prevention__adaptation_priority, TR),
    TR >= 0.70.

:- end_tests(climate_harm_prevention__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from future generations and low-adaptation regions through a temporal and geographic asymmetry: present wealthy populations benefit from adaptation investment while outsourcing mitigation costs to the future and the Global South. The extraction is contingent on political economy assumptions (mitigation is infeasible) rather than technological ones. Suppression (0.72): High. Multiple barriers prevent contesting adaptation-priority: institutional lock-in (Adaptation funding has been committed; mitigation pathway is politically inaccessible), knowledge suppression (adaptation-focused research dominates; mitigation studies are marginalized as 'too radical'), and cognitive framing (the 'political feasibility' narrative makes alternative framings unthinkable). Theater ratio (0.68): High. Extensive cost-benefit analyses, scenario modeling, and feasibility assessments create an appearance of rigorous optimization while actually performing a predetermined institutional choice. The theater has increased over the interval as adaptation-priority policy has become institutionalized — new institutions require legitimating narratives, pushing theater higher. Measurements show extraction rising from 0.42 to 0.68 and suppression from 0.55 to 0.72, indicating the constraint's extractive mechanism has hardened as lock-in progresses.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural choice appears as coordination to beneficiaries and extraction to victims. Near-term wealthy populations experience the constraint as tangled_rope — genuine coordination (adaptation infrastructure protects them) with some costs. Future generations experience it as snare — no exit, no negotiating power, maximal costs. Low-adaptation regions experience it as snare — geographic powerlessness, no exit. The adaptation-vendor industries experience it as pure rope — solving their political problem. The climate policy establishment experiences it as piton — institutionalized process that has largely succeeded but persists through inertia. The analytical observer risks mountain classification — treating political feasibility as natural law — but structural data reveals this as false summit: the inevitability is manufactured through institutional choice, not derived from natural constraints. The perspectival gaps reveal that adaptation-priority is not a technical optimization but a distribution of power and cost across time and geography.
 *
 * DIRECTIONALITY LOGIC:
 *   The adaptation-priority reading operates through a specific structural mechanism: it declares mitigation politically infeasible (a claim about institutional capacity) and therefore prioritizes adaptation (near-term resilience for those who can afford it). This framing produces distinct directionality values for different agents. Wealthy populations that benefit from adaptation infrastructure have low d (beneficiaries with immediate exit through adaptation); victims in low-adaptation regions have high d (trapped, bearing extraction with no voice). Future generations have maximum d (trapped by temporal barrier, bearing sunk costs). The derivation chain flows from beneficiary/victim declarations (wealthy adaptive populations benefit; future generations and poor regions are victims) combined with exit options (wealthy have arbitrage through adaptation investment; poor regions have no exit; future generations are temporally trapped). The false summit at the analytical context reveals that the mountain classification (political feasibility as natural law) is actually naturalizing a contingent institutional choice with identifiable beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that adaptation-priority is tangled_rope with a false-summit analytical perspective, not a natural law. The mandate — 'protect the vulnerable through near-term resilience' — is genuine and achieved through adaptation infrastructure. The asymmetric extraction — 'impose higher-warming future on those without negotiating power' — is also genuine and achieved through the political-feasibility framing. Both functions are present. The tension arises because the constraint achieves its coordination mandate (protect near-term vulnerable) while simultaneously enabling extraction (offset mitigation costs to future and poor regions). This is the defining structure of tangled_rope: coordination and extraction are inseparably bound.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_feasibility_contingency,
    'Is the alleged political infeasibility of mitigation a natural constraint or a contingent institutional choice reflecting current power distributions and framing?',
    'Historical analysis of rapid institutional transitions (WW2 mobilization, post-Cold War restructuring, pandemic response); assessment of whether equivalent carbon-reduction effort has ever been attempted and failed, or whether it remains untested due to political capture',
    'If contingent: adaptation-priority is a snare with manufactured inevitability (false summit). If natural: the constraint is legitimate optimization given hard constraints. Classification shifts from tangled_rope+false_summit to legitimate rope or tangled_rope depending on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_feasibility_contingency, conceptual, 'Whether political feasibility of mitigation is a natural or contingent constraint').

omega_variable(
    adaptation_capacity_assumption,
    'Can adaptive capacity actually scale to protect vulnerable populations at 2.7°C warming, or is the adaptation-priority framing contingent on an unrealistic assumption of perfect adaptation?',
    'Empirical modeling: assess adaptation costs/feasibility at various warming levels; identify tipping points where adaptive capacity saturates; track insurance market feedback (insurance retreat from certain regions indicates adaptation capacity ceiling)',
    'If adaptation saturates below 2.7°C: adaptation-priority is false promise to powerless agents (snare reclassifies as pure extraction). If adaptation scales: constraint may legitimately balance near-term resilience with future costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_capacity_assumption, empirical, 'Whether adaptive capacity can actually scale to 2.7°C warming').

omega_variable(
    intergenerational_discounting_implicit,
    'Does the adaptation-priority reading rest on an implicit assumption about acceptable intergenerational harm that would be unacceptable if made explicit?',
    'Comparison of discount rates implied by adaptation-priority policy with explicit social welfare functions; assessment of whether the same policy would be chosen if future generations had negotiating power',
    'If implicit discounting is revealed: constraint loses normative legitimacy; becomes pure power asymmetry extraction. If defensible: constraint remains tangled_rope with contested but internally coherent axiom.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_discounting_implicit, preference, 'Implicit intergenerational discount rate in adaptation-priority framing').

omega_variable(
    adaptation_vs_mitigation_cost_tradeoff_true,
    'Do cost-benefit analyses comparing adaptation-only vs mitigation approaches accurately reflect true costs, or do they systematically undercount future costs and overcount present savings?',
    'Audit of standard climate economics models (DICE, PAGE, etc.) for baseline assumptions about adaptation costs, discount rates, and tipping point probabilities; comparison with recent empirical adaptation cost estimates',
    'If systematic undercount of future costs: adaptation-priority is rational response to flawed cost structure (snare with false cost information). Reclassifies if corrected. If costs are accurately estimated: adaptation-priority reflects genuine tradeoff between present and future welfare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_vs_mitigation_cost_tradeoff_true, empirical, 'Whether adaptation vs mitigation cost-benefit analyses are accurate').

omega_variable(
    kernel_reading_distinction,
    'How does this adaptation-priority reading of climate harm prevention differ structurally from the sibling mitigation-priority reading? Where does the disagreement originate?',
    'Identify the foundational axiom that separates readings: adaptation-priority assumes political economy constraints are binding; mitigation-priority assumes transformation is possible. Document whether disagreement is empirical (mitigation feasibility), normative (acceptable intergenerational tradeoff), or epistemic (confidence in adaptation vs residual warming).',
    'Clarifies whether readings logically foreclose each other or coexist as different parties'' positions. Determines validity of cs_structure.reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Structural distinction between adaptation-priority and mitigation-priority readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_harm_prevention__adaptation_priority, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_adapt_tr_t0, climate_harm_prevention__adaptation_priority, theater_ratio, 0, 0.52).
narrative_ontology:measurement(clim_adapt_tr_t15, climate_harm_prevention__adaptation_priority, theater_ratio, 15, 0.63).
narrative_ontology:measurement(clim_adapt_tr_t30, climate_harm_prevention__adaptation_priority, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(clim_adapt_be_t0, climate_harm_prevention__adaptation_priority, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(clim_adapt_be_t15, climate_harm_prevention__adaptation_priority, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(clim_adapt_be_t30, climate_harm_prevention__adaptation_priority, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(clim_adapt_su_t0, climate_harm_prevention__adaptation_priority, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(clim_adapt_su_t15, climate_harm_prevention__adaptation_priority, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(clim_adapt_su_t30, climate_harm_prevention__adaptation_priority, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_harm_prevention__adaptation_priority, resource_allocation).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__mitigation_priority).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_harm_prevention__degrowth_reading).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, carbon_lock_in).
narrative_ontology:affects_constraint(climate_harm_prevention__adaptation_priority, climate_finance_asymmetry).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the climate_harm_prevention kernel. The mitigation_priority reading has different ε (lower extractiveness because it distributes costs across time more equitably) and different beneficiary/victim structure (benefits future generations, challenges incumbent industries). The degrowth reading rejects the growth-compatible framing of both adaptation and mitigation. All three are linked via network.affects_constraints because the choice of reading shapes institutional investment and political feasibility of alternatives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_harm_prevention__adaptation_priority, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
