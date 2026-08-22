% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__comparative_risk_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__comparative_risk_dominant, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: acceptable_risk_for_energy__comparative_risk_dominant
 *   human_readable: Comparative Risk Framing for Nuclear Energy Acceptability
 *   domain: energy_policy/risk_governance/climate
 *
 * SUMMARY:
 *   The constraint encodes a specific reading of how acceptable risk for
 *   nuclear energy should be determined: by comparison to alternative
 *   energy-system risks (coal emissions, climate catastrophe), not by an
 *   absolute safety threshold or intergenerational-stewardship standard. This
 *   reading emerged during the climate-urgency period (~2010–2025) when
 *   energy policy converged on rapid decarbonization as a binding constraint.
 *   The reading embeds a temporal prioritization (immediate climate
 *   catastrophe > 10,000-year waste stewardship) and a power asymmetry
 *   (high-GDP nations + nuclear operators set the comparative frame;
 *   climate-vulnerable populations + future generations live under it). The
 *   authored extractiveness (0.68) and suppression (0.71) reflect that the
 *   constraint requires active enforcement (alternative risk-framings must be
 *   marginalized in policy forums) and produces asymmetric costs. The
 *   theater_ratio (0.42) is moderate: the constraint does real coordination
 *   work (links climate urgency to energy-infrastructure allocation), but a
 *   growing share of enforcement activity defends the specific comparison
 *   against intergenerational-justice objections rather than solving the
 *   founding coordination problem.
 *
 * KEY AGENTS:
 *   - nuclear_industry: agenda-setter and primary beneficiary; sets comparative-risk terms in regulatory forums
 *   - climate_vulnerable_populations: powerless, trapped payers; experience climate catastrophe now but bear assigned nuclear risk relative to coal emissions
 *   - future_generations (abstracted): non-agent payers; assigned 10,000+ year waste-stewardship obligations to defer intergenerational-justice costs
 *   - low_income_communities_near_plants: moderate-power payers; bear localized accident and contamination risk under comparative framing
 *   - carbon_intensive_energy_operators: beneficiaries; gain years of asset-depreciation runway as nuclear becomes decarbonization priority
 *   - high_gdp_developed_nations: institutional beneficiaries; can maintain energy consumption while appearing climate-responsible; export comparative risk to lower-GDP nations
 *   - renewable_energy_sector: excluded; would reframe the comparison as 'nuclear vs. renewable saturation' rather than 'nuclear vs. coal'
 *   - intergenerational_justice_advocates: observers; document the temporal weighting and distributional asymmetry, but lack authority to set risk terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__comparative_risk_dominant, 0.71).
domain_priors:theater_ratio(acceptable_risk_for_energy__comparative_risk_dominant, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__comparative_risk_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__comparative_risk_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__comparative_risk_dominant, "Comparative Risk Framing for Nuclear Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__comparative_risk_dominant, "energy_policy/risk_governance/climate").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__comparative_risk_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__comparative_risk_dominant, '1f68546d-eb15-4a92-97e4-1e4d81d12bc5').
narrative_ontology:cs_kernel_codification('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', distributed).
narrative_ontology:cs_authority_grounding('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', extraction).
narrative_ontology:cs_reading_relation('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', acceptable_risk_for_energy__expected_value_dominant, coexists_with).
narrative_ontology:cs_axiom('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', foundational, temporal_urgency_dominates_intergenerational_stewardship).
narrative_ontology:cs_axiom_status(temporal_urgency_dominates_intergenerational_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', temporal_urgency_dominates_intergenerational_stewardship, empirically_contingent).
narrative_ontology:cs_axiom('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', foundational, comparative_risk_is_decision_relevant).
narrative_ontology:cs_axiom_status(comparative_risk_is_decision_relevant, holdable).
narrative_ontology:cs_axiom_grounding('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', comparative_risk_is_decision_relevant, conventional).
narrative_ontology:cs_reference_frame('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', climate_emergency_binding_constraint).
narrative_ontology:cs_drift_state('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', contemporary_renewable_acceleration, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f68546d-eb15-4a92-97e4-1e4d81d12bc5', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, carbon_intensive_energy_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__comparative_risk_dominant, high_gdp_developed_nations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_burden).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__comparative_risk_dominant, low_income_communities_near_plants).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, comparative_risk_analysis_framework).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__comparative_risk_dominant, temporal_urgency_climate_emergency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates existing reactors and licenses new builds under comparative-risk framing. Argues that nuclear's low-carbon operation is acceptable relative to coal emissions and climate catastrophe. Actively establishes this reading in regulatory testimony, safety standards committees, and climate-policy forums. Benefits from the constraint by justifying continued operation and new investment under a favorable risk calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry, beneficiary).

% In low-lying island nations, arid regions, flood-prone areas, and Global South economies, bear climate catastrophe risks now (displacement, famine, economic collapse) if decarbonization stalls. The comparative-risk frame says: accept nuclear risk to prevent climate risk. They are trapped: climate change is already happening; exit from the energy system is not available; their political power to set risk terms is minimal.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, climate_vulnerable_populations, payer,
    powerless, immediate, trapped, global).

% Non-agent abstraction representing long-term stewardship obligations. The comparative-risk reading subordinates the intergenerational waste-management problem to immediate climate urgency: 'we must decarbonize now; future generations will solve waste storage.' This reading pays the cost by deferring it — accepting 10,000+ year stewardship burdens to avoid climate tipping points in the next 20–30 years.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_burden, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__comparative_risk_dominant, future_generations_waste_burden).

% Bear localized accident risk, tritium contamination, thermal discharge effects on water, and property value impacts. The comparative-risk frame says these harms are acceptable relative to coal's particulate emissions and climate catastrophe. They have some exit (relocation, legal action) but face housing-market constraints and often lack resources to litigate. Their political voice in siting decisions is muted.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, low_income_communities_near_plants, payer,
    moderate, biographical, constrained, regional).

% Coal, natural gas, and oil-production interests benefit from the comparative-risk framing because it makes nuclear the primary decarbonization option, delaying transition to renewables and demand reduction. They co-author the frame in policy forums, fund think tanks, and shape regulatory debate. The constraint gives them years to depreciate assets before forced transition.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, carbon_intensive_energy_operators, beneficiary,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__comparative_risk_dominant, carbon_intensive_energy_operators, agenda_setter).

% Possess the capital and technical capacity to build and operate nuclear plants safely, and have established waste-disposal strategies (deep repositories, political stability for long-term containment). The comparative-risk reading allows them to maintain high energy consumption while appearing climate-responsible. Lower-GDP nations and island states export the comparative risk.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, high_gdp_developed_nations, beneficiary,
    institutional, generational, arbitrage, global).

% Would argue for treating renewable deployment speed and storage as the primary decarbonization pathway, making nuclear risk-acceptability conditional on renewable saturation rather than on coal-avoidance. Their voice is excluded from the primary risk-setting forums because the comparative-risk frame has already locked in nuclear as the climate solution.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, renewable_energy_sector, excluded,
    organized, biographical, constrained, global).

% Analyze the constraint from the standpoint of future burden and present inequality. They take testimony from all seats, compare temporal discounting across readings, and document the distributional asymmetry: present benefits accrue to high-GDP operators and nations; long-term waste costs distribute intergenerationally. They cannot set the risk threshold but generate evidence the constraint's framing marginalizes.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__comparative_risk_dominant, intergenerational_justice_advocates, observer,
    moderate, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__comparative_risk_dominant, nuclear_industry).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__comparative_risk_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a coherent risk-evaluation framework for decarbonization that allows high-energy economies to maintain consumption while transitioning away from carbon-intensive sources. Solves the coordination problem of linking climate-emergency urgency to energy-technology deployment — all parties need a shared calculus to allocate trillions in infrastructure investment.
% TRANSFER_FUNCTION: Moves immediate climate-catastrophe risk FROM high-GDP economies and coal operators TO climate-vulnerable populations and future generations (via waste stewardship). The constraint transfers decision authority from local communities and intergenerational trustees TO energy-policy institutions and nuclear operators who set the comparative terms.
% ABSENT_VOICES: Renewable-energy advocates are excluded from primary risk-framing forums; they would argue the comparison should be nuclear vs. renewable+storage saturation, not nuclear vs. coal. Low-income communities near plants have minimal voice in siting and risk-acceptance decisions despite bearing localized accident exposure. Future waste-management institutions do not exist to negotiate their own risk burden.
% DISAPPEARANCE_RATIONALE: If the comparative-risk framework disappeared and were replaced by another reading (catastrophic-tail or expected-value dominant), the global energy infrastructure investment portfolio would shift: nuclear builds would slow, renewable deployment would accelerate, temporal urgency would be rebalanced against intergenerational stewardship. The decarbonization pathway would reorganize around different risk-acceptability terms.
% FOUNDING_PROBLEM: Climate catastrophe requires rapid decarbonization, but coal-phase-out timelines conflict with renewable deployment speed; nuclear offers low-carbon baseload generation. The founding problem is: how to achieve climate goals given energy-system inertia and transitional constraints?
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and energy-system analysts outside the nuclear industry attest that decarbonization urgency is real and deployment timelines are constrained. However, renewable-energy researchers dispute that nuclear is the necessary pathway; they attest that storage and demand-management can close the gap faster. The founding problem (urgent decarbonization) is live; the claim that comparative-risk framing is the solution is contested.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__comparative_risk_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__comparative_risk_dominant, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__comparative_risk_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__comparative_risk_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__comparative_risk_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__comparative_risk_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 at t=0 to 0.68 at t=25, then plateaus. The rise reflects maturation of the comparative-risk framing in policy institutions (IEA climate scenarios, IPCC integrated-assessment models, regulatory risk-assessment standards). The plateau indicates the frame has solidified: by t=25, the reading is institutionally embedded and achieves its asymptotic extraction level. Suppression follows a similar curve: it rises from 0.52 to 0.71 as alternative risk-framings (catastrophic-tail, expected-value) are marginalized from primary policy forums. The theater_ratio rises gradually (0.28 to 0.42) and flattens, indicating increasing performative content in risk-communication: safety reviews and environmental-impact studies remain substantive, but their framing in policy documents shifts toward defending the comparative logic against intergenerational objections. The measurement grid is uniform across all time points, allowing the engine to track the constraint's lifecycle as it transitions from emerging reading to institutionalized norm. All three metrics are authored honestly from the reading's structural dynamics, independent of any prediction about the engine's classification.
 *
 * PERSPECTIVAL GAP:
 *   The nuclear-industry and high-GDP-nation seats experience the comparative-risk frame as genuine coordination (linking climate urgency to energy infrastructure, solving a real collective-action problem). From the climate-vulnerable and low-income seats, the same constraint operates as enforced extraction (risk redistribution, voice exclusion, temporal subordination). The engine computes this seat-by-seat: the agenda-setter seats will likely compute as rope-adjacent (coordination dominates); the payer seats will compute as snare (extraction, suppression, no voice). This divergence is the point — the authored claim (tangled_rope) sits between them, capturing both the coordination truth (there is a real problem being solved) and the extraction truth (asymmetric distribution and active enforcement are required to maintain it).
 *
 * DIRECTIONALITY LOGIC:
 *   The nuclear industry (institutional, arbitrage exit) and high-GDP nations (institutional, arbitrage exit) are structural beneficiaries: the comparative-risk frame justifies their investment and operations. Their effective extraction d approaches 0.2–0.3 (beneficiary end). Climate-vulnerable populations (powerless, trapped) are structural targets: the frame assigns them comparative risk without voice in the risk-setting process. Their d approaches 0.85–0.95 (target end). Low-income communities near plants (moderate, constrained) sit around d=0.7–0.8: they bear accident risk and house-value impacts but have some exit (relocation, legal remedies). The renewable-energy sector, if a seat rather than excluded, would be a moderate target (d~0.65) because the frame deprioritizes their technology. Future generations are abstract (non-agent) but their d in the stewardship chain is near 1.0: they inherit the waste burden with zero choice. The engine's directionality derivation should reflect these asymmetries from the declared power atoms, exit options, and beneficiary/victim structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is: decarbonization urgency given energy-system inertia. That problem is live and disputed (climate scientists and energy modelers attest urgency; renewable-energy researchers dispute that nuclear is necessary; energy operators dispute that coal-phase-out timelines are binding). The disappearance verdict is world_rearranges because if the comparative-risk frame disappeared, the energy-infrastructure portfolio would reorganize around different risk-acceptability terms. However, the founding-problem status (live, not dead) combined with the constraint's maturation and theater-ratio growth creates a mandatrophy candidate: the framing has achieved institutional stability such that the cost to change it (the energy-portfolio reorganization) now exceeds the cost to maintain the status quo (continue managing alternative risk-framings as non-live options). The constraint is not dead (the underlying coordination problem persists), but the institutional machinery maintaining it has calcified. This is tangled-rope character: the coordination function is real, but the constraint's persistence increasingly depends on suppression (excluding alternative framings) rather than participant preference.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    renewable_deployment_acceleration_counterfactual,
    'Would renewable+storage+demand-management deployment reach required decarbonization targets faster than nuclear+renewables under realistic capital and manufacturing constraints?',
    'Energy-system modeling with binding manufacturing and deployment-timeline constraints; natural experiments from high-renewable-penetration grids (Denmark, Uruguay); capacity-expansion analysis from independent research institutes outside nuclear advocacy.',
    'If renewables alone can meet decarbonization timelines, the comparative-risk frame (coal vs. nuclear) becomes a false dilemma — the comparison should be nuclear vs. renewable saturation. This would reduce extractiveness (the risk redistribution is not necessary) and shift the constraint toward snare (pure extraction masquerading as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_deployment_acceleration_counterfactual, empirical, 'Whether nuclear is necessary for climate goals or renewable deployment alone suffices.').

omega_variable(
    temporal_discount_rate_ethical_choice,
    'Is subordinating 10,000+ year intergenerational waste burden to 30-year climate-urgency windows a defensible weighting, or does it reflect a privileged present generation extracting from the future?',
    'Ethical analysis and intergenerational-justice frameworks; deliberative forums with representation from waste-management trustees (where they exist institutionally); comparison of discount-rate practices across long-term infrastructure commitments (ice-age-timescale climate models, geologic carbon storage, cultural heritage preservation).',
    'If the temporal weighting is indefensible, the constraint shifts from tangled_rope (coordination + asymmetric extraction) toward snare (pure extraction using climate urgency as cover story). The reading''s axiom (temporal_urgency_dominates) moves from holdable toward overridden.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(temporal_discount_rate_ethical_choice, preference, 'Whether present decarbonization urgency justifies future waste stewardship debt.').

omega_variable(
    distributional_asymmetry_coincidence_vs_design,
    'Does the comparative-risk frame happen to concentrate risk on powerless populations, or is the framing actively shaped to produce that asymmetry because it lowers resistance from powerful stakeholders?',
    'Historical analysis of risk-frame adoption (who proposed it, in which forums, against which alternatives); examination of alternative frames (e.g., ''acceptable to those bearing the risk'' vs. ''acceptable to those deciding'') that would reverse the distribution; testimony from communities in nuclear-siting decisions about voice and power.',
    'If design (even indirect incentive design), the constraint is more extractive than authored — suppression mechanisms are more deliberate, resistance is more actively managed. The extraction shifts from byproduct to primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributional_asymmetry_coincidence_vs_design, empirical, 'Whether risk distribution is incidental or structurally designed.').

omega_variable(
    kernel_reading_contestation,
    'Is the comparative-risk reading one legitimate read of an ambiguous kernel (acceptable_risk_for_energy), or does it foreclose sibling readings within a single coherent framework?',
    'Examination of the kernel''s foundational text/doctrine/tradition: can a single decision-making authority (regulator, legislature, court) endorse both comparative-risk and catastrophic-tail framings, or does adopting one rule out the other? If coexistence is possible, the readings are sibling interpretations; if adoption of one eliminates the other''s logical coherence, one reading forecloses.',
    'If the readings coexist (different authorities can hold different frames), the constraint is one reading competing in a multi-reading field and gains no foreclosure power. If comparative_risk_dominant forecloses catastrophic_tail_dominant, the relation changes from coexists_with to forecloses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'The logical structure of kernel readings and foreclosure relationships.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__comparative_risk_dominant, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(acce_tr_t25, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t35, acceptable_risk_for_energy__comparative_risk_dominant, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(acce_tr_t35, projected).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(acce_be_t25, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t35, acceptable_risk_for_energy__comparative_risk_dominant, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(acce_be_t35, projected).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(acce_su_t25, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t35, acceptable_risk_for_energy__comparative_risk_dominant, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(acce_su_t35, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__comparative_risk_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__comparative_risk_dominant, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__comparative_risk_dominant, acceptable_risk_for_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the acceptable_risk_for_energy kernel. The constraint family decomposes a single natural-language concept into three structurally distinct constraint stories, each with its own ε, beneficiary/victim structure, and temporal weighting. They are linked by the shared kernel (the ambiguous policy question: 'What makes nuclear risk acceptable?') and by the reading_relations in cs_structure. All three readings are live in policy discourse; none has foreclosed the others in a single authoritative framework. Each reading's ε reflects its own referent (the standing arrangement under contest from that reading's perspective); the three stories are not readings of each other but sibling instantiations of the same kernel under different authority structures and institutional framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__comparative_risk_dominant, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
