% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: acceptable_risk_energy__option_value_preserving
 *   human_readable: Option Value Preservation in Energy Pathway Decisions
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   This constraint instantiates the option-value-preserving reading of the
 *   acceptable-risk-in-energy-policy kernel. The reading frames energy
 *   decarbonization as navigating irreducible deep uncertainty about
 *   renewable cost curves, storage breakthroughs, and climate sensitivity.
 *   Its core claim: maintaining multiple energy pathways (nuclear, fossil,
 *   renewable) open longer preserves real optionality, allowing decisions to
 *   defer until empirical evidence resolves uncertainty. This reading
 *   COEXISTS WITH two siblings: a catastrophic-tail-dominant reading
 *   (prioritize avoiding low-probability climate catastrophes even at cost of
 *   higher near-term expected harm) and an expected-value-dominant reading
 *   (minimize aggregate mortality-per-TWh using current best-estimate
 *   models). The three readings share a kernel (the concept of 'acceptable
 *   risk' under energy uncertainty) but derive different constraint
 *   structures and victim sets. This story is ONLY the option-value reading;
 *   the others are separate constraint files.
 *
 * KEY AGENTS:
 *   - Uncertainty hedging advocates (institutional beneficiaries): energy economists, decision theorists, risk-averse planners who frame optionality as epistemic prudence
 *   - Diversified energy sector (institutional beneficiaries): fossil, nuclear, and hybrid portfolio companies that extract sustained revenue from delayed pathway closure
 *   - Accelerated transition advocates (organized payers): climate scientists, environmental NGOs, rapid-decarbonization proponents suppressed by policy signals favoring optionality
 *   - Premature pathway closures (powerless payers): future generations bearing opportunity costs of delayed decarbonization (trapped, no exit)
 *   - Financial hedging institutions (institutional beneficiaries): portfolios that profit from sustained asset optionality and delayed commitment
 *   - Renewable energy sector (moderate, dual-positioned): benefits from any transition but constrained by policy signals keeping fossil/nuclear viable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__option_value_preserving, 0.58).
domain_priors:suppression_score(acceptable_risk_energy__option_value_preserving, 0.47).
domain_priors:theater_ratio(acceptable_risk_energy__option_value_preserving, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, extractiveness, 0.58).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(acceptable_risk_energy__option_value_preserving, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__option_value_preserving, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__option_value_preserving, "Option Value Preservation in Energy Pathway Decisions").
narrative_ontology:topic_domain(acceptable_risk_energy__option_value_preserving, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__option_value_preserving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__option_value_preserving, '05c3cb62-c019-49fa-97a4-5f6d349bb85f').
narrative_ontology:cs_kernel_codification('05c3cb62-c019-49fa-97a4-5f6d349bb85f', distributed).
narrative_ontology:cs_authority_grounding('05c3cb62-c019-49fa-97a4-5f6d349bb85f', expertise).
narrative_ontology:cs_interpretation_layer_present('05c3cb62-c019-49fa-97a4-5f6d349bb85f').
narrative_ontology:cs_reading_relation('05c3cb62-c019-49fa-97a4-5f6d349bb85f', acceptable_risk_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('05c3cb62-c019-49fa-97a4-5f6d349bb85f', acceptable_risk_energy__expected_value_dominant, influences).
narrative_ontology:cs_axiom('05c3cb62-c019-49fa-97a4-5f6d349bb85f', foundational, deep_uncertainty_justifies_optionality).
narrative_ontology:cs_axiom_status(deep_uncertainty_justifies_optionality, holdable).
narrative_ontology:cs_axiom_grounding('05c3cb62-c019-49fa-97a4-5f6d349bb85f', deep_uncertainty_justifies_optionality, empirically_contingent).
narrative_ontology:cs_axiom('05c3cb62-c019-49fa-97a4-5f6d349bb85f', foundational, flexibility_preservation_over_committed_pathway).
narrative_ontology:cs_axiom_status(flexibility_preservation_over_committed_pathway, holdable).
narrative_ontology:cs_axiom_grounding('05c3cb62-c019-49fa-97a4-5f6d349bb85f', flexibility_preservation_over_committed_pathway, instrumental).
narrative_ontology:cs_reference_frame('05c3cb62-c019-49fa-97a4-5f6d349bb85f', epistemic_uncertainty_framework).
narrative_ontology:cs_drift_state('05c3cb62-c019-49fa-97a4-5f6d349bb85f', post_empirical_evidence_accumulation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('05c3cb62-c019-49fa-97a4-5f6d349bb85f', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, uncertainty_hedging_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, diversified_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, premature_pathway_closures).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, accelerated_transition_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, financial_hedging_institutions).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__option_value_preserving, renewable_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, renewable_energy_sector).
narrative_ontology:constraint_victim(acceptable_risk_energy__option_value_preserving, coal_dependent_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Energy economists, decision theorists, and risk-averse policy planners who frame energy decarbonization as navigating irreducible deep uncertainty. They argue maintaining nuclear and fossil pathways open longer is prudent optionality: renewable cost curves may not decline as expected, storage breakthroughs may not materialize, climate sensitivity may exceed current estimates. They benefit from policy frameworks that justify delayed commitment to any single pathway and extended operation of multiple technologies.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, uncertainty_hedging_advocates, beneficiary,
    institutional, generational, arbitrage, national).

% Energy companies with diversified portfolios (traditional fossil, nuclear, renewables). The option-value framework justifies continued investment in coal and gas plants alongside renewables. They collect sustained revenue from extended operational lifespans, delayed retirement schedules, and policy tolerance for long-term fossil operation. Portfolio companies benefit from optionality because it lowers asset write-down risk and extends revenue streams.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, diversified_energy_sector, beneficiary,
    institutional, biographical, mobile, global).

% Climate scientists, environmental organizations, renewable-energy advocates, and rapid-decarbonization proponents. They argue the option-value framework extracts an opportunity cost: capital and policy attention diverted to keeping fossil plants viable could accelerate renewable buildout, storage deployment, and grid hardening. They face institutional suppression: policy signals that favor optionality and fossil viability discourage aggressive transition investment. They contend the founding problem (deep uncertainty) is partially resolved by empirical evidence, yet the constraint persists.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, accelerated_transition_advocates, payer,
    organized, biographical, constrained, national).

% Future populations subject to the climate and physical-risk consequences of delayed decarbonization justified by current optionality hedging. The constraint keeps fossil pathways open longer than carbon budgets and climate thresholds may permit. They cannot negotiate current policy or opt out of the inherited energy infrastructure and atmospheric composition.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, future_generations, payer,
    powerless, generational, trapped, global).

% Investment portfolios, insurance companies, pension funds, and financial institutions that profit from optionality and sustained uncertainty. Keeping multiple energy pathways open extends the useful life and valuation of energy assets, reducing portfolio write-down risk. They extract value from the option premium: longer operational lifespans and delayed commitment mean lower portfolio turnover and sustained asset values.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, financial_hedging_institutions, beneficiary,
    institutional, immediate, mobile, global).

% Companies, investors, and communities focused on wind, solar, and storage deployment. They benefit from any energy transition but pay the opportunity cost when capital allocation is modulated to maintain fossil viability. Policy signals of optionality-preservation reduce their near-term investment signals and constrain growth. Their advancement is conditional on optionality being eventually abandoned in favor of committed transition.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, renewable_energy_sector, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__option_value_preserving, renewable_energy_sector, beneficiary).

% Transmission and balancing authorities managing minute-to-minute grid reliability. They observe the constraint as an infrastructure tradeoff: maintaining dispatchable fossil and nuclear capacity ensures near-term reliability given current grid design; rapidly closing those pathways demands massive storage, grid hardening, and demand-management investments. They measure whether the constraint's claimed flexibility preservation is real (can they actually defer decisions while maintaining reliability) or theoretical.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, grid_reliability_operators, observer,
    institutional, biographical, constrained, regional).

% Towns and regions economically dependent on coal plants for tax revenue, employment, and local services. The option-value framework delays their facility's closure, but when closure eventually arrives (if renewables exceed expectations or policy shifts), they face concentrated adjustment costs — workforce retraining, tax base collapse, infrastructure maintenance — that the distributed optionality benefit from other regions never compensated them for. They are geographically trapped and face identity lock (coal mining heritage, multi-generational employment).
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, coal_dependent_communities, payer,
    powerless, biographical, trapped, local).

% Scientists and modeling teams quantifying climate sensitivity, tipping points, and impact thresholds. They measure whether the 'deep uncertainty' that justifies option preservation is empirically deep or bounded by evidence. Their analysis feeds the contention between option-value and tail-risk-dominant framings. High empirical confidence in renewable deployment and climate sensitivity would undermine the founding problem (deep uncertainty) but current constraint persists.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__option_value_preserving, climate_risk_modelers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__option_value_preserving, diversified_energy_sector).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__option_value_preserving, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under deep uncertainty about renewable cost decline rates, storage breakthroughs, climate sensitivity, and deployment constraints, maintaining multiple energy pathways (fossil, nuclear, renewable) open preserves real optionality: decisions can be deferred and updated as empirical evidence accumulates, reducing lock-in risk to suboptimal pathways.
% TRANSFER_FUNCTION: Transfers opportunity cost from hedging-benefit recipients (energy companies, financial institutions, policy planners) to acceleration-benefit recipients (climate advocates, renewable investors, future high-impact populations). The transfer is: capital and policy attention allocated to keeping fossil plants viable are capital and attention NOT allocated to accelerated renewable/storage buildout and climate-risk reduction.
% ABSENT_VOICES: Future generations subject to climate outcomes determined by current decarbonization pace; climate-threshold-dependent ecosystems; populations in high-impact regions vulnerable to climate tipping points. These are abstracted into statistical models rather than represented as contending seats in the optionality-preservation negotiation. Rapid-decarbonization advocates are present but institutionally suppressed.
% DISAPPEARANCE_RATIONALE: If the option-value constraint vanished and commitment to rapid fossil closure became binding policy, energy capital allocation would reorganize within 2–3 years: coal retirements would accelerate, gas plant life-extension approvals would cease, renewable and storage investment would concentrate heavily, grid hardening would prioritize high-renewables scenarios. The energy system architecture and climate pathway would shift toward faster decarbonization; fossil-dependent communities would face immediate stranded-asset adjustment. The optionality framework would be replaced by committed-pathway selection.
% FOUNDING_PROBLEM: Energy system decarbonization operates under deep, irreducible uncertainty: the rate of renewable cost decline, storage breakthrough timing, grid integration constraints, and climate sensitivity thresholds are all contested and empirically unresolved. Committing irreversibly to closing fossil and nuclear pathways risks locking into inadequate pathways; maintaining optionality preserves flexibility as new evidence arrives.
% FOUNDING_PROBLEM_CORROBORATION: Uncertainty-hedging advocates (energy economists, diversified utilities, portfolio investors) attest the founding problem is live and will remain deep. Expected-value-dominant advocates (climate modelers, empirical transition analysts, renewable-industry economists) attest the founding problem is substantially resolved by evidence: renewable cost curves have tracked or beaten optimistic projections, storage deployment has accelerated, climate sensitivity estimates have tightened, deployment constraints have proven more manageable than modeled. Independent peer-reviewed energy pathways analyses (IPCC, IEA) support the contested status: empirical evidence now supports higher renewable deployment confidence, yet genuine tail risks about tipping points and stranded capital remain. The disagreement is between those who read evidence as resolving uncertainty and those who read the same evidence as revealing deeper tail-risk hedging requirements.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__option_value_preserving, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__option_value_preserving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__option_value_preserving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_energy__option_value_preserving, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__option_value_preserving, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.58 at interval end) because the constraint shifts opportunity costs: resources allocated to maintaining fossil capacity are resources not allocated to accelerated renewable/storage buildout. Suppression is moderate (0.47) because the constraint requires active enforcement against both extremes: accelerated transition advocates face institutional suppression (policy signals, regulatory tolerance for extended fossil operation), while catastrophic-tail advocates face epistemic suppression (deep-uncertainty framing brackets climate tipping-point arguments). Theater is moderate-low (0.28): the optionality function is theoretically real (decisions deferred until uncertainty resolves), but an increasing share of enforcement activity defends extended fossil operation and delayed renewable capital allocation rather than preserving genuine flexibility. The measurement series shows extractiveness and theater rising gently over the interval (renewable deployment confidence increased, reducing genuine uncertainty, yet fossil pathways remain open) — the constraint's theoretical justification weakens as empirical uncertainty declines, but its enforcement persists. Suppression holds stable: the conflict between option-value and transition-acceleration framings remains structurally latent.
 *
 * PERSPECTIVAL GAP:
 *   From the hedging-advocate seat, the constraint is genuine coordination: optionality is a real coordination good in the face of uncertainty, and multiple pathways remain viable. From the accelerated-transition seat, the same structure operates as enforced extraction: maintaining optionality is cover for extracting opportunity cost from those bearing decarbonization delay and climate risk. The engine computes per-seat classification from beneficiary/victim + exit options; the divergence is the measurement it exists to surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Uncertainty hedging advocates and diversified energy companies are structural beneficiaries: they collect the optionality rent (longer operational life for fossil/nuclear capacity, delayed write-downs, sustained policy tolerance). Accelerated transition advocates and stranded asset communities are targets: they pay the opportunity cost (capital diverted from renewable buildout, delayed climate action, concentrated adjustment risk). The constraint requires active enforcement because it suppresses BOTH committed transition (by maintaining fossil/nuclear viability) and catastrophic-tail-dominant arguments (by treating uncertainty as strategically deep rather than empirically bounded). Without enforcement, transition advocates would close fossil pathways faster, and tail-risk advocates would prioritize climate tipping-point avoidance over optionality preservation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (energy system uncertainty as deep and unresolved) has partially resolved: renewable cost curves, storage deployment, and climate sensitivity are now better quantified than when option-value frameworks dominated energy policy (2010–2015). The constraint persists by shifting its justification: initial framing emphasized epistemically deep uncertainty; current framing emphasizes tail-risk hedging and lock-in avoidance. The problem-status is genuinely contested because empirical evidence has accumulated (supporting faster decarbonization confidence) while policy commitment to optionality has hardened. This is mandatrophy candidate: a constraint whose original founding problem has partially died but whose institutional enforcement persists, rewritten to serve different beneficiaries (now: financial hedging and extended fossil operation, rather than prudent navigation of uncertainty).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deep_uncertainty_empirical_status,
    'Is the uncertainty about renewable deployment rates, storage breakthroughs, and climate sensitivity genuinely ''deep'' (irreducible, unresolvable by evidence accumulation) or empirically bounded (quantifiable via data, resolvable as evidence arrives)?',
    'Track realized renewable cost curves, actual storage deployment trajectories, and empirical climate sensitivity estimates against 2015-era models. If realization tracks expected-value-dominant projections closely, uncertainty was empirically bounded; if surprises accumulate in tail directions, uncertainty was deeper than expected.',
    'If deep: option-value preservation remains justified and the constraint''s founding problem is live. If bounded: the founding problem has partially resolved and maintaining optionality becomes pure extraction, shifting the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deep_uncertainty_empirical_status, empirical, 'Whether ''deep uncertainty'' in energy decarbonization remains irreducible or is empirically resolving.').

omega_variable(
    optionality_preservation_vs_fossil_defense,
    'Does the option-value framework preserve genuine decision flexibility, or does it function primarily as an institutional mechanism for defending extended fossil operation?',
    'Examine actual policy decisions made under the option-value framing: when renewable deployment exceeded expectations, did policy actively shift capital toward renewables, or did optionality logic shift to justify maintaining fossil ''insurance''? Compare expected-value-dominant jurisdictions with option-value-preserving jurisdictions on capital allocation shifts.',
    'If genuine flexibility: some portion of the measured extraction is coordination cost (the cost of real optionality). If fossil-defense mechanism: the constraint is primarily extractive cover, shifting classification fully toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optionality_preservation_vs_fossil_defense, empirical, 'Whether option-value framework enables real decision flexibility or rationalizes fossil pathway extension.').

omega_variable(
    reading_contention_location,
    'Where exactly do the option-value and catastrophic-tail framings logically diverge? Both claim to be hedging against uncertainty; is the difference empirical (disagreement about tail probability) or normative (disagreement about how much tail risk justifies near-term harm)?',
    'Decompose each reading''s axioms: identify claims about empirical facts (tail probability, renewable deployment rates) versus normative claims (what level of tail risk justifies opportunity costs). Map which disagreements resolve with data versus which remain normative.',
    'If the divergence is primarily empirical: evidence about renewable costs and climate sensitivity will move the readings toward convergence. If normative: the readings may coexist indefinitely regardless of empirical resolution. Identifies whether the reading contention is empirically resolvable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contention_location, conceptual, 'The structural location of disagreement between option-value and catastrophic-tail readings.').

omega_variable(
    stranded_asset_distribution_asymmetry,
    'Who bears the stranded-asset adjustment costs when fossil pathways eventually close (due to renewable success or policy shift)? Are costs distributed proportionally to the beneficiaries of the option-value preservation, or concentrated on local communities?',
    'Examine coal-plant closure cases in option-value-preserving jurisdictions: map which parties benefited from extended operation (utilities, portfolio investors, energy companies) against which parties bore adjustment costs (coal-dependent communities, local governments, workers). Assess whether compensation was proportional.',
    'If distributed proportionally: the extraction is symmetric and the constraint is closer to rope. If asymmetric: the constraint exhibits snare-like victim concentration, with powerless parties bearing costs that institutional beneficiaries imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_distribution_asymmetry, empirical, 'Whether stranded-asset costs of optionality hedging are distributed proportionally or concentrated on powerless parties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__option_value_preserving, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__option_value_preserving, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t5, acceptable_risk_energy__option_value_preserving, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(acce_tr_t5, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__option_value_preserving, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t15, acceptable_risk_energy__option_value_preserving, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(acce_tr_t15, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__option_value_preserving, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(acce_tr_t20, projected).
narrative_ontology:measurement(acce_tr_t25, acceptable_risk_energy__option_value_preserving, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(acce_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__option_value_preserving, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t5, acceptable_risk_energy__option_value_preserving, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(acce_be_t5, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__option_value_preserving, base_extractiveness, 10, 0.56).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t15, acceptable_risk_energy__option_value_preserving, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(acce_be_t15, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__option_value_preserving, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(acce_be_t20, projected).
narrative_ontology:measurement(acce_be_t25, acceptable_risk_energy__option_value_preserving, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(acce_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__option_value_preserving, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t5, acceptable_risk_energy__option_value_preserving, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(acce_su_t5, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__option_value_preserving, suppression_requirement, 10, 0.46).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t15, acceptable_risk_energy__option_value_preserving, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(acce_su_t15, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__option_value_preserving, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(acce_su_t20, projected).
narrative_ontology:measurement(acce_su_t25, acceptable_risk_energy__option_value_preserving, suppression_requirement, 25, 0.47).
narrative_ontology:measurement_basis(acce_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__option_value_preserving, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_energy__option_value_preserving, 0.18).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__option_value_preserving, acceptable_risk_energy__expected_value_dominant).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel acceptable_risk_energy. The kernel describes the concept of 'acceptable risk' in energy decarbonization policy under deep uncertainty about renewable deployment and climate sensitivity. Three structurally distinct constraints result from three different readings of that kernel: option-value-preserving (maintains multiple pathways for flexibility), catastrophic-tail-dominant (prioritizes avoiding climate catastrophe), and expected-value-dominant (minimizes aggregate mortality-per-TWh). All three share the kernel but derive different victim sets, beneficiaries, and suppression mechanisms. This story (option-value-preserving) links to its siblings via network.affects_constraints and documents the reading relationships in cs_structure.reading_relations. The sibling stories each have their own constraint_id files; this is the option-value reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_energy__option_value_preserving, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
