% ============================================================================
% CONSTRAINT STORY: acceptable_risk_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_energy__catastrophic_tail_dominant, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: acceptable_risk_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic-Tail-Dominant Acceptable Risk Rule (Energy Pathway Licensing)
 *   domain: risk_assessment/energy_policy/decision_theory
 *
 * SUMMARY:
 *   In most OECD jurisdictions the operative answer to 'how safe is safe
 *   enough' for nuclear energy is catastrophic-tail-dominant: a technology
 *   whose worst credible accident is region-scale contamination is held to a
 *   standard no probability-of-profit calculation can satisfy, while the
 *   distributed mortality of the fossil generation that fills the gap enters
 *   no ledger at all. This story instantiates ONE reading of the
 *   acceptable_risk_energy kernel — the catastrophic_tail_dominant reading —
 *   as a clean, epsilon-invariant constraint; the expected_value_dominant and
 *   option_value_preserving readings are separate stories with their own
 *   victim sets and their own epsilon values, linked through
 *   network.affects_constraints. The epsilon referent is the standing
 *   arrangement under contest — the asymmetric acceptable-risk regime
 *   governing nuclear licensing as it actually operates — assessed by this
 *   reading's own lights, never by the endorsed alternatives of the sibling
 *   readings. Claim and metrics are authored independently: the reading is
 *   claimed as tangled_rope because it possesses both a genuine coordination
 *   function and asymmetric transfer; the metrics describe the arrangement's
 *   actual operation without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - fossil_fuel_incumbents: Primary beneficiary (powerful/arbitrage) — collects displaced market share whenever nuclear stalls
 *   - anti_nuclear_advocacy_organizations: Secondary beneficiary (organized/identity_locked) — collects legitimacy, membership, and funding from permanent vigilance
 *   - nuclear_regulatory_bureaucracies: Agenda setter (institutional/constrained) — administers licensing and collects budget, scope, and authority
 *   - nuclear_developers_operators: Primary target (powerful/constrained) — bears approval risk and sunk-capital exposure
 *   - fossil_mortality_exposed_populations: Diffuse target (powerless/trapped) — bears substituted combustion mortality with no representation
 *   - electricity_ratepayers: Dual-positioned payer-beneficiary (moderate/constrained)
 *   - advanced_reactor_developers: Excluded innovator (organized/constrained) — locked out of criteria-setting
 *   - energy_system_decision_analysts: Analytical observer (analytical/analytical) — sees the full comparative structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_energy__catastrophic_tail_dominant, 0.8).
domain_priors:theater_ratio(acceptable_risk_energy__catastrophic_tail_dominant, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_energy__catastrophic_tail_dominant, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_energy__catastrophic_tail_dominant, "Catastrophic-Tail-Dominant Acceptable Risk Rule (Energy Pathway Licensing)").
narrative_ontology:topic_domain(acceptable_risk_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/decision_theory").

domain_priors:requires_active_enforcement(acceptable_risk_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_energy__catastrophic_tail_dominant, 'b9415e66-1d33-4db7-8ef8-22354d925106').
narrative_ontology:cs_kernel_codification('b9415e66-1d33-4db7-8ef8-22354d925106', formalized).
narrative_ontology:cs_authority_grounding('b9415e66-1d33-4db7-8ef8-22354d925106', lineage).
narrative_ontology:cs_interpretation_layer_present('b9415e66-1d33-4db7-8ef8-22354d925106').
narrative_ontology:cs_reading_relation('b9415e66-1d33-4db7-8ef8-22354d925106', acceptable_risk_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('b9415e66-1d33-4db7-8ef8-22354d925106', acceptable_risk_energy__option_value_preserving, coexists_with).
narrative_ontology:cs_axiom('b9415e66-1d33-4db7-8ef8-22354d925106', foundational, worst_credible_outcome_governs_acceptability).
narrative_ontology:cs_axiom_status(worst_credible_outcome_governs_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('b9415e66-1d33-4db7-8ef8-22354d925106', worst_credible_outcome_governs_acceptability, deontological).
narrative_ontology:cs_axiom('b9415e66-1d33-4db7-8ef8-22354d925106', secondary, concentrated_catastrophe_distinct_from_distributed_harm).
narrative_ontology:cs_axiom_status(concentrated_catastrophe_distinct_from_distributed_harm, holdable).
narrative_ontology:cs_axiom_grounding('b9415e66-1d33-4db7-8ef8-22354d925106', concentrated_catastrophe_distinct_from_distributed_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('b9415e66-1d33-4db7-8ef8-22354d925106', post_accident_precautionary_licensing_settlement).
narrative_ontology:cs_drift_state('b9415e66-1d33-4db7-8ef8-22354d925106', contemporary_comparative_risk_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b9415e66-1d33-4db7-8ef8-22354d925106', '').
narrative_ontology:cs_kernel_id(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_bureaucracies).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_developers_operators).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, fossil_mortality_exposed_populations).
narrative_ontology:constraint_victim(acceptable_risk_energy__catastrophic_tail_dominant, electricity_ratepayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_energy__catastrophic_tail_dominant, electricity_ratepayers).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, precautionary_principle_asymmetric_application).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, linear_no_threshold_dose_model).
narrative_ontology:constraint_vindicates(acceptable_risk_energy__catastrophic_tail_dominant, worst_case_scenario_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate coal- and gas-fired generation fleets whose dispatch share and asset values depend on the pace of nuclear buildout. When reactor licensing stalls or plants retire early, replacement generation is overwhelmingly gas and, in some markets, coal. The firms fund policy engagement and public communication emphasizing reactor accident scenarios; they can shift capital between fuel types and geographies as regulation moves.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents, beneficiary,
    powerful, biographical, arbitrage, global).

% Membership organizations whose programs, staffing, and donor bases are built around opposing nuclear projects and demanding stricter accident safeguards. Their interventions shape licensing hearings and public opinion. Their organizational purpose is constituted by the opposition itself; winding down the campaign would dissolve the institution's reason for existence, and diversification into broader environmental work has been only partial.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, anti_nuclear_advocacy_organizations, beneficiary,
    organized, generational, identity_locked, global).

% Agencies that write and administer the licensing criteria, run the accident-scenario reviews, and hold veto power over reactor construction and operation. Each tightening of requirements expands review scope, staffing, and budget. The agencies answer politically for any accident occurring on their watch, which ties their incentives to ever-larger safety margins; abandoning the review function would dissolve their mandate.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_bureaucracies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_regulatory_bureaucracies, beneficiary).

% Firms that finance, build, and operate reactors under licenses granted at administrative discretion. Sunk capital in certified designs and sites cannot be redeployed if reviews extend indefinitely; new entrants face decade-long approval processes before any revenue. Operating plants hold grandfathered positions but absorb escalating retrofit and reporting demands. Exit means writing off plant-scale assets.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, nuclear_developers_operators, payer,
    powerful, biographical, constrained, national).

% Urban and downwind populations breathing combustion-related air pollution from the fossil generation that fills the gap left by unbuilt reactors. The harm arrives as elevated background mortality spread across millions of individuals, each death medically attributed to ordinary causes. There is no compensation channel, no registry, and no organization representing them as a class harmed by the generation mix.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, fossil_mortality_exposed_populations, payer,
    powerless, biographical, trapped, regional).

% Households and businesses paying tariffs that reflect the generation mix. Where nuclear is delayed or cancelled they absorb higher-cost replacement generation and price volatility; they also receive the assurance of intensive accident oversight and, in normal operation, low-carbon baseload from the plants that do run. Switching retail suppliers rarely changes the underlying mix.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, electricity_ratepayers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_energy__catastrophic_tail_dominant, electricity_ratepayers, beneficiary).

% Startups and vendors developing small modular and next-generation reactor designs intended to shrink accident consequences. They hold no seat in the criteria-setting process that determines whether their designs can reach customers; the review framework predates their products and evaluates them under categories built for gigawatt light-water plants. Capital waits on approvals the current framework is not structured to grant quickly.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, advanced_reactor_developers, excluded,
    organized, biographical, constrained, global).

% Researchers comparing mortality-per-TWh across energy pathways, modeling portfolio outcomes under deep uncertainty, and publishing critiques of asymmetric risk weighting. They hold no licensing authority; their influence runs through advisory reports and the slow turnover of regulatory doctrine.
narrative_ontology:constraint_stakeholder(acceptable_risk_energy__catastrophic_tail_dominant, energy_system_decision_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_energy__catastrophic_tail_dominant, fossil_fuel_incumbents).
narrative_ontology:fixing_cost_class(acceptable_risk_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective-action problem: societies must decide collectively how much residual accident risk to tolerate in shared infrastructure, and divergent individual risk perceptions otherwise stall every siting and licensing decision. The rule converts that disagreement into an administrable standard — evaluate the worst credible outcome first — giving regulators a defensible answer to 'how safe is safe enough' and giving the public a legible guarantee.
% TRANSFER_FUNCTION: Moves permitting certainty, capital viability, and dispatch market share away from the nuclear pathway toward incumbent fossil generation; moves regulatory budget and authority toward accident-scenario administration; and moves the mortality burden of the resulting generation mix onto populations exposed to combustion pollution, uncompensated and unattributed.
% ABSENT_VOICES: The populations carrying substituted fossil-combustion mortality have no collective seat: their harm is statistically distributed, individually unattributable, and unrepresented by any organization. Advanced-reactor developers stand outside the criteria-setting conversation. Energy-poor households bear tariff effects without standing in risk deliberations. Unanimity in licensing hearings arises partly because the seats most burdened by the rule's operation were never in the room.
% DISAPPEARANCE_RATIONALE: If the catastrophic-tail-dominant rule vanished overnight, licensing would reprice on comparative-harm terms, multiple jurisdictions would restart buildouts, fossil dispatch share would fall as reactors came online, the anti-nuclear coalition's organizing frame would collapse, and regulatory review scope would contract — the energy system, the advocacy sector, and the regulatory apparatus would all reorganize around the new weighting.
% FOUNDING_PROBLEM: Early civilian nuclear power carried genuine uncertainty about catastrophic failure modes, and postwar publics had watched reactors sited without consent; societies needed a defensible, administrable answer to 'how safe is safe enough' for technologies whose worst credible case is region-scale contamination and whose accidents could destroy public trust in technological governance generally.
% FOUNDING_PROBLEM_CORROBORATION: Independent corroboration exists outside the beneficiary set: UNSCEAR and WHO epidemiological assessments document actual accident consequences far smaller than founding-era worst-case estimates; insurer actuarial data prices reactor risk as manageable; IPCC mitigation pathways include expanded nuclear shares. These sources corroborate that the founding uncertainty was real and simultaneously that present-day accident frequencies sit orders of magnitude below the estimates the framework was built around. No beneficiary-independent body attests that the founding problem remains unsolved at its original severity; the regulatory agencies and advocacy organizations attest that it does.
narrative_ontology:disappearance_verdict(acceptable_risk_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_energy__catastrophic_tail_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(acceptable_risk_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_energy__catastrophic_tail_dominant, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.68 because the rule transfers large, uncompensated costs — substituted combustion mortality, foregone low-carbon capacity, stranded design capital — while still performing real protective work, so it sits well above coordination cost but short of cover-story-only operation. Suppression is 0.80 and is predominantly structural (roughly 70%): licensing discretion, litigation exposure, and financing blockage close the nuclear pathway regardless of any participant's beliefs; the remaining ~30% is internalized public dread that the framing itself sustains and that would partially persist if barriers fell. Theater ratio is 0.40: the safety-review function is real, but a growing share of activity is ritual — hearings over designs never built, waste programs with no reachable endpoint, margin requirements far beyond any risk-relevant threshold maintained for reassurance. Accessibility_collapse is 0.52: alternative risk philosophies survive in adjacent sectors and academia but are foreclosed inside nuclear licensing, where the rule binds. Resistance is 0.62: five decades of sustained industry, economic, and climate-policy contestation. The measurement series run on one shared time grid (t=0..50, years since 1970) with every tracked metric authored at every point; trajectories are monotonic — extraction accumulating as rent layers onto the original protective settlement, enforcement hardening through the post-accident ratchet then plateauing, theatrical share growing as the reviewed fleet shrinks relative to review activity. No cyclical dynamics are asserted.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. The agenda-setter seat experiences the rule as its mandate and professional duty — the agency cannot concede the framework without dissolving itself. The incumbent beneficiary seat experiences it as favorable market structure and defends it as prudence. The developer seat experiences the same structure as an existential barrier to entry. The starkest gap is epistemic, not merely positional: concentrated, perceptible, dramatizable risk (a reactor accident) is heavily represented in the conversation, while distributed, imperceptible, statistically aggregated harm (combustion mortality) is borne by a population with no seat at all — the rule's costs are felt precisely where they are least visible.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: fossil_fuel_incumbents and anti_nuclear_advocacy_organizations sit near the beneficiary pole (low d), with the incumbents' arbitrage-grade exit reinforcing subsidy-side placement. nuclear_regulatory_bureaucracies derive slightly higher d than pure beneficiaries — they collect budget and authority but bear accident accountability — placing them modestly off the pole. nuclear_developers_operators sit near the full-target end (high d, constrained exit amplifying), and fossil_mortality_exposed_populations sit at the extreme target end: full cost-bearing, trapped, with zero compensating flow. electricity_ratepayers occupy the middle: declared victims of the tariff effect yet recipients of oversight assurance and low-carbon baseload, so their effective position is near-symmetric rather than target-side. The observer seat takes no directional position.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two symmetric mislabels. Calling the rule pure extraction erases its real coordination achievement: democracies genuinely need administrable tail-risk constitutions, dread and trust-cascade dynamics are real constraints on technology acceptance, and the founding problem was not fabricated. Calling it pure coordination erases the asymmetric transfer: the same structure that guarantees the public against catastrophe protects incumbent generation from competition and shifts mortality onto an unrepresented population. The hybrid classification keeps both faces visible and directs scrutiny to the enforcement dependency — the arrangement holds only while licensing discretion, litigation, and coalition pressure are actively supplied. On the genealogy side, the founding problem's status is contested rather than dead, so the zombie-capture mismatch (dead problem + world_rearranges) does not fire cleanly; the omegas on discount validity and displacement scale are the instruments that would settle whether the protective half of the hybrid has atrophied into cover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the catastrophic_tail_dominant reading of the acceptable_risk_energy kernel; what changes structurally if the expected_value_dominant sibling governs instead?',
    'Adoption of uniform mortality-per-TWh valuation in licensing would erase the victim-set asymmetry — reactor accidents entering the ledger at finite weight and combustion deaths at full weight — producing a different constraint with different beneficiaries and a different classification.',
    'Under the expected-value sibling, the same licensing apparatus loses its protective justification and computes as delay machinery serving incumbents; under this reading it retains a genuine, if costly, protective function. Per-seat classifications flip for every stakeholder.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer position: one of three live readings of the acceptable-risk kernel; sibling readings instantiate different constraints.').

omega_variable(
    tail_aversion_origin,
    'Is catastrophic-tail prioritization a deep feature of human risk cognition and social trust maintenance (closer to a natural regularity), or a constructed political settlement that identifiable interests assembled and maintain?',
    'Cross-cultural and historical comparison of risk constitutions — societies that adopted tail-dominant rules without fossil incumbents or anti-nuclear movements — plus experimental work separating dread-weighting from interest-driven framing.',
    'If primarily constructed, the coordination claim weakens and the classification slides toward snare; if primarily cognitive, the rule approximates an unavoidable feature of democratic risk acceptance and the measured costs are partly the price of legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_aversion_origin, empirical, 'Whether the tail-dominant weighting is cognitive bedrock or constructed settlement.').

omega_variable(
    substitution_discount_validity,
    'Does the discounting of distributed fossil-combustion mortality track a genuine moral distinction (diffuse causation, historical normalization, reversibility) or motivated protection of incumbent generation?',
    'Within-reading consistency tests: apply the same discount to other distributed harms (road fatalities, industrial pollution) in the same jurisdictions and observe whether the weighting survives; revealed-preference studies separating dread from magnitude.',
    'If the discount is principled, part of the measured cost is the honest price of the reading''s protective function; if motivated, the discount is interest protection dressed as philosophy and effective extraction rises sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_discount_validity, empirical, 'Validity of the asymmetric weighting of distributed versus concentrated harm.').

omega_variable(
    displacement_counterfactual_scale,
    'How much aggregate mortality and emissions is actually attributable to suppressed nuclear buildout, net of hydro, renewables, demand growth, and efficiency gains?',
    'Energy-system counterfactual modeling calibrated to licensing timelines, using phase-out episodes as natural experiments.',
    'Small displacement shrinks the victim set and softens the hybrid reading toward pure coordination; large displacement confirms the asymmetric-transfer structure and pushes toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_counterfactual_scale, empirical, 'Scale of the substituted-harm victim set attributable to the rule.').

omega_variable(
    advocacy_identity_lock_depth,
    'Is the anti-nuclear organizations'' persistence driven by structural funding dependencies or by internalized identity fusion with the opposition role?',
    'Post-victory behavior analysis: organizations that continue campaigning after their specific demands are met reveal identity-driven persistence rather than grievance-driven persistence.',
    'Identity-locked beneficiaries sustain pressure beyond material interest, raising effective suppression above what funding flows alone predict; a purely structural account would predict rapid demobilization once licensing hardened past any realistic reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advocacy_identity_lock_depth, empirical, 'Structural versus internalized persistence mechanism in the beneficiary coalition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_energy__catastrophic_tail_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(acce_tr_t10, observed).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(acce_tr_t20, observed).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(acce_tr_t30, observed).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(acce_tr_t40, observed).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_energy__catastrophic_tail_dominant, theater_ratio, 50, 0.4).
narrative_ontology:measurement_basis(acce_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(acce_be_t10, observed).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(acce_be_t20, observed).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(acce_be_t30, observed).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 40, 0.66).
narrative_ontology:measurement_basis(acce_be_t40, observed).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_energy__catastrophic_tail_dominant, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(acce_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(acce_su_t10, observed).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(acce_su_t20, observed).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 30, 0.78).
narrative_ontology:measurement_basis(acce_su_t30, observed).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 40, 0.8).
narrative_ontology:measurement_basis(acce_su_t40, observed).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_energy__catastrophic_tail_dominant, suppression_requirement, 50, 0.8).
narrative_ontology:measurement_basis(acce_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_energy__catastrophic_tail_dominant, acceptable_risk_energy__option_value_preserving).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'acceptable risk' in energy policy covers three structurally distinct decision rules, not one observable-dependent claim. This story instantiates the catastrophic_tail_dominant reading (asymmetric weighting: concentrated catastrophe weighted near-infinitely, distributed combustion mortality discounted); acceptable_risk_energy__expected_value_dominant instantiates uniform mortality-per-TWh valuation; acceptable_risk_energy__option_value_preserving instantiates portfolio-flexibility valuation. Each has its own victim set, its own epsilon, and its own classification; they are linked here because the tail-dominant reading's institutional victories (moratoria, ratcheted licensing) remove the nuclear leg from the option-value portfolio and supply the baseline that expected-value critics measure against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
