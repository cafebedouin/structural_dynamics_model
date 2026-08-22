% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__expected_value_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value Risk Acceptability for Energy Systems
 *   domain: energy_policy/risk_assessment/public_safety_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the expected_value_dominant reading of
 *   the acceptable_risk_for_energy kernel. The reading holds that energy
 *   system acceptability is determined by annualized expected costs
 *   (probability × consequence integrals) and climate benefits. Rare events
 *   are weighted by their probability-consequence product, not by their raw
 *   consequence magnitude. Under this reading, nuclear power exits the victim
 *   set when its expected-value profile is favorable versus fossil
 *   alternatives; tail-risk framings face low active suppression (the PRA
 *   community absorbs them methodologically); and waste disposal is treated
 *   as a solvable engineering challenge with manageable institutional
 *   continuity requirements. The constraint operates as a coordination
 *   mechanism: it provides a shared calculus that lets regulators, operators,
 *   and analysts converge on licensing decisions without negotiating every
 *   tail scenario ad hoc. The claimed type is rope — genuine coordination
 *   with minimal coercive overhead — but the metrics show modest
 *   extractiveness that has oscillated over the interval, particularly rising
 *   during periods of nuclear expansion advocacy (1970s, 2000s renaissance)
 *   where the framework was deployed to override local opposition.
 *
 * KEY AGENTS:
 *   - nuclear_operators: Primary beneficiary (institutional/arbitrage) — the reading legitimizes their risk profile and licensing pathway
 *   - grid_planners: Beneficiary (organized/mobile) — the calculus enables portfolio optimization across energy sources
 *   - climate_policy_analysts: Beneficiary (analytical/arbitrage) — expected-value framing integrates climate externalities into the same metric
 *   - radiological_protection_professionals: Beneficiary (organized/mobile) — professional authority rests on the PRA framework
 *   - downwind_communities: Payer (powerless/trapped to constrained) — bear residual risk when expected-value calculus underweights low-probability high-consequence events affecting them specifically
 *   - future_generations: Payer (powerless/trapped) — inherit waste stewardship burden under institutional continuity assumptions
 *   - tail_risk_advocates: Excluded (moderate/constrained) — catastrophic_tail_dominant reading proponents systematically excluded from PRA standard-setting bodies
 *   - comparative_risk_pragmatists: Observer (powerful/mobile) — hold the comparative_risk_dominant reading; engage selectively
 *   - regulatory_analysts: Observer (institutional/analytical) — apply the framework; see its structural limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.32).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.22).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.32).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value Risk Acceptability for Energy Systems").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "energy_policy/risk_assessment/public_safety_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'ceea6f8f-b269-40f1-9e92-1b05c802381a').
narrative_ontology:cs_kernel_codification('ceea6f8f-b269-40f1-9e92-1b05c802381a', distributed).
narrative_ontology:cs_authority_grounding('ceea6f8f-b269-40f1-9e92-1b05c802381a', expertise).
narrative_ontology:cs_interpretation_layer_present('ceea6f8f-b269-40f1-9e92-1b05c802381a').
narrative_ontology:cs_reading_relation('ceea6f8f-b269-40f1-9e92-1b05c802381a', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('ceea6f8f-b269-40f1-9e92-1b05c802381a', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('ceea6f8f-b269-40f1-9e92-1b05c802381a', foundational, expected_value_sufficiency_for_acceptability).
narrative_ontology:cs_axiom_status(expected_value_sufficiency_for_acceptability, holdable).
narrative_ontology:cs_axiom_grounding('ceea6f8f-b269-40f1-9e92-1b05c802381a', expected_value_sufficiency_for_acceptability, instrumental).
narrative_ontology:cs_axiom('ceea6f8f-b269-40f1-9e92-1b05c802381a', secondary, tail_risk_methodologically_absorbable).
narrative_ontology:cs_axiom_status(tail_risk_methodologically_absorbable, holdable).
narrative_ontology:cs_axiom_grounding('ceea6f8f-b269-40f1-9e92-1b05c802381a', tail_risk_methodologically_absorbable, empirically_contingent).
narrative_ontology:cs_axiom('ceea6f8f-b269-40f1-9e92-1b05c802381a', secondary, institutional_continuity_for_waste_stewardship).
narrative_ontology:cs_axiom_status(institutional_continuity_for_waste_stewardship, holdable).
narrative_ontology:cs_axiom_grounding('ceea6f8f-b269-40f1-9e92-1b05c802381a', institutional_continuity_for_waste_stewardship, conventional).
narrative_ontology:cs_reference_frame('ceea6f8f-b269-40f1-9e92-1b05c802381a', wash1400_pra_framework).
narrative_ontology:cs_drift_state('ceea6f8f-b269-40f1-9e92-1b05c802381a', post_fukushima_reassessment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ceea6f8f-b269-40f1-9e92-1b05c802381a', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, grid_planners).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, climate_policy_analysts).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, radiological_protection_professionals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, downwind_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, future_generations).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, probabilistic_risk_assessment_framework).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, expected_value_decision_rule).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, linear_no_threshold_debate_position).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, intergenerational_discounting_applicability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate nuclear plants under licensing frameworks built on PRA/expected-value methodology. The framework legitimizes their risk profile, enables standardized safety cases, and provides predictable regulatory pathways. They can deploy capital across jurisdictions that share the framework. Their profits and asset values depend on the framework's continued dominance.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators, beneficiary,
    institutional, generational, arbitrage, global).

% Use expected-value risk calculus to optimize generation portfolios across nuclear, fossil, and renewable sources. The common metric enables integrated resource planning and climate compliance modeling. They can switch analytical frameworks if a better one emerges, but the installed base of PRA-based tools creates switching costs.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, grid_planners, beneficiary,
    organized, generational, mobile, national).

% Rely on expected-value framing to integrate climate externalities (social cost of carbon) into the same risk metric used for radiological risk. This enables direct comparison of nuclear vs. fossil vs. renewable risk-benefit profiles. Their analytical authority is tied to the framework's acceptance in integrated assessment models.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, climate_policy_analysts, beneficiary,
    analytical, generational, arbitrage, global).

% Professional community (ICRP, national bodies) whose standards and authority rest on the PRA/expected-value framework. They maintain the methodology (ICRP publications, IAEA safety standards) and license practitioners. They can migrate to alternative frameworks but would lose the institutional capital built over decades.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, radiological_protection_professionals, beneficiary,
    organized, biographical, mobile, global).

% Communities near nuclear facilities bear the site-specific tail risk (low-probability, high-consequence releases) that the expected-value calculus smooths into population-level averages. Their geographic immobility, property ties, and lack of political leverage make exit nearly impossible. They receive no direct compensation for the risk differential between their local exposure and the population average.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, downwind_communities, payer,
    powerless, biographical, trapped, local).

% Inherit the waste stewardship burden under the reading's assumption that institutional continuity will maintain repository safety over geological timescales. They have no voice in the present framework's adoption, no exit from their temporal position, and no recourse if institutional continuity fails. The 'solvable engineering challenge' framing makes their burden invisible in present-day calculus.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations, payer,
    powerless, civilizational, trapped, global).

% Proponents of the catastrophic_tail_dominant reading (environmental NGOs, some independent scientists, affected community networks). They are systematically excluded from PRA standard-setting bodies (ICRP, IAEA committees, national regulatory advisory groups) where the expected-value framework's parameters are defined. They can publish, litigate, and mobilize public opinion but cannot shape the technical baseline.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_advocates, excluded,
    moderate, biographical, constrained, global).

% Hold the comparative_risk_dominant reading: nuclear risk is acceptable only relative to coal emissions and climate catastrophe. They engage with the expected-value framework instrumentally — using it when it supports their comparative argument, challenging it when it doesn't. They have institutional access (energy ministries, climate agencies) and can switch analytical frames freely.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, comparative_risk_pragmatists, observer,
    powerful, biographical, mobile, national).

% Apply the PRA/expected-value framework in licensing decisions (NRC, ONR, CNSC, etc.). They see the framework's structural limits — where it works for routine operations and where it strains against tail events. They can propose methodological improvements but operate within the framework's paradigm; paradigm shifts come from political/external pressure, not internal analysis.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, regulatory_analysts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common probabilistic metric (expected annual cost/risk) that enables regulators, operators, and planners to compare disparate energy technologies (nuclear, fossil, hydro, renewables) on a single scale, facilitating licensing, portfolio optimization, and climate policy integration without ad hoc negotiation of every risk dimension.
% TRANSFER_FUNCTION: Moves tail-risk exposure from the nuclear operator's balance sheet to downwind communities (geographically concentrated low-probability high-consequence risk) and to future generations (waste stewardship burden under institutional continuity assumptions), while moving climate-risk reduction benefits to the broader population via nuclear's displacement of fossil generation.
% ABSENT_VOICES: Downwind communities and future generations are structurally absent from the standard-setting bodies that define the PRA framework's parameters (ICRP, IAEA, national regulatory advisory committees). Tail-risk advocates (catastrophic_tail_dominant reading proponents) are excluded from the technical baseline-setting process, though not from public discourse. Their objection would be that the probability-consequence product systematically underweights events that are ruinous for the specific people who experience them.
% DISAPPEARANCE_RATIONALE: If the expected-value framework vanished overnight, nuclear licensing would lose its common metric — regulators would revert to deterministic design-basis approaches or ad hoc case-by-case judgments, portfolio optimization would lose its risk integration capability, and the climate-nuclear risk comparison would lose its shared calculus. The nuclear industry would face higher regulatory uncertainty; climate policy would lose a key integration tool; downwind communities might gain stronger site-specific protections but lose the (diffuse) benefit of standardized safety goals.
% FOUNDING_PROBLEM: Post-WWII nuclear expansion required a rational method to compare reactor risks against each other and against non-nuclear risks, to allocate limited regulatory and engineering resources, and to justify public acceptance. The 1975 Reactor Safety Study (WASH-1400) established the PRA/expected-value framework as that method.
% FOUNDING_PROBLEM_CORROBORATION: The nuclear industry and IAEA attest the problem remains live: new reactor designs (SMRs, Gen IV, fusion) need the framework for licensing. Tail-risk advocates and some independent scholars (e.g., Perrow on normal accidents, Shrader-Frechette on probabilistic risk assessment ethics) attest the founding problem was mis-specified — the framework was built to enable deployment, not to protect the most vulnerable from ruin. The 1979 NRC critique of WASH-1400 (Lewis report) and post-Chernobyl/Fukushima reassessments corroborate the contested status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.32, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).
:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32 at interval end) reflects that the framework does transfer some tail risk to downwind communities and future generations without their consent — but the transfer is diffuse and the framework also provides genuine coordination value (standardized licensing, interoperable safety cases, climate integration). Suppression (0.22) is low because tail-risk framings are not banned; they are methodologically absorbed (Level 2/3 PRA, uncertainty distributions) or marginalized through professional gatekeeping rather than explicit prohibition. Theater ratio (0.18) rose during nuclear renaissance periods when the framework was performatively deployed to legitimize expansion, then fell as the community incorporated lessons. Accessibility collapse (0.45) is moderate: alternative risk framings exist and are technically expressible within the framework (just not dominant). Resistance (0.55) is substantial: the catastrophic_tail_dominant reading maintains active opposition, especially post-Chernobyl and post-Fukushima.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (operators, planners, analysts), the constraint appears as rope: a working coordination mechanism that solves the genuine problem of comparing disparate energy risks. From the payer seats (downwind communities, future generations), the same structure operates as extraction — their specific vulnerabilities are smoothed into probability distributions that underweight the events that would devastate them. The engine computes this divergence from the structural data. The claimed rope type reflects the authoring seat's judgment that the coordination function is primary and the extraction is secondary/diffuse; the engine may compute tangled_rope for the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators, grid planners, and climate analysts are structural beneficiaries — the constraint subsidizes their decision-making and legitimizes their preferred technology. Radiological protection professionals benefit professionally. Downwind communities and future generations are payers — they bear the residual tail risk and waste burden, but their exit options differ: downwind communities are trapped/constrained (geographic immobility, property ties); future generations are trapped (no exit from temporal position). Tail-risk advocates are excluded from standard-setting but not from public discourse. Comparative-risk pragmatists hold a sibling reading and engage from a mobile position. Regulatory analysts sit at the analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — comparing disparate energy risks on a common metric to enable rational portfolio decisions — remains live (status: contested). The constraint has not become a piton: it is actively maintained because the coordination problem persists (new reactor designs, small modular reactors, fusion licensing all need the framework). Theater spikes correlate with advocacy cycles, not functional decay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the expected-value reading a genuine coordination mechanism for energy governance, or a frame that benefits nuclear incumbents by backgrounding tail risks that would dominate under the catastrophic-tail reading?',
    'Compare regulatory outcomes in jurisdictions that adopted expected-value PRA (US NRC post-1975 Reactor Safety Study lineage) versus those that adopted precautionary/tail-dominant frameworks (Germany post-Chernobyl, Austria, post-Fukushima Japan). Track whether nuclear deployment correlates with the reading, controlling for other factors.',
    'If the reading systematically produces nuclear-favorable outcomes where tail-dominant readings do not, the beneficiary structure is real and the constraint leans toward tangled_rope. If outcomes converge, the reading is a genuine coordination mechanism (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, empirical, 'Whether the expected-value reading functions as coordination or extraction relative to sibling readings').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low measured suppression of tail-risk framing structural (institutional openness to alternative readings) or internalized (the PRA community has incorporated tail-risk considerations into its own methodology, making explicit suppression unnecessary)?',
    'Trace the history of PRA methodology: did Level 2/3 PRA, uncertainty analysis, and severe accident management guidelines emerge from internal methodological evolution or external pressure? If internal, suppression is partially internalized — the constraint carries its own containment.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target (tail-risk framing) carries the suppression with it through methodological absorption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression of competing risk framings within the PRA community').

omega_variable(
    waste_disposal_solvability,
    'Does the reading''s treatment of waste disposal as a ''solvable engineering challenge'' reflect genuine technical consensus, or does it depend on deferring the intergenerational burden to future institutional arrangements that may not materialize?',
    'Track repository licensing outcomes (WIPP, Onkalo, Yucca Mountain cancellation, Cigeo) against the ''solvable engineering challenge'' claim. Measure whether institutional continuity assumptions hold over the required timescales.',
    'If waste disposal solvability depends on institutional continuity that cannot be guaranteed, the reading''s beneficiary structure extends to future generations as implicit victims — shifting the constraint toward tangled_rope or snare depending on enforceability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(waste_disposal_solvability, preference, 'Whether waste disposal solvability is a technical fact or an institutional bet').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.12).
narrative_ontology:measurement(acce_tr_t10, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 10, 0.1).
narrative_ontology:measurement(acce_tr_t20, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 20, 0.22).
narrative_ontology:measurement(acce_tr_t30, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 30, 0.28).
narrative_ontology:measurement(acce_tr_t40, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 40, 0.2).
narrative_ontology:measurement(acce_tr_t50, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 50, 0.18).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(acce_be_t10, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(acce_be_t20, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(acce_be_t30, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(acce_be_t40, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 40, 0.38).
narrative_ontology:measurement(acce_be_t50, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 50, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(acce_su_t10, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(acce_su_t20, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(acce_su_t30, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 30, 0.15).
narrative_ontology:measurement(acce_su_t40, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(acce_su_t50, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 50, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, nuclear_licensing_framework).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, waste_disposal_regulation).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, climate_mitigation_portfolio_optimization).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, radiological_protection_standards).

% DUAL FORMULATION NOTE:
% Part of the acceptable_risk_for_energy constraint family. This reading (expected_value_dominant) coordinates energy portfolio decisions by providing a common metric. The catastrophic_tail_dominant reading constrains the same domain by elevating irreversibility and intergenerational burden. The comparative_risk_dominant reading coordinates by relative comparison only. All three share the kernel 'acceptable risk for energy' but instantiate different constraints with different beneficiary/victim structures and ε values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, powerless, 0.85).
constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, institutional, 0.1).
constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, organized, 0.25).
constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, analytical, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
