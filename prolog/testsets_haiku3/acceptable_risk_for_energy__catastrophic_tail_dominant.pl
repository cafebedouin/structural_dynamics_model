% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__catastrophic_tail_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_acceptable_risk_for_energy__catastrophic_tail_dominant, []).

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
 *   constraint_id: acceptable_risk_for_energy__catastrophic_tail_dominant
 *   human_readable: Catastrophic Tail Risk Dominance in Energy Acceptability
 *   domain: risk_assessment/energy_policy/public_safety
 *
 * SUMMARY:
 *   Energy policy governance confronts a fundamental framing choice: whether
 *   to evaluate energy systems using expected-value optimization (probability
 *   × consequence) or tail-risk dominance (low-probability catastrophic
 *   outcomes override probabilistic discounting). This constraint story
 *   instantiates the catastrophic-tail-dominant reading, in which
 *   irreversibility and intergenerational burden outweigh expected-value
 *   optimization. Nuclear energy enters the victim set through this reading's
 *   application of tail-risk weighting. The constraint is CLAIMED as
 *   tangled_rope (coordinates intergenerational precaution while extracting
 *   from nuclear operators) and the metrics reflect high suppression of
 *   comparative-risk framing and steady extraction accumulation since
 *   Chernobyl. The sibling readings—expected-value-dominant and
 *   comparative-risk-dominant—represent structurally distinct constraints
 *   with different beneficiary/victim topologies and different ε values. This
 *   reading alone is instantiated here; the siblings are referenced as
 *   separate constraints in the network.
 *
 * KEY AGENTS:
 *   - low_carbon_energy_advocates: agenda-setters (institutional power) — frame tail-risk dominance as axiomatic and suppress probabilistic trade-off discourse
 *   - nuclear_industry_operators: payers (powerful, constrained exit) — absorb extraction through regulatory delays and suppressed comparative-risk arguments
 *   - future_generations: beneficiaries and victims (powerless, trapped exit) — benefit from tail-risk protection, pay via delayed decarbonization if nuclear is restricted
 *   - nuclear_accident_host_regions: payers (moderate power, trapped exit) — bear geographic concentration of tail risk
 *   - fossil_fuel_providers: excluded (powerful, structurally) — excluded from comparative-risk framing that would expose their own tail risks
 *   - probabilistic_risk_analysts: excluded (organized, constrained) — their methodologies are suppressed as illegitimate when tail-dominance presupposed
 *   - regulatory_agencies: observers (institutional) — manage operational contradiction between tail-dominance presupposition and multi-domain energy-system governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.79).
domain_priors:theater_ratio(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__catastrophic_tail_dominant, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__catastrophic_tail_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__catastrophic_tail_dominant, "Catastrophic Tail Risk Dominance in Energy Acceptability").
narrative_ontology:topic_domain(acceptable_risk_for_energy__catastrophic_tail_dominant, "risk_assessment/energy_policy/public_safety").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__catastrophic_tail_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__catastrophic_tail_dominant, '01a12e92-a43c-4cc0-8ba0-2b9ca3e45977').
narrative_ontology:cs_kernel_codification('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', distributed).
narrative_ontology:cs_authority_grounding('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', distributed).
narrative_ontology:cs_reading_relation('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', acceptable_risk_for_energy__expected_value_dominant, forecloses).
narrative_ontology:cs_reading_relation('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', acceptable_risk_for_energy__comparative_risk_dominant, influences).
narrative_ontology:cs_axiom('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', foundational, irreversible_harm_tail_risk_dominates_probability).
narrative_ontology:cs_axiom_status(irreversible_harm_tail_risk_dominates_probability, holdable).
narrative_ontology:cs_axiom_grounding('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', irreversible_harm_tail_risk_dominates_probability, deontological).
narrative_ontology:cs_axiom('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', foundational, intergenerational_equity_prohibits_legacy_tail_risk).
narrative_ontology:cs_axiom_status(intergenerational_equity_prohibits_legacy_tail_risk, holdable).
narrative_ontology:cs_axiom_grounding('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', intergenerational_equity_prohibits_legacy_tail_risk, deontological).
narrative_ontology:cs_reference_frame('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', precautionary_intergenerational_governance).
narrative_ontology:cs_drift_state('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', contemporary_climate_urgency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('01a12e92-a43c-4cc0-8ba0-2b9ca3e45977', '').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, low_carbon_energy_advocates).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_coalition).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_accident_host_regions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the framing that tail-risk dominance (low-probability catastrophic outcomes) should override expected-value optimization in energy policy. Advocates exclude from consideration the relative risk of competing energy sources and demand absolute safety thresholds for nuclear. Derives authority and legitimacy from the precautionary principle and intergenerational justice arguments. Suppresses probabilistic trade-off framing in policy discourse.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, low_carbon_energy_advocates, agenda_setter,
    institutional, generational, arbitrage, global).

% Bears the constraint's extraction through construction delays, regulatory costs, and suppression of comparative-risk arguments that would justify nuclear as lower-carbon alternative to fossil fuels. Must defend operations under a framework that treats tail-risk dominance as axiomatic, preventing cost-benefit arguments from being heard in policy forums.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_industry_operators, payer,
    powerful, biographical, constrained, global).

% Benefit from the constraint's protection against tail-risk catastrophe (permanent habitat loss, multigenerational contamination). Simultaneously bear the cost of slower decarbonization if nuclear capacity is restricted and fossil-fuel generation fills the gap, accumulating climate damage that tail-risk framing does not fully price.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, beneficiary,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations, payer).
narrative_ontology:stakeholder_non_agent(acceptable_risk_for_energy__catastrophic_tail_dominant, future_generations).

% Bear the catastrophic tail risk directly: if an accident occurs, they absorb permanent displacement, habitat damage, and healthcare burden. The constraint's tail-dominance framing legitimizes their precaution and exclusion from sites, but also concentrates risk geographically on host regions that accepted nuclear siting decades ago under different risk narratives.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_accident_host_regions, payer,
    moderate, civilizational, trapped, regional).

% Are structurally excluded from the comparative-risk discussion that would present fossil fuels' own tail risks (climate tipping points, cascading infrastructure failure) as commensurate with nuclear tail risk. The constraint's suppression of probabilistic trade-off framing indirectly benefits them by removing nuclear from consideration as climate mitigation.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, fossil_fuel_energy_providers, excluded,
    powerful, biographical, constrained, global).

% Benefits from the constraint's establishment of intergenerational-justice and precautionary-principle language, which they use to justify aggressive climate action (though some within the coalition dispute whether restricting nuclear actually serves their mitigation goals, creating internal fissure).
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_coalition, beneficiary,
    organized, generational, mobile, global).

% Are formally excluded from policy discourse when tail-dominance is presupposed: their core tools (probability × consequence weighting, relative risk ranking, uncertainty quantification) are treated as illegitimate framings rather than inputs to be weighed against precautionary concerns. Their objections to the framing are suppressed as 'defending industry' rather than entertained as methodological critique.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, probabilistic_risk_analysts, excluded,
    organized, biographical, constrained, global).

% Must implement and enforce policy based on the tail-dominance reading while managing the operational contradiction: they administer approval processes that presuppose tail-risk dominance yet must also license energy systems knowing fossil alternatives carry their own tail risks (climate catastrophe, grid cascades, resource wars). This contradiction is managed by theatrical separation of risk-assessment domains.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, regulatory_agencies, observer,
    institutional, generational, analytical, national).

% Are excluded from pricing nuclear tail risk competitively; government backstop for nuclear liability means reinsurers cannot price the true tail risk into premiums. This exclusion from market discovery suppresses the signal that would otherwise reveal whether tail-risk dominance framing matches the aggregate risk-pricing of decentralized actors with full information.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__catastrophic_tail_dominant, insurance_and_reinsurance_markets, excluded,
    powerful, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__catastrophic_tail_dominant, low_carbon_energy_advocates).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__catastrophic_tail_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framing for intergenerational risk allocation: articulates that low-probability catastrophic harms with irreversible effects (permanent habitat loss, thousand-year half-life contamination) should not be discounted by their probability in cost-benefit analysis. Coordinates precautionary norm-setting across jurisdictions and time horizons.
% TRANSFER_FUNCTION: Transfers burden from present-day decision-makers (who enjoy low-carbon energy's climate and cost benefits) to future generations and host regions (who bear the tail risk). Present-day actors benefit from the constraint's restriction of nuclear through slower climate change; future generations both benefit (no Fukushima-scale accident) and pay (higher baseline climate damage if fossil fuels fill the gap).
% ABSENT_VOICES: Fossil-fuel producers (structurally excluded from comparative-risk framing); probabilistic-risk analysts whose methods presuppose trade-offs; unborn future generations (not present in policy forums); potential host regions for future nuclear sites who would make different risk-allocation choices than existing sites made decades ago.
% DISAPPEARANCE_RATIONALE: If the tail-dominance constraint disappeared and expected-value framing resumed, nuclear would re-enter energy portfolios as a climate-mitigation option, construction timelines would compress, and the relative weighting of nuclear tail risk vs. climate tail risk would move toward the comparative-risk reading. Policy legitimacy would shift from precautionary grounds to expected-value grounds, changing which actors' preferences dominate decision-making.
% FOUNDING_PROBLEM: Chernobyl and Fukushima demonstrated that human error, natural disaster, and cascading failure can defeat engineering safeguards; the permanent displacement of millions and multi-century contamination raised the question whether probabilistic risk assessment adequately accounts for irreversible harm. The founding problem: how should energy systems that pose tail risks be governed ethically when the tail's consequences are permanent and intergenerational?
% FOUNDING_PROBLEM_CORROBORATION: The precautionary-principle reading attests the founding problem is live and permanent: any nuclear deployment carries irreducible tail risk. The comparative-risk reading (sibling constraint) attests the founding problem has been partially solved: modern reactor designs and international oversight have reduced tail-risk frequency far below historical rates, making coal and climate catastrophe the comparatively larger tail risk. Independent post-accident analysis from Fukushima (Japanese regulatory commissions, international technical reviews) supports the live-hazard reading while also highlighting engineering improvements that the comparative-risk reading emphasizes. No corroboration exists outside the energy-policy stakeholder set; the founding problem is contested entirely within the domain.
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__catastrophic_tail_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__catastrophic_tail_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__catastrophic_tail_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__catastrophic_tail_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__catastrophic_tail_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint's operation persistently extracts capacity and policy optionality from the nuclear energy industry while benefiting low-carbon advocates and climate mitigation frameworks. Suppression is higher still (0.79) because maintaining tail-dominance framing requires active suppression of comparative-risk arguments (fossil-fuel tail risks, climate catastrophe tail risks) and probabilistic-methodology legitimacy. Theater is moderate (0.41) and rising: the constraint coordinates genuine intergenerational-justice concern, but an increasing share of enforcement activity is devoted to suppressing alternative framings rather than addressing the underlying physical tail risk. The measurement series trace forty years from Chernobyl (1986, when the constraint begins to crystallize) through Fukushima (2011, when extraction accelerates) to present (2026, when suppression and theater ratio stabilize at elevated levels). Extraction plateaus after 2018 because the constraint has already achieved its primary goal (nuclear expansion halted in most developed economies); further extraction requires increasingly theatrical enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seat experience radically different constraint topologies from the same arrangement. The advocates see coordination of an intergenerational-justice norm and precautionary governance. Nuclear operators see extraction: suppression of their cost-benefit arguments, regulatory delays, and permanent restriction of their market. The engine derives these divergences from power + exit + beneficiary/victim declarations, not from authored per-seat claims. The constraint is claimed as tangled-rope (coordination + asymmetric extraction) precisely because this seat divergence is structurally true: genuine intergenerational-justice concern is present AND that concern is weaponized to suppress comparative-risk framing that would justify nuclear deployment.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations map as follows: low_carbon_energy_advocates benefit from the constraint's restriction of nuclear and amplification of climate-precaution language. nuclear_industry_operators are victims: their policy optionality is restricted, their cost-benefit arguments are suppressed, their market is constrained. future_generations are declared both (secondary_role): they benefit from tail-risk protection against Fukushima-scale accidents, but pay via slower decarbonization if fossil fuels fill the gap. The directionality derivation chain follows beneficiary/victim → power → exit_options. Advocates: beneficiary + institutional + arbitrage → d ≈ 0.15–0.25 (full beneficiary end). Operators: victim + powerful + constrained → d ≈ 0.80–0.85 (full target end). Host regions: victim + moderate + trapped → d ≈ 0.90 (trapped target). This derivation is independent of the claimed type and reflects the structural asymmetry that makes the constraint extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live (tail-risk catastrophe can still occur, Fukushima showed it, ongoing reactor operation carries non-zero tail risk). But the constraint's operation has drifted from solving that problem to suppressing alternative problem-framings. A nuclear operator cannot argue that comparative-risk analysis suggests nuclear is lower-tail-risk than coal or climate change; that framing is excluded from legitimate policy discourse. The constraint exhibits mandatrophy in its secondary function: it was founded to prevent tail-risk catastrophe, but it now persists largely to prevent comparative-risk arguments from being heard. Theater_ratio rising from 0.18 to 0.41 captures this drift: more of the constraint's enforcement activity is devoted to maintaining the framing (excluding voices, delegitimizing probabilistic methods) than to materially reducing tail risk. The mandatrophy is partial, not total: the intergenerational-justice coordination function is still live. But the constraint's persistence is increasingly dependent on suppression rather than on solving the founding problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tail_risk_vs_climate_catastrophe,
    'If nuclear tail-risk dominance is axiomatic, why is climate catastrophe tail-risk not equally axiomatic in energy-acceptability calculus?',
    'Comparative risk accounting: quantify the tail-risk profiles of fossil-fuel energy systems (climate tipping points, cascading grid failures, resource wars) against nuclear tail-risk profiles (reactor accident, meltdown, long-term contamination). The question resolves if both tail risks are weighted equally in policy discourse or if one is systematically suppressed.',
    'If climate catastrophe tail-risk is suppressed while nuclear tail-risk is foregrounded, the constraint is revealed as asymmetrically extractive rather than coordinatively precautionary. If both are equally weighted, the constraint is genuinely coordinative and the extraction is cost-of-coordination rather than asymmetric rent-taking.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tail_risk_vs_climate_catastrophe, empirical, 'Whether tail-risk dominance is uniformly applied or selectively invoked to suppress nuclear.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression of comparative-risk framing structural (regulatory exclusion, institutional gatekeeping, funding constraints on research) or internalized (advocates and analysts believe tail-dominance is correct and voluntarily exclude comparative-risk arguments)?',
    'Post-exit suppression trajectory: if comparative-risk arguments re-emerge in policy forums when regulatory barriers are lifted (e.g., energy-policy pivot away from tail-dominance, shift in administrative power), the suppression was structural. If suppression persists even after barriers are removed, it is internalized.',
    'If structural, the constraint''s effective suppression is higher than the authored metric suggests—the target (nuclear industry) carries the suppression only while enforcement machinery is active. If internalized, the constraint''s effective suppression is lower—the exclusion persists after the mechanism is removed because advocates have absorbed the framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression of comparative-risk framing is external enforcement or internal normalization.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the catastrophic_tail_dominant reading logically foreclose the expected_value_dominant reading within a single decision-making framework, or do they coexist as rival readings held by different institutional actors?',
    'Meta-level analysis: if a single decision-maker (a court, a regulatory body, a government agency) can coherently hold both readings simultaneously (applying tail-dominance in some contexts, expected-value in others), they coexist. If the two readings generate contradictory directives when applied to the same decision, foreclosure obtains.',
    'If foreclosure: this reading''s core axiom (irreversibility dominates probability) logically rules out expected-value optimization; the sibling reading is not merely disagreed-with but structurally incompatible. If coexistence: the readings compete in public discourse but neither eliminates the other within any single framework—institutional actors and different jurisdictions hold different readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether catastrophic-tail dominance logically excludes expected-value framing or merely conflicts with it institutionally.').

omega_variable(
    intergenerational_benefit_quantification,
    'How should the intergenerational benefit (avoided tail-risk catastrophe) be quantified and compared against the intergenerational cost (delayed decarbonization, higher baseline climate damage)?',
    'Integrated assessment modeling: construct scenarios where tail-dominance framing restricts nuclear vs. scenarios where expected-value framing permits nuclear, and calculate cumulative intergenerational burden under each path (tail-risk probability × long-term consequences vs. climate-damage probability × long-term consequences).',
    'If the avoided nuclear tail-risk damage is smaller than the additional climate damage from delayed decarbonization, the constraint extracts more from future generations than it protects them—reversed victim status. If the avoided nuclear damage exceeds climate damages, the constraint is genuinely coordinatively protective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_benefit_quantification, empirical, 'Whether tail-dominance framing actually protects future generations or imposes greater aggregate burden on them.').

omega_variable(
    reading_context_kernel_decomposition,
    'Are catastrophic_tail_dominant, expected_value_dominant, and comparative_risk_dominant three readings of one contested kernel, or three distinct constraints sharing rhetorical similarity?',
    'Structural test: if all three are responses to the SAME core standing commitment (''what acceptability threshold applies to energy systems?''), they are readings of one kernel. If they presuppose different foundational claims and cannot be held in tension within any coherent framework, they are separate constraints.',
    'If kernel readings: the relationship is committer-layer (presupposition-level disagreement; contention is between different framings of the same standing arrangement). If separate constraints: each has its own beneficiary/victim topology and none forecloses the others—they compete at the policy level rather than the logical level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_context_kernel_decomposition, conceptual, 'Whether the three risk-framing variants constitute a reading family (committer-axis) or three independent constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__catastrophic_tail_dominant, 1986, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1986, 0.18).
narrative_ontology:measurement(acce_tr_t1995, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(acce_tr_t2005, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2005, 0.31).
narrative_ontology:measurement(acce_tr_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2011, 0.37).
narrative_ontology:measurement(acce_tr_t2018, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2018, 0.39).
narrative_ontology:measurement(acce_tr_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(acce_be_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1986, 0.42).
narrative_ontology:measurement(acce_be_t1995, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 1995, 0.51).
narrative_ontology:measurement(acce_be_t2005, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(acce_be_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2011, 0.64).
narrative_ontology:measurement(acce_be_t2018, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2018, 0.66).
narrative_ontology:measurement(acce_be_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t1986, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1986, 0.52).
narrative_ontology:measurement(acce_su_t1995, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 1995, 0.61).
narrative_ontology:measurement(acce_su_t2005, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(acce_su_t2011, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2011, 0.75).
narrative_ontology:measurement(acce_su_t2018, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2018, 0.77).
narrative_ontology:measurement(acce_su_t2026, acceptable_risk_for_energy__catastrophic_tail_dominant, suppression_requirement, 2026, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__catastrophic_tail_dominant, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__catastrophic_tail_dominant, 0.14).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__expected_value_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, nuclear_waste_disposal_long_term_governance).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__catastrophic_tail_dominant, climate_mitigation_policy_decarbonization_pathway).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel acceptable_risk_for_energy. The kernel concerns the legitimacy standard for accepting low-probability high-consequence energy systems. The catastrophic_tail_dominant reading (this constraint) weights irreversibility and intergenerational burden above expected-value calculation. The expected_value_dominant reading applies probability × consequence weighting without presupposition of tail dominance. The comparative_risk_dominant reading evaluates nuclear acceptability only relative to alternative energy tail risks (coal emissions, climate catastrophe), not against an absolute threshold. These three readings instantiate three structurally distinct constraints with different beneficiary/victim sets, different ε values, and different suppression mechanisms. Each is a separate constraint story; the network edges document their committer-axis kinship. The ε-invariance principle: each reading has its own invariant ε (the extraction inherent to that reading's presuppositions), not observable-dependent ε. The three readings differ in what they presuppose, not in how they measure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__catastrophic_tail_dominant, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
