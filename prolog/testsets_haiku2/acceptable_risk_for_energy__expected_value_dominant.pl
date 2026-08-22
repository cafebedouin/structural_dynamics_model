% ============================================================================
% CONSTRAINT STORY: acceptable_risk_for_energy__expected_value_dominant
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: acceptable_risk_for_energy__expected_value_dominant
 *   human_readable: Expected-Value Risk Dominant Acceptability Standard
 *   domain: energy_policy/risk_governance/climate
 *
 * SUMMARY:
 *   This constraint instantiates the expected-value-dominant reading of the
 *   acceptable-risk-for-energy kernel. The reading operationalizes risk
 *   acceptability through expected-cost-and-benefit calculus: rare
 *   high-consequence events are weighted by their probability of occurrence,
 *   and the product (expected cost per year) is compared to alternatives
 *   (coal emissions externalities, climate-driven famine risk, grid stability
 *   cost). Under this reading, nuclear power exits the victim set if expected
 *   annual cost is lower than the alternative energy mix's expected cost,
 *   regardless of tail-risk magnitude. The constraint is the authority
 *   structure that adjudicates energy policy acceptability on these grounds.
 *   Competitors to this reading — the catastrophic-tail-dominant and
 *   comparative-risk-dominant readings — contest whether irreversibility and
 *   intergenerational ethics should create a precautionary floor, and whether
 *   the comparison frame is appropriate. This story narrates the
 *   expected-value reading's operation, beneficiaries, and suppression
 *   landscape as the reading itself frames it.
 *
 * KEY AGENTS:
 *   - Nuclear Operators (Institutional): set investment, decommissioning, and waste-handling policies on the grounds that expected value favors nuclear expansion; benefit from the expected-value frame because it legitimizes continued operation.
 *   - Energy Economics Profession (Organized): maintains and refines expected-value methodology; their career capital and publication stream depend on the frame's legitimacy.
 *   - Grid Decarbonization Agenda (Organized Coalition): climate scientists, renewable-energy advocates, and decarbonization policymakers who benefit from nuclear expansion as a low-carbon baseload source; expected value frames their climate urgency.
 *   - Waste-Host Communities (Organized/Powerless): bear the concentrated burden of intergenerational waste stewardship; geographically trapped by sunk storage infrastructure; argue that 100,000-year institutional-memory demands cannot coherently be subjected to annual expected-value calculus.
 *   - Future Generations (Excluded/Analytically Represented): structurally barred from the decision-making table; unable to consent to or even comprehend the tail risks they will inherit.
 *   - Tail-Risk-Sensitive Publics (Organized/Moderate): accept climate urgency but argue for precautionary floors on irreversible decisions; experience suppression via framing (told their concerns are 'scientifically illiterate' rather than engaging the precaution question).
 *   - Fossil-Fuel Incumbents (Institutional/Powerful): resist the frame indirectly by funding tail-risk sensitivity campaigns; their suppression is market-based rather than regulatory.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, 0.68).
domain_priors:suppression_score(acceptable_risk_for_energy__expected_value_dominant, 0.52).
domain_priors:theater_ratio(acceptable_risk_for_energy__expected_value_dominant, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, extractiveness, 0.68).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(acceptable_risk_for_energy__expected_value_dominant, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(acceptable_risk_for_energy__expected_value_dominant, tangled_rope).
narrative_ontology:human_readable(acceptable_risk_for_energy__expected_value_dominant, "Expected-Value Risk Dominant Acceptability Standard").
narrative_ontology:topic_domain(acceptable_risk_for_energy__expected_value_dominant, "energy_policy/risk_governance/climate").

domain_priors:requires_active_enforcement(acceptable_risk_for_energy__expected_value_dominant).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(acceptable_risk_for_energy__expected_value_dominant, 'd739b725-92cf-4491-9b95-caa02a977212').
narrative_ontology:cs_kernel_codification('d739b725-92cf-4491-9b95-caa02a977212', fixed_text).
narrative_ontology:cs_authority_grounding('d739b725-92cf-4491-9b95-caa02a977212', expertise).
narrative_ontology:cs_interpretation_layer_present('d739b725-92cf-4491-9b95-caa02a977212').
narrative_ontology:cs_reading_relation('d739b725-92cf-4491-9b95-caa02a977212', acceptable_risk_for_energy__catastrophic_tail_dominant, coexists_with).
narrative_ontology:cs_reading_relation('d739b725-92cf-4491-9b95-caa02a977212', acceptable_risk_for_energy__comparative_risk_dominant, coexists_with).
narrative_ontology:cs_axiom('d739b725-92cf-4491-9b95-caa02a977212', foundational, expected_value_sufficient_for_energy_decisions).
narrative_ontology:cs_axiom_status(expected_value_sufficient_for_energy_decisions, holdable).
narrative_ontology:cs_axiom_grounding('d739b725-92cf-4491-9b95-caa02a977212', expected_value_sufficient_for_energy_decisions, empirically_contingent).
narrative_ontology:cs_axiom('d739b725-92cf-4491-9b95-caa02a977212', secondary, intergenerational_discount_rate_legitimate).
narrative_ontology:cs_axiom_status(intergenerational_discount_rate_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('d739b725-92cf-4491-9b95-caa02a977212', intergenerational_discount_rate_legitimate, instrumental).
narrative_ontology:cs_reference_frame('d739b725-92cf-4491-9b95-caa02a977212', annual_expected_value_optimization).
narrative_ontology:cs_drift_state('d739b725-92cf-4491-9b95-caa02a977212', contemporary_climate_urgency_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d739b725-92cf-4491-9b95-caa02a977212', '2026-06-12T14:23:15Z').
narrative_ontology:cs_kernel_id(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, energy_economics_profession).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, grid_decarbonization_agenda).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, waste_host_communities).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, future_generations).
narrative_ontology:constraint_victim(acceptable_risk_for_energy__expected_value_dominant, tail_risk_sensitive_publics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(acceptable_risk_for_energy__expected_value_dominant, grid_decarbonization_coalition).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, expected_value_calculus_supremacy).
narrative_ontology:constraint_vindicates(acceptable_risk_for_energy__expected_value_dominant, anthropogenic_climate_change_urgency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utility companies and reactor operators who set expansion, decommissioning, and waste-handling schedules on the grounds that expected-value analysis favors nuclear. They commission economic studies, lobby regulatory bodies to adopt expected-value standards, and enforce waste-storage protocols under federal mandate. Under this frame, they expand nuclear capacity because the expected annual cost of a 1-in-100,000-year meltdown is lower than coal-emissions externality. They benefit from the expected-value framing because it legitimizes continued operation and defers waste costs to future generations. Their exit from this arrangement is high-quality: they can switch to solar/wind if the frame changed, but prefer nuclear under current rules.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators, agenda_setter,
    institutional, generational, arbitrage, national).

% Economists, risk analysts, and energy-systems modelers who maintain and refine expected-value methodology. Their career capital, publication streams, and consulting revenue depend on the legitimacy of expected-value framing. They author the cost-benefit analyses that regulators cite, define what counts as a 'valid' risk assessment, and train the next generation of energy professionals in the methodology. They benefit from the frame by being positioned as objective arbiters of risk. Their exit is mobile: if the frame shifted to precautionary or comparative standards, they would retrain and publish in the new framework (many already do in parallel).
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, energy_economics_profession, beneficiary,
    organized, biographical, mobile, global).

% Climate scientists, renewable-energy advocates, grid-modernization engineers, and decarbonization policymakers who benefit from nuclear expansion as a proven low-carbon baseload source. The expected-value frame legitimizes nuclear as part of the climate solution, allowing them to avoid the political cost of defending nuclear on its own terms (safety, waste). They rely on the frame to hold together a coalition of climate urgency and nuclear expansion. Their exit is constrained: they could switch to pure-renewable pathways, but fossil-fuel incumbency and grid-stability concerns make them dependent on nuclear in the medium term. Under this frame, they gain a way to frame nuclear as instrumental to their founding problem (climate urgency).
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, grid_decarbonization_coalition, beneficiary,
    organized, generational, constrained, global).

% Local and regional communities where waste-storage sites (Yucca Mountain, interim repositories, geologically designated zones) are located or proposed. They bear the concentrated burden of 100,000-year intergenerational stewardship: maintaining institutional memory, preventing institutional collapse that would abandon the site, monitoring for seal failure or social forgetting, and managing the cultural/spiritual relationship to radioactivity for timescales beyond human institutional precedent. They argue that the expected-value formula is incoherent at 100,000-year horizons because institutional memory caps at ~10,000 years. Their exit is trapped: they cannot relocate the storage infrastructure once sited, cannot refuse it without federal coercion, and cannot opt out of the future generations' burden. The frame extracts from them by rendering their concerns 'irrational fear' rather than legitimate governance uncertainty.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, waste_host_communities, payer,
    powerless, civilizational, trapped, national).

% Parties born 100+ years in the future who will inherit the waste-stewardship burden and the institutional arrangements (or failures) that present generations establish. They have no seat at the table where expected-value calculations are made, no ability to consent to or object to tail risks they will encounter, and no ability to comprehend or influence the infrastructure they will maintain. The expected-value formula treats them as irrelevant because the calculation is annual and assumes institutional continuity. Their exclusion is structural: future people cannot participate in present decision-making by definition.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, future_generations, excluded,
    powerless, civilizational, trapped, universal).

% Organized constituencies (some indigenous nations, environmental organizations, precautionary-ethics advocates, some religious communities) who accept climate urgency but argue that irreversibility and intergenerational ethics demand a precautionary floor on expected-value calculus. They experience suppression not through overt coercion but through epistemic delegitimization: their concerns are framed as 'scientifically illiterate' or 'economically irrational' rather than as valid alternative framings. They are formally included in comment periods and hearings but excluded from authoritative decision-making (policy is set by economists and operators, not by tail-risk constituencies). Their constraint-specific exit is constrained: they can leave the debate but the siting and operation continues; they cannot opt out of risk. They analytically observe the frame's operation and articulate the precautionary alternative.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, tail_risk_sensitive_publics, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(acceptable_risk_for_energy__expected_value_dominant, tail_risk_sensitive_publics, observer).

% Coal, oil, and natural-gas companies that resist nuclear expansion (their market competitor) by indirectly funding tail-risk sensitivity campaigns, research into coal-plant scrubbing, and alternative energy narratives. They are formally excluded from energy-policy decision-making (regulators do not consult them) but materially affect the constraint's operation through market pressure and lobbying. Their stake is to maintain the expected-value frame's vulnerability by amplifying tail-risk narratives, hoping that precautionary sentiment will stall nuclear and preserve fossil-fuel market share. Their exit is arbitrage-quality: they can switch to renewable projects or divest entirely, but prefer fossil incumbency. They benefit from low suppression of tail-risk framing (the frame's own weakness).
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, fossil_fuel_incumbents, excluded,
    powerful, biographical, arbitrage, global).

% Nuclear Regulatory Commission, Department of Energy, and state public utilities commissions that operationalize the expected-value standard in licensing and decommissioning decisions. They adopt the framework as the legitimate decision rule, reject applications that fail the expected-value test, and enforce compliance with approved risk thresholds. They benefit from the frame by having a clear, defensible decision criterion (expected value) rather than a value-laden political choice. Their analytical exit is clean: if the frame changed, they would adopt the new standard, but they have institutional interest in some stable criterion.
narrative_ontology:constraint_stakeholder(acceptable_risk_for_energy__expected_value_dominant, regulatory_authorities, agenda_setter,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(acceptable_risk_for_energy__expected_value_dominant, nuclear_operators).
narrative_ontology:fixing_cost_class(acceptable_risk_for_energy__expected_value_dominant, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, mathematically defensible risk-assessment methodology for energy policy: risk acceptability is determined by comparing annual expected costs (probability × consequence) across energy sources, enabling rational aggregation of climate urgency, grid stability, safety, and economic efficiency into one decision rule. Solves the coordination problem of how to weight incommensurable goods (climate risk, reactor safety, economic development, intergenerational equity) into a single scalar.
% TRANSFER_FUNCTION: Moves intergenerational waste-stewardship costs from present-generation energy producers and consumers (who benefit from low-carbon electricity) to future generations (who inherit the institutional and environmental burden of 100,000-year storage stewardship). Also redistributes institutional authority from local/community voices to centralized expert bodies (economists, energy professionals) who define acceptable risk.
% ABSENT_VOICES: Future generations and tail-risk-sensitive communities are structurally excluded. Future generations cannot be consulted; tail-risk publics are formally included in comment processes but excluded from decision-making authority (policy is set by operators and economists, not by precaution advocates). Fossil-fuel incumbents are excluded from regulatory discussion but materially affect outcomes via market competition.
% DISAPPEARANCE_RATIONALE: If the expected-value frame vanished overnight, energy policy would reorganize around precautionary or comparative standards. Nuclear expansion would face higher approval thresholds, waste-host communities would gain standing in the decision rule, intergenerational ethics would be explicitly weighted, and the grid-decarbonization agenda would shift to accelerate renewable+storage investments instead of relying on nuclear as baseload. The absence of this frame would rearrange the entire energy economy and the allocation of future-generation responsibility.
% FOUNDING_PROBLEM: How to make rational, transparent decisions about energy-source acceptability when multiple incommensurable values (climate stability, safety, cost, equity, intergenerational burden) must be aggregated into policy. The founding problem was posed in the 1970s as nuclear expansion faced growing safety concerns: regulators needed a defensible way to say whether nuclear was 'acceptable' given its low-probability high-consequence risks. Expected-value calculus provided a mathematically universal answer: convert all values to annual-cost equivalents and compare. This solved the coordination problem of how to defend nuclear in the face of public concern without appearing to be favoring operators.
% FOUNDING_PROBLEM_CORROBORATION: Regulators, energy economists, and operators affirm the founding problem is still live: energy decarbonization urgency and grid-stability concerns require a transparent risk-assessment standard, and expected-value calculus fulfills that role. Tail-risk publics, waste-host communities, and intergenerational-ethics advocates contend the founding problem was solved for a different purpose (defending nuclear) and that the real problem is how to include intergenerational ethics in the calculus; expected value is not a solution to that problem, it is an evasion of it. Independent corroboration from outside the benefiting parties is limited: most intergenerational ethics literature explicitly critiques expected-value calculus as inadequate for irreversible intergenerational decisions, but this critique is systematically excluded from regulatory decision-making forums (part of the suppression mechanism).
narrative_ontology:disappearance_verdict(acceptable_risk_for_energy__expected_value_dominant, world_rearranges).
narrative_ontology:founding_problem_status(acceptable_risk_for_energy__expected_value_dominant, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(acceptable_risk_for_energy__expected_value_dominant, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(acceptable_risk_for_energy__expected_value_dominant, 'none', 1).
narrative_ontology:epsilon_provenance(acceptable_risk_for_energy__expected_value_dominant, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(acceptable_risk_for_energy__expected_value_dominant, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(acceptable_risk_for_energy__expected_value_dominant_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the frame externalizes intergenerational costs onto future generations who bear the waste burden without participating in the benefit calculus. The expected-value formula is advantageous to present-generation operators and decarbonization coalitions (they capture the climate-stability benefit and avoid the waste-storage cost). Theater ratio is moderate (0.41) because the expected-value calculus is partially functional (cost-benefit analysis does guide investment) but increasingly performs a legitimacy function as tail-risk sensitivity rises. The measurement series show extractiveness and theater rising together: as public concern for tail risks intensifies, the reading increasingly operates as a barrier (suppressing alternative framings) rather than as a transparent methodology. Suppression is moderate (0.52) because tail-risk framing is not overt-coerced but rather epistemically delegitimized ('unscientific,' 'economically illiterate'); publics can voice tail-risk concerns, but they encounter active institutional resistance and are excluded from authoritative decision-making forums. The accessibility collapse is low (0.48) because alternatives do remain conceivable — the catastrophic-tail and comparative-risk readings are articulated in policy debates, published in journals, and held by organized constituencies. Resistance is high (0.71) because intergenerational ethics, indigenous land-use rights, and waste-host-community opposition provide live structural opposition to the frame.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (nuclear operators, energy economists), this is a transparent, universal risk methodology that fairly aggregates costs and benefits; the constraint is rope and near-natural-law. From the payer seats (waste hosts, future generations, tail-risk publics), the same constraint is a mechanism to externalize costs they cannot consent to; it is snare. The gap reflects not disagreement about facts (all parties accept the expected-value formula is mathematically correct) but about whether a 10,000-year decision can coherently be subjected to annual expected-value analysis. The engine should compute this as a tangled-rope from the operator seat (genuine coordination function for grid decarbonization + asymmetric extraction of intergenerational costs) and as snare-candidate from the payer seats (extracted from, excluded from decision-making, suppressed via epistemic delegitimization). The measurement series showing rising theater_ratio and persistent high resistance are diagnostic: if the formula were universally accepted as legitimate, theater and suppression would be low and resistance near-zero.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear operators: role=agenda_setter (set policy on expected-value grounds, enforce waste-handling by federal mandate); power=institutional; direction toward the constraint d ≈ 0.2 (they benefit substantially from the frame — it legitimizes expansion; their exit is arbitrage-quality, they can shift to other energy forms but prefer nuclear under this frame). Energy-economics profession: role=beneficiary (their expertise is validated); power=organized; d ≈ 0.15 (they benefit from the frame's authority but have minimal stake in the outcome). Grid-decarbonization coalition: role=beneficiary (frame legitimizes nuclear as climate solution); power=organized; d ≈ 0.25 (moderate benefit; they would still exist under alternative frames but face legitimacy questions). Waste-host communities: role=payer (bear the intergenerational burden); power=powerless; d ≈ 0.85 (the frame extracts from them by rendering their concerns 'outside the formula'). Future generations: role=excluded; power=powerless; d ≈ 0.95 (the formula's time discount effectively treats them as irrelevant). Tail-risk-sensitive publics: role=payer + secondary_role=observer (bear suppression; analytically observe the frame's operation); power=moderate; d ≈ 0.72 (the frame suppresses their alternative framing; they lack institutional power to shift it). The payer seats should compute as snare-adjacent because they are systematically excluded from the decision formula; the beneficiary seats should compute as rope-beneficiary (they benefit from genuine coordination of energy policy). The per-seat divergence is the point: the expected-value frame is a coordination mechanism for climate decarbonization that functions as extraction for waste-bearers and future generations.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (aggregating energy-source risks to decarbonize the grid) is live and urgent. Its status in this reading is contested: the reading claims the founding problem justifies expected-value calculus as sufficient, while tail-risk and comparative readings claim additional principles (precaution, context-sensitivity) are required. The disappearance verdict is world_rearranges: if this framing vanished, energy policy would reorganize around precautionary or comparative frameworks, nuclear expansion would face different approval thresholds, and waste-host communities would have standing in the decision rule. The constraint has not become mandatrophic (its function is not yet atrophied), but it is under sustained structural pressure from high resistance and rising theater. The mandatrophy risk is concentrated: if climate urgency ever relaxes, the reading's legitimacy will collapse because it depends on the urgency to override tail-risk sensitivity. Future mandatrophy would occur if the founding problem (grid decarbonization) is solved via renewable + storage breakthroughs, because the frame's authority relies on urgency. The analysis of why mandatrophy has not occurred is: the frame persists because climate urgency is still rising, the beneficiary coalition is organized and powerful, and the payer seats (waste hosts, future generations) lack institutional voice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_conflict,
    'Is expected-value optimization the correct framework for evaluating nuclear risk, or does catastrophic irreversibility demand a precautionary floor independent of probability?',
    'Intergenerational ethics and irreversibility theory: if waste remains hazardous for 100,000+ years (institutional memory ceiling ~10,000), what decision rule respects the sovereignty of parties who cannot consent to (or even comprehend) the tail risk?',
    'If precaution dominates, the expected-value framing is a cover story for present-generation discount-rate tyranny; nuclear becomes snare. If expected value holds, catastrophic_tail_dominant reading is factually overweighted. The reading relations (coexists_with vs forecloses) turn on this resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_conflict, conceptual, 'Whether expected-value calculus can coherently govern risks that persist beyond institutional memory horizons.').

omega_variable(
    comparative_vs_absolute_framing,
    'Should nuclear risk acceptability be determined relative to alternatives (coal emissions, climate-driven famine) or against an absolute safety threshold?',
    'If determined relative: this reading coexists_with comparative_risk_dominant (both describe decision rules, different referents). If absolute: both readings are incoherent — the decision rule is derived from comparison anyway, just implicit rather than explicit.',
    'Determines whether the three sibling readings partition the space of defensible positions (three incompatible rules) or whether this reading and comparative_risk_dominant are actually describing the same operation with different emphasis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparative_vs_absolute_framing, conceptual, 'Whether the reading''s decision rule is relative or absolute.').

omega_variable(
    suppression_of_tail_sensitivity,
    'Is the low suppression (0.52) of tail-risk framing actually a lack of enforcement power, or a reflection that the reading''s core axiom (expected-value dominance) is structurally weak against irreversibility intuitions?',
    'Observe resistance patterns: if tail-risk publics remain organized and vocal despite no overt enforcement, the suppression is low by lack of institutional power, not by lack of need. If tail-risk framing weakens after one cycle of policy loss, suppression is structural.',
    'If structural: the reading is genuinely weak and relies on coalition with grid-decarbonization urgency to persist. If power-limited: the 0.52 reflects political contingency, and suppression could rise sharply with institutional investment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_tail_sensitivity, empirical, 'Whether the measured suppression reflects theoretical weakness or practical power limits.').

omega_variable(
    waste_disposal_engineering_status,
    'Is waste disposal genuinely a ''solvable engineering challenge'' (per the structural delta claim), or does the intergenerational institutional-memory gap make it an unsolvable governance problem even if technical storage is proven?',
    'Examine 70-year historical track record: has any institutional arrangement successfully committed future generations to anything for 100,000 years? Test against precedent (religious covenants, architectural persistence, legal frameworks).',
    'If solvable: nuclear cost-benefit is transparent; waste is a sunk cost, not an open-ended liability. If unsolvable: the beneficiaries (present-generation operators, decarbonization agenda) externalize a governance problem onto future generations. Reclassifies from tangled_rope (asymmetric extraction + coordination) to snare (pure extraction, governance trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(waste_disposal_engineering_status, conceptual, 'Whether intergenerational governance can bind future institutions to waste stewardship.').

omega_variable(
    climate_urgency_as_authority_override,
    'Does the framing of anthropogenic climate catastrophe as imminent and existential justify overriding tail-risk sensitivity via expected-value calculus?',
    'Compare decision timescales: if climate decision horizon is 30 years but nuclear waste hazard horizon is 100,000 years, are they comparable aggregates in one formula?',
    'If yes: climate urgency legitimizes the expected-value frame and suppresses tail-risk framing as a luxury absent publics cannot afford. If no: the urgency is orthogonal to the intertemporal ethics question. Affects whether the reading is defensible or relies on asymmetric framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_urgency_as_authority_override, preference, 'Whether climate urgency overrides intergenerational precaution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(acceptable_risk_for_energy__expected_value_dominant, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(acce_tr_t0, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(acce_tr_t0, observed).
narrative_ontology:measurement(acce_tr_t8, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(acce_tr_t8, observed).
narrative_ontology:measurement(acce_tr_t16, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(acce_tr_t16, observed).
narrative_ontology:measurement(acce_tr_t24, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(acce_tr_t24, observed).
narrative_ontology:measurement(acce_tr_t32, acceptable_risk_for_energy__expected_value_dominant, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(acce_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(acce_be_t0, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 0, 0.51).
narrative_ontology:measurement_basis(acce_be_t0, observed).
narrative_ontology:measurement(acce_be_t8, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(acce_be_t8, observed).
narrative_ontology:measurement(acce_be_t16, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(acce_be_t16, observed).
narrative_ontology:measurement(acce_be_t24, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(acce_be_t24, observed).
narrative_ontology:measurement(acce_be_t32, acceptable_risk_for_energy__expected_value_dominant, base_extractiveness, 32, 0.68).
narrative_ontology:measurement_basis(acce_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(acce_su_t0, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(acce_su_t0, observed).
narrative_ontology:measurement(acce_su_t8, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 8, 0.43).
narrative_ontology:measurement_basis(acce_su_t8, observed).
narrative_ontology:measurement(acce_su_t16, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 16, 0.47).
narrative_ontology:measurement_basis(acce_su_t16, observed).
narrative_ontology:measurement(acce_su_t24, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(acce_su_t24, observed).
narrative_ontology:measurement(acce_su_t32, acceptable_risk_for_energy__expected_value_dominant, suppression_requirement, 32, 0.52).
narrative_ontology:measurement_basis(acce_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(acceptable_risk_for_energy__expected_value_dominant, resource_allocation).
narrative_ontology:boltzmann_floor_override(acceptable_risk_for_energy__expected_value_dominant, 0.12).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__catastrophic_tail_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, acceptable_risk_for_energy__comparative_risk_dominant).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, intergenerational_waste_governance).
narrative_ontology:affects_constraint(acceptable_risk_for_energy__expected_value_dominant, climate_urgency_as_decision_override).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the acceptable-risk-for-energy kernel. The three sibling readings (expected-value-dominant, catastrophic-tail-dominant, comparative-risk-dominant) describe three incompatible decision rules for energy acceptability. They share the same beneficiaries/victims but attribute different constraint types to different seats. Expected-value-dominant is tangled-rope from operator seats (coordination + extraction) and snare-candidate from payer seats (extraction + exclusion); catastrophic-tail-dominant should be rope or mountain from payer seats; comparative-risk-dominant should be rope from decarbonization seats. The three stories together instantiate the kernel's reading contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(acceptable_risk_for_energy__expected_value_dominant, powerless, 0.87).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
