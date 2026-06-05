% ============================================================================
% CONSTRAINT STORY: expected_value_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_expected_value_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: expected_value_reading
 *   human_readable: Expected-Value Risk Framing in Energy Policy
 *   domain: energy_policy/risk_governance/climate_mitigation
 *
 * SUMMARY:
 *   The expected-value reading of acceptable risk in energy policy frames
 *   risk governance as a probability-weighted comparison of outcomes across
 *   energy alternatives (coal, nuclear, renewable). This reading instantiates
 *   ONE interpretation of the contested kernel of 'acceptable risk for energy
 *   systems.' The expected-value approach legitimizes nuclear energy
 *   expansion by assigning low probability to catastrophic accident scenarios
 *   (reactor breach, long-term waste failure) and averaging them against
 *   high-probability benefits of carbon mitigation. This reading coexists
 *   with two sibling readings: the catastrophic-tail reading, which
 *   foregrounds tail-event scenarios and assigns them decision-theoretic
 *   weight independent of probability; and the precautionary reading, which
 *   inverts the burden of proof (requiring proof of safety rather than proof
 *   of unacceptable risk). The expected-value reading concentrates its
 *   victims among powerless agents (fossil fuel workers, tail-risk
 *   populations) and diffuse pollution bearers, while concentrating benefits
 *   among climate mitigation advocates and the nuclear industry. The
 *   constraint exhibits high suppression (0.52) through technical language
 *   barriers, parameter uncertainty that obscures policy sensitivity, and
 *   institutional dominance that treats alternative framings as irrational.
 *   Theater ratio (0.48) reflects that expected-value calculation performs
 *   mathematical precision while obscuring that the inputs (probability
 *   estimates, value assignments, discount rates, temporal horizons) are
 *   contested.
 *
 * KEY AGENTS:
 *   - Coal Community Workers: Primary victim (powerless/trapped/local) — concentrated displacement cost, no exit option, no participation in decision framing
 *   - Tail-Risk Populations: Primary victim (powerless/trapped/local) — geographically trapped near waste sites or reactor locations, bear uncompensated catastrophic risk, no exit option
 *   - Diffuse Pollution Bearers: Secondary victim (moderate/constrained/national) — benefit from coal phase-out (reduced respiratory illness, acid rain) but distribution of benefits is unequal; constrained by inability to organize diffuse interest
 *   - Climate Mitigation Advocates: Primary beneficiary (institutional/arbitrage/global) — expected-value framing legitimizes nuclear expansion as climate solution; high agency in policy framing
 *   - Nuclear Industry: Primary beneficiary (institutional/arbitrage/global) — benefits from favorable probability weighting and policy legitimacy; high arbitrage capacity
 *   - Precautionary Voices (Organized): Secondary actor (organized/constrained/national) — possess expertise to challenge expected-value assumptions but face professional credibility barriers and institutional dismissal
 *   - Regulatory Risk Assessment Apparatus: Institutional actor (institutional/arbitrage/national) — maintains expected-value as official legitimacy gate; perceives own process as degraded (piton perspective)
 *   - Analytical Observer: Observing context (analytical/analytical/universal) — risks naturalizing methodological choice as rational requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(expected_value_reading, 0.58).
domain_priors:suppression_score(expected_value_reading, 0.52).
domain_priors:theater_ratio(expected_value_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(expected_value_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(expected_value_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(expected_value_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(expected_value_reading, tangled_rope).
narrative_ontology:human_readable(expected_value_reading, "Expected-Value Risk Framing in Energy Policy").
narrative_ontology:topic_domain(expected_value_reading, "energy_policy/risk_governance/climate_mitigation").

domain_priors:requires_active_enforcement(expected_value_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(expected_value_reading, formalized).
narrative_ontology:cs_authority_grounding(expected_value_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(expected_value_reading).
narrative_ontology:cs_kernel_id(expected_value_reading, acceptable_risk_for_energy).
narrative_ontology:cs_reading_relation(expected_value_reading, catastrophic_tail_reading, coexists_with).
narrative_ontology:cs_reading_relation(expected_value_reading, precautionary_reading, coexists_with).
narrative_ontology:cs_axiom(expected_value_reading, foundational, risk_is_commensurable).
narrative_ontology:cs_axiom_status(risk_is_commensurable, holdable).
narrative_ontology:cs_axiom(expected_value_reading, foundational, probability_weights_decision_gate).
narrative_ontology:cs_axiom_status(probability_weights_decision_gate, holdable).
narrative_ontology:cs_reference_frame(expected_value_reading, rational_expected_utility_maximization).
narrative_ontology:cs_drift_state(expected_value_reading, contemporary_energy_policy, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(expected_value_reading, climate_mitigation_advocates).
narrative_ontology:constraint_beneficiary(expected_value_reading, nuclear_industry).
narrative_ontology:constraint_victim(expected_value_reading, fossil_fuel_workers).
narrative_ontology:constraint_victim(expected_value_reading, diffuse_pollution_bearers).
narrative_ontology:constraint_victim(expected_value_reading, tail_risk_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL COMMUNITY WORKER (SNARE) — Trapped within the logic of expected-value comparison that averages away their concentrated loss (mine closure, pension defaults, community dissolution) against diffuse climate benefits accruing to distant populations. No exit option; bears full extraction cost through dislocation without receiving mitigation. Expected-value framing legitimizes their sacrifice as a rational trade-off they had no say in making.
constraint_indexing:constraint_classification(expected_value_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TAIL-RISK POPULATION (SNARE) — Expected-value calculation assigns probability-weighted average risk, which systematically underweights catastrophic tail events (reactor breach, waste storage failure, weapons-grade proliferation). Powerless, geographically trapped near waste sites or potential accident zones. Cannot opt out of the probability distribution; bears concentrated cost if tail event occurs. Expected-value methodology legitimizes accepting uncompensated tail risk.
constraint_indexing:constraint_classification(expected_value_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DIFFUSE POLLUTION BEARERS (TANGLED ROPE) — Receive genuine coordination benefit: expected-value rejection of coal reduces respiratory illness, acid rain, and carbon accumulation that would otherwise harm them. But also bear costs: nuclear expansion may bring waste transport risk or accident proximity; expected-value framing obscures distribution of who receives benefits vs. who bears risks. Constrained by limited capacity to organize diffuse interest; benefits are real but unequally distributed.
constraint_indexing:constraint_classification(expected_value_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NUCLEAR INDUSTRY (ROPE) — Primary beneficiary. Expected-value framing legitimizes nuclear expansion by averaging accident risk against climate benefit. Experiences constraint as coordination problem: communicating risk numbers to policy enables deployment. Net extraction runs toward this agent through favorable probability weighting and access to decision-making infrastructure. High arbitrage capacity — can exit to different technologies or regulatory regimes.
constraint_indexing:constraint_classification(expected_value_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CLIMATE MITIGATION ADVOCATES (ROPE) — Primary beneficiary. Expected-value framing enables nuclear as climate solution by assigning low probability to accident scenarios and averaging them against avoided carbon. Experiences constraint as coordination: the calculation translates climate urgency into policy action. Net beneficiary — the methodology legitimizes their preferred technology. High arbitrage capacity — can shift to renewable-only strategy if political conditions change.
constraint_indexing:constraint_classification(expected_value_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PRECAUTIONARY VOICES / ORGANIZED OPPOSITION (TANGLED ROPE) — Possess genuine coordination function: forcing articulation of tail-risk assumptions and uncertainty ranges that expected-value frameworks often hide. But experience extraction through being systematically dismissed as 'anti-science' or 'irrational fear-mongering' when they highlight model assumptions. Constrained by technical capacity barriers — need expert credibility to challenge expected-value orthodoxy in policy contexts. Both constrain and coordinate.
constraint_indexing:constraint_classification(expected_value_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGULATORY RISK ASSESSMENT APPARATUS (PITON) — Maintains expected-value methodology as the official legitimacy gate for energy approvals despite known limitations: parameter uncertainty, tail-event under-weighting, incommensurable value scales. Theater: the apparatus performs risk comparison with mathematical precision while obscuring that the inputs (probability estimates, value assignments) are contested. Persists through institutional inertia — alternatives (precautionary, catastrophic) have been theoretically rejected even as practical limitations of expected-value become visible. Sees its own process as degraded but maintains it.
constraint_indexing:constraint_classification(expected_value_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal analytical standpoint, expected-value risk calculation appears as an immutable logical requirement: any rational decision under uncertainty must compare probability-weighted outcomes. This perspective sees the expected-value constraint as following from rationality itself. However, the structural data contradicts the mountain classification — the false summit detector will identify this as naturalization of a contingent methodological choice. The constraint's beneficiaries, suppression mechanisms, and theater ratio reveal that 'rational calculation' is doing work for a specific constituency.
constraint_indexing:constraint_classification(expected_value_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(expected_value_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(expected_value_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(expected_value_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(expected_value_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(expected_value_reading, TR),
    TR >= 0.70.

:- end_tests(expected_value_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The expected-value reading legitimizes policy that concentrates benefits (nuclear approval, climate mitigation) among powerful agents while distributing costs (worker displacement, tail risk) to powerless agents. The extraction is not maximal because the benefits are partly genuine (climate mitigation is real) and because some diffuse pollution bearers do receive countervailing benefits. Measurement trajectory (0.42→0.50→0.58) reflects growing institutional dominance of expected-value framing over time as it becomes the official methodology for energy policy, gradually foreclosing alternative readings and consolidating beneficiary control. Suppression (0.52): Moderate. Significant barriers include technical language that excludes non-expert stakeholders, uncertainty in probability estimates that makes assumptions non-transparent, and institutional gatekeeping that treats precautionary and tail-risk framings as irrational within the expected-value framework. But suppression is not total — organized precautionary voices exist and can challenge the framing, though with difficulty. Theater ratio (0.48): Moderate. Expected-value calculation performs mathematical precision (generating point estimates, confidence intervals, sensitivity analyses) while obscuring that policy-relevant inputs are contested: probability estimates for tail events are uncertain by orders of magnitude, value conversions between incommensurable goods (lives, livelihoods, climate damages) are normatively assumed, discount rates embed temporal preferences, and scope boundaries (which populations count in the calculation) are politically determined. The theater has increased over the interval as expected-value framing has become more institutionalized and technically sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between this reading's beneficiaries and victims is stark. The nuclear industry and climate advocates experience expected-value as coordination (rope) — it enables policy action and legitimizes their preferred alternative. Powerless agents experience it as pure extraction (snare) — it legitimizes imposing uncompensated risk and displacement costs. The precautionary voices see it as extraction with coordination function (tangled rope) — they provide genuine challenge but face institutional suppression. The regulatory apparatus sees it as degraded ritual (piton) — mathematically sophisticated but obscuring the political choices embedded in the inputs. The analytical observer risks seeing it as immutable rationality (mountain) — the false summit detector reveals this as naturalization. None of these perspectives is 'incorrect' — they reflect real structural differences in how the constraint operates for agents in different positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness is derived from its structural position relative to the expected-value constraint. Coal community workers (powerless/trapped) experience maximum extraction because they bear concentrated losses without receiving benefits or having exit options; the expected-value framing legitimizes their sacrifice as rational. Tail-risk populations experience similar maximum extraction — they are trapped in geographic proximity to the mechanism of possible catastrophe. Diffuse pollution bearers experience tangled extraction — they receive genuine benefits (reduced illness) but unequally distributed and without control over the decision frame. Climate advocates and nuclear industry (institutional/arbitrage) experience low or negative effective extraction because they benefit from the framing and maintain high arbitrage capacity. Precautionary voices (organized/constrained) experience tangled extraction — they provide genuine coordination function (forcing articulation of assumptions) but face suppression through credentialing barriers. The regulatory apparatus (institutional/arbitrage) sees its own process as degraded but maintains it through inertia. The analytical observer risks naturalizing the expected-value choice as rational requirement, producing a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that expected-value risk framing IS a legitimate coordination mechanism for certain agents (climate advocates needing policy traction, nuclear industry needing legitimacy) while simultaneously functioning as an extraction mechanism for other agents (powerless workers, tail-risk populations). The constraint is genuinely tangled rope: it solves a real coordination problem (how to compare incommensurable energy alternatives) while extracting from those who cannot participate in setting the comparison frame. The classification is not 'expected-value is either coordination or extraction' but 'expected-value is coordination for positioned agents and extraction for displaced agents.' The constraint's mandatrophy is resolved by disaggregating the seemingly-neutral 'risk calculation' into its distribution: who frames it, who benefits, who bears uncompensated cost, who has exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    probability_estimation_validity,
    'Are probability estimates for tail events (reactor accidents, waste containment failure, weapons proliferation) derived from empirical data, expert elicitation, or structural assumption? How does uncertainty in the probability assignments affect the expected-value conclusion?',
    'Sensitivity analysis: vary probability estimates within confidence intervals; compare policy recommendations across the range. Historical frequency analysis: do actual accident rates match pre-incident probability estimates? Structural assumption check: what prior beliefs about technology reliability or human error are embedded in the probability assignments?',
    'If probabilities are uncertain by >0.5 orders of magnitude: expected-value comparison is under-determined (different probability estimates flip the policy recommendation). If actual accident rates exceed pre-incident estimates: the expected-value methodology is systematically under-counting tail risk. Resolves whether the constraint is genuinely a coordination problem (rope) or an extraction mechanism that legitimizes accepting uncompensated risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(probability_estimation_validity, empirical, 'Whether probability estimates for tail events are empirically grounded or structurally assumed').

omega_variable(
    incommensurable_value_comparison,
    'Can respiratory illness prevented by coal phase-out, climate damages averted, concentrated worker displacement, and tail-event catastrophic harm be compared on a single utility scale, or does expected-value calculation require collapsing incommensurable values into a common metric that obscures real disagreements?',
    'Value mapping: identify what common unit (QALY, dollars, disability-adjusted life-years) the calculation uses to render different harms comparable. Stakeholder disagreement analysis: do affected populations accept the value conversions (e.g., is a coal miner''s livelihood equivalent to a global-average person-year of climate benefit)? Decompose analysis: calculate expected value separately for each stakeholder group without aggregation.',
    'If value conversions are contested or asymmetrically imposed on powerless groups: expected-value framing is an extraction mechanism disguised as rational analysis. If value conversions are negotiated and accepted across stakeholder groups: expected-value is genuine coordination. Determines whether suppression (0.52) reflects technical disagreement or coercive imposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incommensurable_value_comparison, conceptual, 'Whether incommensurable values can be compared on a single utility scale').

omega_variable(
    distribution_of_risk_and_benefit,
    'Who receives the average benefit (climate mitigation) and who bears the concentrated or tail risk? Do coal workers receive compensation proportional to their displacement? Do tail-risk populations receive insurance or relocation capacity?',
    'Distributional mapping: disaggregate expected-value aggregate into winner/loser groups. Compensation audit: compare promised vs. actual remedies for displaced workers. Risk-bearer consent: do tail-risk populations (near waste storage, reactor siting) consent to the risk in exchange for benefit (energy supply, economic activity)? Exit analysis: can affected populations opt out of the constraint without extreme cost?',
    'If benefits are concentrated in climate advocacy and nuclear industry while risks are distributed to powerless groups: the constraint is extraction masquerading as rational risk management (snare + tangled rope classification confirmed). If benefits and risks are negotiated across groups with meaningful exit options: constraint is coordination with higher χ (rope or weak tangled rope). Determines whether this reading forecloses the precautionary reading or merely influences it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distribution_of_risk_and_benefit, empirical, 'Whether risk and benefit distribution is equitable across stakeholder groups').

omega_variable(
    alternative_framings_availability,
    'Do decision-makers genuinely have access to catastrophic-tail and precautionary readings as live policy options, or has expected-value methodology become structurally dominant such that alternative framings are treated as irrational from within the institutional framework?',
    'Policy audit: trace which risk framings are cited in recent energy policy approvals. Authority structure check: who has standing to propose tail-risk or precautionary arguments in regulatory contexts? Institutional capture analysis: has the expected-value framing become enforced through peer-review gatekeeping, professional credentialing, or technical language barriers that exclude non-expert voices?',
    'If alternative readings are live policy options: the constraint is coexisting with other readings. If alternative readings have been formally excluded from legitimate policy deliberation: the constraint forecloses them. Determines the nature of the reading_relations field in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framings_availability, conceptual, 'Whether alternative risk framings remain live policy options').

omega_variable(
    temporal_horizon_mismatch,
    'Expected-value calculation typically uses present-discounted value, which assigns lower weight to distant future harms (nuclear waste management over 10,000+ years) and future climate damages. Does the temporal discounting embedded in expected-value calculation systematically underweight the interests of future generations?',
    'Discount rate analysis: vary the social discount rate (typical range 1–7% annually) and show how policy recommendations change. Intergenerational equity audit: identify whose interests are favored by different discount rates. Constraint comparison: see whether catastrophic-tail reading uses non-discounted or slower-discounting temporal frame.',
    'If expected-value methodology systematically discounts future harms: the constraint extracts from future populations (victims who cannot participate in current decisions) in favor of present decision-makers. If temporal framing is negotiable: the constraint is coordination with a contestable parameter. Determines whether the constraint has suppressed temporal alternatives through methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_horizon_mismatch, conceptual, 'Whether present-discounted expected value systematically underweights future generations'' interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(expected_value_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(expval_tr_t0, expected_value_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(expval_tr_t5, expected_value_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(expval_tr_t10, expected_value_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(expval_be_t0, expected_value_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(expval_be_t5, expected_value_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(expval_be_t10, expected_value_reading, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(expected_value_reading, resource_allocation).
narrative_ontology:affects_constraint(expected_value_reading, catastrophic_tail_reading).
narrative_ontology:affects_constraint(expected_value_reading, precautionary_reading).
narrative_ontology:affects_constraint(expected_value_reading, carbon_lock_in_through_coal_subsidies).
narrative_ontology:affects_constraint(expected_value_reading, nuclear_waste_intergenerational_obligation).

% DUAL FORMULATION NOTE:
% Expected-value reading is one of three structurally distinct constraints operating within the kernel 'acceptable_risk_for_energy.' The catastrophic-tail reading emphasizes tail-event probability and severity independently of expected value; the precautionary reading inverts the burden of proof. Each reading has different ε and different beneficiary/victim structure. All three are valid indexical classifications of the same domain problem (how to compare energy risk) but from different institutional positions and normative commitments. This constraint links to downstream constraints: carbon lock-in through coal subsidies (if expected-value rejects nuclear, policy may default to coal as 'proven'); nuclear waste intergenerational obligation (temporal scope of expected-value calculation affects how future harm is weighted).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
