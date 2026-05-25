% ============================================================================
% CONSTRAINT STORY: option_value_preserving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_option_value_preserving, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: option_value_preserving
 *   human_readable: Option Value Preservation in Energy Policy Under Deep Uncertainty
 *   domain: energy_policy/risk_assessment/decision_theory
 *
 * SUMMARY:
 *   Energy policy under deep uncertainty faces a fundamental dilemma: should
 *   decision-makers commit to rapid decarbonization (closing fossil pathways)
 *   based on climate risk models, or preserve flexibility by keeping multiple
 *   technological pathways open until uncertainty resolves? The
 *   option-value-preserving reading answers: keep options open. This
 *   constraint model captures how that choice operates as a structural
 *   mechanism that coordinates some agents (institutions that benefit from
 *   flexibility, stranded-asset holders, technology-neutral frameworks) while
 *   extracting from others (climate-vulnerable agents who bear tail risk,
 *   rapid-decarbonization advocates forced into longer timelines). The
 *   constraint is not a natural law of decision-making, though it risks being
 *   naturalized as such; nor is it pure extraction, though tail-risk bearers
 *   experience high asymmetry. It is a Tangled Rope: a genuinely mixed
 *   coordination-extraction hybrid where the coordination function (keeping
 *   options open under irreducible uncertainty) is real, but the asymmetry is
 *   severe (those who design the option-preservation framework do not bear
 *   the cost of preserved fossil pathways; those who bear climate tail risk
 *   did not choose the framework). The measurement trajectory shows
 *   increasing extractiveness and theater over the interval: as climate
 *   science narrows uncertainty and renewable costs drop, the 'deep
 *   uncertainty' rationale weakens, yet the constraint persists through
 *   institutional inertia and explicit reaffirmation of technology neutrality
 *   — the theater rises as the actual coordination function declines.
 *
 * KEY AGENTS:
 *   - Climate-vulnerable nations and populations: Primary victim (powerless/trapped) — bear tail-risk cost of delayed decarbonization; no exit or organizing capacity at global scale
 *   - Rapid-decarbonization advocates (climate scientists, green-tech advocates): Secondary victim (moderate/constrained) — forced into slower transition timelines; benefit from renewable pathway preservation but absorb political delay cost
 *   - Stranded-asset holders (fossil infrastructure, utilities): Primary beneficiary (powerful/constrained) — benefit from extended pathway viability; forced to absorb regulatory uncertainty as extraction cost
 *   - Technology-neutral policy frameworks (energy regulators, international climate bodies): Institutional beneficiary (institutional/arbitrage) — constraint solves their coordination problem; enables fence-sitting during uncertainty
 *   - Just-transition advocates (workers, coal-region communities): Mixed victim-beneficiary (organized/constrained) — benefit from extended fossil viability during transition window, but face sunset logic; moderate power through unionization
 *   - Incumbent energy corporations (oil majors, utilities): Institutional beneficiary (powerful/arbitrage) — experience constraint as pure coordination; arbitrage between fossil and renewable portfolios
 *   - Climate tail-risk internalizers (low-latitude nations, island states, future generations): Powerless victim (powerless/trapped) — abstract collective good that cannot exit; bears full cost of unmitigated warming risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(option_value_preserving, 0.38).
domain_priors:suppression_score(option_value_preserving, 0.48).
domain_priors:theater_ratio(option_value_preserving, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(option_value_preserving, extractiveness, 0.38).
narrative_ontology:constraint_metric(option_value_preserving, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(option_value_preserving, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(option_value_preserving, tangled_rope).
narrative_ontology:human_readable(option_value_preserving, "Option Value Preservation in Energy Policy Under Deep Uncertainty").
narrative_ontology:topic_domain(option_value_preserving, "energy_policy/risk_assessment/decision_theory").

domain_priors:requires_active_enforcement(option_value_preserving).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(option_value_preserving, decision_flexibility_preservers).
narrative_ontology:constraint_beneficiary(option_value_preserving, technology_neutral_frameworks).
narrative_ontology:constraint_victim(option_value_preserving, rapid_decarbonization_advocates).
narrative_ontology:constraint_victim(option_value_preserving, stranded_asset_holders).
narrative_ontology:constraint_victim(option_value_preserving, climate_tail_risk_internalisers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE TAIL RISK INTERNALISERS (SNARE) — Those forced to internalize the cost of unmitigated warming (low-latitude nations, island states, future generations) have no exit. The option-value-preserving framework delays decisive decarbonization, which bears full weight onto this agent. Maximum extraction: the constraint trades their catastrophic risk for the option value of keeping fossil pathways open for others. Cannot organize globally; bears asymmetric burden.
constraint_indexing:constraint_classification(option_value_preserving, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RAPID DECARBONIZATION ADVOCATES (TANGLED ROPE) — Constrained by lock-in risk (they cannot force retirement of functioning coal plants) and political barriers, but genuinely benefit from the option-preservation framework's commitment to keep low-carbon pathways viable. Mixed experience: forced to accept slower transition timeline (extraction), but gain guarantees that renewable investment pathways remain open (coordination). Moderate power to organize regionally; significant but not maximal extraction.
constraint_indexing:constraint_classification(option_value_preserving, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY-NEUTRAL FRAMEWORKS (ROPE) — Policy institutions (energy regulators, planning bodies, international climate bodies) benefit from the flexibility option-value-preserving logic provides. This constraint is their coordination mechanism: it legitimizes keeping multiple pathways open, reduces political commitment risk, and enables fence-sitting during deep uncertainty. Pure beneficiary; experienced as coordination without extraction. Arbitrage exit: can shift between carbon-pricing, tech-subsidy, and tech-neutral frameworks.
constraint_indexing:constraint_classification(option_value_preserving, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STRANDED ASSET HOLDERS (TANGLED ROPE) — Coal and gas infrastructure investors experience the constraint as coordination (it keeps their assets viable longer than rapid-decarbonization scenarios) and extraction (it forces them to absorb volatility and regulatory uncertainty rather than plan terminal decline). Constrained exit: cannot abandon sunk infrastructure; benefit from option preservation but cannot exit if circumstances change. Powerful agents but structurally constrained in THIS decision context.
constraint_indexing:constraint_classification(option_value_preserving, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUST TRANSITION COALITION (SCAFFOLD) — Organized labor, coal-region communities, and transition advocates see the constraint as temporary support with a sunset: option preservation keeps coal jobs viable during the transition window (coordination benefit), but the framework is explicitly temporary — it decays as renewable deployment scales and worker reskilling completes. Moderate power through unionization and political coalition; constrained by lock-in to regional economies, but sunset logic provides exit path. Theater ratio high: much commitment rhetoric to 'just transition' with lower functional support.
constraint_indexing:constraint_classification(option_value_preserving, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CLIMATE GOVERNANCE APPARATUS (PITON) — The UNFCCC, IPCC, and related bodies maintain the option-value-preserving logic through repeated reaffirmation of 'technology neutrality' in climate frameworks, despite escalating climate science showing that some pathways are incompatible with warming limits. The apparatus preserves the legitimacy fiction that all pathways can coexist under acceptable risk, sustaining itself through theater (expert reports affirming flexibility, Paris Agreement's ambiguous temperature goals). Performative rather than functional: the apparatus sees its own framework as degraded (knows deep decarbonization is necessary) but maintains it through institutional inertia and to avoid political collapse.
constraint_indexing:constraint_classification(option_value_preserving, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / DEEP UNCERTAINTY VIEW (MOUNTAIN) — From a civilizational/universal perspective, deep uncertainty about technology pathways, economic costs, and climate sensitivity is an irreducible feature of long-term energy planning. From this view, option-value preservation is a natural law of rational decision-making: you cannot know which technologies will succeed, so you must keep multiple pathways open. This perspective risks naturalizing what is actually a contingent institutional choice. False summit candidate: identifiable beneficiaries (technology-neutral frameworks, stranded-asset holders) suggest the naturalization may be motivated.
constraint_indexing:constraint_classification(option_value_preserving, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 8: INCUMBENT ENERGY CORPORATIONS (ROPE) — At the immediate/regional level, energy incumbents (oil majors, utilities) experience the constraint as pure coordination: it solves the collective action problem of maintaining investment in mature infrastructure while new technologies scale. The option-preservation frame legitimizes continued capex in fossil assets. Net beneficiary with arbitrage exit: can shift between fossil, renewables, and hybrid portfolios. Low experienced extraction because they designed this constraint partly to solve their own coordination problem.
constraint_indexing:constraint_classification(option_value_preserving, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(option_value_preserving_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(option_value_preserving, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(option_value_preserving, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(option_value_preserving, TR),
    TR >= 0.70.

:- end_tests(option_value_preserving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts by delaying decarbonization, but not maximally — the option-value-preserving framework does maintain renewable and nuclear pathways as viable alternatives. The extraction is bounded by the genuine coordination benefit (keeping options open is not purely coercive; it solves a real decision problem under uncertainty). The base value reflects that the framework privileges incumbent pathways by deferring commitment costs. Rising trajectory (0.22 → 0.44) shows that as uncertainty shrinks (solar costs drop, climate models sharpen), the extraction mechanism becomes more visible — the 'option value' rationale weakens, yet the constraint persists. Suppression (0.48): Moderate-high. Significant barriers exist to rapid decarbonization (sunk infrastructure, lock-in, political economy of stranded assets) and to indefinite fossil continuation (climate science, renewable competitiveness, net-zero commitments). The constraint maintains suppression on both extremes — neither total decarbonization nor business-as-usual is institutional viable. Theater ratio (0.52): Moderate-high and rising. Much of the 'deep uncertainty' framing in energy policy is performative: climate science has narrowed uncertainty range considerably; renewable cost curves are well-understood; tail risk probabilities are quantifiable. Yet policy institutions continue to reaffirm 'technology neutrality' through repeated commitments and expert reports. The theater rises (0.38 → 0.52) as the rationale weakens but the constraint persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits six distinct classifications from the same base properties, revealing how deeply indexical the constraint is. The beneficiary's institutional perspective (Rope/Mountain) sees the constraint as legitimate coordination or natural law. The victim's powerless perspective (Snare) sees pure extraction with no option. The organized coalition perspective (Scaffold) sees the constraint as temporary with sunset logic — a coordination mechanism that decays as alternatives mature. The incumbent perspective (Rope) sees pure coordination; the rapid-advocate perspective (Tangled Rope) sees mixed benefit and harm. The governance apparatus perspective (Piton) sees its own logic as degraded — performative affirmation of flexibility that no longer reflects genuine uncertainty. The perspectival gap is maximal between powerless and institutional perspectives: the same decision to 'keep options open' appears as legitimate flexibility from one angle and as imposed tail-risk externality from another. The gap is not due to disagreement about facts (uncertainty is real) but about who bears the cost of preserving flexibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from the agent's power level, exit options, and beneficiary/victim relationship to the constraint. Powerless tail-risk bearers (d ≈ 0.95, trapped) experience maximum effective extraction through the sigmoid, despite moderate base ε, because they have no exit. Institutional beneficiaries with arbitrage (d ≈ 0.10, technology-neutral frameworks) experience low or negative χ — the constraint subsidizes their decision-making flexibility. Stranded-asset holders (d ≈ 0.55, constrained, both beneficiary and victim) experience moderate χ reflecting their mixed position: they benefit from extended viability but absorb volatility risk. The perspectival gap between beneficiaries (rope experience) and tail-risk victims (snare experience) is extreme despite shared ε — the difference is entirely driven by directionality: who controls the decision, who bears the consequence.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy — the pressure to mislabel Tangled Rope as either pure Rope (coordination) or pure Snare (extraction) — is resolved by observing that the constraint is genuinely both. The option-value-preservation logic (Rope element) is real: keeping multiple pathways open under irreducible uncertainty about technology costs, learning rates, and climate sensitivity is a legitimate coordination problem. The extraction (Snare element) is also real: the cost of that preservation is externalized onto agents (climate-vulnerable populations, future generations) who did not choose the framework and cannot exit. The constraint exists because the coordination benefit accrues to decision-making institutions and incumbent producers, while the extraction cost accrues to tail-risk bearers. Pure coordination (Rope) would require all agents to benefit from flexibility or to explicitly accept the tail-risk cost. Pure extraction (Snare) would require no genuine coordination function. Tangled Rope correctly captures the simultaneity: the constraint coordinates institutional decision-making AND extracts from climate-vulnerable actors. The false summit (analytical observer seeing this as Mountain/natural law) is exposed by the presence of identifiable beneficiaries — the constraint's naturalization benefits specific actors, suggesting that the 'deep uncertainty' framing may be partly motivated rather than purely descriptive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of ''acceptable risk in energy policy'' governs the constraint: option-value-preservation, catastrophic-tail-dominance, or expected-value-dominance?',
    'Empirical comparison of realized climate outcomes against each reading''s threshold: does actual warming exceed catastrophic tail (> 3°C), does cost-benefit favor expected-value closure, or does realized technological learning justify option preservation?',
    'Option-value reading: Tangled Rope with moderate extraction. Catastrophic-tail reading: Snare with high extraction (victim set expands, suppression rises). Expected-value reading: Rope/Mountain (extraction falls, constraint becomes technical efficiency metric).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which reading of acceptable risk governs the constraint (option-value vs tail-dominant vs expected-value)').

omega_variable(
    option_value_decay_timeline,
    'Over what timeline does the option value of keeping fossil pathways open decay to zero? Is it 10 years, 30 years, or is it indefinite?',
    'Empirical observation of technological learning rates, renewable cost curves, and battery storage deployment; comparison of actual pathway costs against option-value-preserving projections made in prior decades. If renewables cost trajectory beats historical projections, option value decays faster; if surprises occur, decay slows.',
    'Fast decay (< 15 years): constraint should reclassify to Scaffold (temporary support) rather than Tangled Rope (ongoing hybrid). Slow decay (> 40 years): constraint becomes institutionalized Piton (degraded, theater-driven). Indefinite: constraint is misnamed — it is either Rope (pure coordination of legitimate technological uncertainty) or Mountain (natural law of decision-making under irreducible uncertainty).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(option_value_decay_timeline, empirical, 'Timeline over which option value of fossil pathways decays to zero').

omega_variable(
    tail_risk_internalization_asymmetry,
    'Do decision-makers applying the option-value-preserving framework actually internalize the tail-risk cost they impose on climate-vulnerable agents, or is this cost invisible to the framework?',
    'Institutional analysis of decision-making processes: do energy policy bodies formally model climate tail-risk distributions? Do they assign shadow prices or moral weights to low-probability high-impact warming scenarios? Do they include voices of tail-risk bearers in deliberation?',
    'If internalized: constraint is more pure Rope (fair coordination across perspectives); extraction falls. If invisible: constraint is more pure Snare from tail-risk perspective; suppression rises; asymmetry deepens. If partially invisible (some internalization, incomplete): classification stands as Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tail_risk_internalization_asymmetry, empirical, 'Whether tail-risk costs are internalized in option-value decision-making').

omega_variable(
    technology_neutrality_cover_story,
    'Is ''technology neutrality'' an accurate description of energy policy, or a framing that favors incumbent technologies by equalizing pathways with radically different deployment timelines and costs?',
    'Comparative cost analysis: do ''technology-neutral'' frameworks actually treat coal, gas, nuclear, and renewables as substitutes, or do they embed hidden assumptions (baseline load requirements, grid stability requirements) that privilege dispatchable sources? Do they account for learning curves and scale effects that favor renewables?',
    'If truly neutral: constraint is Rope (legitimate coordination). If biased toward incumbents: constraint is Tangled Rope with higher extraction (beneficiaries are identifiable; victims include decarbonization-track countries). If highly biased: constraint becomes Snare from rapid-decarbonization perspective (extraction via framework bias).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technology_neutrality_cover_story, empirical, 'Whether technology neutrality is descriptively accurate or a cover story').

omega_variable(
    sibling_reading_catastrophic_tail,
    'If the catastrophic-tail-dominant reading governed instead (climate tail risk dominates; early decarbonization required), how would the constraint reclassify?',
    'Structural reanalysis: under tail-dominant reading, ''acceptable risk'' would be defined by tail-risk threshold (e.g., 10% chance of > 3°C warming is unacceptable), not by decision flexibility. Constraint would become a Snare (forced early closure of fossil pathways) from the incumbent perspective, pure Rope from the climate-advocate perspective.',
    'Sibling classification: Snare (high extraction from stranded-asset holders, energy-intensive economies) with ε ≈ 0.65. This reading is ''live'' in climate policy — some actors adopt it fully, others reject it. The option-value reading is the chosen one for THIS constraint story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_catastrophic_tail, conceptual, 'Sibling reading: catastrophic-tail-dominant').

omega_variable(
    sibling_reading_expected_value,
    'If the expected-value-dominant reading governed instead (climate cost-benefit dominates; rational actors optimize expected value), how would the constraint reclassify?',
    'Structural reanalysis: under expected-value reading, energy policy becomes a technical optimization problem (minimize expected cost across probability-weighted scenarios). The constraint would be seen as either pure Rope (coordination mechanism for finding optimal mix) or a Mountain (rational decision theory is a natural law). Extraction becomes invisible because the framework is purely cost-minimizing.',
    'Sibling classification: Rope or Mountain (ε ≤ 0.15), depending on whether you see cost-benefit logic as coordination (Rope) or as inevitable (Mountain). This reading dominates energy economics textbooks but is rejected by those who believe tail-risk dominates textbook expected values. The option-value reading sits between these siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_expected_value, conceptual, 'Sibling reading: expected-value-dominant').

omega_variable(
    false_summit_institutional_naturalization,
    'Is the ''deep uncertainty, keep options open'' framing (Mountain perspective) a natural law of decision-making, or an institutional choice that benefits technology-neutral frameworks and incumbent energy producers?',
    'Historical institutional analysis: has energy policy always preserved multiple options, or did this shift occur after fossil fuels became entrenched and rapid-decarbonization pathways emerged as a threat? Does the ''deep uncertainty'' narrative appear in policy before or after stranded-asset risk became salient?',
    'If natural law: constraint is Mountain from analytical perspective (legitimate). If institutional choice: constraint is false summit — benefits are concentrated (beneficiaries clearly identified); the ''deep uncertainty'' framing naturalizes contingent power dynamics. Engine will reclassify if sufficient coupling evidence and beneficiary data accumulate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_institutional_naturalization, conceptual, 'Whether deep uncertainty/option-preservation is natural law or institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(option_value_preserving, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opti_tr_t0, option_value_preserving, theater_ratio, 0, 0.38).
narrative_ontology:measurement(opti_tr_t8, option_value_preserving, theater_ratio, 8, 0.48).
narrative_ontology:measurement(opti_tr_t16, option_value_preserving, theater_ratio, 16, 0.52).

% Extraction over time
narrative_ontology:measurement(opti_be_t0, option_value_preserving, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(opti_be_t8, option_value_preserving, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(opti_be_t16, option_value_preserving, base_extractiveness, 16, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(option_value_preserving, resource_allocation).
narrative_ontology:affects_constraint(option_value_preserving, catastrophic_tail_dominant).
narrative_ontology:affects_constraint(option_value_preserving, expected_value_dominant).
narrative_ontology:affects_constraint(option_value_preserving, stranded_asset_accumulation).
narrative_ontology:affects_constraint(option_value_preserving, climate_commitment_ratchet).

% DUAL FORMULATION NOTE:
% The 'acceptable_risk_energy' kernel has three structurally distinct constraint readings: option-value-preserving (this story, ε ≈ 0.38, Tangled Rope), catastrophic-tail-dominant (sibling, ε ≈ 0.65, Snare), and expected-value-dominant (sibling, ε ≤ 0.15, Rope). Each reading generates different victim sets, beneficiary distributions, and suppression profiles. They are not variations of one constraint but distinct structural phenomena that the same natural-language domain concept ('acceptable risk') conflates. The option-value reading influences the stranded-asset accumulation constraint (keeping fossil pathways open extends asset viability) and the climate-commitment-ratchet constraint (each affirmation of flexibility delays binding commitments). The sibling readings would affect these downstream constraints differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(option_value_preserving, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
