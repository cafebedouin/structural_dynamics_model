% ============================================================================
% CONSTRAINT STORY: sotu_2003_bush_medicare_prescription_drug_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2003_bush_medicare_prescription_drug_expansion, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_2003_bush_medicare_prescription_drug_expansion
 *   human_readable: Medicare Expansion: Prescription Drug Coverage and Preventive Care (2003 SOTU Commitment)
 *   domain: healthcare_policy/entitlement_expansion
 *
 * SUMMARY:
 *   The 2003 Medicare prescription drug expansion and preventive care
 *   commitment represents a transformative entitlement extension that embeds
 *   a $400 billion funding obligation into the federal fiscal structure over
 *   a ten-year horizon. The constraint creates a structural hybrid: it
 *   genuinely solves a coordination problem (connecting seniors who need
 *   medications to pharmaceutical supply and ensuring access) while
 *   simultaneously imposing extraction costs on taxpayers, discretionary
 *   budgets, and future fiscal flexibility. The expansion preserves senior
 *   choice (opt-in plan structures) while constraining the federal
 *   government's future policy options. The classification as tangled_rope at
 *   the analytical level reflects that the constraint cannot be accurately
 *   described as either pure coordination (rope) or pure extraction (snare) —
 *   it authentically performs both functions, with the balance shifting
 *   temporally: coordination benefits accrue immediately to seniors and the
 *   pharmaceutical industry; extraction pressure accumulates across the
 *   ten-year horizon and extends to future generations. The theater ratio
 *   (0.55) indicates that administrative overhead is moderate — the
 *   constraint requires substantial bureaucratic machinery for means-testing,
 *   formulary management, and eligibility administration, but most of this
 *   apparatus serves genuine accountability functions rather than pure
 *   performance.
 *
 * KEY AGENTS:
 *   - Seniors with Chronic Drug Needs: Primary beneficiary (powerless/trapped) — gain guaranteed access to medications, solving access barriers; experience constraint as pure coordination
 *   - Pharmaceutical Industry: Institutional beneficiary (institutional/arbitrage) — gain predictable market demand and revenue stream; simultaneously constrained by cost-control mechanisms; experience constraint as tangled rope
 *   - Mid-Income Taxpayers (Working Generation): Moderate-power victim (moderate/constrained) — bear payroll tax increases and opportunity costs; also benefit from protecting elderly relatives; experience mixed extraction and coordination
 *   - Future Retirees: Powerless victim (powerless/trapped) — inherit fiscal constraint and reduced policy flexibility; face potential benefit pressures if costs exceed projections; experience pure extraction at generational timescale
 *   - Federal Discretionary Budget: Institutional victim (institutional/constrained) — crowding out of alternative spending priorities; constrained fiscal space for non-entitlement programs
 *   - Social Security System: Institutional victim (institutional/constrained) — potential downstream pressure if fiscal constraints accelerate solvency crisis
 *   - Medicare Administration: Institutional manager (institutional/arbitrage) — derives organizational role and budgetary justification from expansion administration; experiences constraint as piton (performative bureaucracy)
 *   - Fiscal Conservative Coalition: Organized victim (organized/constrained) — perceives constraint as unfunded mandate; advocates for adjustment; cannot exit but can influence future policy modification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2003_bush_medicare_prescription_drug_expansion, 0.52).
domain_priors:suppression_score(sotu_2003_bush_medicare_prescription_drug_expansion, 0.48).
domain_priors:theater_ratio(sotu_2003_bush_medicare_prescription_drug_expansion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2003_bush_medicare_prescription_drug_expansion, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_2003_bush_medicare_prescription_drug_expansion, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_2003_bush_medicare_prescription_drug_expansion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2003_bush_medicare_prescription_drug_expansion, tangled_rope).
narrative_ontology:human_readable(sotu_2003_bush_medicare_prescription_drug_expansion, "Medicare Expansion: Prescription Drug Coverage and Preventive Care (2003 SOTU Commitment)").
narrative_ontology:topic_domain(sotu_2003_bush_medicare_prescription_drug_expansion, "healthcare_policy/entitlement_expansion").

domain_priors:requires_active_enforcement(sotu_2003_bush_medicare_prescription_drug_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2003_bush_medicare_prescription_drug_expansion, seniors_with_drug_needs).
narrative_ontology:constraint_beneficiary(sotu_2003_bush_medicare_prescription_drug_expansion, pharmaceutical_industry).
narrative_ontology:constraint_beneficiary(sotu_2003_bush_medicare_prescription_drug_expansion, low_income_americans).
narrative_ontology:constraint_victim(sotu_2003_bush_medicare_prescription_drug_expansion, federal_discretionary_budget).
narrative_ontology:constraint_victim(sotu_2003_bush_medicare_prescription_drug_expansion, social_security_funding_trajectory).
narrative_ontology:constraint_victim(sotu_2003_bush_medicare_prescription_drug_expansion, taxpayers_without_immediate_benefit).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SENIOR WITH CHRONIC DRUG NEEDS (ROPE) — For seniors with multiple prescription requirements (diabetes, hypertension, cardiac medications), the Medicare expansion is pure coordination benefit. The constraint solves a genuine collective action problem: seniors and pharmaceutical suppliers both benefit from a structured payment mechanism that guarantees access and revenue. No meaningful suppression at individual level — seniors retain choice of plans. Powerless agents experience this as coordination because the beneficiary relationship is symmetric (both seniors and the system benefit from access).
constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-INCOME TAXPAYER (TANGLED ROPE) — Moderate-power agents in their working years bear extraction (higher payroll taxes, opportunity cost of $400B in foregone deficit reduction or alternative spending) while being geographically and institutionally trapped within the funding system. They also benefit coordinatively: the expansion protects against future catastrophic healthcare costs and supports elderly relatives. Mixed extraction and coordination from this perspective — genuine mutual benefit wrapped in structural cost-shifting.
constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FUTURE RETIREE / LONG-TERM BUDGET PRESSURE (SNARE) — At generational timescale, the $400B commitment creates downstream pressure on discretionary spending, infrastructure investment, and Social Security sustainability. Powerless future agents inherit both the expansion obligation and the fiscal constraints it creates. The constraint extracts from future generations in the form of reduced fiscal flexibility and potential benefit reductions. Maximum suppression from this view — they cannot exit the entitlement or the taxation system; the commitment is binding across decades.
constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL INDUSTRY (TANGLED ROPE) — Institutional actor with arbitrage options (pricing, market segmentation, international operations). The expansion is explicitly coordinating: it guarantees a substantial market segment and predictable reimbursement, solving the industry's access-to-seniors problem. Simultaneously extractive: the expansion embeds cost-control mechanisms (means testing, tiered formularies, negotiation frameworks) that constrain pricing freedom. Genuine hybrid — coordinating the market while extracting negotiation leverage over the industry.
constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MEDICARE ADMINISTRATION (PITON) — The constraint embeds massive administrative overhead: formulary management, plan administration, means-testing eligibility, coordination with insurers. The theater ratio (0.55) reflects that much of the administrative apparatus exists for accountability and equity performance rather than pure function. The bureaucracy maintains its own institutional capacity and justification by managing the expansion — it experiences the constraint as a source of organizational role rather than as meaningful coordination or extraction pressure.
constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FISCAL CONSERVATIVE COALITION (TANGLED ROPE) — Organized agents (Congressional deficit hawks, fiscal policy advocates) see the expansion as an unfunded mandate that constrains future fiscal options. They experience it as extractive (reduction in future policy flexibility, crowding out of other priorities) but coordinating (ensures healthcare access predictably, preventing crises that would require more expensive emergency interventions). Constrained exit — they cannot abolish the expansion but can influence future adjustments. Moderate experienced extraction at organizational level.
constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the constraint embeds a genuine coordination function (guaranteed pharmaceutical access for seniors) alongside a structural extraction mechanism (fiscal commitment that constrains future policy space and shifts costs across cohorts and time horizons). The constraint is neither pure coordination nor pure extraction — it is a hybrid that solves one coordination problem while creating downstream extraction pressure. This is the analytically dominant reading: tangled rope best describes the structural reality.
constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2003_bush_medicare_prescription_drug_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2003_bush_medicare_prescription_drug_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2003_bush_medicare_prescription_drug_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2003_bush_medicare_prescription_drug_expansion, TR),
    TR >= 0.70.

:- end_tests(sotu_2003_bush_medicare_prescription_drug_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. Initial extractiveness (0.32 at commitment) is low because the expansion is perceived as broadly beneficial and the fiscal impact is diffuse. Mid-point extractiveness (0.42 at year 3) rises as actual costs accumulate and crowdout effects become visible. Final extractiveness (0.58 at year 10) increases further as the commitment's fiscal pressure constrains future policy options and deficit impacts become structural. The rising trajectory reflects that the constraint's extraction mechanism is cumulative and temporally extended — costs are real and growing; their concentration on non-beneficiaries becomes increasingly apparent. Suppression (0.48): Moderate. Seniors have choice in plan selection and can opt out of certain components, reducing suppression from the beneficiary perspective. However, taxpayers and future generations face high suppression — the commitment is embedded in the fiscal structure and cannot be easily reversed. The aggregate suppression is therefore moderate, weighted between high suppression for some agents (future taxpayers) and low suppression for others (seniors choosing plans). Theater ratio (0.55): Moderate. The administrative apparatus includes genuine accountability functions (means-testing to target benefits, formulary management to prevent over-utilization) but also includes performative elements (monthly program publicity, administrative complexity that doesn't directly improve health outcomes). The theater increases over time (from 0.35 to 0.62) because cost-control measures proliferate as utilization exceeds initial projections, and much of the new administrative overlay serves to manage political rather than clinical concerns.
 *
 * PERSPECTIVAL GAP:
 *   The greatest perspectival gap appears between the 'Senior with Chronic Drug Needs' perspective (rope) and the 'Future Retiree' perspective (snare). Both are ostensibly beneficiaries of the expansion — they both gain healthcare coverage. Yet one experiences pure coordination (benefits with minimal suppression) while the other experiences pure extraction (inherits costs with no consent or exit). This gap reveals the temporal structure of the constraint: it is genuinely a rope at immediate timescale (coordination that works) and genuinely a snare at generational timescale (cost commitment that constrains future options). The analytical observer's tangled_rope classification is the bridge — it acknowledges that both are structurally accurate from their respective vantage points, and that the constraint's essence is precisely this temporal duality: immediate coordination, long-term extraction. The second-order gap is between the 'Pharmaceutical Industry' perspective (tangled rope with negotiation constraint) and the 'Fiscal Conservative Coalition' perspective (tangled rope with fiscal constraint) — both are constrained hybrid actors, but the pharmaceutical industry's constraints are commercial (negotiation leverage, formulary control) while the fiscal conservative's constraints are political (inability to reduce the commitment without massive institutional change). The extraction they experience is structurally different even though both classify as tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (seniors_with_drug_needs, pharmaceutical_industry, low_income_americans) identify who benefits from the constraint's core coordination function — guaranteed pharmaceutical access. The victim declarations (federal_discretionary_budget, social_security_funding_trajectory, taxpayers_without_immediate_benefit) identify who bears extraction costs. The directionality engine derives d from the beneficiary/victim categorization plus exit options: beneficiaries with mobile options (seniors can choose plans, pharma can adjust pricing) get lower d; victims with trapped exit (future taxpayers inherit commitment with no consent) get higher d. The rising extractiveness trajectory (0.32 → 0.58) reflects that as the commitment matures, its extraction component becomes increasingly visible — the fiscal costs are real and growing, and they accumulate without corresponding benefit accrual to the taxpayers bearing them. The suppression level (0.48) is determined by exit capacity: seniors and pharma have significant exit optionality; taxpayers and future generations have minimal exit capacity. The constraint's suppression is therefore highest for powerless agents (future retirees) and lowest for institutional agents (pharma with arbitrage, seniors with plan choice).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids the mandatrophy (classification ambiguity between coordination and extraction) through the tangled_rope type, which explicitly acknowledges both functions. At immediate timescale (0-5 years), the constraint is primarily coordination: it solves the access problem (rope dominates). At mid-horizon (5-10 years), extraction becomes visible: fiscal pressure accumulates (tangled_rope dominates). At generational timescale (10-30 years), extraction dominates: future options constrained (snare-tendency visible). The mandatrophy is resolved by accepting that the constraint genuinely is a hybrid that performs both functions, with the balance shifting temporally. The analytical observer avoids the false choice between 'this is just coordination' and 'this is just extraction' by recognizing that the temporal structure IS the constraint structure. The measurements support this resolution: theater_ratio rises over time (0.35 → 0.62) because the administrative apparatus must expand to manage cost pressures; base_extractiveness rises (0.32 → 0.58) because the fiscal commitment's extraction component becomes empirically undeniable as costs accumulate. The constraint's type is not ambiguous — it is tangled_rope across all perspectives except where temporal weighting produces secondary classifications (rope at very short timescale for seniors, snare at very long timescale for future generations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_trajectory_divergence,
    'Will actual pharmaceutical costs track the $400B ten-year projection, or will utilization and price growth diverge significantly?',
    'Historical cost tracking of Medicare drug benefits post-2006; comparison of CMS projections to actualized spending; analysis of utilization acceleration vs. price control effectiveness',
    'If actual costs remain near projection: constraint maintains its tangled_rope classification. If actual costs exceed projection by >30%: extraction mechanism intensifies, reclassifying to snare-dominant at generational timescale. If costs come in below projection: coordination benefits increase, moderating extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_trajectory_divergence, empirical, 'Whether pharmaceutical cost growth tracks initial projections').

omega_variable(
    crowdout_mechanism_magnitude,
    'To what degree does the $400B commitment crowd out other federal priorities (infrastructure, education, discretionary R&D) vs. being funded through deficit spending or revenue increases?',
    'Budget allocation analysis over 10+ year period; comparison of counterfactual spending without the expansion to actual appropriations; identification of programs that received reduced funding',
    'If primarily deficit-funded: extraction is deferred to future generations (snare intensifies). If primarily crowdout: extraction is immediate and concentrated (snare intensifies at different timescale). If funded by revenue increases: extraction distributes across current taxpayers (maintains tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crowdout_mechanism_magnitude, empirical, 'Magnitude of crowdout vs. deficit funding vs. revenue increases').

omega_variable(
    social_security_sustainability_link,
    'Does the $400B commitment materially affect Social Security''s long-term funding trajectory or trigger benefit reductions/taxation changes?',
    'Actuarial analysis linking Medicare prescription drug spending to Social Security trustees reports; identification of policy changes explicitly attributed to the expansion commitment',
    'If causal link confirmed: the constraint''s victim set includes social_security_beneficiaries as a sub-cohort. Extraction mechanism extends across entitlements. If no causal link: Social Security pressure is independent and extraction is isolated to discretionary budget.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_security_sustainability_link, empirical, 'Whether the constraint affects Social Security sustainability').

omega_variable(
    means_testing_regressive_depth,
    'Do means-testing provisions and tiered cost-sharing create regressive extraction within the senior population, concentrating costs on lower-income beneficiaries?',
    'Distributional analysis of out-of-pocket costs by income quintile; tracking of coverage gaps and benefit limitations across income strata; comparison of beneficiary experience across socioeconomic groups',
    'If regressive: the constraint embeds asymmetric extraction within the beneficiary class (lower-income seniors bear disproportionate costs despite being labeled beneficiaries). Suppression increases for lower-income seniors. If progressive: coordination benefits are genuinely shared.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(means_testing_regressive_depth, empirical, 'Regressive depth of means-testing and cost-sharing provisions').

omega_variable(
    pharmaceutical_pricing_negotiation_constraint,
    'Do cost-control mechanisms (formularies, prior authorization, reference pricing) effectively constrain pharmaceutical pricing power, or do they function primarily as bureaucratic theater while prices continue to rise?',
    'Price trend analysis pre- and post-expansion; comparison of US drug prices to international benchmarks; tracking of formulary changes and manufacturer responses; identification of cost-control failures and workarounds',
    'If effective constraint: the expansion extracts pricing leverage from the pharmaceutical industry, maintaining tangled_rope balance. If theater: prices continue rising independent of controls, and the pharmaceutical industry''s benefit from the expansion increases (shifting classification toward rope from pharma perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharmaceutical_pricing_negotiation_constraint, empirical, 'Whether cost-control mechanisms effectively constrain pricing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2003_bush_medicare_prescription_drug_expansion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_2003_bush_medicare_prescription_drug_expansion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu_tr_t3, sotu_2003_bush_medicare_prescription_drug_expansion, theater_ratio, 3, 0.45).
narrative_ontology:measurement(sotu_tr_t6, sotu_2003_bush_medicare_prescription_drug_expansion, theater_ratio, 6, 0.55).
narrative_ontology:measurement(sotu_tr_t10, sotu_2003_bush_medicare_prescription_drug_expansion, theater_ratio, 10, 0.62).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_2003_bush_medicare_prescription_drug_expansion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(sotu_be_t3, sotu_2003_bush_medicare_prescription_drug_expansion, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(sotu_be_t6, sotu_2003_bush_medicare_prescription_drug_expansion, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(sotu_be_t10, sotu_2003_bush_medicare_prescription_drug_expansion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2003_bush_medicare_prescription_drug_expansion, resource_allocation).
narrative_ontology:affects_constraint(sotu_2003_bush_medicare_prescription_drug_expansion, federal_budget_fiscal_trajectory).
narrative_ontology:affects_constraint(sotu_2003_bush_medicare_prescription_drug_expansion, pharmaceutical_pricing_power_constraint).
narrative_ontology:affects_constraint(sotu_2003_bush_medicare_prescription_drug_expansion, social_security_long_term_solvency).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
