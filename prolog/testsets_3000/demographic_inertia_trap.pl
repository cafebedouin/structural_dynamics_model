% ============================================================================
% CONSTRAINT STORY: demographic_inertia_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demographic_inertia_trap, []).

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
 *   constraint_id: demographic_inertia_trap
 *   human_readable: The Generational Wealth Siphon
 *   domain: social/economic
 *
 * SUMMARY:
 *   The generational wealth siphon describes a structural trap where an aging
 *   demographic majority uses electoral power in majoritarian democratic
 *   systems to enforce sustained economic transfers from a shrinking youth
 *   minority. As fertility rates decline and life expectancy increases, the
 *   demographic composition shifts toward older citizens. In systems where
 *   voting is proportional to population (one person, one vote), this creates
 *   an electoral supermajority with direct incentive to vote for policies
 *   that maximize their own consumption: defined-benefit pensions,
 *   age-stratified healthcare, property tax caps, and reduced
 *   education/infrastructure investment. The youth minority cannot exit the
 *   jurisdiction easily (constrained by visa regimes, employment opportunity
 *   concentration, family ties), cannot vote their way out of majority rule
 *   (demographic arithmetic), and cannot refuse payment (compulsory
 *   taxation). The constraint exhibits high suppression (0.68) because
 *   structural barriers to exit are tight, and the extraction is enforced
 *   through legal/institutional mechanisms. The theater ratio (0.42) is
 *   moderate because the democratic ritual itself (elections, representation,
 *   taxation) performs legitimacy, but the underlying fiscal arithmetic —
 *   underfunded liabilities, compounding debt obligations passed to future
 *   generations — is transparent. This is not a snare that requires
 *   deception; it requires only that the electoral majority act rationally in
 *   its own interest.
 *
 * KEY AGENTS:
 *   - Aging Demographic Majority: Primary beneficiary (institutional/arbitrage) — electoral supermajority voting for maximum pension, healthcare, and property asset protection; captures wealth transfer from youth tax base
 *   - Youth Minority: Primary victim (powerless/trapped) — outnumbered in voting, compressed wage growth, reduced home affordability, burdened by future liabilities; constrained emigration options
 *   - Future Generations: Secondary victim (powerless/trapped) — inheriting depleted public finances, unfunded pension liabilities, degraded infrastructure; no real-time negotiation power
 *   - Social Capital Commons: Tertiary victim (powerless/trapped) — intergenerational social contract degraded; reduced public trust in institutions; polarization driven by zero-sum resource competition
 *   - Organized Youth Coalition: Intermediate actor (organized/mobile) — unions, political parties, advocacy groups representing younger workers; can partially exit through coalition-building and interstate migration advocacy
 *   - Electoral System Institution: Institutional actor (institutional/arbitrage) — maintains legitimacy performance (democratic voting) while structurally amplifying demographic majorities; theater-driven maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demographic_inertia_trap, 0.58).
domain_priors:suppression_score(demographic_inertia_trap, 0.68).
domain_priors:theater_ratio(demographic_inertia_trap, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demographic_inertia_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(demographic_inertia_trap, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(demographic_inertia_trap, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demographic_inertia_trap, snare).
narrative_ontology:human_readable(demographic_inertia_trap, "The Generational Wealth Siphon").
narrative_ontology:topic_domain(demographic_inertia_trap, "social/economic").

domain_priors:requires_active_enforcement(demographic_inertia_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demographic_inertia_trap, aging_demographic_majority).
narrative_ontology:constraint_victim(demographic_inertia_trap, youth_minority).
narrative_ontology:constraint_victim(demographic_inertia_trap, future_generations).
narrative_ontology:constraint_victim(demographic_inertia_trap, social_capital_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUTH MINORITY (SNARE) — Structurally powerless: outnumbered in electoral systems, economically burdened by transfer payments, unable to exit national tax/pension systems. Exit options are limited to emigration (constrained by visa regimes, skill requirements, family ties). Bears full extraction load: intergenerational transfers, reduced intergenerational social mobility, compressed wage growth due to benefit obligations, diminished access to homeownership. No alternative pathway exists within the jurisdiction.
constraint_indexing:constraint_classification(demographic_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Constrained by path dependency: inheriting depleted public finances, degraded infrastructure investment, reduced educational funding, and compounding debt service obligations. Cannot exit or negotiate in real time. The extraction is deferred but locked in — policy choices made by the electoral majority today determine the feasibility space for future agents. Structural constraint is enforcement through debt instruments and unfunded liabilities.
constraint_indexing:constraint_classification(demographic_inertia_trap, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: AGING DEMOGRAPHIC MAJORITY (ROPE) — Net beneficiary of the constraint. Electoral power (high vote share) translates directly into policy control through democratic majoritarian systems. Experience the constraint as pure coordination: voting for leaders who promise pension security, healthcare access, and property tax caps. Zero coercion from their perspective — they are winning a coordination game. Effective extraction runs toward them; they experience it as legitimate reward for accumulated savings and labor history.
constraint_indexing:constraint_classification(demographic_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED YOUTH COALITION (TANGLED ROPE) — Youth interest groups, worker associations, and political movements can partially exit through coalition-building, interstate migration advocacy, or policy capture. They experience both extraction (fiscal burden) and coordination (unionization, benefit negotiation). Exit options (mobile + organized) are intermediate — political organization creates some leverage, but demographic arithmetic remains fundamentally against them. Effective extraction is dampened by agency but not eliminated.
constraint_indexing:constraint_classification(demographic_inertia_trap, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ELECTORAL SYSTEM (PITON) — Democratic majoritarian voting is maintained as the functional legitimacy mechanism, but its actual verification power has degraded. The electoral system performs legitimacy (one person, one vote) but structurally amplifies demographic majorities into supermajority policy control. The theater is high (democratic ritual remains performative) while the functional verification of intergenerational equity has decayed. Maintained through institutional inertia and constitutional constraint rather than genuine coordination efficiency.
constraint_indexing:constraint_classification(demographic_inertia_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DEMOGRAPHIC NECESSITY (MOUNTAIN) — From a universal civilizational view, demographic structure creates irreducible constraints: aging populations require healthcare and pension support, shrinking populations reduce tax bases and collective labor supply, and these are physical/biological facts independent of policy choice. The observer may see the extraction as an immutable consequence of demographic law. However, the structural data — low theater_ratio (0.42), active enforcement requirements, clear beneficiaries/victims — contradicts the mountain classification. The engine will flag this as a false summit: demographic necessity is real, but the DISTRIBUTION of burden via electoral power and policy choice is contingent.
constraint_indexing:constraint_classification(demographic_inertia_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demographic_inertia_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(demographic_inertia_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demographic_inertia_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(demographic_inertia_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(demographic_inertia_trap, TR),
    TR >= 0.70.

:- end_tests(demographic_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The aging majority captures direct wealth transfers (pension payments exceed contribution ratios, healthcare subsidized by payroll taxes, property value protection through zoning/tax caps). The extraction is not maximal because some transfers are genuine social insurance (pooled risk) rather than pure transfer — retirees did contribute during their working years. However, increased longevity and reduced worker-to-retiree ratios mean current transfers significantly exceed actuarial fairness. The value reflects sustained asymmetric extraction with a minimal fairness veneer. Suppression (0.68): High. Exit barriers are structural: national taxation systems compel payment, visa regimes limit emigration, family/social ties create immobility costs, housing markets driven by older property owners create affordability barriers. The constraint is enforced through legal/institutional mechanisms that require active evasion to escape. Theater ratio (0.42): Moderate-low. Democratic elections perform legitimacy, but the fiscal mathematics is transparent — intergenerational accounting is easily calculated. The performative content is lower than constraints requiring deception; the snare operates through direct majoritarian power, not theater.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap here is acute. The aging majority experiences the constraint as pure Rope (coordination): they vote for leaders who promise pension security and healthcare access. Zero coercion from their perspective. The youth minority experiences Snare: compulsory transfers, demographic powerlessness, trapped exit. Future generations experience an even more severe Snare: inherited debt and policy path-dependency they had no voice in creating. The organized youth coalition experiences Tangled Rope: they have some leverage through unionization and political organizing (mobile exit option), but the fundamental demographic asymmetry constrains their gains. The electoral system institution experiences Piton: democratic voting performs legitimacy, but its functional role in representing all generations has degraded — it now amplifies demographic majorities at the expense of minorities and future generations. The analytical observer risks seeing Mountain (demographic necessity is real), but this naturalizes what is actually a contingent policy choice: the distribution mechanism (electoral majoritarian voting) is not inherent to aging; it is a choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position: Aging majority occupies d ≈ 0.05 (full beneficiary, arbitrage exit) — electoral supermajority extracts toward them. Youth minority occupies d ≈ 0.95 (full target, trapped exit) — extraction runs away from them, maximum burden. Organized youth coalition occupies d ≈ 0.55 (partial target, mobile exit) — can partially counter through organization but demographic arithmetic limits leverage. Future generations occupy d ≈ 1.0 (pure target, no exit) — locked into inherited obligations. Electoral system institution occupies d ≈ 0.15 (beneficiary via legitimacy maintenance, arbitrage exit through institutional preservation) — system benefits from continued majoritarian rule. The analytical observer occupies d ≈ 0.72 (analytical perspective, neither beneficiary nor victim) — sees full structure but risks naturalizing it as demographic law. Directionality values feed the sigmoid f(d) to compute experienced extractiveness (χ = ε × f(d) × σ(S)) for each perspective. National scope (σ=1.0) means χ scales directly with f(d) and ε.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by clarifying that the classification depends on the voter's structural position, not on whether the transfer is 'fair' or 'necessary.' The aging majority legitimately sees Rope (coordination benefit). The youth minority legitimately sees Snare (extraction). Both perspectives are correct within their indexical context. The mandatrophy is avoided by recognizing that one agent's coordinated benefit is another agent's extraction. The false summit (analytical mountain view) is avoided by noting that while demographic composition is a law of biology, the DISTRIBUTION of burden through electoral majoritarian voting is a contingent institutional choice. Alternative mechanisms (cross-generational fairness norms, automatic stabilizers that reduce benefits if demographics shift, citizen assemblies, constitutional constraints on intergenerational transfers) could redistribute the burden differently. The snare classification is robust because even if some transfers are actuarially fair, the combination of demographic majoritarian voting + constrained youth exit + passive policy toward intergenerational accounting = sustained extraction from a group that cannot exit or vote its way out.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demographic_pivot_point,
    'At what threshold does demographic composition reverse sufficiently to flip political control from aging majority to younger minority?',
    'Cohort replacement analysis; historical precedent from jurisdictions with rapid demographic reversals (Japan, South Korea, Eastern Europe); projection of voting-age population shares 20-50 years forward',
    'If pivot occurs within 15-20 years: snare classification is temporary (boundary case for scaffold). If pivot is 30+ years distant: extraction is locked in for 2-3 generations. If pivot never occurs (net negative migration, low birth rates): snare is permanent until system collapse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_pivot_point, empirical, 'Timeline for demographic control reversal').

omega_variable(
    fiscal_sustainability_boundary,
    'What debt/obligation level triggers fiscal insolvency, triggering forced austerity, default, or restructuring that breaks the extraction mechanism?',
    'Debt-to-GDP trajectory modeling; historical analysis of fiscal crises and their political consequences; identification of triggering ratios in comparable jurisdictions',
    'If boundary is far (debt-to-GDP > 200%): extraction persists until economic crisis forces reset. If boundary is near (150-180%): extraction may self-terminate within 10-15 years through fiscal collapse. Classification remains snare up to the crisis point, then transitions to economic disorder (outside DR framework).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_sustainability_boundary, empirical, 'Fiscal sustainability boundary for extraction collapse').

omega_variable(
    intergenerational_bargain_reconstruction,
    'Can political coalitions reconstruct an intergenerational bargain (shared sacrifice, cross-generational fairness norms) that transforms the snare into genuine rope?',
    'Historical precedent from post-WWII consensus building, Nordic model sustainability, or other cases of sustained cross-generational fairness institutions; analysis of what political conditions enable norm-shifting away from maximalist self-interest',
    'If bargain is achievable: constraint could transition from snare to tangled rope or rope (lower suppression, genuine coordination benefit). If bargain is structurally impossible (demographic arithmetic locked in, electoral incentives immutable): snare persists until collapse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_bargain_reconstruction, conceptual, 'Whether intergenerational fairness norms can be reconstructed').

omega_variable(
    emigration_feedback_loop,
    'Does youth emigration accelerate, creating a feedback loop (smaller tax base → higher per-capita burden → more emigration → smaller base) that collapses the constraint faster?',
    'Time-series analysis of youth migration rates; correlation with fiscal burden indicators; modeling of positive feedback dynamics; comparison with jurisdictions experiencing brain drain',
    'If feedback loop is strong: constraint collapses in 15-25 years. If weak: constraint persists longer through residual compulsion (family ties, visa restrictions). Classification remains snare throughout, but timeline to collapse is critical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emigration_feedback_loop, empirical, 'Strength of youth emigration feedback loop').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demographic_inertia_trap, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demog_tr_t0, demographic_inertia_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(demog_tr_t10, demographic_inertia_trap, theater_ratio, 10, 0.39).
narrative_ontology:measurement(demog_tr_t20, demographic_inertia_trap, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(demog_be_t0, demographic_inertia_trap, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(demog_be_t10, demographic_inertia_trap, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(demog_be_t20, demographic_inertia_trap, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demographic_inertia_trap, resource_allocation).
narrative_ontology:affects_constraint(demographic_inertia_trap, housing_affordability_trap).
narrative_ontology:affects_constraint(demographic_inertia_trap, pension_liability_spiraling).
narrative_ontology:affects_constraint(demographic_inertia_trap, intergenerational_wage_compression).

% DUAL FORMULATION NOTE:
% The generational wealth siphon is upstream of housing affordability and wage compression constraints — older property holders vote for zoning restrictions that raise home prices (benefiting them, harming youth), and pension obligations compress working-age wages. These downstream constraints are partially caused by the demographic inertia trap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
