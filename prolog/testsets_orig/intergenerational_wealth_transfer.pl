% ============================================================================
% CONSTRAINT STORY: intergenerational_wealth_transfer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intergenerational_wealth_transfer, []).

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
 *   constraint_id: intergenerational_wealth_transfer
 *   human_readable: Intergenerational Wealth Transfer Constraint
 *   domain: economic/social/institutional
 *
 * SUMMARY:
 *   Intergenerational wealth transfer is a structural constraint that
 *   exhibits simultaneous coordination and extraction functions. At the
 *   biological level, parental investment in children is universal and
 *   functional. At the economic level, the institutional constraint of
 *   heritable wealth creates asymmetric accumulation advantages that compound
 *   across generations. The constraint operates through multiple mechanisms:
 *   legal frameworks (wills, trusts, property rights), economic structures
 *   (asset inflation, wage stagnation, capital access barriers), and cultural
 *   narratives (inherited wealth as natural, earned prosperity as reward for
 *   virtue). The system coordinates legitimate intergenerational provision
 *   while extracting from non-inheriting populations through suppressed
 *   mobility and wealth concentration. The increasing theater ratio (0.48 at
 *   endpoint) reflects growing performative elements: inheritance portrayed
 *   as earned despite growing recognition of luck and privilege in wealth
 *   outcomes.
 *
 * KEY AGENTS:
 *   - Wealthy Dynasties: Primary beneficiary (institutional/arbitrage) — capture exponential wealth compounding; experience the constraint as legitimate family coordination
 *   - Non-Inheriting Workers: Primary victim (powerless/trapped) — trapped in lifetime wage dependency; bear cost of suppressed alternative accumulation pathways
 *   - Estate Planning & Financial Professionals: Secondary beneficiary (institutional/arbitrage) — profit from wealth preservation and optimization services; experience constraint as pure coordination opportunity
 *   - Aspiring Wealth Accumulators: Secondary victim (moderate/constrained) — face real barriers but some exit paths exist; experience mixed coordination and extraction
 *   - Regulatory and Tax Authorities: Organized institutional actor (organized/constrained) — constrained by enforcement limits and political economy; experience both coordination role and extraction suppression
 *   - Legal/Inheritance Framework: Institutional structure (institutional/mobile) — persists through inertia; experiences own degradation as theater increases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intergenerational_wealth_transfer, 0.58).
domain_priors:suppression_score(intergenerational_wealth_transfer, 0.65).
domain_priors:theater_ratio(intergenerational_wealth_transfer, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intergenerational_wealth_transfer, extractiveness, 0.58).
narrative_ontology:constraint_metric(intergenerational_wealth_transfer, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(intergenerational_wealth_transfer, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intergenerational_wealth_transfer, tangled_rope).
narrative_ontology:human_readable(intergenerational_wealth_transfer, "Intergenerational Wealth Transfer Constraint").
narrative_ontology:topic_domain(intergenerational_wealth_transfer, "economic/social/institutional").

domain_priors:requires_active_enforcement(intergenerational_wealth_transfer).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intergenerational_wealth_transfer, wealthy_dynasties).
narrative_ontology:constraint_beneficiary(intergenerational_wealth_transfer, estate_planning_professionals).
narrative_ontology:constraint_beneficiary(intergenerational_wealth_transfer, financial_institutions).
narrative_ontology:constraint_victim(intergenerational_wealth_transfer, non_inheriting_populations).
narrative_ontology:constraint_victim(intergenerational_wealth_transfer, wage_dependent_workers).
narrative_ontology:constraint_victim(intergenerational_wealth_transfer, economically_mobile_youth).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-INHERITING WORKER (SNARE) — Trapped in the constraint through economic dependency. Birth into a family without inheritable wealth means lifetime structural disadvantage in wealth accumulation. Exit from the constraint requires generational timescale and is blocked by suppression mechanisms: wage stagnation, housing cost inflation, student debt, and unequal access to capital. The constraint extracts lifetime labor value through asymmetric opportunity.
constraint_indexing:constraint_classification(intergenerational_wealth_transfer, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ASPIRING WEALTH ACCUMULATOR (TANGLED ROPE) — Constrained but not trapped. Faces real barriers to wealth accumulation (high debt service, limited access to investment capital, real estate gatekeeping) but some exit exists through entrepreneurship, professional mobility, or savings discipline. The constraint coordinates legitimate intergenerational incentives (parents supporting children) alongside asymmetric extraction (inherited wealth compounds exponentially while earned wealth stagnates). Experiences both coordination and extraction.
constraint_indexing:constraint_classification(intergenerational_wealth_transfer, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INSTITUTIONS & ESTATE PLANNING (ROPE) — Net beneficiaries experiencing the constraint as pure coordination. Tax optimization structures, trust arrangements, and wealth management services solve the collective action problem of intergenerational capital preservation. The industry benefits from the constraint but also enables genuine coordination: helping families preserve and transfer assets according to their intentions. Experiences low extraction because they have full arbitrage — can exit, restructure, or pivot their services.
constraint_indexing:constraint_classification(intergenerational_wealth_transfer, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY/TAX AUTHORITY (TANGLED ROPE) — Organized institutional actor constrained by enforcement capacity and political economy. The constraint coordinates legitimate intergenerational incentives (families want to support heirs) alongside extraction (wealth preservation under the constraint reduces tax revenue and prevents wealth redistribution). The authority benefits from compliance and legitimacy but also bears costs — managing sophisticated tax avoidance, enforcing estate taxation, and negotiating political pressure from wealthy constituencies. Constrained by the need to maintain both system legitimacy and enforcement capacity.
constraint_indexing:constraint_classification(intergenerational_wealth_transfer, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INHERITANCE LAW FRAMEWORK (PITON) — Institutional structure that persists through inertia despite functional degradation. Traditional inheritance law (fee simple property, testamentary freedom, trust instruments) was designed for agrarian land-based wealth but now coordinates mostly financial and intangible assets where the framework's assumptions break down. The constraint is maintained through legal institutionalization and legitimacy theater (inheritance portrayed as 'natural family right') rather than functional necessity. Theater ratio high because the performative aspects (will ceremonies, probate rituals) persist despite digitization making them obsolete.
constraint_indexing:constraint_classification(intergenerational_wealth_transfer, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some form of intergenerational resource transfer is inherent to any species with parental investment and prolonged offspring dependency. Parents naturally allocate resources to their children; this is immutable biology + basic reciprocity. From this view, wealth transfer constraints are simply the economic manifestation of a universal pattern. However, the structural data reveals this as a false summit: the natural law of parental provisioning (low extraction) is distinct from the institutional constraint of inheritable wealth inequality (high extraction). Confusing them naturalizes what is actually contingent architecture.
constraint_indexing:constraint_classification(intergenerational_wealth_transfer, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intergenerational_wealth_transfer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intergenerational_wealth_transfer, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intergenerational_wealth_transfer, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intergenerational_wealth_transfer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intergenerational_wealth_transfer, TR),
    TR >= 0.70.

:- end_tests(intergenerational_wealth_transfer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from non-inheriting populations through several mechanisms: (1) suppressed wealth accumulation pathways due to capital access barriers, (2) wage stagnation relative to asset inflation, (3) inheritance-dependent housing and education access. But extractiveness is not maximal (< 0.70) because some legitimate coordination genuinely occurs — parental provision for children is functional and not entirely extractive. The rising trajectory (0.35 → 0.58 over 50 years) reflects increasing wealth concentration as inherited wealth compounds while wage-dependent accumulation stagnates. Suppression (0.65): Moderate-high. Multiple mechanisms suppress non-inheritor exit: student debt loads, real estate price inflation, equity market access barriers, professional credentialing costs, unequal access to capital. But suppression is not total — entrepreneurship and professional advancement remain possible, though increasingly costly. Theater ratio (0.48): Moderate. The constraint increasingly exhibits performative elements as its actual function becomes visible: inheritance law portrayed as natural family right despite growing recognition of luck and privilege; meritocratic narratives (earned wealth) contradicted by inheritance data; mobility rhetoric unsupported by actual intergenerational mobility rates.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is stark. A wealthy family experiences intergenerational wealth transfer as natural family coordination, personal responsibility, and earned asset preservation — classification: rope. A non-inheriting worker experiences the same institutional system as structural barrier to wealth accumulation, generational disadvantage, and compounding inequality — classification: snare. Both are evaluating identical legal/economic structures but from opposite positions on the extraction flow. The gap reveals that the constraint's coordination function (legitimate parental provision) and extraction function (compounding inequality) are structurally entangled and cannot be separated by tweaking legal language or tax codes alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's structural position. Wealthy dynasties with inherited capital enjoy arbitrage exit options and beneficiary status (d ≈ 0.05-0.15), experiencing low or negative effective extraction. Non-inheritors trapped in wage dependency without inherited capital face high d (0.85-0.95), experiencing maximum extraction. Aspiring accumulators with some capital but significant barriers face moderate d (0.50-0.65). Financial professionals with full arbitrage and beneficiary status face low d. The regulatory authority constrained by enforcement capacity and political economy faces moderate-high d (0.45-0.55). The analytical observer faces highest d from the false summit perspective (0.75), being deceived into naturalizing contingent architecture. The suppression mechanisms that block exit paths (debt, housing inflation, capital barriers) all increase experienced d by reducing actual exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   Intergenerational wealth transfer resolves the mandatrophy by decomposing into structurally distinct constraints: (1) PARENTAL PROVISION (ε ≈ 0.05, Mountain or Rope) — the natural biological/economic reality of parents provisioning children, low extraction, high legitimacy; (2) INHERITABLE WEALTH ACCUMULATION (ε ≈ 0.58, Tangled Rope) — the institutional architecture enabling wealth compounding across generations, moderate-high extraction, mixed legitimacy; (3) WEALTH CONCENTRATION MECHANISM (ε ≈ 0.72, Snare) — the emergent effect of compound inheritance plus suppressed alternative accumulation for non-inheritors, high extraction from systemic perspective. The tangled rope classification at the system level is correct: genuine coordination (families providing for heirs) is inseparable from asymmetric extraction (non-inheritors trapped in wage dependency). The theater ratio tracks how much the system's narrative (meritocracy, earned prosperity) diverges from its actual function (inheritance-driven compounding), indicating Goodhart drift and increasing performative maintenance of legitimacy as the actual extraction becomes visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inheritance_vs_earned_wealth_boundary,
    'What portion of observed wealth concentration derives from inheritance privileges versus from differential earning capacity?',
    'Longitudinal wealth tracking of family dynasties versus high-earning individuals without family wealth; generational mobility analysis; counterfactual scenarios with equal starting capital',
    'If inheritance > 60% of concentration: snare classification confirmed; constraint is primarily extractive. If inheritance < 30% of concentration: rope classification more accurate; constraint primarily enables coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inheritance_vs_earned_wealth_boundary, empirical, 'Proportion of wealth concentration due to inheritance versus earning').

omega_variable(
    exit_mechanisms_authenticity,
    'Are the exit paths for non-inheriting populations (entrepreneurship, professional advancement, savings) structurally genuine or theatrically available but practically blocked?',
    'Success rate analysis for self-made wealth in each income decile; comparison of mobility rates across countries with different inheritance tax regimes; identification of suppression mechanisms that block apparent exit paths',
    'If exits are authentic: constrained classification more accurate; multiple paths exist. If exits are theatrical: trapped classification more accurate; apparent mobility is illusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_mechanisms_authenticity, empirical, 'Whether exit mechanisms for non-inheritors are structurally genuine').

omega_variable(
    tax_avoidance_sophistication_gap,
    'Does the sophistication gap between wealthy family tax optimization and regulatory enforcement create a de facto dual system with systematically different effective tax rates by wealth level?',
    'Effective tax rate analysis by wealth decile; audit rate disparity; complexity of avoidance strategies available only to high-wealth households; comparison of statutory versus realized tax burdens',
    'If gap is large: extraction mechanism confirmed; constraint enforces different rules by wealth level. If gap is small: equal enforcement confirmed; constraint is more transparent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_avoidance_sophistication_gap, empirical, 'Effective tax rate gap between wealthy families and others').

omega_variable(
    suppression_mechanism_internalization,
    'To what degree is the suppression of non-inheriting populations'' wealth accumulation structural (external barriers like asset inflation, wage stagnation) versus internalized (learned helplessness, identity as non-owner, cultural narratives of inevitability)?',
    'Post-barrier removal trajectory analysis; comparison of mobility between jurisdictions with different suppression levels; psychological assessment of wealth-building self-efficacy among income groups',
    'If suppression is primarily structural: removing barriers would enable exit. If suppression is primarily internalized: psychological intervention and cultural reframing would be necessary alongside barrier removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Degree of structural versus internalized suppression').

omega_variable(
    global_wealth_migration_arbitrage,
    'Does the option to migrate to jurisdictions with lower inheritance taxation or less-enforced wealth transfer restrictions constitute genuine arbitrage for wealthy families, creating a race-to-the-bottom dynamic that weakens all regulatory frameworks?',
    'Tracking of wealth migration patterns; correlation between inheritance tax rates and capital flight; effectiveness of international wealth taxation agreements',
    'If arbitrage is strong: institutional actors effectively mobile; tangled rope classification accurate. If arbitrage is weak: institutional actors constrained; regulatory authority''s snare classification accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_wealth_migration_arbitrage, empirical, 'Degree of global arbitrage reducing domestic wealth transfer regulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intergenerational_wealth_transfer, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(igwt_tr_t0, intergenerational_wealth_transfer, theater_ratio, 0, 0.32).
narrative_ontology:measurement(igwt_tr_t25, intergenerational_wealth_transfer, theater_ratio, 25, 0.4).
narrative_ontology:measurement(igwt_tr_t50, intergenerational_wealth_transfer, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(igwt_be_t0, intergenerational_wealth_transfer, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(igwt_be_t25, intergenerational_wealth_transfer, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(igwt_be_t50, intergenerational_wealth_transfer, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intergenerational_wealth_transfer, resource_allocation).
narrative_ontology:affects_constraint(intergenerational_wealth_transfer, wealth_concentration).
narrative_ontology:affects_constraint(intergenerational_wealth_transfer, intergenerational_mobility).
narrative_ontology:affects_constraint(intergenerational_wealth_transfer, estate_tax_enforcement).
narrative_ontology:affects_constraint(intergenerational_wealth_transfer, real_estate_access_barriers).

% DUAL FORMULATION NOTE:
% Intergenerational wealth transfer decomposes into three structurally distinct constraints with different ε values. The parental provision coordination (ε ≈ 0.05) is natural and low-extraction. The institutional wealth inheritance architecture (ε ≈ 0.58) is contingent and moderate-extraction. The emergent wealth concentration outcome (ε ≈ 0.72) is highly extractive from systemic perspective. This story addresses the institutional constraint (0.58); separate stories address parental provision and concentration mechanics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intergenerational_wealth_transfer, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
