% ============================================================================
% CONSTRAINT STORY: family_social_conditions_hope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_family_social_conditions_hope, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: family_social_conditions_hope
 *   human_readable: Family Formation and Intergenerational Hope Under AI-Driven Precarity
 *   domain: catholic_social_teaching/technology_ethics/political_theology
 *
 * SUMMARY:
 *   The constraint 'family formation and intergenerational hope under
 *   AI-driven precarity' describes how platform economy volatility, gig work
 *   insecurity, and automation-driven unemployment undermine the material and
 *   psychological conditions necessary for young adults to form families and
 *   transmit hope to the next generation. This is downstream of the
 *   work_dignity_automation_unemployment constraint but structurally
 *   distinct: where the upstream constraint addresses the dignity of work
 *   itself, this constraint addresses the social reproduction consequences —
 *   the inability to transition into stable adulthood, form households, bear
 *   and raise children, and maintain intergenerational continuity. The
 *   constraint exhibits piton characteristics because traditional family
 *   support institutions (unemployment insurance designed for stable jobs,
 *   housing assistance assuming geographic stability, child benefits
 *   calibrated to single-earner households) persist but have atrophied —
 *   their eligibility criteria, benefit levels, and delivery mechanisms no
 *   longer match the precarious employment reality they were built to
 *   address. What remains is largely theatrical: the institutions perform
 *   support functions but cannot deliver meaningful stability to families
 *   navigating gig economy volatility. The theater_ratio has risen steadily
 *   from 0.35 (2000, pre-platform economy) to 0.68 (2025, mature platform
 *   precarity) as the gap between institutional form and functional capacity
 *   widened. Base extractiveness has risen more gradually (0.18 to 0.35) as
 *   platform intermediaries and automation capital holders captured the
 *   productivity gains from flexible labor while young adults and families
 *   bore the risk. The constraint is classified as piton from the social
 *   safety net institutional perspective, but as snare from the trapped young
 *   adult perspective, tangled_rope from the constrained family perspective,
 *   and scaffold from the organized UBI coalition perspective. The analytical
 *   perspective grounded in Catholic Social Teaching diagnoses the constraint
 *   as tangled_rope at the civilizational scale: genuine coordination
 *   challenges (labor markets adapting to technological change) are real, but
 *   the distribution of adaptation costs as precarity rather than shared
 *   prosperity is a political choice that violates solidarity, subsidiarity,
 *   and the dignity of work. The CST framework explicitly rejects
 *   technological determinism — AI's impact on family formation is contingent
 *   on human governance choices, not a natural law.
 *
 * KEY AGENTS:
 *   - Young Adults Seeking Stable Employment: Primary victim (powerless/trapped) — structurally unable to access employment stability that enables family formation; bears maximum extraction through foregone life-stage transitions
 *   - Families in Formation: Secondary victim (moderate/constrained) — experience mixed coordination (platform flexibility enables some participation) and extraction (precarity undermines long-term planning, forces delayed childbearing)
 *   - Platform Labor Intermediaries: Primary beneficiary (institutional/arbitrage) — capture value from labor market flexibility; experience constraint as coordination mechanism
 *   - Automation Capital Holders: Secondary beneficiary (institutional/arbitrage) — benefit from reduced labor costs and increased workforce flexibility enabled by AI-driven precarity
 *   - Social Safety Net Institutions: Institutional actor (institutional/constrained) — maintain atrophied support structures designed for stable employment paradigm; see own degradation but lack mandate to redesign (piton perspective)
 *   - UBI Advocacy Coalition: Organized agents (organized/mobile) — building alternative institutional pathways (UBI pilots, child allowances, housing guarantees) with sunset logic; see constraint as transitional coordination failure
 *   - Intergenerational Hope Transmission: Abstract victim (powerless/trapped) — collective good that cannot organize or exit; bears cost of despair accumulation across generations
 *   - Catholic Social Teaching Framework: Analytical observer (analytical/analytical) — diagnoses constraint as tangled_rope (genuine coordination challenges + extractive distribution choices); rejects technological determinism; insists on primacy of human dignity and common good
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(family_social_conditions_hope, 0.35).
domain_priors:suppression_score(family_social_conditions_hope, 0.4).
domain_priors:theater_ratio(family_social_conditions_hope, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(family_social_conditions_hope, extractiveness, 0.35).
narrative_ontology:constraint_metric(family_social_conditions_hope, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(family_social_conditions_hope, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(family_social_conditions_hope, piton).
narrative_ontology:human_readable(family_social_conditions_hope, "Family Formation and Intergenerational Hope Under AI-Driven Precarity").
narrative_ontology:topic_domain(family_social_conditions_hope, "catholic_social_teaching/technology_ethics/political_theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(family_social_conditions_hope, 'c271a875-c359-49e1-8c2d-d14665275f56').
narrative_ontology:cs_kernel_codification('c271a875-c359-49e1-8c2d-d14665275f56', formalized).
narrative_ontology:cs_authority_grounding('c271a875-c359-49e1-8c2d-d14665275f56', lineage).
narrative_ontology:cs_interpretation_layer_present('c271a875-c359-49e1-8c2d-d14665275f56').
narrative_ontology:cs_created_at('c271a875-c359-49e1-8c2d-d14665275f56', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(family_social_conditions_hope, platform_labor_intermediaries).
narrative_ontology:constraint_beneficiary(family_social_conditions_hope, automation_capital_holders).
narrative_ontology:constraint_victim(family_social_conditions_hope, young_adults_seeking_stable_employment).
narrative_ontology:constraint_victim(family_social_conditions_hope, families_in_formation).
narrative_ontology:constraint_victim(family_social_conditions_hope, intergenerational_hope_transmission).
narrative_ontology:constraint_vindicates(family_social_conditions_hope, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(family_social_conditions_hope, labor_flexibility_imperative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG ADULTS (SNARE) — Trapped in gig economy precarity with no pathway to stable employment that enables family formation. Job insecurity is structural, not cyclical. Maximum extraction: career instability prevents life-stage transitions (marriage, children, home ownership) that previous generations accessed at comparable ages. The coordination story (labor market flexibility) is cover for extraction.
constraint_indexing:constraint_classification(family_social_conditions_hope, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILIES IN FORMATION (TANGLED ROPE) — Constrained by dual-income necessity and childcare costs, but also benefit from some platform economy flexibility (remote work, gig income supplements). Mixed experience: genuine coordination function (flexible work arrangements enable some family participation) alongside asymmetric extraction (precarity undermines long-term planning, forces delayed childbearing, concentrates risk on households).
constraint_indexing:constraint_classification(family_social_conditions_hope, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM INTERMEDIARIES (ROPE) — Net beneficiaries. Experience the constraint as coordination: matching labor supply to demand, enabling flexible work arrangements. Arbitrage exit options: can shift between markets, regulatory jurisdictions, and business models. Low effective extraction because extraction flows toward this agent.
constraint_indexing:constraint_classification(family_social_conditions_hope, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOCIAL SAFETY NET (PITON) — Traditional family support institutions (unemployment insurance, housing assistance, child benefits) were designed for stable employment paradigm. These institutions persist but their function has atrophied: eligibility criteria assume continuous employment, benefit levels assume single-earner households, delivery mechanisms assume geographic stability. What remains is largely theatrical maintenance of a support structure that no longer matches the employment reality it was built to address. The institutions see their own degradation but lack mandate or resources to redesign for precarity.
constraint_indexing:constraint_classification(family_social_conditions_hope, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UBI COALITION (SCAFFOLD) — Organized agents (policy advocates, pilot programs, research networks) see the family formation crisis as a temporary coordination failure with a sunset: universal basic income or guaranteed employment programs would decouple survival from precarious labor markets, enabling family formation independent of gig economy volatility. The constraint is transitional — the coalition is building alternative institutional pathways (UBI pilots, child allowances, housing guarantees) that bypass the employment-stability requirement. Estimated sunset: 15-25 years for policy maturation.
constraint_indexing:constraint_classification(family_social_conditions_hope, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL / CST FRAMEWORK (TANGLED ROPE) — From the civilizational analytical perspective grounded in Catholic Social Doctrine, this constraint exhibits both genuine coordination challenges (labor markets must adapt to technological change) and extractive structures (AI-driven precarity concentrates wealth, undermines subsidiarity, violates dignity of work). The CST framework diagnoses the constraint as a failure of solidarity and common good: technological change is inevitable, but its distribution as precarity rather than shared prosperity is a political choice, not a natural law. The analytical classification is tangled_rope rather than mountain because the framework explicitly rejects technological determinism — the encyclical insists that AI's impact on family formation is contingent on human choices about regulation, distribution, and the primacy of human dignity.
constraint_indexing:constraint_classification(family_social_conditions_hope, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_social_conditions_hope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(family_social_conditions_hope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_social_conditions_hope, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(family_social_conditions_hope, TR),
    TR >= 0.70.

:- end_tests(family_social_conditions_hope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. Platform intermediaries and automation capital holders capture productivity gains from flexible labor arrangements while young adults and families bear the risk of income volatility, delayed family formation, and intergenerational hope erosion. The extraction is substantial but not maximal — some genuine coordination benefits exist (remote work flexibility, supplemental gig income), and the constraint has not yet collapsed family formation entirely (fertility rates have declined but not reached zero). The value reflects that extraction is real and rising but not yet at snare-level severity for all agents. Suppression (0.40): Moderate. Young adults face significant barriers to stable employment (credential inflation, geographic concentration of opportunities, automation of entry-level positions, gig economy lock-in) but suppression is not total — some pathways to stability remain (credentialed professions, public sector, geographic arbitrage). The suppression is higher for less-educated workers and lower for those with in-demand technical skills. Theater ratio (0.68): High. Traditional family support institutions perform their historical functions (processing unemployment claims, administering housing vouchers, distributing child benefits) but these functions are increasingly disconnected from the precarious employment reality they purport to address. Eligibility criteria assume continuous employment; benefit levels assume single-earner households; delivery mechanisms assume geographic stability. The gap between institutional form and functional capacity has widened steadily as platform economy precarity matured. The theater is maintained through institutional inertia and political symbolism (politicians defend 'family values' while presiding over systems that cannot support family formation), not because the institutions work. The piton classification derives from this theater gate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — AI-driven employment precarity undermining family formation — appears as different constraint types depending on the observer's structural position and time horizon. Young adults trapped in gig economy volatility experience pure extraction (snare): the coordination story (labor market flexibility) is cover, and they bear maximum cost through foregone life-stage transitions. Families in formation experience mixed coordination and extraction (tangled_rope): platform flexibility provides some genuine benefits (remote work, schedule control) but precarity concentrates risk on households and undermines long-term planning. Platform intermediaries experience coordination (rope): they are net beneficiaries solving the legitimate problem of matching labor supply to demand. Social safety net institutions see their own degraded ritual (piton): support structures persist through inertia but can no longer deliver meaningful stability. The UBI coalition sees a temporary problem with a sunset (scaffold): alternative institutional pathways are being built. The analytical observer grounded in Catholic Social Teaching sees tangled_rope at civilizational scale: genuine coordination challenges exist, but the distribution of costs as precarity rather than shared prosperity is a political choice that violates solidarity and dignity. The perspectival gap reveals that 'is this extraction or coordination?' depends on who is answering and from what time horizon. The constraint is not one type — it is a presheaf over observation sites.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. Young adults seeking stable employment are victims with trapped exit → high d → high effective extraction (snare classification). Families in formation are both victims (precarity risk) and beneficiaries (platform flexibility) with constrained exit → moderate d → moderate effective extraction, modulated by the mixed structural relationship (tangled_rope classification). Platform labor intermediaries are beneficiaries with arbitrage exit → low d → low or negative effective extraction (rope classification). Social safety net institutions are neither clear beneficiaries nor victims — they are institutional actors maintaining atrophied functions — but their constrained exit options (cannot redesign without legislative mandate) and lack of clear extraction flow toward them produces moderate d. The piton classification derives from the theater gate, not from high effective extraction. The UBI coalition has mobile exit options (can shift advocacy strategies, build pilots in different jurisdictions) and sees itself as solving the problem → low d. The analytical observer has analytical exit and sees the constraint as a mixed structure → moderate d, but the classification (tangled_rope) derives from the structural diagnosis (genuine coordination + extractive distribution) rather than from experienced extraction. The intergenerational hope transmission victim is an abstract collective with no exit options → maximum d, but as an abstract good it cannot be assigned a power level in the standard tuple (it appears in the victim list but not as a perspective agent).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's mandate (family support institutions designed to enable stable family formation in an industrial employment paradigm) has outlived its function (those institutions cannot address precarity in a platform economy). The piton classification captures this: the institutions persist through inertia and political theater, not because they work. The mandatrophy is resolved by recognizing that the constraint is no longer serving its original coordination function — what remains is performance. The scaffold perspective (UBI coalition) represents the potential resolution pathway: new institutions (universal basic income, guaranteed employment, child allowances decoupled from employment status) could restore the coordination function the old institutions have lost. The analytical perspective grounded in Catholic Social Teaching diagnoses the mandatrophy as a failure of solidarity: the institutions were designed for a social contract (stable employment in exchange for productive labor) that has been unilaterally abrogated by capital, and the persistence of the old institutional forms without adaptation is itself an extractive choice — it allows political actors to claim they support families while presiding over systems that structurally undermine family formation. The mandatrophy is not an accident; it is maintained because redesigning the institutions would require redistribution that current beneficiaries resist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precarity_permanence,
    'Is AI-driven employment precarity a permanent structural feature of post-industrial economies, or a transitional phase resolvable through policy intervention?',
    'Longitudinal comparison of family formation rates in jurisdictions with strong vs. weak labor protections and social safety nets; analysis of UBI pilot outcomes on family stability metrics; historical precedent from previous automation waves (mechanization, computerization) and their eventual policy responses.',
    'If permanent: scaffold perspective is aspirational, piton perspective is terminal state, young adults remain structurally trapped. If transitional: scaffold sunset is real, policy intervention can restore family formation pathways, constraint resolves within generational timeframe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precarity_permanence, empirical, 'Whether AI-driven precarity is permanent or policy-resolvable').

omega_variable(
    intergenerational_hope_threshold,
    'At what level of youth unemployment and family formation delay does intergenerational hope transmission collapse irreversibly?',
    'Cross-national comparison of societies with varying youth unemployment rates and their measured social trust, civic participation, and fertility intentions; psychological research on hope formation and its dependence on perceived life-course predictability; historical analysis of post-crisis hope recovery timelines.',
    'If threshold is low (e.g., >15% youth unemployment for >5 years): many developed economies have already crossed into irreversible hope collapse, and the constraint''s victim set expands to include future generations who inherit despair. If threshold is high (e.g., >30% for >10 years): current precarity is damaging but recoverable, and policy intervention remains viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_hope_threshold, empirical, 'Threshold at which intergenerational hope transmission becomes irreversible').

omega_variable(
    platform_flexibility_genuine_benefit,
    'Does platform economy flexibility (remote work, gig income, schedule control) provide genuine family coordination benefits, or is the flexibility narrative a cover story for risk transfer?',
    'Controlled comparison of family wellbeing metrics (relationship stability, parental stress, child development outcomes) between platform workers with ''flexible'' arrangements and traditional workers with stable employment; analysis of whether flexibility is chosen (genuine benefit) or imposed (extraction masked as choice).',
    'If genuine benefit: tangled_rope classification for families is accurate — real coordination function exists alongside extraction. If cover story: families experience snare, not tangled_rope — the flexibility narrative is extractive framing that obscures pure risk transfer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_flexibility_genuine_benefit, empirical, 'Whether platform flexibility is genuine coordination or extractive framing').

omega_variable(
    cst_framework_authority_erosion,
    'Does Catholic Social Teaching retain sufficient moral authority in secularized societies to influence AI governance, or has its authority eroded to the point where its analytical framework is heard only within the Catholic community?',
    'Analysis of CST citation patterns in secular policy documents, legislative debates, and corporate governance frameworks; comparison of policy outcomes in majority-Catholic vs. secularized jurisdictions; measurement of CST influence on AI ethics frameworks outside explicitly Catholic institutions.',
    'If authority eroded: the analytical perspective''s tangled_rope diagnosis is structurally sound but politically inert — the framework cannot influence the constraint''s evolution. If authority retained: CST provides a live alternative to technocratic and market-fundamentalist framings, and its solidarity/subsidiarity principles could shape policy intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cst_framework_authority_erosion, conceptual, 'Whether CST retains moral authority in secular AI governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(family_social_conditions_hope, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fam_hope_theater_2000, family_social_conditions_hope, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fam_hope_theater_2005, family_social_conditions_hope, theater_ratio, 5, 0.42).
narrative_ontology:measurement(fam_hope_theater_2010, family_social_conditions_hope, theater_ratio, 10, 0.51).
narrative_ontology:measurement(fam_hope_theater_2015, family_social_conditions_hope, theater_ratio, 15, 0.59).
narrative_ontology:measurement(fam_hope_theater_2020, family_social_conditions_hope, theater_ratio, 20, 0.64).
narrative_ontology:measurement(fam_hope_theater_2025, family_social_conditions_hope, theater_ratio, 25, 0.68).

% Extraction over time
narrative_ontology:measurement(fam_hope_extract_2000, family_social_conditions_hope, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fam_hope_extract_2005, family_social_conditions_hope, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(fam_hope_extract_2010, family_social_conditions_hope, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(fam_hope_extract_2015, family_social_conditions_hope, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(fam_hope_extract_2020, family_social_conditions_hope, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(fam_hope_extract_2025, family_social_conditions_hope, base_extractiveness, 25, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(family_social_conditions_hope, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of work_dignity_automation_unemployment but structurally distinct. The upstream constraint addresses the dignity of work and the right to employment; this constraint addresses the social reproduction consequences — family formation, intergenerational hope transmission, and the material conditions for stable adulthood. Both constraints share beneficiaries (platform intermediaries, automation capital holders) and some victim overlap (young adults, precarious workers), but their observables and ε values differ. The upstream constraint's ε reflects the extraction embedded in work itself (wage theft, algorithmic management, deskilling); this constraint's ε reflects the extraction embedded in the inability to form families and transmit hope. The constraints are linked but not identical.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
