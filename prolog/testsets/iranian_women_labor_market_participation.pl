% ============================================================================
% CONSTRAINT STORY: iranian_women_labor_market_participation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_iranian_women_labor_market_participation, []).

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
 *   constraint_id: iranian_women_labor_market_participation
 *   human_readable: Iranian Women Labor Market Participation Constraint
 *   domain: economic/political/gender
 *
 * SUMMARY:
 *   Iranian women's labor market participation is structurally constrained by
 *   a complex system combining formal legal restrictions (spousal permission
 *   requirements, occupational prohibitions in certain sectors), state
 *   religious enforcement through institutional mechanisms, cultural
 *   suppression through honor frameworks, and material barriers (childcare
 *   unavailability, wage penalties, occupational segregation). The constraint
 *   exhibits characteristics of a snare from the perspective of women seeking
 *   market entry: high suppression, asymmetric enforcement, and limited exit
 *   options. However, multiple institutional perspectives reveal the
 *   constraint as a mixed coordination-extraction hybrid. The state religious
 *   authority experiences the constraint as pure coordination (family
 *   stability maintenance). Male wage-earners experience it as coordination
 *   (household role differentiation). Women's rights movements and working
 *   women experience it as extraction layered onto genuine household economic
 *   coordination. The theater ratio increase over the interval reflects
 *   growing divergence between formal legal frameworks (which have
 *   incrementally expanded female employment rights) and actual enforcement
 *   mechanisms (which rely increasingly on informal sanction and honor-based
 *   social pressure rather than state coercion). This gap suggests the
 *   constraint is transitioning from direct enforcement (snare with state
 *   coercion) toward institutional inertia (piton) as formal legal barriers
 *   erode faster than enforcement mechanisms adapt.
 *
 * KEY AGENTS:
 *   - Iranian Women (all categories): Primary victims (powerless/trapped or identity_locked) — face legal restrictions, cultural enforcement, economic barriers; bear full cost of labor market exclusion
 *   - State Religious Authority: Primary beneficiary (institutional/arbitrage) — maintains social legitimacy through family structure regulation; experiences constraint as coordination mechanism
 *   - Male Wage-Earners: Primary beneficiary (powerful/arbitrage) — monopolize household economic decision-making and wage-earning status; benefit from female domestic labor concentration
 *   - Women's Rights Movements: Organized secondary actor (organized/constrained) — perceive constraint as changeable through generational action but operate within state repression constraints
 *   - Working Women: Secondary victim/moderate (moderate/constrained) — have entered labor market but experience extraction through dual burden, wage penalties, occupational ceiling effects
 *   - Patriarchal Household Institution: Structural actor (institutional/arbitrage) — maintains itself through cultural inertia and informal enforcement; increasingly theatrical
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(iranian_women_labor_market_participation, 0.68).
domain_priors:suppression_score(iranian_women_labor_market_participation, 0.72).
domain_priors:theater_ratio(iranian_women_labor_market_participation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(iranian_women_labor_market_participation, extractiveness, 0.68).
narrative_ontology:constraint_metric(iranian_women_labor_market_participation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(iranian_women_labor_market_participation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(iranian_women_labor_market_participation, snare).
narrative_ontology:human_readable(iranian_women_labor_market_participation, "Iranian Women Labor Market Participation Constraint").
narrative_ontology:topic_domain(iranian_women_labor_market_participation, "economic/political/gender").

domain_priors:requires_active_enforcement(iranian_women_labor_market_participation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(iranian_women_labor_market_participation, patriarchal_household_structure).
narrative_ontology:constraint_beneficiary(iranian_women_labor_market_participation, male_wage_earners).
narrative_ontology:constraint_beneficiary(iranian_women_labor_market_participation, state_religious_authority).
narrative_ontology:constraint_victim(iranian_women_labor_market_participation, women_labor_force_participants).
narrative_ontology:constraint_victim(iranian_women_labor_market_participation, female_economic_autonomy).
narrative_ontology:constraint_victim(iranian_women_labor_market_participation, household_economic_resilience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IRAQI WOMAN SEEKING LABOR PARTICIPATION (SNARE) — Faces legal restrictions (spousal permission requirements), cultural enforcement (family honor frameworks), economic barriers (childcare unavailability), and career penalties for motherhood. Exit options are materially constrained by legal dependency in marriage, social ostracism risks, and absence of economic infrastructure supporting female workforce participation. Maximum experienced extraction — no alternatives available within structural constraints.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WOMAN AS IDENTITY-LOCKED FAMILY MEMBER (SNARE) — Identity constituted through familial roles (wife, mother, daughter) and honor frameworks makes labor market participation literally unthinkable for many women even where legal barriers are technically surmountable. The woman cannot perceive exit because exit would mean dissolution of core identity. Structurally mobile (could work) but psychologically/culturally trapped through identity fusion with household role. Snare classification at biographical horizon; would be rope at generational if identity frame shifted, revealing the lock as cognitive rather than material.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: WORKING WOMAN MANAGING DUAL BURDEN (TANGLED ROPE) — Women who have entered labor market face genuine coordination function (household economic management, childcare coordination with employers) alongside asymmetric extraction (disproportionate domestic labor, wage penalties, occupational segregation). Not trapped (has employment) but constrained by high costs of work-life coordination. Mixed experience: genuine economic benefit from employment but systematic extraction through unpaid domestic work and occupational ceiling effects.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: STATE RELIGIOUS AUTHORITY (ROPE) — Experiences the constraint as pure coordination: enforcement of gender role differentiation maintains social stability and religious legitimacy through family structure regulation. The state sees this as solving a coordination problem (maintaining family structure and social order). Net beneficiary through institutional legitimacy maintenance. Low experienced extraction because the constraint serves the authority's primary function.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MALE WAGE-EARNER AS FAMILY PROVIDER (ROPE) — Benefits from female labor market exclusion through monopoly on wage-earning status, reduced domestic labor burden, and concentration of household economic decision-making. Experiences the constraint as coordinating family economic roles. Exit options are arbitrage (can negotiate household arrangements at low cost). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: WOMEN'S RIGHTS MOVEMENT (TANGLED ROPE) — Organized actors (women's NGOs, female scholars, organized women workers) see genuine coordination function (society needs female labor force participation for economic productivity and household welfare) alongside state enforcement mechanisms that extract legitimacy from traditional frames. The movement has constrained exit options (operating within state constraints, subject to repression) but perceives the constraint as changeable through collective action at generational time scale.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TRADITIONAL PATRIARCHAL HOUSEHOLD STRUCTURE (PITON) — The household institution itself has become substantially theatrical — many enforcement mechanisms persist through cultural inertia and informal sanction rather than formal law. Legal reforms have created gaps (women can legally work in some sectors; spousal permission legally required but often waived in practice) between formal constraints and actual practice. Theater ratio reflects that enforcement relies on reputation maintenance and family honor narratives rather than direct state coercion. The institution persists because alternatives haven't fully replaced it, not because it functions effectively.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURALIZED ESSENTIALISM (MOUNTAIN) — From a civilizational perspective, this perspective naturalizes gender role differentiation as inherent to social order, framing women's labor exclusion as a natural consequence of biological reproduction roles and necessary family stability. However, structural data contradicts the mountain classification — comparative evidence shows female labor participation is compatible with family stability, religious commitment, and social order across diverse societies. This is a false summit: the engine will identify it as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(iranian_women_labor_market_participation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(iranian_women_labor_market_participation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(iranian_women_labor_market_participation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(iranian_women_labor_market_participation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(iranian_women_labor_market_participation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(iranian_women_labor_market_participation, TR),
    TR >= 0.70.

:- end_tests(iranian_women_labor_market_participation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High but not maximal. The constraint extracts significant economic value through female labor market exclusion (unpaid domestic labor, foregone wages, household economic dependence on male earning). However, extractiveness is not at maximum (0.85+) because: (a) many women have achieved labor market entry in recent decades, (b) some household welfare does genuinely depend on role differentiation coordination, (c) extraction is framed as protection rather than pure coercion. The value reflects that extraction occurs alongside a partially genuine coordination function. Suppression (0.72): High. Multiple suppression mechanisms operate simultaneously: legal (spousal permission requirements), cultural (honor/reputation frameworks creating family/community sanction), economic (childcare unavailability, wage penalties making work unaffordable for many), and institutional (state enforcement through employment regulations and religious authority messaging). These are not surmountable at individual level; they create structural barriers. Theater ratio (0.58): Moderate-high. Legal reforms have created theatrical gaps — formal law increasingly permits female employment, but enforcement mechanisms rely on informal cultural pressure and honor narratives rather than direct state prohibition. The theater ratio's increase over time reflects this divergence: formal constraints have eased while enforcement mechanisms have shifted from law to reputation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence despite uniform structural data. Women seeking entry see snare (high extraction, no exit, trapped). Working women see tangled_rope (genuine household economic coordination mixed with extraction through dual burden). Men see rope (coordination mechanism for family roles, net beneficial). State authority sees rope (coordination for social stability, net beneficial). Women's movements see tangled_rope (genuine need for household economic coordination alongside extractive enforcement mechanisms). The patriarchal household institution sees itself as piton (increasingly theater-dependent, enforced through reputation rather than function). The analytical observer risks seeing mountain (naturalizing gender role differentiation as inherent to social order). This perspectival gap reveals that the constraint is NOT a natural law but a contingent institutional arrangement that benefits specific actors and suppresses alternatives through cultural framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to the extraction flow. Powerless women seeking entry (trapped or identity_locked) occupy maximum d positions (0.92-0.98) — the constraint extracts directly from them. Identity-locked women have slightly lower d (0.85-0.92) because their exit options are cognitively constrained rather than materially impossible, creating a marginal possibility of frame-shift. Organized women's movements (constrained exit, organized power) occupy moderate d (0.55-0.65) — they experience extraction but have collective agency to resist. Male wage-earners (arbitrage exit, powerful) occupy low d (0.15-0.25) — the constraint subsidizes them. State religious authority (arbitrage exit, institutional) occupies very low d (0.05-0.15) — the constraint serves state interests. These directionality values feed the sigmoid f(d) to produce experienced extractiveness (chi) from each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by decomposing the constraint into its genuine coordination function and its extractive layer. The coordination function is real: households do need to coordinate labor, childcare, and household work. The extraction is real: that coordination is asymmetrically organized to benefit male economic monopoly and state religious authority. A coherent snare classification requires showing that: (1) the suppression mechanisms are sufficient to prevent exit (yes — legal, cultural, economic, institutional), (2) the extraction flow is substantial (yes — unpaid domestic labor, wage penalties, occupational segregation), and (3) minimal coordination benefit accrues to victims (yes — working women see coordination benefit, but most excluded women see none). The constraint qualifies as snare from the perspective of women seeking entry (trapped, no coordination benefit, high suppression, high extraction). For women already participating, it reclassifies as tangled_rope (constrained exit, genuine coordination of household and work alongside extraction through dual burden). This perspectival multiplicity does not indicate failed classification — it indicates the constraint operates differently across the affected population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_vs_customary_enforcement,
    'What proportion of the suppression barrier is enforced by formal law versus customary social sanction?',
    'Comparative analysis of women''s labor outcomes in regions with identical cultural norms but different legal frameworks; evaluation of behavior change when formal legal barriers are removed',
    'If primarily formal: legal reform could rapidly reduce suppression. If primarily customary: suppression persists through identity and honor frameworks even after legal reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_vs_customary_enforcement, empirical, 'Proportion of suppression from formal law versus customary enforcement').

omega_variable(
    identity_lock_permeability,
    'Is the identity lock preventing female labor participation (woman-as-wife/mother identity) permeable to economic incentives, or does it require identity-frame transformation?',
    'Longitudinal tracking of female labor participation changes correlated with: (a) economic necessity thresholds, (b) peer network shifts (female relative or neighbor entering workforce), (c) educational exposure, (d) spousal attitude change',
    'If permeable to economic incentives: constraint can degrade through material pressure. If requires identity transformation: constraint persists despite economic pressure and requires cultural-narrative intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_permeability, empirical, 'Whether identity lock responds to economic incentives or requires identity-frame change').

omega_variable(
    household_economic_resilience_tradeoff,
    'What is the net household welfare effect of women''s labor market exclusion — does male wage monopoly plus female unpaid domestic labor produce greater household economic resilience than dual-income structures?',
    'Comparative household economic analysis: income stability, poverty vulnerability, shock absorption capacity (male unemployment, health crisis) across single-income vs dual-income households at equivalent education/skill levels',
    'If female exclusion increases household resilience: suppression mechanism serves a genuine welfare function (coordination framing becomes credible). If dual-income households show greater resilience: suppression mechanism is pure extraction framed as protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(household_economic_resilience_tradeoff, empirical, 'Net household economic resilience from female exclusion versus dual-income participation').

omega_variable(
    occupational_segregation_extraction,
    'Is occupational segregation (women concentrated in lower-wage sectors) a function of genuine skill matching/preference differences or asymmetric extraction?',
    'Comparative wage analysis: same occupation, same education level, female vs male wages; hiring patterns when gender information is obscured; career trajectory analysis for occupational switching patterns',
    'If skill/preference matching: occupational pattern reflects genuine coordination. If wage penalty unexplained by productivity: occupational segregation is extraction mechanism layered onto participation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(occupational_segregation_extraction, empirical, 'Whether occupational segregation reflects skills/preferences or systematic extraction').

omega_variable(
    state_enforcement_cost_benefit,
    'Does the state''s enforcement of gender-differentiated labor market participation serve genuine state interests (social stability, religious legitimacy, fertility maintenance) or has enforcement become purely theatrical, persisting through institutional inertia?',
    'Analysis of enforcement intensity changes over economic cycles; correlation of enforcement with: (a) fertility rates, (b) social stability indicators, (c) state religious authority legitimacy, (d) economic growth; comparison with enforcement patterns in states with different fundamental interests',
    'If state interests are served: constraint is tangled_rope from state perspective. If enforcement is theatrical: constraint is piton, maintained through inertia despite reduced state benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_cost_benefit, empirical, 'Whether state enforcement serves actual state interests or persists through inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(iranian_women_labor_market_participation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iwlmp_tr_t0, iranian_women_labor_market_participation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(iwlmp_tr_t10, iranian_women_labor_market_participation, theater_ratio, 10, 0.54).
narrative_ontology:measurement(iwlmp_tr_t20, iranian_women_labor_market_participation, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(iwlmp_be_t0, iranian_women_labor_market_participation, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(iwlmp_be_t10, iranian_women_labor_market_participation, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(iwlmp_be_t20, iranian_women_labor_market_participation, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(iranian_women_labor_market_participation, identity_coordination).
narrative_ontology:affects_constraint(iranian_women_labor_market_participation, iranian_household_economic_dependency).
narrative_ontology:affects_constraint(iranian_women_labor_market_participation, middle_east_gender_wage_gap).
narrative_ontology:affects_constraint(iranian_women_labor_market_participation, religious_authority_legitimacy_maintenance).

% DUAL FORMULATION NOTE:
% This constraint is downstream of state religious authority's legitimacy maintenance and household structure regulation (affects_constraints lists dependencies). The upstream constraints define the state's interest in gender role enforcement; this constraint story focuses on the labor market manifestation of that interest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(iranian_women_labor_market_participation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
