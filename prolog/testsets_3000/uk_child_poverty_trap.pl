% ============================================================================
% CONSTRAINT STORY: uk_child_poverty_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_child_poverty_trap, []).

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
 *   constraint_id: uk_child_poverty_trap
 *   human_readable: UK Child Poverty Trap: Intergenerational Extraction Through Welfare Means-Testing
 *   domain: social_policy/welfare_economics
 *
 * SUMMARY:
 *   The UK child poverty trap emerges from the design of means-tested welfare
 *   provisions introduced in the 1997-2010 period, particularly the phased
 *   replacement of universal child benefit with means-tested tax credits. The
 *   constraint creates a structural lock where children born to low-income
 *   parents face systematic suppression of their parents' economic incentives
 *   to work, earn, and accumulate resources, perpetuating poverty across
 *   generations. The welfare system coordinates genuine provisioning
 *   functions (child protection, nutrition, educational access) alongside an
 *   embedded extraction mechanism: means-testing creates effective marginal
 *   tax rates of 60-73% for low-income working families, eliminating economic
 *   incentive for work intensification or second-earner participation. This
 *   extraction operates at multiple levels simultaneously: immediate (annual
 *   welfare cliff suppresses parental work choice), biographical (parents
 *   locked into benefit dependency), and generational (children born into
 *   poverty have constrained mobility trajectories). The constraint is
 *   structurally engineered rather than natural — comparative analysis shows
 *   alternative welfare designs (universalist child benefit, negative income
 *   tax) achieve provisioning with lower suppression. The theater ratio
 *   reflects the ongoing gap between the policy narrative ('targeted support
 *   for families who need it') and the structural reality (universalism
 *   replaced with means-testing that penalizes work).
 *
 * KEY AGENTS:
 *   - Children in low-income households: Primary victim (powerless/trapped) — bears full cost of suppression through constrained life opportunity set, reduced intergenerational mobility, poverty-correlated health and educational underinvestment
 *   - Working poor parents: Secondary victim (moderate/constrained) — face welfare cliff with effective MTR 60-73%; can exit through accepting welfare loss or moving, but at high cost
 *   - Single parent families: Concentrated victim (moderate/trapped) — welfare cliff hits harder for single earner; second-earner exit option unavailable; higher suppression burden than dual-parent low-income households
 *   - Treasury Department: Primary beneficiary (institutional/arbitrage) — extracts fiscal savings through means-testing design that reduces welfare expenditure below universalist alternatives
 *   - Property owners in affluent areas: Secondary beneficiary (institutional/arbitrage) — benefit from intergenerational poverty concentration preventing low-income housing pressure in high-value areas
 *   - Civil Service welfare administration: Secondary actor (organized/constrained) — operates the coordination mechanism; experiences means-testing as necessary gate to prevent freeriding
 *   - Intergenerational poverty cohort: Distributed victim (powerless/trapped) — at generational timescale, constraint exhibits maximum extraction; escape pathways are suppressed across lifetime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_child_poverty_trap, 0.68).
domain_priors:suppression_score(uk_child_poverty_trap, 0.72).
domain_priors:theater_ratio(uk_child_poverty_trap, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_child_poverty_trap, extractiveness, 0.68).
narrative_ontology:constraint_metric(uk_child_poverty_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(uk_child_poverty_trap, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_child_poverty_trap, snare).
narrative_ontology:human_readable(uk_child_poverty_trap, "UK Child Poverty Trap: Intergenerational Extraction Through Welfare Means-Testing").
narrative_ontology:topic_domain(uk_child_poverty_trap, "social_policy/welfare_economics").

domain_priors:requires_active_enforcement(uk_child_poverty_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_child_poverty_trap, treasury_fiscal_balance).
narrative_ontology:constraint_beneficiary(uk_child_poverty_trap, property_owners_in_affluent_areas).
narrative_ontology:constraint_victim(uk_child_poverty_trap, children_in_low_income_households).
narrative_ontology:constraint_victim(uk_child_poverty_trap, working_poor_parents).
narrative_ontology:constraint_victim(uk_child_poverty_trap, single_parent_families).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHILD IN POVERTY TRAP (SNARE) — Structurally trapped by parental income status. Educational outcomes, health trajectories, and life opportunity set are constrained by mechanisms outside child agency. Cannot exit; bears full cost of extraction through opportunity deprivation. Suppression is near-total: welfare means-testing creates cliff edges where earning marginally more income loses benefits worth 70-80p per pound earned, eliminating all economic incentive for parental work intensification. Alternative exit pathways (social mobility through education) are themselves suppressed by poverty-correlated educational underinvestment.
constraint_indexing:constraint_classification(uk_child_poverty_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING POOR PARENT (TANGLED ROPE) — Faces genuine coordination problem (childcare, work scheduling, transport) alongside extractive means-testing. Welfare system provides real support (child benefit, tax credits, child care support) but embedded in asymmetric extraction: effective marginal tax rates 60-73% eliminate work incentives for second earner or increased hours. Exit is costly but possible: some parents absorb the welfare cliff and enter formal work, or migrate to higher-wage regions. Mixed experience: partial beneficiary from coordination mechanisms + substantial victim of extraction through suppressed income.
constraint_indexing:constraint_classification(uk_child_poverty_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CIVIL SERVICE WELFARE ADMINISTRATION (ROPE) — Experiences the welfare system as a genuine coordination mechanism: universal child benefit, free school meals, educational funding allocation all solve legitimate collective provisioning problems. The means-testing layer is enforcement overhead, not extraction; administrators see it as a necessary gate to prevent freeriding. Suppression appears as administrative necessity, not coercion from this angle. Classification as Rope reflects genuine coordination function despite the extraction mechanisms visible from lower-power perspectives.
constraint_indexing:constraint_classification(uk_child_poverty_trap, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TREASURY DEPARTMENT (TANGLED ROPE) — Coordinates social safety net (genuine function: prevent child destitution, maintain workforce capacity) while extracting fiscal benefits through means-testing design that reduces expenditure below universalist alternatives. The constraint solves a real problem (unsustainable welfare costs in 1990s) but perpetuates extraction as cost control mechanism becomes institutionalized. Effective extraction benefits fiscal balance at cost of suppressing lower-income family economic participation. Treasury experiences the trade-off as justified policy (preventing dependency) rather than extraction.
constraint_indexing:constraint_classification(uk_child_poverty_trap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERGENERATIONAL POVERTY COHORT (SNARE) — At generational timescale, the constraint exhibits maximum extraction and suppression. Children born into low-income households experience constrained educational outcomes, reduced life expectancy, lower intergenerational mobility than UK peers. The suppression mechanism (welfare cliff, educational underinvestment correlated with poverty, neighborhood effects) prevents the escape pathways that generational timescale would normally permit. The constraint operates as a semi-permanent status assignment across generations, not temporary support.
constraint_indexing:constraint_classification(uk_child_poverty_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: UNIVERSAL CHILD BENEFIT INSTITUTIONAL MEMORY (PITON) — The UK abolished universal child benefit in 2013 and replaced it with means-tested child tax credit/universal credit. The institutional narrative retains the ghost of universality ('child benefit for all families') while the actual mechanism is means-tested, income-based, and employment-contingent. The constraint exhibits piton characteristics: the original coordination function (universal provision) is mostly symbolic; the actual mechanism is extractive but justified through the theater of 'targeting support to those who need it.' Theater ratio rises as the legitimating narrative ('we still support all children') drifts from reality (means-testing suppresses parental work).
constraint_indexing:constraint_classification(uk_child_poverty_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE AT CIVILIZATIONAL SCALE) — From a civilizational/universal perspective, the UK child poverty trap is a structurally engineered snare, not an inevitable feature of welfare provision. Comparative analysis (Universal Basic Income pilots, Nordic universalist child support, negative income tax proposals) shows alternative coordination mechanisms with lower suppression. The constraint persists not as a natural limit but as a political choice embedded in means-testing design. The analytical classification reveals the choice architecture: the welfare cliff is engineered, not emergent.
constraint_indexing:constraint_classification(uk_child_poverty_trap, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_child_poverty_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_child_poverty_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_child_poverty_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_child_poverty_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_child_poverty_trap, TR),
    TR >= 0.70.

:- end_tests(uk_child_poverty_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The welfare system extracts substantially from working poor families through means-testing design. The magnitude increased over the 14-year interval as universal child benefit was phased out and replaced with means-tested tax credits (2003-2013), raising extractiveness from 0.48 to 0.68. The extraction is not maximal (0.85+) because some coordination benefits persist: the system still provides child protection and educational access, preventing the most extreme deprivation outcomes. However, the primary function has shifted from coordination (universal provision) toward extraction (fiscal control through benefit suppression). Suppression (0.72): Very high. The means-testing design creates multiple independent suppression mechanisms: (1) welfare cliff — earning above threshold loses benefits worth 70-80p per pound, eliminating work incentive; (2) psychological capture — parents internalize low work expectations and benefit identity; (3) neighborhood effects — poverty concentration prevents opportunity exposure and limits peer effects toward work norms; (4) childcare and transport barriers — working poor face costs that welfare system does not fully offset. Suppression is not total (0.90+) because some families do exit through work intensification, relocation, or educational credential accumulation. Theater ratio (0.58): Moderate-high. The policy narrative frames means-testing as 'targeting support to families who need it most,' implying both universalist concern and efficient allocation. In reality, means-testing is primarily a fiscal control mechanism that suppresses parental work and perpetuates poverty. The theater has risen over the interval (0.35→0.58) as the gap between 'targeted support' rhetoric and 'benefit cliff penalty' reality has widened. This gap represents Goodhart drift: the legitimating metric (targeting efficiency) has become decoupled from the functional metric (family economic participation and child welfare outcomes).
 *
 * PERSPECTIVAL GAP:
 *   The gap between the powerless child's Snare and the Treasury's Rope is maximal and irreducible within single-timeline analysis. The same means-testing mechanism that appears to the Treasury as efficient targeting (Rope: coordination with minimal overhead) appears to the child as pure extraction (Snare: maximum suppression, no exit). The gap cannot be resolved by better measurement — both perspectives are structurally accurate descriptions of the mechanism's real effects. The mandatrophy is resolved by recognizing that the typology does not claim a single true classification but rather a presheaf of legitimate readings across the observation site. The Rope reading requires ignoring the suppression; the Snare reading requires centering it. The actual constraint is best understood through the perspectival ensemble: a Snare from powerless positions, a Tangled Rope from moderate positions, a Rope from institutional positions, a Piton from the institutional memory perspective. The constraint 'is' all six simultaneously because the structural data (beneficiaries, victims, suppression, extraction) determines all six readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the structural relationship to extraction flow. Children (powerless/trapped) have d≈0.95: they are pure targets with no exit and no benefit. Working poor parents (moderate/constrained) have d≈0.70: they bear significant extraction (welfare cliff) but receive some coordination benefit (child support, education funding) and can exit at high cost. The Treasury (institutional/arbitrage) has d≈0.08: they are net beneficiaries, extracting fiscal savings while ostensibly providing coordination. Single-parent families (moderate/trapped) have d≈0.85: they experience higher extraction than dual-parent low-income families because the welfare cliff hits a single earner harder. The directionality computation produces these d values from beneficiary/victim declarations and exit options: children appear in victims array with trapped exit → maximum d → maximum f(d) ≈ 1.42 → maximum χ when scaled by suppression and scope. The Treasury appears in beneficiaries array with arbitrage exit → low d → negative f(d) ≈ -0.12 → negative χ (benefits flow toward them). The analytical observer has d≈0.72: high because the analytical position reveals what lower-power positions cannot see — the structural engineering of the trap — and the observer's capacity to measure and communicate this structure is itself threatened by institutional defensiveness of the policy.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH INSTITUTIONAL CAPTURE: The constraint exhibits high extractiveness (0.68) and very high suppression (0.72), placing it in the snare regime. The mandatrophy is resolved by recognizing that the constraint combines institutional capture with extractive design. The Treasury perspective (institutional/arbitrage) legitimates the snare as necessary and beneficial ('targeting support efficiently'). The child and intergenerational cohort perspectives (powerless/trapped) experience it as pure extraction ('my life choices are determined by a policy cliff I cannot see'). The analytical observer perspective reveals that this is a Snare with institutional beneficiaries successfully narrating extraction as coordination. The resolution does not consist of changing the classification — Snare is correct — but in naming the gap: the constraint's persistence depends on institutional actors experiencing it as Rope while victim populations experience it as Snare. The mandatrophy is fully resolved by this perspectival description: all six types are legitimate readings, and the constraint's structural identity is precisely the Presheaf of these readings, not a single type that measurement could establish. The policy implication: reform requires either (a) changing the mechanism to genuinely coordinate (move toward universal provision and reduce effective MTR), or (b) organizing the victim populations to change the power balance and force their reading to dominate institutional policy (organized Snare → leverage for Rope redesign).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_cliff_behavioral_response,
    'What proportion of suppression derives from welfare cliff design (structural) versus behavioral adaptation to poverty (internalized suppression)?',
    'Longitudinal comparison: work participation rates of parents at identical income levels in high-cliff vs low-cliff welfare regimes; behavioral response following cliff reduction pilots (e.g., enhanced child tax credit phase-out experiments)',
    'If cliff-driven (>60%): suppression is externally imposed and removable by policy redesign. If behavior-driven (>50%): suppression persists even after cliff removal because parents have internalized low work expectations and identity-locked into welfare dependency. Affects classification stability across policy reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_cliff_behavioral_response, empirical, 'Welfare cliff structural vs. behavioral suppression mechanisms').

omega_variable(
    intergenerational_escape_probability,
    'What is the true exit probability for a child born into the bottom income decile — the real alternative to lifetime poverty trap classification?',
    'Intergenerational mobility analysis: cohort-traced earnings and employment outcomes from age 25-50 for children born 1990-2000; comparison to pre-means-testing cohorts and international peer countries',
    'If escape probability >30% over lifetime: constraint is Tangled Rope (mixed coordination + extraction). If <15%: classification as Snare is confirmed. If trending downward: constraint is accumulating and may be reclassified from Tangled Rope to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_escape_probability, empirical, 'True intergenerational escape probability from child poverty trap').

omega_variable(
    policy_intent_extraction_decoupling,
    'Is the suppression a deliberate extractive design choice or an unintended consequence of rational fiscal policy attempting to prevent freeriding?',
    'Policy archive analysis: Cabinet papers, Treasury impact assessments, parliamentary testimony on welfare cliff effects 1995-2010; counterfactual modeling of ''prevention of freeriding'' goal under alternative means-testing designs',
    'If deliberate extraction: constraint is Snare by design. If unintended consequence: constraint is Tangled Rope with catastrophic side effects. Determines whether classification should emphasize intent-based (policy motive) or structural (outcome) analysis. Mandatrophy resolution depends partly on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_extraction_decoupling, conceptual, 'Policy intent vs. unintended extractive consequence in welfare design').

omega_variable(
    neighborhood_effects_independent_suppression,
    'Do neighborhood poverty concentration effects (limited opportunity exposure, peer effects, institutional disinvestment) suppress mobility independently of welfare mechanism design?',
    'Quasi-experimental analysis: children moved from high-poverty to low-poverty neighborhoods via housing mobility program; tracking whether intergenerational mobility improves after move; comparison to peers who remain. Control for welfare cliff exposure.',
    'If neighborhood effects >50% of suppression: constraint has two independent mechanisms and requires bifurcated story (welfare trap + neighborhood constraint family). If <20%: welfare cliff dominates suppression. Affects story decomposition and network relationships.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(neighborhood_effects_independent_suppression, empirical, 'Neighborhood effects independent suppression mechanism').

omega_variable(
    identity_lock_in_poverty_narrative,
    'Do parents trapped in the poverty system develop internalized identity (''I am someone on benefits,'' ''work is not for people like me'') that persists as suppression independent of external barriers?',
    'Ethnographic and psychological assessment: narrative interviews with parents at welfare cliff; identity-fusion measures; post-welfare-reform psychological integration; comparison to parents in non-means-tested contexts',
    'If identity lock >40% of suppression: constraint includes psychological capture and requires omega variable for therapeutic/narrative intervention. If <20%: suppression is primarily structural and removable by policy change alone. Affects interventional design and classification stability after cliff removal.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_poverty_narrative, empirical, 'Identity-lock mechanism in poverty welfare narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_child_poverty_trap, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ukpov_tr_t0, uk_child_poverty_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ukpov_tr_t7, uk_child_poverty_trap, theater_ratio, 7, 0.5).
narrative_ontology:measurement(ukpov_tr_t14, uk_child_poverty_trap, theater_ratio, 14, 0.58).
narrative_ontology:measurement(ukpov_tr_t3, uk_child_poverty_trap, theater_ratio, 3, 0.42).
narrative_ontology:measurement(ukpov_tr_t10, uk_child_poverty_trap, theater_ratio, 10, 0.54).

% Extraction over time
narrative_ontology:measurement(ukpov_be_t0, uk_child_poverty_trap, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ukpov_be_t7, uk_child_poverty_trap, base_extractiveness, 7, 0.62).
narrative_ontology:measurement(ukpov_be_t14, uk_child_poverty_trap, base_extractiveness, 14, 0.68).
narrative_ontology:measurement(ukpov_be_t3, uk_child_poverty_trap, base_extractiveness, 3, 0.54).
narrative_ontology:measurement(ukpov_be_t10, uk_child_poverty_trap, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_child_poverty_trap, resource_allocation).
narrative_ontology:affects_constraint(uk_child_poverty_trap, uk_educational_inequality).
narrative_ontology:affects_constraint(uk_child_poverty_trap, uk_health_inequality_by_region).
narrative_ontology:affects_constraint(uk_child_poverty_trap, uk_housing_affordability_regional).

% DUAL FORMULATION NOTE:
% The child poverty trap is downstream of welfare policy choice but represents a distinct structural constraint with its own extractiveness trajectory. The constraint affects educational inequality (suppressed school investment in poverty areas), health inequality (poverty-correlated health outcomes for children), and housing affordability (intergenerational poverty concentration). Each downstream constraint has its own ε value reflecting specific measurement basis. The poverty trap story treats welfare design as the primary mechanism; downstream stories treat the poverty trap as causal input affecting their respective domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
