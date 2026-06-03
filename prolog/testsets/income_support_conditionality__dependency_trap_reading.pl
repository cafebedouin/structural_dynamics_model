% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap (Skill Atrophy Reading)
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the dependency-trap reading of the
 *   income-support-conditionality kernel. The reading holds that
 *   unconditional income support (UBI) undermines work incentives, creating
 *   long-term dependency and skill atrophy. UBI recipients become trapped in
 *   idleness as work incentives decline; skills atrophy from disuse; re-entry
 *   barriers accumulate over time (employer skepticism of employment gaps,
 *   internalized identity shift away from worker role). Simultaneously, the
 *   constraint extracts from taxpayers who fund transfers to non-productive
 *   recipients, and from low-wage workers whose wages are suppressed because
 *   employers can offer lower compensation when workers have UBI subsistence.
 *   This reading focuses on behavioral responses to incentives and the
 *   accumulation of human capital loss over time. It contrasts with the
 *   freedom-floor reading (which emphasizes positive freedom from coercive
 *   labor) and the wage-subsidy reading (which emphasizes the structural
 *   benefit flowing to employers). This is one coherent reading of a
 *   contested empirical and normative claim about what unconditional income
 *   support actually does in the world.
 *
 * KEY AGENTS:
 *   - UBI Recipients Trapped in Idleness: Primary victims (powerless/trapped, generational) — face skill atrophy, internalized identity loss, compounding re-entry barriers. Extractiveness accumulates over time.
 *   - Low-Skill Workers in Labor Market: Secondary victims (moderate/constrained, biographical) — experience both positive freedom (refusal of exploitative work) and negative constraint (wage suppression). Constrained rather than trapped.
 *   - Taxpayers Funding Non-Productive Transfers: Secondary victims (moderate/constrained, generational) — bear fiscal cost when recipients don't return to labor market. Extraction is conditional on long-term non-participation.
 *   - Welfare-Administering State: Primary beneficiary (institutional/arbitrage, immediate) — achieves poverty reduction and social stability with lower administrative cost. Experiences coordination, not extraction.
 *   - Low-Wage Employers: Secondary beneficiary (powerful/mobile, biographical) — benefit from wage suppression enabled by UBI subsistence. This is pure extraction mechanism from their perspective, but the reading treats them as beneficiaries extracting surplus.
 *   - Program Administration: Institutional actor (organized/constrained, generational) — maintains dependency-trap narrative through institutional inertia and political interest in downsizing programs. Theater ratio indicates moderate motivated reasoning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.58).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.65).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap (Skill Atrophy Reading)").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '988a751d-2887-4d66-9424-60d60de1dbd3').
narrative_ontology:cs_kernel_codification('988a751d-2887-4d66-9424-60d60de1dbd3', formalized).
narrative_ontology:cs_authority_grounding('988a751d-2887-4d66-9424-60d60de1dbd3', extraction).
narrative_ontology:cs_reading_relation('988a751d-2887-4d66-9424-60d60de1dbd3', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('988a751d-2887-4d66-9424-60d60de1dbd3', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('988a751d-2887-4d66-9424-60d60de1dbd3', foundational, unconditional_income_reduces_work_incentive_at_margin).
narrative_ontology:cs_axiom_status(unconditional_income_reduces_work_incentive_at_margin, holdable).
narrative_ontology:cs_axiom_grounding('988a751d-2887-4d66-9424-60d60de1dbd3', unconditional_income_reduces_work_incentive_at_margin, empirically_contingent).
narrative_ontology:cs_axiom('988a751d-2887-4d66-9424-60d60de1dbd3', foundational, skill_atrophy_from_non_participation_compounds_over_generations).
narrative_ontology:cs_axiom_status(skill_atrophy_from_non_participation_compounds_over_generations, holdable).
narrative_ontology:cs_axiom_grounding('988a751d-2887-4d66-9424-60d60de1dbd3', skill_atrophy_from_non_participation_compounds_over_generations, empirically_contingent).
narrative_ontology:cs_reference_frame('988a751d-2887-4d66-9424-60d60de1dbd3', labor_market_participation_as_normalcy).
narrative_ontology:cs_drift_state('988a751d-2887-4d66-9424-60d60de1dbd3', contemporary_automation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('988a751d-2887-4d66-9424-60d60de1dbd3', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_administering_state).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, employers_suppressing_wages).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients_trapped_in_idleness).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers_funding_non_productive_transfers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UBI RECIPIENT (SNARE) — Unconditional income removes immediate survival pressure but simultaneously removes work incentives and skill-development pathways. Long-term: cognitive decline, erosion of work discipline, atrophy of labor-market-relevant skills. Exit is costly — returning to work after years outside the labor market requires overcoming employer skepticism, re-entry barriers, and internalized belief that 'I'm not a worker anymore.' Trapped by both external barriers (employer discrimination) and internal identity shift (identity_locked dynamics). Maximum experienced extraction because the agent bears full cost of skill loss and faces compounding exit barriers over time.
constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-SKILL WORKER (TANGLED ROPE) — Experiences UBI as both coordination and extraction. The income floor enables refusal of exploitative work (positive freedom) AND suppresses wages throughout the low-skill sector (extraction). Employers can offer lower wages when workers have UBI subsistence; labor market coordination happens at depressed wage levels. Constrained exit because quitting means losing both wage and UBI (most programs claw back income) — the constraint is real but surmountable with significant cost.
constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE-ADMINISTERING STATE (ROPE) — Experiences UBI as a coordination mechanism that achieves poverty reduction and social stability with minimal bureaucratic overhead compared to means-tested systems. The state benefits from reduced administrative cost and simplified eligibility verification. Extractiveness is minimal because the state's primary interest (population stability) aligns with the program's ostensible function. Arbitrage exit available — can modify program structure, funding level, or withdrawal rules at will.
constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LOW-WAGE EMPLOYERS (SNARE) — Experience UBI as extraction subsidy. The income floor allows them to suppress wages below productivity-justified levels while maintaining worker subsistence. This is pure extraction with minimal coordination function — the constraint exists to transfer employer surplus to the firm. Technically mobile (can relocate business, automate, adjust wages) but in practice sticky within local labor markets and willing to maintain the status quo. For this agent, the constraint is massively beneficial (negative extraction) despite formal snare classification — the label captures the structural mechanism (pure extraction), not the direction.
constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: TAXPAYER (SNARE) — Experiences UBI as extraction when recipients remain outside the labor market. The taxation funds consumption without proportional productive contribution or tax revenue generation. This reading emphasizes the fiscal drain and assumes that skill atrophy prevents future tax recovery. Constrained exit because changing tax structure requires political mobilization; individual taxpayers cannot opt out without penalty. Extraction is significant but not total — some taxpayers benefit from aggregate demand effects if UBI spending circulates productively.
constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PROGRAM ADMINISTRATION (PITON) — The dependency-trap framing persists through institutional inertia and selective evidence interpretation. Early pilot results showing modest employment effects are reified into permanent claims about 'UBI creating dependency,' while contradictory evidence is treated as exceptional or short-term. The theater ratio (0.35) reflects moderate but notable performative content — the 'welfare trap' narrative is politically salient and reinforces means-testing advocacy, but is decoupled from actual longitudinal outcome data. Not quite rope (too much theater, too much motivated reasoning) but not quite snare (the institutional actor has some genuine interest in program stability). Piton classification reflects institutional degradation: the dependency-trap framing is maintained because it serves political interests in downsizing universal programs, not because it accurately summarizes evidence.
constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some behavioral response to income transfers is inherent to human incentive structure: unconditional income always reduces work incentive at the margin — this is an immutable behavioral law. However, the structural data contradicts pure mountain status. The claim naturalizes what is actually a contingent institutional outcome: whether skill atrophy occurs depends on labor market structure (availability of re-entry pathways), macroeconomic conditions (job density, wage floors), program design (asset limits, duration, wage-subsidy interactions), and social mechanisms (stigma, identity, community). The 'natural law' framing conceals these contingencies. Engine false-summit detection will identify this as naturalization of institutional artifacts.
constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(income_support_conditionality__dependency_trap_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, TR),
    TR >= 0.70.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time from 0.35 to 0.58. The constraint exhibits extraction accumulation — the extraction mechanism (skill atrophy, wage suppression, fiscal drain) intensifies as the program persists. Initially, extractiveness is lower (0.35) because some recipients remain employed at UBI-supplemented wages (coordination function). Over time (5-10 year horizon), extractiveness increases as employment rates decline and skill atrophy becomes measurable. This measurement trajectory supports the snare classification — extraction accumulates rather than stabilizing. Suppression (0.65): Moderate-high and stable. The suppression mechanism operates through work-incentive reduction (behavioral) and low-wage employer capacity to suppress wages (structural). Suppression is not total — some recipients remain employed, wage-floor effects exist in tight labor markets — but significantly constrains exit options. Theater ratio (0.35): Low-to-moderate. This reading has substantial empirical content (employment rate tracking, skill measurement, wage trajectory analysis) but also significant narrative and rhetorical elements (the term 'dependency trap' carries moral valence; the causation from UBI to atrophy is contested; the comparison group is often implicit). Theater increases slightly over time (0.28 to 0.35) as the narrative hardens and becomes more resistant to contrary evidence.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits stark perspectival divergence across power levels and time horizons. UBI recipients at powerless/trapped context classify the constraint as snare (maximum extraction, no exit). Low-skill workers at moderate/constrained context classify as tangled rope (mixed coordination and extraction, significant but surmountable costs). The welfare state at institutional/arbitrage context classifies as rope (pure coordination, program works as designed). Low-wage employers at powerful/mobile context classify as snare with negative extraction (pure benefit). The program administration at organized/constrained context classifies as piton (performative maintenance of a degraded framing). The analytical observer risks classifying as mountain (immutable behavioral law). The gap between the UBI recipient's snare and the welfare state's rope represents the core contest: does the program function as intended (rope: reduce poverty, enable dignity) or does it trap recipients in idleness while achieving state goals incidentally (snare: extraction via skill loss and wage suppression)? No single type is correct — the presheaf of perspectives reveals the structure. However, the freedom-floor and wage-subsidy readings would produce different perspectival landscapes entirely, with UBI recipients as beneficiaries (freedom) or victims of wage suppression (wage-subsidy).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from beneficiary/victim declarations and exit options. The dependency-trap reading places UBI recipients in the victim set (trapped, identity-locked pathway into long-term non-participation). This produces high d (~0.85-0.95) for powerless agents with trapped or identity_locked exit. Taxpayers enter the victim set (moderate/constrained), producing moderate d (~0.60-0.70). The welfare state enters beneficiary set (institutional/arbitrage), producing low d (~0.05-0.15). Low-wage employers enter beneficiary set (powerful/mobile), producing low d despite mobile exit (~0.10-0.25) because they benefit structurally from wage suppression. The engine derives d from these declarations and applies the sigmoid f(d) to compute experienced extractiveness χ. The high-d victims experience χ amplified by f(d) ≈ 1.28-1.42 (powerless with trapped exit), while low-d beneficiaries experience χ dampened or negative. The national scope (σ=1.0) does not amplify or dampen relative to baseline. Final χ = 0.58 × f(d) × 1.0 — for a trapped victim, χ ≈ 0.74 (high); for an institutional beneficiary with arbitrage, χ ≈ -0.07 (massively negative). This directionality structure captures why the snare classification emerges: pure extraction mechanism (high ε), targeting specific victims while benefiting institutional actors, with suppression preventing escape.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    work_incentive_elasticity_threshold,
    'At what UBI level does the work incentive reduction cross from negligible to significant, and does this threshold vary by demographic, labor market condition, and job quality?',
    'Randomized controlled trials with varying UBI levels; longitudinal employment tracking across income deciles and skill categories; cross-national comparison of employment rates at equivalent UBI purchasing-power levels',
    'If threshold is high (UBI > 70% of median wage): skill atrophy risk is real and significant. If threshold is low (UBI < 40% of median wage): behavioral effects are marginal and skill atrophy is primarily demographic (age, education) rather than causal. If threshold varies by job quality: the constraint is not a snare (pure extraction) but tangled rope (mixed effects) or even rope (coordination with distributional fairness).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(work_incentive_elasticity_threshold, empirical, 'Behavioral response threshold to unconditional income transfers').

omega_variable(
    skill_atrophy_causation_vs_selection,
    'Do lower employment rates among UBI recipients represent true skill atrophy and motivation loss (causal, this reading''s claim) or selection effects (people with lower productivity or motivation are more likely to accept UBI, pre-existing differences)?',
    'Matched-pair longitudinal studies comparing UBI recipients to non-recipients on skill measures (literacy, numeracy, technical certifications) before and after program enrollment; instrumental variable analysis using program rollout variation; assessment of re-employment wage trajectories (declining wages = atrophy; stable wages = selection).',
    'If atrophy is causal: snare classification confirmed, victims include both recipients (trapped in skill loss) and taxpayers. If selection dominates: the constraint may be rope (efficient matching) or even positive coordination (people better suited to non-market activities are enabled to pursue them). Skill atrophy becomes a measurement artifact of pre-existing heterogeneity, not a structural extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_atrophy_causation_vs_selection, empirical, 'Causal vs. selection explanation for lower employment rates').

omega_variable(
    wage_suppression_empirical_magnitude,
    'How much of the low-wage labor market''s stagnation is attributable to UBI''s existence (employers can suppress wages because UBI provides subsistence), versus macro factors (weak productivity growth, excess labor supply, globalization, union decline)?',
    'Difference-in-differences analysis comparing wage trajectories in regions with vs. without UBI programs, controlling for macroeconomic conditions; employer survey data on wage-setting rationales; historical comparison of wage floors before and after UBI introduction in pilot jurisdictions.',
    'If UBI suppression is large (≥15% wage depression): the constraint is snare (pure extraction benefiting employers). If UBI suppression is negligible (≤3% wage depression): wages are set by structural factors and UBI''s effect is primarily distributional (shifting surplus from taxpayers to recipients, not creating new extraction). If suppression is moderate (5-10%) but recoverable through tight labor markets: the constraint is tangled rope (mixed coordination and extraction), not snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_empirical_magnitude, empirical, 'Empirical magnitude of wage suppression attributable to UBI').

omega_variable(
    reading_contest_kernel_specification,
    'Which reading of the income-support-conditionality kernel is correct? Does unconditional income function primarily as skill-atrophy trap (dependency reading), positive-freedom enabler (freedom reading), or employer subsidy (wage-subsidy reading)?',
    'This is the fundamental reading contest. No single empirical fact resolves it — the three readings emphasize different structural outcomes and have different empirical signatures. Resolution requires value commitments: Which outcome matters most — labor market participation, individual autonomy, or wage floor? Which causal pathway is primary — behavioral response to incentives, liberation from coercive labor, or institutional extraction by employers? Empirical resolution pathway: longitudinal study of countries at different UBI levels (Finland, Kenya, Stockton CA, South Korea) measuring simultaneously (1) employment/skill trajectories, (2) subjective autonomy and choice, (3) wage levels and employer surplus. The sibling readings'' empirical signatures (freedom reading expects employment to remain stable or rise due to better job matching and less desperation; wage-subsidy reading expects wages to stagnate and employer profitability to rise; dependency reading expects long-term employment decline and skill loss) should diverge at program scale > 40% of median wage.',
    'Determines constraint type: dependency reading → snare (extraction). Freedom reading → rope or tangled rope (coordination with positive freedom effects). Wage-subsidy reading → tangled rope (mixed coordination and employer extraction). Also determines victim set and beneficiary set. If freedom reading is correct, UBI recipients are beneficiaries (gaining autonomy) not victims. If wage-subsidy reading is correct, UBI recipients are victims of wage suppression and employers are primary beneficiaries. This reading contest is not resolvable from within the dependency-trap framing — requires cross-position analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_kernel_specification, conceptual, 'Fundamental contest between three readings of the income-support-conditionality kernel').

omega_variable(
    program_design_constraint_variability,
    'Does the snare classification depend on specific UBI program design choices (unconditional, flat-rate, permanent, clawback-free), or is it inherent to any unconditional income support?',
    'Comparative analysis of UBI variants: conditional vs. unconditional, flat vs. means-tested, temporary vs. permanent, clawback vs. no-clawback. Empirical tracking of outcomes across design variants. Historical case studies of program modifications and their effects on employment, skill, wages.',
    'If snare classification depends on design choices: the constraint is contingent and can be remedied through program restructuring (e.g., combining UBI with mandatory training, adding clawback for non-participation, making program temporary with sunset). If inherent: the snare is a fundamental structural feature of any unconditional income support. Design variability enables the scaffold reading (UBI as temporary until labor market conditions improve) or rope reading (UBI combined with job guarantees or training).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_design_constraint_variability, empirical, 'Whether snare classification is inherent or contingent on specific program design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(income_dep_theater_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(income_dep_theater_t5, income_support_conditionality__dependency_trap_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(income_dep_theater_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(income_dep_extractiveness_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(income_dep_extractiveness_t5, income_support_conditionality__dependency_trap_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(income_dep_extractiveness_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(income_dep_suppression_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(income_dep_suppression_t5, income_support_conditionality__dependency_trap_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(income_dep_suppression_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, labor_market_wage_suppression).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, means_testing_surveillance_burden).

% DUAL FORMULATION NOTE:
% The income-support-conditionality kernel decomposes into three structurally distinct constraint stories with different ε values and beneficiary/victim structures. The dependency-trap reading (this file) emphasizes behavioral responses and skill atrophy, yielding ε=0.58 and snare classification. The freedom-floor reading emphasizes positive autonomy and improved labor market matching, yielding lower ε and rope/tangled-rope classification. The wage-subsidy reading emphasizes structural wage suppression mechanisms, yielding ε~0.55 and tangled-rope classification. Each reading instantiates a different causal theory about what UBI actually does. They are not the same constraint measured differently — they are logically incompatible claims about the same institutional phenomenon. Link them through network edges to enable contamination and comparative analysis across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
