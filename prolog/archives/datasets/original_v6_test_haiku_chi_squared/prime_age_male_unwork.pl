% ============================================================================
% CONSTRAINT STORY: prime_age_male_unwork
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prime_age_male_unwork, []).

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
 *   constraint_id: prime_age_male_unwork
 *   human_readable: The "New Misery" of Prime-Age Male Labor Force Exit
 *   domain: social/economic
 *
 * SUMMARY:
 *   Beginning in the 1990s and accelerating through the 2000s and 2010s, the
 *   United States experienced a dramatic decline in labor force participation
 *   among prime-age men (aged 25-54). By 2020, approximately 1 in 10
 *   prime-age men were neither employed nor actively seeking work — the
 *   highest rate since the Great Depression. This constraint — the "New
 *   Misery" — is fundamentally a snare: a structural mechanism that extracts
 *   labor force participation, family stability, and economic agency from a
 *   specific demographic group while concentrating benefits in capital owners
 *   and wage-disciplining employers. The extraction operates through multiple
 *   overlapping mechanisms: wage suppression (real wages for less-educated
 *   men have declined sharply), skill obsolescence (manufacturing job losses
 *   combined with underinvestment in retraining), regional economic collapse
 *   (deindustrialized areas receive minimal reinvestment), family
 *   destabilization (benefits cliffs that punish marriage or secondary
 *   income), disability gatekeeping (SSDI/SSI function as de facto
 *   unemployment programs with theatrical medical justification), and the
 *   opioid epidemic (a symptom and amplifier of deeper labor market
 *   dysfunction). The constraint exhibits different classifications from
 *   different perspectives precisely because the structural data reveals an
 *   asymmetric extraction mechanism dressed up in coordination language
 *   ("labor market flexibility," "natural technological change") and
 *   institutional inertia (degraded social safety net, performative
 *   retraining programs). The suppression (0.68) is particularly high because
 *   workers who exit have no realistic pathway to re-entry: skill development
 *   opportunities are inadequate, wage floors are below subsistence, family
 *   obligations prevent relocation, and disability systems create perverse
 *   incentives against work. The theater ratio (0.55) reflects that many
 *   policy responses (job training programs, disability determinations, labor
 *   market participation campaigns) maintain surface legitimacy while failing
 *   to address underlying wage suppression and regional disinvestment.
 *
 * KEY AGENTS:
 *   - Prime-age males (25-54, less-educated): Primary victim (powerless/trapped) — face wage suppression, skill obsolescence, regional collapse, family instability
 *   - Low-skill male workers: Primary target (powerless/constrained) — especially in manufacturing-dependent regions with high job losses
 *   - Family dependents (spouses, children): Secondary victim (moderate/constrained) — bear costs of lost income, household instability, reduced social mobility
 *   - Capital owners and employers: Primary beneficiary (institutional/arbitrage) — benefit from wage suppression, reduced bargaining power, lower unionization
 *   - Regional employers (surviving firms): Beneficiary (powerful/mobile) — can discipline remaining workforce through threat of exit, access low-wage labor
 *   - Labor unions and worker organizations: Secondary actor (organized/constrained) — fragmented by exit, lose bargaining power, unable to organize exited workers
 *   - Federal safety net (SSDI/SSI): Institutional actor (institutional/constrained) — functions as de facto unemployment program; maintains theater of medical necessity; provides inadequate income
 *   - Community reintegration programs: Organized agents (moderate/mobile) — attempt to coordinate skill development and job matching; capacity-constrained; partial sunset logic
 *   - Analytical observer: Sees full structure (analytical/analytical) — must avoid naturalizing contingent policy choices as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prime_age_male_unwork, 0.58).
domain_priors:suppression_score(prime_age_male_unwork, 0.68).
domain_priors:theater_ratio(prime_age_male_unwork, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prime_age_male_unwork, extractiveness, 0.58).
narrative_ontology:constraint_metric(prime_age_male_unwork, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(prime_age_male_unwork, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prime_age_male_unwork, snare).
narrative_ontology:human_readable(prime_age_male_unwork, "The \"New Misery\" of Prime-Age Male Labor Force Exit").
narrative_ontology:topic_domain(prime_age_male_unwork, "social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prime_age_male_unwork, capital_owners).
narrative_ontology:constraint_beneficiary(prime_age_male_unwork, wage_disciplining_employers).
narrative_ontology:constraint_victim(prime_age_male_unwork, prime_age_males).
narrative_ontology:constraint_victim(prime_age_male_unwork, low_skill_male_workers).
narrative_ontology:constraint_victim(prime_age_male_unwork, family_dependents).
narrative_ontology:constraint_victim(prime_age_male_unwork, labor_market_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXITED PRIME-AGE MALE (SNARE) — Trapped by wage floors set below reservation levels, skill obsolescence, disability (official or undiagnosed), addiction/mental health barriers, and family instability. No meaningful exit options within the formal labor market. Withdrawal is the only available response. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81. High effective extraction: the constraint extracts labor force participation, family stability, and dignity.
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY DEPENDENT (SNARE) — Wife/partner, children, or aging parent dependent on the exited worker's income. Constrained by geography (job loss concentrated in specific regions), caregiving obligations, and secondary earner status. Bears cost of lost income, household instability, and reduced social mobility. d≈0.85, f(d)≈1.25, σ=1.0 → χ≈0.73. Significant extraction through income loss and family dissolution.
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CAPITAL/EMPLOYERS (ROPE) — Experiences labor force exit as coordination mechanism: lower wage pressure, reduced unionization, increased employer monopsony power, greater workforce discipline. Workers who drop out cannot negotiate; remaining workers work harder for lower real wages. Capital flows to labor-scarce sectors or automation. d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Negative effective extraction = net beneficiary. The constraint disciplines labor supply through exit threat.
constraint_indexing:constraint_classification(prime_age_male_unwork, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR UNIONS/WORKER ORGANIZATIONS (TANGLED ROPE) — See coordinating function (solidarity, wage standards) but also extraction mechanism (workforce fragmentation, loss of bargaining power when prime-age men exit). Union density correlates with regions of high male labor force exit. Coordination mechanisms are weakened by exit — the threat of exit is weaponized against remaining workers. d≈0.62, f(d)≈0.95, σ=1.0 → χ≈0.55. Moderate extraction: unions benefit from coordination logic but lose power as members disappear.
constraint_indexing:constraint_classification(prime_age_male_unwork, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL SAFETY NET (PITON) — Provides subsistence but has degraded from income replacement to income supplementation. Theater ratio high: Disability Insurance and Supplemental Security Income function as de facto unemployment programs, yet maintain fiction of medical necessity. Enforcement is ritualistic (periodic reviews, benefit denials and appeals). The SSDI/SSI system persists as primary income support for exited males despite being designed for unable-to-work populations. theater_ratio=0.62 (close to piton threshold). d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.38. Moderate extraction: safety net provides minimal income while extracting labor force participation and agency.
constraint_indexing:constraint_classification(prime_age_male_unwork, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REINTEGRATION PROGRAMS (SCAFFOLD) — Community colleges, trade apprenticeships, peer recovery networks, and local job training see labor force exit as a temporary failure to match workers with available roles. Coordinate around skill development and local employer engagement. Theater low (direct outcome measurement). Many programs have sunset logic: they work themselves out of jobs if successful reintegration occurs. d≈0.35, f(d)≈0.25, σ=0.8 → χ≈0.07. Low effective extraction: genuine coordination benefit with declining need as exit rates improve. has_sunset_clause_rationale: Successful reintegration reduces exited population, which reduces program justification and funding.
constraint_indexing:constraint_classification(prime_age_male_unwork, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, male labor force exit might appear to reflect immutable economic constraints: automation, globalization, skill-biased technological change, and fertility decline are presented as natural limits on employment opportunity. Wage floors are set by 'market clearing' logic. However, the base metrics (ε=0.58, suppression=0.68, theater=0.55) contradict mountain classification — this is a false summit. The constraint is contingent on wage policy choices, enforcement of labor standards, and regional investment decisions, not natural law.
constraint_indexing:constraint_classification(prime_age_male_unwork, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / EXTRACTION VIEW (SNARE) — Comprehensive structural analysis reveals the constraint as extraction mechanism: labor force exit discipline, wage suppression, destruction of bargaining power, and concentration of benefits in capital. The suppression (0.68) is high: skill obsolescence is manufactured through lack of investment in training; wage floors are below subsistence through policy choice; family instability is reinforced through benefits cliffs that punish marriage or part-time work; regional economies collapse through outsourcing and underinvestment. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.64. Significant extraction: the system functions to extract labor participation and concentrate wealth.
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_age_male_unwork_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prime_age_male_unwork, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_age_male_unwork, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prime_age_male_unwork, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prime_age_male_unwork, TR),
    TR >= 0.70.

:- end_tests(prime_age_male_unwork_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts labor force participation, family income, and dignity. Unlike pure unemployment (cyclical, reversible), male labor force exit is structural and persistent. The extraction is not as severe as extreme snares (slavery, debt bondage, ε > 0.75) because some exit is voluntary (workers choosing non-market activity or subsistence); however, the lack of meaningful alternatives (no jobs at living wages, no mobility, no retraining) makes the 'choice' more coerced than free. The upward trajectory (0.35 → 0.58 over 30 years) reflects increasing extraction: as manufacturing jobs disappeared, replacement employment opportunities failed to materialize, wage floors declined in real terms, and regional economies stagnated. Suppression (0.68): High. Multiple constraints prevent exit from exit: (1) Geographic immobility — exited workers are concentrated in deindustrialized regions with no employment alternatives; relocation requires capital and social networks they lack. (2) Wage suppression — replacement jobs (if available) offer wages below subsistence or below reservation level given family obligations. (3) Skill obsolescence — training opportunities inadequate and poorly integrated with employer demand; opportunity cost of training (foregone income) prohibitive. (4) Family obligations — caregiving, debt, and social ties anchor workers to failing regions. (5) Disability gatekeeping — SSDI approval rates create perverse incentive against part-time work or job search (earnings tests, benefits cliffs). (6) Stigma and identity — cultural devaluation of non-working males, loss of status and self-concept. Theater ratio (0.55): Moderate. The constraint maintains considerable theatrical legitimacy through policy narratives and institutional performances: (a) Job training programs proliferate but fail to match skills to actual employer demand or compensate for wage suppression. (b) SSDI determinations invoke medical necessity, but medical justification is theater — many beneficiaries have no disabling condition, yet are trapped by benefits structure. (c) Labor force participation campaigns blame worker motivation rather than job availability or wage floors. (d) Regional economic policy emphasizes 'attracting investment' but delivers minimal results. (e) Opioid crisis is medicalized rather than framed as symptom of labor market dysfunction. Theater is rising (0.38 → 0.55) as the gap between policy claims (solutions exist) and reality (exits persist) widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The exited worker (powerless/trapped) sees a snare: no jobs at living wages, no escape from their region, no family situation stable enough for employment, benefits that punish work. The family dependent (moderate/constrained) sees a snare: lost income, household dissolution, reduced life chances. Capital and employers (institutional/arbitrage) see a rope: labor force exit is coordination mechanism that disciplines remaining workers, reduces wage pressure, eliminates union power, increases monopsony rents. The analytical observer sees a snare with high confidence — the structural data is unambiguous. However, policy institutions (SSDI, retraining programs) see piton (degraded system maintained by inertia) or scaffold (temporary problem solvable by better matching). This gap reveals that the constraint persists precisely because beneficiaries experience it as coordination while victims experience it as extraction. Power asymmetry is the gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Prime-age males: Victim + trapped → d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81. Maximum extraction — no exit options within formal labor market. Family dependents: Victim + constrained → d≈0.85, f(d)≈1.25, σ=1.0 → χ≈0.73. High extraction — can leave (relocation, new marriage) but costs prohibitive. Capital/employers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08, σ=1.0 → χ≈-0.05. Net beneficiary — can exit to automation, offshoring, or capital reallocation. Labor unions: Mixed (coordination benefit, extraction harm) + constrained → d≈0.62, f(d)≈0.95, σ=1.0 → χ≈0.55. Moderate extraction — unions benefit from coordination logic but are weakened by member exit. Safety net institutions: Provides subsistence (beneficiary aspect) but constrains beneficiaries through earnings tests (victim aspect) + constrained → d≈0.50 (symmetric) → f(d)≈0.65, σ=1.0 → χ≈0.38. Moderate extraction. Reintegration programs: Mobile (see exit as solvable) + moderate agents → d≈0.35, f(d)≈0.25, σ=0.8 → χ≈0.07. Low extraction — genuine coordination benefit. Analytical observer: analytical → d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.64. Snare classification is robust.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE-PITON DISTINCTION: The constraint could be misclassified as piton (degraded system) rather than snare (extraction mechanism) if policy makers focus on institutional inertia (SSDI theater, ineffective retraining programs) rather than underlying wage suppression and regional disinvestment. Piton would suggest that better administration (more rigorous disability determinations, better job matching) would solve the problem. Snare reveals the true structure: the system functions to extract labor participation and suppress wages. The theater (SSDI ritual, training programs) is not the constraint; it is a consequence of the constraint. MANDATROPHY RESOLUTION: The snare classification is mandatrophy-robust because (1) Beneficiaries are clearly identified: capital owners, wage-disciplining employers, monopsony-wielding firms. (2) Victims are clearly identified: prime-age males, family dependents, labor organizations. (3) Extraction mechanism is structural (wage suppression, skill obsolescence, regional disinvestment) not relational (trade-off) or performative (theater alone). (4) The constraint persists precisely because beneficiaries perceive coordination benefit while victims perceive extraction. (5) Alternative policies are feasible (higher wage floors, regional investment, family-friendly benefits, aggressive skill development) but actively avoided by benefit-holders. (6) The constraint has strengthened over 30 years as wage floors stagnated and deindustrialization accelerated — this is not natural law (mountain) or inevitable decline (piton) but chosen extraction. The suppression (0.68) and extractiveness (0.58) are consistent with snare, not mountain (would require ε ≤ 0.25) or piton (would require low suppression and theater ≥ 0.70).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_floor_policy_choice,
    'Is the current wage floor (real minimum wage adjusted for inflation) a natural market outcome or a policy choice that could be changed?',
    'Comparative analysis of wage floors across OECD countries and historical wage floors in US; counterfactual modeling of employment effects under higher wage floors; analysis of employer wage-setting behavior in tight vs slack labor markets',
    'If wage floor is policy choice: labor force exit is partly endogenous to wage suppression policy, shifting classification toward snare (extraction). If wage floor is natural market outcome: classification toward piton (degraded system) or mountain (unavoidable). High confidence that policy choice explanation is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_floor_policy_choice, empirical, 'Whether wage floors reflect policy choice or natural market clearing').

omega_variable(
    disability_gatekeeping_effect,
    'Does SSDI/SSI function as intentional unemployment program or as degraded income support system where medical necessity is theater?',
    'Analysis of approval rates over time; comparison of SSDI enrollment to medical evidence of disability; interview studies on program experience; administrative cost analysis (overhead per beneficiary)',
    'If intentional unemployment program: safety net is scaffold with sunset logic (better design possible). If degraded system: piton classification confirmed. High theatrical content suggests piton, but high extraction suggests snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_gatekeeping_effect, empirical, 'Whether SSDI functions as deliberate or degraded income support').

omega_variable(
    regional_investment_reversibility,
    'Are regions of high male labor force exit economically abandoned through structural inevitability (resource depletion, geographic disadvantage) or through policy-driven disinvestment that could be reversed?',
    'Regional development studies comparing deindustrialized regions that recovered vs those that remained stagnant; analysis of public/private investment flows by region; counterfactual modeling of regional development under different policy regimes',
    'If structural/inevitable: constraint approaches mountain. If policy-driven disinvestment: constraint is snare (extraction through underinvestment). Evidence strongly suggests policy-reversibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regional_investment_reversibility, empirical, 'Whether regional decline is structural or policy-driven').

omega_variable(
    skill_obsolescence_investment_gap,
    'Does the gap between worker skills and employer demands reflect natural technological change or deliberate underinvestment in training and education?',
    'Comparative analysis of training investment (public/private) in countries with low male labor force exit; analysis of employer investment in worker development vs wage suppression; econometric studies of training returns and employer contribution rates',
    'If natural technological change: piton or mountain classification. If underinvestment: snare classification (suppression is policy-driven). Evidence suggests significant underinvestment component.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_obsolescence_investment_gap, empirical, 'Whether skill gaps reflect technology or underinvestment').

omega_variable(
    family_instability_causality,
    'Does male labor force exit cause family instability (divorce, single parenthood) or does pre-existing family instability cause exit? What is the causal direction?',
    'Longitudinal analysis with lagged variables; studies of male exit timing relative to family transition events; analysis of exit rates by family structure before exit; intervention studies on family stability and employment',
    'If exit causes instability: snare classification (extraction through family degradation). If instability causes exit: exit is response to family constraint rather than cause. Mixed evidence suggests bidirectional feedback.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_instability_causality, empirical, 'Causal direction between labor force exit and family instability').

omega_variable(
    opioid_crisis_externality,
    'To what extent does opioid crisis (external shock) explain male labor force exit vs. is it symptom of deeper labor market dysfunction?',
    'Timing analysis of opioid availability vs labor force exit onset; regional correlation studies; comparison of exit rates in regions with/without opioid epidemics; analysis of whether opioid use precedes or follows exit',
    'If external shock: exit is piton (degraded response to shock). If symptom of dysfunction: exit is snare (opioid use is adaption to labor market extraction). Evidence suggests opioids amplified pre-existing exit trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opioid_crisis_externality, empirical, 'Whether opioid crisis causes or amplifies labor force exit').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prime_age_male_unwork, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pmmu_tr_t0, prime_age_male_unwork, theater_ratio, 0, 0.38).
narrative_ontology:measurement(pmmu_tr_t15, prime_age_male_unwork, theater_ratio, 15, 0.48).
narrative_ontology:measurement(pmmu_tr_t30, prime_age_male_unwork, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(pmmu_be_t0, prime_age_male_unwork, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pmmu_be_t15, prime_age_male_unwork, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(pmmu_be_t30, prime_age_male_unwork, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prime_age_male_unwork, resource_allocation).
narrative_ontology:affects_constraint(prime_age_male_unwork, wage_floor_insufficiency).
narrative_ontology:affects_constraint(prime_age_male_unwork, regional_economic_collapse).
narrative_ontology:affects_constraint(prime_age_male_unwork, family_stability_benefits_cliff).
narrative_ontology:affects_constraint(prime_age_male_unwork, deindustrialization_automation).

% DUAL FORMULATION NOTE:
% Prime-age male labor force exit is downstream of specific labor market failures: wage suppression (ε≈0.45), regional disinvestment (ε≈0.52), benefits design (ε≈0.35), skill development underinvestment (ε≈0.40). The aggregate constraint (ε=0.58) reflects reinforcing interaction of these upstream mechanisms. Each upstream constraint has its own snare signature; the aggregate is more extractive than any single component because they interact to suppress all exit paths simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(prime_age_male_unwork, organized, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
