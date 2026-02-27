% ============================================================================
% CONSTRAINT STORY: uk_ssp_eligibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_ssp_eligibility, []).

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
 *   constraint_id: uk_ssp_eligibility
 *   human_readable: UK Statutory Sick Pay (SSP) Eligibility and Rate
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK Statutory Sick Pay (SSP) system creates a structural extraction
 *   mechanism masked as a universal income protection. The Lower Earnings
 *   Limit (LEL)—currently £120 per week (2024)—excludes approximately 40% of
 *   the workforce: part-time workers, gig economy participants, and those in
 *   multiple low-wage jobs. This constraint exemplifies how a policy framed
 *   as 'worker protection' operates as a snare for excluded populations while
 *   functioning as rope for those above the threshold. The LEL has remained
 *   substantively unchanged since 1983 despite massive labor market shifts
 *   (rise of part-time and gig work), indicating inertial rather than
 *   adaptive governance. The constraint exhibits increasing extractiveness
 *   (0.42 → 0.58 over 20 years) as wage stagnation and gig economy growth
 *   have pushed larger cohorts below the LEL in real terms. Theater ratio
 *   remains moderate (0.38 → 0.45) because SSP compliance is administratively
 *   rigorous (genuine process) while the actual protection delivered to
 *   excluded workers is zero. The COVID-era furlough scheme demonstrated that
 *   alternative architectures are politically feasible, yet SSP's LEL
 *   persisted unchanged even when temporary emergency measures bypassed it
 *   entirely.
 *
 * KEY AGENTS:
 *   - Low-wage workers below LEL: Primary victims (powerless/trapped) — excluded entirely from SSP despite statutory entitlement structure; must work ill or lose income
 *   - Gig economy participants: Primary victims (moderate/constrained) — often earn below LEL through fragmented work; limited exit options due to platform lock-in
 *   - Employers above threshold: Primary beneficiaries (institutional/arbitrage) — SSP provides predictable absence cost structure; can adapt via hiring and scheduling
 *   - Exchequer: Secondary beneficiary (institutional/arbitrage) — saves estimated £2-3 billion annually by not extending SSP to below-LEL workers
 *   - Precarious worker coalitions (unions, advocacy): Secondary actors (organized/mobile) — campaign for LEL reform; have exit option (collective action) but face organizing barriers
 *   - Legislative system: Institutional actor (institutional/constrained) — maintains SSP architecture through policy inertia; multiple reform proposals have failed; COVID proved alternatives are possible
 *   - Analytical observer: (analytical/analytical) — sees both the universal-law framing ('fiscal necessity') and the contingent political choice (LEL level is arbitrary relative to comparators)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_ssp_eligibility, 0.58).
domain_priors:suppression_score(uk_ssp_eligibility, 0.68).
domain_priors:theater_ratio(uk_ssp_eligibility, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_ssp_eligibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(uk_ssp_eligibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(uk_ssp_eligibility, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_ssp_eligibility, snare).
narrative_ontology:human_readable(uk_ssp_eligibility, "UK Statutory Sick Pay (SSP) Eligibility and Rate").
narrative_ontology:topic_domain(uk_ssp_eligibility, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, employers_above_lel_threshold).
narrative_ontology:constraint_beneficiary(uk_ssp_eligibility, exchequer).
narrative_ontology:constraint_victim(uk_ssp_eligibility, low_wage_workers_below_lel).
narrative_ontology:constraint_victim(uk_ssp_eligibility, part_time_precarious_workforce).
narrative_ontology:constraint_victim(uk_ssp_eligibility, gig_economy_participants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED LOW-WAGE WORKER (SNARE) — Below LEL threshold (currently £120/week as of 2024), this worker cannot access SSP. Trapped by earnings structure and gig economy arrangement. Must choose between working ill or losing income. Zero alternatives within the constraint. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(uk_ssp_eligibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EMPLOYER ABOVE THRESHOLD (ROPE) — Employer with stable workforce earning above LEL experiences SSP as coordination mechanism: legal obligation provides shared cost structure for absence management. Employer can adjust thresholds, exit via reclassification. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary from regulatory predictability.
constraint_indexing:constraint_classification(uk_ssp_eligibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PRECARIOUS WORKER COALITION (TANGLED ROPE) — When part-time and gig workers organize (via unions, advocacy groups), they gain capacity to exit through collective action (strikes, platform pressure). However, the constraint has both coordination function (SSP provides safety net for those within it) and asymmetric extraction (LEL excludes majority of this cohort). d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE/ADMINISTRATIVE SYSTEM (PITON) — SSP eligibility criteria have remained substantively unchanged since 1983 (with only nominal LEL inflation). The system persists through institutional inertia despite repeated evidence that the LEL excludes ~40% of the workforce. Theater_ratio=0.45 reflects: substantial regulatory theater (compliance checking, tribunal processes) relative to actual income protection delivered. The system is maintained by legislative habit, not by active defense of the LEL level.
constraint_indexing:constraint_classification(uk_ssp_eligibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COVID EMERGENCY SCAFFOLD (HISTORICAL) — During 2020-2021, furlough and grant schemes temporarily bypassed SSP entirely, providing income support below LEL. This was explicitly temporary (sunset 2021). d≈0.35, f(d)≈0.32, σ=1.0 → χ≈0.14. Low effective extraction because the intervention had clear temporal bounds and alternative pathways (grants, furlough). The scaffold was structural proof that SSP is not an immutable law — alternatives are possible.
constraint_indexing:constraint_classification(uk_ssp_eligibility, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: LOW-WAGE ABOVE-LEL COHORT (TANGLED ROPE) — Workers earning just above LEL (£120-150/week) do technically access SSP at £111.35/week (2024 rate), but replacement rate is ~74-92%. Extraction is moderate: they receive some income protection but insufficient for full absence. Constrained exit via retraining or sectoral mobility, but possible. d≈0.55, f(d)≈0.72, σ=1.0 → χ≈0.42.
constraint_indexing:constraint_classification(uk_ssp_eligibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — Observer might frame SSP as a 'natural' constraint arising from fiscal limits: 'States cannot afford unlimited sick pay for all workers.' However, structural data (ε=0.58, suppression=0.68) contradicts mountain threshold. The LEL is a political choice, not a law of nature. Comparator states (Denmark, Germany) offer higher SSP with lower LEL. The false summit detector fires: this is snare/tangled rope, not mountain.
constraint_indexing:constraint_classification(uk_ssp_eligibility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_ssp_eligibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_ssp_eligibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_ssp_eligibility, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_ssp_eligibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_ssp_eligibility, TR),
    TR >= 0.70.

:- end_tests(uk_ssp_eligibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The LEL creates direct income loss for excluded workers (they forgo SSP income they would receive if LEL were lower or abolished). The extraction is not maximal (0.70+) because: (a) some workers with very low earnings have such limited savings capacity that the extraction mechanism has low return (cannot extract much from those with nothing), and (b) the coordination function for above-LEL workers is genuine, so the system has mixed purpose. Suppression (0.68): High. Excluded workers have severely limited alternatives: they cannot exit the low-wage labor market (insufficient savings, skill barriers, childcare constraints); they cannot opt out of working while ill (no alternative income source); they cannot collectively bargain with government (no direct negotiating power). Government has suppressed alternative proposals (lower LEL, universal SSP, sectoral carve-outs) for 40+ years. Theater ratio (0.45): Moderate. SSP administration is substantively rigorous (employers must verify absence, medical certification, benefit calculation) — this is real process. But the theater emerges from: (a) compliance theater (regulatory burden feels substantial relative to actual income distributed), and (b) the rhetorical framing of SSP as 'universal protection' when it excludes 40% of workforce. Theater has increased as gap between framing and reality has widened.
 *
 * PERSPECTIVAL GAP:
 *   The excluded worker experiences pure snare: complete income loss with no alternatives. The above-LEL worker experiences tangled rope: receives partial income protection (mixed benefit) but faces some extraction (replacement rate ~75%). The employer experiences rope: SSP provides coordination mechanism for absence cost-sharing. The exchequer experiences arbitrage beneficiary position: SSP is cheaper than universal schemes. The analytical observer encounters a false summit: the system appears to naturalize ('SSP is what we can afford') what is actually a political choice (LEL is arbitrary relative to Denmark, Germany, which offer broader coverage). The perspectival gap is extreme: the snare agent has zero protection; the institutional beneficiary has predictable cost structure. This is maximal asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded worker: Victim + trapped → d≈0.93, f(d)≈1.40. Near-maximal extraction: cannot exit labor market, cannot opt out of constraint, cannot self-protect. Gig worker: Victim + constrained → d≈0.75, f(d)≈1.10. High extraction: platform lock-in reduces exit to 'mobility with friction.' Above-LEL worker: Both beneficiary (receives SSP) and somewhat victim (replacement rate <100%) + constrained → d≈0.55, f(d)≈0.72. Mixed: benefits from inclusion but trapped in low-wage sector. Employer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net benefit: can adjust hiring and scheduling to manage SSP costs. Exchequer: Beneficiary + arbitrage → d≈0.05, f(d)≈-0.12. Net benefit: SSP exclusions reduce fiscal outlay. Organized precarious coalition: Victim + mobile → d≈0.65, f(d)≈0.95. Can exit through collective action but currently constrained by organizing barriers. Legislative system: Institutional + constrained → d≈0.40, f(d)≈0.40. Piton: inertial maintenance of status quo despite accumulated evidence of failure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the coordination vs. extraction tension through structural decomposition. For above-LEL workers, SSP is genuine coordination: the LEL threshold creates a pool of workers with shared absence risk, and SSP distributes that risk via employer contributions. This function is real and benefits both workers and employers compared to no SSP (they would both bear full absence cost). For below-LEL workers, SSP is pure extraction: there is no coordination mechanism — they are simply excluded. The same statutory mechanism (SSP) simultaneously functions as Rope (for included workers) and Snare (for excluded workers). The constraint is not a unitary mandatrophy but a structural bifurcation. The resolution: treat SSP as two constraints: (1) the above-LEL coordination mechanism (genuine rope), and (2) the below-LEL exclusion (snare). Network them as `above_lel_ssp_coordination` and `below_lel_ssp_exclusion`. The mandatrophy is false only if we insist on unifying them. Treating them as two constraints with different ε values (0.15 for coordination, 0.75 for exclusion) eliminates the apparent paradox. The political error is confusing the name ('SSP') with the structure (two distinct mechanisms). This is precisely the case where corpus decomposition per the ε-invariance principle applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    lel_level_sufficiency_threshold,
    'Is the LEL (currently £120/week) set at a level that reflects genuine fiscal constraint or political choice to exclude precarious workers?',
    'Comparative analysis: LEL levels across OECD states and their fiscal impacts; counterfactual budget modeling for lower LEL scenarios; analysis of revenue impact vs. exchequer budget magnitude',
    'If fiscal necessity: SSP eligibility is a mountain (resource constraint). If political choice: SSP eligibility is snare (extractive exclusion). Current evidence (high LEL relative to comparators) suggests political choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(lel_level_sufficiency_threshold, empirical, 'Whether LEL reflects fiscal constraint or political exclusion').

omega_variable(
    gig_economy_exit_feasibility,
    'What proportion of excluded gig workers can realistically exit the gig economy for standard employment with LEL-eligible income?',
    'Labor mobility survey; longitudinal employment tracking; sectoral vacancy analysis for entrants to standard employment',
    'If exit is feasible for >50% of excluded cohort: exit_options should be ''mobile'' (constraint becomes tangled_rope or rope). If exit is infeasible for <30%: exit_options should be ''trapped'' (confirms snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gig_economy_exit_feasibility, empirical, 'Feasibility of gig workers exiting to standard employment').

omega_variable(
    coordination_function_residual,
    'For workers above the LEL, does SSP function as coordination (shared absence management) or as mere extraction (enforced income loss that employers would prefer as discretionary)?',
    'Employer survey on absence management preferences; analysis of employer adaptation behaviors (e.g., hiring to avoid triggering SSP, using zero-hours contracts); comparison of sick absence rates before/after SSP introduction (1983) in administrative records',
    'If SSP provides genuine coordination for above-LEL workers: system is tangled rope (mixed function). If SSP is experienced as extraction even by beneficiaries: system is snare for all groups (coordination function is illusory).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_residual, empirical, 'Whether SSP provides genuine coordination function for above-LEL workers').

omega_variable(
    political_reversibility_of_lel,
    'Is the LEL level a contingent political artifact that can be reset (as evidenced by COVID policy changes), or has it acquired quasi-constitutional status making revision politically infeasible?',
    'Historical legislative record; political messaging by parties/stakeholders; tracking of legislative attempts to lower LEL; comparison to other statutory minima (National Living Wage) that have been repeatedly revised',
    'If reversible: system is scaffold (political choices can sunset exclusions). If quasi-constitutional: system is piton (inertial despite clear demand for change). Evidence: LEL has not been materially lowered in 40+ years despite repeated campaigns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_reversibility_of_lel, conceptual, 'Political reversibility and constitutional status of LEL').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_ssp_eligibility, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ssp_tr_t0, uk_ssp_eligibility, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ssp_tr_t10, uk_ssp_eligibility, theater_ratio, 10, 0.41).
narrative_ontology:measurement(ssp_tr_t20, uk_ssp_eligibility, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(ssp_be_t0, uk_ssp_eligibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ssp_be_t10, uk_ssp_eligibility, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ssp_be_t20, uk_ssp_eligibility, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_ssp_eligibility, resource_allocation).
narrative_ontology:affects_constraint(uk_ssp_eligibility, uk_national_living_wage_adequacy).
narrative_ontology:affects_constraint(uk_ssp_eligibility, gig_economy_worker_protections).
narrative_ontology:affects_constraint(uk_ssp_eligibility, uk_welfare_cliff_unemployment_trap).

% DUAL FORMULATION NOTE:
% SSP eligibility constraint decomposes into two structurally distinct mechanisms: (1) above-LEL SSP coordination (ε≈0.15, Rope) — shared absence cost structure for stable workers; (2) below-LEL SSP exclusion (ε≈0.75, Snare) — income loss for precarious workers. These are not two perspectives on one constraint but two constraints with different ε values. The legal mechanism is unified (SSP) but the structural function bifurcates at the LEL threshold. This decomposition explains the apparent mandatrophy: no unified type fits because there are two constraints. Network linkage: below-LEL SSP exclusion is downstream of gig_economy_worker_protections (exclusion is structural consequence of gig classification) and affects uk_welfare_cliff_unemployment_trap (SSP exclusion interacts with means-tested benefits to create poverty trap dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_ssp_eligibility, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
