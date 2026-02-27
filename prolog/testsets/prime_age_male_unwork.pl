% ============================================================================
% CONSTRAINT STORY: prime_age_male_unwork
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Between 1990 and 2020, roughly 7 million men aged 25-54 exited the U.S.
 *   labor force — roughly 1 in 10 prime-age males are now neither working nor
 *   looking for work. This exodus represents a structural reversal: for
 *   decades, female labor force participation rose while male participation
 *   was stable; now male participation is falling while female participation
 *   plateaus. The 'New Misery' label reflects the concentration of exit among
 *   men without college degrees, in regions dependent on manufacturing, and
 *   with limited family formation prospects. The constraint operates as a
 *   hybrid coordination-extraction system: disability benefits and
 *   reduced-income living arrangements provide coordination infrastructure
 *   (social insurance, extended family support, video-game economy), while
 *   simultaneously extracting through wage suppression of remaining workers,
 *   family formation collapse, and intergenerational occupational knowledge
 *   loss. The exits are not voluntary — most exited males express willingness
 *   to work if suitable jobs existed at acceptable wages — yet the constraint
 *   structures alternatives (disability benefits, family support, gig work)
 *   that, while individually rational, collectively reduce pressure for job
 *   creation or wage increases that would draw them back.
 *
 * KEY AGENTS:
 *   - Prime-age exited males (25-54, no college degree, 7 million individuals): Primary victims (powerless/trapped) — bear full cost of labor market restructuring with no arbitrage options
 *   - Remaining workforce cohort (college-educated, service-sector concentrated): Secondary victims (moderate/constrained) — face wage compression from labor force slack despite continuing participation
 *   - Capital-intensive employers (manufacturing, logistics, retail automation leaders): Primary beneficiaries (institutional/arbitrage) — benefit from reduced labor bargaining power, automation-friendly market conditions
 *   - Regional manufacturing communities (Midwest post-industrial cities, union communities): Organized victims (organized/constrained) — maintain social insurance infrastructure but face tax base collapse and intergenerational skill loss
 *   - Social safety net bureaucracy (SSA, state benefit administrators): Institutional actor (institutional/arbitrage) — manages disability and unemployment transfer systems; benefits from program continuity despite rising rolls
 *   - Analytical observer (policy researcher, civilizational perspective): Risks naturalizing technological unemployment as law rather than policy artifact
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
narrative_ontology:constraint_claim(prime_age_male_unwork, tangled_rope).
narrative_ontology:human_readable(prime_age_male_unwork, "The \"New Misery\" of Prime-Age Male Labor Force Exit").
narrative_ontology:topic_domain(prime_age_male_unwork, "social/economic").

domain_priors:requires_active_enforcement(prime_age_male_unwork).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prime_age_male_unwork, capital_intensive_employers).
narrative_ontology:constraint_beneficiary(prime_age_male_unwork, high_skill_service_sectors).
narrative_ontology:constraint_victim(prime_age_male_unwork, prime_age_male_workers).
narrative_ontology:constraint_victim(prime_age_male_unwork, family_formation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXITED MALE WORKER (SNARE) — Trapped by deskilling, geographic immobility, disability (28% of exited prime-age males report disability), and broken family formation signaling. No arbitrage opportunity; few exit-compatible jobs; social stigma around non-breadwinner status. Bears full cost of labor market restructuring with no escape mechanism. Maximum extraction experienced.
constraint_indexing:constraint_classification(prime_age_male_unwork, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REMAINING WORKFORCE COHORT (TANGLED ROPE) — Constrained by regional wage compression, credential inflation, and employer skill-matching gatekeeping. Experiences both coordination (matching mechanisms, wage standards) and extraction (downward wage pressure from exited workers' non-participation suppressing bargaining power across the board). Mixed: benefits from coordination infrastructure but bears extraction from structural unemployment.
constraint_indexing:constraint_classification(prime_age_male_unwork, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CAPITAL-INTENSIVE EMPLOYERS (ROPE) — Primary beneficiaries. Automation and offshoring reduce demand for prime-age male labor precisely where these workers have concentrated skills. Labor force exit reduces bargaining power of remaining workers, suppresses wage pressure, and enables employer arbitrage across global labor markets. Arbitrage exit means they can shift to alternative labor pools (immigration, automation, outsourcing). Experience constraint as pure coordination: supply adjusts to demand without friction.
constraint_indexing:constraint_classification(prime_age_male_unwork, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL MANUFACTURING COMMUNITIES (TANGLED ROPE) — Organized actors (local governments, unions, civic institutions) see both coordination function (unemployment insurance, disability benefits, community social structures providing substitute income) and extraction (tax base collapse, intergenerational loss of occupational knowledge, social capital decay). Constrained by state-level policy authority and inability to reverse deindustrialization. Active enforcement required to maintain substitution programs.
constraint_indexing:constraint_classification(prime_age_male_unwork, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SOCIAL SAFETY NET BUREAUCRACY (PITON) — Disability Insurance (SSDI) and Supplemental Security Income (SSI) programs function largely as hidden unemployment insurance for prime-age males. Theater ratio (0.55) reflects ritualized medicalization: ~14 million prime-age males on disability, but only ~28% of exited males report work-limiting disability. The disability classification persists through institutional inertia and bureaucratic convenience rather than functional assessment. Theater is lower than pure Piton would suggest because the programs do provide genuine income support; the performative element is the disability justification rather than the transfer itself.
constraint_indexing:constraint_classification(prime_age_male_unwork, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — Risk of naturalizing technological unemployment as immutable natural law: if artificial intelligence and automation reduce demand for prime-age male labor irreversibly, then withdrawal is adaptive rather than extractive. However, the structural data reveals this as false summit: the exit is driven by contingent policy choices (disability benefit availability, immigration levels, credential gatekeeping), employer automation timing, and regional concentration of deindustrialization — not by absolute technological inevitability. The engine will detect this as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(prime_age_male_unwork, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prime_age_male_unwork_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prime_age_male_unwork, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prime_age_male_unwork, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high, increasing over the interval. The labor market restructuring (automation, offshoring, credential inflation) has created structural unemployment in the male-dominated occupations where exited workers concentrated. The extraction element comes from the fact that employer arbitrage options (automation, immigration, outsourcing) reduce pressure to create or maintain jobs that would pay wages sufficient to reintegrate exited males. The initial level (0.32 in 1990) reflects that the exodus was small and partly compensated by continued female entry; the current level (0.58) reflects that large numbers of men experience permanent exit with declining reintegration prospects. Suppression (0.68): High. Barriers to exit-reversal include occupational deskilling (10-15 years out of work), geographic immobility (manufacturing jobs were regionally concentrated), credential requirements that exited workers cannot now meet, family formation collapse (unmarried men have weaker work incentives and fewer household economic pressures), and cultural stigma around older male job-seeking. Disability classification serves as both insurance and barrier — it provides income but signals permanent work limitation, reducing reintegration attempts. Theater ratio (0.55): Moderate-high. The disability benefit system provides genuine income transfer but relies on ritualized medicalization where ~50% of classifications may not reflect work-limiting conditions. The 'disability' label enables income transfer while avoiding the political controversy of explicit unemployment insurance or basic income — the theater is the disability justification rather than the transfer itself. This is lower than pure Piton (theater ≥ 0.70) because the functional element (income support) is substantial, even though the framing (medical disability vs. structural unemployment) is performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (7 million men out of the labor force) classifies as pure extraction (snare) to the trapped worker, mixed coordination-extraction (tangled rope) to the remaining workforce, pure coordination (rope) to the employer, and false-summit naturalization (mountain) to the civilizational observer. The gap arises because the workers' constraints (no jobs at livable wages, health barriers, geographic immobility) are structurally distinct from employers' constraints (labor cost reduction enables other strategies). The workers experience the loss of occupational viability; the employers experience the completion of occupational obsolescence. The safety net provides genuine coordination (prevents destitution) and genuine extraction (benefits structure discourages work attempts).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position within the extraction flow. Exited males have no exit options (trapped) and are victims of the restructuring (high d → high chi). Employers have arbitrage options and benefit from wage slack (low d → low/negative chi). Remaining workers are neither pure beneficiaries nor pure victims — they experience wage pressure (victim element) but continue participation in functioning labor market (beneficiary element). This derives moderate d, producing tangled rope rather than pure snare. Regional communities are organized (higher power atom) but constrained by state-level policy authority (constrained exit) and dependent on federal benefit structure. Disability bureaucracy benefits from program growth and maintains institutional legitimacy through benefit provision (low d as beneficiary). The analytical observer has no structural position within the extraction — observes from outside (analytical power, analytical exit), but risks naturalizing contingent policy as natural law. Directionality overrides are not needed; the structural data maps cleanly to the tuple framework.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CLASSIFICATION RESOLVES MANDATROPHY: The constraint must be Tangled Rope, not pure Snare or pure Rope, because it simultaneously exhibits a genuine coordination function (disability benefits, social insurance, extended family support structures) AND asymmetric extraction (labor market slack suppresses wages for those still participating; employer arbitrage reduces pressure to create jobs). The beneficiaries (capital-intensive employers, benefit bureaucracy) and victims (exited workers, remaining workforce) are structurally distinct. Active enforcement is required — the disability benefit system, reduced work expectations around older men, and family support all require institutional maintenance. If this were pure Snare, there would be no coordination function and exited workers would be in absolute destitution (which has not occurred in the U.S.). If this were pure Rope, both workers and employers would see coordination benefits (neither true — employers benefit from slack, workers bear costs). The classification as Tangled Rope means: the constraint is simultaneously solving a real problem (providing income for structurally unemployed workers) and extracting (preventing labor market pressure that might create jobs, suppressing wages, enabling employer arbitrage). The mandatrophy is resolved by recognizing that both functions are real and structural, not by claiming one is 'really' coordination and the other is 'really' extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disability_classification_threshold,
    'What portion of the 7 million exited prime-age males genuinely cannot work vs. are classified disabled for lack of suitable alternative income?',
    'Longitudinal capacity-to-work assessments independent of benefit status; tracking of return-to-work rates if labor demand shifted; comparison of disability prevalence across countries with different benefit structures',
    'If >60% genuinely work-limited: constraint is Snare (structural unemployment). If <40% work-limited: constraint is primarily extraction mechanism (disability benefits as hidden unemployment insurance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_classification_threshold, empirical, 'Portion of exited males genuinely work-limited vs. classified disabled for income substitution').

omega_variable(
    job_destruction_vs_skill_mismatch,
    'Is the exodus driven primarily by jobs disappearing (structural automation/offshoring) or by jobs existing but requiring credentials/skills the exited population lacks?',
    'Regional job growth analysis controlling for skill requirements; wage levels of available jobs vs. reservation wages; comparison of exited males'' stated barriers (disability, no jobs, low wages, lack skills)',
    'If destruction: Mountain view gains plausibility (jobs don''t exist to exit to). If mismatch: Snare view confirmed (jobs exist but trapped by credential requirements, wage insufficiency, or geographic immobility).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(job_destruction_vs_skill_mismatch, empirical, 'Whether exit driven by job destruction vs. skill/credential mismatch').

omega_variable(
    family_formation_causality,
    'Does labor force exit cause family formation decline, or does prior family formation failure (declining marriage, more single-parent households) make labor exit less costly?',
    'Timing analysis of marriage/cohabitation collapse vs. labor force exit by cohort; comparison of exit rates among married vs. single men; analysis of whether marriage provides reintegration incentive',
    'If exit causes decline: constraint is extraction (labor market removes breadwinner role, collapses family formation). If prior collapse enables exit: constraint is symptomatic (exit reveals pre-existing family destabilization not primarily caused by labor market).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_formation_causality, conceptual, 'Causal direction between labor exit and family formation decline').

omega_variable(
    policy_dependency_lock,
    'Are disability benefits and unemployment insurance creating a poverty trap where exited males cannot earn more than benefits provide without losing coverage, thus incentivizing permanent exit?',
    'Cliff analysis of benefit phase-out vs. market wages; tracking of return-to-work attempts and earnings loss from benefit clawback; comparison of exit rates in states with benefit designs vs. earned income disregards',
    'If lock is severe: constraint includes strong extraction element (policy structure extracts via benefit trap). If lock is minimal: exit reflects genuine absence of suitable work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(policy_dependency_lock, empirical, 'Whether benefit structure creates poverty trap incentivizing permanent exit').


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
narrative_ontology:measurement(pmmu_be_t0, prime_age_male_unwork, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pmmu_be_t15, prime_age_male_unwork, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(pmmu_be_t30, prime_age_male_unwork, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prime_age_male_unwork, resource_allocation).
narrative_ontology:affects_constraint(prime_age_male_unwork, opioid_mortality_deaths_despair).
narrative_ontology:affects_constraint(prime_age_male_unwork, marriage_formation_collapse).
narrative_ontology:affects_constraint(prime_age_male_unwork, credential_inflation_employment_gate).
narrative_ontology:affects_constraint(prime_age_male_unwork, manufacturing_job_offshoring).

% DUAL FORMULATION NOTE:
% The prime-age male unwork constraint is downstream of specific sectoral shocks (manufacturing offshoring, automation wave) but represents a distinct structural constraint on labor market re-entry. The upstream constraints have their own extractiveness values reflecting the timing and sectoral concentration of job destruction; the unwork constraint has its own extractiveness reflecting the long-term institutional lock-in via disability benefits and family structure collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
