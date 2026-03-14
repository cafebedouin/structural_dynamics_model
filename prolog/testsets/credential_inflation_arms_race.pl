% ============================================================================
% CONSTRAINT STORY: credential_inflation_arms_race
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credential_inflation_arms_race, []).

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
 *   constraint_id: credential_inflation_arms_race
 *   human_readable: Credential Inflation Arms Race
 *   domain: labor_market/education/economic
 *
 * SUMMARY:
 *   The credential inflation arms race is a labor market coordination failure
 *   where individual incentives to acquire credentials produce collectively
 *   irrational outcomes. Employers use educational credentials as a screening
 *   mechanism to reduce hiring risk. Prospective workers acquire credentials
 *   to signal productivity and secure employment. But when all workers
 *   credential-inflate simultaneously, the relative value of any given
 *   credential declines, forcing further inflation to maintain
 *   competitiveness. The system exhibits genuine coordination function
 *   (credentials do help match workers to roles) alongside pure extraction
 *   (workers must spend increasingly more time and money in education for the
 *   same real wage outcome). The constraint has strengthened over the past 40
 *   years: jobs that required high school diplomas now require bachelor's
 *   degrees; those requiring bachelor's degrees now require master's degrees.
 *   Theater has increased as credential requirements persist despite
 *   declining signal fidelity — employers demand higher credentials not
 *   because the content is relevant but because 'credential X is what we've
 *   always required for this role.' The theater ratio reflects that much
 *   credentialing is performative signaling rather than functional skill
 *   development.
 *
 * KEY AGENTS:
 *   - Credential Seekers: Primary victims (powerless/trapped) — bear costs of escalating education requirements while experiencing eroding wage premiums
 *   - Educational Institutions: Primary beneficiaries (institutional/arbitrage) — capture tuition, fees, and expanded enrollment from credential inflation
 *   - Employers: Mixed (moderate/constrained) — benefit from credential sorting but forced to continuously raise hiring bars; trapped in coordination problem with other employers
 *   - Premium-Tier Institution Holders: Secondary beneficiary (institutional/arbitrage) — their credentials maintain wage premium longer; can arbitrage against mass-credentialed cohorts
 *   - Labor Market Efficiency: Primary victim (powerless/trapped) — abstract collective good; allocation becomes less efficient as screening mechanism degrades
 *   - Credential Ritual System: Institutional actor (institutional/arbitrage) — performative gatekeeping persists through inertia despite declining function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credential_inflation_arms_race, 0.58).
domain_priors:suppression_score(credential_inflation_arms_race, 0.65).
domain_priors:theater_ratio(credential_inflation_arms_race, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credential_inflation_arms_race, extractiveness, 0.58).
narrative_ontology:constraint_metric(credential_inflation_arms_race, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credential_inflation_arms_race, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credential_inflation_arms_race, tangled_rope).
narrative_ontology:human_readable(credential_inflation_arms_race, "Credential Inflation Arms Race").
narrative_ontology:topic_domain(credential_inflation_arms_race, "labor_market/education/economic").

domain_priors:requires_active_enforcement(credential_inflation_arms_race).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credential_inflation_arms_race, credential_issuers).
narrative_ontology:constraint_beneficiary(credential_inflation_arms_race, premium_institution_holders).
narrative_ontology:constraint_victim(credential_inflation_arms_race, credential_seekers).
narrative_ontology:constraint_victim(credential_inflation_arms_race, labor_market_efficiency).
narrative_ontology:constraint_victim(credential_inflation_arms_race, wage_premiums_erosion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENTRY-LEVEL CREDENTIAL SEEKER (SNARE) — Trapped in escalating credentialing requirements. Jobs that required high school now require bachelor's degree; those requiring bachelor's now require master's degree. Cannot exit without forgoing career prospects. Bears full cost of inflation while the wage premium shrinks. Maximum extraction — the agent cannot avoid the race without economic ruin.
constraint_indexing:constraint_classification(credential_inflation_arms_race, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER WORKER (TANGLED ROPE) — Constrained by retraining barriers and opportunity costs, but also benefits from credential signaling in labor market searches. The system coordinates job-candidate matching (genuine coordination) alongside extracting labor value through credential requirements (asymmetric extraction). Can exit by accepting lower-status roles, but at significant career cost.
constraint_indexing:constraint_classification(credential_inflation_arms_race, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PREMIUM EDUCATIONAL INSTITUTION (ROPE) — Net beneficiary. Experiences the constraint as pure coordination: their credential is a stable signal in a noisy labor market. Can arbitrage between students seeking premium signals and employers seeking filtered candidates. Extraction runs toward this agent. Sees the system as functional.
constraint_indexing:constraint_classification(credential_inflation_arms_race, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMPLOYER (TANGLED ROPE) — Constrained by coordination problem: must use credentials as filter because alternatives (skill testing, apprenticeship) are costly. Benefits from credential sorting (coordination function) but forced to raise hiring bar annually to maintain signal quality. Bears extraction cost through inflation but also coordinates their talent acquisition. Mixed experience.
constraint_indexing:constraint_classification(credential_inflation_arms_race, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDENTIAL-REQUIREMENT RITUAL (PITON) — The credential requirement persists through institutional inertia despite declining signal quality. Employers require Bachelor's degrees for administrative jobs not because the degree content is necessary (much of it isn't) but because 'that's what we've always required.' The theater ratio is high: credential gatekeeping persists as performative ritual even as its filtering function has atrophied from over-credentialing. The system maintains itself through habit, not because it solves the original problem.
constraint_indexing:constraint_classification(credential_inflation_arms_race, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SYSTEMIC INEFFICIENCY (SNARE) — From a generational/global perspective, the credential arms race is a pure negative-sum extraction mechanism: all agents invest more in credentials, but relative positioning doesn't improve — the race continues indefinitely. The analytical view captures the tragic coordination failure: individually rational moves (get more credentials) produce collectively irrational outcomes (everyone credential-inflates, wages decline in real terms, intergenerational mobility stalls). Seen from this height, extraction is maximal because the entire system transfers wealth to credential issuers while producing no genuine productivity gain.
constraint_indexing:constraint_classification(credential_inflation_arms_race, snare,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credential_inflation_arms_race_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credential_inflation_arms_race, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credential_inflation_arms_race, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credential_inflation_arms_race, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credential_inflation_arms_race, TR),
    TR >= 0.70.

:- end_tests(credential_inflation_arms_race_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The credential inflation arms race involves substantial transfer of wealth to educational institutions (tuition increases, expanded enrollment) and delayed entry to productive labor (increased education duration). However, extractiveness is not maximal (snare-level 0.66+) because the coordination function is genuine — credentials do provide meaningful labor market filtering, even if declining in fidelity. The extraction is embedded in a real coordination service. Suppression (0.65): High. Barriers to exit are substantial: credential requirements are enforced by employer hiring practices, carry legal weight (licensing), and are reinforced by social norms. An individual cannot exit the race without significant economic penalty. But suppression is not total because alternatives (trade schools, apprenticeships, skill-based hiring) exist at non-zero cost. Theater ratio (0.68): High. Much credentialing has become performative. Employers require credentials not because course content is directly necessary but because the credential has become a habitual screening proxy. Bachelor's degree requirements for administrative roles where most job tasks could be learned in weeks reflect theater rather than genuine skill requirements. Theater has increased as employers have progressively raised requirements in response to inflation rather than changing hiring practices.
 *
 * PERSPECTIVAL GAP:
 *   The credential seeker sees a snare: they cannot avoid the race without career destruction, and the prize (stable wage premium) keeps shrinking. The employer sees tangled rope: they benefit from credential sorting but are trapped in a coordination problem with other employers, forcing them to continuously raise requirements. The premium institution sees rope: they coordinate talent matching and capture extraction value through tuition. The analytical observer sees snare: from a system level, everyone is running faster just to stay in the same place, and the entire system has become extraction with no coordination residue. The theater ratio (0.68) indicates that much of the credential requirement is now performative — the original coordination function (screening for productivity) has degraded as inflation reduced signal fidelity. The piton perspective captures this degradation: the ritual persists through institutional inertia, not function.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) value for each agent is derived from their structural position relative to extraction flow. Credential seekers are powerless with no exit options (trapped) — they face maximum d (near 1.0), producing high experienced extraction. Educational institutions are powerful beneficiaries with arbitrage options — they face low d (near 0.0), experiencing negative effective extraction (they benefit). Employers are in the middle: they are both victims (forced to accept higher credentialing costs) and beneficiaries (they get better screening). The employer's d value is around 0.50 (symmetric), but they experience the constraint differently than seekers because their alternatives are less costly to implement (they could switch to skill-based hiring). Mid-career workers show moderate extraction because they can partially exit (change fields, accept lower-status roles) but face significant costs. The piton classification derives from the high theater_ratio — the credential requirement persists as performative ritual, not because it solves the screening problem optimally, but because inertia and norm lock keep it in place.
 *
 * MANDATROPHY ANALYSIS:
 *   The credential inflation arms race is a canonical tangled rope that risks misclassification as pure snare. Mandatrophy resolution requires showing that genuine coordination exists alongside asymmetric extraction. COORDINATION FUNCTION: Employers use credentials to reduce hiring search costs and filter for baseline capability. This function is real — employers with credential requirements do reduce hiring risk compared to random selection. Credentials provide honest signals in labor market matching. ASYMMETRIC EXTRACTION: Educational institutions extract tuition and expanded market share. Credential seekers lose time and money to inflation while wage premiums erode. The extraction is real and flows toward credential issuers. The classification is tangled_rope, not snare, because both functions exist. However, as extractiveness increases toward 0.70 and theater_ratio approaches 0.80, the system risks degrading toward piton (ritual without function) or snare (extraction disguised as coordination). The omega variables address this: if wage premiums decline rapidly and alternative screening methods prove viable, the coordination function may collapse, making reclassification to snare appropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_signal_fidelity_decay,
    'At what credential inflation rate does the signal quality of a degree become too degraded to coordinate labor market matching?',
    'Longitudinal analysis of degree-holder productivity by cohort, controlling for selection bias; comparison of wage premiums per unit of degree content retention over time',
    'If signal degradation is already advanced: the entire constraint may reclassify from tangled_rope (coordination + extraction) to pure snare (extraction with no coordination function). If signal still maintains value: constraint remains tangled_rope with viable coordination component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_signal_fidelity_decay, empirical, 'Degree signal quality decay due to inflation').

omega_variable(
    employer_alternative_screening_viability,
    'Can employers realistically implement skill-based hiring (coding tests, work samples, apprenticeships) to replace credential gatekeeping, or are the transaction costs prohibitive?',
    'Cost analysis of alternative screening mechanisms; comparison of hiring cycle times and accuracy between credential-based vs skill-based screening in controlled sectors; diffusion rate of alternative screening methods where available',
    'If alternatives are viable: the trap is cognitive/institutional, not structural (employers ''could'' exit but don''t). If alternatives are genuinely more costly: suppression is higher than measured (employers are trapped too). Classification may shift from snare (seekers trapped) to tangled_rope at employer perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_alternative_screening_viability, empirical, 'Viability of skill-based screening alternatives').

omega_variable(
    intergenerational_credential_requirement_escalation,
    'Is credential inflation rate accelerating, plateauing, or reversing across cohorts and sectors?',
    'Analysis of job posting databases and labor statistics: modal credential requirements by job title/sector tracked annually; comparison across cohorts entering labor market (e.g., high school grads of 1980 vs 2000 vs 2020 — what credentials required for equivalent entry-level positions?)',
    'If accelerating: the trap is deepening, suppression is increasing, and classification trends toward maximal snare. If plateauing: the system may be approaching equilibrium where further inflation is blocked by saturation (negative feedback). If reversing: employer resistance and alternative screening adoption may be starting to work.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_credential_requirement_escalation, empirical, 'Credential inflation rate trajectory').

omega_variable(
    wage_premium_persistence,
    'Are real wage premiums for credential holders declining as predicted by credential inflation theory, or are they stable?',
    'Time-series analysis of wage premiums (Bachelor''s degree vs high school, Master''s vs Bachelor''s) controlling for cohort effects and inflation; sector-specific analysis for roles with historically clear credential gradients',
    'If premiums are declining: extraction is effective and the constraint is working as snare/tangled_rope analysis predicts. If premiums are stable despite inflation: the story may be misframed — the inflation may reflect genuine productivity growth rather than arms race dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_premium_persistence, empirical, 'Wage premium sustainability under credential inflation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credential_inflation_arms_race, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credential_inflation_arms_race, theater_ratio, 0, 0.48).
narrative_ontology:measurement(cred_tr_t5, credential_inflation_arms_race, theater_ratio, 5, 0.58).
narrative_ontology:measurement(cred_tr_t10, credential_inflation_arms_race, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credential_inflation_arms_race, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(cred_be_t5, credential_inflation_arms_race, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cred_be_t10, credential_inflation_arms_race, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credential_inflation_arms_race, information_standard).
narrative_ontology:affects_constraint(credential_inflation_arms_race, student_debt_trap).
narrative_ontology:affects_constraint(credential_inflation_arms_race, intergenerational_mobility_stagnation).
narrative_ontology:affects_constraint(credential_inflation_arms_race, skills_gap_labor_market_mismatch).

% DUAL FORMULATION NOTE:
% The credential inflation arms race is downstream of labor market information asymmetries but independent in its structural dynamics. Related constraints (student debt, mobility stagnation, skills-gap mismatch) are affected by credential inflation but have distinct extractiveness values and verification mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
