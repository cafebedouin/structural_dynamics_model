% ============================================================================
% CONSTRAINT STORY: male_demographic_decline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_male_demographic_decline, []).

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
 *   constraint_id: male_demographic_decline
 *   human_readable: Male Demographic Decline in Post-Industrial Societies
 *   domain: demographic/social/economic
 *
 * SUMMARY:
 *   Male demographic decline in post-industrial societies manifests as
 *   reduced educational attainment, labor market disadvantage, declining
 *   marriage rates, and eroding institutional infrastructure supporting male
 *   social cohesion. This constraint exhibits the signature of a tangled rope
 *   — genuine coordination mechanisms (labor market sorting, credential
 *   distribution, occupational specialization) operate alongside asymmetric
 *   extraction benefiting credentialed women, service economy employers, and
 *   educational institutions. The constraint's complexity lies in
 *   distinguishing between impersonal market realignment (neutral economic
 *   adaptation) and active institutional enforcement of credential inflation
 *   and occupational segregation that suppresses alternative male skill
 *   pathways. Extractiveness has risen from 0.32 (1980s, when
 *   deindustrialization began but male institutional infrastructure remained)
 *   to 0.58 (contemporary), indicating accumulation of extraction atop
 *   coordination mechanisms. Theater ratio (0.48) reflects that the gender
 *   equity institutional apparatus (Title IX, diversity initiatives,
 *   workplace protections) has become increasingly performative — producing
 *   equity rhetoric and compliance metrics while mechanical market processes
 *   continue extracting value from credential inflation.
 *
 * KEY AGENTS:
 *   - Working Class Men: Primary victims (powerless/trapped) — face labor market contraction, credential barriers, and institutional disinvestment with no viable exit
 *   - Moderately-Positioned Men: Secondary victims (moderate/constrained) — navigate credential inflation and labor market segmentation with limited pathways
 *   - Credentialed Professional Women: Primary beneficiaries (powerful/mobile) — gain credential value, labor market access, professional advancement; minimal experienced extraction
 *   - Service Economy Employers: Institutional beneficiary (institutional/arbitrage) — coordinate labor supply through credential requirements; extract value through wage pressure reduction and expanded labor supply
 *   - Educational Institutions: Institutional beneficiary (institutional/arbitrage) — coordinate credential distribution; extract value from credential inflation and expanded female enrollment
 *   - Male Social Cohesion Institutions: Secondary actors (organized/constrained) — unions, fraternal organizations, craft guilds facing suppression through credential inflation and institutional displacement
 *   - Gender Equity Institutional Framework: Institutional actor (institutional/arbitrage) — maintains performative compliance while market mechanisms proceed; arbitrage between equity rhetoric and mechanical extraction
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as economic determinism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(male_demographic_decline, 0.58).
domain_priors:suppression_score(male_demographic_decline, 0.65).
domain_priors:theater_ratio(male_demographic_decline, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(male_demographic_decline, extractiveness, 0.58).
narrative_ontology:constraint_metric(male_demographic_decline, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(male_demographic_decline, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(male_demographic_decline, tangled_rope).
narrative_ontology:human_readable(male_demographic_decline, "Male Demographic Decline in Post-Industrial Societies").
narrative_ontology:topic_domain(male_demographic_decline, "demographic/social/economic").

domain_priors:requires_active_enforcement(male_demographic_decline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(male_demographic_decline, credentialed_professional_women).
narrative_ontology:constraint_beneficiary(male_demographic_decline, service_economy_employers).
narrative_ontology:constraint_beneficiary(male_demographic_decline, educational_institutions).
narrative_ontology:constraint_victim(male_demographic_decline, working_class_men).
narrative_ontology:constraint_victim(male_demographic_decline, male_social_cohesion).
narrative_ontology:constraint_victim(male_demographic_decline, intergenerational_male_transmission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING CLASS MALE (SNARE) — Trapped by deindustrialization, credential inflation, and shifting occupational structure. No viable exit pathway in the constrained labor market. Suppression manifests as declining earning power, reduced marriageability premium, and absence of institutional support for male-typical skill trajectories. Experiences maximum extraction from the constraint: bearing costs of labor market realignment while institutional structures reward alternatives.
constraint_indexing:constraint_classification(male_demographic_decline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MODERATELY-POSITIONED MALE (TANGLED ROPE) — Constrained but not trapped. Faces credential requirements and labor market segmentation but has some pathways through technical training, trades, or lower-tier credentialed work. Benefits from educational institutions (gains access to training) while bearing costs of credential inflation (higher barriers than previous generations faced). Mixed coordination and extraction.
constraint_indexing:constraint_classification(male_demographic_decline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SERVICE ECONOMY EMPLOYERS (ROPE) — Institutional beneficiaries with arbitrage options. Coordinate labor supply through credential requirements and occupational structure (coordination function: matching skills to jobs). Benefit from the demographic decline through reduced wage pressure in service sectors, expanded labor supply (women entering workforce), and reduced need to invest in male-retention infrastructure. Effective extraction is coordinated through market mechanisms that appear neutral.
constraint_indexing:constraint_classification(male_demographic_decline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDUCATIONAL INSTITUTIONS (ROPE) — Coordinate credential distribution and labor market entry through curriculum and degree programs. Benefit from female enrollment growth (expands revenue base, reduces recruitment costs relative to male-targeted outreach). The constraint operates as pure coordination: institutions solve the problem of matching educational supply to labor demand shifts. Effective extraction appears as credential inflation (higher barriers for entry) but is experienced as efficient market adaptation.
constraint_indexing:constraint_classification(male_demographic_decline, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CREDENTIALED PROFESSIONAL WOMEN (ROPE) — Primary beneficiaries with mobile exit options. Gain credential value, labor market access, and professional status from the demographic transition. The constraint operates as coordination for this group: institutional structures that distribute credentials and professional roles have adapted to include this population while marginalizing alternative skill pathways. Experience minimal extraction — the constraint subsidizes their advancement.
constraint_indexing:constraint_classification(male_demographic_decline, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: MALE SOCIAL COHESION INSTITUTIONS (TANGLED ROPE) — Organized agents (unions, fraternal organizations, craft guilds, religious male auxiliaries) face suppression through credential inflation and labor market realignment that erodes their functional basis. These institutions provided intergenerational transmission, mentorship, and social support — they coordinated male community. They simultaneously benefited from and enabled extraction through initiation rites, loyalty demands, and restricted access. Now facing extinction through both benign obsolescence (their functions are being absorbed elsewhere) and active displacement by credential-based sorting. Constrained but organized — capable of partial exit and adaptation.
constraint_indexing:constraint_classification(male_demographic_decline, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: GENDER EQUITY INSTITUTIONAL FRAMEWORK (PITON) — The policy and institutional apparatus explicitly aimed at gender equity (Title IX, affirmative action, diversity initiatives, workplace protections) has become largely performative. Theater ratio: the apparatus produces diversity metrics and compliance documentation while the underlying mechanisms (credential inflation, occupational sorting) proceed mechanically. Institutions maintain the equity framework through inertia despite its diminishing functional connection to actual equity outcomes. Arbitrage exits for powerful institutional actors (claim equity commitment while extracting value from credential inflation).
constraint_indexing:constraint_classification(male_demographic_decline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / ECONOMIC DETERMINISM VIEW (MOUNTAIN) — From a civilizational view, male demographic decline is an irreversible consequence of post-industrial economy structure: manufacturing decline, credential-based labor market sorting, and occupational segregation are structural features that cannot be exited. The constraint appears as natural economic law — the inevitable outcome of comparative advantage in cognitive skills for the contemporary labor market. This perspective risks naturalizing what is actually a contingent institutional arrangement (credential inflation, occupational sorting by gender, absence of infrastructure for male-coded skill transmission).
constraint_indexing:constraint_classification(male_demographic_decline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(male_demographic_decline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(male_demographic_decline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(male_demographic_decline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(male_demographic_decline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(male_demographic_decline, TR),
    TR >= 0.70.

:- end_tests(male_demographic_decline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint exhibits significant extraction from working-class men (wage pressure, credential barriers, institutional disinvestment) benefiting credentialed women and service employers. However, extractiveness is not maximal (0.66+) because substantial portions of the demographic shift represent genuine labor market adaptation (skill-task matching, occupational specialization) rather than pure extraction. The upward trend (0.32 → 0.58 over 20 years) indicates accumulation of extraction atop coordination mechanisms. Suppression (0.65): High. Structural barriers include credential inflation, occupational licensing, male skill transmission infrastructure degradation, and absence of institutional support for male-typical pathways. Working-class men face suppression through credential requirements that appear meritocratic but function as occupational gatekeeping. Suppression operates both structurally (real barriers) and institutionally (educational/policy infrastructure tilted toward credential pathways). Theater ratio (0.48): Moderate. Gender equity institutional apparatus produces metrics and compliance documentation (equity reports, diversity initiatives) with diminishing functional connection to actual inclusion of marginal males. The apparatus is becoming piton-like (theatrical) as its original coordination function (reducing barriers) transitions to enforcement of credential standards that favor credentialed women.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival disagreement: snare from powerless view, rope from beneficiary view, tangled rope from moderate and organized views, piton from institutional view, mountain from civilizational view. This range indicates that the classification is stable but deeply position-dependent. The snare classification (powerless perspective) reflects genuine structural entrapment: no viable exit pathways in contemporary labor market. The rope classification (beneficiary perspective) reflects genuine coordination benefit: credential systems distributing occupational entry. The gap is not measurement error but structural reality — the same constraint is simultaneously a trap and an opportunity depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: power level, exit options, and beneficiary/victim status. Working-class men (powerless/trapped) experience maximal d (near 1.0) — high f(d) producing high experienced extraction. Credentialed women (powerful/mobile) experience minimal d (near 0.1) — low f(d) producing subsidization. Service employers (institutional/arbitrage) experience low d (near 0.2) — institutional arbitrage allows exit. Educational institutions (institutional/arbitrage) experience low d through similar arbitrage logic. Male social cohesion institutions (organized/constrained) experience moderate-high d (0.55-0.65) — organized but constrained by credential system lock-in. The directionality pattern confirms tangled rope: beneficiaries can extract through market mechanisms because losers have no exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that tangled rope is the correct analytical classification: genuine coordination (credential distribution, occupational matching, labor market allocation) operates alongside asymmetric extraction (working-class men bear costs while credentialed women and employers capture benefits). The mandate question is not resolved by showing all six types exist (they do), but by identifying which type captures the structural constraint — and tangled rope does: it requires both coordination function AND victims AND active enforcement. The false mountain classification (civilizational economic determinism) is detected by noting that credential inflation and occupational segregation are socially constructed, not physical laws — the constraint's appearance as immutable derives from institutional lock-in and suppressed alternative pathways, not natural economic law. The piton classification (gender equity apparatus) is correctly identified as theatrical because equity rhetoric has become decoupled from actual resource allocation to marginal males.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    credential_inflation_driver,
    'What proportion of male labor market disadvantage derives from genuine skill mismatch vs. credential inflation and occupational licensing barriers that are socially constructed rather than functionally necessary?',
    'Comparative analysis of wage premiums for specific credentials; examination of task-credential fit across occupations; cross-national variation in credential requirements for similar work',
    'If credential inflation is primary: the constraint is enforcement of artificial scarcity (snare/tangled rope confirmed). If skill mismatch is primary: the constraint is real market adaptation (rope confirmed). Affects whether exit pathways exist through alternative credentialing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_inflation_driver, empirical, 'Proportion of male disadvantage from credential inflation vs. genuine skill mismatch').

omega_variable(
    male_intergenerational_transmission,
    'Is the decline in male-coded skill transmission (mentorship in trades, craft knowledge, occupational culture) a side effect of economic shifts or an active extraction mechanism targeting the institutional basis of male cohesion?',
    'Historical analysis of institutional investment in male skill transmission; comparison of apprenticeship funding, vocational education, trade union membership across time; cross-national variation in male occupational pathway infrastructure',
    'If side effect: constraint is impersonal economic reallocation (less extractive). If active targeting: constraint exhibits deliberate suppression of alternative male infrastructure (more extractive, snare elements strengthen).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(male_intergenerational_transmission, empirical, 'Whether male skill transmission decline is passive or actively suppressed').

omega_variable(
    beneficiary_coalition_stability,
    'Is the coalition of institutional beneficiaries (credentialed women, service employers, educational institutions) stable or contingent on continued credential inflation? Would the constraint persist if credential barriers fell?',
    'Longitudinal analysis of coalition interests; scenario modeling of wage compression if credential barriers relaxed; cross-sectional analysis of institutional support for credential maintenance vs. alternative skill pathways',
    'If stable: constraint has deep structural basis (tangled rope confirmed). If contingent: constraint is brittle and relies on continued enforcement (snare elements, suppression mechanism is critical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_coalition_stability, conceptual, 'Stability of beneficiary coalition under relaxed credential enforcement').

omega_variable(
    performative_equity_mechanism,
    'Does the gender equity institutional framework (Title IX, diversity initiatives) actually function to include marginal males or primarily to document institutional commitment while market mechanisms proceed unconstrained?',
    'Audit of actual resource allocation to male-targeted support programs; comparison of equity rhetoric vs. funding flows; analysis of whether equity policies address credential inflation or reinforce it',
    'If functional inclusion: equity framework is rope (genuine coordination). If performative: framework is piton (theatrical compliance concealing mechanical extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(performative_equity_mechanism, empirical, 'Whether equity framework actually includes marginal males or performs inclusion').

omega_variable(
    reversibility_of_occupational_sorting,
    'Can occupational gender sorting be reversed through institutional intervention or is it locked in by path dependence and network effects in credential systems?',
    'Cross-national comparison of countries attempting credential system reform; analysis of historical precedents where occupational gender ratios shifted; study of credential system network effects and switching costs',
    'If reversible: constraint has policy solutions and is not a mountain (current mountain classification is false summit). If locked in: constraint is nearer to mountain classification but through contingent institutional lock-in, not natural law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_occupational_sorting, empirical, 'Path dependence in occupational gender sorting').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(male_demographic_decline, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdemo_tr_t0, male_demographic_decline, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mdemo_tr_t10, male_demographic_decline, theater_ratio, 10, 0.35).
narrative_ontology:measurement(mdemo_tr_t20, male_demographic_decline, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(mdemo_be_t0, male_demographic_decline, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mdemo_be_t10, male_demographic_decline, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(mdemo_be_t20, male_demographic_decline, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(male_demographic_decline, resource_allocation).
narrative_ontology:affects_constraint(male_demographic_decline, credential_inflation).
narrative_ontology:affects_constraint(male_demographic_decline, occupational_gender_segregation).
narrative_ontology:affects_constraint(male_demographic_decline, male_social_institution_decline).

% DUAL FORMULATION NOTE:
% Male demographic decline is downstream of multiple distinct constraints: credential inflation (ε≈0.55, primary driver), occupational gender segregation (ε≈0.52, secondary reinforcement), and male social institution decline (ε≈0.68, feedback loop). This story models the integrated effect. Upstream constraints have their own extractiveness values reflecting specific mechanisms; this story captures their combined demographic impact.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(male_demographic_decline, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
