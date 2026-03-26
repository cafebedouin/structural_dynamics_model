% ============================================================================
% CONSTRAINT STORY: meritocracy_theater
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meritocracy_theater, []).

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
 *   constraint_id: meritocracy_theater
 *   human_readable: Meritocracy Theater: Performative Selection Masking Structural Extraction
 *   domain: social/institutional/economic
 *
 * SUMMARY:
 *   Meritocracy theater is a structural constraint where selection
 *   institutions (educational systems, hiring processes, credential markets)
 *   use a performative narrative of objective merit-based selection to
 *   legitimize inequality while extracting effort, cost, and compliance from
 *   populations barred from advancement. The constraint operates by shifting
 *   blame from structural exclusion to individual inadequacy: if you did not
 *   advance, you did not try hard enough or lack sufficient merit. This
 *   narrative serves multiple functions simultaneously — it justifies
 *   hierarchy, it motivates continued compliance even from those
 *   systematically excluded, it legitimizes gatekeeper authority, and it
 *   privatizes the costs of selection (credential expenses, unpaid
 *   internships, continuous performance) while concentrating benefits among
 *   those already advantaged. The theater ratio (0.78) reflects that
 *   contemporary merit measurement (standardized tests, GPA, interview
 *   protocols) functions primarily to legitimize gatekeeping decisions rather
 *   than to predict capability — the apparatus persists through institutional
 *   inertia and credentialing industry capture long after research has shown
 *   weak predictive validity. The extractiveness (0.58) reflects that trapped
 *   populations bear enormous costs (credential debt, time spent on
 *   certification cycles, psychological harm from internalized blame) while
 *   gatekeepers capture disproportionate benefits (authority justification,
 *   legitimation dividend, reduced accountability). The suppression (0.65) is
 *   high because alternative status systems and selection mechanisms are
 *   delegitimized or monopolized — a trapped population cannot exit into a
 *   parallel system because meritocracy's narrative dominance prevents
 *   alternatives from being credible.
 *
 * KEY AGENTS:
 *   - Credential-Trapped Aspirants: Primary victims (powerless/trapped) — must perform continuous merit signaling to remain eligible for advancement; bear full cost of credential inflation and performance demands
 *   - Systemically Excluded Populations: Primary victims (moderate/constrained) — face compounding resource and discrimination barriers; experience blame-shifting and narrative denial via meritocratic framing
 *   - Institutional Gatekeepers: Primary beneficiaries (institutional/arbitrage) — capture authority legitimation, reduce accountability, and concentrate power through meritocratic narrative; experience constraint as functional coordination
 *   - Credentialing Industry: Institutional beneficiary (institutional/arbitrage) — profits from credential inflation; maintains and expands merit theater through regulatory capture and narrative investment
 *   - Merit Measurement Apparatus: Institutional actor (institutional/arbitrage) — standardized tests, GPA systems, rankings, certifications; maintains authority through inertia despite degraded validity
 *   - Reform Coalition: Organized agents (organized/constrained) — diversity advocates, policy makers, educational reformers; experience tangled coordination-extraction through visibility and institutional resistance
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing meritocracy as unchangeable law while data reveals it as contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meritocracy_theater, 0.58).
domain_priors:suppression_score(meritocracy_theater, 0.65).
domain_priors:theater_ratio(meritocracy_theater, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meritocracy_theater, extractiveness, 0.58).
narrative_ontology:constraint_metric(meritocracy_theater, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(meritocracy_theater, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meritocracy_theater, snare).
narrative_ontology:human_readable(meritocracy_theater, "Meritocracy Theater: Performative Selection Masking Structural Extraction").
narrative_ontology:topic_domain(meritocracy_theater, "social/institutional/economic").

domain_priors:requires_active_enforcement(meritocracy_theater).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meritocracy_theater, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(meritocracy_theater, incumbent_advantage_holders).
narrative_ontology:constraint_victim(meritocracy_theater, excluded_populations).
narrative_ontology:constraint_victim(meritocracy_theater, credential_debt_bearers).
narrative_ontology:constraint_victim(meritocracy_theater, performative_compliance_exhausted).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREDENTIAL-TRAPPED ASPIRANT (SNARE) — Bears full cost of meritocratic performance demands without exit. Must perform merit continuously (credentials, networking, interview preparation, certification cycles) to stay eligible, while structural barriers (cost of credentials, unpaid internships, social capital requirements) ensure most effort yields no advancement. Experiences maximum extraction with no exit route.
constraint_indexing:constraint_classification(meritocracy_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SYSTEMICALLY EXCLUDED POPULATION (SNARE) — Constrained by resource barriers, discrimination in screening, and credential inflation designed to exclude. The meritocratic narrative denies structural exclusion — 'you didn't try hard enough' — placing moral blame on the excluded while extraction intensifies. Massive suppression: cannot exit into alternative status systems because meritocracy's monopoly on legitimacy prevents alternatives from being credible.
constraint_indexing:constraint_classification(meritocracy_theater, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL GATEKEEPER (ROPE) — Benefits from meritocratic framing as a coordination mechanism: the narrative justifies selection without requiring explicit favoritism, reduces accountability for inequality, and legitimizes power concentration. Gatekeepers experience the constraint as a functional (if performative) allocation system. Low extraction experienced because benefits flow consistently and extraction is hidden under meritocratic rhetoric.
constraint_indexing:constraint_classification(meritocracy_theater, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MERIT MEASUREMENT APPARATUS (PITON) — Standardized tests, credentials, interview protocols, and rankings persist as status markers long after their predictive validity has degraded or been superseded by better measures. These instruments maintain authority through institutional inertia and regulatory capture by the credentialing industry. Theater ratio is extremely high: the apparatus's primary function is now legitimation, not selection. Actual merit prediction has atrophied, but the rituals continue.
constraint_indexing:constraint_classification(meritocracy_theater, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (TANGLED ROPE) — Organized actors (diversity advocates, policy makers, educational reformers) see genuine coordination value in actual merit-based selection while recognizing that current systems extract through performative theater. This perspective is tangled: the coalition benefits from visibility (their critique raises awareness) while bearing costs (institutional resistance, career risk for advocates). Mixed extraction and coordination.
constraint_indexing:constraint_classification(meritocracy_theater, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational view risks naturalizing meritocracy as an inevitable, unchangeable social law: 'hierarchies reflect differences in ability and effort; this cannot be otherwise.' But this mountain claim contradicts the structural data — meritocratic theater is a contingent institutional arrangement maintained by gatekeepers and credentialing industries, not a natural law. The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(meritocracy_theater, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meritocracy_theater_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meritocracy_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meritocracy_theater, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meritocracy_theater, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meritocracy_theater, TR),
    TR >= 0.70.

:- end_tests(meritocracy_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial and increasing. The constraint extracts effort (credential spending, performance anxiety, unpaid internship time), cost (credential debt, testing fees, access requirements), and psychological compliance (internalized blame, motivation despite exclusion) from trapped and excluded populations. Beneficiaries capture legitimation (reduced accountability), authority (authority to select), and status (hierarchy appears justified rather than arbitrary). The 0.58 reflects mixed mechanism: some extraction is pure (cost-shifting to applicants), some is hidden (legitimation value), some is behavioral (continued effort despite low probability of advancement). Suppression (0.65): High. Structural barriers include credential costs, time requirements, unpaid internship prerequisites, social capital requirements, and discrimination in screening. But suppression also operates through narrative dominance — alternative selection mechanisms are delegitimized or eliminated, so trapped populations cannot exit into competing systems. The meritocratic narrative itself suppresses alternatives by claiming they would be 'unfair' or 'inefficient.' Theater ratio (0.78): Very high. Merit measurement apparatus (tests, GPA, interviews, rankings) functions primarily as legitimation rather than selection. Predictive validity research shows weak to moderate correlation between these instruments and actual job performance, yet their use intensifies. They persist because they reduce gatekeepers' accountability ('we used objective criteria') and because credentialing industries profit from their expansion. The theater has increased over the measurement interval as credential requirements have inflated without corresponding performance gains.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival divergence. Trapped aspirants and excluded populations perceive extraction and blame-shifting (Snare). Institutional gatekeepers perceive functional selection with reduced accountability (Rope). The measurement apparatus perceives its own degraded legitimacy but continues performing through inertia (Piton). Reformers perceive mixed coordination-extraction with resistance (Tangled Rope). The false natural law analyst risks seeing immutable social hierarchy as inevitable (Mountain). Each perspective is structurally accurate from that agent's position — the gap is not error but genuinely different experienced constraint types. The analytical observer's mountain is false because meritocratic theater is contingent and could be replaced; the gatekeeper's rope is their actual experience (reduced accountability, functional justification); the snare is the trapped population's actual constraint (no exit, full extraction cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (gatekeepers, credentialing industries) derive their low d values from institutional power and arbitrage exit options — they can leave meritocratic systems and preserve status through other mechanisms (inherited position, network access, lateral moves). Their d values remain low even as suppression increases because their exit is always available. Victims (credential-trapped, excluded populations) derive high d values from trap/constraint exit options — they cannot leave without accepting permanent status loss (credentials required across all institutions, alternative systems delegitimized). The gap between beneficiary d (~0.15, institutional/arbitrage) and victim d (~0.85-0.95, powerless/trapped or moderate/constrained) drives the chi differential: beneficiaries experience low/negative effective extraction while victims experience maximum extraction despite identical base extractiveness value.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the snare classification is not mislabeled as pure coordination. A pure Rope classification would require: (1) low suppression (alternatives available), (2) genuine coordination function (selection actually predicts capability), (3) low asymmetry (costs and benefits distributed). Meritocratic theater fails all three: suppression is high (alternatives delegitimized), coordination function has atrophied (weak predictive validity, theater dominates), and asymmetry is extreme (trapped populations bear costs, gatekeepers capture benefits). The piton perspective correctly identifies that the apparatus is degraded — peer review of credential validity shows weak results, but the rituals persist. The tangled rope perspective on reform reveals real coordination tension: actual merit-based selection would improve organizational function, but the current system serves gatekeeper legitimation more than capability allocation. The snare classification is justified: extraction exceeds coordination function; suppression prevents exit; trapped populations bear unsustainable compliance costs while experiencing narrative blame for structural barriers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merit_definition_malleability,
    'Is ''merit'' defined by gatekeepers post-hoc to match incumbent characteristics, or is there an objective definition that would produce different outcomes?',
    'Longitudinal analysis of merit criteria changes; correlation between definition shifts and demographic outcomes; counterfactual comparison with alternative merit definitions applied retroactively',
    'If merit is malleable and gatekeeper-defined: extraction is pure — meritocracy is theater masking favoritism. If merit is objective: some performance differences are real but structural barriers still create trapped populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(merit_definition_malleability, empirical, 'Whether merit criteria are objectively defined or post-hoc gatekeeper selection').

omega_variable(
    credential_predictive_validity_decay,
    'Do specific credentials (GPA, test scores, degree prestige) predict actual job performance, or does their use persist purely for legitimation and cost-shifting?',
    'Meta-analysis of credential-performance correlation studies; comparison with non-credentialed cohorts; tracking of credential requirements vs actual competency demands in hiring',
    'If validity persists: credentials are coordination function (Rope elements legitimate). If validity has decayed: credentials are pure theater (Snare extraction via cost-shifting and blame-shifting).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_predictive_validity_decay, empirical, 'Credential predictive validity for job performance').

omega_variable(
    alternative_allocation_mechanisms,
    'What alternative selection mechanisms (randomization, apprenticeship, capability-based hiring, portfolio assessment) exist and how do their outcomes compare to credentialism on quality, diversity, and extraction metrics?',
    'Comparative study of organizations using alternative mechanisms; analysis of outcome quality, demographic representation, and structural extraction; pilot programs testing credential reduction',
    'If alternatives are superior or equivalent: meritocratic theater is a choice to maintain extraction. If meritocratic selection is genuinely optimal: constraint may be Rope (legitimate coordination) rather than Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_allocation_mechanisms, empirical, 'Comparative performance of alternative selection mechanisms').

omega_variable(
    suppression_internalization_mechanism,
    'To what extent is the suppression of alternatives structural (credentialism legally/economically enforced) vs internalized (individuals accept meritocratic narrative and self-impose effort/compliance)?',
    'Study of exit trajectories post-constraint removal; analysis of self-limiting beliefs in populations with access to alternative status paths; cross-cultural comparison of internalization rates',
    'If suppression is primarily structural: removing barriers should enable rapid exit. If highly internalized: even with barriers removed, trapped populations may persist in compliance (constraint persists as identity_locked even without material barriers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Structural vs internalized suppression mechanisms').

omega_variable(
    gatekeeper_coordination_necessity,
    'Do institutional gatekeepers solve a genuine coordination problem (selecting capable people) or is the ''selection function'' primarily a legitimation device for maintaining hierarchy?',
    'Analysis of gatekeeper decision-making data; correlation between selection criteria and actual assignment of responsibility/resources; counterfactual: would organization function without gatekeepers?',
    'If coordination function is genuine: constraint has Rope elements. If selection is primarily legitimation: constraint is pure Snare extraction with coordination theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_coordination_necessity, empirical, 'Whether gatekeeping solves coordination or serves legitimation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meritocracy_theater, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(merit_tr_t0, meritocracy_theater, theater_ratio, 0, 0.55).
narrative_ontology:measurement(merit_tr_t10, meritocracy_theater, theater_ratio, 10, 0.68).
narrative_ontology:measurement(merit_tr_t20, meritocracy_theater, theater_ratio, 20, 0.78).

% Extraction over time
narrative_ontology:measurement(merit_be_t0, meritocracy_theater, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(merit_be_t10, meritocracy_theater, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(merit_be_t20, meritocracy_theater, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meritocracy_theater, identity_coordination).
narrative_ontology:affects_constraint(meritocracy_theater, credential_debt_trap).
narrative_ontology:affects_constraint(meritocracy_theater, social_capital_gatekeeping).
narrative_ontology:affects_constraint(meritocracy_theater, diversity_theater).

% DUAL FORMULATION NOTE:
% Meritocracy theater is the overarching constraint coordinating multiple sub-mechanisms (credential inflation, test-based sorting, interview protocols). Downstream constraints include credential debt (the financial extraction mechanism), social capital gatekeeping (the network exclusion mechanism), and diversity theater (the performative reformist narrative that intensifies suppression by framing diversity as merit-based rather than structural corrective). All are linked via the meritocratic narrative: each claims to measure or reward individual merit while extracting from trapped populations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(meritocracy_theater, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
