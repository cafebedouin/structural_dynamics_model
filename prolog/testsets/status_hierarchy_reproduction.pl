% ============================================================================
% CONSTRAINT STORY: status_hierarchy_reproduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_status_hierarchy_reproduction, []).

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
 *   constraint_id: status_hierarchy_reproduction
 *   human_readable: Status Hierarchy Reproduction Across Generations
 *   domain: social/economic/institutional
 *
 * SUMMARY:
 *   Status hierarchy reproduction is the structural constraint through which
 *   social inequality perpetuates across generations. The constraint exhibits
 *   genuine coordination function (allocating talent to roles, signaling
 *   capability through credentials, creating incentive structures for skill
 *   development) while simultaneously extracting resources from low-status
 *   populations and suppressing alternative status systems. This hybrid
 *   structure makes it a canonical tangled rope: beneficiaries (credentialing
 *   institutions, high-status lineages) experience it as pure coordination;
 *   victims (low-status populations trapped in intergenerational poverty)
 *   experience it as pure extraction; organized moderate agents experience
 *   the mixed form. The theater ratio (0.64) reflects the credential
 *   inflation mechanism: as more candidates pursue credentials, the signal
 *   value degrades while institutional gatekeepers maintain the illusion of
 *   meritocracy through ideology. Over the 50-year interval modeled here,
 *   extractiveness has increased from 0.42 to 0.58 while theater has grown
 *   from 0.48 to 0.64, consistent with credential inflation outpacing
 *   functional information gain.
 *
 * KEY AGENTS:
 *   - High-Status Lineages: Primary beneficiary (institutional/arbitrage) — inherit capital, networks, cultural capital that credentialing institutions recognize. Capture credential premium while bearing minimal cost.
 *   - Credentialing Gatekeepers: Secondary beneficiary (institutional/arbitrage) — universities, professional licensing bodies, certification authorities. Benefit from credential demand and maintain power through scarcity and complexity.
 *   - Low-Status Populations: Primary victim (powerless/trapped) — face inadequate educational resources, absent professional networks, credential cost barriers, and systemic discrimination. Trapped in intergenerational poverty by suppressed alternatives.
 *   - Aspiring Credentialed: Secondary victim (moderate/constrained) — invest in education and credentials to escape low status but face time debt, opportunity costs, and systemic gatekeeping. Experience tangled rope: genuine skill development mixed with extractive taxation.
 *   - Credential-Holding Cohort: Organized actors (organized/constrained) — professionals, white-collar workers who benefit from credential gatekeeping but compete for scarce high-status positions. Organized but suppressed through positional competition.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees full structure: coordination function (talent allocation) + extraction mechanism (generational inequality reproduction) + theatrical maintenance (merit ideology) = tangled rope
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(status_hierarchy_reproduction, 0.58).
domain_priors:suppression_score(status_hierarchy_reproduction, 0.67).
domain_priors:theater_ratio(status_hierarchy_reproduction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(status_hierarchy_reproduction, extractiveness, 0.58).
narrative_ontology:constraint_metric(status_hierarchy_reproduction, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(status_hierarchy_reproduction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(status_hierarchy_reproduction, tangled_rope).
narrative_ontology:human_readable(status_hierarchy_reproduction, "Status Hierarchy Reproduction Across Generations").
narrative_ontology:topic_domain(status_hierarchy_reproduction, "social/economic/institutional").

domain_priors:requires_active_enforcement(status_hierarchy_reproduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(status_hierarchy_reproduction, high_status_lineages).
narrative_ontology:constraint_beneficiary(status_hierarchy_reproduction, credentialing_gatekeepers).
narrative_ontology:constraint_victim(status_hierarchy_reproduction, low_status_populations).
narrative_ontology:constraint_victim(status_hierarchy_reproduction, social_mobility_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-STATUS FAMILY (SNARE) — Trapped in generational poverty with minimal exit mechanisms. Educational resources are inadequate, professional networks are absent, and credential costs are prohibitive. The constraint extracts labor value across generations while suppressing alternative pathways through restricted access to capital, mentorship, and opportunity signals.
constraint_indexing:constraint_classification(status_hierarchy_reproduction, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ASPIRING CREDENTIALED (TANGLED ROPE) — Constrained by credential cost, time debt through education, and systemic gatekeeping. Also benefits from the coordination function: education provides genuine skill development and access to professional networks. Mixed extraction and coordination — the constraint both enables and taxes upward mobility.
constraint_indexing:constraint_classification(status_hierarchy_reproduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIALING INSTITUTION (ROPE) — Benefits from the status hierarchy as a coordination mechanism. Universities, professional licensing bodies, and credential-issuing organizations solve the signal problem: how to allocate talent to roles. The constraint coordinates market information flows. Net beneficiary experiencing low effective extraction — arbitrage exit and institutional power protect from coercion.
constraint_indexing:constraint_classification(status_hierarchy_reproduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDENTIAL-HOLDING COHORT (TANGLED ROPE) — Organized agents (union members, professional associations) experience the hierarchy as both enabling (credential value is protected by gatekeeping) and extractive (competing for scarce status positions). Organized power allows some negotiation leverage but suppression remains significant through credential inflation and positional competition.
constraint_indexing:constraint_classification(status_hierarchy_reproduction, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MERIT IDEOLOGY (PITON) — The institutional narrative that status is earned through merit performs substantive coordination (allocating talent to roles) but increasingly lacks functional content. Credential inflation, legacy admissions, and inherited social capital undermine the merit signal while the ideology persists. Theater ratio ≥ 0.70 reflects the gap between the performance of meritocracy and its actual function. The ideology is maintained through institutional inertia despite degraded signal quality.
constraint_indexing:constraint_classification(status_hierarchy_reproduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, status hierarchies coordinate labor allocation and distribute incentives for skill development (genuine coordination function). However, the constraint also reproduces inequality across generations, suppresses alternative status systems, and extracts resources from low-status populations to maintain credentialing infrastructure. The constraint is genuinely hybrid: both coordination and extraction. Classification differs by observables — different measurement frameworks reveal different aspects.
constraint_indexing:constraint_classification(status_hierarchy_reproduction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(status_hierarchy_reproduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(status_hierarchy_reproduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(status_hierarchy_reproduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(status_hierarchy_reproduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(status_hierarchy_reproduction, TR),
    TR >= 0.70.

:- end_tests(status_hierarchy_reproduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significant value from low-status populations through credential taxation (time, money, opportunity cost), suppressed wage returns despite credential attainment, and legacy capital advantage. However, extraction is not maximal because genuine skill development does occur through education, and some low-status individuals do achieve mobility. The value reflects that extraction is substantial but not totalizing. Suppression (0.67): High. Barriers to exit include inadequate educational resources in low-status communities, geographic isolation from credentialing centers, capital barriers to credential pursuit, social network disadvantage, and discrimination. Additionally, suppression is partly psychological: internalized beliefs that status is meritocratic reduce perceived alternatives. Theater ratio (0.64): Moderate-high. The merit ideology performs real coordination (talent allocation) but increasingly lacks signal fidelity (credential inflation, legacy admissions, inherited social capital). Over the interval, credential inflation (rising requirements for equivalent roles) has increased theater while not substantially improving signal quality. The constraint exhibits Goodhart drift: as credentials become the target rather than the signal, the signal degrades.
 *
 * PERSPECTIVAL GAP:
 *   The gap between high-status and low-status perspectives is maximal: rope vs. snare, net-negative extraction vs. maximum extraction, institutional/arbitrage exit vs. powerless/trapped. A high-status family sees status as earned through education and individual effort (meritocratic frame). A low-status family sees status as inherited and defended through credential gatekeeping (structural frame). Neither perspective is wrong — both are materially true from their position. The gap reveals that the constraint is not naturally agreed-upon (not a mountain) but politically contested (tangled rope). The merit ideology fills the gap, allowing high-status agents to deny extraction while low-status agents experience it acutely. The theatrical quality of merit ideology (theater ratio 0.64) reflects exactly this gap-filling function — the ideology makes the extraction socially tolerable by naturalizing it as meritocratic.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d values are derived from beneficiary/victim status combined with exit options. High-status lineages benefit (low d toward victimhood) and have arbitrage exit (can change systems if exposed to alternatives), producing low d ≈ 0.15, f(d) ≈ -0.01, negative effective extraction. Low-status populations are victims (high d toward victimhood) and are trapped (no exit options), producing high d ≈ 0.95, f(d) ≈ 1.42, maximum experienced extraction. Moderate agents aspiring to credentials are victims (high d) but constrained rather than trapped (exit is possible at cost), producing d ≈ 0.65, f(d) ≈ 1.00, high experienced extraction but below maximum. Organized agents with credentials are partially beneficiaries (compete for scarce status) but also partially victims (face credential inflation and positional competition), producing d ≈ 0.55, f(d) ≈ 0.75, moderate experienced extraction. The scope modifier σ(S) applies: at global scope (σ=1.2), effective extraction is amplified; at local scope, it is dampened. This constraint operates at national-to-global scope across perspectives, so the scope amplification applies across most viewpoints.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that status hierarchy reproduction is genuinely hybrid: it coordinates labor allocation (function) while reproducing inequality (extraction). The constraint cannot be mislabeled as pure coordination (rope) because it suppresses alternatives and extracts from low-status populations. It cannot be mislabeled as pure extraction (snare) because it genuinely develops skills and allocates talent to productive roles. The tangled rope classification prevents both errors simultaneously: it acknowledges the real coordination benefit while flagging the asymmetric extraction. The theater ratio (0.64) and increasing trend (0.48 → 0.64 over 50 years) indicates credential inflation driving Goodhart drift — the target (credential attainment) is being gamed (credential requirements rising) in ways that degrade the signal (returns to credentials falling). This is the classic piton transition pathway: a real coordination mechanism (education) is becoming increasingly theatrical (credential inflation) as function is decoupled from signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_capital_transfer_mechanism,
    'Does the status hierarchy primarily reproduce through capital transfer (structural), cultural transmission (internalized), or both equally?',
    'Comparative analysis of mobility rates across societies with identical capital inequality but different cultural transmission mechanisms (e.g., meritocratic vs. aristocratic ideologies); sibling correlations in status; adoption studies controlling for genetic similarity',
    'If primarily capital transfer: the constraint is structurally contingent and removable through redistribution. If primarily cultural: the constraint is internalized and requires identity-frame disruption to dislodge. If both equally: the constraint is bifurcated — different agents perceive different binding mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_capital_transfer_mechanism, empirical, 'Whether status reproduction is driven by capital transfer or cultural transmission').

omega_variable(
    meritocratic_signal_fidelity,
    'What fraction of variance in adult economic status is actually explained by individual productivity differences vs. inherited position?',
    'Longitudinal data on income mobility within educational cohorts; skill-adjusted wage gap analysis controlling for test scores and IQ; social experiment data on hiring discrimination conditional on credentials',
    'If >70% explained by merit: credential system is functional coordination mechanism. If <40% explained by merit: credential system is primarily theater for status reproduction. If 40-70%: hybrid (tangled rope is correct classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(meritocratic_signal_fidelity, empirical, 'Signal fidelity of credentials for predicting productive capacity').

omega_variable(
    credential_inflation_trajectory,
    'Is credential inflation (rising education requirements for equivalent roles) accelerating, stabilizing, or reversing?',
    'Historical analysis of job requirements for same-role occupations across 30+ year periods; comparison of wage returns to credentials over time; data on time-to-employment for credential cohorts',
    'If accelerating: extraction is increasing (theater rising faster than function). If stabilizing: tangled rope is stable equilibrium. If reversing: alternative status signals are emerging (possible scaffold or rope transition).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(credential_inflation_trajectory, empirical, 'Whether credential inflation is accelerating or stabilizing').

omega_variable(
    suppression_internalization_boundary,
    'Can low-status agents perceive status hierarchy as contingent and changeable, or is the suppression sufficiently internalized that alternatives become unthinkable?',
    'Survey and interview data on aspirational framing; analysis of alternative status systems that emerge in contexts where formal hierarchy is disrupted (utopian communities, online gaming hierarchies, post-revolutionary societies); longitudinal tracking of attitude shift in mobility-experienced individuals',
    'If fully internalized: suppression is psychological (agents are identity_locked) and resistance requires identity reframing. If partially internalized: suppression is structural (agents are constrained) and resistance requires structural change. If minimal internalization: suppression is purely material and resistance requires resource access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Degree of internalization of status hierarchy suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(status_hierarchy_reproduction, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, status_hierarchy_reproduction, theater_ratio, 0, 0.48).
narrative_ontology:measurement(stat_tr_t25, status_hierarchy_reproduction, theater_ratio, 25, 0.56).
narrative_ontology:measurement(stat_tr_t50, status_hierarchy_reproduction, theater_ratio, 50, 0.64).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, status_hierarchy_reproduction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(stat_be_t25, status_hierarchy_reproduction, base_extractiveness, 25, 0.5).
narrative_ontology:measurement(stat_be_t50, status_hierarchy_reproduction, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(status_hierarchy_reproduction, resource_allocation).
narrative_ontology:affects_constraint(status_hierarchy_reproduction, educational_access_stratification).
narrative_ontology:affects_constraint(status_hierarchy_reproduction, credential_inflation).
narrative_ontology:affects_constraint(status_hierarchy_reproduction, wealth_inequality_reproduction).
narrative_ontology:affects_constraint(status_hierarchy_reproduction, intergenerational_poverty).

% DUAL FORMULATION NOTE:
% Status hierarchy reproduction decomposes into three structurally distinct constraints: (1) credential_inflation (ε≈0.45, theater_ratio increasing, Tangled Rope becoming Piton) — the mechanism by which signal degrades; (2) inherited_social_capital (ε≈0.52, suppression≈0.72, Snare from low-status view) — the mechanism by which advantage persists; (3) meritocratic_ideology (ε≈0.35, theater_ratio≈0.70, Piton) — the institutional myth that justifies both. These are linked: the ideology legitimates the capital advantage, credential inflation masks the advantage, and capital advantage reproduces status. This story addresses the aggregate constraint; decomposed stories address specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(status_hierarchy_reproduction, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
