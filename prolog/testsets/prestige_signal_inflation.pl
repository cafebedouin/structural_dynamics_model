% ============================================================================
% CONSTRAINT STORY: prestige_signal_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_prestige_signal_inflation, []).

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
 *   constraint_id: prestige_signal_inflation
 *   human_readable: The Credential Red Queen
 *   domain: social/economic/educational
 *
 * SUMMARY:
 *   The Credential Red Queen constraint models the self-reinforcing cycle of
 *   educational credentialization that traps status-seeking actors in a
 *   perpetual arms race. As credentials become more common (high school
 *   diploma → college degree → graduate degree → specialized certifications),
 *   their value as differentiation signals decays. Actors respond by pursuing
 *   ever-higher credentials, but the process repeats — the new level becomes
 *   normalized and its value erodes. The constraint benefits credential
 *   issuers (universities, certification bodies) and early adopters (who
 *   established status when credentials were scarce) while extracting
 *   continuous cost and effort from late entrants and low-income aspirants.
 *   The system exhibits both genuine coordination functions (credentials do
 *   provide reliable signals of conscientiousness and trainability) and pure
 *   extraction mechanisms (credentialization as gatekeeping to maintain
 *   artificial scarcity). Theater ratio (0.68) reflects that much educational
 *   effort is performative — degree completion as credential acquisition
 *   rather than skill development — especially for credentials that no longer
 *   track job requirements. The alternative credentialing movement
 *   (apprenticeships, bootcamps, skill-based hiring) represents a structural
 *   challenge to the traditional system with a real sunset mechanism: as
 *   alternative credentials gain employer recognition, the artificial
 *   requirement for traditional degrees can be eliminated. The constraint
 *   demonstrates mandatrophy resolution: it is simultaneously a coordination
 *   mechanism (providing reliable signals) and an extraction mechanism
 *   (preventing mobility through continuous cost escalation). The
 *   classification depends entirely on the observer's structural position.
 *
 * KEY AGENTS:
 *   - Perpetual Credential Seeker (powerless/trapped): Low-income, low-education entry-level workers facing mandatory credential requirements without sufficient capital to acquire them efficiently
 *   - Middle-Class Aspirant (moderate/constrained): Individuals with access to some education financing, seeking credentials to enter or maintain professional status, experiencing both opportunity and extraction
 *   - Credential Issuer (institutional/arbitrage): Universities, certification bodies, and training programs that benefit from increasing credential demand; experience constraint as pure coordination mechanism
 *   - Employer Coordination Coalition (organized/constrained): HR departments and industry standards bodies that maintain credential requirements for hiring efficiency but are trapped in credential arms races
 *   - Educational Theater System (institutional/arbitrage): Formal education institutions maintaining credentialization despite functional decay; sees its own role as increasingly performative
 *   - Alternative Credentialing Movement (organized/mobile): Bootcamps, apprenticeships, skill-based hiring advocates building parallel verification systems with explicit sunset mechanism
 *   - Analytical Observer (analytical/analytical): Civilizational perspective risking naturalization of credential inflation as inevitable rather than as contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(prestige_signal_inflation, 0.58).
domain_priors:suppression_score(prestige_signal_inflation, 0.65).
domain_priors:theater_ratio(prestige_signal_inflation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(prestige_signal_inflation, extractiveness, 0.58).
narrative_ontology:constraint_metric(prestige_signal_inflation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(prestige_signal_inflation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(prestige_signal_inflation, tangled_rope).
narrative_ontology:human_readable(prestige_signal_inflation, "The Credential Red Queen").
narrative_ontology:topic_domain(prestige_signal_inflation, "social/economic/educational").

domain_priors:requires_active_enforcement(prestige_signal_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(prestige_signal_inflation, credential_issuers).
narrative_ontology:constraint_beneficiary(prestige_signal_inflation, early_adopters_and_gatekeepers).
narrative_ontology:constraint_victim(prestige_signal_inflation, late_entrants_and_majority_seekers).
narrative_ontology:constraint_victim(prestige_signal_inflation, economic_mobility_aspirants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERPETUAL CREDENTIAL SEEKER (SNARE) — Entry-level workers cannot exit the credential treadmill. Each credential that becomes normative is replaced by higher requirements (bachelor's degree → master's degree → specialized certificates). No exit option exists; the constraint extracts continuous effort and cost without convergence to actual sufficiency. Maximum experienced extraction — trapped in an infinite game where the finish line recedes.
constraint_indexing:constraint_classification(prestige_signal_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS ASPIRANT (TANGLED ROPE) — Constrained by education costs and time investment, but also benefits from credential signaling as a mechanism to differentiate themselves from those without credentials. The system both enables aspiration (credentials do correlate with opportunity) and extracts (endless qualification requirements). Moderate extraction because some agency exists — can choose which credentials to pursue, when to stop — but exit remains costly.
constraint_indexing:constraint_classification(prestige_signal_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIAL ISSUER (ROPE) — Experiences the constraint as a pure coordination mechanism. Credential inflation solves a real signaling problem: employers need a way to filter applicants, and credentials provide that filter. The issuer benefits from increasing credential demand without perceiving extraction — their role is genuinely coordinating information flow. Arbitrage exit: can issue new credential types when old ones inflate.
constraint_indexing:constraint_classification(prestige_signal_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EMPLOYER COORDINATION COALITION (TANGLED ROPE) — Organized actors (HR departments, industry standards bodies) actively maintain credential requirements as hiring filters, but are also trapped by their own enforcement: if one employer raises requirements, others must follow or lose talent arbitrage. Benefits from coordination (shared screening mechanism) but also extracted from by credential arms races (must constantly update hiring thresholds). Constrained exit because unilateral credential reduction signals weakness to competitors.
constraint_indexing:constraint_classification(prestige_signal_inflation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EDUCATIONAL THEATER SYSTEM (PITON) — Formal education for many positions has degraded into performative credential accumulation disconnected from job-specific skills. The constraint persists through institutional inertia: employers continue requiring degrees because credentials serve as a rough-cut signal, but the functional content of many degree programs no longer tracks actual job requirements. Theater ratio (0.68) reflects that much educational effort is theatrical — student effort expended on degree completion rather than relevant skill development. The system maintains itself through path dependency and coordination failure, not genuine function.
constraint_indexing:constraint_classification(prestige_signal_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ALTERNATIVE CREDENTIALING MOVEMENT (SCAFFOLD) — Organized actors (apprenticeships, bootcamps, skill-based hiring initiatives, portfolio-based assessment) are building alternative verification pathways with an explicit sunset: as alternative credentials (AWS certifications, GitHub portfolios, bootcamp completion) gain employer recognition, the traditional degree requirement can be deprecated. Low extraction because the coalition has agency and sees a concrete exit mechanism. Sunset clause: estimated 10-15 years as skill-based hiring becomes industry norm.
constraint_indexing:constraint_classification(prestige_signal_inflation, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SIGNALING THEORY VIEW (MOUNTAIN) — From a civilizational perspective, credential inflation is an inevitable consequence of Spence's signaling economics: in conditions of information asymmetry, actors competing for opportunity cannot unilaterally reduce signaling effort without losing competitive position, creating a coordination trap with no individual exit. This appears as an immutable law of competitive signaling. However, the structural data contradicts the mountain classification — the constraint exhibits beneficiaries, victims, and enforcement, revealing it as a Tangled Rope, not a natural law. The mountain classification naturalizes what is actually a contingent institutional arrangement around credential verification.
constraint_indexing:constraint_classification(prestige_signal_inflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(prestige_signal_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(prestige_signal_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(prestige_signal_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(prestige_signal_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(prestige_signal_inflation, TR),
    TR >= 0.70.

:- end_tests(prestige_signal_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts substantial cost from late entrants through credential requirement escalation, but the extraction is not total because credentials do provide genuine signaling value — some of the cost reflects real information asymmetry problem-solving. The value reflects measurement of credential devaluation (continuous requirement escalation) balanced against genuine signal content (credentials do correlate with job success, though the correlation has weakened over the interval). The upward trajectory from 0.35 to 0.58 over 30 years reflects that educational theater has increased: early credentials retained more signal content; modern credentials are more theatrical. Suppression (0.65): High. Actors cannot easily exit the credential treadmill — switching to non-credentialized pathways carries severe labor market penalties. Traditional employers maintain degree requirements as hiring filters even when specific degree content is irrelevant. Career penalties for credential gaps are substantial, creating near-total suppression of alternative pathways. But suppression is not absolute: alternative credentials are emerging, some employers are beginning to relax requirements, and non-traditional pathways do exist (though at reduced opportunity). Theater ratio (0.68): High. Reflects that modern degree programs increasingly function as credential acquisition mechanisms rather than skill development. Students optimize for degree completion rather than learning; institutions optimize for credential issuance rather than competency verification. The high theater indicates that functional content is degraded — the constraint persists partly through institutional inertia and path dependency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the maximum perspectival gap: beneficiaries and victims have nearly opposite experiences of identical structural conditions. Credential issuers see pure coordination — solving a real signaling problem. Perpetual credential seekers see pure extraction — trapped in requirement escalation. The middle-class aspirant sees both (Tangled Rope). Employers see coordination through a self-reinforcing requirement cycle (constrained by competitive credential arms races). The analytical observer risks seeing an immutable law of signaling (Mountain). The gap exists because the constraint operates on information asymmetry while simultaneously using institutionalized gatekeeping. The coordination function (filtering by signal) is real. The extraction function (artificial requirement escalation) is equally real. They are the same constraint viewed from different structural positions. The credentialism that appears as necessary coordination to the employer appears as impossible gatekeeping to the low-income seeker.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. Perpetual credential seekers (powerless/trapped) derive high d → high f(d) → high experienced χ because they cannot exit and bear full cost of requirement escalation. Middle-class aspirants (moderate/constrained) derive moderate d → moderate f(d) → moderate experienced χ because they face barriers but retain some choice. Credential issuers (institutional/arbitrage) derive low d → negative f(d) → negative experienced χ because they benefit from credential demand (beneficiary position) and can switch to issuing new credential types when old ones inflate (arbitrage exit). Employers (organized/constrained) derive moderate-high d because they participate in extraction (maintaining requirements) while being partially victimized by their own enforcement (credential arms races). The alternative credentialing movement (organized/mobile) derives moderate d because they can exit traditional systems (mobile option) and benefit from alternative credential adoption. The key structural fact: beneficiaries with exit options (credential issuers, early adopters) derive low-to-negative d; victims without exit (late entrants, low-income seekers) derive high d. The disparity in directionality drives the tangled rope classification — high suppression combined with genuine coordination function and asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED TANGLED ROPE: The constraint exhibits both genuine coordination (credentials provide reliable signals; employers need filtering mechanisms; aspirants need credible progress markers) and asymmetric extraction (artificial requirement escalation; systematic advantage for high-income actors; perpetual cost escalation for low-income seekers). The mandatrophy is resolved by recognizing that both functions are simultaneously real and structurally coupled. The coordination function cannot be separated from the extraction function — they operate through the same mechanism (credentialization). The beneficiaries (credential issuers, early adopters, high-income actors) benefit precisely because the system creates scarcity through requirement escalation; the victims (late entrants, low-income seekers) are extracted from precisely by the same requirement escalation. Eliminating the extraction would require eliminating the coordination. The system cannot be cleanly separated into a coordination component and an extraction component — they are locked together. This is the defining feature of Tangled Rope. The theater ratio (0.68) indicates that significant institutional inertia and performative activity now masks degraded functional content, suggesting the constraint is degrading toward Piton. However, the essential structure remains Tangled Rope: coordination and extraction remain structurally coupled. The alternative credentialing movement's scaffold perspective is real — it proposes to decouple the two functions by providing coordination through alternative means (portfolio-based hiring, skill verification) that don't require artificial scarcity. If the scaffold succeeds (sunset occurs), the traditional credential system degrades from Tangled Rope to pure Piton (institutional theater with no function). The current classification reflects the system at a point before the sunset is irreversible — the traditional system is still coordinating information, though with increasing theater overhead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_signal_content,
    'What fraction of modern credential requirements actually predict job performance vs. merely filter for conscientiousness or socioeconomic status?',
    'Longitudinal studies comparing credentialed vs non-credentialed workers in same roles; measurement of task performance correlation with specific degree content vs. general education level',
    'If content > 0.60: credentials provide genuine coordination (Rope dominates). If content < 0.40: credentials are pure filtering theater (Snare/Piton dominates). Boundary determination affects whether the constraint is functional or purely extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_signal_content, empirical, 'Empirical signal content of credentials').

omega_variable(
    alternative_adoption_threshold,
    'At what penetration rate of alternative credentials (bootcamp, portfolio-based hiring) do traditional degree requirements collapse as coordination mechanism?',
    'Tracking adoption curves for alternative credentials across industries; correlation analysis of degree-requirement elimination with alternative credential availability; critical mass identification',
    'If threshold < 25% adoption: scaffold sunset is real and near. If threshold > 60%: alternative credentials may remain perpetually niche. Determines whether scaffold classification is structural or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_adoption_threshold, empirical, 'Critical mass threshold for alternative credential adoption').

omega_variable(
    signal_gaming_asymmetry,
    'Do high-income actors have systematically better ability to acquire and maintain credential advantage than low-income actors?',
    'Statistical analysis of parental income correlation with degree completion, time-to-degree, graduate school enrollment, and continued education; measurement of credential-acquisition capacity as function of baseline resource access',
    'If asymmetry > 0.70: credential system functions as systematic wealth extraction (confirms Snare for low-income victims). If asymmetry < 0.40: credential system provides genuine mobility (Rope dominates). Core question for determining whether extraction is feature or bug.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(signal_gaming_asymmetry, empirical, 'Income-based asymmetry in credential acquisition').

omega_variable(
    inflation_decay_rate,
    'How quickly do newly normalized credentials lose signaling value once they become widespread?',
    'Historical time-series analysis of when credentials entered mainstream (all high school graduates have diploma) vs. when they became baseline requirement vs. when they lost differentiation value; measurement of wage premium trajectory as credential adoption expands',
    'If decay < 3 years: system is in rapid-cycle inflation (high theater, high extraction). If decay > 10 years: credentials maintain value longer (more genuine signal content). Affects measurement trajectory and classification trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inflation_decay_rate, empirical, 'Credential value decay timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(prestige_signal_inflation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prestige_tr_t0, prestige_signal_inflation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(prestige_tr_t15, prestige_signal_inflation, theater_ratio, 15, 0.58).
narrative_ontology:measurement(prestige_tr_t30, prestige_signal_inflation, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(prestige_be_t0, prestige_signal_inflation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prestige_be_t15, prestige_signal_inflation, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(prestige_be_t30, prestige_signal_inflation, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(prestige_signal_inflation, information_standard).
narrative_ontology:affects_constraint(prestige_signal_inflation, educational_access_bottleneck).
narrative_ontology:affects_constraint(prestige_signal_inflation, professional_gatekeeping).
narrative_ontology:affects_constraint(prestige_signal_inflation, economic_mobility_barrier).

% DUAL FORMULATION NOTE:
% The credential red queen constraint family decomposes into three distinct but coupled constraints: (1) educational_access_bottleneck (ε≈0.65, Snare) — models the pure extraction of high education costs from low-income seekers; (2) professional_gatekeeping (ε≈0.42, Tangled Rope) — models employer credential requirements as mixed coordination/extraction; (3) prestige_signal_inflation (ε≈0.58, Tangled Rope, this story) — models the credential value decay and requirement escalation cycle. Each has distinct ε because they represent different structural questions: access costs, employer filtering, and signal devaluation. All three are upstream of economic_mobility_barrier (ε≈0.72, Snare), which is the systematic outcome of the credential system's extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(prestige_signal_inflation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
