% ============================================================================
% CONSTRAINT STORY: us_military_recruitment_advertising
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_military_recruitment_advertising, []).

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
 *   constraint_id: us_military_recruitment_advertising
 *   human_readable: US Military Recruitment Advertising
 *   domain: political/labor_market
 *
 * SUMMARY:
 *   US military recruitment advertising operates at the intersection of labor
 *   market information asymmetry and economic vulnerability. The military
 *   targets young adults aged 17-24, with disproportionate targeting of
 *   communities with limited economic alternatives. The constraint exhibits
 *   significant suppression (0.68)—barriers to exit include service
 *   commitments (8-10 years), occupational hazards (combat deployment, injury
 *   risk), and information deficits about actual quality of life. Theater
 *   ratio is high (0.81), reflecting that recruitment campaigns emphasize
 *   adventure, camaraderie, and personal growth while minimizing casualty
 *   statistics, PTSD prevalence, and post-service employment barriers.
 *   Extractiveness (0.52) reflects that the military captures labor
 *   substantially below what would be required in a competitive market with
 *   full information, sustained through limited alternative pathways for
 *   economically vulnerable populations. The constraint's classification
 *   varies across perspectives: pure snare for trapped recruits, mixed
 *   rope-snare for middle-class recruits, pure rope for recruiters, temporary
 *   scaffold for transparency advocates, degraded piton for the institutional
 *   apparatus, and false mountain for those who naturalize information
 *   asymmetry as inevitable.
 *
 * KEY AGENTS:
 *   - Economically Vulnerable Recruits: Primary victims (powerless/trapped) — limited regional job opportunities, inadequate healthcare access, housing insecurity create structural desperation that recruitment messaging exploits
 *   - Military Recruitment Command: Primary beneficiary (institutional/arbitrage) — achieves staffing targets through targeted messaging; benefits directly from information asymmetry
 *   - Defense Contractors: Secondary beneficiary (institutional/arbitrage) — benefit from sustained military recruitment and staffing levels that support procurement contracts
 *   - Middle-Class Recruits: Secondary actors (moderate/constrained) — experience mixed coordination and extraction; have backup options but still bear deployment risk
 *   - Veterans' Advocacy Groups: Organized agents (organized/constrained) — push for transparency, disclosure requirements, and alternative pathways; see sunset potential
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing labor market information asymmetry as inherent rather than contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_military_recruitment_advertising, 0.52).
domain_priors:suppression_score(us_military_recruitment_advertising, 0.68).
domain_priors:theater_ratio(us_military_recruitment_advertising, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_military_recruitment_advertising, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_military_recruitment_advertising, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_military_recruitment_advertising, snare).
narrative_ontology:human_readable(us_military_recruitment_advertising, "US Military Recruitment Advertising").
narrative_ontology:topic_domain(us_military_recruitment_advertising, "political/labor_market").

domain_priors:requires_active_enforcement(us_military_recruitment_advertising).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_military_recruitment_advertising, military_recruitment_command).
narrative_ontology:constraint_beneficiary(us_military_recruitment_advertising, defense_contractors).
narrative_ontology:constraint_victim(us_military_recruitment_advertising, economically_vulnerable_recruits).
narrative_ontology:constraint_victim(us_military_recruitment_advertising, young_adults_without_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY VULNERABLE RECRUIT (SNARE) — Limited employment alternatives in their region, inadequate information about actual deployment risks and quality of life. High suppression: housing costs, healthcare barriers, and limited job market create desperation that recruitment messaging exploits. No meaningful exit option once enlisted; bears full cost of extraction through low initial pay, service commitment, and occupational hazards.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS RECRUIT WITH ALTERNATIVES (TANGLED ROPE) — Benefits from military service (education benefits, job training, healthcare, career pathway) but also experiences extraction through training intensity, deployment risk, and service commitment constraints. Has constrained exit (can exit with penalty but has backup options). Mixed extraction and coordination — the service provides genuine training while also extracting labor.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MILITARY RECRUITMENT COMMAND (ROPE) — Benefits directly from recruitment targets. Advertising enables coordination of labor supply with military staffing needs. Views the constraint as pure coordination: presenting military service as attractive solves the labor-matching problem. Has arbitrage options and faces minimal enforcement cost for advertising messaging.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSPARENCY AND ADVOCACY COALITION (SCAFFOLD) — Organized agents (veterans' advocacy groups, parent organizations, transparency advocates) view recruitment advertising as a temporary coordination problem being solved through disclosure requirements, truth-in-advertising enforcement, and alternative education pathways. See sunset mechanism: GI Bill modernization, apprenticeship programs, and income-share agreements are creating exit alternatives that reduce reliance on military recruitment for economic mobility.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL RECRUITMENT RITUAL (PITON) — The formal military recruitment apparatus persists through institutional inertia despite declining effectiveness. High school recruitment programs, slick advertising campaigns, and enlistment incentives continue despite competition from civilian labor market. Theater ratio high (0.81): much of recruitment activity is performative—maintaining recruiting stations, running advertisements, participating in career fairs—relative to actual conversion impact. The constraint persists because alternatives are not yet fully institutionalized.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some information asymmetry in labor markets is inherent: employers always present idealized images of work, and young adults always lack full information about long-term career consequences. This perspective sees recruitment advertising as an immutable feature of labor markets themselves. However, structural data contradicts the mountain classification—the measured suppression (0.68) and theater (0.81) reveal this as a false summit, exposing that what appears 'natural' is actually a contingent regulatory choice.
constraint_indexing:constraint_classification(us_military_recruitment_advertising, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_military_recruitment_advertising_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_military_recruitment_advertising, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_military_recruitment_advertising, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_military_recruitment_advertising, TR),
    TR >= 0.70.

:- end_tests(us_military_recruitment_advertising_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Military recruitment achieves below-market labor compensation through information advantage and limited alternatives. A recruit earning $28,000-$32,000 annually in base pay, with housing and food provided, would earn $50,000+ in equivalent civilian employment for comparable skills. The gap is extraction, but not maximal—the military also provides genuine benefits (healthcare, housing, training, education assistance). Suppression (0.68): High. Significant barriers to informed decision-making: casualty and injury statistics are not routinely disclosed in recruitment messaging; PTSD prevalence is understated; post-service employment barriers are rarely mentioned; regional economic alternatives are not presented. Career changers have constrained options once enlisted. Theater ratio (0.81): High and increasing. Recruitment campaigns emphasize emotional appeal and aspirational narratives (leadership development, global impact, brotherhood/sisterhood) relative to functional information about actual service conditions. The ratio has increased over the 20-year interval as campaigns have become more sophisticated and economically vulnerable populations have faced greater desperation, making emotional appeals more effective.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. Military recruiters perceive pure coordination—matching labor supply to organizational need. Economically vulnerable recruits perceive pure extraction—trapped in a coercive arrangement. Transparency advocates perceive a temporary problem with architectural solutions (disclosure, alternative pathways). The institutional apparatus perceives its own degradation (piton)—maintaining recruitment theater despite declining effectiveness. The analytical observer risks naturalizing asymmetry as inevitable but the structural metrics expose this as false: the suppression and theater values reveal design choices, not laws of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The economically vulnerable recruit's directionality is derived from their trapped exit position and victim status: they have no meaningful alternatives outside the military for accessing healthcare, housing stability, and economic security. This produces high d (approximately 0.92), and the sigmoid f(d) converts this to strong experienced extraction. The military recruiter's directionality is derived from beneficiary status and arbitrage exit options: they are capturing labor surplus and face no personal cost for information asymmetry. This produces low d (approximately 0.10), yielding negative experienced extraction (they benefit). The middle-class recruit has constrained exit (can exit with penalties) and mixed victim/beneficiary status (they gain training but lose freedom), producing intermediate d (approximately 0.55). The difference in perspectives arises from the true asymmetry in the structural relationship: exit options differ fundamentally between economic strata.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: The constraint classifies as snare from the powerless/trapped perspective because it meets the three-gate combination: extractiveness (0.52) ≥ 0.46, suppression (0.68) ≥ 0.60, and effective extraction chi (via f(d) with d ≈ 0.92 for trapped agents) ≥ 0.66. The mandatrophy test requires demonstrating that this is not misclassified as pure extraction when coordination elements exist. Coordination elements are present—military service genuinely provides training, healthcare, and economic stability. However, these are not proportional to the extraction: (1) a vulnerable recruit has no choice to participate; (2) the extraction premium (below-market labor compensation) is non-consensual; (3) the information deficit is actively maintained through selective messaging. The coordination function is subordinate to extraction function. The middle-class perspective (tangled_rope) confirms that when exit options improve, the classification shifts—the same institution appears as hybrid rather than pure extraction. This perspectival difference validates the snare classification for trapped agents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    information_asymmetry_threshold,
    'What level of information disclosure would reduce suppression to acceptable levels without eliminating recruitment capacity?',
    'Comparative analysis of disclosure requirements across allied militaries; A/B testing of recruitment messaging with varying risk and casualty information; longitudinal tracking of recruitment success under different transparency regimes',
    'If threshold is achievable: suppression can be reduced without structural change (snare → tangled_rope possible). If threshold requires suppression to remain high: snare classification is structural, not remediable through disclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_threshold, empirical, 'Threshold for information disclosure that maintains recruitment while reducing asymmetry').

omega_variable(
    economic_alternative_sufficiency,
    'Do civilian education and apprenticeship pathways provide genuine alternatives that reduce economic pressure driving military recruitment of vulnerable populations?',
    'Regional analysis of recruitment rates vs. availability of alternative economic pathways; comparison of military vs civilian compensation and benefits; longitudinal tracking of substitute program expansion',
    'If alternatives are sufficient: scaffold sunset is real, extraction mechanism weakens over time. If alternatives remain insufficient: economic trapping persists, snare classification is stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_alternative_sufficiency, empirical, 'Whether civilian alternatives provide sufficient economic mobility').

omega_variable(
    advertising_conversion_efficacy,
    'How much of military recruitment success derives from advertising messaging versus structural economic desperation?',
    'Regional controlled trials varying advertising intensity while holding economic conditions constant; analysis of recruitment success during economic booms vs busts; comparison of advertising spend to conversion rates across economic strata',
    'If advertising is primary driver: constraint is primarily manipulation (snare confirmed, theater high). If economic desperation dominates: advertising is performative cover for structural extraction (piton classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advertising_conversion_efficacy, empirical, 'Relative contribution of advertising vs economic desperation to recruitment success').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_military_recruitment_advertising, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(milrec_tr_t0, us_military_recruitment_advertising, theater_ratio, 0, 0.65).
narrative_ontology:measurement(milrec_tr_t10, us_military_recruitment_advertising, theater_ratio, 10, 0.73).
narrative_ontology:measurement(milrec_tr_t20, us_military_recruitment_advertising, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(milrec_be_t0, us_military_recruitment_advertising, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(milrec_be_t10, us_military_recruitment_advertising, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(milrec_be_t20, us_military_recruitment_advertising, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_military_recruitment_advertising, resource_allocation).
narrative_ontology:affects_constraint(us_military_recruitment_advertising, information_asymmetry_labor_markets).
narrative_ontology:affects_constraint(us_military_recruitment_advertising, socioeconomic_mobility_constraints).

% DUAL FORMULATION NOTE:
% Military recruitment advertising is downstream of broader labor market information asymmetries but represents a distinct structural constraint focused on military-specific recruitment. The upstream constraint (labor market asymmetry) has different extractiveness; this constraint is more specific to the military context and the strategic deployment of advertising to exploit economic vulnerability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_military_recruitment_advertising, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
