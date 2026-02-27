% ============================================================================
% CONSTRAINT STORY: credibility_inflation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credibility_inflation, []).

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
 *   constraint_id: credibility_inflation
 *   human_readable: The Meritocratic Dilution: Credibility Inflation
 *   domain: social/academic/economic
 *
 * SUMMARY:
 *   Credibility inflation occurs when the metrics used to signal competence
 *   or trustworthiness (academic degrees, professional certifications, online
 *   verification badges, publication counts, social media follower counts)
 *   proliferate so rapidly that their marginal signal value approaches zero.
 *   Each individual institution or credential issuer is incentivized to
 *   continue producing credentials — they solve the employer's immediate
 *   signal problem and generate revenue. But collectively, the proliferation
 *   of credentials dilutes the signal: employers respond by raising screening
 *   thresholds (requiring higher degrees, more certifications, longer
 *   experience), which forces job applicants into escalating credential
 *   pursuit. This creates a Tangled Rope dynamic: the system has a genuine
 *   coordination function (solving the employer-applicant information gap),
 *   but it extracts from those trapped in credential escalation. New labor
 *   market entrants face a treadmill of continuous education and
 *   certification, bearing the full cost of the dilution. Elite institutions
 *   and credential issuers benefit from increased demand for credentials.
 *   Organized groups (skill-based hiring movements, alternative assessment
 *   platforms) see this as a temporary coordination failure with a real
 *   sunset — direct competency verification can replace traditional
 *   credentials at lower cost. The traditional credentialing system itself
 *   has become substantially performative (high theater ratio), maintaining
 *   legitimacy through institutional inertia and ceremonial authority rather
 *   than through reliable signal content.
 *
 * KEY AGENTS:
 *   - New Labor Market Entrants: Primary victim (powerless/trapped) — face escalating credential requirements with no exit option
 *   - Signal Reliability Commons: Primary victim (powerless/trapped) — abstract collective good degraded by credential inflation; cannot organize to defend signal fidelity
 *   - Elite Institutions & Universities: Primary beneficiary (institutional/arbitrage) — increase tuition and credential offerings as demand grows; capture value from signal authority
 *   - Credential Issuers (certifying bodies, online platforms): Secondary beneficiary (institutional/arbitrage) — proliferate credentials and charge verification fees as alternative signals emerge
 *   - Mid-Career Professionals: Secondary victim (moderate/constrained) — benefit from earlier credentials while signal value remains but constrained by ongoing devaluation
 *   - Skill-Based Hiring Coalition: Organized agents (organized/constrained) — building alternative verification pathways (portfolios, competency assessments, apprenticeships); see credential inflation as temporary market failure
 *   - Employers: Mixed role (institutional/arbitrage) — benefit from credential screening infrastructure but bear cost of threshold escalation and signal degradation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credibility_inflation, 0.48).
domain_priors:suppression_score(credibility_inflation, 0.58).
domain_priors:theater_ratio(credibility_inflation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credibility_inflation, extractiveness, 0.48).
narrative_ontology:constraint_metric(credibility_inflation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(credibility_inflation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credibility_inflation, tangled_rope).
narrative_ontology:human_readable(credibility_inflation, "The Meritocratic Dilution: Credibility Inflation").
narrative_ontology:topic_domain(credibility_inflation, "social/academic/economic").

domain_priors:requires_active_enforcement(credibility_inflation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credibility_inflation, credential_issuers).
narrative_ontology:constraint_beneficiary(credibility_inflation, elite_institutions).
narrative_ontology:constraint_victim(credibility_inflation, signal_reliability).
narrative_ontology:constraint_victim(credibility_inflation, labor_market_entrants).
narrative_ontology:constraint_victim(credibility_inflation, trust_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LABOR MARKET ENTRANT (SNARE) — Trapped in credential escalation with no exit. Must pursue ever more certifications and degrees to signal what a single degree once did. Bears full cost of signal inflation through years of education, debt accumulation, and deferred earnings. No option to opt out — employers screen by credential thresholds that continuously rise. Powerless against the system because the action is distributed: each employer individually rational to raise screening thresholds as the signal degrades.
constraint_indexing:constraint_classification(credibility_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Constrained but not trapped. Initial credentials created genuine career advantage (signal worked), but now faces credential devaluation over career horizon. Can shift to reputation/network-based signals and can leverage existing position, but cannot fully escape the credential system's re-calibration. Benefits from having early credentials while they still signal; bears cost of watching those credentials dilute for younger cohorts.
constraint_indexing:constraint_classification(credibility_inflation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDENTIAL ISSUERS (ROPE) — Institutional beneficiaries. Experience the constraint as a pure coordination mechanism: producing credentials solves the employer's signal problem. Can exit at any time (stop issuing credentials, change standards) but choose not to because credential issuance is their primary revenue model. High arbitrage capacity: can raise tuition as demand increases, can create new credential tiers (master's programs, micro-credentials), can charge for verification. Net beneficiary — extraction flows toward these agents.
constraint_indexing:constraint_classification(credibility_inflation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SKILL-BASED HIRING COALITION (SCAFFOLD) — Organized agents (alternative credential systems, portfolio-based hiring, competency assessments, apprenticeship models) see credential inflation as a temporary market failure with a sunset. Direct skill verification (coding challenges, work samples, apprenticeships) bypass traditional credential theater. These alternatives have low coordination cost and high verification fidelity. Effective extraction from this perspective is low because the coalition perceives and is building real exit pathways. Has sunset clause: as alternative verification matures, traditional credential inflation loses force.
constraint_indexing:constraint_classification(credibility_inflation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE CREDENTIALING RITUAL AS DEGRADED INSTITUTION (PITON) — Institutional perspective on the credentialing system itself. The ritual persists through inertia: employers continue credential screening because 'that's how we've always hired', educational institutions continue issuing credentials because that is their legitimated role, accreditors continue validating institutions because accreditation is their function. But the signal value has decayed substantially — credentials now function primarily as entry fee to application review rather than as reliable competence indicators. Theater ratio is high: the ritual of degree-conferral, graduation ceremonies, transcript exchanges persists despite diminished informational content. The system maintains itself through institutional inertia and ceremonial authority, not because it effectively solves the signal problem.
constraint_indexing:constraint_classification(credibility_inflation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, credential inflation might appear to be an immutable consequence of information asymmetry between employers and job applicants: any cost-based signal will eventually be competed away and require escalation. This perspective risks naturalizing what is actually a contingent institutional arrangement (the credential system) as an unchangeable feature of labor markets. However, the structural data contradicts this — the constraint exhibits beneficiaries, victims, and exit options, revealing it as a Tangled Rope, not a Mountain. The 'inevitable' framing masks the institutional choices that perpetuate the system.
constraint_indexing:constraint_classification(credibility_inflation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credibility_inflation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credibility_inflation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credibility_inflation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credibility_inflation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credibility_inflation, TR),
    TR >= 0.70.

:- end_tests(credibility_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The credential system extracts from those trapped in escalation (new entrants) through additional years of education, debt, and deferred earnings. However, the extraction is not total (snare level) because mid-career professionals can partially exit through reputation-based signals, and some employers are beginning to accept alternative credentials. The escalation is structural but not uniformly applied. Suppression (0.58): Moderate-high. Significant barriers to exit include: employer reliance on credential screening as liability protection, institutional inertia in hiring practices, coordination failure (no single employer can unilaterally shift to alternative signals without risk), accreditation structures that lock in degree requirements, and the self-reinforcing cycle where credential inflation itself makes credentials appear more necessary. But suppression is not total — alternative hiring methods exist and are growing. Theater ratio (0.68): High and increasing. Traditional credentialing has become substantially performative: degree conferral ceremonies, transcript exchanges, accreditation reviews persist with ritualistic authority despite diminished signal content. The ritual maintains legitimacy through institutional tradition and ceremonial markers (graduation ceremonies, diploma framing) rather than through demonstrated ability to predict job performance. The theater has increased from 0.38 to 0.68 over the interval as the gap between signal content and credential abundance has widened.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme and reveals the constraint's true nature as a Tangled Rope with significant extraction asymmetry. The new labor market entrant sees a Snare — they are trapped in an escalating treadmill with no option to opt out. The credential issuer sees a Rope — they are solving a coordination problem (employers need signals, applicants need to demonstrate competence) and capturing value legitimately. The mid-career professional sees a Tangled Rope — they benefited from the system initially but are now watching it dilute. The skill-based hiring coalition sees a Scaffold with a sunset — alternative signals are real, scalable, and will eventually replace credential screening. The credentialing system itself (viewed as an institution) appears as a Piton — it persists through ceremonial authority and institutional inertia despite declining signal content. The analytical observer risks seeing this as a Mountain (inevitable feature of labor markets with information asymmetry), but the structural data reveals it as a contingent institutional arrangement: the extraction and theater are products of specific choices by credential issuers and employers, not immutable laws.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies significantly by agent position. New labor market entrants occupy the target position (d ≈ 0.95): trapped with no exit, bearing maximum extraction. Credential issuers occupy the beneficiary position (d ≈ 0.05): they have arbitrage exit (can stop issuing credentials, raise prices, create new credential tiers) and capture value. Mid-career professionals occupy an intermediate position (d ≈ 0.55): they benefited from credentials when the signal was stronger and have some career lock-in, but they are not trapped in the same way new entrants are. Employers occupy a complex position: nominally beneficiaries (credentials reduce hiring uncertainty) but actually constrained (trapped in credential screening because peers use it). The skill-based hiring coalition occupies the position of organized agents with exit options (d ≈ 0.40): they perceive real alternatives and are building them, so the constraint's extraction force is weaker from their perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mandatrophy because it exhibits genuine dual structure. It is not 'really just extraction hiding as coordination' — there is authentic coordination content: the system does solve the employer-applicant information gap, albeit at increasing cost. The beneficiaries (credential issuers) are genuinely providing a service that reduces information asymmetry. But the extraction is also real: the cost of escalating credentials is borne disproportionately by those with fewer alternatives (new entrants, low-income students). The Tangled Rope classification captures this hybrid: the same system that coordinates supply and demand for labor signals also extracts rent from those trapped in credential escalation. The classification resists collapsing into 'this is really a Snare' or 'this is really a Rope' because both truths are structurally present. The mandatrophy is resolved by the indexical structure: from the new entrant's perspective it approaches Snare; from the credential issuer's perspective it approaches Rope; from the organizing coalition's perspective it is a Scaffold with sunset. The constraint does not mislabel coordination as extraction or vice versa — it manifests as different types from different observational positions, and that perspectival divergence IS the informational content.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    signal_recovery_threshold,
    'What combination of alternative verification methods (portfolios, direct assessment, reputation systems) would sufficiently lower credential screening thresholds to stop the escalation treadmill?',
    'Empirical tracking of hiring practices as skill-based assessment tools mature; measurement of correlation between direct competency metrics and job performance vs. credential metrics; adoption rates of alternative screening in different sectors',
    'If threshold low: coordinated shift to alternative signals is feasible and scaffold sunset is real. If threshold high: credential escalation persists indefinitely because no single alternative is sufficient to replace traditional credentials.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(signal_recovery_threshold, empirical, 'Threshold at which alternative verification methods enable signal recovery').

omega_variable(
    employer_herding_dynamics,
    'Do employers raise credential screening thresholds primarily from rational response to signal degradation or from competitive mimicry and status signaling?',
    'Analysis of hiring threshold changes in controlled contexts (e.g., firms adopting skill-based hiring vs. peers; sectors with labor shortages vs. surplus); controlled labor market experiments; interviews with hiring managers on decision-making',
    'If rational response: credential inflation is an equilibrium of information asymmetry (more structurally difficult to break). If mimicry/status: credential thresholds are maintained by coordination failure and could shift with policy or institution-led re-coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employer_herding_dynamics, empirical, 'Whether credential escalation is driven by information needs or institutional mimicry').

omega_variable(
    micro_credential_sufficiency,
    'Can stacked micro-credentials (certificate chains, digital badges, course completions) reduce the need for traditional degree escalation, or do they simply create a new parallel inflation dynamic?',
    'Tracking of micro-credential adoption and employer acceptance over 5-10 year horizon; comparative analysis of wage premiums for micro-credential stacks vs. traditional degrees; measurement of portfolio-hiring adoption rates',
    'If sufficient: micro-credentials represent a real exit pathway and the scaffold perspective is validated. If insufficient: micro-credentials become an additional requirement on top of degrees (further escalation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(micro_credential_sufficiency, empirical, 'Whether micro-credentials can substitute for traditional degree escalation').

omega_variable(
    institutional_lock_in,
    'To what degree is credential inflation maintained by institutional structures (accreditation requirements, HR policy templates, legal liability concerns) rather than by true information needs?',
    'Comparative analysis of credential inflation rates across jurisdictions with different accreditation regimes; case studies of organizations that shifted to alternative verification and their outcomes; historical analysis of credential threshold changes relative to actual job requirement changes',
    'If high lock-in: policy intervention (eliminating credentialing requirements, mandating competency assessment) could rapidly shift the system. If low lock-in: the system is a true information equilibrium and changes more slowly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_lock_in, conceptual, 'Extent to which institutional structures lock in credential requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credibility_inflation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credibility_inflation, theater_ratio, 0, 0.38).
narrative_ontology:measurement(cred_tr_t10, credibility_inflation, theater_ratio, 10, 0.55).
narrative_ontology:measurement(cred_tr_t20, credibility_inflation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credibility_inflation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cred_be_t10, credibility_inflation, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(cred_be_t20, credibility_inflation, base_extractiveness, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credibility_inflation, information_standard).
narrative_ontology:affects_constraint(credibility_inflation, status_signal_arms_race).
narrative_ontology:affects_constraint(credibility_inflation, educational_debt_trap).
narrative_ontology:affects_constraint(credibility_inflation, employer_liability_aversion).

% DUAL FORMULATION NOTE:
% Credibility inflation is part of a constraint family examining how information systems degrade under competitive pressure. It is downstream of the status signal arms race (the underlying game of signaling competence through costly displays) and upstream of educational debt trap (the financial consequence of credential escalation) and employer liability aversion (the structural driver of credential screening). Each constraint in the family has distinct extractiveness; credibility_inflation specifically tracks how the signal itself dilutes over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credibility_inflation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
