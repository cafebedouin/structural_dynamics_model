% ============================================================================
% CONSTRAINT STORY: hiv_prep_prevention_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hiv_prep_prevention_2026, []).

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
 *   constraint_id: hiv_prep_prevention_2026
 *   human_readable: PrEP-Mediated HIV Prevention as Coordination and Extraction
 *   domain: healthcare/technological/social
 *
 * SUMMARY:
 *   Pre-exposure prophylaxis (PrEP) is a daily antiviral medication that
 *   reduces HIV transmission risk by over 90% when taken correctly. Since FDA
 *   approval in 2012, PrEP has become a cornerstone of HIV prevention in
 *   wealthy nations, particularly for men who have sex with men, sex workers,
 *   and people in serodiscordant relationships. However, PrEP's prevention
 *   benefit is structurally entangled with pharmaceutical control, cost
 *   barriers, and medicalization of sexual health. The constraint exhibits
 *   simultaneous coordination and extraction: PrEP solves the genuine public
 *   health problem of HIV prevention while creating gatekeeping mechanisms
 *   that extract value from access inequity. Uninsured populations face
 *   snare-like barriers; insured populations experience tangled
 *   coordination-extraction; pharmaceutical manufacturers and insurers
 *   experience pure coordination benefit; public health coalitions see a
 *   temporary coordination problem solvable through policy and generic
 *   access. The theater_ratio reflects rising performative activity:
 *   surveillance reporting, adherence monitoring, and pharmacy gatekeeping
 *   have increased as PrEP adoption has grown, without proportional increases
 *   in prevention outcomes in low-access populations.
 *
 * KEY AGENTS:
 *   - Pharmaceutical Manufacturers: Primary beneficiary (institutional/arbitrage) — capture market value from PrEP intellectual property and distribution; benefit from expansion into high-margin prevention markets
 *   - Uninsured and Underinsured Populations: Primary victim (powerless/trapped) — HIV risk persists despite available prevention; trapped by cost barriers and insurance gatekeeping; no exit options
 *   - Insured Urban Populations: Secondary beneficiary (moderate/constrained) — gain prevention autonomy and sexual health options; constrained by daily medication dependency and medical surveillance
 *   - Public Health Coalitions: Organized agents (organized/constrained) — Medicaid programs, community health departments, LGBTQ+ advocacy organizations building alternative access pathways through policy and generic supply chains
 *   - Healthcare Systems and Insurers: Institutional actors (institutional/arbitrage) — control PrEP access through formulary decisions, prior authorization, and coverage policies; arbitrage between prevention costs and downstream treatment costs
 *   - Epidemiological Surveillance Systems: Institutional infrastructure (institutional/arbitrage) — maintain monitoring systems that track uptake and outcomes; see own processes as partially degraded (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hiv_prep_prevention_2026, 0.38).
domain_priors:suppression_score(hiv_prep_prevention_2026, 0.52).
domain_priors:theater_ratio(hiv_prep_prevention_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, extractiveness, 0.38).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(hiv_prep_prevention_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hiv_prep_prevention_2026, tangled_rope).
narrative_ontology:human_readable(hiv_prep_prevention_2026, "PrEP-Mediated HIV Prevention as Coordination and Extraction").
narrative_ontology:topic_domain(hiv_prep_prevention_2026, "healthcare/technological/social").

domain_priors:requires_active_enforcement(hiv_prep_prevention_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, healthcare_systems_with_prep_access).
narrative_ontology:constraint_beneficiary(hiv_prep_prevention_2026, high_risk_populations_with_insurance).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, uninsured_or_underinsured_populations).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, sexual_health_autonomy).
narrative_ontology:constraint_victim(hiv_prep_prevention_2026, prevention_alternative_pathways).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED HIGH-RISK POPULATION (SNARE) — Trapped by cost barriers. PrEP at $1,300-1,600/month without insurance creates an extraction mechanism: HIV risk persists despite available pharmaceutical solution, but access is gatekept by insurance status and pharmacy distribution networks. No exit option — cannot afford alternative prevention, cannot exit sexual health infrastructure, cannot arbitrage into parallel systems. Maximum experienced extraction with high suppression.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED URBAN POPULATIONS WITH PREP ACCESS (TANGLED ROPE) — Constrained but mobile. PrEP functions as coordination: enables sexual autonomy and partner protection through transparent mechanism. But also exhibits extraction: daily medication dependency, pharmacy surveillance, medical gatekeeping of sexual health decisions, data surveillance through prescription and healthcare systems. Benefits from prevention tool; constrained by medicalization and monitoring infrastructure.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURERS AND HEALTHCARE SYSTEMS (ROPE) — Institutional actors with arbitrage options. PrEP functions as pure coordination from their perspective: solves the legitimate public health problem of HIV prevention, enables market expansion, creates sustainable revenue model. Net beneficiaries — extract value from the system but provide genuine prevention service. Can arbitrage into alternative therapeutic markets if PrEP loses profitability.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC HEALTH ADVOCATES AND POLICY COALITIONS (SCAFFOLD) — Organized agents (Medicaid programs, public health departments, LGBTQ+ advocacy groups) see PrEP as a temporary coordination problem with sunset logic: universal PrEP access through Medicaid expansion, community health worker distribution, and decriminalized sexual health norms are creating alternative pathways that bypass pharmaceutical gatekeeping. Low effective extraction because the coalition has agency and sees an exit path toward post-pharmaceutical prevention infrastructure.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EPIDEMIOLOGICAL SURVEILLANCE INFRASTRUCTURE (PITON) — The surveillance system that tracks PrEP uptake, efficacy, and side effects is substantially performative. Aggregate statistics are collected and reported, but feedback loops are slow, and surveillance often serves pharmaceutical interests rather than community health. The infrastructure persists through institutional inertia — public health agencies maintain it because alternatives haven't fully replaced it, not because it optimizes for prevention. Theater_ratio reflects that much reporting is ritual.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some form of biomedical prevention intervention is inherent to HIV disease management: infectious disease control always requires some access barrier or selection mechanism, and the gap between prevention availability and universal adoption is a structural property of epidemiology itself. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that medicalization and cost barriers are contingent institutional arrangements, not laws of nature.
constraint_indexing:constraint_classification(hiv_prep_prevention_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hiv_prep_prevention_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hiv_prep_prevention_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hiv_prep_prevention_2026, TR),
    TR >= 0.70.

:- end_tests(hiv_prep_prevention_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. PrEP's pharmaceutical gatekeeping creates an extraction mechanism where access is controlled by insurance status, pharmacy networks, and cost. However, extraction is not total — generic substitutes exist, public health programs are expanding access, and non-pharmaceutical prevention alternatives remain available. The metric reflects that pharmaceutical control extracts value but is contested and eroding. Suppression (0.52): Moderate. Barriers include cost ($1,300-1,600/month retail), insurance requirements, pharmacy availability, medical gatekeeping (prescription requirement), and behavioral barriers (daily adherence, stigma). These are high but not absolute — some populations can and do access PrEP, public programs exist, and cost barriers are being contested through policy. Theater ratio (0.58): Moderate-high. Surveillance and reporting for PrEP includes substantial performative activity: adherence monitoring, side effect reporting, epidemiological tracking, and public health communications that serve pharmaceutical interests as much as prevention outcomes. The ratio has increased as PrEP programs have matured — early adoption was more outcome-focused; mature systems now include more ritual and compliance monitoring.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's perspectival gap reveals that PrEP's classification depends entirely on insurance status and position in the access pipeline, not on pharmacology. An uninsured person sees a snare: prevention exists but is cost-gatekept. An insured person sees tangled_rope: prevention is accessible but requires daily medication, medical surveillance, and pharmacy gatekeeping. A pharmaceutical manufacturer sees rope: legitimate problem solved with sustainable revenue. A public health advocate sees scaffold: temporary barrier being dismantled through Medicaid expansion and generic supply. An epidemiologist sees piton: prevention infrastructure persists through performative surveillance, not functional outcomes. The civilizational observer risks naturalizing medicalization: 'HIV prevention requires pharmaceutical management' — but the structural data reveals this as a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are computed from beneficiary/victim status and exit options. Uninsured populations emerge as victims (explicit in base_properties) with trapped exit — they bear extraction without being able to arbitrage, producing d ≈ 0.95. Insured populations are ambiguously positioned: they are users (not explicit victims in base_properties), but they experience constraining surveillance and medication dependency. Their d ≈ 0.50 reflects symmetric costs and benefits. Pharmaceutical manufacturers are explicit beneficiaries with arbitrage options: can shift markets, can develop alternative therapeutics, can arbitrage into different regions or price points. Their d ≈ 0.05, producing negative f(d), capturing that they experience institutional subsidy. Public health coalitions have constrained but real exit through policy advocacy and generic supply chains: d ≈ 0.45, moderate extraction experienced. Healthcare systems and insurers are institutional beneficiaries with high arbitrage: d ≈ 0.10. Epidemiological surveillance has arbitrage through reporting choices: d ≈ 0.40.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids conflating coordination (HIV prevention solved) with extraction (access gatekeeping) by maintaining beneficiary/victim distinctions across perspectives. The tangled_rope classification correctly separates the coordination function (PrEP prevents HIV transmission) from the extraction function (pharmaceutical and insurance gatekeeping). Uninsured populations experience snare (pure extraction, no coordination benefit). Insured populations experience tangled_rope (both coordination and extraction). Pharmaceutical manufacturers experience rope (pure coordination — they solve the problem, no extraction from their end). The mandatrophy resolves by recognizing that 'PrEP is prevention' is true (coordination) AND 'PrEP access is gatekept' is true (extraction), and these are not contradictory — they are simultaneously true at different positions in the institutional pipeline. The classification structure prevents the error of calling PrEP pure coordination (false rope) or pure extraction (false snare) globally; instead, it correctly assigns different types to different observational positions and notes the extractive flow from uninsured to pharmaceutical/healthcare beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prep_access_threshold_equity,
    'What proportion of high-risk populations must have access to PrEP before the cost barrier transitions from snare to tangled_rope?',
    'Longitudinal analysis of access coverage, viral suppression rates by insurance status, and behavioral outcome data; comparison of prevention outcomes in high-access vs low-access regions',
    'If threshold < 40% access: constraint remains snare for majority. If threshold > 75% access: constraint is tangled_rope for most populations. At intermediate thresholds: mixed classification across populations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prep_access_threshold_equity, empirical, 'Access threshold for classifying extraction mechanism').

omega_variable(
    behavioral_substitution_effects,
    'Does PrEP availability reduce or increase overall HIV transmission risk through behavioral compensation?',
    'RCT data on sexual behavior changes in PrEP users; epidemiological modeling of transmission outcomes with/without behavioral response; long-term cohort follow-up of sexual partner infection rates',
    'If transmission risk reduces: PrEP functions as pure coordination (stronger rope classification). If transmission risk unchanged: PrEP functions as risk displacement (stronger snare/tangled_rope classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_substitution_effects, empirical, 'Whether PrEP reduces net transmission risk or displaces it').

omega_variable(
    alternative_prevention_sufficiency,
    'Do non-pharmaceutical prevention methods (partner notification, regular testing, condom use, U=U sexual strategies) constitute structurally independent alternatives to PrEP-mediated prevention?',
    'Comparative effectiveness analysis of prevention pathways; cost-effectiveness analysis of alternative prevention models; community implementation studies of non-pharmaceutical prevention in resource-constrained settings',
    'If structurally independent: PrEP is a coordination option, not a gatekeeper (rope from more perspectives). If dependent on PrEP for high-risk populations: PrEP is an extraction chokepoint (snare from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_prevention_sufficiency, empirical, 'Whether non-pharmaceutical prevention methods are sufficient alternatives').

omega_variable(
    generic_substitution_viability,
    'Can generic tenofovir/emtricitabine formulations achieve the same prevention efficacy as branded PrEP at significantly lower cost?',
    'Bioequivalence studies of generic formulations; epidemiological outcomes in regions with generic PrEP access; cost analysis of generic vs branded supply chains',
    'If viable: pharmaceutical gatekeeping is contingent (snare→rope/scaffold). If barriers remain: extraction persists even with generic availability (snare remains structural).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_substitution_viability, empirical, 'Whether generics can substitute for branded PrEP').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hiv_prep_prevention_2026, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, hiv_prep_prevention_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_tr_t8, hiv_prep_prevention_2026, theater_ratio, 8, 0.5).
narrative_ontology:measurement(prep_tr_t16, hiv_prep_prevention_2026, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, hiv_prep_prevention_2026, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(prep_be_t8, hiv_prep_prevention_2026, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(prep_be_t16, hiv_prep_prevention_2026, base_extractiveness, 16, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hiv_prep_prevention_2026, global_infrastructure).
narrative_ontology:affects_constraint(hiv_prep_prevention_2026, sexual_health_medicalization).
narrative_ontology:affects_constraint(hiv_prep_prevention_2026, pharmaceutical_pricing_extraction).
narrative_ontology:affects_constraint(hiv_prep_prevention_2026, hiv_stigma_suppression).

% DUAL FORMULATION NOTE:
% PrEP-mediated prevention decomposes into two structurally distinct constraints: (1) biomedical prevention efficacy (ε ≈ 0.08, Mountain — biological property of tenofovir), and (2) access gatekeeping through pharmaceutical and insurance control (ε ≈ 0.38, Tangled Rope — contingent institutional arrangement). The present story addresses the second constraint. The first constraint (biological efficacy) would classify as Mountain from all perspectives. These are linked via network.affects_constraints because access barriers affect the implementation of biomedical efficacy, but they have distinct ε values and failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hiv_prep_prevention_2026, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
