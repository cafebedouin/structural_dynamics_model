% ============================================================================
% CONSTRAINT STORY: welfare_eligibility_verification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_welfare_eligibility_verification, []).

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
 *   constraint_id: welfare_eligibility_verification
 *   human_readable: Welfare Eligibility Verification Systems
 *   domain: social_policy/administrative_burden
 *
 * SUMMARY:
 *   Welfare eligibility verification systems create a structural constraint
 *   between the legitimate policy need to prevent fraud and misuse of public
 *   funds against the administrative burden imposed on low-income applicants
 *   seeking benefits. The constraint exhibits characteristics of tangled
 *   rope: genuine coordination function (sorting eligible from ineligible
 *   recipients, preventing duplicate enrollment, maintaining program
 *   integrity) coexists with asymmetric extraction (administrative burden
 *   concentrated on applicants with fewest resources to bear it). The theater
 *   ratio (0.64) reflects that verification requirements have expanded beyond
 *   fraud prevention needs — recertification cycles, document requirements,
 *   and appointment-based processing persist despite evidence that fraud is
 *   rare (1-3% of claims) and administrative errors are more common. The
 *   constraint's suppression (0.68) reflects structural barriers: applicants
 *   lack time, transportation, documentation, and literacy resources required
 *   for verification. The trajectory shows increasing theater and
 *   extractiveness over the 20-year interval, indicating institutional drift
 *   toward more burdensome verification practices despite stable or declining
 *   fraud rates.
 *
 * KEY AGENTS:
 *   - Low-income Applicants: Primary victims (powerless/trapped) — bear full administrative burden; material dependency prevents exit
 *   - State Budget Gatekeepers: Primary beneficiaries (institutional/arbitrage) — control fund access; use verification as extraction mechanism to suppress applications
 *   - Program Administrators: Secondary beneficiary (institutional/arbitrage) — justify institutional authority through compliance strictness
 *   - Community Service Providers: Secondary victim (moderate/constrained) — absorb verification costs through staff time and administrative overhead; constrained by funding dependency
 *   - Welfare Bureaucracy: Institutional actor (institutional/arbitrage) — experiences verification as pure coordination; maintains gatekeeping authority
 *   - Welfare Reform Coalition: Organized agents (organized/constrained) — poverty organizations, digital access nonprofits building simplified verification pathways
 *   - Fraud Prevention Theater: Institutional practice (institutional/arbitrage) — symbolic verification maintains political legitimacy despite low actual fraud
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(welfare_eligibility_verification, 0.58).
domain_priors:suppression_score(welfare_eligibility_verification, 0.68).
domain_priors:theater_ratio(welfare_eligibility_verification, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(welfare_eligibility_verification, extractiveness, 0.58).
narrative_ontology:constraint_metric(welfare_eligibility_verification, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(welfare_eligibility_verification, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(welfare_eligibility_verification, tangled_rope).
narrative_ontology:human_readable(welfare_eligibility_verification, "Welfare Eligibility Verification Systems").
narrative_ontology:topic_domain(welfare_eligibility_verification, "social_policy/administrative_burden").

domain_priors:requires_active_enforcement(welfare_eligibility_verification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(welfare_eligibility_verification, state_budget_gatekeepers).
narrative_ontology:constraint_beneficiary(welfare_eligibility_verification, program_administrators).
narrative_ontology:constraint_victim(welfare_eligibility_verification, low_income_applicants).
narrative_ontology:constraint_victim(welfare_eligibility_verification, program_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME APPLICANT (SNARE) — Trapped by material dependency on benefits and unable to exit verification demands without losing access. Bears full weight of administrative burden: document collection, appointment scheduling, compliance deadlines. No alternative pathway; suppression is structural and complete.
constraint_indexing:constraint_classification(welfare_eligibility_verification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY SERVICE PROVIDER (TANGLED ROPE) — Constrained by funding dependency on government contracts tied to program compliance metrics. Provides genuine coordination function (verifying eligibility prevents fraud and maintains program legitimacy) but absorbs extraction: verification costs borne disproportionately by staff time, administrative overhead, and blame for access delays. Can exit but at high cost to organizational mission.
constraint_indexing:constraint_classification(welfare_eligibility_verification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WELFARE BUREAUCRACY (ROPE) — Benefits from verification systems as pure coordination mechanism: sorting eligible from ineligible applicants, preventing duplicate enrollment, maintaining audit trail. Experiences the constraint as legitimate gatekeeping, not extraction. Net beneficiary — system preserves their institutional authority and compliance function.
constraint_indexing:constraint_classification(welfare_eligibility_verification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WELFARE REFORM COALITION (SCAFFOLD) — Organized advocates (poverty organizations, digital access nonprofits) are building alternative pathways: simplified income verification, data-sharing between agencies, presumptive eligibility for known-poor households. These represent temporary scaffolding toward a sunset where algorithmic pre-screening eliminates manual application burdens. High suppression is tolerated because the coalition sees a visible exit path.
constraint_indexing:constraint_classification(welfare_eligibility_verification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FRAUD PREVENTION THEATER (PITON) — Welfare verification systems maintain high theater ratio: burdensome document requirements, recertification cycles, and eligibility reviews persist despite evidence that applicant fraud is rare (typically 1-3% of claims) and administrator error is more common. The ritual persists through institutional inertia — politicians maintain tough eligibility scrutiny to signal fiscal responsibility, even though verification costs exceed fraud prevented. The system is maintained because alternatives haven't replaced the symbolic function.
constraint_indexing:constraint_classification(welfare_eligibility_verification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing perspective, some verification is inherent to any transfer program: distinguishing eligible from ineligible recipients is a logical requirement, not a policy choice. From this view, welfare eligibility verification is an immutable structural feature of resource distribution. However, the empirical data contradicts this — verification burden is highly contingent on policy design choices (digital vs paper, real-time vs periodic, presumptive vs application-based). The mountain classification naturalizes what are actually design decisions.
constraint_indexing:constraint_classification(welfare_eligibility_verification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(welfare_eligibility_verification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(welfare_eligibility_verification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(welfare_eligibility_verification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(welfare_eligibility_verification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(welfare_eligibility_verification, TR),
    TR >= 0.70.

:- end_tests(welfare_eligibility_verification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The system extracts through administrative burden (time, transportation, documentation costs) that is borne by those least able to afford it. However, extraction is not maximal (snare-level) because some applicants do complete verification and access benefits, and some advocates are successfully reducing burden. The value reflects that extraction is built into design but not absolute. Suppression (0.68): Moderately high. Structural barriers include: lack of transportation to offices, work schedules incompatible with appointment availability, missing documents (ID, address proof) that are costly to obtain, literacy barriers for complex forms, language barriers, technology access gaps, and temporal burden of recertification. Suppression is not total because some groups (organized advocates, community providers) can reduce barriers for specific populations. Theater ratio (0.64): Moderately high and increasing. Evidence: fraud rates in welfare programs are consistently 1-3%, yet verification procedures have intensified over time; recertification cycles persist even when applicant status hasn't materially changed; document requirements duplicate information agencies already possess; appointment-based processing creates artificial scarcity that delays access. Digital simplification efforts (online applications, data-sharing) would reduce theater without reducing actual fraud detection, yet these remain incomplete.
 *
 * PERSPECTIVAL GAP:
 *   The low-income applicant experiences snare-level effective extraction through the combination of base extractiveness (0.58), high d from victim status (0.95), and national scope (1.0). The reform coalition experiences lower effective extraction through the combination of same base extractiveness, lower d from organized status (0.55), and constrained exit that partially offsets power disadvantage through collective action. The welfare bureaucracy experiences rope (pure coordination) because d is low (0.10) and they benefit from the system. This perspectival gap — snare vs rope at the same time and place — is the diagnostic signature of tangled rope: the constraint functions as coordination for some agents (budget control, program integrity) and as pure extraction for others (access barriers for powerless agents). No single classification captures both truths; tangled rope captures both.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is derived from each agent's structural position relative to the extraction flow. Low-income applicants are victims with trapped exit options: high d (≈0.95) yielding high f(d) (≈1.42) and high experienced extractiveness chi. State budget gatekeepers are beneficiaries with arbitrage options: low d (≈0.05) yielding negative f(d) (≈-0.12) and negative experienced extractiveness. Community providers are victims with constrained exit: moderate-high d (≈0.70) yielding moderate f(d) (≈1.00) and moderate chi. Welfare bureaucracy is beneficiary with arbitrage: low d (≈0.10) yielding f(d) ≈0.00, rope classification. The reform coalition is organized with constrained exit: moderate d (≈0.55) yielding f(d) ≈0.75 — they experience extraction but have organizational power to contest it. The directionality pipeline is the mechanism by which the same constraint (0.58 base extractiveness) produces different effective extraction (χ) for different agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that verification systems are genuinely hybrid: they solve legitimate coordination problems (preventing fraud, avoiding duplicate enrollment, maintaining audit trails) while simultaneously extracting through administrative burden. The mandatrophy is NOT 'is this coordination or extraction?' but 'whose experience dominates the classification?' From the analytical perspective (civilizational scope), the constraint appears as mountain — verification is inherent to transfers. But from the powerless perspective (biographical scope), it appears as snare — verification is an extraction mechanism. The tangled rope classification reflects that both are true: the constraint has real coordination function (snare-only classification would be false natural law) AND real asymmetric extraction (rope-only classification would ignore the burden). The resolution is that the constraint IS tangled rope at the institutional level but appears as snare to those it most affects. This gap is not an error in classification — it is the core structural phenomenon the constraint story must capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fraud_detection_vs_access_barrier_tradeoff,
    'What proportion of current verification burden is justified by fraud prevention versus representing unnecessary access barriers?',
    'Comparative analysis of fraud rates across jurisdictions with varying verification stringency; cost-benefit analysis of prevented fraud vs applicant burden',
    'If fraud prevention justifies current burden: classify as legitimate mountain. If burden exceeds fraud prevention value by 3x+: classification shifts toward snare for powerless agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fraud_detection_vs_access_barrier_tradeoff, empirical, 'Proportion of verification burden justified by fraud prevention').

omega_variable(
    administrative_burden_extraction_mechanism,
    'Is administrative burden (long application forms, document collection, appointment scheduling) an intentional extraction mechanism or an unintended side effect of verification design?',
    'Policy analysis of jurisdictions that simplified application processes without reducing fraud detection; interviews with policy designers regarding burden intentionality',
    'If intentional: snare classification accurate — burden is extraction mechanism. If unintended: tangled rope classification more appropriate — extraction is side effect, not primary function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_burden_extraction_mechanism, conceptual, 'Whether administrative burden is intentional extraction or unintended design effect').

omega_variable(
    data_sharing_integration_technical_feasibility,
    'Are the technical barriers to integrated multi-agency eligibility verification (IRS, Social Security, state unemployment) genuinely insurmountable or primarily organizational/political?',
    'Technical feasibility audits of data-sharing integration; comparison with other countries that have implemented integrated systems; cost estimation for full integration',
    'If genuinely technical: verification burden is structural necessity, mountain classification justified. If primarily organizational: scaffold sunset is real — simplified verification is achievable, making reform coalition''s vision structural rather than aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_sharing_integration_technical_feasibility, empirical, 'Technical feasibility of integrated multi-agency eligibility verification').

omega_variable(
    identity_locked_compliance_cycle,
    'To what extent do low-income applicants internalize verification burden as legitimate cost of receiving benefits (identity lock to compliance identity) versus viewing it as external barrier?',
    'Qualitative research on applicant framing of verification requirements; comparison of exit capacity with actual exit attempts; analysis of appeal rates and dispute engagement',
    'If identity locked: change applicants'' exit_options from trapped to identity_locked, potentially changing classification for some perspectives. If purely trapped: current classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_compliance_cycle, empirical, 'Degree of identity lock versus material trapping in welfare verification burden').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(welfare_eligibility_verification, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(welfare_tr_t0, welfare_eligibility_verification, theater_ratio, 0, 0.48).
narrative_ontology:measurement(welfare_tr_t10, welfare_eligibility_verification, theater_ratio, 10, 0.58).
narrative_ontology:measurement(welfare_tr_t20, welfare_eligibility_verification, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(welfare_be_t0, welfare_eligibility_verification, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(welfare_be_t10, welfare_eligibility_verification, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(welfare_be_t20, welfare_eligibility_verification, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(welfare_eligibility_verification, enforcement_mechanism).
narrative_ontology:affects_constraint(welfare_eligibility_verification, means_tested_benefit_access).
narrative_ontology:affects_constraint(welfare_eligibility_verification, administrative_burden_poverty_trap).
narrative_ontology:affects_constraint(welfare_eligibility_verification, algorithmic_welfare_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(welfare_eligibility_verification, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
