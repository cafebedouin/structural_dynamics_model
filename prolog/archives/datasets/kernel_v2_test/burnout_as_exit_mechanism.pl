% ============================================================================
% CONSTRAINT STORY: burnout_as_exit_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burnout_as_exit_mechanism, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: burnout_as_exit_mechanism
 *   human_readable: Burnout as Exit Mechanism in Healthcare Workforce
 *   domain: health_workforce_economics/organizational_behavior/gender_labor
 *
 * SUMMARY:
 *   Burnout in healthcare workers — characterized by emotional exhaustion,
 *   depersonalization, and reduced sense of accomplishment — functions as a
 *   mechanism that converts workplace dissatisfaction into labor market exit.
 *   Approximately 50% of U.S. physicians report burnout symptoms, and burnout
 *   scores correlate strongly with exit within 2 years. The constraint
 *   exhibits different types depending on the worker's exit options: for
 *   mobile workers with transferable credentials, burnout is a coordination
 *   mechanism (Rope) that signals 'this workplace is extractive; reallocate
 *   to better employer.' For trapped workers with geographic, financial, or
 *   credential barriers, burnout becomes pure extraction (Snare) — the signal
 *   fires but cannot be acted upon, producing suffering without resolution.
 *   The mechanism is genuinely hybrid at population level (Tangled Rope)
 *   because exit option distribution is heterogeneous: roughly 40-60% of
 *   healthcare workers have meaningful exit options, while the remainder face
 *   structural barriers. The constraint is downstream of
 *   administrative_burden_extraction and patient_demand_escalation — these
 *   upstream constraints produce the workplace stressors that trigger
 *   burnout, while burnout-as-exit is the labor market's response mechanism.
 *   Theater ratio is low (0.22) because burnout measurement and exit behavior
 *   are genuinely functional — this is not performative compliance but real
 *   psychological distress producing real labor reallocation. Suppression has
 *   increased over the interval (0.25 → 0.35) as exit barriers have risen:
 *   credential requirements have intensified, geographic monopsony has
 *   increased in rural areas, and debt burdens have grown.
 *
 * KEY AGENTS:
 *   - Mobile Physician: Primary beneficiary (moderate/mobile) — burnout functions as coordination signal enabling exit to better workplace; experiences low extraction because exit is available
 *   - Trapped Rural Nurse: Primary victim (powerless/trapped) — burnout without exit options produces maximum extraction; geographic isolation and monopsony employer create structural trap
 *   - Mid-Career Specialist: Mixed position (moderate/constrained) — burnout coordinates exit for those who can afford high switching costs while extracting from those who cannot; genuinely hybrid experience
 *   - Healthcare System Administrator: Institutional beneficiary (institutional/arbitrage) — burnout-driven exit disciplines extractive employers and reallocates labor to better organizations; net beneficiary of market clearing mechanism
 *   - Physician Union Coalition: Organized agents (organized/constrained) — building collective voice alternatives to individual exit; sees mechanism as transitional with sunset as bargaining power grows
 *   - Labor Economist: Analytical observer (analytical/analytical) — sees burnout-as-exit as coordination mechanism solving information asymmetry in labor markets; low extraction at civilizational scale despite localized harm to trapped agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burnout_as_exit_mechanism, 0.28).
domain_priors:suppression_score(burnout_as_exit_mechanism, 0.35).
domain_priors:theater_ratio(burnout_as_exit_mechanism, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burnout_as_exit_mechanism, extractiveness, 0.28).
narrative_ontology:constraint_metric(burnout_as_exit_mechanism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(burnout_as_exit_mechanism, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burnout_as_exit_mechanism, rope).
narrative_ontology:human_readable(burnout_as_exit_mechanism, "Burnout as Exit Mechanism in Healthcare Workforce").
narrative_ontology:topic_domain(burnout_as_exit_mechanism, "health_workforce_economics/organizational_behavior/gender_labor").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burnout_as_exit_mechanism, healthcare_workers_with_exit_options).
narrative_ontology:constraint_beneficiary(burnout_as_exit_mechanism, labor_market_efficiency).
narrative_ontology:constraint_victim(burnout_as_exit_mechanism, healthcare_workers_without_exit_options).
narrative_ontology:constraint_victim(burnout_as_exit_mechanism, patient_care_continuity).
narrative_ontology:constraint_vindicates(burnout_as_exit_mechanism, exit_voice_loyalty_framework).
narrative_ontology:constraint_vindicates(burnout_as_exit_mechanism, job_demands_resources_model).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MOBILE PHYSICIAN (ROPE) — Burnout functions as a coordination mechanism that converts diffuse workplace dissatisfaction into legible exit signal. The physician with transferable credentials and geographic mobility experiences burnout as information: 'this workplace is extractive; I should leave.' Low extraction because exit is available and burnout accelerates the decision rather than trapping the agent.
constraint_indexing:constraint_classification(burnout_as_exit_mechanism, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: TRAPPED RURAL NURSE (SNARE) — Burnout without exit options becomes pure extraction. Geographic isolation, family ties, credential non-portability, and local monopsony employer create structural trap. Burnout signals 'leave' but exit is unavailable — the mechanism produces suffering without resolution. Maximum extraction because the signal cannot be acted upon.
constraint_indexing:constraint_classification(burnout_as_exit_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MID-CAREER SPECIALIST (TANGLED ROPE) — Burnout coordinates exit for those who can afford it while extracting from those who cannot. The specialist with subspecialty training, practice equity, and family obligations faces high exit costs but not impossibility. Burnout functions as both signal (coordination) and penalty (extraction) — the mechanism is genuinely hybrid.
constraint_indexing:constraint_classification(burnout_as_exit_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: HEALTHCARE SYSTEM (ROPE) — Burnout-driven exit is a labor market clearing mechanism. High-burnout environments lose workers to low-burnout competitors, creating pressure for workplace improvement. The system benefits from this coordination: burnout signals which organizations are failing and reallocates labor accordingly. Net beneficiary because the mechanism disciplines extractive employers.
constraint_indexing:constraint_classification(burnout_as_exit_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PHYSICIAN UNION (SCAFFOLD) — Organized labor sees burnout-as-exit as a temporary coordination failure with a sunset: collective bargaining, staffing ratios, duty-hour limits, and workplace protections are building structural alternatives to individual exit. The mechanism is transitional — as collective voice strengthens, individual exit becomes less necessary as the primary feedback signal.
constraint_indexing:constraint_classification(burnout_as_exit_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, burnout-as-exit is a coordination mechanism that solves the information asymmetry problem in labor markets: workers have private information about workplace quality that employers and regulators cannot directly observe. Burnout converts private suffering into public signal (exit), enabling market discipline. Low extraction at this scale because the mechanism is genuinely functional for labor market efficiency, though it produces localized extraction for trapped agents.
constraint_indexing:constraint_classification(burnout_as_exit_mechanism, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burnout_as_exit_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burnout_as_exit_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burnout_as_exit_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(burnout_as_exit_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-moderate. The mechanism produces genuine psychological harm (emotional exhaustion, depersonalization) but this harm is functional for labor market coordination — it converts private information about workplace quality into public signal (exit). For workers with exit options, the extraction is minimal because burnout accelerates a decision they can act on. For trapped workers, extraction is severe because the signal fires without resolution. Population-level extractiveness reflects the heterogeneous distribution of exit options (roughly 40-60% mobile, 40-60% constrained or trapped). The value has increased modestly over the interval (0.18 → 0.28) as exit barriers have risen and workplace stressors have intensified. Suppression (0.35): Moderate. Exit barriers include geographic immobility (family ties, rural isolation), credential non-portability (specialty training, state licensing), financial constraints (practice equity, debt burden, visa status), and monopsony market structure (single dominant employer in region). Suppression has increased over the interval as these barriers have intensified. Theater ratio (0.22): Low. Burnout measurement (Maslach Burnout Inventory) and exit behavior are genuinely functional — workers report real distress and act on it when able. Some performative element exists (burnout as socially acceptable exit justification, strategic reporting during contract negotiation) but the mechanism is primarily functional rather than theatrical. Theater has increased slightly as burnout discourse has become institutionalized and workers have learned to frame exit in burnout terms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how exit options transform the same mechanism from coordination to extraction. The mobile physician sees Rope — burnout is information enabling better job match. The trapped nurse sees Snare — burnout is suffering without resolution. The mid-career specialist sees Tangled Rope — burnout coordinates for some while extracting from others. The healthcare system sees Rope — burnout-driven exit disciplines bad employers. The union sees Scaffold — the mechanism is transitional as collective voice builds. The analytical observer sees Rope at civilizational scale — the mechanism solves information asymmetry despite localized harm. No perspective is wrong — each reflects the agent's structural position. The population-level classification (Tangled Rope) reflects heterogeneous exit option distribution rather than uniform experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by exit options and beneficiary/victim status. Mobile workers with exit options are beneficiaries — burnout functions as coordination signal they can act on, producing low or negative effective extraction. Trapped workers without exit options are victims — burnout signal fires but cannot be acted upon, producing maximum effective extraction. Constrained workers with high-cost exit options experience intermediate extraction — the mechanism coordinates for those who can afford switching costs while extracting from those who cannot. The healthcare system is a beneficiary because burnout-driven exit disciplines extractive employers and reallocates labor efficiently. The analytical observer sees low extraction at civilizational scale because the mechanism is genuinely functional for labor market efficiency, though it produces localized harm. Gender asymmetry likely exists (omega variable) — women may face systematically higher exit barriers due to caregiving obligations, specialty segregation, and harassment, making the mechanism more extractive for women even at the same burnout level.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that burnout-as-exit is simultaneously coordination and extraction depending on exit option distribution. For workers who can leave, burnout is a coordination mechanism that improves job match quality — it converts diffuse dissatisfaction into legible exit signal, enabling labor market discipline of extractive employers. For workers who cannot leave, burnout is pure extraction — the signal fires but cannot be acted upon, producing psychological harm without resolution. The mechanism is not 'really' one or the other — it is genuinely both, and the classification depends on the observer's structural position. The analytical observer's Rope classification at civilizational scale is not a false summit — at that scale, the mechanism genuinely coordinates labor reallocation and improves market efficiency. But this coordination function coexists with severe localized extraction for trapped agents. The Tangled Rope classification at population level reflects this irreducible hybridity: the same mechanism coordinates and extracts simultaneously because exit option distribution is heterogeneous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_option_distribution,
    'What proportion of healthcare workers have genuine exit options versus being structurally trapped?',
    'Empirical survey of exit barriers: geographic mobility constraints, credential portability, family obligations, debt burden, visa status, monopsony market structure. Stratify by specialty, geography, gender, and career stage.',
    'If >70% have exit options: burnout is primarily coordination (Rope from most perspectives). If <30% have exit options: burnout is primarily extraction (Snare from most perspectives). Current estimate ~40-60% have meaningful exit options, placing the constraint in the Tangled Rope zone at population level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_distribution, empirical, 'Distribution of genuine exit options across healthcare workforce').

omega_variable(
    gender_exit_asymmetry,
    'Does burnout-as-exit function differently for women versus men due to differential exit barriers (caregiving obligations, specialty segregation, harassment)?',
    'Gender-stratified analysis of burnout prevalence, exit rates, and post-exit outcomes. Control for specialty, career stage, and family structure. Measure whether women experience higher burnout but lower exit rates (trapped) or similar exit rates with different destinations (constrained).',
    'If women face systematically higher exit barriers: the mechanism is more extractive for women (gender-differentiated directionality). If exit barriers are symmetric: the mechanism''s coordination function is gender-neutral. Preliminary evidence suggests asymmetry (women report higher burnout but lower exit rates in some specialties).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_exit_asymmetry, empirical, 'Gender asymmetry in exit barriers and burnout-exit correlation').

omega_variable(
    collective_voice_substitution,
    'Can collective bargaining and workplace protections substitute for individual exit as a feedback mechanism, or is exit irreducibly necessary for labor market discipline?',
    'Comparative analysis of healthcare systems with strong collective bargaining (unionized hospitals, European national health systems) versus exit-dependent systems (U.S. private practice). Measure workplace quality improvements, burnout prevalence, and exit rates. Test whether voice and exit are complements or substitutes.',
    'If voice substitutes for exit: Scaffold perspective is correct — burnout-as-exit has a genuine sunset as collective mechanisms mature. If voice and exit are complements: the mechanism persists even with strong unions, and Scaffold perspective is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_voice_substitution, empirical, 'Whether collective voice can substitute for individual exit').

omega_variable(
    burnout_measurement_validity,
    'Do self-reported burnout scores measure genuine psychological distress or strategic signaling of exit intent?',
    'Validation studies comparing self-reported burnout (Maslach Burnout Inventory) with objective measures (cortisol levels, sleep disruption, cognitive performance) and behavioral outcomes (actual exit, reduced hours, specialty switching). Test whether burnout scores predict exit independent of job satisfaction and whether scores change strategically during exit negotiation.',
    'If burnout is genuine distress: the mechanism''s extraction component is real psychological harm. If burnout is partly strategic signaling: the mechanism is more purely coordinative (workers use burnout framing to justify exit). Likely mixed: genuine distress that also functions as legible exit justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burnout_measurement_validity, empirical, 'Validity of burnout measurement as distress versus signal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burnout_as_exit_mechanism, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burnout_theater_2000, burnout_as_exit_mechanism, theater_ratio, 0, 0.15).
narrative_ontology:measurement(burnout_theater_2005, burnout_as_exit_mechanism, theater_ratio, 5, 0.17).
narrative_ontology:measurement(burnout_theater_2010, burnout_as_exit_mechanism, theater_ratio, 10, 0.19).
narrative_ontology:measurement(burnout_theater_2015, burnout_as_exit_mechanism, theater_ratio, 15, 0.21).
narrative_ontology:measurement(burnout_theater_2020, burnout_as_exit_mechanism, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(burnout_extract_2000, burnout_as_exit_mechanism, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(burnout_extract_2005, burnout_as_exit_mechanism, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(burnout_extract_2010, burnout_as_exit_mechanism, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(burnout_extract_2015, burnout_as_exit_mechanism, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(burnout_extract_2020, burnout_as_exit_mechanism, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(burnout_suppress_2000, burnout_as_exit_mechanism, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(burnout_suppress_2010, burnout_as_exit_mechanism, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(burnout_suppress_2020, burnout_as_exit_mechanism, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burnout_as_exit_mechanism, resource_allocation).

% DUAL FORMULATION NOTE:
% Burnout-as-exit is downstream of administrative_burden_extraction and patient_demand_escalation. The upstream constraints produce workplace stressors (administrative load, patient volume, emotional demands); burnout-as-exit is the labor market's response mechanism. The upstream constraints have their own extractiveness values reflecting the direct burden on workers; burnout-as-exit has its own extractiveness reflecting the exit mechanism's functionality versus harm. The constraints are structurally distinct: upstream constraints could exist without producing burnout (if workers had infinite resilience), and burnout could exist without the specific upstream stressors (if other workplace factors produced it). They are linked because the upstream constraints are empirically the primary drivers of burnout prevalence in contemporary U.S. healthcare.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
