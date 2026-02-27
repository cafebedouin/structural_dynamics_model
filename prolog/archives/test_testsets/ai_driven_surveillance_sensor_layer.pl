% ============================================================================
% CONSTRAINT STORY: ai_driven_surveillance_sensor_layer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_driven_surveillance_sensor_layer, []).

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
 *   constraint_id: ai_driven_surveillance_sensor_layer
 *   human_readable: AI-Driven Real-Time Surveillance (Sensor Layer)
 *   domain: technological/security/political
 *
 * SUMMARY:
 *   This constraint models the 'sensor layer' of modern governance: the
 *   network of AI-driven cameras and sensors performing real-time facial
 *   recognition, gait analysis, and behavioral detection. Deployed under the
 *   rationale of public safety ('smart cities'), this system creates a
 *   profound structural conflict between security and autonomy. Its core
 *   function is the non-consensual extraction of biometric and behavioral
 *   data from the public, creating a permanent, searchable record of movement
 *   and association.
 *
 * KEY AGENTS:
 *   - State Security Agencies: Primary beneficiary (institutional/arbitrage) — gains unprecedented power for monitoring and social control.
 *   - Surveillance Technology Vendors: Primary beneficiary (powerful/mobile) — profits from the sale and maintenance of the system.
 *   - Marginalized Communities: Primary victim (powerless/trapped) — disproportionately misidentified and targeted by biased algorithms with no recourse.
 *   - General Public: Secondary victim (moderate/constrained) — loses privacy and autonomy in exchange for a claimed increase in security.
 *   - Civil Liberties Groups: Organized opposition (organized/constrained) — attempts to challenge the system through legal and political means.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_driven_surveillance_sensor_layer, 0.75).
domain_priors:suppression_score(ai_driven_surveillance_sensor_layer, 0.9).
domain_priors:theater_ratio(ai_driven_surveillance_sensor_layer, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, extractiveness, 0.75).
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(ai_driven_surveillance_sensor_layer, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_driven_surveillance_sensor_layer, snare).
narrative_ontology:human_readable(ai_driven_surveillance_sensor_layer, "AI-Driven Real-Time Surveillance (Sensor Layer)").
narrative_ontology:topic_domain(ai_driven_surveillance_sensor_layer, "technological/security/political").

domain_priors:requires_active_enforcement(ai_driven_surveillance_sensor_layer).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_driven_surveillance_sensor_layer, state_security_agencies).
narrative_ontology:constraint_beneficiary(ai_driven_surveillance_sensor_layer, surveillance_technology_vendors).
narrative_ontology:constraint_beneficiary(ai_driven_surveillance_sensor_layer, municipal_governments).
narrative_ontology:constraint_victim(ai_driven_surveillance_sensor_layer, general_public).
narrative_ontology:constraint_victim(ai_driven_surveillance_sensor_layer, marginalized_communities).
narrative_ontology:constraint_victim(ai_driven_surveillance_sensor_layer, political_dissidents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED COMMUNITY (SNARE) — Disproportionately targeted by biased algorithms and unable to exit the system. Experiences the constraint as pure, coercive extraction with no recourse. d≈0.95, f(d)≈1.42, σ=0.8 → χ≈0.85.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: STATE SECURITY AGENCY (ROPE) — Experiences the system as a pure coordination tool for public safety and threat neutralization. The agency is the primary beneficiary and has full control. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.09. Negative effective extraction signifies a net subsidy.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: AVERAGE CITIZEN (TANGLED ROPE) — Perceives a genuine coordination benefit (public safety) but also bears the cost of lost privacy and autonomy. Cannot easily opt-out, creating a hybrid experience. d≈0.85, f(d)≈1.15, σ=0.8 → χ≈0.69.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SNARE) — Sees the full structure: a coordination function (safety) used to justify a disproportionate, non-consensual, and coercive extraction of privacy and autonomy, with extremely high suppression of alternatives. The high ε and suppression values firmly place it in the Snare category. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈1.04.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNOLOGY VENDOR (TANGLED ROPE) — Provides a coordination tool for profit. Benefits from the system but is also subject to market and regulatory pressures. The product itself is a hybrid of function and extraction. d≈0.15, f(d)≈-0.01, σ=1.2 → χ≈-0.01.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CIVIL LIBERTIES UNION (SNARE) — As an organized opponent, this agent has more power than an individual but is still constrained by the system's legal and political entrenchment. It perceives the system as a pure Snare targeting fundamental rights. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.56. While χ is below the Snare threshold, the classification reflects the agent's structural opposition to a system with ε=0.75 and suppression=0.90.
constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_driven_surveillance_sensor_layer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_driven_surveillance_sensor_layer, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_driven_surveillance_sensor_layer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ai_driven_surveillance_sensor_layer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.75): Extremely high. The value extracted is the total loss of public anonymity and the creation of a comprehensive dataset for social control and prediction, a resource of immense value to the state. Suppression (0.90): Near-total. In a 'smart city,' it is physically impossible to opt out of surveillance in public spaces. Alternatives like masks are often flagged as suspicious, actively suppressing avoidance. Theater Ratio (0.30): Low. While there is a 'security theater' component, the system is brutally functional. Its primary purpose is active data collection and analysis, not just passive deterrence.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The state (beneficiary) perceives a pure Rope for coordinating public safety. The tech vendor sees a profitable Tangled Rope. The average citizen, weighing perceived safety against privacy loss, also sees a Tangled Rope. However, for marginalized communities (powerless victims), who are most likely to be misidentified and have no power to contest the system, it is an inescapable Snare. The analytical view aligns with the victim's, concluding that the system's coercive and suppressive nature makes it a Snare, where the 'public safety' rationale serves as a pretext for mass data extraction and control.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (State, Vendors) have arbitrage/mobile exit options, leading to low 'd' values and a perception of coordination (Rope/Tangled Rope). Victims (Public, Marginalized Groups) are constrained or trapped, leading to high 'd' values and a perception of extraction (Tangled Rope/Snare). The analytical observer's 'd' value is canonical, and when combined with the extremely high base extractiveness (ε=0.75) and global scope modifier (σ=1.2), the resulting effective extraction (χ) is well above the Snare threshold.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the stated mandate, 'public safety,' is used to justify a system whose primary structural effect is disproportionate and coercive extraction. The coordination function is real but serves as the vehicle for the extraction of a more fundamental good: privacy and autonomy. The system fails the test of a legitimate Tangled Rope because the extraction is not a side effect but the core purpose, and alternatives are systematically suppressed. Therefore, the analytical classification is Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_bias_reducibility,
    'Is the observed algorithmic bias against marginalized groups a correctable flaw or an inherent structural property of systems trained on historical data?',
    'Independent, transparent, third-party audits of training data and model performance across demographic groups. Longitudinal studies on the impact of bias mitigation techniques.',
    'If bias is correctable, the system might be re-classified as a severe Tangled Rope. If inherent, it confirms the Snare classification by structurally encoding discrimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_bias_reducibility, empirical, 'Whether algorithmic bias is a correctable flaw or inherent property').

omega_variable(
    chilling_effect_magnitude,
    'To what extent does the presence of pervasive surveillance suppress legitimate political dissent, free association, and expression?',
    'Sociological surveys measuring self-censorship; analysis of protest attendance and online speech patterns in surveilled vs. non-surveilled areas.',
    'A high measured chilling effect solidifies the Snare classification by demonstrating extraction of political freedom. A low effect would weaken the Snare case, suggesting the primary extraction is data, not liberty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(chilling_effect_magnitude, empirical, 'The degree to which surveillance suppresses legitimate dissent').

omega_variable(
    crime_reduction_causality,
    'What is the actual causal impact of AI surveillance on crime rates, versus the claimed deterrent effect (security theater)?',
    'Causal inference studies (e.g., regression discontinuity) comparing crime rates in cities before and after deployment, controlling for other socioeconomic factors.',
    'If the causal link to crime reduction is strong, the ''coordination'' function is real, supporting a Tangled Rope view. If the link is weak or non-existent, the system is primarily a tool of social control, confirming the Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crime_reduction_causality, empirical, 'The causal link between AI surveillance and crime reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_driven_surveillance_sensor_layer, 2020, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ai_d_tr_t2020, ai_driven_surveillance_sensor_layer, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(ai_d_tr_t2023, ai_driven_surveillance_sensor_layer, theater_ratio, 2023, 0.3).
narrative_ontology:measurement(ai_d_tr_t2026, ai_driven_surveillance_sensor_layer, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(ai_d_be_t2020, ai_driven_surveillance_sensor_layer, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(ai_d_be_t2023, ai_driven_surveillance_sensor_layer, base_extractiveness, 2023, 0.7).
narrative_ontology:measurement(ai_d_be_t2026, ai_driven_surveillance_sensor_layer, base_extractiveness, 2026, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_driven_surveillance_sensor_layer, enforcement_mechanism).
narrative_ontology:affects_constraint(ai_driven_surveillance_sensor_layer, social_credit_systems).
narrative_ontology:affects_constraint(ai_driven_surveillance_sensor_layer, predictive_policing_algorithms).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
