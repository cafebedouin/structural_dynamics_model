% ============================================================================
% CONSTRAINT STORY: nsl_hk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_hk, []).

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
 *   constraint_id: nsl_hk
 *   human_readable: Hong Kong National Security Law (NSL)
 *   domain: political/legal
 *
 * SUMMARY:
 *   The Hong Kong National Security Law (NSL) was imposed on Hong Kong by
 *   Beijing in 2020. It has significantly curtailed civil liberties and
 *   autonomy, leading to widespread concerns about human rights and the rule
 *   of law. The law criminalizes secession, subversion, terrorism, and
 *   collusion with foreign forces, with penalties including lengthy prison
 *   sentences. It also established new security agencies with broad powers,
 *   raising concerns about due process and accountability. The imposition of
 *   the NSL represents a major shift in the 'one country, two systems'
 *   framework that governed Hong Kong since its handover from British rule in
 *   1997.
 *
 * KEY AGENTS:
 *   - Central Government in Beijing: Primary beneficiary (institutional/arbitrage) – Gains increased control over Hong Kong.
 *   - Hong Kong Civil Society: Primary victim (powerless/trapped) – Suffers from curtailed freedoms and increased surveillance.
 *   - Pro-Democracy Movement in Hong Kong: Secondary victim (moderate/constrained) – Faces suppression and legal prosecution.
 *   - Hong Kong Business Community: Constrained actor (moderate/constrained) – Adapts to the new legal environment while navigating business risks.
 *   - International Community: Organized observer (organized/mobile) – Expresses concerns and imposes sanctions but also maintains economic ties.
 *   - Pro-Beijing Establishment in Hong Kong: Secondary beneficiary (institutional/constrained) – Reinforces political power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_hk, 0.75).
domain_priors:suppression_score(nsl_hk, 0.85).
domain_priors:theater_ratio(nsl_hk, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_hk, extractiveness, 0.75).
narrative_ontology:constraint_metric(nsl_hk, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nsl_hk, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_hk, snare).
narrative_ontology:human_readable(nsl_hk, "Hong Kong National Security Law (NSL)").
narrative_ontology:topic_domain(nsl_hk, "political/legal").

domain_priors:requires_active_enforcement(nsl_hk).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_hk, central_government_beijing).
narrative_ontology:constraint_beneficiary(nsl_hk, pro_beijing_establishment_hk).
narrative_ontology:constraint_victim(nsl_hk, hong_kong_civil_society).
narrative_ontology:constraint_victim(nsl_hk, pro_democracy_movement_hk).
narrative_ontology:constraint_victim(nsl_hk, independent_media_hk).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Hong Kong civil society faces severe restrictions on freedom of expression, assembly, and association under the NSL. Trapped due to the law's broad reach and enforcement mechanisms.
constraint_indexing:constraint_classification(nsl_hk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The Hong Kong business community benefits from stability but is constrained by the NSL's impact on international reputation and potential restrictions on business activities. Hybrid coordination and extraction.
constraint_indexing:constraint_classification(nsl_hk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% The central government in Beijing views the NSL as a tool for maintaining stability and national security. Experiences it as a coordination mechanism to ensure control.
constraint_indexing:constraint_classification(nsl_hk, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% The Hong Kong legal system, once independent, is now constrained by the NSL. The system is forced to implement the law, even if it conflicts with previous legal principles. High theater ratio due to performative alignment.
constraint_indexing:constraint_classification(nsl_hk, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% The international community experiences mixed effects. Some benefit from the economic relationship with Hong Kong but are also negatively impacted by the NSL's implications for human rights and democratic values. Can exert influence through sanctions and diplomatic pressure.
constraint_indexing:constraint_classification(nsl_hk, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical observer sees the NSL as a complex instrument with both coordination and extraction functions. Aimed at maintaining control but also undermines Hong Kong's autonomy and international standing.
constraint_indexing:constraint_classification(nsl_hk, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_hk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsl_hk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsl_hk, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_hk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nsl_hk, TR),
    TR >= 0.70.

:- end_tests(nsl_hk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75): High extraction due to the significant curtailment of freedoms and autonomy, particularly for pro-democracy voices and civil society. Suppression (0.85): High suppression due to the broad scope of the law and its strict enforcement, limiting avenues for dissent and opposition. Theater Ratio (0.75): High. There is significant performative alignment with legal standards, but the law's practical effect is primarily repressive.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives diverge significantly. The central government in Beijing views the NSL as a necessary tool for maintaining stability (rope). Hong Kong civil society experiences it as a snare, trapping them under restrictive laws. The international community sees a tangled rope, with both benefits from economic ties and concerns about human rights. The analytical observer views the NSL as a blend of control and undermining Hong Kong's autonomy (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   The central government benefits from increased control, resulting in low directionality. Hong Kong civil society bears the brunt of the law's restrictions, resulting in high directionality. The international community's directionality is intermediate due to mixed impacts.
 *
 * MANDATROPHY ANALYSIS:
 *   The NSL clearly leans towards extraction, despite claims of maintaining stability. The high extractiveness and suppression scores, combined with the perspectives of the powerless and the analytical observer, confirm its classification as a snare. The potential for coordination is overshadowed by the law's repressive effects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_national_security,
    'What activities legitimately constitute a threat to national security?',
    'Case law analysis, comparative legal studies, and expert opinions on international human rights standards.',
    'A narrow definition would mitigate the NSL''s impact on civil liberties. A broad definition would further suppress dissent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_national_security, conceptual, 'Determining the legitimate scope of national security concerns.').

omega_variable(
    enforcement_independence,
    'To what extent are law enforcement agencies and the judiciary independent from political influence in NSL cases?',
    'Monitoring court proceedings, analyzing judicial decisions, and assessing the transparency of law enforcement investigations.',
    'Greater independence would ensure fairer trials and protect defendants'' rights. Political influence would undermine the rule of law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_independence, empirical, 'Assessing the independence of law enforcement and the judiciary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_hk, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_hk, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl__tr_t2, nsl_hk, theater_ratio, 2, 0.5).
narrative_ontology:measurement(nsl__tr_t4, nsl_hk, theater_ratio, 4, 0.75).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_hk, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(nsl__be_t2, nsl_hk, base_extractiveness, 2, 0.65).
narrative_ontology:measurement(nsl__be_t4, nsl_hk, base_extractiveness, 4, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_hk, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
