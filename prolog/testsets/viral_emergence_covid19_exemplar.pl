% ============================================================================
% CONSTRAINT STORY: viral_emergence_covid19_exemplar
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-14
% ============================================================================

:- module(constraint_viral_emergence_covid19_exemplar, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: viral_emergence_covid19_exemplar
 *   human_readable: Societal Response to SARS-CoV-2 Emergence
 *   domain: social/political/health
 *
 * SUMMARY:
 *   Models the evolving "social response" constraint following the emergence
 *   of a novel virus, using COVID-19 as the exemplar. The constraint begins
 *   as the virus itself (a Snare), transforms into a collective action problem
 *   (a Scaffold), degrades into political theater (a Tangled Rope), and
 *   finally settles into institutional inertia (a Piton) as the virus
 *   becomes endemic.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Immunocompromised/Elderly: Primary targets of the viral constraint (powerless/trapped).
 *   - Public Health Officials: Beneficiaries of the coordination constraint (institutional/mobile).
 *   - The General Public: Bears the cost of the response constraint (moderate/constrained).
 *   - Epidemiologist: The analytical observer.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS) - Represents T=20 (Endemic State)
   ========================================================================== */

% --- Numerical metrics (End State: Endemicity) ---
domain_priors:base_extractiveness(viral_emergence_covid19_exemplar, 0.15). % Low-grade cost of living with the virus.
domain_priors:suppression_score(viral_emergence_covid19_exemplar, 0.30).   % Social pressure, but few formal rules remain.
domain_priors:theater_ratio(viral_emergence_covid19_exemplar, 0.85).       % High theater, e.g., "hygiene theater".

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, extractiveness, 0.15).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, theater_ratio, 0.85).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(viral_emergence_covid19_exemplar, tangled_rope).
narrative_ontology:human_readable(viral_emergence_covid19_exemplar, "Societal Response to SARS-CoV-2 Emergence").

% --- Binary flags ---
domain_priors:requires_active_enforcement(viral_emergence_covid19_exemplar). % The remnants of the response are institutionalized.
narrative_ontology:has_sunset_clause(viral_emergence_covid19_exemplar).      % The initial Scaffold phase had an implicit sunset.

% --- Structural relationships ---
% Beneficiary: The public health apparatus that grew around the response.
% Victim: Those left behind with chronic conditions or who still bear social costs.
narrative_ontology:constraint_beneficiary(viral_emergence_covid19_exemplar, public_health_bureaucracy).
narrative_ontology:constraint_victim(viral_emergence_covid19_exemplar, long_covid_sufferers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE IMMUNOCOMPROMISED (SNARE)
% For those with high vulnerability, the endemic virus remains a Snare.
% The "return to normal" for the majority suppresses alternatives (e.g., clean
% air standards, widespread masking) that would allow their safe participation
% in society, trapping them in a state of heightened risk.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: A CITIZEN DURING THE INITIAL LOCKDOWN (SCAFFOLD)
% The early, coordinated response is seen as a necessary, temporary Scaffold
% to "flatten the curve" and prevent hospital collapse.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, scaffold,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: A PUBLIC HEALTH OFFICIAL IN THE ENDEMIC PHASE (PITON)
% For an official maintaining the remnants of the response, the system is a Piton.
% Its original emergency function is gone, but it persists due to institutional
% inertia and theatrical displays of preparedness.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))) :-
    domain_priors:theater_ratio(viral_emergence_covid19_exemplar, TR), TR > 0.70.

% PERSPECTIVE 4: AN EPIDEMIOLOGIST (TANGLED ROPE)
% The analytical observer sees the entire arc as a Tangled Rope: a necessary
% coordination effort that became inextricably tangled with political extraction,
% misinformation, and permanent social costs.
constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(viral_emergence_covid19_exemplar_tests).

test(perspectival_shift_over_time) :-
    % This test confirms the logic for different phases/perspectives exists.
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, scaffold, context(agent_power(moderate), _, _, _)),
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, piton, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(viral_emergence_covid19_exemplar, tangled_rope, context(agent_power(analytical), _, _, _)).

test(final_state_is_piton) :-
    narrative_ontology:constraint_metric(viral_emergence_covid19_exemplar, theater_ratio, TR),
    TR > 0.7.

:- end_tests(viral_emergence_covid19_exemplar_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This story models the shifting nature of the "constraint" during a pandemic.
 *   - It begins as the VIRUS itself (a Snare, T=0-3).
 *   - The RESPONSE becomes the new constraint, initially a SCAFFOLD (T=4-8).
 *   - Politicization turns the response into a TANGLED_ROPE (T=9-14), with factions using it for their own ends.
 *   - Finally, it settles into an endemic PITON (T=15-20), where the bureaucracy and social rituals outlive the emergency.
 *   The base properties reflect the final, high-theater, low-extraction endemic state. The has_sunset_clause/1 fact
 *   refers to the temporary, emergency nature of the initial Scaffold phase.
 *
 * PERSPECTIVAL GAP:
 *   An agent's classification depends on their structural position and WHEN they are observing. A citizen in the early
 *   phase sees a temporary Scaffold. A late-stage official maintaining the bureaucracy sees a Piton. The analyst,
 *   viewing the whole arc, sees the Tangled Rope that connects them. Crucially, the immunocompromised (powerless/trapped)
 *   experience the endemic virus as a persistent Snare, as the societal move to "normalcy" suppresses the accommodations
 *   needed for their safety.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_covid_origin,
    'Was the virus a natural emergence (Mountain) or a lab leak (a man-made Snare)?',
    'Full, transparent access to epidemiological and genomic data from the initial outbreak zone.',
    'If Mountain, it is a recurring risk to be managed. If Snare, it represents a catastrophic failure of institutional containment with different policy implications.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(viral_emergence_covid19_exemplar, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% T=0-3: Outbreak (Snare) - High extraction from the virus itself
narrative_ontology:measurement(covid_ex_t0, viral_emergence_covid19_exemplar, base_extractiveness, 0, 0.90).
narrative_ontology:measurement(covid_th_t0, viral_emergence_covid19_exemplar, theater_ratio, 0, 0.05).
narrative_ontology:measurement(covid_su_t0, viral_emergence_covid19_exemplar, suppression_requirement, 0, 0.95).

% T=4-8: Coordinated Response (Scaffold) - Extraction is now the cost of the response, not the virus
narrative_ontology:measurement(covid_ex_t4, viral_emergence_covid19_exemplar, base_extractiveness, 4, 0.30).
narrative_ontology:measurement(covid_th_t4, viral_emergence_covid19_exemplar, theater_ratio, 4, 0.10).
narrative_ontology:measurement(covid_su_t4, viral_emergence_covid19_exemplar, suppression_requirement, 4, 0.85). % High suppression from lockdowns
narrative_ontology:measurement(covid_ex_t8, viral_emergence_covid19_exemplar, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(covid_th_t8, viral_emergence_covid19_exemplar, theater_ratio, 8, 0.25).
narrative_ontology:measurement(covid_su_t8, viral_emergence_covid19_exemplar, suppression_requirement, 8, 0.70).

% T=9-14: Politicization (Tangled Rope) - Metrics become noisy and contested
narrative_ontology:measurement(covid_ex_t9, viral_emergence_covid19_exemplar, base_extractiveness, 9, 0.45).
narrative_ontology:measurement(covid_th_t9, viral_emergence_covid19_exemplar, theater_ratio, 9, 0.50).
narrative_ontology:measurement(covid_su_t9, viral_emergence_covid19_exemplar, suppression_requirement, 9, 0.60).
narrative_ontology:measurement(covid_ex_t14, viral_emergence_covid19_exemplar, base_extractiveness, 14, 0.40).
narrative_ontology:measurement(covid_th_t14, viral_emergence_covid19_exemplar, theater_ratio, 14, 0.65).
narrative_ontology:measurement(covid_su_t14, viral_emergence_covid19_exemplar, suppression_requirement, 14, 0.50).

% T=15-20: Endemicity (Piton) - Emergency is over, inertia and theater remain
narrative_ontology:measurement(covid_ex_t15, viral_emergence_covid19_exemplar, base_extractiveness, 15, 0.20).
narrative_ontology:measurement(covid_th_t15, viral_emergence_covid19_exemplar, theater_ratio, 15, 0.80). % Theater spikes
narrative_ontology:measurement(covid_su_t15, viral_emergence_covid19_exemplar, suppression_requirement, 15, 0.40).
narrative_ontology:measurement(covid_ex_t20, viral_emergence_covid19_exemplar, base_extractiveness, 20, 0.15). % Final value
narrative_ontology:measurement(covid_th_t20, viral_emergence_covid19_exemplar, theater_ratio, 20, 0.85).   % Final value
narrative_ontology:measurement(covid_su_t20, viral_emergence_covid19_exemplar, suppression_requirement, 20, 0.30).   % Final value


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(viral_emergence_covid19_exemplar, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */