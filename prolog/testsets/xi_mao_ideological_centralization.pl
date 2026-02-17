% ============================================================================
% CONSTRAINT STORY: xi_mao_ideological_centralization
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_xi_mao_ideological_centralization, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: xi_mao_ideological_centralization
 *   human_readable: Ideological Centralization and the Leadership Core
 *   domain: political
 *
 * SUMMARY:
 *   This constraint analyzes the structural centralization of power through ideological
 *   orthodoxy and the dismantling of institutional succession norms. It tracks the
 *   transition from Mao's revolutionary mass mobilization to Xi's bureaucratic,
 *   tech-driven "Leadership Core" model, which extracts loyalty and suppresses
 *   dissent.
 *
 * KEY AGENTS (by structural relationship):
 *   - Chinese Citizens: Primary target (powerless/trapped) — bears extraction of political autonomy.
 *   - CCP Leadership Core: Primary beneficiary (institutional/arbitrage) — benefits from consolidated power.
 *   - Party Cadres: Secondary actors (moderate/constrained) — enforcers who are also subjects of the system.
 *   - Political Analysts: Analytical observer — sees the full structure of the system.
 *
 * NARRATIVE ARC:
 *   Post-Mao, the system developed a "Rope" of collective leadership to prevent the
 *   return of personalistic rule. Under Xi, these constraints were dismantled,
 *   replaced by a personalistic Snare that presents itself as a "Mountain" of
 *   natural necessity for national survival, extracting total loyalty from the
 *   elite and suppressing autonomy from the population.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Extraction is 0.75 as informational pluralism is liquidated and political loyalty is mandated.
domain_priors:base_extractiveness(xi_mao_ideological_centralization, 0.75).
% Suppression is 0.85 due to the Great Firewall, social credit systems, and elimination of political opposition.
domain_priors:suppression_score(xi_mao_ideological_centralization, 0.85).
% High theater (0.82) from mandatory study apps ("Xuexi Qiangguo") and performative loyalty displays.
domain_priors:theater_ratio(xi_mao_ideological_centralization, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, extractiveness, 0.75).
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(xi_mao_ideological_centralization, theater_ratio, 0.82).

% --- Constraint claim (must match analytical perspective type) ---
% The analytical view, based on high extraction and suppression, is a Snare.
narrative_ontology:constraint_claim(xi_mao_ideological_centralization, snare).
narrative_ontology:human_readable(xi_mao_ideological_centralization, "Ideological Centralization and the Leadership Core").

% --- Binary flags ---
% Required for Tangled Rope classification of the Party Cadre perspective.
domain_priors:requires_active_enforcement(xi_mao_ideological_centralization).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(xi_mao_ideological_centralization, ccp_leadership_core).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(xi_mao_ideological_centralization, chinese_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CCP LEADERSHIP CORE (PRIMARY BENEFICIARY)
% For the architects, the system is a pure coordination tool (Rope) for
% maintaining stability and projecting power. Their arbitrage exit options
% and beneficiary status result in a negative effective extraction (χ).
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PARTY CADRE (ENFORCER/SUBJECT)
% For the mid-level enforcer, the system is a Tangled Rope. It provides a
% path to power and resources (coordination) but also subjects them to
% intense scrutiny and disciplinary inspection (extraction).
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE CITIZEN (PRIMARY TARGET)
% For the ordinary citizen, trapped by surveillance and censorship, the system
% is a Snare, extracting political autonomy and cognitive freedom with no
% viable alternative.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The analyst sees through the "Mountain of necessity" rhetoric. Based on the
% high base extraction and suppression, the objective classification is Snare.
constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(xi_mao_centralization_tests).

test(perspectival_gap) :-
    % Verify the gap between the beneficiary (Rope) and the target (Snare).
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
        context(agent_power(powerless), _, exit_options(trapped), _)).

test(analytical_claim_matches_metrics) :-
    % The analytical perspective must be a Snare, consistent with high ε and suppression.
    constraint_indexing:constraint_classification(xi_mao_ideological_centralization, snare,
        context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(xi_mao_ideological_centralization, snare).

test(claim_type_is_valid) :-
    % Verify the claim is within the allowed set required by the linter.
    narrative_ontology:constraint_claim(xi_mao_ideological_centralization, Claim),
    member(Claim, [mountain, rope, snare, tangled_rope, scaffold, piton]).

:- end_tests(xi_mao_centralization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high scores for base extractiveness (0.75) and suppression (0.85) are
 *   justified by the systematic nature of state control, including the Great
 *   Firewall, social credit systems, and the elimination of political alternatives.
 *   The high theater ratio (0.82) reflects the state's investment in performative
 *   loyalty, such as mandatory political study applications.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: the ruling elite experiences the system as a Rope for
 *   coordinating national policy and securing power. The citizen, however,
 *   experiences it as a Snare that strips them of autonomy. The Party cadre is
 *   caught in the middle, experiencing a Tangled Rope of benefit and coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: The 'ccp_leadership_core' directly benefits from the
 *     consolidation of power and suppression of dissent.
 *   - Victim: 'chinese_citizens' bear the cost through loss of political and
 *     informational freedom.
 *   This clear structural relationship drives the directionality calculations,
 *   leading to negative chi for the beneficiary and high positive chi for the victim.
 *
 * MANDATROPHY ANALYSIS:
 *   The system's classification as a Snare from the analytical perspective
 *   prevents the mislabeling of this coercive structure as a "Mountain" of
 *   historical necessity, which is its primary ideological defense. The framework
 *   correctly identifies that while it functions as a Rope for the powerful, its
 *   foundational metrics define it as an extractive Snare for its subjects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    xi_mao_extraction_intent,
    "Is the high extraction of political autonomy a functional necessity for national survival (Mountain) or a predatory choice for power maintenance (Snare)?",
    "Audit of institutional stability vs. resource allocation for surveillance vs. public welfare.",
    "If necessity: Mountain. If predatory choice: Snare.",
    confidence_without_resolution(medium)
).

omega_variable(
    digital_totalitarian_ceiling,
    "Can tech-driven ideological suppression maintain its profile indefinitely, or does it inevitably create 'Brittle Stability' that risks catastrophic failure?",
    "Long-term tracking of social credit non-compliance rates and underground informational markets.",
    "If sustainable: Stable Snare. If brittle: Risk of state failure.",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(xi_mao_ideological_centralization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional bureaucratic coordination (0.35)
% to high-theater "Study the Great Nation" performative loyalty (0.82).
narrative_ontology:measurement(xi_mao_tr_t0, xi_mao_ideological_centralization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(xi_mao_tr_t5, xi_mao_ideological_centralization, theater_ratio, 5, 0.60).
narrative_ontology:measurement(xi_mao_tr_t10, xi_mao_ideological_centralization, theater_ratio, 10, 0.82).

% Extraction: Tracking the intensification of cognitive/political extraction
% as the "Leadership Core" dismantled collective succession norms.
narrative_ontology:measurement(xi_mao_ex_t0, xi_mao_ideological_centralization, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(xi_mao_ex_t5, xi_mao_ideological_centralization, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(xi_mao_ex_t10, xi_mao_ideological_centralization, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is a core enforcement mechanism for the state's political model.
narrative_ontology:coordination_type(xi_mao_ideological_centralization, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */