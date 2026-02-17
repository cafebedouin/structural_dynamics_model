% ============================================================================
% CONSTRAINT STORY: russian_war_cannibalization
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_russian_war_cannibalization, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: russian_war_cannibalization
 * human_readable: Russian Military Cannibalization of Civilian Infrastructure
 * domain: political/economic
 * * SUMMARY:
 * This constraint models the "Invisible Mandatrophy" in Russia, where the state's
 * mandate to prosecute the war in Ukraine forces a massive reallocation of
 * resources from the civilian economy to the military-industrial complex. This
 * "Military Keynesianism" provides a veneer of prosperity (e.g., high wages for
 * soldiers) while systematically extracting the maintenance and investment
 * margin from public utilities and civil aviation. The result is a
 * "Cannibalization Economy" where parts are stripped from aircraft and
 * infrastructure funds are diverted, creating a Snare that becomes visible
 * during physical failure events like heating grid collapses or aviation incidents.
 * * KEY AGENTS:
 * - The Kremlin/Siloviki: Beneficiary (Institutional) - Prioritizes the war mandate over civilian welfare.
 * - Regional Residents: Victim (Powerless) - Face collapsing infrastructure (e.g., heating failures in winter).
 * - Analytical Observer: Auditor (Analytical) - Observes the structural decay beneath the war economy's surface.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: High extraction (0.80); the 2025 budget redirects ~40% of all
% federal spending to defense and security, cannibalizing funds for housing,
% communal services, and human capital.
domain_priors:base_extractiveness(russian_war_cannibalization, 0.80).
% Rationale: High suppression (0.75); any effort to redirect funds back to
% civilian priorities is suppressed by the "existential war" narrative and
% active censorship.
domain_priors:suppression_score(russian_war_cannibalization, 0.75).
% Rationale: Low theater (0.15); the extraction is highly functional and
% direct, not performative. The system is efficiently reallocating resources,
% albeit with destructive long-term consequences.
domain_priors:theater_ratio(russian_war_cannibalization, 0.15).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(russian_war_cannibalization, extractiveness, 0.80).
narrative_ontology:constraint_metric(russian_war_cannibalization, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(russian_war_cannibalization, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% The state frames this as a necessary coordination effort for national survival.
narrative_ontology:constraint_claim(russian_war_cannibalization, tangled_rope).
narrative_ontology:human_readable(russian_war_cannibalization, "Russian Military Cannibalization of Civilian Infrastructure").

% Binary flags
% Requires active enforcement via anti-protest laws, state media control,
% and military censorship to suppress dissent over failing infrastructure.
domain_priors:requires_active_enforcement(russian_war_cannibalization).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(russian_war_cannibalization, military_industrial_complex).
narrative_ontology:constraint_beneficiary(russian_war_cannibalization, front_line_contractors).
narrative_ontology:constraint_victim(russian_war_cannibalization, civilian_infrastructure_users).
narrative_ontology:constraint_victim(russian_war_cannibalization, domestic_aviation_passengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE REGIONAL RESIDENT (SNARE)
% For a citizen in a provincial city, the policy is a Snare. The "stimulus"
% of the war has choked the basic survival infrastructure of their hometown,
% leaving them trapped with failing utilities during harsh winters.
constraint_indexing:constraint_classification(russian_war_cannibalization, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE KREMLIN (ROPE)
% The state views the "War Economy" as a Rope—a functional coordination
% mechanism that stimulates economic activity and unifies the population.
% They treat the extraction of civilian margins as a manageable trade-off.
constraint_indexing:constraint_classification(russian_war_cannibalization, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An analyst sees both the coordination function (resource mobilization for
% the war effort) and the severe, asymmetric extraction from the civilian
% sphere. It requires active enforcement to maintain, fitting the Tangled Rope profile.
constraint_indexing:constraint_classification(russian_war_cannibalization, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russian_war_cannibalization_tests).

test(perspectival_gap_is_rope_vs_snare) :-
    constraint_indexing:constraint_classification(russian_war_cannibalization, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(russian_war_cannibalization, snare,
        context(agent_power(powerless), _, _, _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(russian_war_cannibalization, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(high_extraction_and_suppression_are_set) :-
    narrative_ontology:constraint_metric(russian_war_cannibalization, extractiveness, E),
    narrative_ontology:constraint_metric(russian_war_cannibalization, suppression_requirement, S),
    E >= 0.80,
    S >= 0.75.

:- end_tests(russian_war_cannibalization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system undergoing "mandatrophy"—where a central mandate
 * (the war) consumes the margins necessary for the system's own long-term survival.
 * The base extractiveness (0.80) and suppression (0.75) are high, reflecting the
 * non-negotiable diversion of national resources.
 *
 * The Perspectival Gap is stark:
 * - The Kremlin (Institutional) sees a successful Rope, coordinating the nation for victory.
 * - The Citizen (Powerless) experiences a Snare, as their immediate safety and
 *   quality of life are sacrificed.
 * - The Analyst sees a Tangled Rope, acknowledging the coordination function but
 *   recognizing it is inseparable from coercive, asymmetric extraction that
 *   requires active state enforcement to sustain. This classification prevents
 *   the system from mislabeling the functional coordination as either pure
 *   extraction (Snare) or benign coordination (Rope), capturing the hybrid nature.
 * [RESOLVED MANDATROPHY]: The high extraction is identified as a structural
 * mandate, not a simple policy choice, resolving the ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_rwc_threshold,
    'At what point does the cannibalization of civilian systems (aviation, utilities) trigger a cascading failure that threatens regime stability, transforming the perceived Rope into a systemic Snare even for the elite?',
    'Monitoring the rate of major infrastructure failures and aviation incidents against internal polling data on public discontent.',
    'If failures plateau, the Tangled Rope holds. If they accelerate and correlate with unrest, the constraint is approaching a terminal state.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russian_war_cannibalization, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified significantly after 2022. The data models the
% sharp increase in extraction as the "special military operation" transitioned
% into a full-scale war economy, while theater remained low and functional.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(rwc_tr_t0, russian_war_cannibalization, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rwc_tr_t5, russian_war_cannibalization, theater_ratio, 5, 0.12).
narrative_ontology:measurement(rwc_tr_t10, russian_war_cannibalization, theater_ratio, 10, 0.15).

% Extraction over time (sharp increase):
narrative_ontology:measurement(rwc_ex_t0, russian_war_cannibalization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rwc_ex_t5, russian_war_cannibalization, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(rwc_ex_t10, russian_war_cannibalization, base_extractiveness, 10, 0.80).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint is a form of top-down resource allocation, a key coordination type.
narrative_ontology:coordination_type(russian_war_cannibalization, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */