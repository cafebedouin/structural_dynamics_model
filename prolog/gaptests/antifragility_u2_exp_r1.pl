% ============================================================================
% CONSTRAINT STORY: antifragility_u2_exp_r1
% ============================================================================
% Version: 7.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_exp_r1, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: antifragility_u2_exp_r1
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that gain from disorder, stressors, and
 *   volatility. This property manifests differently depending on an agent's
 *   position within the system. While it can be seen as a fundamental law of
 *   nature for complex systems (a Mountain), its application in social and
 *   economic contexts creates a stark divide between those who can harness it
 *   and those who are consumed by it.
 *
 * KEY AGENTS:
 *   - Optimized Serfs (Victim): Powerless individuals in rigid, optimized systems who bear the costs of volatility.
 *   - Antifragile Practitioner (Beneficiary): Agents with agency and resources (the 'barbell strategy') who can harvest upside from volatility while limiting downside.
 *   - Fragile Institutions (Victim/Enforcer): Bureaucracies and corporations that enforce stability, suppress volatility, and become catastrophically fragile as a result.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_exp_r1, 0.75).
domain_priors:suppression_score(antifragility_u2_exp_r1, 0.65).
domain_priors:theater_ratio(antifragility_u2_exp_r1, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_exp_r1, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_exp_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_exp_r1, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_exp_r1, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_exp_r1, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_exp_r1, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u2_exp_r1, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_exp_r1, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Experiences the downside of volatility without access to the upside. The system extracts resilience and resources from them under stress.
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Actively uses volatility for gain, coordinating personal resources to benefit from disorder. For this agent, it's a tool with negative effective extraction.
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Enforces policies that create short-term stability (coordination) but generate hidden, systemic fragility, leading to asymmetric extraction during crises.
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% Views antifragility as a fundamental, unchangeable property of complex adaptive systems, akin to natural selection. From this scope, the extraction is a feature of reality, not a policy.
constraint_indexing:constraint_classification(antifragility_u2_exp_r1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_exp_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (ε=0.75) represents the 'convexity bias' where beneficiaries capture unbounded upside from positive 'black swan' events, while the costs of negative events are socialized and borne by the victims. Suppression (0.65) is high because the fragile system is presented as the only rational choice, discouraging the adoption of more resilient, less 'optimal' strategies.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the practitioner with arbitrage, it's a Rope for navigating reality. For the trapped serf, it's a Snare that punishes them for the system's instability. For the long-term analytical observer, it's a Mountain—an inescapable feature of evolution. The institutional agent sees a Tangled Rope because they are trying to coordinate for stability while inadvertently enabling the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the practitioners who have the knowledge and capital to structure their affairs to gain from shocks. Victims are the individuals and institutions optimized for a narrow, predictable future, from whom resilience and resources are extracted when that future fails to materialize. The system actively transfers wealth and power from the fragile to the antifragile during crises.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that 'antifragility' is not a pure coordination good (Rope) or a pure natural law (Mountain) in human systems. The Tangled Rope classification correctly identifies that the coordination function (maintaining a stable, predictable economy) is coupled with a severe, asymmetric extraction mechanism that activates under stress. This prevents mislabeling a predatory system as a simple tool or a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antifragility_extraction_intent,
    'Is the high extraction a functional necessity for system evolution or a predatory feature engineered by specific agents?',
    'Audit of 'skin in the game' metrics. If beneficiaries bear proportional downside risk, it's functional. If downside is fully externalized to victims, it's predatory.',
    'If functional necessity: the analytical perspective shifts from Tangled Rope to Mountain. If predatory: it solidifies the Snare classification for victims and reveals the practitioner's Rope to be a deception.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antifragility_extraction_intent, empirical, 'Whether the high extraction is a functional necessity or predatory.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_exp_r1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anti_tr_t0, antifragility_u2_exp_r1, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anti_tr_t5, antifragility_u2_exp_r1, theater_ratio, 5, 0.35).
narrative_ontology:measurement(anti_tr_t10, antifragility_u2_exp_r1, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(anti_be_t0, antifragility_u2_exp_r1, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(anti_be_t5, antifragility_u2_exp_r1, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(anti_be_t10, antifragility_u2_exp_r1, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
