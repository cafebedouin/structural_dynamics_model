% ============================================================================
% CONSTRAINT STORY: antifragility_u3_exp_r1
% ============================================================================
% Version: 4.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2027-08-15
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u3_exp_r1, []).

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
 *   constraint_id: antifragility_u3_exp_r1
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes systems that gain from disorder, stressors, and
 *   volatility. This principle manifests differently depending on the
 *   observer's position within the system. For the species or an ecosystem
 *   over evolutionary time, it is an unchangeable law (Mountain). For an
 *   informed practitioner who can position themselves correctly, it is a tool
 *   for generating wealth (Rope). For the fragile subject optimized for
 *   stability, it is a predatory mechanism that transfers risk and harm to
 *   them (Snare). The overall human-level system that enables this is a
 *   Tangled Rope.
 *
 * KEY AGENTS:
 *   - Optimized Serfs (victims): Individuals or institutions optimized for efficiency and stability, making them fragile to shocks. [powerless/trapped]
 *   - Antifragile Practitioners (beneficiaries): Agents who adopt strategies (e.g., barbell) to harvest upside from volatility while capping downside. [moderate/arbitrage]
 *   - Central Planners / Fragilistas (enforcers): Institutions that attempt to suppress volatility, thereby creating hidden and more severe systemic risks. [institutional/constrained]
 *   - Evolutionary System (observer): The overarching complex system within which these dynamics play out. [analytical/analytical]
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u3_exp_r1, 0.75).
domain_priors:suppression_score(antifragility_u3_exp_r1, 0.65).
domain_priors:theater_ratio(antifragility_u3_exp_r1, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u3_exp_r1, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u3_exp_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u3_exp_r1, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u3_exp_r1, tangled_rope).
narrative_ontology:human_readable(antifragility_u3_exp_r1, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u3_exp_r1, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u3_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u3_exp_r1, antifragile_practitioner).
narrative_ontology:constraint_victim(antifragility_u3_exp_r1, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u3_exp_r1, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of an individual optimized for stability within a fragile system, antifragility is a mechanism that externalizes all costs of volatility onto them. They are trapped in a system that harvests their stability for others' gain.
constraint_indexing:constraint_classification(antifragility_u3_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For an agent who understands the principle and can structure their affairs (e.g., via a barbell strategy), antifragility is a pure coordination tool to align with volatility and extract upside. Exit options are high, and the constraint is a source of subsidy.
constraint_indexing:constraint_classification(antifragility_u3_exp_r1, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% An institution attempting to manage risk and ensure stability sees both the coordination function (smoothing out small shocks) and the asymmetric extraction (catastrophic failure when a large shock occurs). They are constrained by their mandate to prevent volatility, which paradoxically creates it.
constraint_indexing:constraint_classification(antifragility_u3_exp_r1, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a sufficiently long-term and detached perspective, the process of gaining from disorder is a fundamental, unchangeable law of complex adaptive systems. It is the mechanism of evolution itself, appearing as a Mountain.
constraint_indexing:constraint_classification(antifragility_u3_exp_r1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u3_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u3_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u3_exp_r1, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u3_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u3_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high base extractiveness (ε=0.75) represents the 'convexity bias'—the systematic transfer of value from those who are harmed by volatility to those who benefit from it. The high suppression (0.65) reflects how modern economic and social systems enforce optimization for short-term stability, which eliminates the redundancies and buffers that protect against shocks, leaving no alternative to being fragile.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The victim sees a Snare (pure extraction). The beneficiary sees a Rope (a beneficial tool). The institutional manager sees a Tangled Rope (a flawed coordination system). The long-term analyst sees a Mountain (a law of nature). This gap arises because the costs and benefits of volatility are asymmetrically distributed across agents and time horizons.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by exposure to volatility. The beneficiaries ('antifragile_practitioner') are structured to have positive, convex exposure, making d low. The victims ('optimized_serfs', 'fragile_institutions') have negative, concave exposure, making d high. The system actively transfers resources from the latter to the former during shocks.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying the overarching socio-economic implementation as a Tangled Rope resolves the mandatrophy of calling it a pure Snare or a pure Mountain. It acknowledges the genuine (though often misguided) coordination attempt by institutions to create stability, while also capturing the highly extractive, asymmetric outcomes that this attempt produces. It correctly identifies that the 'coordination' function is what creates the very fragility that is then exploited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_is_predatory_or_functional,
    'Is the extraction from fragile entities a necessary feature of evolutionary selection, or is it a predatory mechanism designed by antifragile agents?',
    'Comparative analysis of antifragility in natural vs. human-engineered systems, measuring the degree of externalized harm and intentionality of system design.',
    'If a functional necessity, the analytical perspective solidifies as Mountain. If primarily predatory, the institutional perspective shifts from Tangled Rope to Snare, as the 'coordination' function is revealed as pure theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_is_predatory_or_functional, conceptual, 'Whether the high extraction is a functional necessity of evolution or a predatory design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u3_exp_r1, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antifragility_tr_t0, antifragility_u3_exp_r1, theater_ratio, 0, 0.15).
narrative_ontology:measurement(antifragility_tr_t50, antifragility_u3_exp_r1, theater_ratio, 50, 0.35).
narrative_ontology:measurement(antifragility_tr_t100, antifragility_u3_exp_r1, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(antifragility_be_t0, antifragility_u3_exp_r1, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(antifragility_be_t50, antifragility_u3_exp_r1, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(antifragility_be_t100, antifragility_u3_exp_r1, base_extractiveness, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u3_exp_r1, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
