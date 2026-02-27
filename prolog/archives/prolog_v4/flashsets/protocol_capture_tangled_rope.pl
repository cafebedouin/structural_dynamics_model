% ============================================================================
% CONSTRAINT STORY: protocol_capture_tangled_rope
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_protocol_capture_tangled_rope, []).

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
 *   constraint_id: protocol_capture_tangled_rope
 *   human_readable: The Captured Commons (Embrace, Extend, Extinguish)
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The "embrace, extend, extinguish" strategy describes a scenario where a
 *   dominant entity adopts an open protocol, adds proprietary extensions that
 *   lock users in, and ultimately makes the original protocol obsolete. This
 *   constraint examines the dynamics of this process, focusing on the
 *   relationships between the dominant entity, the open source community, and
 *   downstream developers.
 *
 * KEY AGENTS:
 *   - Dominant Entity: Primary beneficiary (institutional/arbitrage) - captures market share and extracts value through proprietary extensions
 *   - Open Source Community: Primary victim (powerless/trapped) - suffers as the dominant entity fragments the ecosystem and renders the original implementation obsolete
 *   - Downstream Developers: Secondary victim (moderate/constrained) - become dependent on the dominant platform and subject to its terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(protocol_capture_tangled_rope, 0.65).
domain_priors:suppression_score(protocol_capture_tangled_rope, 0.7).
domain_priors:theater_ratio(protocol_capture_tangled_rope, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, extractiveness, 0.65).
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(protocol_capture_tangled_rope, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(protocol_capture_tangled_rope, tangled_rope).
narrative_ontology:human_readable(protocol_capture_tangled_rope, "The Captured Commons (Embrace, Extend, Extinguish)").
narrative_ontology:topic_domain(protocol_capture_tangled_rope, "technological/economic").

domain_priors:requires_active_enforcement(protocol_capture_tangled_rope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(protocol_capture_tangled_rope, dominant_entity).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, open_source_community).
narrative_ontology:constraint_victim(protocol_capture_tangled_rope, downstream_developers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The open source community initially benefits from the wider adoption of the protocol but ultimately suffers as the dominant entity's extensions fragment the ecosystem and render the original implementation obsolete. They are trapped because switching costs are high and network effects favor the dominant platform.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Downstream developers are initially attracted to the platform due to its reach and potential for monetization. However, they become constrained by the platform's lock-in effects and the risk of being out-competed by the dominant entity's first-party extensions. They benefit from the initial reach but are ultimately subject to extraction.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% The dominant entity benefits from embracing the open protocol as it allows them to leverage existing community efforts and network effects. They then extend the protocol with proprietary features to create lock-in and extract value from the ecosystem. They have arbitrage options because they control the platform and can dictate its evolution.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational perspective, the embrace, extend, extinguish strategy represents a mixed bag. It can lead to faster innovation and wider adoption in the short term, but it also poses a threat to the long-term viability of open standards and decentralized ecosystems. The net effect is tangled — coordination followed by extraction.
constraint_indexing:constraint_classification(protocol_capture_tangled_rope, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(protocol_capture_tangled_rope_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(protocol_capture_tangled_rope, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(protocol_capture_tangled_rope, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(protocol_capture_tangled_rope, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(protocol_capture_tangled_rope_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): The dominant entity captures significant value from the ecosystem through its proprietary extensions and control over the platform. Suppression (0.70): The dominant entity actively suppresses alternatives through lock-in effects, compatibility issues, and aggressive marketing tactics. Theater ratio (0.30): The dominant entity does engage in some genuine coordination efforts, such as providing developer tools and hosting community events, but these activities are largely performative and serve to mask the underlying extraction.
 *
 * PERSPECTIVAL GAP:
 *   The open source community views the situation as a snare, as they are trapped in a system where their original contributions are co-opted and exploited. Downstream developers experience a tangled rope, as they benefit from the platform's reach but are also subject to its control. The dominant entity, of course, sees the situation as a rope, as they are able to coordinate the ecosystem and extract value from it. The analytical observer recognizes the mixed nature of the situation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the structural position of each agent. The dominant entity has arbitrage options and benefits from the ecosystem, so it experiences low or negative effective extraction. The open source community is trapped and bears the full cost of the dominant entity's actions, so it experiences maximum extraction. Downstream developers are constrained and experience a mixed bag, so they experience moderate extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This scenario resolves the mandatrophy by highlighting the different perspectives of the various actors involved. The dominant entity may genuinely believe that it is coordinating the ecosystem and providing value to users, but the open source community and downstream developers experience the situation as a form of extraction. The analytical observer recognizes the conflicting perspectives and the mixed nature of the situation. The proper classification is the presheaf over all observation sites, not any single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_threshold,
    'What level of switching costs makes it infeasible for users and developers to migrate away from the dominant platform?',
    'Analysis of user behavior and platform adoption rates; surveys of developers and businesses regarding switching costs',
    'If low: Open source alternative has a viable path for survival. If high: Extinguishment is inevitable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_threshold, empirical, 'Switching cost threshold for platform lock-in').

omega_variable(
    ecosystem_fragmentation,
    'To what extent do the dominant entity''s proprietary extensions fragment the ecosystem and create compatibility issues with the original open standard?',
    'Technical analysis of platform APIs and implementation details; measurement of ecosystem diversity and fragmentation',
    'If low: Ecosystem remains relatively unified and interoperable. If high: Open standard is effectively abandoned in favor of the dominant platform''s proprietary extensions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecosystem_fragmentation, empirical, 'The extent of ecosystem fragmentation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(protocol_capture_tangled_rope, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prot_tr_t0, protocol_capture_tangled_rope, theater_ratio, 0, 0.1).
narrative_ontology:measurement(prot_tr_t5, protocol_capture_tangled_rope, theater_ratio, 5, 0.2).
narrative_ontology:measurement(prot_tr_t10, protocol_capture_tangled_rope, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(prot_be_t0, protocol_capture_tangled_rope, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(prot_be_t5, protocol_capture_tangled_rope, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(prot_be_t10, protocol_capture_tangled_rope, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(protocol_capture_tangled_rope, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
