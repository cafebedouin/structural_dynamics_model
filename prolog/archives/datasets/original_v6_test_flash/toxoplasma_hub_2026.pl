% ============================================================================
% CONSTRAINT STORY: toxoplasma_hub_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_toxoplasma_hub_2026, []).

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
 *   constraint_id: toxoplasma_hub_2026
 *   human_readable: Toxoplasma Cyst as an Active Metabolic Hub
 *   domain: biological/medical
 *
 * SUMMARY:
 *   Recent single-cell RNA sequencing has revealed that Toxoplasma gondii
 *   cysts are not dormant but are active metabolic hubs. This constraint
 *   story examines the implications of this discovery for the host-parasite
 *   interaction, focusing on the metabolic activity of the cyst as a
 *   persistent source of extraction from the host organism and suppression of
 *   the host's immune system. The metabolic activity supports parasite
 *   survival but also creates a burden on the host.
 *
 * KEY AGENTS:
 *   - Toxoplasma Parasite: Primary beneficiary (institutional/arbitrage) - benefits from the cyst's metabolic activity for long-term survival and transmission.
 *   - Host Immune System: Primary victim (powerless/trapped) - actively suppressed by the parasite within the cyst.
 *   - Host Organism: Secondary victim (moderate/constrained) - bears the cost of chronic inflammation and potential organ damage due to the parasite's persistence.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(toxoplasma_hub_2026, 0.5).
domain_priors:suppression_score(toxoplasma_hub_2026, 0.6).
domain_priors:theater_ratio(toxoplasma_hub_2026, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(toxoplasma_hub_2026, extractiveness, 0.5).
narrative_ontology:constraint_metric(toxoplasma_hub_2026, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(toxoplasma_hub_2026, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(toxoplasma_hub_2026, tangled_rope).
narrative_ontology:human_readable(toxoplasma_hub_2026, "Toxoplasma Cyst as an Active Metabolic Hub").
narrative_ontology:topic_domain(toxoplasma_hub_2026, "biological/medical").

domain_priors:requires_active_enforcement(toxoplasma_hub_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(toxoplasma_hub_2026, toxoplasma_parasite).
narrative_ontology:constraint_victim(toxoplasma_hub_2026, host_immune_system).
narrative_ontology:constraint_victim(toxoplasma_hub_2026, host_organism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The host's immune system is trapped and actively suppressed by the parasite, with limited exit options and a narrow scope. The parasite's persistence within the cyst extracts resources and weakens the host's defenses.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% The host organism is constrained by the infection, but can mount partial immune responses and may receive medical treatment, providing limited exit options and a broader scope. The extraction manifests as chronic inflammation and potential organ damage, but there's also a degree of adaptation and resilience.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The Toxoplasma parasite benefits from the cyst's active metabolism, allowing it to persist within the host and increase its chances of transmission. Its exit options are arbitrage, leveraging the host's resources for its own propagation. The parasite benefits from this dynamic, classifying it as a rope.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% An analytical observer, looking at the long-term dynamics, sees that the parasite's metabolic hub strategy is a tangled rope: it benefits the parasite by facilitating long-term survival and transmission (coordination) but extracts resources from the host (asymmetric extraction), with consequences for the host's health and well-being.
constraint_indexing:constraint_classification(toxoplasma_hub_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(toxoplasma_hub_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(toxoplasma_hub_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(toxoplasma_hub_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(toxoplasma_hub_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(toxoplasma_hub_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.5): Moderate. The parasite actively extracts resources from the host to maintain the cyst's metabolic activity, contributing to chronic inflammation and potential organ damage. Suppression (0.6): Moderate-high. The parasite actively suppresses the host's immune system within the cyst, limiting the host's ability to clear the infection. Theater ratio (0.3): Low. The metabolic activity is primarily functional for parasite survival, with limited performative elements.
 *
 * PERSPECTIVAL GAP:
 *   The host immune system experiences the constraint as a snare due to its limited ability to escape or overcome the parasite's suppression. The host organism experiences a tangled rope, balancing the costs of infection with the potential for adaptation and partial immune control. The parasite experiences a rope, benefiting from the cyst's metabolic activity without bearing significant costs. The analytical observer sees the tangled rope, balancing coordination (parasite survival) and asymmetric extraction (host damage).
 *
 * DIRECTIONALITY LOGIC:
 *   The parasite benefits from the cyst's active metabolism (low directionality), while the host immune system and host organism bear the costs of suppression and resource extraction (high directionality). The perspectives reflect these structural relationships, with the parasite seeing a rope and the host seeing a snare or tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by recognizing the inherent tension between the parasite's survival and the host's health. The tangled rope classification captures the mixed coordination and extraction that characterize this interaction, preventing mislabeling as pure extraction or pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cyst_metabolic_activity_level,
    'How variable is the metabolic activity level within different Toxoplasma cysts, and what factors influence this variability?',
    'Comparative single-cell RNA sequencing of cysts from different host tissues and infection stages.',
    'If activity is highly variable: the constraint''s impact on the host is context-dependent. If activity is consistently high: the constraint is a more severe and predictable burden on the host.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cyst_metabolic_activity_level, empirical, 'Variability of metabolic activity in Toxoplasma cysts.').

omega_variable(
    immune_evasion_mechanisms,
    'What specific mechanisms does Toxoplasma employ to evade the host''s immune system within the cyst?',
    'In vitro studies of cyst-immune cell interactions, identifying secreted factors and surface molecules involved in immune suppression.',
    'If evasion is highly effective: the constraint persists and extracts over long time scales. If evasion is only partially effective: the constraint is more dynamic, with potential for immune clearance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immune_evasion_mechanisms, empirical, 'Mechanisms of Toxoplasma immune evasion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(toxoplasma_hub_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(toxo_tr_t0, toxoplasma_hub_2026, theater_ratio, 0, 0.1).
narrative_ontology:measurement(toxo_tr_t5, toxoplasma_hub_2026, theater_ratio, 5, 0.2).
narrative_ontology:measurement(toxo_tr_t10, toxoplasma_hub_2026, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(toxo_be_t0, toxoplasma_hub_2026, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(toxo_be_t5, toxoplasma_hub_2026, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(toxo_be_t10, toxoplasma_hub_2026, base_extractiveness, 10, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(toxoplasma_hub_2026, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
