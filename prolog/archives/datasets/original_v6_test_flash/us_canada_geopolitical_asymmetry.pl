% ============================================================================
% CONSTRAINT STORY: us_canada_geopolitical_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_canada_geopolitical_asymmetry, []).

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
 *   constraint_id: us_canada_geopolitical_asymmetry
 *   human_readable: The Geopolitical Constraint of US Proximity on Canadian Sovereignty
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The vast and structural power imbalance between the United States and
 *   Canada imposes a permanent constraint on Canadian strategic autonomy.
 *   This asymmetry manifests in various forms, including economic dependence,
 *   cultural influence, and security alignment. While Canada benefits from
 *   its relationship with the US, it also experiences limitations on its
 *   ability to pursue independent policies and protect its cultural
 *   distinctiveness.
 *
 * KEY AGENTS:
 *   - US Strategic Interests: Primary beneficiary (institutional/arbitrage) - Benefits from Canada's resources, cooperation, and alignment with US foreign policy objectives.
 *   - Canadian Strategic Autonomy: Primary victim (powerless/trapped) - Constrained by US influence and limited ability to deviate from US policies.
 *   - Canadian Cultural Sovereignty: Secondary victim (powerless/trapped) - Vulnerable to cultural and economic influence from the US.
 *   - Canadian Federal Government: Balancing actor (moderate/constrained) - Navigates a complex relationship, seeking to maximize benefits while preserving some degree of autonomy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_canada_geopolitical_asymmetry, 0.6).
domain_priors:suppression_score(us_canada_geopolitical_asymmetry, 0.5).
domain_priors:theater_ratio(us_canada_geopolitical_asymmetry, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(us_canada_geopolitical_asymmetry, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_canada_geopolitical_asymmetry, tangled_rope).
narrative_ontology:human_readable(us_canada_geopolitical_asymmetry, "The Geopolitical Constraint of US Proximity on Canadian Sovereignty").
narrative_ontology:topic_domain(us_canada_geopolitical_asymmetry, "geopolitical").

domain_priors:requires_active_enforcement(us_canada_geopolitical_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_canada_geopolitical_asymmetry, us_strategic_interests).
narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, canadian_strategic_autonomy).
narrative_ontology:constraint_victim(us_canada_geopolitical_asymmetry, canadian_cultural_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CANADIAN STRATEGIC AUTONOMY (SNARE) - Canada, due to its geographic proximity and economic dependence, is often constrained in its ability to pursue strategic objectives that diverge significantly from those of the United States. The 'trapped' exit option reflects the practical limitations on Canada's ability to fully distance itself from US influence.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CANADIAN FEDERAL GOVERNMENT (TANGLED ROPE) - The Canadian government navigates a complex relationship, benefiting from economic ties and security cooperation with the US, while also experiencing constraints on its policy choices due to US influence. It has limited mobility and must balance competing interests.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US STRATEGIC INTERESTS (ROPE) - The US benefits from a stable and cooperative neighbor, facilitating trade, security, and resource access. The relationship is viewed as a coordination mechanism to achieve shared strategic goals, with the US able to pursue its interests with a high degree of latitude. The arbitrage exit option reflects the US's ability to pursue its interests through various channels.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) - From a long-term, global perspective, the US-Canada relationship is a complex interplay of coordination and extraction. The US benefits from Canada's resources and cooperation, while Canada experiences constraints on its sovereignty and strategic autonomy. The 'analytical' exit option reflects the ability to observe and analyze the structural dynamics of the relationship.
constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_canada_geopolitical_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_canada_geopolitical_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_canada_geopolitical_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_canada_geopolitical_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. The US extracts strategic and economic benefits from Canada due to the power asymmetry. Canada's policy choices are often influenced or constrained by US interests, reducing its strategic autonomy. Suppression (0.50): Moderate. Canada faces significant pressure to align with US policies, but it retains some degree of freedom to pursue its own interests. Theater ratio (0.30): Low. The US-Canada relationship is characterized by a relatively low level of performative activity, with a focus on practical cooperation and mutual benefit.
 *
 * PERSPECTIVAL GAP:
 *   Canadian strategic autonomy (Snare) experiences the constraint as a restriction on its ability to pursue independent policies. The Canadian government (Tangled Rope) navigates a complex relationship, balancing benefits and constraints. US strategic interests (Rope) views the relationship as a coordination mechanism to achieve shared goals. The analytical observer (Tangled Rope) sees a complex interplay of coordination and extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The US benefits from Canada's resources and cooperation (low d), while Canada experiences constraints on its sovereignty and strategic autonomy (high d). The Canadian government occupies an intermediate position, benefiting from economic ties while also facing limitations on its policy choices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    degree_of_canadian_policy_divergence,
    'To what extent can Canadian policies diverge from those of the US without incurring significant economic or political costs?',
    'Empirical analysis of historical instances of policy divergence and their consequences.',
    'Determines the degree of actual strategic autonomy Canada possesses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(degree_of_canadian_policy_divergence, empirical, 'The tolerance for policy divergence between Canada and the US.').

omega_variable(
    strength_of_canadian_national_identity,
    'How resilient is Canadian national identity to cultural and economic influence from the US?',
    'Sociological studies and cultural indicators measuring the distinctiveness of Canadian culture.',
    'Indicates the vulnerability of Canadian cultural sovereignty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strength_of_canadian_national_identity, empirical, 'Resilience of Canadian national identity to US influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_canada_geopolitical_asymmetry, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_canada_geopolitical_asymmetry, theater_ratio, 0, 0.2).
narrative_ontology:measurement(us_c_tr_t50, us_canada_geopolitical_asymmetry, theater_ratio, 50, 0.3).
narrative_ontology:measurement(us_c_tr_t100, us_canada_geopolitical_asymmetry, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_canada_geopolitical_asymmetry, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_c_be_t50, us_canada_geopolitical_asymmetry, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(us_c_be_t100, us_canada_geopolitical_asymmetry, base_extractiveness, 100, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_canada_geopolitical_asymmetry, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
