% ============================================================================
% CONSTRAINT STORY: family_succession_system
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_family_succession_system, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: family_succession_system
 * human_readable: Meiji-Taisho Family Succession and the "Ie" System
 * domain: social/familial
 * * SUMMARY:
 * Based on Jun'ichirō Tanizaki's "Atsumono", this constraint models the rigid "Ie" (House) system of early 20th century Japan. The requirement for an adopted son-in-law to maintain a family lineage collides with individual desires, functioning as an immutable force (Mountain) to those within it. This drives the protagonist toward a self-destructive cycle of decadence as a failed exit strategy.
 * * KEY AGENTS:
 * - Tachibana Sōichi: The subject, a young man whose future is extracted by the system (Powerless).
 * - Sōichi's Mother (O-shina): An enforcer of the system, seeing it as necessary for social order (Institutional).
 * - The 'Ie' Lineage: The abstract beneficiary of the system's continuity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(family_succession_system, 0.8). % The system extracts the total future and happiness of individuals to ensure the abstract continuity of the "House".
domain_priors:suppression_score(family_succession_system, 0.9).   % Alternatives (e.g., elopement) result in social/economic death, making them effectively impossible.
domain_priors:theater_ratio(family_succession_system, 0.1).       % The system is brutally functional and direct, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(family_succession_system, extractiveness, 0.8).
narrative_ontology:constraint_metric(family_succession_system, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(family_succession_system, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The system presents itself as a natural, unchangeable part of the social order.
narrative_ontology:constraint_claim(family_succession_system, tangled_rope).
narrative_ontology:human_readable(family_succession_system, "Meiji-Taisho Family Succession and the \"Ie\" System").

% Binary flags
domain_priors:requires_active_enforcement(family_succession_system). % Requires active enforcement by family heads.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(family_succession_system, ie_lineage). % The abstract House lineage
narrative_ontology:constraint_victim(family_succession_system, young_heirs). % e.g., Tachibana Sōichi, Miyoko

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (Sōichi) - MOUNTAIN
% For the youth, the "Ie" system is as unchangeable as a mountain. They cannot
% negotiate with it; they can only collide with it and break.
constraint_indexing:constraint_classification(family_succession_system, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ENFORCER (Mother) - ROPE
% To the mother, the system is a functional tool (Rope) to ensure stability,
% prevent "shame," and guide the family safely through generations.
constraint_indexing:constraint_classification(family_succession_system, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - TANGLED ROPE
% An observer sees a system with a genuine coordination function (inheritance)
% achieved via extreme coercion and asymmetric extraction, a classic Tangled Rope.
constraint_indexing:constraint_classification(family_succession_system, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(family_succession_system_tests).

test(perspectival_gap) :-
    % Verify the gap between the powerless (Mountain) and institutional (Rope) views.
    constraint_indexing:constraint_classification(family_succession_system, tangled_rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_succession_system, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(family_succession_system, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that the conditions for a Tangled Rope classification are met.
    narrative_ontology:constraint_beneficiary(family_succession_system, _),
    narrative_ontology:constraint_victim(family_succession_system, _),
    domain_priors:requires_active_enforcement(family_succession_system).

test(extraction_threshold) :-
    % Verify the system is correctly identified as highly extractive.
    domain_priors:base_extractiveness(family_succession_system, E),
    E >= 0.46.

:- end_tests(family_succession_system_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a social system that is brutally effective at its stated goal (lineage preservation) at an extreme human cost.
 * - Extractiveness (0.8): The system extracts the entire life-path and autonomy of individuals to serve the abstract 'Ie' (House).
 * - Suppression (0.9): Social and legal structures of the Meiji/Taisho era made alternatives like elopement a form of social suicide, hence non-viable.
 * - Perspectival Gap: The core conflict is between the institutional view (Mother), which sees the system as a necessary 'Rope' for social order (dōri), and the powerless view (Sōichi), which experiences it as an immutable, life-destroying 'Mountain'.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.8) is not a measurement error but the central feature of the constraint. The analytical classification as a 'Tangled Rope' resolves the potential mandatrophy by acknowledging the system's dual nature. It does perform a genuine coordination function (managing succession and inheritance, benefiting the abstract 'ie_lineage'), but this function is inseparable from its coercive, asymmetric extraction from individuals (the victims). This prevents misclassification as a pure 'Snare' (which would ignore the coordination role) or a 'Rope' (which would ignore the immense human cost). The system is a tool of order that functions by crushing dissent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_miyoko_will,
    "To what extent did Miyoko, the betrothed, possess the will to resist her parents' decision?",
    "Analysis of her letters vs. contemporary accounts of female agency.",
    "If she was a willing participant, the constraint shifts from Mountain to Rope for her index.",
    confidence_without_resolution(medium)
).

omega_variable(
    omega_parental_collusion,
    "Was the sacrifice of Sōichi's happiness a pre-meditated act by the family elders?",
    "Investigation of private correspondence between the families.",
    "Determines if the constraint was a planned extraction (closer to Snare) or an emergent social friction (closer to Mountain).",
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(family_succession_system, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system was stable and rigid during the period in question. The values
% show no drift over the interval.
%
% Theater ratio over time:
narrative_ontology:measurement(fss_tr_t0, family_succession_system, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fss_tr_t5, family_succession_system, theater_ratio, 5, 0.1).
narrative_ontology:measurement(fss_tr_t10, family_succession_system, theater_ratio, 10, 0.1).

% Extraction over time:
narrative_ontology:measurement(fss_ex_t0, family_succession_system, base_extractiveness, 0, 0.8).
narrative_ontology:measurement(fss_ex_t5, family_succession_system, base_extractiveness, 5, 0.8).
narrative_ontology:measurement(fss_ex_t10, family_succession_system, base_extractiveness, 10, 0.8).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The system's primary coordination function is managing the transfer of
% assets, status, and responsibility across generations.
narrative_ontology:coordination_type(family_succession_system, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */