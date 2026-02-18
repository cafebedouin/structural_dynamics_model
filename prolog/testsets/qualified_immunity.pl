% ============================================================================
% CONSTRAINT STORY: qualified_immunity
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_qualified_immunity, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: qualified_immunity
 * human_readable: Qualified Immunity Doctrine
 * domain: political/legal
 * * SUMMARY:
 * Qualified immunity is a U.S. legal doctrine shielding government officials from liability for constitutional violations unless their conduct violates "clearly established" law. In practice, this creates a "catch-22" where a right cannot be clearly established without a prior case involving nearly identical facts, effectively preventing remedies for victims of misconduct.
 * * KEY AGENTS:
 * - Civil Rights Plaintiff: Subject (Powerless), seeking redress for violations.
 * - State Official / Judiciary: Beneficiary (Institutional), protected from lawsuits and litigation floodgates.
 * - Legal Scholar: Auditor (Analytical), observing the structural effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(qualified_immunity, 0.8). % It extracts the right to a legal remedy from victims, even when a violation is found.
domain_priors:suppression_score(qualified_immunity, 0.7).   % The "clearly established" rule suppresses the creation of new legal precedents.
domain_priors:theater_ratio(qualified_immunity, 0.1).       % The doctrine is highly functional and not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(qualified_immunity, extractiveness, 0.8).
narrative_ontology:constraint_metric(qualified_immunity, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qualified_immunity, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The judiciary presents it as a necessary coordination tool to prevent chilling government action.
narrative_ontology:constraint_claim(qualified_immunity, tangled_rope).
narrative_ontology:human_readable(qualified_immunity, "Qualified Immunity Doctrine").
narrative_ontology:topic_domain(qualified_immunity, "political/legal").

% Binary flags
domain_priors:requires_active_enforcement(qualified_immunity). % Requires active judicial enforcement via summary judgment.

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(qualified_immunity, state_officials).
narrative_ontology:constraint_victim(qualified_immunity, civil_rights_plaintiffs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the victim, the doctrine is a Snare. They suffer a violation, yet the
% legal system uses a technicality to strangle their right to a remedy.
constraint_indexing:constraint_classification(qualified_immunity, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% From the bench or the perspective of a state official, the doctrine is a
% Rope, a necessary tool to prevent litigation from paralyzing government.
constraint_indexing:constraint_classification(qualified_immunity, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Analytically, it's a Tangled Rope. It has a genuine (if debatable)
% coordination function (protecting officials) but also imposes severe,
% asymmetric extraction on a specific class of victims, requiring active
% judicial enforcement to maintain.
constraint_indexing:constraint_classification(qualified_immunity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE PRACTITIONER (MOUNTAIN)
% For a civil rights attorney, it's a Mountain—an immovable feature of the
% legal landscape that makes most cases "dead on arrival."
constraint_indexing:constraint_classification(qualified_immunity, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_tests).

test(perspectival_gap_snare_rope) :-
    % Verify the core perspectival gap between the powerless and institutional views.
    constraint_indexing:constraint_classification(qualified_immunity, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qualified_immunity, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict by identifying it as a Tangled Rope.
    constraint_indexing:constraint_classification(qualified_immunity, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify that all three conditions for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(qualified_immunity, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(qualified_immunity, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(qualified_immunity).

test(high_extraction_and_suppression) :-
    domain_priors:base_extractiveness(qualified_immunity, E), E >= 0.46,
    domain_priors:suppression_score(qualified_immunity, S), S >= 0.6.

:- end_tests(qualified_immunity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect the dual nature of qualified immunity. The high base extractiveness (0.8) represents the near-total removal of legal remedy for victims. The high suppression (0.7) captures the "clearly established" clause's effect of preventing the law from evolving. The perspectival gap is stark: for beneficiaries (the state), it's a coordination tool (Rope) to ensure decisive action by officials. For victims (the powerless), it's a trap that denies justice (Snare).
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] The high extraction (0.8) could be misread as a pure Snare. However, the system's claim that it serves a coordination function is non-zero. The Tangled Rope classification from the analytical perspective correctly resolves this ambiguity. It acknowledges the coordination claim (beneficiary exists) while formally recognizing the severe asymmetric extraction (victim exists) and coercive structure (requires enforcement). This prevents the system from simplifying the constraint into either a pure tool of oppression (Snare) or a benign coordination mechanism (Rope), capturing its complex and contested nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_qualified_immunity,
    'Is the chilling effect on official conduct, which QI purports to prevent, a real and substantial threat or a pretext for shielding misconduct?',
    'Comparative analysis of official conduct and litigation rates in jurisdictions that have abolished or limited qualified immunity (e.g., Colorado).',
    'If the threat is real, the coordination aspect is stronger, leaning it towards a necessary (though harsh) Rope. If pretextual, it is almost purely a Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(qualified_immunity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the doctrine's evolution from a "good faith" defense
% to a near-absolute barrier, showing extraction accumulation.
% Theater ratio remains low as the mechanism is functionally potent.
%
% Theater ratio over time:
narrative_ontology:measurement(qi_tr_t0, qualified_immunity, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qi_tr_t5, qualified_immunity, theater_ratio, 5, 0.1).
narrative_ontology:measurement(qi_tr_t10, qualified_immunity, theater_ratio, 10, 0.1).

% Extraction over time:
narrative_ontology:measurement(qi_ex_t0, qualified_immunity, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(qi_ex_t5, qualified_immunity, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(qi_ex_t10, qualified_immunity, base_extractiveness, 10, 0.8).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% As a legal doctrine, it is a core enforcement mechanism of the state.
narrative_ontology:coordination_type(qualified_immunity, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */