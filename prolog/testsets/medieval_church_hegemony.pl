% ============================================================================
% CONSTRAINT STORY: medieval_church_hegemony
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_medieval_church_hegemony, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: medieval_church_hegemony
 * human_readable: The Medieval Ecclesiastical Hegemony
 * domain: religious/economic/political
 * * SUMMARY:
 * A comprehensive social and spiritual constraint system where the Church
 * controls the "means of salvation." It features mandatory tithing (a 10% tax),
 * Latin-only liturgy (blocking direct textual access), and the threat of
 * excommunication to enforce social and political compliance. The system
 * provides genuine coordination (social stability, administration, poor relief)
 * while simultaneously performing massive, asymmetric extraction.
 * * KEY AGENTS:
 * - The Peasantry & Dissenters: Subjects (Powerless) providing the economic base via tithes.
 * - The Clergy & Nobility: Beneficiaries (Institutional) who use the system for coordination and extraction.
 * - The Historian: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(medieval_church_hegemony, 0.70). % High: Mandatory 10% tithe + vast land ownership.
domain_priors:suppression_score(medieval_church_hegemony, 0.90).   % Very High: Vernacular scripture suppressed; heresy actively prosecuted.
domain_priors:theater_ratio(medieval_church_hegemony, 0.10).       % Low: The system was highly functional, not merely performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(medieval_church_hegemony, extractiveness, 0.70).
narrative_ontology:constraint_metric(medieval_church_hegemony, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(medieval_church_hegemony, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It presented itself as divine, unchangeable law.
narrative_ontology:constraint_claim(medieval_church_hegemony, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(medieval_church_hegemony). % Required for Tangled Rope (Inquisition, Interdicts, Tithe collectors).

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(medieval_church_hegemony, clergy_and_nobility).
narrative_ontology:constraint_victim(medieval_church_hegemony, peasantry_and_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the peasant or dissenter, the system is an inescapable trap. The tithe
% is extracted under threat of spiritual and temporal punishment.
% χ = 0.70 * 1.5 (powerless) * 0.8 (local) = 0.84. This is a clear Snare.
constraint_indexing:constraint_classification(medieval_church_hegemony, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For the clergy, the system is a pure coordination mechanism for maintaining
% social order, funding public works, and guiding souls to salvation.
% χ = 0.70 * -0.2 (institutional) * 1.0 (national) = -0.14. Negative extraction.
constraint_indexing:constraint_classification(medieval_church_hegemony, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The historian sees a system with both a genuine coordination function
% (social stability, administration) and massive asymmetric extraction. It
% requires active enforcement, making it a textbook Tangled Rope.
% χ = 0.70 * 1.15 (analytical) * 1.2 (global) = 0.966.
constraint_indexing:constraint_classification(medieval_church_hegemony, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(medieval_church_hegemony_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(medieval_church_hegemony, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(medieval_church_hegemony, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless = snare,
    TypeInstitutional = rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(medieval_church_hegemony, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(medieval_church_hegemony, _),
    narrative_ontology:constraint_victim(medieval_church_hegemony, _),
    domain_priors:requires_active_enforcement(medieval_church_hegemony).

:- end_tests(medieval_church_hegemony_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that was both functional and highly extractive.
 * Suppression (0.9) is based on the active prosecution of heresy and the restriction
 * of scripture to maintain clerical authority. Extractiveness (0.7) reflects the
 * mandatory 10% tithe and vast church landholdings. The low theater ratio (0.1)
 * indicates this was a working system, not just a performance of power.
 *
 * The Perspectival Gap is stark: the peasantry experiences an inescapable Snare,
 * while the institutional clergy views it as a necessary Rope for social coordination.
 * The analytical classification of Tangled Rope is critical, as it acknowledges
 * both realities simultaneously, preventing a simplistic "pure evil" or "pure good"
 * assessment.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] This constraint is a classic example of what Mandatrophy
 * is designed to detect. A purely extractive analysis (Snare) would miss the
 * genuine coordination functions (social stability, literacy, poor relief) that
 * gave the system its longevity and legitimacy. A purely coordination-focused
 * analysis (Rope) would ignore the immense, coercive extraction. The Tangled Rope
 * classification correctly identifies the hybrid nature of the constraint, where
 * a coordination mandate is coupled with an extractive apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_medieval_church_hegemony,
    'Was the system primarily a spiritual coordination mechanism that became extractive, or an extractive system that used spirituality as its control mechanism?',
    'Comparative analysis of Church budgets vs. charitable spending; tracing the evolution of canon law regarding tithes and property.',
    'If primarily spiritual, its degradation into a Snare is a story of institutional decay. If primarily extractive, it was a Snare from its inception, masquerading as a Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(medieval_church_hegemony, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of the Church's power.
% At T=0 (c. 1100), the system was still consolidating. By T=10 (c. 1400),
% its extractive and enforcement mechanisms were fully developed.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(mch_tr_t0, medieval_church_hegemony, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mch_tr_t5, medieval_church_hegemony, theater_ratio, 5, 0.08).
narrative_ontology:measurement(mch_tr_t10, medieval_church_hegemony, theater_ratio, 10, 0.10).

% Extraction over time (increases as tithes become more systematized):
narrative_ontology:measurement(mch_ex_t0, medieval_church_hegemony, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(mch_ex_t5, medieval_church_hegemony, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(mch_ex_t10, medieval_church_hegemony, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The Church provided the legal and moral framework for governance.
narrative_ontology:coordination_type(medieval_church_hegemony, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */