% ============================================================================
% CONSTRAINT STORY: qwerty_persistence__incumbent_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence__incumbent_preservation_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence__incumbent_preservation_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Incumbent Preservation Reading)
 *   domain: technology_history/industrial_standards
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout,
 *   viewed through the lens of incumbent preservation. It argues that
 *   QWERTY's continued dominance is not solely due to its inherent
 *   coordination value, but is actively maintained by beneficiaries
 *   (manufacturers, typing schools, trained typists) who have sunk costs in
 *   the standard and suppress alternatives to protect their investments. This
 *   reading instantiates one perspective on the 'qwerty_persistence' kernel,
 *   focusing on the extractive and suppressive aspects of its maintenance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, 0.65).
domain_priors:suppression_score(qwerty_persistence__incumbent_preservation_reading, 0.7).
domain_priors:theater_ratio(qwerty_persistence__incumbent_preservation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence__incumbent_preservation_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence__incumbent_preservation_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence__incumbent_preservation_reading, "QWERTY Keyboard Layout Persistence (Incumbent Preservation Reading)").
narrative_ontology:topic_domain(qwerty_persistence__incumbent_preservation_reading, "technology_history/industrial_standards").

domain_priors:requires_active_enforcement(qwerty_persistence__incumbent_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence__incumbent_preservation_reading, '3346be9c-838c-4006-b204-eaba4b133050').
narrative_ontology:cs_kernel_codification('3346be9c-838c-4006-b204-eaba4b133050', implicit).
narrative_ontology:cs_authority_grounding('3346be9c-838c-4006-b204-eaba4b133050', extraction).
narrative_ontology:cs_interpretation_layer_present('3346be9c-838c-4006-b204-eaba4b133050').
narrative_ontology:cs_reading_relation('3346be9c-838c-4006-b204-eaba4b133050', qwerty_persistence__lapsed_alternatives_reading, coexists_with).
narrative_ontology:cs_axiom('3346be9c-838c-4006-b204-eaba4b133050', foundational, incumbent_capital_must_be_protected).
narrative_ontology:cs_axiom_status(incumbent_capital_must_be_protected, holdable).
narrative_ontology:cs_axiom_grounding('3346be9c-838c-4006-b204-eaba4b133050', incumbent_capital_must_be_protected, conventional).
narrative_ontology:cs_axiom('3346be9c-838c-4006-b204-eaba4b133050', secondary, switching_costs_are_natural_market_barriers).
narrative_ontology:cs_axiom_status(switching_costs_are_natural_market_barriers, holdable).
narrative_ontology:cs_axiom_grounding('3346be9c-838c-4006-b204-eaba4b133050', switching_costs_are_natural_market_barriers, conventional).
narrative_ontology:cs_reference_frame('3346be9c-838c-4006-b204-eaba4b133050', stable_market_dominance).
narrative_ontology:cs_drift_state('3346be9c-838c-4006-b204-eaba4b133050', contemporary_ergonomics_awareness, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3346be9c-838c-4006-b204-eaba4b133050', '').
narrative_ontology:cs_kernel_id(qwerty_persistence__incumbent_preservation_reading, qwerty_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, typing_schools).
narrative_ontology:constraint_beneficiary(qwerty_persistence__incumbent_preservation_reading, trained_typists).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_designers).
narrative_ontology:constraint_victim(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invested heavily in QWERTY tooling and supply chains. They actively resist adoption of alternative layouts to protect these sunk costs, using marketing and distribution channels to maintain QWERTY's dominance.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from a stable, widely adopted standard that simplifies curriculum development and ensures a large student base. They have little incentive to teach alternative layouts that lack market demand.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, typing_schools, beneficiary,
    organized, biographical, constrained, national).

% Have invested time and effort in mastering QWERTY. They resist switching to alternative layouts due to the retraining cost and the perceived loss of their existing skill, even if alternatives offer efficiency gains.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, trained_typists, beneficiary,
    moderate, biographical, identity_locked, local).

% Develop more efficient or ergonomic keyboard layouts (e.g., Dvorak, Colemak) but face immense barriers to adoption due to QWERTY's entrenched status and the active resistance from incumbents. They bear the cost of market exclusion.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, alternative_keyboard_designers, payer,
    powerless, generational, trapped, global).

% Are aware of the potential efficiency gains from alternative layouts but are constrained by the ubiquity of QWERTY hardware and the social cost of using a non-standard layout in shared environments. They pay in lost productivity.
narrative_ontology:constraint_stakeholder(qwerty_persistence__incumbent_preservation_reading, efficiency_seeking_users, payer,
    moderate, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, familiar keyboard layout that allows users to easily switch between different computers and collaborate without needing to learn new input methods.
% TRANSFER_FUNCTION: Transfers market dominance and associated profits from potential alternative keyboard designs to incumbent QWERTY manufacturers and training institutions, at the cost of efficiency and ergonomic improvements for users.
% ABSENT_VOICES: The collective voice of potential users who would benefit from more efficient layouts but are unaware of alternatives or lack the means to overcome switching costs. Their preferences are not aggregated into market demand for alternatives.
% DISAPPEARANCE_RATIONALE: If QWERTY's dominance vanished overnight, a rapid shift to more efficient or ergonomic layouts would occur, driven by user demand and competitive innovation from manufacturers. The entire keyboard industry and typing education would reorganize.
% FOUNDING_PROBLEM: The original problem was to design a mechanical typewriter layout that prevented typebars from jamming, a technical constraint of early typewriter mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology and ergonomic researchers widely corroborate that the original mechanical jamming problem is long dead with modern keyboard technology. The persistence is now attributed to path dependence and network effects, not the original technical constraint.
narrative_ontology:disappearance_verdict(qwerty_persistence__incumbent_preservation_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence__incumbent_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence__incumbent_preservation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence__incumbent_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence__incumbent_preservation_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence__incumbent_preservation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence__incumbent_preservation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence__incumbent_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high because the standard imposes significant switching costs and foregoes efficiency gains, effectively extracting from those who would benefit from alternatives. Suppression (0.7) is also high, reflecting the active efforts by incumbents to maintain market dominance through marketing, distribution control, and the inertia of training institutions. Theater ratio (0.2) is low because the coordination function (universal compatibility) is still real, but a significant portion of the effort goes into defending the status quo rather than purely improving coordination. The historical measurements show a clear trend of increasing extractiveness and suppression as QWERTY became more entrenched and its original technical justification faded.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of incumbent manufacturers and trained typists, QWERTY is a stable, beneficial standard that provides essential coordination. From the perspective of alternative designers and efficiency-seeking users, it is an outdated, inefficient standard whose persistence is enforced by market power and inertia, extracting costs from those who seek improvement.
 *
 * DIRECTIONALITY LOGIC:
 *   Keyboard manufacturers, typing schools, and trained typists are beneficiaries, as they profit from or are locked into the existing standard. Alternative keyboard designers and efficiency-seeking users are victims, bearing the costs of market exclusion and lost productivity. The 'agenda_setter' role for manufacturers highlights their active role in maintaining the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy: its original mandate (preventing typebar jams) is dead. The current persistence is driven by a new, unstated mandate of incumbent preservation. Classifying it as a Tangled Rope prevents mislabeling it as a pure Rope (which would ignore the extraction and suppression) or a Piton (which would understate the active defense by beneficiaries).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_suppression_vs_passive_inertia,
    'To what extent is QWERTY''s persistence due to active suppression by incumbents versus passive network effects and user inertia?',
    'Empirical studies on marketing spend by manufacturers to promote QWERTY, lobbying efforts against alternative standards, and the actual market penetration of alternative layouts in open-source or niche communities.',
    'If active suppression is dominant, the constraint is more Snare-like; if passive inertia, it leans more towards a Piton or a Rope with high switching costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_suppression_vs_passive_inertia, empirical, 'Distinguishing active enforcement from passive path dependence in QWERTY''s persistence.').

omega_variable(
    efficiency_gains_quantification,
    'What is the quantifiable efficiency gain (e.g., typing speed, error reduction, ergonomic benefit) of leading alternative layouts compared to QWERTY, and what is the economic cost of not adopting them?',
    'Large-scale, longitudinal studies comparing typing performance and health outcomes across different layouts, and economic modeling of productivity losses due to QWERTY''s inefficiencies.',
    'Higher quantifiable gains would strengthen the ''extraction'' argument, as the foregone benefits represent a clear cost to victims. Lower gains would weaken it, suggesting the ''cost'' is less significant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficiency_gains_quantification, empirical, 'Quantifying the real-world benefits of alternative keyboard layouts.').

omega_variable(
    incumbent_preservation_vs_coordination_value,
    'Is QWERTY''s primary function now incumbent preservation, or does it still provide genuine coordination value that outweighs the costs of alternatives?',
    'Analysis of manufacturer R&D spending: if R&D focuses on QWERTY-compatible innovations rather than exploring new layouts, it suggests preservation. Also, user surveys on perceived coordination benefits vs. desire for alternatives.',
    'If preservation is primary, the constraint is a Tangled Rope or Snare. If coordination value is primary, it leans towards a Rope, with the extraction being a necessary cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_preservation_vs_coordination_value, conceptual, 'The core conceptual ambiguity between this reading and the ''lapsed_alternatives_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence__incumbent_preservation_reading, 1878, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1878, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1878, 0.05).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence__incumbent_preservation_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1878, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1878, 0.1).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1950, 0.5).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence__incumbent_preservation_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1878, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1878, 0.1).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1920, 0.3).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence__incumbent_preservation_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence__incumbent_preservation_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence__incumbent_preservation_reading, lapsed_alternatives_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'incumbent_preservation_reading' of the 'qwerty_persistence' kernel. It focuses on active defense by beneficiaries. The 'lapsed_alternatives_reading' (constraint_qwerty_persistence__lapsed_alternatives_reading) focuses on coordination value and the failure of alternatives to reach critical mass. Both are linked as part of the 'qwerty_persistence' constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
