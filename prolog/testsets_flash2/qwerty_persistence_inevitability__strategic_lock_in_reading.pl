% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence: Strategic Lock-in Reading
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint story analyzes the persistence of the QWERTY keyboard
 *   layout as a result of strategic lock-in engineered by manufacturers,
 *   rather than a purely accidental path dependency. It argues that the
 *   dominance of QWERTY is maintained through active suppression of
 *   alternatives, training partnerships, and cartel standardization, which
 *   benefits manufacturers at the expense of typists' ergonomics and
 *   efficiency. This is one reading of the 'qwerty_persistence_inevitability'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.78).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.85).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence: Strategic Lock-in Reading").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '3fcb2639-c496-4b5a-b1fe-2565a39cd6ce').
narrative_ontology:cs_kernel_codification('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', implicit).
narrative_ontology:cs_authority_grounding('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', extraction).
narrative_ontology:cs_interpretation_layer_present('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce').
narrative_ontology:cs_reading_relation('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', foundational, manufacturer_strategic_action_drives_standardization).
narrative_ontology:cs_axiom_status(manufacturer_strategic_action_drives_standardization, holdable).
narrative_ontology:cs_axiom_grounding('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', manufacturer_strategic_action_drives_standardization, empirically_contingent).
narrative_ontology:cs_axiom('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', foundational, standardization_as_rent_extraction_mechanism).
narrative_ontology:cs_axiom_status(standardization_as_rent_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', standardization_as_rent_extraction_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', manufacturer_controlled_standardization).
narrative_ontology:cs_drift_state('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('3fcb2639-c496-4b5a-b1fe-2565a39cd6ce', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_modern).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_ergonomic_costs).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_tutors_and_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original cartel that standardized QWERTY, establishing training programs and market dominance. They actively suppressed Dvorak and other alternatives to maintain their installed base and extract rents from training and replacement parts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from the continued dominance of QWERTY, which simplifies manufacturing, reduces R&D costs for alternative layouts, and ensures a stable market for standard keyboards. They passively maintain the status quo through inertia and market power.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturers_modern, beneficiary,
    organized, biographical, constrained, global).

% Bear the ergonomic costs (e.g., carpal tunnel syndrome) and reduced typing efficiency associated with the QWERTY layout. They face high retraining barriers and a lack of widely available alternative hardware, making exit from QWERTY difficult.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_ergonomic_costs, payer,
    powerless, biographical, identity_locked, global).

% Develop more ergonomically efficient or specialized keyboard layouts (e.g., Dvorak, Colemak). They struggle to gain market share due to the entrenched QWERTY standard, high switching costs for users, and lack of institutional support.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers, payer,
    moderate, generational, constrained, global).

% Benefit from the established QWERTY curriculum and the demand for QWERTY training. They are incentivized to perpetuate the standard due to existing teaching materials and career paths.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_tutors_and_educators, beneficiary,
    moderate, biographical, constrained, local).

% Study the historical development and persistence of QWERTY, analyzing the role of strategic decisions, market power, and network effects in its dominance. They provide critical analysis of the constraint's origins and mechanisms.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, economic_historians_and_analysts, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal standard for keyboard layouts, enabling interoperability across devices and facilitating training for a large pool of typists.
% TRANSFER_FUNCTION: Transfers ergonomic and efficiency costs from manufacturers to typists, while transferring market dominance and reduced R&D costs to manufacturers.
% ABSENT_VOICES: The designers and proponents of more efficient keyboard layouts (e.g., Dvorak) were actively suppressed in the early 20th century and continue to be marginalized by market inertia and institutional resistance. Their voices would advocate for a more meritocratic or ergonomically driven standard.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing infrastructure would face immediate chaos, but over time, a more efficient and ergonomic standard would likely emerge, leading to a reorganization of keyboard manufacturing, training, and user habits.
% FOUNDING_PROBLEM: The original problem was to prevent typewriter keys from jamming by separating common letter pairs, and to establish a universal standard for typing instruction.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and ergonomic studies corroborate that the original jamming problem is long obsolete and the current persistence is driven by lock-in, not functional necessity. The coordination benefits are now outweighed by the costs for typists.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because typists bear significant ergonomic and efficiency costs without commensurate benefits, while manufacturers enjoy reduced R&D and simplified production. Suppression is very high (0.85) due to the active exclusion of alternative layouts from mainstream markets, the high switching costs for users, and the institutional inertia in training. Theater ratio is moderate (0.4) as the 'coordination' justification increasingly serves as cover for rent extraction, with less genuine functional benefit for users compared to alternatives. Accessibility collapse is high (0.75) because alternatives are effectively unavailable to most users, and resistance is low (0.3) due to the diffuse nature of the harm and the difficulty of organizing collective action against an entrenched standard.
 *
 * PERSPECTIVAL GAP:
 *   Manufacturers perceive QWERTY as a beneficial coordination standard that ensures interoperability and ease of training. Typists and alternative designers, however, experience it as an extractive constraint that imposes costs and limits choice. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The original 1893 cartel and modern keyboard manufacturers are clear beneficiaries (d near 0.0) as they profit from the standardized layout and lack of competition. Typists bearing ergonomic costs and alternative keyboard designers are clear victims (d near 1.0) due to the costs and barriers they face. Typing tutors are beneficiaries as their established curriculum is maintained. Economic historians are observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (preventing key jams) is long dead. Its persistence is now driven by the benefits it provides to manufacturers (reduced costs, market control) and the high switching costs for users, not by its original coordination function. This analysis prevents mislabeling it as a 'Rope' (genuine coordination) or 'Piton' (atrophied function with no concentrated beneficiary), instead identifying it as a 'Tangled Rope' where a coordination story covers ongoing extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_weight_of_strategic_action,
    'What is the precise causal weight of strategic manufacturer actions (cartelization, training partnerships) versus pure path dependency (network effects, switching costs) in QWERTY''s persistence?',
    'Counterfactual historical analysis: modeling what would have happened if early manufacturers had not actively suppressed alternatives, or if Dvorak had received institutional support.',
    'A higher causal weight for strategic action strengthens the ''Tangled Rope'' classification by emphasizing deliberate extraction; a lower weight might shift it towards a ''Piton'' or even ''Rope'' if the coordination function is genuinely dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_weight_of_strategic_action, empirical, 'Distinguishing active lock-in from passive path dependency.').

omega_variable(
    ergonomic_cost_quantification,
    'What is the precise economic and health cost borne by typists due to QWERTY''s ergonomic inefficiencies, and how does it compare to the benefits of standardization?',
    'Large-scale epidemiological studies on typing-related injuries across different layouts, combined with economic modeling of productivity losses and retraining costs.',
    'Higher quantified costs for typists would increase the measured extractiveness and strengthen the ''Tangled Rope'' classification; lower costs might weaken the victim declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ergonomic_cost_quantification, empirical, 'Quantifying the hidden costs of QWERTY for typists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1893, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.1).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1920, 0.15).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.6).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1920, 0.65).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.7).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1920, 0.75).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1950, 0.8).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1980, 0.83).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2000, 0.84).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'qwerty_persistence_inevitability' kernel. It focuses on strategic lock-in, while the 'path_dependency_reading' emphasizes accidental historical factors. Both are linked as they describe different facets of the same phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
