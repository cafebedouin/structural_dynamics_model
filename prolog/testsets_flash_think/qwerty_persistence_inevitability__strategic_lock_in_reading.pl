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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   as a result of deliberate manufacturer strategy and cartel
 *   standardization, leading to lock-in for typists and suppression of
 *   superior alternatives. It is a 'strategic lock-in' reading of the 'QWERTY
 *   persistence inevitability' kernel, emphasizing active rent extraction
 *   rather than accidental historical path dependency. The claimed type is
 *   'tangled_rope' because it provides a coordination function
 *   (standardization) but couples it with asymmetric extraction and active
 *   suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.75).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.8).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence: Strategic Lock-in Reading").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '78b0f04f-e739-43ed-a5c6-84da2525ac6f').
narrative_ontology:cs_kernel_codification('78b0f04f-e739-43ed-a5c6-84da2525ac6f', formalized).
narrative_ontology:cs_authority_grounding('78b0f04f-e739-43ed-a5c6-84da2525ac6f', extraction).
narrative_ontology:cs_reading_relation('78b0f04f-e739-43ed-a5c6-84da2525ac6f', qwerty_persistence_inevitability__path_dependency_reading, forecloses).
narrative_ontology:cs_axiom('78b0f04f-e739-43ed-a5c6-84da2525ac6f', foundational, qwerty_design_is_suboptimal_and_harmful).
narrative_ontology:cs_axiom_status(qwerty_design_is_suboptimal_and_harmful, holdable).
narrative_ontology:cs_axiom_grounding('78b0f04f-e739-43ed-a5c6-84da2525ac6f', qwerty_design_is_suboptimal_and_harmful, empirically_contingent).
narrative_ontology:cs_axiom('78b0f04f-e739-43ed-a5c6-84da2525ac6f', foundational, standardization_was_strategic_not_accidental).
narrative_ontology:cs_axiom_status(standardization_was_strategic_not_accidental, holdable).
narrative_ontology:cs_axiom_grounding('78b0f04f-e739-43ed-a5c6-84da2525ac6f', standardization_was_strategic_not_accidental, empirically_contingent).
narrative_ontology:cs_reference_frame('78b0f04f-e739-43ed-a5c6-84da2525ac6f', qwerty_as_optimal_standard).
narrative_ontology:cs_drift_state('78b0f04f-e739-43ed-a5c6-84da2525ac6f', contemporary_ergonomics_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('78b0f04f-e739-43ed-a5c6-84da2525ac6f', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_industry).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original group of manufacturers who standardized QWERTY, benefiting from reduced competition and control over the nascent typing industry. They actively promoted QWERTY through training partnerships and suppressed alternatives, extracting rents from its dominance.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefited from a standardized curriculum and the continuous demand for QWERTY training, which became a prerequisite for many office jobs. Their business model is tied to the persistence of the QWERTY standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_industry, beneficiary,
    organized, biographical, constrained, national).

% Bear the ergonomic costs (e.g., carpal tunnel syndrome) and efficiency losses associated with the QWERTY layout. They face high retraining barriers and limited access to alternative, more efficient keyboards, making them identity-locked into the dominant standard for professional reasons.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typists, payer,
    powerless, biographical, identity_locked, global).

% Develop more ergonomic and efficient keyboard layouts (e.g., Dvorak, Colemak) but face immense market entry barriers due to QWERTY's entrenched position, training infrastructure, and user expectations. Their innovations are largely suppressed by the existing standard.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers, excluded,
    moderate, biographical, constrained, global).

% Document the ergonomic disadvantages and efficiency deficits of the QWERTY layout compared to alternatives. They provide scientific evidence that challenges the perceived inevitability or optimality of QWERTY, but their findings often struggle to translate into market change.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, ergonomics_researchers, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a universal, standardized keyboard layout that facilitated mass production of typewriters and simplified training for typists across different manufacturers and regions.
% TRANSFER_FUNCTION: Transfers market dominance and sustained revenue streams to manufacturers and training institutions, while transferring ergonomic costs, efficiency losses, and retraining burdens to typists and suppressing innovation from alternative designers.
% ABSENT_VOICES: Alternative keyboard designers and typists advocating for ergonomic layouts were systematically excluded from the standardization processes and market adoption, their voices drowned out by the entrenched interests of manufacturers and the inertia of the installed base.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing ecosystem would undergo massive disruption and reorganization. While short-term productivity would plummet, it would likely accelerate the adoption of more ergonomic and efficient layouts, fundamentally altering keyboard design, manufacturing, and training paradigms.
% FOUNDING_PROBLEM: The original problem was to create a robust, standardized keyboard layout for early mechanical typewriters that prevented key jamming and facilitated efficient mass production and training.
% FOUNDING_PROBLEM_CORROBORATION: Ergonomics researchers and alternative keyboard designers corroborate that the original mechanical problem of key jamming is long dead with modern technology. However, legacy manufacturers and some training institutions might still claim 'compatibility' or 'familiarity' as a live problem, though this is largely contested by independent analysis.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) reflects the ongoing ergonomic costs and efficiency losses borne by typists, which translate into sustained benefits for the entrenched standard. Suppression (0.80) is high due to the active exclusion of alternative layouts from mainstream adoption through training partnerships, market inertia, and the sheer cost of switching. The theater ratio is low (0.10) because the constraint's persistence is functionally effective in maintaining market control, not merely performative. Accessibility collapse is high (0.70) because while alternatives exist and are known, the practical barriers to adopting them are substantial for most users.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the original manufacturers, QWERTY was a necessary coordination mechanism that solved early technical problems and facilitated market growth. From the perspective of typists and alternative designers, it became an extractive mechanism, locking them into an inferior standard for the benefit of entrenched interests. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The original typewriter manufacturers and the typing school industry are clear beneficiaries, profiting from the standardized training and market control. Typists are the primary victims, bearing the costs of an inefficient and ergonomically suboptimal design. Alternative keyboard designers are excluded, their innovations unable to penetrate the market due to the entrenched standard. Ergonomics researchers act as observers, documenting the costs without direct influence on the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strategic_vs_path_dependency,
    'Is QWERTY persistence primarily due to deliberate manufacturer-engineered lock-in, or is it an accident-driven path dependency without strategic beneficiaries?',
    'Historical analysis of corporate archives, patent filings, and industry meeting minutes from the late 19th and early 20th centuries to determine the extent of coordinated strategic action versus emergent, uncoordinated market dynamics.',
    'If resolved as primarily strategic lock-in, it strengthens the ''tangled_rope'' classification and highlights the role of active enforcement. If resolved as pure path dependency, it would shift the classification towards ''piton'' or ''rope'' (if coordination benefits were symmetric) by reducing the perceived extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_path_dependency, empirical, 'Distinguishing between deliberate strategy and accidental historical evolution as the primary driver of QWERTY''s persistence.').

omega_variable(
    true_ergonomic_cost,
    'What is the precise, quantifiable ergonomic cost (e.g., healthcare costs, lost productivity) borne by typists due to the QWERTY layout, and how does it compare to the benefits of standardization?',
    'Large-scale epidemiological studies on typist health outcomes, detailed biomechanical analyses of typing efficiency across layouts, and economic modeling of productivity differences.',
    'Higher quantifiable ergonomic costs would increase the measured extractiveness and strengthen the victim status of typists, reinforcing the ''tangled_rope'' classification. Lower costs would weaken the extraction claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_ergonomic_cost, empirical, 'Quantifying the health and efficiency costs of the QWERTY layout.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1893, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1893, 0.05).
narrative_ontology:measurement(qwer_tr_t1923, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1923, 0.07).
narrative_ontology:measurement(qwer_tr_t1953, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1953, 0.08).
narrative_ontology:measurement(qwer_tr_t1983, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1983, 0.09).
narrative_ontology:measurement(qwer_tr_t2003, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(qwer_tr_t2023, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2023, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1893, 0.5).
narrative_ontology:measurement(qwer_be_t1923, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1923, 0.6).
narrative_ontology:measurement(qwer_be_t1953, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1953, 0.68).
narrative_ontology:measurement(qwer_be_t1983, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1983, 0.72).
narrative_ontology:measurement(qwer_be_t2003, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2003, 0.74).
narrative_ontology:measurement(qwer_be_t2023, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2023, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1893, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1893, 0.65).
narrative_ontology:measurement(qwer_su_t1923, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1923, 0.7).
narrative_ontology:measurement(qwer_su_t1953, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1953, 0.75).
narrative_ontology:measurement(qwer_su_t1983, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1983, 0.78).
narrative_ontology:measurement(qwer_su_t2003, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2003, 0.79).
narrative_ontology:measurement(qwer_su_t2023, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 2023, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_skill_acquisition_norms).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, keyboard_manufacturing_standards).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'QWERTY persistence inevitability' kernel. This 'strategic lock-in' reading emphasizes deliberate manufacturer action and rent extraction, while the 'path dependency' reading (qwerty_persistence_inevitability__path_dependency_reading) emphasizes accidental historical events and self-reinforcing feedback loops.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
