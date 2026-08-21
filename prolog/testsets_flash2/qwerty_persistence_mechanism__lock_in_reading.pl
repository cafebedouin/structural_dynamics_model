% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_mechanism__lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_mechanism__lock_in_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: qwerty_persistence_mechanism__lock_in_reading
 *   human_readable: QWERTY Keyboard Layout Persistence (Lock-in Reading)
 *   domain: economic_history/technology_studies
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout,
 *   interpreted through the lens of 'lock-in' and path dependence. It argues
 *   that QWERTY, despite its technical inferiority for modern typing,
 *   persists due to network effects, learned behavior, and the high switching
 *   costs for individuals and manufacturers. This reading emphasizes a
 *   collective suboptimality and market failure, where no single actor
 *   actively extracts rents, but the system as a whole imposes costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_mechanism__lock_in_reading, 0.4).
domain_priors:suppression_score(qwerty_persistence_mechanism__lock_in_reading, 0.6).
domain_priors:theater_ratio(qwerty_persistence_mechanism__lock_in_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(qwerty_persistence_mechanism__lock_in_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_mechanism__lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_mechanism__lock_in_reading, "QWERTY Keyboard Layout Persistence (Lock-in Reading)").
narrative_ontology:topic_domain(qwerty_persistence_mechanism__lock_in_reading, "economic_history/technology_studies").

domain_priors:requires_active_enforcement(qwerty_persistence_mechanism__lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_mechanism__lock_in_reading, '747724bf-df43-4c16-9659-63b910738584').
narrative_ontology:cs_kernel_codification('747724bf-df43-4c16-9659-63b910738584', implicit).
narrative_ontology:cs_authority_grounding('747724bf-df43-4c16-9659-63b910738584', practice).
narrative_ontology:cs_reading_relation('747724bf-df43-4c16-9659-63b910738584', qwerty_persistence_mechanism__beneficiary_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('747724bf-df43-4c16-9659-63b910738584', qwerty_persistence_mechanism__naturalization_reading, coexists_with).
narrative_ontology:cs_axiom('747724bf-df43-4c16-9659-63b910738584', foundational, collective_suboptimality_without_active_extraction).
narrative_ontology:cs_axiom_status(collective_suboptimality_without_active_extraction, holdable).
narrative_ontology:cs_axiom_grounding('747724bf-df43-4c16-9659-63b910738584', collective_suboptimality_without_active_extraction, empirically_contingent).
narrative_ontology:cs_axiom('747724bf-df43-4c16-9659-63b910738584', foundational, network_effects_drive_persistence).
narrative_ontology:cs_axiom_status(network_effects_drive_persistence, holdable).
narrative_ontology:cs_axiom_grounding('747724bf-df43-4c16-9659-63b910738584', network_effects_drive_persistence, empirically_contingent).
narrative_ontology:cs_reference_frame('747724bf-df43-4c16-9659-63b910738584', efficient_market_selection).
narrative_ontology:cs_drift_state('747724bf-df43-4c16-9659-63b910738584', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('747724bf-df43-4c16-9659-63b910738584', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_mechanism__lock_in_reading, qwerty_persistence_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, existing_qwerty_users).
narrative_ontology:constraint_beneficiary(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, new_typists).
narrative_ontology:constraint_victim(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ubiquity of QWERTY, making it easy to use any keyboard. However, they are locked into a suboptimal layout due to their learned muscle memory and the high cost of retraining, which makes them resistant to change despite potential long-term gains from alternatives.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, existing_qwerty_users, beneficiary,
    organized, biographical, identity_locked, global).

% Bear the cost of learning an inefficient layout, often without awareness of superior alternatives. Their choice is constrained by the overwhelming prevalence of QWERTY and the lack of accessible training for other layouts.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, new_typists, payer,
    powerless, immediate, constrained, global).

% Benefit from the standardization of QWERTY, simplifying production and reducing design costs. While they could produce alternative layouts, the lack of demand from a locked-in user base and the risk of market fragmentation disincentivizes them.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, keyboard_manufacturers, agenda_setter,
    institutional, generational, constrained, global).

% Invest time and effort into developing technically superior keyboard layouts (e.g., Dvorak, Colemak) but face immense barriers to adoption due to the entrenched QWERTY standard and user lock-in. They bear the cost of market exclusion.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, alternative_layout_developers, payer,
    moderate, biographical, trapped, global).

% Analyze the historical and economic factors contributing to QWERTY's persistence, identifying it as a classic case of path dependence and market failure. They do not directly benefit or pay but provide critical analysis.
narrative_ontology:constraint_stakeholder(qwerty_persistence_mechanism__lock_in_reading, economic_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal standard for keyboard layouts, ensuring interoperability across devices and facilitating communication by minimizing learning friction for new users entering the existing system.
% TRANSFER_FUNCTION: Transfers efficiency costs (slower typing speeds, increased error rates) from the collective of existing users and manufacturers to new typists and developers of superior alternatives, in exchange for maintaining a stable, ubiquitous standard.
% ABSENT_VOICES: The 'ghosts' of potentially superior, unadopted keyboard layouts and the collective efficiency gains that were never realized. Their voices are absent because the market mechanism failed to select for optimal design due to lock-in effects.
% DISAPPEARANCE_RATIONALE: If QWERTY vanished overnight, the global typing infrastructure would collapse, requiring a massive, coordinated effort to adopt a new standard. This would be a chaotic but potentially beneficial rearrangement, as a more efficient layout could emerge.
% FOUNDING_PROBLEM: Early mechanical typewriters faced jamming issues due to common letter combinations, leading to the QWERTY layout designed to slow typists down and separate frequently used keys.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and technology studies scholars widely corroborate that the original mechanical problem is long dead, rendered obsolete by electronic keyboards. The persistence is attributed to network effects and lock-in, not ongoing technical necessity. Keyboard manufacturers, while acknowledging the history, emphasize current user familiarity as the primary driver.
narrative_ontology:disappearance_verdict(qwerty_persistence_mechanism__lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_mechanism__lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_mechanism__lock_in_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_mechanism__lock_in_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qwerty_persistence_mechanism__lock_in_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_mechanism__lock_in_reading_tests).
:- end_tests(qwerty_persistence_mechanism__lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.4) is moderate, representing the cumulative efficiency losses and learning burdens imposed by QWERTY. Suppression (0.6) is significant, driven by the overwhelming ubiquity of QWERTY, the lack of readily available alternatives, and the psychological cost of retraining. Theater ratio (0.1) is low, as there's little active 'performance' to maintain QWERTY beyond its inertial presence; its persistence is more a function of structural lock-in than active defense. The claimed type is Tangled Rope because it provides a coordination function (ubiquitous standard) but imposes asymmetric costs (inefficiency, suppressed alternatives) that require active enforcement (manufacturers defaulting to QWERTY, lack of alternative training).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existing QWERTY users, the layout is a convenient, if imperfect, standard. From the perspective of new typists or alternative layout developers, it's a barrier to efficiency and innovation. The 'lock-in' reading highlights this divergence, where individual rational choices (learning QWERTY because everyone else uses it) lead to collectively suboptimal outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing QWERTY users are beneficiaries of the standard's ubiquity but also payers of its inefficiency, making their directionality complex but leaning towards beneficiary due to the perceived ease of use. New typists and alternative layout developers are clear payers, bearing the costs of inefficiency and market exclusion. Keyboard manufacturers are agenda-setters and beneficiaries, as the standard simplifies their production, even if it's not optimal. Economic historians are observers, analyzing the system without direct participation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (typewriter jamming) is dead, yet the constraint persists. This indicates mandatrophy. The 'lock-in' reading prevents mislabeling this as pure extraction by a single beneficiary, instead framing it as a coordination failure where diffuse costs are borne by many, and no single party benefits enough to actively maintain it beyond inertia, but no single party is harmed enough to overcome the collective action problem of switching. The persistence is due to the structure of the network effects, not active rent-seeking by a concentrated agent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    active_extraction_vs_passive_lock_in,
    'To what extent is QWERTY''s persistence due to passive lock-in (network effects, switching costs) versus active efforts by manufacturers to suppress alternatives and protect their investments?',
    'Analysis of historical corporate strategies, lobbying efforts against alternative layouts, and patent enforcement related to keyboard design. If active suppression is found, reclassify towards ''beneficiary_extraction_reading''.',
    'If active extraction is significant, the constraint''s extractiveness and suppression are higher than currently estimated, and its classification shifts towards Snare or a more extractive Tangled Rope. If passive lock-in dominates, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_extraction_vs_passive_lock_in, empirical, 'Distinguishing between market failure and deliberate rent-seeking in QWERTY''s persistence.').

omega_variable(
    social_cost_quantification,
    'What is the precise economic and cognitive cost of QWERTY''s inefficiency on a global scale, and how does it compare to the benefits of standardization?',
    'Large-scale empirical studies on typing speed, error rates, and learning curves across different layouts, combined with economic modeling of productivity losses and retraining costs.',
    'A higher quantified social cost would strengthen the ''tangled_rope'' classification and highlight the magnitude of the collective suboptimality. A lower cost might lend more credence to the ''naturalization_reading''.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(social_cost_quantification, empirical, 'Quantifying the social costs of QWERTY''s persistence.').

omega_variable(
    naturalization_vs_lock_in_framing,
    'Is QWERTY''s current perceived ''adequacy'' a genuine reflection of its utility, or an internalized rationalization of a locked-in, suboptimal standard?',
    'Controlled experiments comparing user satisfaction and performance with QWERTY versus optimal layouts after equivalent training periods, minimizing bias from prior experience. If users consistently prefer QWERTY even after optimal training on alternatives, it supports the ''naturalization_reading''.',
    'If perceived adequacy is an internalized rationalization, the ''lock-in'' reading is strengthened, highlighting the cognitive dimension of path dependence. If genuine, the ''naturalization_reading'' gains ground, suggesting lower extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalization_vs_lock_in_framing, conceptual, 'Distinguishing between genuine adequacy and rationalized suboptimality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_mechanism__lock_in_reading, 1873, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t1873, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1873, 0.0).
narrative_ontology:measurement(qwer_tr_t1920, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(qwer_tr_t1950, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(qwer_tr_t1980, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(qwer_tr_t2000, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(qwer_tr_t2024, qwerty_persistence_mechanism__lock_in_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(qwer_be_t1873, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1873, 0.1).
narrative_ontology:measurement(qwer_be_t1920, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1920, 0.2).
narrative_ontology:measurement(qwer_be_t1950, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(qwer_be_t1980, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(qwer_be_t2000, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2000, 0.38).
narrative_ontology:measurement(qwer_be_t2024, qwerty_persistence_mechanism__lock_in_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t1873, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1873, 0.1).
narrative_ontology:measurement(qwer_su_t1920, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1920, 0.3).
narrative_ontology:measurement(qwer_su_t1950, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(qwer_su_t1980, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(qwer_su_t2000, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2000, 0.58).
narrative_ontology:measurement(qwer_su_t2024, qwerty_persistence_mechanism__lock_in_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_mechanism__lock_in_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is the 'lock-in' reading of the QWERTY persistence mechanism. It is one of three distinct readings (lock_in_reading, beneficiary_extraction_reading, naturalization_reading) that together form a constraint family, each with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
