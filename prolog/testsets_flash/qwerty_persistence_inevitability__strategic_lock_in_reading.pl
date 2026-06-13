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
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence (Strategic Lock-in Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   This constraint describes the persistence of the QWERTY keyboard layout
 *   as a result of strategic lock-in engineered by early typewriter
 *   manufacturers. Rather than being a purely accidental path dependency,
 *   this reading emphasizes the active role of a cartel in standardizing
 *   QWERTY through training partnerships and suppressing superior
 *   alternatives. The constraint is claimed as a Tangled Rope, reflecting its
 *   dual function of coordinating typing standards while extracting rents
 *   through manufactured inevitability.
 *
 * KEY AGENTS:
 *   - typewriter_manufacturers_1893_cartel: Agenda setter (institutional/arbitrage) — actively enforced QWERTY standardization.
 *   - typing_school_partnerships: Beneficiary (organized/constrained) — profited from standardized QWERTY training.
 *   - all_typists: Payer (powerless/identity_locked) — bore ergonomic costs and retraining barriers.
 *   - alternative_keyboard_designers: Payer (moderate/trapped) — faced market exclusion and suppression of innovations.
 *   - analytical_historians: Observer (analytical/analytical) — analyze the historical evidence for strategic lock-in.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.68).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.75).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence (Strategic Lock-in Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, 'ca6d9f74-35aa-4fcd-8302-5a61cd35f4df').
narrative_ontology:cs_kernel_codification('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', implicit).
narrative_ontology:cs_authority_grounding('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', extraction).
narrative_ontology:cs_interpretation_layer_present('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df').
narrative_ontology:cs_reading_relation('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', foundational, standardization_as_strategic_tool).
narrative_ontology:cs_axiom_status(standardization_as_strategic_tool, holdable).
narrative_ontology:cs_axiom_grounding('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', standardization_as_strategic_tool, empirically_contingent).
narrative_ontology:cs_axiom('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', secondary, market_power_shapes_technical_standards).
narrative_ontology:cs_axiom_status(market_power_shapes_technical_standards, holdable).
narrative_ontology:cs_axiom_grounding('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', market_power_shapes_technical_standards, empirically_contingent).
narrative_ontology:cs_reference_frame('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', manufacturer_controlled_standardization).
narrative_ontology:cs_drift_state('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', contemporary_digital_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ca6d9f74-35aa-4fcd-8302-5a61cd35f4df', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_partnerships).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, all_typists).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The group of manufacturers who actively promoted and enforced QWERTY as the industry standard, forming partnerships with typing schools and resisting alternative layouts to maintain market control and avoid retooling costs. They directly benefited from the stability and lack of competition.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel, agenda_setter,
    institutional, generational, arbitrage, global).

% Educational institutions that partnered with manufacturers to exclusively teach QWERTY, ensuring a steady supply of QWERTY-proficient typists and reinforcing the standard. They benefited from the curriculum stability and industry endorsement.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_school_partnerships, beneficiary,
    organized, biographical, constrained, national).

% Individuals who learned to type on QWERTY keyboards, internalizing the layout through muscle memory. They bore the ergonomic costs of an inefficient layout and faced significant retraining barriers if they wished to switch to a more optimal design. Their professional identity became tied to QWERTY proficiency.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, all_typists, payer,
    powerless, biographical, identity_locked, global).

% Innovators who developed more ergonomically efficient or faster keyboard layouts (e.g., Dvorak). They faced immense market entry barriers, lack of manufacturing support, and active suppression of their designs by the entrenched QWERTY standard-bearers, effectively trapping their innovations outside the mainstream market.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_keyboard_designers, payer,
    moderate, generational, trapped, global).

% Researchers who analyze historical documents, corporate archives, and economic data to reconstruct the origins and persistence of the QWERTY layout, seeking to distinguish between accidental path dependency and deliberate strategic action by manufacturers.
narrative_ontology:constraint_stakeholder(qwerty_persistence_inevitability__strategic_lock_in_reading, analytical_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:fixing_cost_class(qwerty_persistence_inevitability__strategic_lock_in_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardized keyboard layout, allowing typists to easily switch between different typewriters and facilitating mass training programs, thereby creating a large, interoperable pool of typists.
% TRANSFER_FUNCTION: Transfers the cost of retooling and innovation from manufacturers to typists (via ergonomic inefficiency and retraining barriers) and to alternative designers (via market exclusion). It also transfers market dominance and sustained profits to the QWERTY-aligned manufacturers.
% ABSENT_VOICES: Designers and proponents of alternative, more efficient keyboard layouts (e.g., Dvorak) were actively marginalized and excluded from market access and educational pipelines. They would argue for open competition based on ergonomic and speed merits.
% DISAPPEARANCE_RATIONALE: If the strategic enforcement and lock-in mechanisms vanished, the market for keyboard layouts would likely diversify rapidly. Typists would have easier access to training and hardware for alternative layouts, leading to a gradual shift towards more efficient designs and a reorganization of keyboard manufacturing and education.
% FOUNDING_PROBLEM: The initial problem was the physical jamming of keys on early mechanical typewriters, which QWERTY was designed to mitigate by separating common letter pairs. Later, the problem shifted to establishing a universal standard for mass production and training.
% FOUNDING_PROBLEM_CORROBORATION: The original key-jamming problem is long dead with modern technology. The 'standardization' problem is contested: manufacturers claim it's still live, but analytical historians (outside the benefiting parties) corroborate that the problem was solved in ways that allowed for more efficient layouts, and QWERTY's persistence is now primarily due to manufactured lock-in, not functional necessity.
narrative_ontology:disappearance_verdict(qwerty_persistence_inevitability__strategic_lock_in_reading, world_rearranges).
narrative_ontology:founding_problem_status(qwerty_persistence_inevitability__strategic_lock_in_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qwerty_persistence_inevitability__strategic_lock_in_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(qwerty_persistence_inevitability__strategic_lock_in_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) because the QWERTY layout, despite its ergonomic inefficiencies, became a de facto standard, allowing manufacturers to avoid retooling costs and maintain market dominance. Suppression is also high (0.75) due to active efforts by the cartel to promote QWERTY through typing schools and resist the adoption of more efficient layouts. The theater ratio is low (0.20) because the coordination function (standardization) was genuinely useful, but the specific choice of QWERTY was strategically enforced rather than purely functional. The measurements track the period from the formation of the 1893 cartel through the mid-20th century, showing increasing extractiveness and suppression as the lock-in became more entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the manufacturers, the standardization was a necessary coordination effort that benefited the industry. From the perspective of typists and alternative designers, it was a costly imposition that limited choice and imposed ergonomic burdens. The engine's per-seat classification will reflect this divergence, with manufacturers as beneficiaries and typists/designers as targets.
 *
 * DIRECTIONALITY LOGIC:
 *   The 1893 cartel and its allied typing schools are clear beneficiaries (d near 0.0) as they profited from the enforced standardization and avoided costs of innovation. Typists and designers of alternative layouts are targets (d near 1.0) as they bore the ergonomic costs, retraining barriers, and market exclusion. The 'identity_locked' exit option for typists reflects the deep entrenchment of QWERTY muscle memory and the high cost of switching.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Tangled Rope prevents mislabeling QWERTY persistence as a pure Mountain (natural inevitability) or a Piton (inertial decay). By identifying active beneficiaries and enforcement, it highlights that the constraint's persistence is not merely accidental or due to diffuse inertia, but is actively maintained for strategic advantage, even if the original 'mandate' of preventing key jamming is long dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is QWERTY persistence a result of strategic lock-in by manufacturers, or an emergent path dependency from historical accident?',
    'Historical analysis of manufacturer archives, cartel agreements, and lobbying efforts to promote QWERTY training and suppress alternatives. Economic modeling of network effects vs. active market manipulation.',
    'If strategic lock-in, the constraint is a Tangled Rope with identifiable beneficiaries and victims. If pure path dependency, it is closer to a Piton or even a Mountain (if truly unchangeable by human action), with diffuse costs and no active beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Distinguishing strategic lock-in from pure path dependency for QWERTY keyboard layout.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative layouts structural (lack of market access, training) or internalized (typists'' belief in QWERTY''s superiority)?',
    'Post-training adoption rates of alternative layouts: if adoption remains low even with accessible training, internalized suppression is higher. Surveys on typists'' perceptions of QWERTY''s efficiency vs. alternatives.',
    'If internalized, the effective suppression is higher than structural measures suggest, as typists carry the suppression with them. If purely structural, removing barriers would lead to faster adoption of alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for QWERTY alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1893, 1943).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwer_tr_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qwer_tr_t25, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(qwer_tr_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(qwer_be_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(qwer_be_t25, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(qwer_be_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qwer_su_t0, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(qwer_su_t25, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(qwer_su_t50, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, information_standard).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'QWERTY persistence inevitability' kernel. This 'strategic_lock_in_reading' emphasizes active manufacturer intervention, while the 'path_dependency_reading' (a sibling constraint) focuses on accidental historical factors and network effects without strategic beneficiaries. Both are linked as part of the same constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
