% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Statehood Requires Recognition (Constitutive Reading)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'constitutive' reading of statehood, where
 *   recognition by existing states is a prerequisite for an entity to be
 *   considered a state under international law. This reading grants existing
 *   states significant power over the creation of new states, often leading
 *   to the prolonged exclusion of polities that otherwise meet objective
 *   criteria. The constraint is classified as a Snare due to its high
 *   extractiveness and suppression, primarily impacting unrecognized polities
 *   and secessionist movements.
 *
 * KEY AGENTS:
 *   - existing_states: Agenda setter (institutional/arbitrage) — benefits from control over state creation.
 *   - unrecognized_polities: Payer (powerless/trapped) — bears the costs of exclusion from international system.
 *   - secessionist_movements: Payer (powerless/trapped) — dependent on recognition for legitimacy.
 *   - established_international_organizations: Beneficiary (institutional/constrained) — benefits from stability of existing state system.
 *   - international_legal_scholars: Observer (analytical/analytical) — analyzes the theory and its effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.85).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.9).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, snare).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Statehood Requires Recognition (Constitutive Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '103be17d-5675-4b1f-a235-c40c1e930480').
narrative_ontology:cs_kernel_codification('103be17d-5675-4b1f-a235-c40c1e930480', formalized).
narrative_ontology:cs_authority_grounding('103be17d-5675-4b1f-a235-c40c1e930480', extraction).
narrative_ontology:cs_interpretation_layer_present('103be17d-5675-4b1f-a235-c40c1e930480').
narrative_ontology:cs_reading_relation('103be17d-5675-4b1f-a235-c40c1e930480', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('103be17d-5675-4b1f-a235-c40c1e930480', montevideo_statehood_criteria__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('103be17d-5675-4b1f-a235-c40c1e930480', foundational, recognition_is_prerequisite_for_statehood).
narrative_ontology:cs_axiom_status(recognition_is_prerequisite_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('103be17d-5675-4b1f-a235-c40c1e930480', recognition_is_prerequisite_for_statehood, conventional).
narrative_ontology:cs_axiom('103be17d-5675-4b1f-a235-c40c1e930480', secondary, existing_states_hold_veto_over_new_state_creation).
narrative_ontology:cs_axiom_status(existing_states_hold_veto_over_new_state_creation, holdable).
narrative_ontology:cs_axiom_grounding('103be17d-5675-4b1f-a235-c40c1e930480', existing_states_hold_veto_over_new_state_creation, conventional).
narrative_ontology:cs_reference_frame('103be17d-5675-4b1f-a235-c40c1e930480', post_westphalian_state_system).
narrative_ontology:cs_drift_state('103be17d-5675-4b1f-a235-c40c1e930480', contemporary_self_determination_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('103be17d-5675-4b1f-a235-c40c1e930480', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, established_international_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, secessionist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the existing community of states who collectively grant or withhold recognition. They benefit from maintaining control over the entry of new states, preserving their own power and influence within international forums, and preventing challenges to their territorial integrity.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Entities that meet objective criteria for statehood (territory, population, government, capacity to enter relations) but lack recognition from the international community. They are denied full participation in international law, treaties, and economic systems, facing severe limitations on their sovereignty and development.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, generational, trapped, regional).

% Groups seeking to establish new states through separation from existing ones. Their success is entirely contingent on gaining recognition, which is often withheld by states fearing similar movements within their own borders. They bear the costs of isolation and lack of international legitimacy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, secessionist_movements, payer,
    powerless, generational, trapped, local).

% Organizations like the UN, World Bank, and IMF whose membership and operational scope are defined by the existing state system. They benefit from the stability and predictability that the constitutive theory provides, as it limits the number of actors they must engage with and legitimizes their existing structures.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, established_international_organizations, beneficiary,
    institutional, generational, constrained, global).

% Analyze the practical effects and theoretical implications of the constitutive theory, often highlighting its political nature and the challenges it poses for self-determination and justice. They do not directly participate in the recognition process but influence its discourse.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the entry of new actors into the international system, providing a mechanism for existing states to manage the expansion of the 'club' and ensure a degree of order and stability.
% TRANSFER_FUNCTION: Transfers the power to define statehood from objective criteria to the political will of existing states, effectively granting them a veto over new state creation and the associated benefits of international participation.
% ABSENT_VOICES: Unrecognized polities and secessionist movements are structurally excluded from the recognition process itself; they would argue for a more objective, less politically driven standard for statehood, emphasizing self-determination and the fulfillment of the Montevideo criteria.
% DISAPPEARANCE_RATIONALE: If the requirement for recognition vanished, numerous unrecognized polities would immediately claim full statehood, leading to a chaotic and unpredictable international system. Existing states would lose their gatekeeping power, and international organizations would face a surge of new, potentially unstable, members, forcing a fundamental reorganization of global governance.
% FOUNDING_PROBLEM: The need to manage the emergence of new political entities and integrate them into an existing, albeit evolving, international order, particularly after periods of decolonization or state dissolution.
% FOUNDING_PROBLEM_CORROBORATION: Existing states and international organizations consistently affirm the problem is live, citing the need for stability and managed transitions. Critics (international legal scholars, human rights advocates) acknowledge the problem of order but contest whether the constitutive reading is the most just or effective solution, arguing it often serves political interests over legal principles.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because unrecognized polities are denied fundamental rights and access to the international system, effectively paying a high price for non-recognition. Suppression is also high (0.9) as the international community actively withholds recognition, limiting alternatives and exits for these entities. The theater ratio is low (0.1) because the constitutive theory is actively applied and enforced, with little performative maintenance; its function is direct control. Accessibility collapse is very high (0.95) because without recognition, the path to full statehood is almost entirely blocked. Resistance is moderate (0.7) from unrecognized polities and their advocates, but it faces the overwhelming power of existing states.
 *
 * PERSPECTIVAL GAP:
 *   Existing states perceive this as a necessary mechanism for international order and stability, a 'rope' that coordinates the system. Unrecognized polities and secessionist movements experience it as a 'snare' that unjustly denies them their rights and extracts their potential for development. The engine's classification reflects the latter, more extractive, perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing states are full beneficiaries (d=0.0) as they control the process and maintain their power. Unrecognized polities and secessionist movements are full targets (d=1.0) as they are directly extracted from and suppressed by the lack of recognition. International organizations are beneficiaries (d=0.15) due to the stability provided. International legal scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constitutive reading prevents mislabeling a politically driven gatekeeping mechanism as a neutral coordination function. By highlighting the active enforcement and high extraction from unrecognized entities, it reveals how the 'mandate' of international order can be used to justify a 'snare' that benefits existing powers at the expense of new entrants. The persistence of unrecognized polities despite meeting objective criteria indicates that the constraint's function has drifted from managing order to maintaining a power hierarchy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_ambiguity,
    'Is statehood primarily a political act of recognition (constitutive) or an objective legal fact based on criteria (declaratory)?',
    'Analysis of state practice: if states consistently treat entities meeting objective criteria as states even without formal recognition, it supports the declaratory view. If non-recognition consistently denies statehood, it supports the constitutive view.',
    'If resolved towards declaratory, unrecognized polities would be reclassified as states, shifting the constraint''s type towards a ''piton'' (if recognition becomes merely theatrical) or ''rope'' (if objective criteria genuinely coordinate). If resolved towards constitutive, the current ''snare'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_ambiguity, conceptual, 'Ambiguity between constitutive and declaratory theories of statehood.').

omega_variable(
    legitimacy_vs_power_dynamics,
    'To what extent does the constitutive reading serve genuine international stability versus the self-interest of powerful existing states?',
    'Empirical study of recognition patterns: correlation between recognition decisions and the geopolitical interests of major powers, versus adherence to normative principles or objective criteria.',
    'If primarily driven by self-interest, the ''snare'' classification is strengthened, and the ''beneficiary'' status of existing states is more clearly tied to extraction. If genuinely driven by stability, the coordination aspect might be re-evaluated, potentially shifting towards a ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_power_dynamics, empirical, 'The balance between legitimate stability and power-driven gatekeeping in state recognition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1933, 0.2).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(mont_tr_t1991, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1991, 0.12).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1933, 0.7).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(mont_be_t1991, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1991, 0.8).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1933, 0.75).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(mont_su_t1991, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1991, 0.85).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, international_treaty_access).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, international_economic_integration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Montevideo Statehood Criteria' kernel. It focuses on the constitutive aspect, where recognition is essential. Sibling readings (declaratory, hybrid) would yield different classifications and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
