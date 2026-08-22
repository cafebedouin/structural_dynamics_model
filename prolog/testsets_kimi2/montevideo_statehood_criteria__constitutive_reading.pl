% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Constitutive Theory of Statehood
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The constitutive theory of statehood holds that an entity becomes a state
 *   in the full legal sense only when recognized as such by the existing
 *   community of states. This reading of the Montevideo statehood criteria
 *   kernel treats recognition not as mere declaratory acknowledgment but as a
 *   constitutive act that creates international legal personality. Under this
 *   reading, objective criteria (population, territory, government, capacity
 *   to enter relations) are necessary but insufficient; the decisive factor
 *   is the political will of existing states. This generates a structural
 *   veto for established powers and places unrecognized polities such as
 *   Taiwan, Kosovo, Palestine, and Northern Cyprus in a liminal legal
 *   condition where they possess many attributes of statehood but lack the
 *   full complement of rights and access. The constraint is claimed as
 *   coordination (stability of the state system, prevention of chaotic
 *   proliferation) but operates with substantial extraction concentrated on
 *   powerless aspirants and unrecognized entities.
 *
 * KEY AGENTS:
 *   - great_powers: Primary agenda-setter (institutional/constrained) â control recognition decisions and benefit from gatekeeping
 *   - recognized_minor_states: Beneficiary (organized/constrained) â benefit from club stability and exclusive membership
 *   - unrecognized_polities: Primary target (powerless/trapped) â meet objective criteria but are denied legal standing and economic/diplomatic access
 *   - secessionist_movements: Secondary target (powerless/trapped) â blocked from statehood by recognition veto
 *   - international_law_scholars: Analytical observer (analytical/analytical) â document and debate the theory's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.72).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.78).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Theory of Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '7800485b-a5f6-4c72-ae44-7dc2a5228cf3').
narrative_ontology:cs_kernel_codification('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', formalized).
narrative_ontology:cs_authority_grounding('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', lineage).
narrative_ontology:cs_interpretation_layer_present('7800485b-a5f6-4c72-ae44-7dc2a5228cf3').
narrative_ontology:cs_reading_relation('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', foundational, recognition_constitutes_legal_personality).
narrative_ontology:cs_axiom_status(recognition_constitutes_legal_personality, holdable).
narrative_ontology:cs_axiom_grounding('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', recognition_constitutes_legal_personality, conventional).
narrative_ontology:cs_axiom('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', foundational, existing_state_community_veto_legitimate).
narrative_ontology:cs_axiom_status(existing_state_community_veto_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', existing_state_community_veto_legitimate, conventional).
narrative_ontology:cs_reference_frame('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', state_system_club_model).
narrative_ontology:cs_drift_state('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', contemporary_multipolar_order, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7800485b-a5f6-4c72-ae44-7dc2a5228cf3', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, great_powers).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, recognized_minor_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, secessionist_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the recognition process through UN Security Council permanent membership, diplomatic leverage, and bilateral recognition decisions. They benefit from gatekeeping access to the state system, using recognition as a geopolitical tool to reward allies and punish adversaries.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, great_powers, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the club nature of the state system; their own sovereignty is stabilized by the high bar for new admission. They participate in UN General Assembly votes on membership and recognition but do not individually control the gate.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, recognized_minor_states, beneficiary,
    organized, generational, constrained, global).

% Meet objective criteria for statehood (territory, population, government, capacity to enter relations) but lack recognition from key existing states. Excluded from UN membership, most treaty regimes, and full diplomatic and economic relations. Cannot force recognition through any legal mechanism.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, biographical, trapped, regional).

% Seek to create new states through territorial separation but are blocked by the recognition requirement. Their populations are denied the legal protections and institutional frameworks of statehood because existing states refuse to recognize the new entity, often to preserve territorial integrity norms.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, secessionist_movements, payer,
    powerless, biographical, trapped, regional).

% Analyze and debate the constitutive versus declaratory theories. They document recognition practice, advise governments and international organizations, and produce the doctrinal frameworks that sustain or challenge the constitutive reading.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents chaotic proliferation of statehood claims by requiring collective acceptance from existing states before full international legal personality attaches, stabilizing the membership of the international community.
% TRANSFER_FUNCTION: Transfers the power to create legal personality from aspirant polities themselves to the existing community of states, and transfers treaty access, diplomatic standing, and institutional participation away from unrecognized polities toward recognized ones.
% ABSENT_VOICES: Populations of unrecognized polities and secessionist territories are excluded from the international legal conversation; they would argue for automatic legal personality upon meeting objective criteria but are structurally absent from recognition decisions.
% DISAPPEARANCE_RATIONALE: If the recognition requirement vanished, dozens of entities meeting objective criteria would demand immediate statehood and UN membership, existing treaty networks would fragment under new entrants, and the geopolitical leverage that existing states derive from gatekeeping would collapse.
% FOUNDING_PROBLEM: Nineteenth and early-twentieth-century instability caused by competing, unilateral claims to statehood, partial recognition by selected powers, and fragile new entities lacking collective acceptance.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and nineteenth-century legal commentators attest the founding problem from outside the great-power beneficiary camp. Contemporary critical international law scholars corroborate that instability was real but contest whether the constitutive solution was necessary or has since outlived its function.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint systematically withholds legal personality, treaty access, and economic participation from entities that would qualify under objective criteria alone. Suppression (0.78) is higher because the exclusion is actively enforced through diplomatic non-recognition, UN membership exclusion, and economic isolation â there is no legal pathway for an unrecognized polity to compel recognition. Theater ratio (0.50) is moderate-high: recognition rituals, diplomatic conferences, and UN admission processes involve genuine coordination signaling but also substantial performative gatekeeping that rehearses existing power hierarchies. Accessibility collapse (0.68) reflects that once non-recognition is established, alternatives vanish â unrecognized polities cannot self-certify into the state system. Resistance (0.55) captures persistent but structurally weak opposition from unrecognized polities and some academic critics.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of great powers, the constraint is legitimate coordination that stabilizes borders and prevents fragmentation. From the seat of unrecognized polities, it is enforced exclusion from legal standing based on political criteria they cannot control. The engine computes this divergence from the structural data: same constraint, opposite directionalities.
 *
 * DIRECTIONALITY LOGIC:
 *   Great powers and recognized minor states are structural beneficiaries (low d): the constraint subsidizes their sovereignty by limiting new entrants and giving them gatekeeping leverage. Unrecognized polities and secessionist movements are structural targets (high d): they bear the full cost of exclusion, and their trapped exit options amplify effective extraction. International law scholars sit at the analytical pole with neutral directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   A pure snare reading would miss the genuine coordination function: the constitutive theory did emerge from a real problem of unstable, competing statehood claims in the 19th and early 20th centuries, and it does prevent chaotic proliferation. A pure rope reading would miss the asymmetric extraction: the coordination is not neutral but concentrates veto power in existing states, particularly great powers, and systematically disadvantages entities that meet all objective criteria. The tangled_rope classification captures both faces: the constraint solves a real collective-action problem (who gets to be a state) while extracting from the excluded through active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_ontology,
    'Is recognition merely declaratory of pre-existing statehood, or constitutive of legal personality?',
    'Comparative analysis of legal consequences across jurisdictions: if unrecognized polities possess identical treaty-making capacity and judicial standing to recognized ones, declaratory theory is descriptively dominant; if recognition gates all meaningful legal personality, constitutive theory holds.',
    'If declaratory theory is descriptively accurate, the constraint''s extractiveness is lower than measured because non-recognition would be a political insult rather than a legal disability. If constitutive theory holds, extraction is as high as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_ontology, conceptual, 'Kernel-level ontological ambiguity between constitutive and declaratory readings of statehood.').

omega_variable(
    unrecognized_polity_objective_criteria,
    'Do unrecognized polities generally meet the four Montevideo criteria, or do they fail objective tests that justify their exclusion independently of recognition politics?',
    'Empirical case-study audit of Taiwan, Palestine, Kosovo, Northern Cyprus, and Somaliland against the four objective criteria.',
    'If most unrecognized polities meet objective criteria, the constitutive reading operates as pure political gatekeeping (high extraction). If they fail objective tests, the recognition requirement may be a declaratory delay rather than constitutive denial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unrecognized_polity_objective_criteria, empirical, 'Whether unrecognized polities are excluded by politics or by failure to meet objective statehood criteria.').

omega_variable(
    great_power_veto_as_extraction,
    'Does the recognition requirement structurally entrench great power privilege, or does it reflect a necessary collective security function?',
    'Historical pattern analysis of recognition decisions: correlation with collective security needs versus bilateral great power interests.',
    'If recognition tracks great power interest, the coordination story is cover for extraction. If it tracks genuine collective security, the coordination function is primary and extraction is incidental.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_veto_as_extraction, empirical, 'Whether recognition gatekeeping serves systemic stability or great power rent-seeking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(mont_tr_t18, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(mont_tr_t36, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(mont_tr_t54, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 54, 0.42).
narrative_ontology:measurement(mont_tr_t72, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 72, 0.48).
narrative_ontology:measurement(mont_tr_t90, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 90, 0.5).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(mont_be_t18, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(mont_be_t36, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(mont_be_t54, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 54, 0.7).
narrative_ontology:measurement(mont_be_t72, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 72, 0.75).
narrative_ontology:measurement(mont_be_t90, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 90, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t0, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(mont_su_t18, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(mont_su_t36, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 36, 0.72).
narrative_ontology:measurement(mont_su_t54, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 54, 0.76).
narrative_ontology:measurement(mont_su_t72, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 72, 0.8).
narrative_ontology:measurement(mont_su_t90, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 90, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, identity_coordination).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% The Montevideo statehood criteria kernel decomposes into three structurally distinct constraints: the constitutive reading (recognition creates statehood), the declaratory reading (objective criteria suffice), and the hybrid reading (objective criteria plus normative legitimacy). Each reading carries a different epsilon, beneficiary structure, and victim set. This decomposition follows the epsilon-invariance principle: the same natural-language label covers multiple structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
