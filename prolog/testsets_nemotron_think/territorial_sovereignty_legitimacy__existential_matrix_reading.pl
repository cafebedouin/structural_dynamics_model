% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Territorial Sovereignty Legitimacy (Matrix Reading)
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint story captures the existential_matrix_reading of the
 *   territorial_sovereignty_legitimacy kernel: the claim that sovereignty
 *   legitimacy derives not from law, history, or democratic principle but
 *   from the existential necessity of territorial control for a people's
 *   survival and identity. The constraint is the zero-sum territorial regime
 *   itself — a structure that makes compromise structurally unstable because
 *   neither side can accept vulnerability without experiencing it as
 *   existential dissolution. The dominant group (currently Israeli Jewish
 *   population) extracts security, land, and demographic dominance; the
 *   subordinate group (Palestinian Arab population) bears dispossession and
 *   containment. Both are identity-locked: their self-concepts are fused to
 *   territorial presence, making exit identity-dissolution rather than mere
 *   relocation. International legal frameworks are structurally excluded —
 *   their pronouncements are real but epiphenomenal, unable to alter the
 *   material logic.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.82).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.78).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Territorial Sovereignty Legitimacy (Matrix Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '78631884-8d57-4591-b14d-9f12e968a0cf').
narrative_ontology:cs_kernel_codification('78631884-8d57-4591-b14d-9f12e968a0cf', distributed).
narrative_ontology:cs_authority_grounding('78631884-8d57-4591-b14d-9f12e968a0cf', extraction).
narrative_ontology:cs_interpretation_layer_present('78631884-8d57-4591-b14d-9f12e968a0cf').
narrative_ontology:cs_reading_relation('78631884-8d57-4591-b14d-9f12e968a0cf', territorial_sovereignty_legitimacy__covenant_continuity_reading, influences).
narrative_ontology:cs_reading_relation('78631884-8d57-4591-b14d-9f12e968a0cf', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('78631884-8d57-4591-b14d-9f12e968a0cf', foundational, territorial_control_precondition_for_survival).
narrative_ontology:cs_axiom_status(territorial_control_precondition_for_survival, holdable).
narrative_ontology:cs_axiom_grounding('78631884-8d57-4591-b14d-9f12e968a0cf', territorial_control_precondition_for_survival, empirically_contingent).
narrative_ontology:cs_axiom('78631884-8d57-4591-b14d-9f12e968a0cf', secondary, zero_sum_territorial_competition_inevitable).
narrative_ontology:cs_axiom_status(zero_sum_territorial_competition_inevitable, holdable).
narrative_ontology:cs_axiom_grounding('78631884-8d57-4591-b14d-9f12e968a0cf', zero_sum_territorial_competition_inevitable, empirically_contingent).
narrative_ontology:cs_reference_frame('78631884-8d57-4591-b14d-9f12e968a0cf', existential_territorial_imperative).
narrative_ontology:cs_drift_state('78631884-8d57-4591-b14d-9f12e968a0cf', contemporary_impasse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('78631884-8d57-4591-b14d-9f12e968a0cf', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_territorial_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_territorial_group).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, existential_territorial_imperative).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__existential_matrix_reading, zero_sum_sovereignty_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds military-administrative control over the contested territory and sets the terms of engagement. Justifies control as existential survival requirement. Collects security, demographic advantage, and resource access from the arrangement. Exit would mean relinquishing the territorial control experienced as survival guarantee — identity-fused with the land control itself.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_territorial_group, agenda_setter,
    institutional, generational, identity_locked, national).

% Subject to military occupation, movement restrictions, and demographic displacement. Experiences territorial control as equally existential for collective survival and identity. Bears the costs of the dominant group's security architecture: land loss, restricted autonomy, cyclical violence. Exit (emigration) experienced as identity dissolution — the group's self-concept is constituted through steadfastness on this specific territory.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_territorial_group, payer,
    organized, generational, identity_locked, national).

% Produces resolutions, legal opinions, and frameworks (two-state parameters, humanitarian law, self-determination rulings) that both parties treat as epiphenomenal. Its pronouncements do not alter the material balance or the existential logic driving either side. Would object to the zero-sum framing but lacks enforcement leverage against identity-locked actors.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_legal_order, excluded,
    institutional, generational, analytical, global).

% Intervene diplomatically and materially to manage escalation, but their interventions are absorbed into the zero-sum calculus — aid to either side becomes a resource in the existential competition. They cannot impose a settlement that violates either side's identity-locked existential claim without triggering resistance that restores the deadlock.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, regional_powers, observer,
    powerful, biographical, mobile, regional).

% Provide security guarantees, diplomatic cover, and material aid to the dominant group, treating the arrangement as a strategic asset. Their sponsorship reinforces the dominant group's capacity to maintain the extraction structure. They experience the constraint analytically — as a geopolitical node — not existentially.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, global_superpowers, observer,
    institutional, biographical, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a territorial container for a people's collective survival and identity expression — the arrangement solves the coordination problem of 'where and how does this people persist as a distinct collective?' by anchoring existence to a specific land.
% TRANSFER_FUNCTION: Moves territorial control, demographic weight, resource access, and security from the subordinate group to the dominant group. The dominant group's existential security is purchased through the subordinate group's dispossession and containment. Recognition, legitimacy, and international standing also flow to the dominant group as the recognized sovereign.
% ABSENT_VOICES: The subordinate population's diaspora (structurally excluded from territorial presence), third-generation refugees (whose return would alter the demographic balance the dominant group treats as existential), and international legal theorists who argue legitimacy derives from law not survival — all would challenge the zero-sum framing but are excluded by the identity-lock that makes territorial presence the only valid political standing.
% DISAPPEARANCE_RATIONALE: If the existential territorial claim vanished overnight, the zero-sum lock would break: the dominant group would lose its legitimating narrative for displacement, the subordinate group would lose its legitimating narrative for rejection of compromise, and the international legal order's dormant frameworks (partition, shared sovereignty, confederation) would become live options. The material geography would remain but the political logic structuring it would collapse.
% FOUNDING_PROBLEM: The post-1948 / post-1967 condition in which two peoples each experienced the other's territorial presence as an existential threat to their collective survival, and no juridical framework (partition, internationalization, binational state) succeeded in converting that mutual threat into a stable coordination.
% FOUNDING_PROBLEM_CORROBORATION: Israeli historians (e.g., Benny Morris, Tom Segev) document the 1948 displacement as creating the Palestinian existential claim; Palestinian historians (e.g., Rashid Khalidi, Walid Khalidi) document the pre-1948 demographic reality as grounding their claim. Both sides' founding narratives are corroborated by the other side's archives — but each side treats the other's corroboration as irrelevant to their own existential premise. No external arbiter corroborates either founding problem as resolved.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   High extractiveness (0.82) reflects that the dominant group's territorial control is actively maintained through the subordinate group's dispossession — the arrangement transfers survival resources from one people to another. Suppression (0.78) is high because the constraint's persistence depends on active military-administrative enforcement (occupation, settlement, movement control) and on suppressing the political alternatives (one-state equality, confederation, right of return) that would dissolve the zero-sum. Theater ratio (0.38) is moderate: the Oslo process (1993-2000) created a performrative peace theater that masked accelerating extraction; the current period shows less theater, more overt dominance. Accessibility collapse (0.71) is high because once the existential framing is accepted, alternatives (shared sovereignty, partition, binationalism) appear not just difficult but suicidal — the framing itself collapses the option space. Resistance (0.65) is substantial: the subordinate group maintains organized resistance (intifadas, diplomatic campaigns, steadfastness) despite overwhelming asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   From the dominant group's seat (agenda_setter, institutional, identity_locked), the constraint appears as a mountain — the territorial control is experienced as a natural law of survival, not a choice. From the subordinate group's seat (payer, organized, identity_locked), it appears as a snare — an enforced extraction justified by a cover story that denies their equal existential claim. From the international legal order's seat (excluded, institutional, analytical), it appears as a tangled_rope — a coordination failure with legal frameworks that could work if parties weren't identity-locked. The engine computes these divergences from the structural data; the authored claim (snare) reflects the analytical observer's assessment of the system's actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant_territorial_group is the structural beneficiary (collects territorial control, security, demographic advantage, international recognition — d near 0.0). The subordinate_territorial_group is the structural target (bears displacement, containment, resource denial — d near 1.0). Both are identity_locked: for the dominant group, relinquishing control is experienced as existential suicide; for the subordinate group, accepting subordination is experienced as existential erasure. This mutual identity-lock is what makes the constraint a snare rather than a simple extraction — the trap holds both sides, but the extraction flows one way. International legal order and regional powers are excluded/observers with analytical exit — they experience the constraint as a geopolitical problem, not an existential one.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mutual existential threat post-1948/1967) is contested: both sides attest it remains live for them. The arrangement was built to solve 'how does each people survive?' but the solution (zero-sum territorial dominance) has become the problem — it generates the very existential threat it claims to solve. Mandatrophy is unresolved: the constraint's mandate (territorial sovereignty as survival guarantee) has outlived any cooperative function and now purely reproduces the extraction. The constraint persists not because it solves the founding problem but because the identity-lock makes the founding problem inseparable from the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (territorial_sovereignty_legitimacy) rather than a free-standing constraint?',
    'Comparative analysis of sibling readings (covenant_continuity_reading, self_determination_reading) to confirm they share a kernel but instantiate different constraints with different ε, beneficiary/victim structures, and classifications.',
    'If confirmed, this story must link to sibling stories via network.affects_constraints and the ε-invariance principle applies: each reading gets its own ε assessed against the shared referent (the standing territorial regime).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committee-frame identity: this story instantiates existential_matrix_reading of territorial_sovereignty_legitimacy kernel.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the sibling readings change the beneficiary/victim structure and ε of this constraint?',
    'Author the sibling constraint stories and compare: covenant_continuity_reading makes Israeli Jewish population the primary beneficiary (covenantal title) with Palestinian Arabs as subordinate; self_determination_reading makes Palestinian Arab population the primary beneficiary (democratic majority) with Israeli Jews as settlers/occupiers. This reading makes the beneficiary structurally contingent on dominance.',
    'Different beneficiary structures produce different directionality derivations and thus different per-seat χ values. The kernel''s contest is exactly a contest over who occupies the beneficiary seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural delta across sibling readings: beneficiary seat assignment differs.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.78) primarily structural (military occupation, legal barriers, geographic enclosure) or internalized (both populations'' identity-fusion with territorial control making compromise subjectively impossible)?',
    'Post-hypothetical-settlement observation: if a comprehensive agreement were implemented and suppression metrics persisted (continued violence, rejectionism, inability to normalize), reclassify as partially internalized. Current data: both sides'' identity narratives treat compromise as existential betrayal.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the populations carry the suppression with them. This would increase χ for both identity-locked seats and reinforce snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in identity-locked territorial conflict.').

omega_variable(
    existential_framing_as_cover,
    'Is the existential survival framing a genuine description of the constraint''s coordination function, or a cover story for a dominance-extraction system that could operate under any legitimating narrative?',
    'Counterfactual: if the dominant group achieved security without territorial maximization (e.g., via guaranteed borders, international guarantees, demographic separation), would it accept? Historical evidence (2000 Camp David, 2008 Olmert offer) suggests security-without-maximal-territory was rejected — supporting cover-story hypothesis.',
    'If cover story, the constraint is a snare (coordination is pretense). If genuine coordination failure, it is a tangled_rope (real coordination need + asymmetric extraction). Current metrics favor snare but tangled_rope remains possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_framing_as_cover, conceptual, 'Whether existential framing masks pure dominance extraction or reflects genuine zero-sum coordination failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsl_existential_tr_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(tsl_existential_tr_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1967, 0.3).
narrative_ontology:measurement(tsl_existential_tr_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(tsl_existential_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(tsl_existential_tr_t2005, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(tsl_existential_tr_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(tsl_existential_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(tsl_existential_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.68).
narrative_ontology:measurement(tsl_existential_be_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1993, 0.62).
narrative_ontology:measurement(tsl_existential_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement(tsl_existential_be_t2005, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(tsl_existential_be_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2024, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(tsl_existential_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(tsl_existential_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(tsl_existential_su_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(tsl_existential_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(tsl_existential_su_t2005, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(tsl_existential_su_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.08).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is the existential_matrix_reading of the territorial_sovereignty_legitimacy kernel. The three readings form a constraint family: each instantiates a different constraint from the same kernel with different ε (this reading: 0.82; covenant: estimated 0.65 with Israeli Jews as beneficiaries; self-determination: estimated 0.70 with Palestinian Arabs as beneficiaries). The ε-invariance principle requires separate stories because the beneficiary/victim structure differs fundamentally across readings — the 'beneficiary is whichever side achieves demographic/military dominance' in this reading, but fixed to specific groups in the siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, organized, 0.92).
constraint_indexing:directionality_override(territorial_sovereignty_legitimacy__existential_matrix_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
