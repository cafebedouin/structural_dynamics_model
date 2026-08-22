% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Constitutive Theory of Statehood — Recognition as Legal Prerequisite
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The constitutive reading of the Montevideo criteria holds that statehood
 *   is not a legal fact arising automatically from satisfying four objective
 *   criteria (permanent population, defined territory, government, capacity
 *   to enter relations) but requires the political act of recognition by
 *   existing states. This reading transforms the Montevideo Convention from a
 *   declaratory standard into a constitutive gate: existing states
 *   collectively hold a veto over new state creation. The constraint operates
 *   as a tangled rope — it provides genuine coordination by preventing
 *   chaotic fragmentation of the international order, but extracts
 *   asymmetrically by allowing existing states to block statehood for
 *   geopolitical reasons unrelated to the objective criteria. Unrecognized
 *   polities (Taiwan, Kosovo, Somaliland, Transnistria, etc.) bear the costs
 *   of exclusion from treaty regimes, international finance, and diplomatic
 *   protection while meeting the substantive criteria.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.68).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.75).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Theory of Statehood — Recognition as Legal Prerequisite").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, 'ad10c5b1-7ca8-4949-ac7f-d8f3b677e468').
narrative_ontology:cs_kernel_codification('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', formalized).
narrative_ontology:cs_authority_grounding('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', extraction).
narrative_ontology:cs_interpretation_layer_present('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468').
narrative_ontology:cs_reading_relation('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', foundational, recognition_legal_prerequisite).
narrative_ontology:cs_axiom_status(recognition_legal_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', recognition_legal_prerequisite, conventional).
narrative_ontology:cs_axiom('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', foundational, existing_states_hold_recognition_veto).
narrative_ontology:cs_axiom_status(existing_states_hold_recognition_veto, holdable).
narrative_ontology:cs_axiom_grounding('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', existing_states_hold_recognition_veto, conventional).
narrative_ontology:cs_reference_frame('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', westphalian_recognition_order).
narrative_ontology:cs_drift_state('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', post_decolonization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ad10c5b1-7ca8-4949-ac7f-d8f3b677e468', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, great_powers).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, regional_hegemonies).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, secessionist_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, post_conflict_entities).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, state_sovereignty_preservation).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, international_legal_order_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the recognition gate through bilateral and multilateral diplomatic channels. Benefit from veto power over new state creation which preserves territorial integrity norms and prevents precedent cascades. Can extend or withhold recognition strategically to advance geopolitical interests.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Exercise disproportionate influence over recognition outcomes through UN Security Council veto power and economic leverage. Use recognition as a tool of great power competition — recognizing or blocking entities based on alliance structures rather than objective criteria.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, great_powers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, great_powers, agenda_setter).

% Dominate recognition politics within their spheres of influence. Use the constitutive framework to legitimize or delegitimize separatist movements in neighboring territories based on regional stability calculations.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, regional_hegemonies, beneficiary,
    powerful, generational, arbitrage, regional).

% Meet all four Montevideo criteria (permanent population, defined territory, government, capacity to enter relations) but lack diplomatic recognition. Cannot access treaty frameworks, international financial institutions, or formal trade agreements. Population bears costs of isolation: restricted movement, economic strangulation, denial of consular protection.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, biographical, trapped, global).

% Control territory and population and maintain governance structures but face systematic non-recognition due to territorial integrity norms. Their exit from non-statehood depends entirely on the political will of the parent state and its allies — not on their own capacity to satisfy objective criteria.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, secessionist_movements, payer,
    organized, biographical, constrained, regional).

% Emerging from conflict with functional governance but contested recognition status (e.g., Kosovo, Somaliland). Recognition depends on great power consensus and regional alignment rather than governance capacity. Bear costs of partial recognition: limited treaty participation, conditional market access, diplomatic isolation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, post_conflict_entities, payer,
    moderate, biographical, constrained, regional).

% Analyze the doctrinal coherence and state practice of recognition. Produce the interpretive literature that frames recognition as either constitutive (legal prerequisite) or declaratory (evidentiary). Their work shapes the normative vocabulary but does not determine state practice.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% UN, WHO, WTO, IMF, World Bank gate membership on recognition. Their procedural rules convert political recognition into functional exclusion from global governance. Act as enforcement mechanism for the constitutive reading by making recognition the entry condition for institutional participation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_organizations, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, international_organizations, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a managed gate for entry into the international legal order, preventing uncontrolled proliferation of state claims that could destabilize the territorial status quo. Coordinates recognition decisions through diplomatic practice rather than leaving statehood to unilateral declaration.
% TRANSFER_FUNCTION: Transfers the legal capacity to hold rights and obligations under international law from aspiring entities to existing states. Existing states retain the power to grant or withhold the legal personality required for treaty participation, diplomatic immunity, and access to international courts and financial systems.
% ABSENT_VOICES: Populations of unrecognized polities who have no diplomatic representation and cannot participate in the recognition discourse that determines their legal status. Indigenous peoples and stateless nations whose self-determination claims are filtered through the recognition veto of existing states.
% DISAPPEARANCE_RATIONALE: If the recognition requirement vanished overnight, every entity meeting the Montevideo criteria could claim full international legal personality unilaterally. The international order would shift from a permissioned system to an automatic one — treaty frameworks, border disputes, and institutional membership would require new coordination mechanisms. The territorial integrity norm would lose its primary enforcement lever.
% FOUNDING_PROBLEM: Post-Westphalian and post-colonial international order needed a mechanism to manage the transition from empire to sovereign states without uncontrolled fragmentation. The recognition requirement provided a political filter to sequence and legitimize new state creation.
% FOUNDING_PROBLEM_CORROBORATION: The constitutive reading's founding problem is corroborated by the drafting history of the Montevideo Convention (1933) and the UN Charter's emphasis on territorial integrity (Article 2(4)). However, decolonization practice (1960s-1970s) and the declaratory reading's proponents argue the founding problem was solved by the self-determination norm, rendering the recognition gate an anachronism. The ICJ's Kosovo Advisory Opinion (2010) reflects this contestation.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is substantial: the recognition gate transfers legal personality and its attendant rights (treaty capacity, diplomatic immunity, institutional access) from qualifying entities to the discretion of existing states. Suppression (0.75) is high because non-recognition is actively enforced through UN membership rules, treaty participation barriers, and financial system exclusion — not merely passive non-acknowledgment. Theater ratio (0.42) reflects that the coordination function (managed entry, territorial stability) is real but increasingly performed by a recognition practice that tracks geopolitical interest rather than objective capacity. The measurement series shows extractiveness and suppression rising over the decolonization and post-Cold War periods as the recognition veto became a tool of great power competition rather than orderly transition management.
 *
 * PERSPECTIVAL GAP:
 *   From the existing state seat, the constraint appears as genuine coordination: a necessary filter preventing a flood of unviable or destabilizing state claims. From the unrecognized polity seat, the same structure operates as enforced exclusion: they meet every substantive criterion but are denied the legal consequences of statehood because the gatekeepers' geopolitical interests diverge from the objective standard. The engine computes this divergence from the structural power/exit asymmetry — the claimed tangled_rope type captures both the coordination reality and the extraction reality without forcing them into a single seat's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing states, great powers, and regional hegemons are structural beneficiaries (d near 0.0-0.2): they collect the veto power and its strategic value. Unrecognized polities, secessionist movements, and post-conflict entities are structural targets (d near 0.8-0.95): they satisfy objective criteria but are denied legal personality by a gate they cannot control. Their exit options are trapped or constrained — they cannot 'exit' the non-recognition condition without the cooperation of the very actors benefiting from withholding it. International legal scholars and organizations are analytical observers (d=0.5) but organizations functionally enforce the gate through membership rules.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managed transition from empire to sovereign states) is substantially resolved — decolonization is largely complete and the territorial status quo is entrenched. Yet the recognition requirement persists and has been repurposed as a geopolitical tool. The constraint now extracts more than it coordinates: the coordination function (preventing chaotic fragmentation) is increasingly performed by the territorial integrity norm itself and great power deterrence, while the recognition gate primarily serves to allocate legal personality along alliance lines. This is a classic mandatrophy pattern — the original mandate has atrophied but the constraint remains because its beneficiaries (existing states) control the exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_boundary,
    'Is the recognition requirement a genuine legal prerequisite (constitutive) or a political practice that has been juridified into a rule?',
    'Track state practice and opinio juris: if states consistently treat non-recognition as legally depriving an entity of rights (not just politically inconvenient), the constitutive reading gains doctrinal weight. The ICJ''s treatment of recognition in the Kosovo Advisory Opinion and subsequent practice is the key evidence.',
    'If constitutive, the constraint is a tangled_rope with high extraction from unrecognized polities. If declaratory, the constraint reduces to a coordination mechanism with near-zero extraction — recognition becomes a diplomatic courtesy, not a legal gate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_boundary, conceptual, 'Whether the recognition gate is a legal rule or a political practice masquerading as law.').

omega_variable(
    recognition_as_geopolitical_instrument,
    'To what extent does the recognition veto function as a geopolitical instrument rather than a coordination mechanism?',
    'Quantify recognition outcomes against objective criteria satisfaction: if entities meeting all four criteria are systematically non-recognized based on great power alignment rather than capacity deficits, the geopolitical instrument reading is supported.',
    'High geopolitical instrumentality pushes the constraint toward snare classification for targeted polities — the coordination story becomes cover for selective exclusion. Low instrumentality supports tangled_rope (genuine coordination with asymmetric costs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_as_geopolitical_instrument, empirical, 'Whether recognition decisions track objective criteria or alliance structures.').

omega_variable(
    kernel_reading_framing_ambiguity,
    'Does the Montevideo Convention text itself support the constitutive reading, or does the constitutive reading import a political practice into the legal text?',
    'Textual analysis of Article 3 of the Montevideo Convention (''The political existence of the state is independent of recognition by the other states'') versus Article 1 (listing the four criteria). The drafting history and subsequent state practice determine which article governs.',
    'If Article 3 is controlling, the constitutive reading is a misreading of the kernel — the kernel itself is declaratory, and the constitutive reading is an interpretive overlay that serves existing state interests. This would make the constraint a false summit: presented as legal necessity, actually political extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_ambiguity, conceptual, 'Whether the kernel''s own text forecloses the constitutive reading.').

omega_variable(
    suppression_mechanism_international_law,
    'Is the suppression of unrecognized polities structural (treaty rules, institutional membership gates) or internalized (entities accept their non-statehood and stop asserting claims)?',
    'Compare entities that persistently assert statehood despite non-recognition (Taiwan, Somaliland) vs. those that accommodate (various post-Soviet entities). Track whether suppression persists after the structural barriers are conceptually removed.',
    'If internalized, the constraint''s effective suppression is higher than institutional measures suggest — the target carries the exclusion internally. If purely structural, exit (conceptual or political) would collapse the suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_international_law, empirical, 'Structural vs. internalized suppression in the international legal order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1933, 0.25).
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1945, 0.3).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement(mont_tr_t1975, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1975, 0.38).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2010, 0.41).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1933, 0.45).
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1945, 0.52).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1960, 0.58).
narrative_ontology:measurement(mont_be_t1975, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1975, 0.62).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1933, 0.55).
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1945, 0.6).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1960, 0.68).
narrative_ontology:measurement(mont_su_t1975, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__constitutive_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, un_membership_rules).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, treaty_participation_gate).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, international_financial_institution_access).

% DUAL FORMULATION NOTE:
% The Montevideo kernel decomposes into three constraint stories: constitutive_reading (this story — recognition as legal prerequisite, high extraction, tangled_rope), declaratory_reading (recognition as evidentiary, low extraction, rope), and hybrid_reading (normative legitimacy criteria, variable extraction). The constitutive reading structurally influences the declaratory reading by providing the political practice that the declaratory reading must contend with; it influences the hybrid reading by establishing the recognition gate that normative criteria must pass through. The declaratory reading does not foreclose the constitutive reading — they coexist as live doctrinal positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, institutional, 0.1).
constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, powerless, 0.9).
constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, organized, 0.8).
constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, moderate, 0.75).
constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
