% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__two_state_coexistence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__two_state_coexistence_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: territorial_legitimacy_dual__two_state_coexistence_reading
 *   human_readable: Two-State Coexistence Framework with 1967 Boundaries
 *   domain: political/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint instantiates the two-state coexistence reading of the
 *   territorial_legitimacy_dual kernel. It treats mutual recognition of
 *   Israeli and Palestinian legitimacy, bounded by the 1967 lines, as the
 *   operative diplomatic framework. The reading accepts 1948 legitimacy for
 *   both peoples, limits refugee return to the Palestinian state, and
 *   substitutes security cooperation for zero-sum competition. Its
 *   siblingsâthe zionist_refuge_reading and
 *   palestinian_autochthony_readingâreject this symmetry. The framework is
 *   not a natural law; it is a constructed diplomatic arrangement that
 *   requires continuous enforcement through international mediation, security
 *   coordination, and the exclusion of maximalist alternatives.
 *
 * KEY AGENTS:
 *   - un_mediator_complex: Primary agenda-setter (institutional/constrained) â administers the framework and sets the 1967 boundary reference line
 *   - israeli_civic_peace_constituency: Primary beneficiary (organized/constrained) â gains recognition and security in exchange for territorial compromise
 *   - palestinian_statehood_constituency: Primary beneficiary (organized/trapped) â gains self-determination claims but remains under partial occupation
 *   - palestinian_refugee_diaspora: Primary target (powerless/trapped) â bears the cost of foreclosed return to 1948 lands
 *   - israeli_settler_movement: Secondary target (moderate/constrained) â bears the cost of territorial evacuation and voided claims beyond the boundary
 *   - bilateral_rejectionist_factions: Excluded constituency (organized/identity_locked) â denied a seat at the table because their maximalist premises violate the framework's dual-recognition axiom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, 0.48).
domain_priors:suppression_score(territorial_legitimacy_dual__two_state_coexistence_reading, 0.55).
domain_priors:theater_ratio(territorial_legitimacy_dual__two_state_coexistence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__two_state_coexistence_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__two_state_coexistence_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__two_state_coexistence_reading, "Two-State Coexistence Framework with 1967 Boundaries").
narrative_ontology:topic_domain(territorial_legitimacy_dual__two_state_coexistence_reading, "political/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__two_state_coexistence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__two_state_coexistence_reading, '399d6b49-f465-4f85-8e25-21bc73668e7d').
narrative_ontology:cs_kernel_codification('399d6b49-f465-4f85-8e25-21bc73668e7d', fixed_text).
narrative_ontology:cs_authority_grounding('399d6b49-f465-4f85-8e25-21bc73668e7d', lineage).
narrative_ontology:cs_interpretation_layer_present('399d6b49-f465-4f85-8e25-21bc73668e7d').
narrative_ontology:cs_reading_relation('399d6b49-f465-4f85-8e25-21bc73668e7d', territorial_legitimacy_dual__zionist_refuge_reading, influences).
narrative_ontology:cs_reading_relation('399d6b49-f465-4f85-8e25-21bc73668e7d', territorial_legitimacy_dual__palestinian_autochthony_reading, influences).
narrative_ontology:cs_axiom('399d6b49-f465-4f85-8e25-21bc73668e7d', foundational, dual_legitimacy_reciprocal_recognition).
narrative_ontology:cs_axiom_status(dual_legitimacy_reciprocal_recognition, holdable).
narrative_ontology:cs_axiom_grounding('399d6b49-f465-4f85-8e25-21bc73668e7d', dual_legitimacy_reciprocal_recognition, deontological).
narrative_ontology:cs_axiom('399d6b49-f465-4f85-8e25-21bc73668e7d', foundational, partition_at_1967_lines).
narrative_ontology:cs_axiom_status(partition_at_1967_lines, holdable).
narrative_ontology:cs_axiom_grounding('399d6b49-f465-4f85-8e25-21bc73668e7d', partition_at_1967_lines, conventional).
narrative_ontology:cs_reference_frame('399d6b49-f465-4f85-8e25-21bc73668e7d', dual_legitimacy_partition_1967).
narrative_ontology:cs_drift_state('399d6b49-f465-4f85-8e25-21bc73668e7d', post_oslo_collapse_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('399d6b49-f465-4f85-8e25-21bc73668e7d', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__two_state_coexistence_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_civic_peace_constituency).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_statehood_constituency).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__two_state_coexistence_reading, regional_stability_actors).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settler_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the peace process architecture through UNSC resolutions, Quartet principles, and monitoring missions. Maintains the 1967 borders as the legitimate reference line and enforces compliance through diplomatic recognition, aid conditionality, and security coordination mandates.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, un_mediator_complex, agenda_setter,
    institutional, generational, constrained, global).

% Gains international recognition and a security cooperation framework in exchange for accepting territorial compromise. Benefits from reduced regional isolation and normalized relations, though internal political contests over settlement policy continue.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_civic_peace_constituency, beneficiary,
    organized, biographical, constrained, national).

% Receives international recognition of national rights and a pathway to sovereign statehood within 1967 boundaries. Dependent on the framework for institutional survival and international legitimacy, but remains under partial occupation with limited sovereignty and movement restrictions.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_statehood_constituency, beneficiary,
    organized, biographical, trapped, national).

% Benefit from reduced spillover conflict, refugee flows, and radicalization. Support the framework diplomatically and economically but do not administer its terms.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, regional_stability_actors, beneficiary,
    organized, generational, mobile, regional).

% Bears the cost of partition through the permanent foreclosing of individual and property return to lands and homes inside Israel proper. The framework explicitly limits return to the future Palestinian state, not to original locations, leaving most refugees in camp conditions or precarious host-country status.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_refugee_diaspora, payer,
    powerless, generational, trapped, regional).

% Bears the cost of territorial compromise through evacuation, absorption into Palestinian sovereignty, or abandonment of communities established beyond the 1967 line. The framework voids property and residency claims east of the boundary that were previously backed by state subsidies and military protection.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, israeli_settler_movement, payer,
    moderate, biographical, constrained, regional).

% Hold zero-sum territorial ideologies that deny the other people's legitimacy entirely. Excluded from the framework's recognition structure and treated as spoilers to be contained by security cooperation rather than parties to be accommodated.
narrative_ontology:constraint_stakeholder(territorial_legitimacy_dual__two_state_coexistence_reading, bilateral_rejectionist_factions, excluded,
    organized, biographical, identity_locked, national).

narrative_ontology:fixing_cost_class(territorial_legitimacy_dual__two_state_coexistence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents zero-sum territorial competition between two national movements by establishing a mutually recognized partition line and security cooperation regime, replacing exclusive sovereignty claims with coordinated bilateral sovereignty.
% TRANSFER_FUNCTION: Transfers territorial ambition and demographic return rights from maximalist constituenciesârefugees seeking return to pre-1948 homes and settlers seeking annexation beyond 1967 linesâto a bilateral recognition structure administered by international actors.
% ABSENT_VOICES: Rejectionist factions on both sides who deny the other's national legitimacy; refugees whose individual return claims are subordinated to collective statehood claims; and binational or confederal advocates who reject partition as a premise. They are kept out of the framework's recognition structure and treated as spoilers rather than stakeholders.
% DISAPPEARANCE_RATIONALE: Without the mutual recognition framework, the legitimating basis for partition dissolves. Both national projects would revert to maximalist territorial claims, international mediation would lose its reference line, and the security cooperation architecture would fragment into unilateral containment or open conflict.
% FOUNDING_PROBLEM: Two national movements claiming the same territory with mutually exclusive sovereignty claims, resulting in recurrent warfare, displacement, and regional instability since 1948.
% FOUNDING_PROBLEM_CORROBORATION: International historians and conflict scholars attest to the persistence of the dual-national claim structure; however, refugee advocates attest that the framework resolves the conflict at the expense of justice for displaced persons, and rejectionist factions on both sides attest the problem is falsely framed as soluble by partition.
narrative_ontology:disappearance_verdict(territorial_legitimacy_dual__two_state_coexistence_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy_dual__two_state_coexistence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__two_state_coexistence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy_dual__two_state_coexistence_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__two_state_coexistence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__two_state_coexistence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate-to-high because the framework explicitly forecloses the right of return for refugees and voids settler territorial claims; these are real, concentrated costs borne by identifiable groups. Suppression (0.55) reflects the active enforcement required to maintain the framework against rejectionist violence, unilateral annexation attempts, and alternative political projects. Theater ratio (0.40) captures the growing gap between diplomatic ritual (repeated peace process launches) and on-the-ground stabilization. Resistance (0.70) is high because rejectionist factions on both sides actively oppose the framework, while accessibility_collapse (0.60) reflects how the two-state framework has crowded out alternative architectures (confederation, binationalism, Gaza-first) in formal diplomatic discourse.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (UN mediator complex) experiences the framework as necessary diplomacy that prevents wider war. The beneficiary constituencies experience it as a hard-won but viable compromise. The payer seats experience it as an imposed foreclosure of their core claimsârefugees see the right of return extinguished, settlers see their communities rendered illegitimate. The engine should compute divergent per-seat classifications: the mediators may see a rope or scaffold, while the refugees and settlers compute tangled_rope or snare depending on enforcement exposure. This divergence is the signal the corpus is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The UN mediator complex and regional stability actors sit near the beneficiary end of the directionality spectrum: they gain legitimacy and order from the framework without paying its concentrated costs. The two national constituencies (Israeli civic peace, Palestinian statehood) sit closer to symmetric: they receive recognition and security but surrender maximalist territorial and demographic claims. The refugee diaspora and settler movement sit near the full-target end: they bear the specific, non-reciprocal costs of partition (foreclosed return, evacuation) and have the weakest exit options. Rejectionist factions are excluded entirely; their identity-locked exit means the framework's suppression machinery is directed partly at them.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the tangled_rope classification, this framework would be misread as either a rope (ignoring the refugee and settler costs) or a snare (ignoring the genuine coordination function of preventing all-out war). The founding problemâdual national claims to the same territoryâremains live, so the constraint has not atrophied into a piton; it is still actively attempting to coordinate. The mandatrophy guard prevents premature reclassification by requiring evidence that the coordination function has died while the structure persists. Here, security cooperation still actively prevents wider conflict, so the coordination function is operative despite severe practice drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    two_state_kernel_position,
    'Does the two-state coexistence reading represent a necessary partition framework or an imposed international construct that forecloses justice claims contained in sibling readings?',
    'Comparative institutional analysis of sibling reading viability: if either the zionist_refuge_reading or palestinian_autochthony_reading achieves stable implementation without mass displacement, the two-state reading is revealed as contingent rather than necessary.',
    'If contingent, the framework shifts toward snare or tangled_rope classification; if necessary, it stabilizes as rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_kernel_position, conceptual, 'Contested kernel position of the two-state reading within territorial legitimacy dualism.').

omega_variable(
    refugee_return_foreclosure,
    'Is the limitation of Palestinian refugee return to the Palestinian state a necessary cost of partition coordination, or asymmetric extraction from a powerless population?',
    'Empirical assessment of whether alternative frameworks (confederation, one-state, return-with-compensation) could satisfy return claims without triggering the security collapse the two-state reading predicts.',
    'If return is feasible under alternatives, the foreclosure is extractive and the classification skews toward snare; if alternatives collapse into civil war, the limitation is a genuine coordination cost internal to tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(refugee_return_foreclosure, empirical, 'Whether refugee right-of-return limitation is coordination cost or extraction.').

omega_variable(
    security_cooperation_nature,
    'Does the security cooperation architecture function as mutual stabilization or as subcontracted suppression of Palestinian agency and Israeli maximalism?',
    'Post-withdrawal trajectory analysis: if security cooperation ends and violence escalates asymmetrically, the architecture was stabilizing; if Palestinian institutional agency expands without violence, it may have been suppressive.',
    'Would reclassify the enforcement mechanism from coordination maintenance to extraction facilitation, altering the suppression metric interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_cooperation_nature, empirical, 'Ambiguity of security cooperation as stabilization versus suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__two_state_coexistence_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(terr_tr_t8, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(terr_tr_t16, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 16, 0.25).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(terr_tr_t32, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement(terr_tr_t40, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(terr_tr_t48, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 48, 0.38).
narrative_ontology:measurement(terr_tr_t56, territorial_legitimacy_dual__two_state_coexistence_reading, theater_ratio, 56, 0.4).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(terr_be_t8, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(terr_be_t16, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(terr_be_t32, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 32, 0.42).
narrative_ontology:measurement(terr_be_t40, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 40, 0.45).
narrative_ontology:measurement(terr_be_t48, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 48, 0.47).
narrative_ontology:measurement(terr_be_t56, territorial_legitimacy_dual__two_state_coexistence_reading, base_extractiveness, 56, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(terr_su_t8, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(terr_su_t16, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement(terr_su_t32, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(terr_su_t40, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(terr_su_t48, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 48, 0.53).
narrative_ontology:measurement(terr_su_t56, territorial_legitimacy_dual__two_state_coexistence_reading, suppression_requirement, 56, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__two_state_coexistence_reading, resource_allocation).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__two_state_coexistence_reading, palestinian_autochthony_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_legitimacy_dual kernel. It decomposes the colloquial 'Israeli-Palestinian conflict resolution' into structurally distinct commitment systems: the two-state coexistence reading accepts dual legitimacy and partition, while siblings reject this symmetry. Each reading carries a distinct epsilon, beneficiary structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
