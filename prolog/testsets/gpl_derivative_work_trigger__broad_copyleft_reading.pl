% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft Linking-as-Derivation Reading
 *   domain: legal/intellectual-property/open-source
 *
 * SUMMARY:
 *   The GPL defines software as a derivative work if it links (statically or
 *   dynamically) to GPL-licensed code. This reading — the broad copyleft
 *   interpretation — treats the act of linking as creating a derivative work,
 *   triggering the obligation to disclose source code to all recipients. The
 *   constraint pulls dependent code into the open-source commons by making
 *   proprietary integration costly unless vendors accept source disclosure.
 *   The reading is contested by proponents of narrower interpretations
 *   (permissive licenses, interface-boundary readings) who argue that linking
 *   through stable APIs is aggregation, not derivation. The broad reading
 *   persists through organizational enforcement (GPL maintainers, Free
 *   Software Foundation) despite legal uncertainty — no court has
 *   definitively confirmed that dynamic linking constitutes derivation under
 *   copyright law.
 *
 * KEY AGENTS:
 *   - gpl_maintainers_and_advocates: Agenda-setter (organized/generational/mobile) — interprets and enforces the broad reading
 *   - proprietary_software_vendors: Payer (powerful/biographical/constrained) — bears the cost of source disclosure or avoidance
 *   - downstream_users: Beneficiary (organized/biographical/mobile) — gains source-access rights from the obligation
 *   - independent_library_developers: Beneficiary+payer (moderate/biographical/constrained) — protected by copyleft but limited in market reach
 *   - permissive_license_maintainers: Excluded (moderate/biographical/mobile) — would argue narrower linking definitions
 *   - courts_and_legal_authorities: Observer (institutional/generational/analytical) — legal uncertainty is the suppression mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.68).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.71).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Linking-as-Derivation Reading").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "legal/intellectual-property/open-source").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, '2b99c6c7-c39d-403f-8eff-a5f2468df958').
narrative_ontology:cs_kernel_codification('2b99c6c7-c39d-403f-8eff-a5f2468df958', fixed_text).
narrative_ontology:cs_authority_grounding('2b99c6c7-c39d-403f-8eff-a5f2468df958', lineage).
narrative_ontology:cs_interpretation_layer_present('2b99c6c7-c39d-403f-8eff-a5f2468df958').
narrative_ontology:cs_reading_relation('2b99c6c7-c39d-403f-8eff-a5f2468df958', gpl_derivative_work_trigger__narrow_linking_permissive_reading, forecloses).
narrative_ontology:cs_reading_relation('2b99c6c7-c39d-403f-8eff-a5f2468df958', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('2b99c6c7-c39d-403f-8eff-a5f2468df958', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('2b99c6c7-c39d-403f-8eff-a5f2468df958', linking_creates_derivative_work, deontological).
narrative_ontology:cs_axiom('2b99c6c7-c39d-403f-8eff-a5f2468df958', foundational, copyleft_gravity_well_necessity).
narrative_ontology:cs_axiom_status(copyleft_gravity_well_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2b99c6c7-c39d-403f-8eff-a5f2468df958', copyleft_gravity_well_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('2b99c6c7-c39d-403f-8eff-a5f2468df958', derivative_work_expansionist_commons_protection).
narrative_ontology:cs_drift_state('2b99c6c7-c39d-403f-8eff-a5f2468df958', contemporary_industrial_integration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b99c6c7-c39d-403f-8eff-a5f2468df958', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_beneficiaries).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, closed_source_libraries).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.48) because the GPL's coordination function is genuine — it solves a real collective-action problem for open-source developers. But extractiveness rises over the interval (to 0.68) because the broad reading's reach expands: as the industry matured, GPL maintainers increasingly asserted the broadest interpretation (dynamic linking as derivation), and proprietary vendors faced mounting compliance pressure despite legal uncertainty. The theater ratio (0.42) reflects a growing proportion of enforcement activity devoted to defending the broad interpretation against narrower readings, rather than purely protecting source access. Suppression (0.71) is high because the constraint's persistence depends on legal uncertainty: vendors cannot legally challenge the reading without court action, and no court has ruled decisively. The organization and enforcement infrastructure (GPL maintainers, license-enforcement pressure on distributors) suppress alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the GPL maintainers' and downstream users' perspective, the constraint is genuine coordination: source disclosure is the price of access to a shared commons, and everyone benefits. From proprietary vendors' perspective, the constraint operates as enforced extraction: they are forced to choose between source disclosure and avoidance, both costly. The broad reading is the mechanism that creates this asymmetry. Independent library developers sit between: they benefit from copyleft protection but bear the cost of reduced market reach. The engine should compute divergent classifications across these seats — maintainers might classify it as rope (coordination with mutual benefit), vendors as snare or tangled_rope (asymmetric extraction), and developers as tangled_rope (hybrid with asymmetric downstream impact).
 *
 * DIRECTIONALITY LOGIC:
 *   GPL maintainers (beneficiaries) have high exit options and institutional power — they can fork the GPL, modify enforcement strategy, or shift to permissive licensing. Their d is low (beneficiary end, ~0.15). Proprietary vendors (payers/victims) have constrained exit: they must either accept source disclosure (which damages their business model), avoid GPL libraries entirely (which limits functionality), or invest in proprietary alternatives (expensive). Their d is high (target end, ~0.85). Downstream users are near symmetric (d~0.5): they benefit from source access and freedom, but bear the cost if proprietary vendors withdraw useful libraries or price increases ripple through the ecosystem. Independent library developers are asymmetrically positioned: they capture some benefits (copyleft protection) but also pay penalties (reduced adoption by vendors). No directionality override is needed — the derivation chain correctly models the structural relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT suffering mandatrophy — the founding problem (preventing proprietary capture of open-source work) remains live, and the broad reading is actively maintained to serve it. However, there is a secondary question about whether the broad reading is the minimal necessary mechanism to solve the founding problem, or whether narrower readings (interface boundaries, aggregation-vs-derivation) could achieve the same outcome with less extraction. The engine's classification should detect this as a constraint whose mandate remains live but whose enforcement mechanism is contested — a rope that some seats experience as tangled (coordination + asymmetric cost) or snare (pure extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_derivation_definition_ambiguity,
    'Does copyright law''s definition of ''derivative work'' include dynamic linking through stable APIs, or only modifications to the original source?',
    'Court ruling in a copyright case challenging the GPL''s linking interpretation (e.g., a vendor defending against GPL enforcement action). The threshold test: does the court treat linking as a technical fact or a legal construct? If technical, narrow linking definitions prevail; if legal, the broad reading''s expansionist approach wins.',
    'A court ruling that dynamic linking is NOT derivation would immediately collapse the broad reading''s enforcement and shift classification from rope/tangled_rope to snare (asymmetric extraction without legal foundation). A ruling confirming the broad reading would legitimize the organizational enforcement infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_derivation_definition_ambiguity, empirical, 'Whether copyright law''s definition of derivation includes linking.').

omega_variable(
    commons_benefit_vs_proprietary_cost_asymmetry,
    'Is the distribution of benefits and costs between open-source commons-builders and proprietary vendors structurally necessary to solve the founding problem, or does the broad reading overreach?',
    'Natural experiment: jurisdictions that adopt narrower linking definitions (interface-boundary or aggregation-based) and observe whether (a) open-source development remains robust, (b) proprietary integration expands without damaging the commons, (c) GPL projects shift to permissive licensing. If the commons thrives under narrower readings, the asymmetry was not necessary.',
    'If the commons thrives under narrower readings, the broad reading is revealed as extractive rent-seeking by GPL maintainers rather than necessary coordination. Classification would shift toward snare. If narrower readings cause open-source development to collapse or proprietary capture to accelerate, the broad reading is validated as the minimal necessary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_benefit_vs_proprietary_cost_asymmetry, empirical, 'Whether the broad linking interpretation is necessary to sustain the open-source commons.').

omega_variable(
    organizational_entrenchment_vs_principle,
    'Is the broad reading sustained by genuine principle (linking IS derivation under a coherent theory) or by organizational entrenchment (GPL maintainers enforce it regardless of legal ambiguity)?',
    'Comparative discourse analysis: examine GPL maintainers'' legal arguments over time. Have they refined the argument as courts and language communities developed narrower technical definitions of linking? Or have they doubled down on the broadest reading despite technical drift? Examine licensing decisions by GPL-adjacent projects (Linux, GNOME, etc.): have they adopted narrower readings under pressure, or maintained the broad interpretation? Examine organizational incentives: does the FSF''s funding and influence increase as copyleft enforcement tightens?',
    'If the reading is principle-driven, the constraint is a contested but coherent legal position. If organizational entrenchment drives it, the constraint is revealed as institutional capture — GPL maintainers enforcing the broadest reading not because law or morality requires it, but because it maximizes their institutional control over dependent code. Classification would shift toward snare if entrenchment dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_entrenchment_vs_principle, conceptual, 'Whether the broad reading is sustained by principle or organizational entrenchment.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the broad copyleft reading logically foreclose the narrow-linking reading, or can both coexist as live positions held by different parties?',
    'Examine the logical relationship: if linking is NOT derivation (narrow reading''s core premise), can the broad reading''s core premise (linking IS derivation) be held in the same legal framework? Answer: no — they directly contradict. Within a single copyright law framework, only one can be true. However, different jurisdictions could adopt different readings, and different communities could voluntarily choose different licenses. The question is whether the broad reading actively forecloses the narrow reading or coexists with it through organizational separation.',
    'If the readings foreclose each other, the broad reading''s enforcement represents a direct conflict over legal truth, not organizational coexistence. If they coexist, the constraint is an organizational choice (GPL projects use broad linking; permissive-license projects use narrow linking) rather than a universal legal fact. This affects how the engine classifies the constraint at different seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Logical relationship between broad and narrow linking readings in GPL derivative-work interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 0, 28).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gpl__tr_t4, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(gpl__tr_t8, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(gpl__tr_t12, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(gpl__tr_t18, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement(gpl__tr_t24, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement(gpl__tr_t28, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 28, 0.42).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(gpl__be_t4, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(gpl__be_t8, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(gpl__be_t12, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(gpl__be_t18, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement(gpl__be_t24, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 24, 0.67).
narrative_ontology:measurement(gpl__be_t28, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 28, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpl__su_t4, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(gpl__su_t8, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(gpl__su_t12, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 12, 0.67).
narrative_ontology:measurement(gpl__su_t18, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 18, 0.69).
narrative_ontology:measurement(gpl__su_t24, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(gpl__su_t28, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 28, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_derivative_work_trigger__broad_copyleft_reading, 0.18).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, open_source_commons_sustainability).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_library_adoption_dynamics).

% DUAL FORMULATION NOTE:
% The GPL derivative-work kernel admits three structurally distinct readings: the BROAD_COPYLEFT_READING (this story) asserts linking is derivation; the NARROW_LINKING_PERMISSIVE_READING asserts linking is aggregation; the INTERFACE_BOUNDARY_READING asserts API boundaries decouple derivation from linking. Each reading instantiates different ε values, different beneficiary/victim sets, and different classifications. The readings are siblings in a constraint family, linked by network.affects_constraints. The broad reading pulls dependent code into the commons and is strongly enforced by GPL maintainers. The narrow reading permits proprietary integration and is maintained by permissive-license communities. The interface reading attempts a middle ground. All three readings coexist organizationally, though they foreclose each other logically within any single legal framework. Stories for the narrow and interface readings document the same kernel from different epistemic seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
