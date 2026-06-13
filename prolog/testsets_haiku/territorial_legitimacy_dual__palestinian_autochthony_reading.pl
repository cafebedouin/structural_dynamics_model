% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy_dual__palestinian_autochthony_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy_dual__palestinian_autochthony_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: territorial_legitimacy_dual__palestinian_autochthony_reading
 *   human_readable: Palestinian Territorial Legitimacy (Autochthony Reading)
 *   domain: political/territorial
 *
 * SUMMARY:
 *   Palestinian legitimacy grounded in autochthony — continuous habitation of
 *   the territories, displacement trauma as ongoing injustice, and right of
 *   return as non-negotiable remedial claim. This is ONE READING of the
 *   contested kernel `territorial_legitimacy_dual`, instantiating the
 *   autochthony frame. The reading asserts Palestinian presence and ownership
 *   through continuous habitation, frames 1948 displacement as an unresolved
 *   injury requiring territorial and property restitution, and positions
 *   right of return as a matter of justice, not negotiation. The constraint
 *   is CLAIMED as tangled_rope (coordination of Palestinian identity +
 *   extraction of territorial concession from the Palestinian side through
 *   the conflict's enforcement). Extraction is high (0.89) because the
 *   reading itself is contested, requires active suppression to prevent
 *   implementation (military occupation, settlement expansion, legal denial
 *   of return), and concentrates territorial deprivation on Palestinian
 *   populations while coordinating Palestinian collective identity around the
 *   claim.
 *
 * KEY AGENTS:
 *   - Palestinian National Authority: sets and maintains the autochthony reading through legal frameworks, education, UN advocacy, and Palestinian civil society
 *   - Palestinian displaces (1948 onwards): bear the extraction — loss of land, homes, property rights, freedom of return
 *   - Palestinian West Bank residents: pay in occupation, settlement expansion, territorial fragmentation, movement restrictions
 *   - Palestinian Gaza residents: pay in blockade, confinement, denial of exit and territorial restoration
 *   - Israeli state and settlers: structurally excluded from this reading's framework; their presence would contest the legitimacy claim
 *   - Two-state advocates: excluded because they bracket right of return as negotiable; this reading asserts it as non-negotiable
 *   - International human rights advocates: benefit from the reading's articulation of displacement as injustice and return as right
 *   - International law interpreters: observe the reading's grounding in self-determination, refugee law, and property rights doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.89).
domain_priors:suppression_score(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.87).
domain_priors:theater_ratio(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(territorial_legitimacy_dual__palestinian_autochthony_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy_dual__palestinian_autochthony_reading, tangled_rope).
narrative_ontology:human_readable(territorial_legitimacy_dual__palestinian_autochthony_reading, "Palestinian Territorial Legitimacy (Autochthony Reading)").
narrative_ontology:topic_domain(territorial_legitimacy_dual__palestinian_autochthony_reading, "political/territorial").

domain_priors:requires_active_enforcement(territorial_legitimacy_dual__palestinian_autochthony_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy_dual__palestinian_autochthony_reading, '0c3897f3-64e4-4915-812f-bbc48f59746b').
narrative_ontology:cs_kernel_codification('0c3897f3-64e4-4915-812f-bbc48f59746b', fixed_text).
narrative_ontology:cs_authority_grounding('0c3897f3-64e4-4915-812f-bbc48f59746b', extraction).
narrative_ontology:cs_interpretation_layer_present('0c3897f3-64e4-4915-812f-bbc48f59746b').
narrative_ontology:cs_reading_relation('0c3897f3-64e4-4915-812f-bbc48f59746b', territorial_legitimacy_dual__zionist_refuge_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c3897f3-64e4-4915-812f-bbc48f59746b', territorial_legitimacy_dual__two_state_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('0c3897f3-64e4-4915-812f-bbc48f59746b', foundational, continuous_habitation_grants_territorial_right).
narrative_ontology:cs_axiom_status(continuous_habitation_grants_territorial_right, holdable).
narrative_ontology:cs_axiom_grounding('0c3897f3-64e4-4915-812f-bbc48f59746b', continuous_habitation_grants_territorial_right, deontological).
narrative_ontology:cs_axiom('0c3897f3-64e4-4915-812f-bbc48f59746b', foundational, displacement_requires_restoration_not_replacement).
narrative_ontology:cs_axiom_status(displacement_requires_restoration_not_replacement, holdable).
narrative_ontology:cs_axiom_grounding('0c3897f3-64e4-4915-812f-bbc48f59746b', displacement_requires_restoration_not_replacement, deontological).
narrative_ontology:cs_axiom('0c3897f3-64e4-4915-812f-bbc48f59746b', foundational, right_of_return_is_nonnegotiable).
narrative_ontology:cs_axiom_status(right_of_return_is_nonnegotiable, holdable).
narrative_ontology:cs_axiom_grounding('0c3897f3-64e4-4915-812f-bbc48f59746b', right_of_return_is_nonnegotiable, deontological).
narrative_ontology:cs_reference_frame('0c3897f3-64e4-4915-812f-bbc48f59746b', palestinian_continuous_habitation_and_territorial_rights).
narrative_ontology:cs_drift_state('0c3897f3-64e4-4915-812f-bbc48f59746b', contemporary_post_2000, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0c3897f3-64e4-4915-812f-bbc48f59746b', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_diaspora_right_of_return).
narrative_ontology:constraint_beneficiary(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_territorial_sovereignty).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_displaces_1948_onwards).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_west_bank_residents).
narrative_ontology:constraint_victim(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_gaza_residents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy_dual__palestinian_autochthony_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy_dual__palestinian_autochthony_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy_dual__palestinian_autochthony_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy_dual__palestinian_autochthony_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.89 reflects the reading's core claim: territorial reduction and displacement are ongoing extraction requiring remedy. High extractiveness also models the reading's non-negotiability — it asserts what MUST be transferred (territory, right of return) without trading space for compromise. Suppression at 0.87 is high because the reading's implementation would require territorial and political reorganization that current power structures actively prevent through military occupation, settlement expansion, blockade, legal denial of return, and denial of property restitution. Theater_ratio at 0.42 reflects that much of the occupation's apparatus is theatricalized as 'security' and 'self-defense' rather than frank territorial control, though the core function is territorial maintenance. Accessibility_collapse at 0.78 reflects that the reading presents itself as grounded in historical and legal fact (continuous habitation, refugee law, self-determination doctrine) such that alternatives (partition acceptance, negotiated boundaries) appear as betrayal rather than reasonable compromise to those inside the reading. Resistance at 0.81 reflects sustained Palestinian mobilization, international advocacy, and legal challenge to the occupation and settlement — the reading generates significant resistance, not passive acceptance. The measurement series spans 78 time units (1948 to 2026 approximately, modeling 78 years of displacement and ongoing claim) with monotonically rising extractiveness and suppression, modeling accumulating territorial loss and hardening enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The Palestinian agenda-setter seat and the Palestinian victim seats experience fundamentally different extractiveness profiles. The agenda-setter maintains the reading as necessary political claim and coordinates collective identity around it; victims bear the extraction (territorial loss, displacement, confinement) that the reading asserts as injustice. The reading's very assertion of injustice creates a seat divergence: it claims to name what is being extracted FROM Palestinian people. The engine will compute this as a high d divergence — the agenda-setter has moderate power and identity-locked options (cannot abandon the claim without dissolving Palestinian national identity), while victims have powerless status and trapped options. From the agenda-setter's seat, the reading may compute as rope (coordination of identity and claim). From victim seats, it computes as snare or mountain (the claim is asserted as truth, but its assertion does not materially restore territory or return). The divergence is structural and intentional: the reading's power lies in its assertion of what is true, not in its capacity to materially implement return.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinian displaces and West Bank/Gaza residents sit at d=1.0 (full targets): the reading denies them territory, property, and return; it asserts their deprivation as injustice; they have trapped/identity_locked exit options. Palestinian National Authority sits at d=0.5 to 0.6 (it coordinated the reading but also bears its non-negotiability as a constraint on its strategic options). Palestinian civil society sits near d=0.4 (benefits from identity articulation, pays through conflict's costs). International human rights advocates sit at d=0.2 (beneficiary through moral clarity, mobile exit, no direct cost). Israeli state is excluded from directionality calculation here (it is not seated in this reading's framework). The engine will compute divergent classifications across seats: from Palestinian displaces' perspective, the reading is a tangled_rope (coordinates identity + extracts territory). From international observers' perspective, it may compute as rope (coordinates legitimate claim articulation) or mountain (natural law of territorial return). The reading's non-negotiability and the high suppression required to prevent its implementation mark it as extractive even where coordination appears.
 *
 * MANDATROPHY ANALYSIS:
 *   The autochthony reading was founded to assert Palestinian legitimacy and territorial claim as a matter of right, not negotiation or external grant. The founding problem (displacement and territorial loss) remains live: Palestinian dispossession is ongoing through settlement expansion, occupation, and blockade. However, the reading's function has undergone significant drift: it has become a vehicle for Palestinian national identity and political mobilization rather than a mechanism for territorial restoration. The reading's mandate was to restore territory and enable return; seven decades later, these material outcomes have not occurred, yet the reading persists. Theater_ratio rising from 0.18 to 0.42 models this drift: much activity is now commemorative, educational, and juridical (asserting the claim in international courts, documenting dispossession, educating diaspora) rather than directly remedial. The mandatrophy is CONTESTED rather than resolved: Palestinian advocates assert the reading remains necessary for any future remedy and that its persistence is prerequisite to eventual implementation. Israeli and international observers argue the reading's persistence without material implementation makes it theatricalized claim-making. This is not a piton (the reading generates high resistance and is actively contested) but a constraint whose original mandate has partly shifted from remedial action to identity maintenance and claim articulation. The constraint persists because it is identity-fused: Palestinian nationhood is constituted through the autochthony claim; abandoning the claim would require fundamental re-articulation of Palestinian identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    right_of_return_implementability,
    'Is the right of return (Palestinian refugees and diaspora returning to pre-1948 homes and territories) materially implementable as the reading asserts, or does it remain indefinitely deferred through political and demographic constraints?',
    'Post-conflict settlement framework implementation; demographic modeling of absorption capacity; negotiated agreement establishing return mechanisms. The omega resolves when a political settlement either implements return-based territorial reorganization or permanently brackets return as non-implementable.',
    'If implementable, the reading computes as tangled_rope with live remedial function. If indefinitely deferred, the reading drifts toward piton (persistent claim without material remedy) or reframes into museum/archival function (maintaining historical record without expectation of restitution). This affects classification and mandatrophy assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_implementability, empirical, 'Whether right of return remains materially feasible or has become institutionally deferred indefinitely.').

omega_variable(
    autochthony_vs_identity_claim,
    'Is the autochthony reading primarily a claim about historical territorial ownership (a land claim) or a claim about Palestinian national identity (a people claim)? Do these decompose into separate constraints?',
    'Discourse analysis of Palestinian articulation; observation of what Palestinian advocates prioritize when forced to choose (territorial return vs. identity recognition). If Palestinian leadership treats return as identity-constitutive (losing return means losing Palestinian identity), the readings are entangled; if return is separable as a strategic demand, they decompose.',
    'If autochthony is primarily identity-constitutive, the constraint''s persistence is explained by identity lock (Palestinians cannot abandon the claim without dissolving their identity) rather than by its material function. This shifts classification toward snare or piton (persistent despite low functional output). If the reading decomposes, separate constraint stories should be authored for ''Palestinian identity'' and ''territorial return claim''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autochthony_vs_identity_claim, conceptual, 'Whether autochthony is one integrated claim or multiple structurally distinct claims entangled in the same narrative.').

omega_variable(
    two_state_foreclosure,
    'Does the autochthony reading (asserting right of return as non-negotiable) logically foreclose the two-state coexistence reading (accepting 1967 boundaries as final), or do the readings merely coexist as incompatible political positions?',
    'Philosophical analysis of the logical structure: if a Palestinian negotiator accepts 1967 boundaries as final, have they abandoned autochthony, or reframed it? If they can hold both claims simultaneously (e.g., ''we retain the right of return in principle while accepting 1967 as practical boundary''), the readings coexist. If they cannot, autochthony forecloses two-state coexistence.',
    'If autochthony forecloses two-state, the two readings cannot coexist in a single framework and Palestinian political strategy must choose between them. If they coexist, Palestinian negotiators can maintain autochthony claim while accepting boundaries (moral right + practical compromise). This affects how the kernel contest is modeled: one foreclosure pair vs. multiple coexistence pairs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_state_foreclosure, conceptual, 'The logical relationship between autochthony and two-state frameworks.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.87) primarily structural (military occupation, blockade, legal denial of return, settlement expansion that materially prevents implementation) or partly internalized (Palestinian population has internalized expectation of permanent displacement and loss of return)?',
    'Post-conflict observation: if suppression decreases rapidly after military occupation ends, it was primarily structural. If internalized suppression persists after the military apparatus is removed, the suppression was partly carried in Palestinian consciousness and identity formation.',
    'If primarily structural, lifting suppression (ending occupation, enabling return) would materially activate the reading''s remedial function. If partly internalized, lifting structural suppression would still leave psychological/identity barriers to return and restoration. This affects estimate of how much enforcement (military, legal, settlement) is necessary to maintain the constraint vs. how much is maintained by internalized acceptance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'The composition of suppression between structural barriers and internalized expectation.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint genuinely instantiating a distinct reading of the territorial_legitimacy_dual kernel, or is it conflating multiple kernels (territorial legitimacy, refugee status, national identity, historical trauma)?',
    'Kernel axiom analysis: if the foundational axiom is ''Palestinian continuous habitation grounds territorial right,'' the reading is autochthony-focused. If the foundational axiom is ''Palestinian national identity constituted through displacement memory,'' the reading is identity-focused. If both are foundational, the constraint may be entangling multiple kernels and should decompose.',
    'If genuinely autochthony-reading, the cs_structure axioms should focus on habitation as legitimacy ground. If also identity-constitutive, a separate constraint story should be authored for Palestinian identity, linked via network.affects_constraints. This affects schema validation and reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this constraint is a single kernel reading or an entanglement of multiple kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy_dual__palestinian_autochthony_reading, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(terr_tr_t12, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 12, 0.22).
narrative_ontology:measurement(terr_tr_t24, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(terr_tr_t39, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 39, 0.35).
narrative_ontology:measurement(terr_tr_t54, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 54, 0.39).
narrative_ontology:measurement(terr_tr_t78, territorial_legitimacy_dual__palestinian_autochthony_reading, theater_ratio, 78, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(terr_be_t12, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement(terr_be_t24, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(terr_be_t39, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 39, 0.85).
narrative_ontology:measurement(terr_be_t54, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 54, 0.87).
narrative_ontology:measurement(terr_be_t78, territorial_legitimacy_dual__palestinian_autochthony_reading, base_extractiveness, 78, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(terr_su_t12, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(terr_su_t24, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(terr_su_t39, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 39, 0.84).
narrative_ontology:measurement(terr_su_t54, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 54, 0.86).
narrative_ontology:measurement(terr_su_t78, territorial_legitimacy_dual__palestinian_autochthony_reading, suppression_requirement, 78, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy_dual__palestinian_autochthony_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(territorial_legitimacy_dual__palestinian_autochthony_reading, 0.12).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__zionist_refuge_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, territorial_legitimacy_dual__two_state_coexistence_reading).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, palestinian_national_identity_genealogy).
narrative_ontology:affects_constraint(territorial_legitimacy_dual__palestinian_autochthony_reading, refugee_right_of_return_doctrine).

% DUAL FORMULATION NOTE:
% This is one reading of the kernel territorial_legitimacy_dual. Sibling readings instantiate different legitimacy grounds (Zionist refuge, two-state compromise). The three readings coexist or foreclose depending on whether their foundational axioms can be held simultaneously in a single framework. The autochthony reading differs from the two-state reading in non-negotiability of return; differs from Zionist reading in asserting Palestinian (not Jewish) continuous habitation as the legitimacy ground. Network links establish the constraint family: all three reading-constraints should be present in the corpus for the kernel contest to be fully modeled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(territorial_legitimacy_dual__palestinian_autochthony_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
