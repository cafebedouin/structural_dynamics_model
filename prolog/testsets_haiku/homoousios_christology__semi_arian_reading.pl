% ============================================================================
% CONSTRAINT STORY: homoousios_christology__semi_arian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_christology__semi_arian_reading, []).

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
 *   constraint_id: homoousios_christology__semi_arian_reading
 *   human_readable: Homoiousios (Similar Substance) Christological Doctrine
 *   domain: historical/ecclesiastical/theological
 *
 * SUMMARY:
 *   The homoiousios ('similar substance') formula emerges in the post-Nicene
 *   period as a compromise position between the strict Pro-Nicene homoousios
 *   ('identical substance,' consubstantiality) and the Arian position (Christ
 *   as created and subordinate). Positioned by its advocates as the rational
 *   via media that allows bishops of different doctrinal convictions to
 *   affirm a single conciliar statement, it is backed by the imperial
 *   authority (especially Constantius II) as the pathway to doctrinal and
 *   ecclesiastical unity. From 340–378, homoiousios dominates multiple
 *   councils and is enforced through institutional mechanisms (exile,
 *   condemnation). However, the formula does not resolve the underlying
 *   doctrinal dispute—it postpones it. By 381, the Council of Constantinople
 *   abandons homoiousios in favor of strict Pro-Nicene doctrine, revealing
 *   the compromise as historically transient and extractive: it functioned as
 *   a platform for the semi-Arian faction to exercise authority while
 *   generating instability for all other participants. The constraint is
 *   CLAIMED as tangled_rope (genuine coordination problem + asymmetric
 *   extraction); the measurement series show precisely this: moderate-high
 *   extraction (0.41 at interval endpoint, rising to 0.47 at peak in 355)
 *   coupled with substantial theater (0.48 overall), indicating the
 *   constraint's 'solution' function becomes increasingly performative as
 *   enforcement intensifies.
 *
 * KEY AGENTS:
 *   - Semi-Arian Episcopal Faction: sets the council agenda and enforces homoiousios adoption; benefits from positioning itself as the rational center and controlling which formula is 'orthodox' at each moment.
 *   - Pro-Nicene Strict Homoousios Advocates: must either abandon doctrinal precision or resist and face exile; principal payer.
 *   - Arian Subordinationists: lose their core doctrinal claim; increasingly excluded and marginalized.
 *   - Imperial Authority (Constantius II): backs homoiousios as political solution to schism risk; gains authority as doctrinal arbiter.
 *   - Athanasius and Strict Nicene Party: excluded from councils during homoiousios dominance; their voice is silenced through institutional enforcement.
 *   - Regional Episcopal Sees: experience repeated forced reaffirmation as imperial backing shifts between formulas; pay in institutional chaos.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_christology__semi_arian_reading, 0.41).
domain_priors:suppression_score(homoousios_christology__semi_arian_reading, 0.52).
domain_priors:theater_ratio(homoousios_christology__semi_arian_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(homoousios_christology__semi_arian_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_christology__semi_arian_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_christology__semi_arian_reading, "Homoiousios (Similar Substance) Christological Doctrine").
narrative_ontology:topic_domain(homoousios_christology__semi_arian_reading, "historical/ecclesiastical/theological").

domain_priors:requires_active_enforcement(homoousios_christology__semi_arian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_christology__semi_arian_reading, '7c7f7f1e-b9b7-456b-a61d-8d9446550e7f').
narrative_ontology:cs_kernel_codification('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', fixed_text).
narrative_ontology:cs_authority_grounding('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', lineage).
narrative_ontology:cs_interpretation_layer_present('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f').
narrative_ontology:cs_reading_relation('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', homoousios_christology__pro_nicene_reading, coexists_with).
narrative_ontology:cs_reading_relation('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', homoousios_christology__arian_reading, coexists_with).
narrative_ontology:cs_axiom('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', foundational, father_son_substance_similarity_not_identity).
narrative_ontology:cs_axiom_status(father_son_substance_similarity_not_identity, holdable).
narrative_ontology:cs_axiom_grounding('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', father_son_substance_similarity_not_identity, deontological).
narrative_ontology:cs_axiom('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', secondary, compromise_doctrine_preserves_empire_unity).
narrative_ontology:cs_axiom_status(compromise_doctrine_preserves_empire_unity, overridden).
narrative_ontology:cs_axiom_grounding('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', compromise_doctrine_preserves_empire_unity, instrumental).
narrative_ontology:cs_reference_frame('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', nicene_council_post_325_ambiguity).
narrative_ontology:cs_drift_state('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', council_of_constantinople_381, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7c7f7f1e-b9b7-456b-a61d-8d9446550e7f', '').
narrative_ontology:cs_kernel_id(homoousios_christology__semi_arian_reading, homoousios_christology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, semi_arian_episcopal_faction).
narrative_ontology:constraint_beneficiary(homoousios_christology__semi_arian_reading, imperial_schism_avoidance_interest).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, arian_subordinationists).
narrative_ontology:constraint_victim(homoousios_christology__semi_arian_reading, pro_nicene_strict_homoousios_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(homoousios_christology__semi_arian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(homoousios_christology__semi_arian_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_christology__semi_arian_reading_tests).
:- end_tests(homoousios_christology__semi_arian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.18 (projected, 325 Nicaea baseline) to 0.47 (peak 355 under Constantius) as the semi-Arian faction consolidates control of councils and intensifies enforcement. Theater rises in parallel (0.22 → 0.55), indicating the constraint increasingly operates as a framework for political control dressed as doctrinal resolution. Suppression tracks enforcement intensity (0.25 → 0.61), peaking when imperial backing is strongest. The sharp collapse (extractiveness 0.47 → 0.08, theater 0.55 → 0.15, suppression 0.61 → 0.12) at 381 reflects the Council of Constantinople's rejection of homoiousios and adoption of strict Pro-Nicene doctrine—the constraint's historical abandonment reveals its transient, extractive nature. The measurement grid uses the shared interval (325–381, one time point per 15–17 years) to allow the temporal dynamics to emerge: the constraint's rise, peak, and sudden collapse are historically documented. Accessibility collapse (0.62) reflects that once bishops understood the formula's role in suppressing their own doctrinal convictions in favor of imperial unity, alternatives (strict Arian or Pro-Nicene positions) did not actually collapse—they persisted and eventually prevailed. Resistance (0.71) is high: the constraint faced continuous pushback from Athanasius, strict Nicenes, and even some Arians who rejected the compromise's pretense to truth.
 *
 * PERSPECTIVAL GAP:
 *   From the semi-Arian faction's and imperial authority's seats, homoiousios is genuine coordination: a formula that allows different bishops to remain in communion while preserving a genuine doctrinal distinction (Father/Son differentiation). From the Pro-Nicene and Arian seats, the same constraint is extraction: forced acceptance of linguistic ambiguity that serves imperial political interest and semi-Arian institutional control, not theological truth. From the strictly Nicene perspective (Athanasius), the constraint is pure suppression—a mechanism to exclude and exile those who insist on doctrinal precision. The engine computes these perspectives from the stakeholder structure: the semi-Arian faction and imperial authority derive low directionality (d approaching beneficiary end: they set the rules and collect authority); Pro-Nicene and Arian factions derive high directionality (d approaching target end: they are forced to affirm a formula they reject). Regional bishops sit at mixed directionality (they benefit from avoided schism but pay in doctrinal instability and repeated forced reaffirmations). The same structural arrangement produces wildly different d values per seat—that is the perspectival gap the measurement is designed to capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Semi-Arian faction: d ≈ 0.15 (beneficiary; they set the agenda, enforce adoption, benefit from institutional control). Imperial authority: d ≈ 0.20 (beneficiary; derives authority from arbitrating doctrinal disputes; political stability interest aligns with homoiousios adoption). Pro-Nicene advocates: d ≈ 0.82 (target; forced to accept linguistic ambiguity that violates their core claim; no exit). Arian subordinationists: d ≈ 0.75 (target; their core claim is rejected; increasing institutional exclusion). Regional episcopal sees: d ≈ 0.55 (symmetric; benefit from avoided schism, pay in repeated forced reaffirmations and doctrinal instability). Athanasius/strict Nicene party: d ≈ 0.88 (target; excluded, exiled, their position suppressed through institutional machinery). No directionality overrides are needed—the structural derivation from beneficiary/victim declarations and exit options produces the right directionality profile for each seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was schism risk under imperial stability pressure—bishops could not affirm a single conciliar statement because they held logically incompatible doctrinal positions. The homoiousios compromise provides a formula flexible enough to contain multiple interpretations (Father and Son are of 'similar' but not 'identical' substance). This is genuine tangled_rope coordination: a real collective-action problem (avoiding empire-fracturing schism) that requires active enforcement (suppressing those who insist on their original positions) to hold. However, the constraint's mandatrophy is visible in the measurement series: by 370, the theater ratio has risen to 0.55, indicating that the constraint increasingly operates as a framework for semi-Arian and imperial political control rather than as a true coordination mechanism. The sharp collapse at 381 confirms this: when the political will to enforce homoiousios evaporated (new imperial regime, changed succession dynamics), the constraint disappeared almost instantly. This is the signature of mandatrophy—the founding coordination problem (avoiding schism) was never actually solved; it was merely deferred through institutional enforcement. The constraint prevented schism only so long as imperial backing persisted. Once that backing shifted, the schism that homoiousios was meant to prevent materialized anyway (the Arian/Pro-Nicene split that persisted until the 381 resolution). Therefore, the constraint should be classified as tangled_rope-to-piton: it begins as a genuine (if fragile) coordination mechanism but degenerates into theatrical maintenance as enforcement intensifies and the founding problem remains unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_deferral,
    'Does homoiousios function as a genuine coordinate mechanism that solves the schism problem, or as an institutional deferral that merely postpones doctrinal confrontation while concentrating power in the semi-Arian faction?',
    'Examine whether bishops holding homoiousios doctrine with genuine conviction (not under imperial coercion) report stable equilibrium, or whether the formula consistently requires intensifying enforcement to prevent defection to Arian or Pro-Nicene positions. The 381 collapse is evidence for deferral; stable maintenance through the 340–378 period would suggest coordination.',
    'If coordination: the constraint is a legitimate tangled_rope with real beneficiaries and real coordination gains, justified in part by the schism-avoidance benefit. If deferral: the constraint is closer to a piton (institutional inertia dressed as unity) or a snare (extraction of authority by the semi-Arian faction under the cover of ''compromise''). The measurement''s sharp collapse at 381 already suggests deferral over genuine coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_deferral, empirical, 'Whether the constraint solves or postpones the underlying doctrinal dispute.').

omega_variable(
    imperial_interest_independence,
    'Would semi-Arian bishops have adopted and defended homoiousios doctrine without imperial backing, or is the doctrine''s persistence entirely dependent on Constantius''s political interest in avoiding schism?',
    'Historical comparison: trace homoiousios adoption and defense across periods of strong vs. weak imperial enforcement (Constantius''s reign vs. post-Constantius periods). If defense correlates perfectly with imperial backing, the doctrine is politically rather than theologically driven; if independent theological rationales persist, the doctrine has intrinsic appeal.',
    'High imperial-interest dependence would reclassify the constraint as closer to a snare (the semi-Arian faction extracts institutional authority by aligning with imperial interests) or a piton (performative unity maintained by state power). Independence would support the tangled_rope classification and legitimate beneficiary claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(imperial_interest_independence, empirical, 'The independence of semi-Arian theological commitment from imperial political interest.').

omega_variable(
    suppression_mechanism_identity_fusion,
    'When Pro-Nicene and Arian bishops affirm homoiousios under imperial pressure, is the suppression structural (external barriers: exile, council exclusion, institutional consequences) or internalized (the bishops come to psychologically believe the formula is true, or fuse their identity with the compromise position)?',
    'Post-suppression trajectory: examine what bishops do once imperial enforcement of homoiousios ends. If they immediately revert to their original positions (Pro-Nicene homoousios, Arian subordinationism), suppression was structural. If they retain homoiousios affiliation or show signs of doctrinal confusion, suppression was partially internalized.',
    'Structural suppression is reversible and does not persist after enforcement ends. Internalized suppression carries higher effective cost because the target absorbs the constraint. The 381 swift reversion to Pro-Nicene doctrine suggests suppression was largely structural, which supports the extraction reading (bishops were forced to affirm a false compromise, not convinced it was true).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_fusion, empirical, 'The locus of suppression—structural enforcement vs. internalized belief.').

omega_variable(
    doctrinal_distinctiveness_semarian,
    'Does the homoiousios formula represent a genuinely distinct doctrinal position with internal theological coherence, or is it primarily a linguistic bridge designed for political reconciliation without substantive doctrinal content?',
    'Examine the theological writings of committed semi-Arian bishops (e.g., Eusebius of Caesarea, Basil of Ancyra) to assess whether they develop the doctrine''s implications with sophistication. Compare the number and rigor of theological arguments for homoiousios vs. for homoousios and subordinationism. If semi-Arian theology is sparse or circular, the reading supports deferral; if rich and independent, it supports genuine doctrinal position.',
    'Genuine doctrinal distinctiveness would elevate the constraint from a mere political compromise to a legitimate theological option and would strengthen the tangled_rope classification. Linguistic-bridge status would support reclassification as piton or snare (institutional performance without substantive content).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_distinctiveness_semarian, conceptual, 'The theological status and internal coherence of homoiousios doctrine.').

omega_variable(
    kernel_committer_ambiguity,
    'Is the kernel dispute (homoousios_christology) properly decomposed into three independent constraint stories (arian_reading, pro_nicene_reading, semi_arian_reading), or do the three readings constitute a single constraint viewed from different institutional seats?',
    'Apply the ε-invariance test: measure each reading''s base extractiveness using the constraint''s operation as measured from each reading''s institutional position. If the three readings produce substantially different ε values (e.g., Arian ε ≈ 0.6 under semi-Arian dominance, Pro-Nicene ε ≈ 0.8, semi-Arian ε ≈ 0.15), the decomposition is justified and each reading is a separate constraint. If ε converges, they may be a single constraint with multi-seat perspective.',
    'Decomposition (three separate constraints, linked via network.affects_constraints) supports the authored structure and enables per-reading analysis. Convergence suggests the kernel should be a single constraint with multiple perspectives—a different authoring approach. The prompt directs decomposition, so this omega documents the risk that decomposition may be incorrect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_committer_ambiguity, conceptual, 'Whether the kernel dispute is properly represented as three independent constraints or as one constraint with multi-seat perspective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_christology__semi_arian_reading, 325, 381).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t325, homoousios_christology__semi_arian_reading, theater_ratio, 325, 0.22).
narrative_ontology:measurement(homo_tr_t340, homoousios_christology__semi_arian_reading, theater_ratio, 340, 0.38).
narrative_ontology:measurement(homo_tr_t355, homoousios_christology__semi_arian_reading, theater_ratio, 355, 0.52).
narrative_ontology:measurement(homo_tr_t370, homoousios_christology__semi_arian_reading, theater_ratio, 370, 0.55).
narrative_ontology:measurement(homo_tr_t378, homoousios_christology__semi_arian_reading, theater_ratio, 378, 0.58).
narrative_ontology:measurement(homo_tr_t381, homoousios_christology__semi_arian_reading, theater_ratio, 381, 0.15).

% Extraction over time
narrative_ontology:measurement(homo_be_t325, homoousios_christology__semi_arian_reading, base_extractiveness, 325, 0.18).
narrative_ontology:measurement(homo_be_t340, homoousios_christology__semi_arian_reading, base_extractiveness, 340, 0.35).
narrative_ontology:measurement(homo_be_t355, homoousios_christology__semi_arian_reading, base_extractiveness, 355, 0.47).
narrative_ontology:measurement(homo_be_t370, homoousios_christology__semi_arian_reading, base_extractiveness, 370, 0.43).
narrative_ontology:measurement(homo_be_t378, homoousios_christology__semi_arian_reading, base_extractiveness, 378, 0.38).
narrative_ontology:measurement(homo_be_t381, homoousios_christology__semi_arian_reading, base_extractiveness, 381, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t325, homoousios_christology__semi_arian_reading, suppression_requirement, 325, 0.25).
narrative_ontology:measurement(homo_su_t340, homoousios_christology__semi_arian_reading, suppression_requirement, 340, 0.42).
narrative_ontology:measurement(homo_su_t355, homoousios_christology__semi_arian_reading, suppression_requirement, 355, 0.58).
narrative_ontology:measurement(homo_su_t370, homoousios_christology__semi_arian_reading, suppression_requirement, 370, 0.61).
narrative_ontology:measurement(homo_su_t378, homoousios_christology__semi_arian_reading, suppression_requirement, 378, 0.54).
narrative_ontology:measurement(homo_su_t381, homoousios_christology__semi_arian_reading, suppression_requirement, 381, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_christology__semi_arian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(homoousios_christology__semi_arian_reading, 0.2).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__pro_nicene_reading).
narrative_ontology:affects_constraint(homoousios_christology__semi_arian_reading, homoousios_christology__arian_reading).

% DUAL FORMULATION NOTE:
% The homoousios_christology kernel decomposes into three constraint stories representing three live doctrinal readings competing for institutional dominance in the post-Nicene period (325–381). The semi_arian_reading is the compromise position: it asserts Christ is homoiousios (similar but not identical substance) rather than homoousios (identical) or subordinate (created). This reading INFLUENCES both the pro_nicene and arian readings by occupying institutional space and forcing them to position themselves relative to the middle. The semi_arian_reading's ε (0.41) is lower than the pro_nicene reading (expected ~0.65: stricter, more enforcement) but higher than a pure rope (genuine coordination carries lower extraction and theater). The reading's eventual abandonment at Council of Constantinople (381) in favor of pro_nicene homoousios reveals the semi_arian compromise as historically transient and extractive. Each reading should be authored as an independent constraint with its own stakeholder structure, beneficiary/victim declarations, and ε invariance; the three are linked via network.affects_constraints to establish the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
