% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: Sovereignty Maximalist Reading of RBIO Practice Norms
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty_maximalist_reading of
 *   the rbio_practice_norm_complex kernel — the claim that state sovereignty
 *   is absolute and RBIO (Rights-Based International Order) norms are
 *   legitimate ONLY when they protect sovereignty against external
 *   interference, with the corollary that humanitarian exceptions are
 *   pretexts for regime change. From this reading's frame, the RBIO system is
 *   a structure that protects weaker states from hegemonic intervention
 *   (founding problem: real) but has been corrupted by humanitarian rhetoric
 *   that serves regime-change projects (functional shift: observed). The
 *   reading vindicates Westphalian sovereignty doctrine and the
 *   non-intervention principle, and delegates accountability authority
 *   entirely to states. This reading is one vertex in a three-reading kernel
 *   contest; the other two readings (liberal_institutional,
 *   hegemonic_extraction) are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Authoritarian regimes: invoke this reading to shield themselves from accountability; beneficiary seat.
 *   - Domestic populations under repressive government: face repression with no external recourse; victim seat.
 *   - Liberal institutional advocates: promote human rights conditionality; payer and excluded seat — delegitimized by the reading itself.
 *   - Cross-border humanitarian advocates: attempt external solidarity; payer seat.
 *   - Hegemonic powers: navigate between rhetoric and interest; observer seat with arbitrage exit (selective enforcement).
 *   - International human rights bodies: issue findings and accountability; excluded seat.
 *   - Analytical observer: sees the full structure — the reading's origin in weaker-state protection, its capture by authoritarian regimes, and the asymmetric cost distribution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.78).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, snare).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "Sovereignty Maximalist Reading of RBIO Practice Norms").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, '82eb02a9-547f-4cdf-bc75-d4e26bb62515').
narrative_ontology:cs_kernel_codification('82eb02a9-547f-4cdf-bc75-d4e26bb62515', formalized).
narrative_ontology:cs_authority_grounding('82eb02a9-547f-4cdf-bc75-d4e26bb62515', lineage).
narrative_ontology:cs_interpretation_layer_present('82eb02a9-547f-4cdf-bc75-d4e26bb62515').
narrative_ontology:cs_reading_relation('82eb02a9-547f-4cdf-bc75-d4e26bb62515', rbio_practice_norm_complex__liberal_institutional_reading, forecloses).
narrative_ontology:cs_reading_relation('82eb02a9-547f-4cdf-bc75-d4e26bb62515', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('82eb02a9-547f-4cdf-bc75-d4e26bb62515', foundational, sovereignty_absolutism).
narrative_ontology:cs_axiom_status(sovereignty_absolutism, holdable).
narrative_ontology:cs_axiom_grounding('82eb02a9-547f-4cdf-bc75-d4e26bb62515', sovereignty_absolutism, deontological).
narrative_ontology:cs_axiom('82eb02a9-547f-4cdf-bc75-d4e26bb62515', foundational, humanitarian_intervention_illegitimacy).
narrative_ontology:cs_axiom_status(humanitarian_intervention_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('82eb02a9-547f-4cdf-bc75-d4e26bb62515', humanitarian_intervention_illegitimacy, deontological).
narrative_ontology:cs_reference_frame('82eb02a9-547f-4cdf-bc75-d4e26bb62515', westphalian_sovereignty_doctrine).
narrative_ontology:cs_drift_state('82eb02a9-547f-4cdf-bc75-d4e26bb62515', contemporary_atrocity_prevention_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('82eb02a9-547f-4cdf-bc75-d4e26bb62515', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, domestic_populations_under_repressive_government).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, cross_border_humanitarian_advocates).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1945, early UN, genuine protection motive mixed with enforcement capacity limits) to 0.78 (2026, the reading is now weaponized shield for systematic atrocity and suppression of accountability). Theater rises from 0.08 to 0.41: the security justification (protection from intervention) remains rhetorically present but an increasing share of the reading's enforcement action defends the ability to commit atrocities without international interference — functional drift from protection to extraction. Suppression requirement (enforcement force needed to hold the reading in place against resistance) rises from 0.48 to 0.72 because affected populations and humanitarian advocates increasingly reject the reading's logic, requiring more active delegitimization and exclusion to maintain it. The constraint claims to be a rope (coordination protecting weaker states) but measures as a snare (extraction shielding repression). Accessibility collapse is moderate (0.38) because alternatives exist — affected populations can seek refuge, humanitarian advocates can pressure states through other channels, international bodies can work around the reading — but each exit option is costly. Resistance is high (0.69) because the reading faces sustained pushback from human rights movements, victim populations, and liberal institutional advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the authoritarian-regime seat: this reading is genuine coordination, protection against hegemonic interference. It is a Rope or even a Mountain (natural law of sovereignty). From the domestic-victim seat: this reading is a Snare that locks them inside a repressive state with no external recourse, actively suppressing their ability to appeal for help. From the liberal-institutional seat: this reading is a false summit — it masquerades as coordination but serves extraction by preventing accountability. The engine computes each seat's type from power, exit, and beneficiary/victim structure; the perspectival gap is the entire point of per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes benefit (d near 0.0, beneficiary: the reading shields them from external accountability pressure, no cost to exit — they invoke it when it suits them, ignore it when convenient). Domestic populations are trapped targets (d near 1.0, victims: the reading forecloses their external recourse, they cannot exit their state's jurisdiction without extraordinary cost, they bear the extraction of accountability removal). Liberal institutional advocates and human rights bodies are also targets (d near 0.85: they pay through delegitimization and institutional marginalization, but have more exit optionality than domestic populations — they can shift strategies, exit the human rights advocacy space, or appeal to different audiences). Hegemonic powers occupy an unusual position (d near 0.5: they benefit from selective enforcement optionality, but also constrained by the reading's invocation against their own interests when they want to intervene). The reading's legitimacy claim (protecting weaker-state sovereignty) provides cover for its extraction (shielding repression), which is the core asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty protection reading was built for a genuine coordination problem: preventing arbitrary great-power intervention in weaker states. That problem persists in form but is substantially solved in practice through deterrence (nuclear weapons, institutional constraints, soft-power costs, regional coalitions). The reading's function has drifted: it now primarily shields authoritarian regimes from humanitarian pressure, not weaker states from hegemonic intervention. The reading is not dead (it is actively invoked and enforced), but its founding mandate has atrophied — it persists through institutional inertia and because authoritarian regimes have captured it as a tool. This is not a piton (which would show high theater and low extraction) but a snare that took the shape of a legitimate protection norm. The classification distinguishes this from a genuine rope by the presence of victims with no choice and an enforcement machinery that actively suppresses alternative framings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the founding protection problem (preventing arbitrary great-power intervention in weaker-state affairs) still live, or has it been substantially solved by other means (deterrence, institutional constraints, power distribution changes)?',
    'Historical analysis of intervention patterns pre/post Cold War; assessment of whether weaker states invoke this reading to prevent intervention they actually face, or primarily to shield internal repression.',
    'If the founding problem is dead, the reading qualifies as Piton (atrophied function, maintained by inertia). If live, the reading carries genuine coordination benefit that partially justifies its extraction cost — a Tangled Rope rather than a pure Snare. The measured mandatrophy depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the reading''s founding mandate remains live or has atrophied.').

omega_variable(
    authoritarian_regime_capture_vs_genuine_protection,
    'Was this reading captured by authoritarian regimes (who seized a protection norm and weaponized it), or did authoritarian regimes emerge as primary beneficiaries because the reading was always structured to shield state power regardless of its use?',
    'Genealogical analysis of when authoritarian regimes began invoking the reading as shield for repression vs. when liberal states invoked it as protection for weaker states; discourse analysis of how the reading''s rhetorical function shifted.',
    'If capture occurred, the reading might be reformed by excluding authoritarian-regime invocations. If the reading was structurally available for extraction from inception, reform would require replacing it. The underlying beneficiary structure either reflects historical contingency (reclaimable) or structural necessity (terminal).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authoritarian_regime_capture_vs_genuine_protection, conceptual, 'Whether authoritarian regime beneficiary status is capture or structural.').

omega_variable(
    humanitarian_exception_legitimacy,
    'Are humanitarian exceptions to sovereignty legitimate when target populations face systematic atrocity, or do they inevitably serve as pretexts for regime change and geopolitical extraction?',
    'Comparative analysis of humanitarian interventions: which were undertaken against geopolitical interests (establishing legitimacy), which were undertaken to serve them (establishing pretext pattern). Assessment of whether non-intervention leaves populations vulnerable to atrocity (harm from inaction) vs. humanitarian exception enabling regime change (harm from action).',
    'This is the crux of the kernel contest. If humanitarian exceptions are legitimate, the reading is false (Tangled Rope, not Snare). If they are pretexts, the reading is vindicated (Snare, not Rope). No amount of empirical evidence fully resolves this because it involves normative judgment about acceptable harm and legitimate authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_exception_legitimacy, preference, 'Whether humanitarian exceptions are legitimate authority or regime-change pretext.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (external pressure from states invoking the reading, legal barriers to humanitarian intervention) or internalized (affected populations and advocates have internalized the reading''s rejection of external help as philosophically legitimate)?',
    'Post-exit suppression trajectory: if humanitarian advocates or populations who exit the constraint''s jurisdiction cease citing the reading''s logic as grounds to avoid external pressure, suppression is primarily structural. If they continue to reject external help after the constraint is removed, suppression is partially internalized.',
    'Structural suppression is removable by reforming or replacing the reading. Internalized suppression persists as cognitive pattern even after the reading''s enforcement is dismantled, requiring deeper re-education. The effective suppression cost differs between these mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized.').

omega_variable(
    kernel_reading_committer_frame,
    'Which reading of the rbio_practice_norm_complex kernel is structurally correct? Is sovereignty actually absolute (sovereignty_maximalist), or are RBIO norms universal and revisable (liberal_institutional), or is RBIO a frozen hegemonic project (hegemonic_extraction)?',
    'This omega records the irreducible uncertainty of the kernel contest itself. No single reading is definitive; the contest is constitutive of the constraint. The engine''s per-seat classification computes each reading''s extracted cost from each seat; the kernel container accommodates all three readings as separate stories (separate ε values, separate beneficiary structures).',
    'Different readings produce different victim sets, different enforcement requirements, and different legitimacy claims. No uncontested resolution exists. The corpus houses all three readings as separate constraint stories, linked via network.affects_constraints, to enable comparative analysis of how the same kernel generates contradictory claims depending on which reading is instantiated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_frame, conceptual, 'Irreducible kernel contest: multiple readings of the same commitment system produce incompatible legitimacy verdicts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1945, 0.08).
narrative_ontology:measurement(rbio_tr_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(rbio_tr_t1980, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(rbio_tr_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement(rbio_tr_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(rbio_tr_t2026, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 2026, 0.41).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(rbio_be_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(rbio_be_t1980, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(rbio_be_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(rbio_be_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2015, 0.73).
narrative_ontology:measurement(rbio_be_t2026, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 2026, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1945, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement(rbio_su_t1960, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1960, 0.54).
narrative_ontology:measurement(rbio_su_t1980, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(rbio_su_t2000, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(rbio_su_t2015, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(rbio_su_t2026, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.12).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the rbio_practice_norm_complex kernel. The liberal_institutional_reading and hegemonic_extraction_reading are sibling constraints in the same kernel family. All three stories share a common founding problem (how to balance sovereignty protection with accountability) but decompose into different constraint types with different beneficiaries, different ε values, and different victim structures. The readings coexist as live positions held by different parties in international relations. The sovereignty_maximalist reading forecloses the liberal_institutional reading's claim that humanitarian intervention can be legitimate (they directly contradict on intervention authority), but coexists with the hegemonic_extraction reading's observation that selective enforcement reveals extraction intent (both readings are skeptical of liberal institutional framing, though for different reasons).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rbio_practice_norm_complex__sovereignty_maximalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
