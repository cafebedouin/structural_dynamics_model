% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology: Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The immutable diagnostic reading of the deferential realism ontology
 *   claims that the constraint typology itself is an observational instrument
 *   with fixed referents: mountains are observer-independent physical
 *   invariants, snares are measurable extraction mechanisms, and
 *   misclassification is an error correctable through better observation.
 *   This reading treats epsilon values, suppression metrics, and
 *   classification types as discoverable properties of constraints rather
 *   than as constructed categories within a particular epistemological
 *   framework. The constraint operates at the metalevel—it governs how
 *   institutional epistemic authority is distributed among competing
 *   frameworks for understanding what constraints ARE. The immutable reading
 *   suppresses alternative framings (pragmatist instrumentalism,
 *   constructivist reflexivity) by insisting that the metrics themselves
 *   settle disputes about classification. This creates a structural tension:
 *   the reading claims to be objective and measurement-based, yet its own
 *   enforcement mechanism exhibits high suppression of competing
 *   interpretations. The trajectory shows increasing extractiveness (0.42 →
 *   0.68) and suppression (0.55 → 0.72) as the reading consolidates
 *   institutional authority over the past decade. Theater remains moderate
 *   (0.58) because the framework maintains genuine methodological standards
 *   even while using those standards to delegitimize alternatives.
 *
 * KEY AGENTS:
 *   - Metric-Oriented Epistemic Authority: Primary beneficiary (institutional/arbitrage) — consolidates legitimacy by defining what counts as valid knowledge claim
 *   - Observationalist Research Program Coalition: Secondary beneficiary (organized/constrained) — coordinates around shared observational standards while extracting from excluded frameworks
 *   - Pragmatist Epistemology: Primary victim (powerless/trapped) — cannot defend its framework without appearing to reject 'evidence-based' reasoning within dominant institutions
 *   - Constructivist Frameworks: Primary victim (moderate/constrained) — early-career scholars face career penalties for pursuing non-observationalist epistemologies
 *   - Interpretive Flexibility: Collective victim (powerless/trapped) — the recognition that frameworks are constructed is delegitimized as 'relativism' or 'postmodern obscurantism'
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a particular reading as universal truth
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.68).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.72).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, snare).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology: Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '9d65fa32-5aec-4acc-9444-d1dd6b5828c1').
narrative_ontology:cs_kernel_codification('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', distributed).
narrative_ontology:cs_authority_grounding('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', extraction).
narrative_ontology:cs_reading_relation('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', foundational, metrics_are_discoverable).
narrative_ontology:cs_axiom_status(metrics_are_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', metrics_are_discoverable, empirically_contingent).
narrative_ontology:cs_axiom('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', foundational, classification_error_is_correctable).
narrative_ontology:cs_axiom_status(classification_error_is_correctable, holdable).
narrative_ontology:cs_axiom_grounding('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', classification_error_is_correctable, instrumental).
narrative_ontology:cs_reference_frame('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', objective_observational_framework).
narrative_ontology:cs_drift_state('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', contemporary_epistemology_wars, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9d65fa32-5aec-4acc-9444-d1dd6b5828c1', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, metric_oriented_epistemic_authority).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, observationalist_research_programs).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, pragmatist_epistemology).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_frameworks).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, interpretive_flexibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRAGMATIST EPISTEMOLOGIST (SNARE) — Trapped at the universal/civilizational scale. Cannot exit the framework without abandoning epistemological coherence within the academy. The immutable diagnostic reading suppresses alternative framings by treating its own metrics as discoverable rather than constructed. Pragmatists bear the cost of exclusion from legitimacy while the framework extracts authority by naturalizing one observational regime.
constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: EARLY-CAREER CONSTRUCTIVIST SCHOLAR (SNARE) — Constrained by career incentives and publication venues dominated by metric-oriented frameworks. Cannot openly challenge the observationalist regime without reputational damage. The constraint extracts conformity from scholars who privately doubt the framework but cannot exit without career cost.
constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: METRIC-ORIENTED EPISTEMIC AUTHORITY (ROPE) — Institutional beneficiary. Experiences the framework as pure coordination: organizing epistemic disputes by appeal to measurable observables solves genuine collective action problems. The authority has arbitrage capacity (can switch frameworks or appeal to alternative authority structures) but chooses to reinforce the observationalist regime because it consolidates institutional power and legitimacy.
constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OBSERVATIONALIST RESEARCH PROGRAM COALITION (TANGLED ROPE) — Organized beneficiaries who genuinely coordinate around shared observational standards while simultaneously extracting from excluded frameworks. They experience both a real coordination function (unified measurement protocols, data standards, replicability norms) and asymmetric extraction (alternative epistemologies are denied epistemic legitimacy). The constraint is active enforcement of methodological hierarchy dressed as methodological rigor.
constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: NATURALIZED OBJECTIVITY CLAIM (PITON) — The framing that observable metrics ARE the constraint's true nature (rather than one reading of a contested kernel) is a degraded institutional narrative maintained through theater. The performative invocation of 'scientific rigor' and 'measurable evidence' carries authority through repetition and institutional weight, but the underlying justification has atrophied. The objectivity claim persists through inertia and institutional momentum, not because the epistemological foundations remain robust.
constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT CANDIDATE) — From a universal/civilizational analytical perspective, the immutable diagnostic reading claims that the DR typology's classifications map to observer-independent reality: mountains are physical invariants, snares are extraction mechanisms, misclassification is error correctable through better observation. This appears as a natural law of epistemic structure. However, the structural data reveals this as a false summit: the claimed observables (extractiveness, suppression, theater) are themselves constructed within a particular reading of the framework. The 'observational instrument with fixed referents' claim naturalizes a contingent interpretive choice.
constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(deferential_realism_ontology__immutable_diagnostic_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, TR),
    TR >= 0.70.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The immutable reading extracts conformity by (a) establishing metric-based authority as the sole legitimate adjudicating mechanism, (b) treating competing epistemologies as errors correctable through 'better observation,' (c) rewarding scholars who adopt the framework and penalizing those who challenge it. The rising trajectory reflects consolidation: as the framework becomes institutionalized, its extractive power increases—dissent becomes not just disagreement but disciplinary heresy. Suppression (0.72): High and rising. The reading suppresses alternatives through (a) excluding them from high-status publication venues, (b) framing non-observationalist work as 'not rigorous,' (c) delegitimizing reflexivity about the framework itself as 'navel-gazing' or 'relativism,' (d) structuring research funding and hiring around observationalist methodologies. The rising trajectory indicates enforcement intensification: the reading's institutional position strengthens, and with it the capacity to suppress dissent. Theater (0.58): Moderate and stable. The reading maintains genuine methodological standards and real measurement practices, but increasingly uses those standards performatively—not to correct error, but to enforce conformity. The theater_ratio remains moderate rather than reaching the piton threshold because the underlying methodology is substantive, even as it is deployed for extraction.
 *
 * PERSPECTIVAL GAP:
 *   The immutable diagnostic reading exhibits extreme perspectival divergence. The metric-oriented authority sees a coordination mechanism (Rope)—unified standards for knowledge claims solve genuine epistemic collective action problems. The pragmatist epistemologist sees pure extraction (Snare)—the metrics are arbitrary choices dressed up as discoverable properties, and the framework extracts conformity while suppressing alternatives. The early-career constructivist sees constrained extraction (Snare)—they cannot exit without career damage, and the constraint forces them into methodological conformity. The observationalist coalition sees mixed coordination and extraction (Tangled Rope)—they genuinely coordinate around standards, but they simultaneously extract from excluded frameworks. The naturalized objectivity claim itself (Piton) is increasingly degraded—the performative invocation of 'scientific rigor' carries institutional weight, but the epistemological justification for treating metrics as fixed referents has become attenuated. The analytical observer (Mountain) risks seeing the immutable reading's claims as natural law—'the typology captures fixed referents'—but this is the false summit that triggers FSM: the reading is one contested interpretation of a contested kernel, naturalized through institutional authority.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality from beneficiary/victim declarations plus power/exit context. The metric-oriented authority benefits (low d) with institutional power and arbitrage exit capacity (can adopt alternative frameworks without cost). The pragmatist epistemologist is victimized (high d) with powerless structural position and trapped exit (cannot exit without abandoning professional identity). The early-career constructivist faces constrained exit and victim status (moderate d). These derivations produce chi values proportional to the agent's experienced extraction: the authority experiences low or negative chi (benefits more than costs), the powerless victim experiences high chi (maximum extraction), the constrained victim experiences moderate chi. The snare classification emerges from victim presence plus high baseline extraction and suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exposing how one reading of a contested kernel can claim the status of natural law. The immutable diagnostic reading asserts that the DR typology maps to observer-independent reality (Mountains are real; Snares are real; correct classification is discoverable). This is the false summit—the reading naturalizes its own interpretive commitments by treating them as fixed referents. The pragmatist and constructivist alternative readings are excluded from legitimacy not because they are wrong (the omega variables document that the question is unresolved), but because they challenge the narrative that metrics are discovered rather than constructed. Resolving the mandatrophy requires acknowledging that the typology is powerful precisely because it can be applied to alternative epistemologies themselves: the immutable reading is a Snare (high extraction, high suppression, beneficiaries+victims) when viewed from pragmatist or constructivist positions. No single perspective gets to claim universality. The six-type repertoire applies to the framework itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observables_constructed_or_discovered,
    'Are extractiveness, suppression, and theater_ratio discovered properties of constraints or constructed metrics that depend on the measurement framework?',
    'Demonstrate whether epsilon values remain invariant across different decompositions of the same constraint (ε-invariance principle). If epsilon changes when the observable changes, epsilon is framework-dependent. If epsilon is stable, the metrics are observer-independent.',
    'If constructed: the immutable diagnostic reading is circular (using metrics it constructed to justify the metrics). If discovered: the reading''s claims about fixed referents are vindicated, and the framework captures real structural differences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(observables_constructed_or_discovered, conceptual, 'Whether epsilon values are discovered or constructed properties').

omega_variable(
    sibling_reading_foreclosure_status,
    'Does the immutable diagnostic reading logically foreclose the pragmatist and constructivist alternatives, or do they coexist as live positions?',
    'Test whether a framework can coherently hold both the immutable diagnostic reading (fixed referents, error-correctable misclassification) and the pragmatist reading (metrics are tools for institutional coordination) simultaneously without contradiction.',
    'If foreclosed: the immutable reading is the unique correct framework, and sibling readings are errors. If coexists: different epistemic communities can occupy different readings without logical contradiction, indicating the kernel is genuinely contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_status, conceptual, 'Whether alternative readings are logically foreclosed or coexistent').

omega_variable(
    suppression_of_alternatives_measurement,
    'Is the measured suppression value (0.72) actually measuring suppression of alternative epistemologies (as the reading claims) or is it measuring something else (institutional authority concentration, incentive misalignment, legitimate methodological coherence)?',
    'Decompose the suppression metric into components: (a) active exclusion of competing frameworks from publication and funding; (b) incentive structures that penalize methodological pluralism; (c) coherence requirements that may be legitimate epistemic standards. Map each component to its true driver.',
    'If (a)+(b) dominate: the suppression is genuine extraction masquerading as methodological rigor. If (c) dominates: the constraint may be legitimate epistemological structure rather than extraction. High (a)+(b) validates the snare classification; high (c) validates the rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives_measurement, empirical, 'What actually constitutes the measured suppression value').

omega_variable(
    false_summit_natural_law_claim,
    'Is the claim that ''the constraint typology is an observational instrument with fixed referents'' a natural law or a constructed institutional narrative?',
    'Examine whether the framework''s classifications would be invariant under different social/institutional configurations. If a constraint classified as snare could legitimately be classified as rope by a differently-structured epistemic community, then the classification depends on the community, not on fixed referents.',
    'If natural law: the immutable reading is correct and alternative readings are errors. If constructed: the immutable reading is one political reading of an inherently contested kernel, and FSM applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_claim, conceptual, 'Whether the typology captures fixed referents or constructed narratives').

omega_variable(
    interpretive_layer_authority_structure,
    'What authority structure is currently interpreting the DR kernel, and is that interpretation layer functioning to absorb drift or preventing it?',
    'Identify the institutions/communities currently mediating interpretations of the DR framework (academic epistemology departments, philosophy of science, the DR framework''s own maintainers). Assess whether their interpretations have shifted over time without explicit kernel revision (interpretation absorbing drift) or whether they enforce invariance of the kernel against reinterpretation pressure (suppression of drift).',
    'If drift-absorbing: the kernel is stable because the interpretation layer is flexible. If drift-suppressing: the kernel appears stable but is actually fragile; alternative interpretations are being excluded through institutional authority rather than epistemic argument.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_layer_authority_structure, empirical, 'Authority structure''s drift-handling mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drimmut_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(drimmut_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.53).
narrative_ontology:measurement(drimmut_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(drimmut_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(drimmut_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(drimmut_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(drimmut_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(drimmut_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(drimmut_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_epistemology_wars).

% DUAL FORMULATION NOTE:
% The immutable_diagnostic_reading is one reading of a contested kernel. Its sibling readings (rhetorical_scaffold, hybrid_pragmatic) will have different epsilon values reflecting different views of what the constraint actually is. The immutable reading claims ε≈0.68 (high extraction) because the reading holds that suppressed alternatives are genuinely excluded. The pragmatist reading would classify differently (lower extraction, higher coordination) because it sees the same institutional mechanisms as legitimate tool-building rather than suppression. Decomposition is not optional here—the readings instantiate structurally different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
