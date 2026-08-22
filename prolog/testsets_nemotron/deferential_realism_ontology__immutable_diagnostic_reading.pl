% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology — Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story represents the 'immutable diagnostic reading' of
 *   the deferential_realism_ontology kernel. It asserts that the six-category
 *   constraint typology (mountain, rope, tangled_rope, snare, scaffold,
 *   piton) is an observational instrument with fixed referents: mountains
 *   correspond to physical/logical invariants, snares to measurable
 *   extraction mechanisms, and misclassification is an observational error
 *   correctable through better measurement. The reading claims the typology
 *   emerges naturally from the structure of constraints themselves, not from
 *   normative choices about what counts as legitimate coordination vs.
 *   extraction. High suppression of alternative framings (0.58 and rising) is
 *   authored as a structural feature — the reading's axioms logically exclude
 *   the 'normative vocabulary' and 'hybrid pragmatic' readings. Beneficiaries
 *   are the framework authors, analysts who use it, and engine implementers
 *   who depend on its stability. The claim/metric independence rule is
 *   observed: claimed_type is mountain (the reading's self-understanding),
 *   while metrics describe a constraint with rising suppression and theater —
 *   the engine will compute whether the structural data supports the mountain
 *   claim or detects a false summit.
 *
 * KEY AGENTS:
 *   - framework_authors: Primary agenda_setter (institutional/biographical/arbitrage) — define and maintain the typology
 *   - institutional_analysts_using_framework: Beneficiary (organized/biographical/mobile) — apply the framework to domains
 *   - engine_implementers: Beneficiary (institutional/generational/arbitrage) — encode the classification logic in software
 *   - rhetorical_scaffold_proponents: Excluded (organized/biographical/trapped) — hold the competing reading that classification is normative vocabulary
 *   - hybrid_pragmatic_proponents: Excluded (moderate/biographical/constrained) — hold the competing reading that periphery categories involve normative judgment
 *   - analytical_observer: Observer (analytical/civilizational/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.18).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.58).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, mountain).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology — Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '4ffad88b-8a57-4df8-988b-32020c14366a').
narrative_ontology:cs_kernel_codification('4ffad88b-8a57-4df8-988b-32020c14366a', formalized).
narrative_ontology:cs_authority_grounding('4ffad88b-8a57-4df8-988b-32020c14366a', expertise).
narrative_ontology:cs_interpretation_layer_present('4ffad88b-8a57-4df8-988b-32020c14366a').
narrative_ontology:cs_reading_relation('4ffad88b-8a57-4df8-988b-32020c14366a', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('4ffad88b-8a57-4df8-988b-32020c14366a', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('4ffad88b-8a57-4df8-988b-32020c14366a', foundational, classification_is_observational_not_nominal).
narrative_ontology:cs_axiom_status(classification_is_observational_not_nominal, holdable).
narrative_ontology:cs_axiom_grounding('4ffad88b-8a57-4df8-988b-32020c14366a', classification_is_observational_not_nominal, deontological).
narrative_ontology:cs_axiom('4ffad88b-8a57-4df8-988b-32020c14366a', foundational, epsilon_values_are_discoverable_not_constructed).
narrative_ontology:cs_axiom_status(epsilon_values_are_discoverable_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('4ffad88b-8a57-4df8-988b-32020c14366a', epsilon_values_are_discoverable_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('4ffad88b-8a57-4df8-988b-32020c14366a', foundational, mountains_are_physical_invariants).
narrative_ontology:cs_axiom_status(mountains_are_physical_invariants, holdable).
narrative_ontology:cs_axiom_grounding('4ffad88b-8a57-4df8-988b-32020c14366a', mountains_are_physical_invariants, empirically_contingent).
narrative_ontology:cs_axiom('4ffad88b-8a57-4df8-988b-32020c14366a', foundational, snares_are_measurable_extraction_mechanisms).
narrative_ontology:cs_axiom_status(snares_are_measurable_extraction_mechanisms, holdable).
narrative_ontology:cs_axiom_grounding('4ffad88b-8a57-4df8-988b-32020c14366a', snares_are_measurable_extraction_mechanisms, empirically_contingent).
narrative_ontology:cs_reference_frame('4ffad88b-8a57-4df8-988b-32020c14366a', formalized_observational_typology).
narrative_ontology:cs_drift_state('4ffad88b-8a57-4df8-988b-32020c14366a', post_engine_implementation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4ffad88b-8a57-4df8-988b-32020c14366a', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, framework_authors).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutional_analysts_using_framework).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, engine_implementers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, classification_is_observational_not_nominal).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epsilon_values_are_discoverable).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, mountains_are_physical_invariants).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, snares_are_measurable_extraction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define, maintain, and authorize the constraint typology. Their epistemic authority and professional standing depend on the framework being accepted as an observational instrument rather than a normative choice. They control the canonical definitions, the engine implementation, and the publication venues where the framework is applied. Exit means abandoning their life's work and institutional position — identity_locked for the core authors, arbitrage for those with transferable formal-methods skills.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, framework_authors, agenda_setter,
    institutional, biographical, arbitrage, global).

% Apply the framework to classify constraints in their domains (policy, economics, organizational design). They benefit from a stable, observationally grounded vocabulary that lets them make diagnostic claims without normative justification. If the framework were reclassified as normative, their analyses would lose 'objective' standing. Exit is mobile — they can adopt alternative frameworks (Ostrom, Williamson, public choice) but face switching costs in retraining and credibility.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_analysts_using_framework, beneficiary,
    organized, biographical, mobile, global).

% Encode the classification logic in the generate_constraint_pl.py compiler and the Prolog engine. Their investment is in the computational stability of the types — if categories are observational, the engine computes; if normative, the engine requires value inputs they cannot supply. They benefit from the framework's claim to observational objectivity. Exit is arbitrage — their skills (compiler engineering, logic programming) transfer, but the specific domain model is a sunk cost.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, engine_implementers, beneficiary,
    institutional, generational, arbitrage, global).

% Hold the competing reading that 'snare' and 'tangled_rope' are not discovered categories but normative declarations made when a mechanism serves illegitimate beneficiaries. They are structurally excluded from the framework's authoritative venues — their papers are rejected as 'category errors,' their classifications treated as 'misuse of the framework.' They cannot exit the exclusion without abandoning their core epistemic claim (that classification is inherently normative). Their situation is trapped: the framework's axioms make their position unintelligible within its terms.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_proponents, excluded,
    organized, biographical, trapped, global).

% Accept the observational core (mountains, ropes as physical/coordination constraints) but argue the periphery (tangled_rope, snare, scaffold, piton) requires normative judgment about legitimate beneficiaries. They are partially included — their core agreement grants them some voice — but their periphery claim is treated as 'confusion about the observational method.' Exit is constrained: they can stay in the framework accepting its full observational claim, or leave for frameworks that explicitly embrace normative classification (critical theory, feminist epistemology), losing the coordination benefits of the shared core.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_proponents, excluded,
    moderate, biographical, constrained, global).

% Sees the full kernel structure: three readings of one contested ontology, each with different epsilon, different beneficiaries, different suppression mechanisms. Does not occupy any reading's internal logic. Observes that the immutable_diagnostic_reading's high accessibility_collapse and rising suppression are consistent with both a genuine mountain AND a false summit — the omega variables capture this ambiguity. The observer's classification is the engine's output, not an authored claim.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, diffuse).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, observationally grounded vocabulary for constraint classification that enables cross-domain analysis without normative dispute — analysts can say 'this is a snare' as a diagnostic claim, not a political accusation.
% TRANSFER_FUNCTION: Moves epistemic authority from the framework_authors (who define the categories) to the institutional_analysts and engine_implementers (who apply them), while extracting interpretive freedom from excluded readings (rhetorical_scaffold, hybrid_pragmatic) whose framings are declared 'non-observational.'
% ABSENT_VOICES: Rhetorical scaffold proponents (who would argue classification is inherently normative vocabulary) and hybrid pragmatic proponents (who would argue the periphery requires normative judgment) are structurally excluded — their framings are treated as category errors rather than live theoretical options. They exist in adjacent literatures (critical policy studies, pragmatic philosophy of science) but are not in the framework's conversation.
% DISAPPEARANCE_RATIONALE: If the immutable diagnostic reading vanished overnight, the deferential_realism_ontology kernel would not disappear — the rhetorical_scaffold_reading and hybrid_pragmatic_reading would become the dominant framings. Analysts would lose the 'observational instrument' warrant for their classifications and would need to justify category choices normatively. Engine implementers would need to build value-input layers. The framework's cross-domain analytical infrastructure would reorganize around normative or hybrid foundations.
% FOUNDING_PROBLEM: Constraint classification had collapsed into nominalism — any arrangement could be called 'coordination' or 'extraction' depending on the speaker's politics. There was no observational instrument to distinguish a physical invariant (gravity) from a constructed extraction (debt trap) except the speaker's assertion. The founding problem was to build a typology where categories correspond to measurable structural features (extraction, suppression, enforcement) so that misclassification is an empirical error, not a political disagreement.
% FOUNDING_PROBLEM_CORROBORATION: The framework_authors attest the problem is live (new domains still produce classification disputes that the typology resolves observationally). Rhetorical_scaffold_proponents attest the problem is dead or misdiagnosed — the 'nominalist collapse' was never the problem; the problem is that observation itself is theory-laden, and the framework's claim to observational purity is the extraction. Hybrid_pragmatic_proponents attest the problem is partially solved (core categories work observationally) but the periphery claim is false. No corroborating source outside the benefiting parties (framework_authors, analysts, implementers) affirms the founding problem as stated — the attestation comes entirely from within the reading's own coalition.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, ExtMetricName, E),
    domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.18) because the reading claims to be a passive observational instrument — it does not transfer resources. Suppression is moderate-high (0.58) because the reading's core axioms (classification_is_observational, epsilon_discoverable) structurally exclude rival readings that treat classification as normative or hybrid. Theater_ratio (0.22) reflects growing performative enforcement of the reading's boundaries (terminological policing, gatekeeping in publications) rather than the observational function itself. Accessibility_collapse is high (0.82) because once the reading's axioms are accepted, alternative framings appear as 'category errors' rather than live options. Resistance is low (0.15) because the reading presents itself as descriptive, not prescriptive — there is little to resist until the frame is imposed institutionally.
 *
 * PERSPECTIVAL GAP:
 *   From the framework_authors seat (agenda_setter), the constraint is a mountain — a discovered structure they merely formalize. From the rhetorical_scaffold_proponents seat (excluded), the same constraint is a snare — an epistemic regime that suppresses their normative vocabulary by declaring it 'not observational.' From the hybrid_pragmatic_proponents seat (excluded), it is a tangled_rope — a genuine coordination core (mountains, ropes) with an extractive periphery (the claim that snare/tangled_rope distinctions are purely observational). The engine computes these per-seat types from the structural data; the authored claim (mountain) is only the agenda_setter's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework_authors and engine_implementers are beneficiaries (d near 0.0): the constraint's stability and 'natural law' status secure their epistemic authority and implementation investments. Institutional_analysts are beneficiaries (d ~ 0.15): they gain a stable analytical instrument but bear some cost if the framework proves rigid. Rhetorical_scaffold_proponents and hybrid_pragmatic_proponents are excluded (identity_locked exit, d near 1.0): their alternative framings are structurally suppressed, not merely disagreed with. The analytical_observer sits at d = 0.5 (symmetric). The omega on beneficiary_ontology_ambiguity captures the uncertainty about whether beneficiaries genuinely coordinate or capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (creating an observationally grounded constraint typology that avoids nominalist collapse) is live — the reading claims the typology solves a genuine epistemological problem. However, the rising suppression and theater metrics suggest the arrangement may be accumulating extractive function (policing the boundary of what counts as 'observational'). If the founding problem is solved but the constraint persists with rising suppression, mandatrophy is unresolved. The reading's own axioms prevent it from recognizing this drift — the 'observational instrument' frame treats rising suppression as 'better measurement,' not as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is this constraint one reading of a contested kernel (deferential_realism_ontology) rather than a standalone classification?',
    'Structural comparison with sibling readings: if the immutable_diagnostic_reading''s core premise (classification is observational with fixed referents) directly contradicts the rhetorical_scaffold_reading''s core premise (classification is normative vocabulary) such that no single epistemic framework can hold both, the kernel framing is validated and the forecloses relation applies.',
    'If validated, this constraint is not a free-standing mountain but one reading in a kernel family — its ''mountain'' claim applies only within its own reading frame. The engine''s computed type for this reading remains mountain, but the network.affects_constraints and cs_structure.reading_relations document the structural dependency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the immutable diagnostic reading is a kernel reading rather than a standalone constraint.').

omega_variable(
    beneficiary_ontology_ambiguity,
    'Do the declared beneficiaries (framework_authors, institutional_analysts, engine_implementers) genuinely benefit from the constraint''s operation as coordination, or does the ''mountain'' claim itself function as a cover for their epistemic authority?',
    'Counterfactual test: if the framework were empirically falsified on a core claim (e.g., a purported mountain computes as snare from multiple seats), would the beneficiaries accept the reclassification or defend the framework''s categories? Their structural response reveals whether they are coordinated by the constraint or capture it.',
    'If beneficiaries would reject falsification, the constraint is a false summit mountain (FSM candidate) — the ''natural law'' claim serves their authority. If they would accept reclassification, the mountain claim is genuine and beneficiaries are coordinated observers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_ontology_ambiguity, conceptual, 'Whether the mountain''s beneficiaries are genuine coordinators or epistemic capturers.').

omega_variable(
    suppression_mechanism,
    'Is the suppression of alternative framings (rhetorical_scaffold_reading, hybrid_pragmatic_reading) structural — the framework''s logical architecture excludes them — or social — the framework''s institutional adoption marginalizes them?',
    'Trace the exclusion mechanism: if the immutable diagnostic reading''s axioms (classification_is_observational, epsilon_discoverable) logically entail the negation of sibling axioms, suppression is structural (forecloses). If the axioms merely make sibling readings ''incorrect by definition'' without logical entailment, and exclusion operates through institutional gatekeeping, suppression is social.',
    'Structural suppression (forecloses) is consistent with a mountain''s high accessibility_collapse. Social suppression suggests the constraint is a constructed epistemic regime — a tangled_rope or snare masquerading as a mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism, conceptual, 'Whether alternative framings are excluded by logical necessity or institutional power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drod_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(drod_tr_t0, observed).
narrative_ontology:measurement(drod_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(drod_tr_t5, observed).
narrative_ontology:measurement(drod_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(drod_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(drod_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(drod_be_t0, observed).
narrative_ontology:measurement(drod_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement_basis(drod_be_t5, observed).
narrative_ontology:measurement(drod_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.18).
narrative_ontology:measurement_basis(drod_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(drod_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(drod_su_t0, observed).
narrative_ontology:measurement(drod_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(drod_su_t5, observed).
narrative_ontology:measurement(drod_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(drod_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This is the immutable_diagnostic_reading of the deferential_realism_ontology kernel. The kernel decomposes into three constraint stories with distinct epsilon values: this reading (epsilon ~0.18, claims mountain), rhetorical_scaffold_reading (epsilon higher, claims snare/tangled_rope — classification as normative vocabulary is itself extractive), hybrid_pragmatic_reading (epsilon intermediate, claims tangled_rope — core coordinates, periphery extracts). The epsilon-invariance principle requires separate stories because the referent ('the constraint typology') yields different epsilon under different readings. This reading's axioms (classification_is_observational, epsilon_discoverable) structurally foreclose the scaffold reading's core axiom (classification_is_nominal) and partially foreclose the hybrid reading's periphery axiom (periphery_requires_normative_judgment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
