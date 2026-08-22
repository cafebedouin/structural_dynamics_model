% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Constraint Typology (Hybrid Pragmatic Reading)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_pragmatic reading of the
 *   deferential_realism_ontology kernel. The reading claims that the
 *   constraint typology has a fixed core (mountains and ropes) grounded in
 *   observationally stable facts about physical and coordination constraints,
 *   but a contested periphery (tangled_ropes and snares) where classification
 *   depends inescapably on normative judgments about legitimate beneficiaries
 *   and acceptable extraction. Unlike the immutable_diagnostic reading (which
 *   treats all classifications as objective facts correctable through better
 *   observation) or the rhetorical_scaffold reading (which treats the
 *   typology as primarily a persuasive vocabulary for policy critique), the
 *   hybrid reading maintains that coherence requires acknowledging where
 *   observation ends and judgment begins—and that both are necessary for the
 *   framework's utility.
 *
 * KEY AGENTS:
 *   - Research community epistemologists: maintain the reading's coherence, teach it, defend it against objections from competing readings.
 *   - Institutional designers (pragmatist school): benefit from a vocabulary that permits rigorous institutional diagnosis while being honest about normative commitments.
 *   - Policy advocates (rhetorical scaffold reading): bear costs from having their preferred frame (snares as declarative acts) presented as one option among others rather than the framework's purpose.
 *   - Strict observationalists (diagnostic reading): bear costs from having their epistemic frame (pure observation) challenged by the explicit claim that normative judgment is inescapable in peripheral cases.
 *   - Excluded radical constructivists: structurally barred from participating because their position (all categories are contingent) cannot be voiced without dissolving the framework's core/periphery distinction.
 *   - Interdisciplinary scholars: benefit from the framework's clarity about where cross-disciplinary consensus ends and domain-specific judgment begins.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.62).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Constraint Typology (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '29ef1769-3212-48c7-8e28-0703ecd4805f').
narrative_ontology:cs_kernel_codification('29ef1769-3212-48c7-8e28-0703ecd4805f', distributed).
narrative_ontology:cs_authority_grounding('29ef1769-3212-48c7-8e28-0703ecd4805f', expertise).
narrative_ontology:cs_interpretation_layer_present('29ef1769-3212-48c7-8e28-0703ecd4805f').
narrative_ontology:cs_reading_relation('29ef1769-3212-48c7-8e28-0703ecd4805f', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('29ef1769-3212-48c7-8e28-0703ecd4805f', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('29ef1769-3212-48c7-8e28-0703ecd4805f', foundational, core_periphery_distinction_stable).
narrative_ontology:cs_axiom_status(core_periphery_distinction_stable, holdable).
narrative_ontology:cs_axiom_grounding('29ef1769-3212-48c7-8e28-0703ecd4805f', core_periphery_distinction_stable, empirically_contingent).
narrative_ontology:cs_axiom('29ef1769-3212-48c7-8e28-0703ecd4805f', foundational, normative_judgment_inescapable_in_peripheral_classification).
narrative_ontology:cs_axiom_status(normative_judgment_inescapable_in_peripheral_classification, holdable).
narrative_ontology:cs_axiom_grounding('29ef1769-3212-48c7-8e28-0703ecd4805f', normative_judgment_inescapable_in_peripheral_classification, deontological).
narrative_ontology:cs_reference_frame('29ef1769-3212-48c7-8e28-0703ecd4805f', constraint_typology_with_epistemologically_honest_core_periphery).
narrative_ontology:cs_drift_state('29ef1769-3212-48c7-8e28-0703ecd4805f', contemporary_corpus_accumulation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('29ef1769-3212-48c7-8e28-0703ecd4805f', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, research_community_epistemologists).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers_committed_to_pragmatism).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, policy_advocates_rhetorical_reading).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, strict_observationalists_diagnostic_reading).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, emerging_interdisciplinary_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Epistemologists and constraint theorists operating within this reading adopt a framework that avoids false dichotomies (purely objective discovery vs. purely constructed convention). They benefit from a stabilized vocabulary that permits precise discussion of the core (mountains/ropes) while explicitly acknowledging the contested periphery (tangled_ropes/snares). They set the research agenda by publishing within this framework, training students in its discipline, and defending its coherence against challenges.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, research_community_epistemologists, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, research_community_epistemologists, agenda_setter).

% Policy-makers and institutional designers who adopt this reading benefit from a vocabulary that permits them to classify institutional constraints rigorously while remaining explicit about the normative choices embedded in peripheral classifications. The reading lets them communicate institutional diagnoses with honesty about where observation ends and judgment begins.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers_committed_to_pragmatism, beneficiary,
    organized, biographical, constrained, global).

% Advocates working within the rhetorical_scaffold_reading bear a cost: this hybrid reading refuses their framing that the typology is primarily a persuasive instrument. By insisting that some classifications (mountains/ropes) are observationally stable and that the framework's coherence depends on this stability, the hybrid reading constrains the rhetorical freedom they might otherwise exercise. Their preferred reading (that snares are declared based on normative judgment about legitimacy, making the entire framework a vocabulary for critique) is presented here as one among competing readings, not the framework's essential purpose.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, policy_advocates_rhetorical_reading, payer,
    powerful, biographical, constrained, global).

% Researchers committed to the immutable_diagnostic_reading (that the typology is a discovery instrument with fixed referents independent of reading) pay a cost in having their framework presented as one reading among others rather than as the framework itself. The hybrid reading's explicit acknowledgment that epsilon is reading-indexed (per OQ-26) and that peripheral classifications are normatively constructed directly challenges their observationalist epistemology. They remain in the discourse but are forced to defend their core premise publicly rather than assuming it.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, strict_observationalists_diagnostic_reading, payer,
    institutional, generational, constrained, global).

% Scholars from economics, organizational behavior, and political economy who encounter the constraint typology benefit from this reading because it clarifies what can be cross-disciplinarily established (core) versus what requires domain-specific normative judgment (periphery). They can adopt the framework without committing to an epistemological stance about whether snares are discovered or declared.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, emerging_interdisciplinary_community, beneficiary,
    moderate, biographical, mobile, global).

% Social constructivists who argue that all categories are contingent social productions (including mountains and ropes) are structurally excluded from this framework. The hybrid reading's insistence on a core/periphery distinction means their position—that all constraint classification is constructed—cannot be voiced within the framework without dissolving the framework itself. They are not represented in the deliberation.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, excluded_radical_constructivists, excluded,
    moderate, biographical, trapped, global).

% A philosophical observer (the auditor of the committer-axis framework itself) notes the hybrid reading's position: it claims that some truths are observational (mountains/ropes) while others are constructed (periphery). This observer has no stake in which reading is adopted but tracks whether the hybrid reading's internal logic holds and whether the core/periphery distinction is stable.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observer_philosophical_auditor, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__hybrid_pragmatic_reading, research_community_epistemologists).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified vocabulary for constraint classification that permits interdisciplinary communication while being explicit about where observation ends and normative judgment begins. Solves the coordination problem of how to classify institutional arrangements (snares vs. tangled_ropes) without collapsing into either pure objectivism or pure relativism.
% TRANSFER_FUNCTION: Transfers epistemic authority from radical constructivists (who would dissolve the core/periphery distinction) to pragmatists who maintain that distinction. Also transfers institutional authority from pure rhetorical advocates (who treat snare classifications as declarative acts) to institutional designers who insist on observational grounding even for peripheral classifications.
% ABSENT_VOICES: Radical constructivists are structurally excluded: their core claim (all categories are contingent constructions) cannot be voiced within this framework without dissolving the core/periphery distinction. Intuitionists or strict empiricists who believe the periphery is as observationally grounded as the core are also effectively absent: the framework defines their position as incoherent rather than as a live alternative.
% DISAPPEARANCE_RATIONALE: If this reading vanished and either immutable_diagnostic or rhetorical_scaffold became dominant, institutional constraint classification would reorganize. Researchers would no longer explicitly signal where normative judgment enters; policy debates would reframe around whether snares are discovered or declared; the vocabulary's utility for interdisciplinary work would shift; institutions would adopt either a pure observationalism or a pure critique-vocabulary, losing the hybrid frame's coherence.
% FOUNDING_PROBLEM: Early constraint theory oscillated between pure observationalism (treating all classifications as objective facts) and pure constructivism (treating all classifications as normative impositions), creating incoherence: mountains are empirically undeniable, yet snare classifications seemed to require normative premises. The founding problem: how to classify constraints coherently while being honest about the role of normative judgment?
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested by researchers across domains (economics, organizational theory, epistemology) who report confusion about when a constraint classification is 'discovered' vs. 'declared.' This is corroborated by scholars working in competing readings (diagnostic and rhetorical) who explicitly identify this tension as their point of departure. Competition authorities and institutional designers independently report this tension when trying to apply the framework to real constraints.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.58 because the hybrid reading extracts epistemic authority from both competing readings: it takes observational stability from the diagnosticists and acknowledges normative construction from the rhetoricians, but refuses full capitulation to either. The reading successfully constrains the interpretive freedom of advocates from both competing readings—rhetoricians cannot simply declare snares into existence without observational grounding, and observationalists cannot dismiss peripheral classifications as mere opinion without engaging the normative structure. Suppression is measured at 0.62 because maintaining the core/periphery distinction against both competing epistemologies requires active enforcement: the framework must exclude radical constructivists (who would dissolve the distinction) and marginalize observationalists (who deny the normative component). Theater is measured at 0.48—moderate—because part of the reading's work is performative: it performs the reconciliation of observation and judgment through careful framing, but its actual empirical claims about constraint classification remain testable. The measurement series shows extractiveness and suppression rising slightly then stabilizing (indicating initial institutional consolidation of the reading, then plateauing) and theater rising then declining (indicating initial rhetorical effort to establish the frame, then declining as it becomes normalized). Basis values shift from 'observed' in the first 15 time points (where evidence about current adoption and institutional traction is available) to 'projected' at 20 and 25 (where the trajectory depends on whether the corpus evidence will sustain or refute the reading's core premises).
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seats (epistemologists, institutional designers) experience this reading as coherence-enabling: it lets them do rigorous work without false epistemological commitments. The payer seats (rhetoricians, observationalists) experience the same reading as constraining: it blocks their preferred epistemic moves and forces public defense of premises they might have treated as foundational. The excluded radicals experience it as a barring mechanism—they cannot participate in the framework at all. The analytical observer (philosophical auditor) experiences the reading as a test case for whether the core/periphery distinction is stable under scrutiny.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from the reading's structural position on the epistemic map. Epistemologists who benefit from the framework experience low directionality (they are beneficiaries, d near 0.0) because the reading enables their research program. Institutional designers are slightly target-positioned (d near 0.35) because they must do normative work to apply the framework to actual constraints—they gain a vocabulary but must also make contestable normative judgments. Rhetorical advocates and observationalists are more target-positioned (d near 0.70) because they bear the cost of having their preferred epistemologies marginalized. Radical constructivists are identity-locked targets (d near 0.95) because the framework's entire core commitment excludes their position structurally.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to classify constraints coherently while being honest about normative judgment) is clearly live—researchers continue to report confusion about core vs. periphery. However, there is a latent mandatrophy risk: if corpus analysis systematically shows that the core/periphery distinction is unstable (that even mountains and ropes require normative framing to classify), then the reading's mandate (to identify and maintain observational grounding) becomes obsolete. Conversely, if the distinction proves stable, the reading's authority is strengthened. The risk is real because the constraint is self-referential: the framework's classification of itself as tangled_rope (coordination + extraction + active enforcement) depends on whether you accept that distinguishing core from periphery is both possible and necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    core_periphery_boundary_sharpness,
    'Is the core/periphery distinction in constraint classification a sharp boundary or a gradient? Where precisely does observational grounding end and normative judgment begin?',
    'Extended case analysis across 50+ institutional constraints, tracking at what point in the classification decision observationalists and normativists begin to diverge. A gradient would blur the distinction; true sharpness would show systematic divergence only at specific constraint types.',
    'If the boundary is fuzzy, the hybrid reading''s claim to a stable core is undermined; the constraint would need reclassification toward snare (the reading''s authority is contaminated). If the boundary is sharp, the reading is sustained.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(core_periphery_boundary_sharpness, empirical, 'Whether the core/periphery distinction is categorical or gradient.').

omega_variable(
    reading_indexing_of_epsilon_validity,
    'Is epsilon (base extractiveness) genuinely reading-indexed as OQ-26 states, or does each reading measure the same objective property and merely disagree about its interpretation?',
    'Systematic comparison of epsilon assignments for the same constraint across readings. If readings measure the same property, epsilon should converge despite different interpretations; if epsilon is reading-indexed (per OQ-26), different readings can author different epsilon values for the same constraint without incoherence.',
    'If epsilon is truly reading-indexed, the hybrid reading is coherent but concedes that classification is less objectively grounded than it claims. If epsilon converges across readings, the reading''s coherence is higher but OQ-26 is wrong.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexing_of_epsilon_validity, conceptual, 'Whether epsilon is an objective property or a reading-indexed construct.').

omega_variable(
    normative_judgment_unavoidability_in_tangled_rope,
    'For tangled_rope constraints (coordination + extraction), is the identification of asymmetric extraction always objective, or does it require normative judgment about ''legitimate'' distribution?',
    'Case studies of tangled_rope classifications where experts disagree: is the disagreement about facts (how much extraction is present) or about norms (what distribution is legitimate)? If all disagreement is factual, extraction identification is objective; if normative framing shifts what counts as asymmetric, judgment is inescapable.',
    'If judgment is inescapable even for tangled_rope, the hybrid reading''s core is more constructed than it claims. If judgment is avoidable, the distinction between core and periphery holds more sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_judgment_unavoidability_in_tangled_rope, empirical, 'Whether tangled_rope identification requires normative judgment about legitimate distribution.').

omega_variable(
    suppression_mechanism_interpretive_variance,
    'When suppression is measured, does the measurement reflect objective barriers (external constraints on exit) or internalized belief-structures (agents'' epistemic access to alternatives)? Does this distinction affect classification?',
    'Follow a constraint''s suppression trajectory after the external barriers are removed. If suppression persists (internalized), the constraint''s extractiveness may need upward revision; if suppression collapses (it was structural), the measurement was accurate. Track whether readings account for this differently.',
    'If readings diverge systematically on suppression mechanism, the hybrid reading''s stability is compromised; if they converge, suppression measurement has observational ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_interpretive_variance, empirical, 'Whether suppression is structural or internalized, and whether readings measure it differently.').

omega_variable(
    kernel_contest_stability,
    'Will the three readings of this kernel (hybrid_pragmatic, immutable_diagnostic, rhetorical_scaffold) remain live positions, or will one eventually become incoherent as the corpus accumulates evidence?',
    'Monitor corpus analysis over 18 months: do measurements and case studies systematically falsify one reading''s core premises? If one reading''s axioms are repeatedly overridden by evidence, it moves from holdable to overridden status within the kernel contest.',
    'If one reading becomes incoherent, the kernel may collapse from three coexisting readings to a dominant reading with marginal alternatives. This would shift the constraint''s classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_stability, empirical, 'Whether the three kernel readings will remain coexistent or one will be foreclosed by evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__hybrid_pragmatic_reading, 0.12).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, constraint_classification_epistemology_family).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, normative_judgment_in_institutional_design).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the deferential_realism_ontology kernel. The hybrid_pragmatic reading maintains a core/periphery distinction: mountains and ropes are observationally grounded, tangled_ropes and snares require normative judgment. Sibling reading immutable_diagnostic_reading claims all classifications are discovery-based. Sibling reading rhetorical_scaffold_reading claims all classifications are declaration-based. The three readings coexist in current discourse but each constrains the others' interpretive freedom. Decomposition is necessary because epsilon values differ: the hybrid reading authors epsilon for its own normative-judgment-inescapable structure; the diagnostic reading would measure the same situation and report lower epsilon (no extraction, just disagreement about observation); the rhetorical reading would report higher epsilon (the reading suppresses radical constructivism). Each reading is a structurally distinct constraint with its own ε, its own stakeholder situation, its own type classification. Links via network.affects_constraints establish the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
