% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology — Immutable Diagnostic Reading
 *   domain: epistemology/institutional_design
 *
 * SUMMARY:
 *   The immutable-diagnostic reading of Deferential Realism claims that
 *   constraint classification is an observational problem: mountains are
 *   physical invariants, snares are measurable extraction mechanisms, and
 *   disagreement about constraint type is resolvable through better metric
 *   definition and empirical observation. This reading treats ε (base
 *   extractiveness) as a discovered property, not a constructed judgment. The
 *   reading benefits metric-empiricist researchers and institutions by
 *   validating their methodology as 'objective' and suppresses alternative
 *   normative and interpretive frameworks by marginalizing them as 'merely
 *   subjective.' This is a tangled_rope: genuine coordination function
 *   (unified measurement language across domains) combined with asymmetric
 *   extraction (intellectual authority, resources, and legitimacy flowing
 *   toward metric-empiricists while normative methodologies are pushed to the
 *   margins) requiring active enforcement through gatekeeping and
 *   institutional pressure.
 *
 * KEY AGENTS:
 *   - metric_empiricist_researchers: Institutional beneficiaries; active agenda-setters defending the reading's epistemic stance through peer review, funding allocation, and curriculum design.
 *   - constraint_classification_institutions: Secondary beneficiaries; enforce the reading by credentialing metric-empiricist work and marginalizing alternatives.
 *   - alternative_normative_frameworks: Victims; suppressed through publication gatekeeping, hiring discrimination, and rhetorical disqualification as 'unscientific.'
 *   - interpretive_methodologies: Victims at the methodological level; pushed into identity-locked status for practitioners who trained in hermeneutics or ethnographic approaches.
 *   - hybrid_pragmatic_theorists: Excluded; their hybrid reading (fixed core, contestable periphery) is structurally kept out of the immutable-diagnostic framework's internal deliberation.
 *   - critical_observers: Analytical seat; track whether the reading's claim to objectivity holds or whether it obscures constructed choices.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.68).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.76).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology — Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'b12d16c9-d017-46e4-a063-76c98b42ad40').
narrative_ontology:cs_kernel_codification('b12d16c9-d017-46e4-a063-76c98b42ad40', distributed).
narrative_ontology:cs_authority_grounding('b12d16c9-d017-46e4-a063-76c98b42ad40', expertise).
narrative_ontology:cs_interpretation_layer_present('b12d16c9-d017-46e4-a063-76c98b42ad40').
narrative_ontology:cs_reading_relation('b12d16c9-d017-46e4-a063-76c98b42ad40', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('b12d16c9-d017-46e4-a063-76c98b42ad40', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('b12d16c9-d017-46e4-a063-76c98b42ad40', foundational, observable_fixed_referents).
narrative_ontology:cs_axiom_status(observable_fixed_referents, holdable).
narrative_ontology:cs_axiom_grounding('b12d16c9-d017-46e4-a063-76c98b42ad40', observable_fixed_referents, empirically_contingent).
narrative_ontology:cs_axiom('b12d16c9-d017-46e4-a063-76c98b42ad40', foundational, frame_transcendent_metrics).
narrative_ontology:cs_axiom_status(frame_transcendent_metrics, holdable).
narrative_ontology:cs_axiom_grounding('b12d16c9-d017-46e4-a063-76c98b42ad40', frame_transcendent_metrics, deontological).
narrative_ontology:cs_reference_frame('b12d16c9-d017-46e4-a063-76c98b42ad40', metric_empiricist_discovery).
narrative_ontology:cs_drift_state('b12d16c9-d017-46e4-a063-76c98b42ad40', contemporary_meta_analysis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b12d16c9-d017-46e4-a063-76c98b42ad40', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, metric_empiricist_researchers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, constraint_classification_institutions).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, alternative_normative_frameworks).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, interpretive_methodologies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Researchers and theorists committed to treating constraint classification as a measurement problem where ε values are discovered through empirical observation, not constructed through interpretive judgment. They benefit from the framework's validation of metric-driven methodology—careers, publication venues, and institutional resources flow to researchers who adopt this epistemic stance. They actively defend the reading by refining measurement instruments and marginalizing alternative framings in peer review and theoretical discourse.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, metric_empiricist_researchers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, metric_empiricist_researchers, agenda_setter).

% Universities, research institutes, funding agencies, and regulatory bodies that have invested in the immutable-diagnostic reading as their operational framework. They benefit from the reading's appearance of objectivity (metrics as truth-bearers), which provides institutional legitimacy for their classification decisions and insulates them from challenge rooted in alternative normative premises. They enforce the reading by gatekeeping funding, hiring, and publication toward metric-empiricist methodology.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constraint_classification_institutions, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, constraint_classification_institutions, agenda_setter).

% Scholars, policymakers, and practitioners who believe constraint classification requires normative judgment about legitimate beneficiaries, social context, or institutional purpose—that ε is not merely measured but partly constituted by how we frame the question. They are actively suppressed through journal rejection, funding denial, institutional marginalization, and the rhetorical move that their work is 'merely interpretive' and therefore scientifically inferior. Their work is forced into the margins or reframed to fit the metric-empiricist terms.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_normative_frameworks, payer,
    moderate, biographical, constrained, global).

% Methods like hermeneutics, deliberative inquiry, ethnographic understanding, and value-pluralist analysis that take the frame-dependence of observation as intrinsic rather than eliminable. These approaches are treated as prescientific or ideologically motivated. Practitioners who trained in these traditions must either convert to metric-empiricism or accept marginalization. The constraint suppresses their intellectual frameworks by defining them out of the respectable discourse—not through refutation but through institutional exclusion.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, interpretive_methodologies, payer,
    powerless, biographical, identity_locked, global).

% Theorists advocating the hybrid reading (fixed core, contestable periphery) are kept out of the immutable-diagnostic reading's internal decision-making. They would argue that some constraint types (mountains, ropes) are genuinely measurement problems while others (snare vs. tangled_rope) require normative judgment baked into the metric definitions themselves. Their voice would moderate the suppression and force acknowledgment of the reading's constructed elements.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_theorists, excluded,
    organized, biographical, constrained, global).

% Philosophers and meta-analysts tracking whether the immutable-diagnostic reading successfully eliminates frame-dependence from constraint classification, or whether it merely obscures constructed choices by calling them measurement. They note that the reading's claim to objectivity is itself a normative stance—a choice to privilege metric-empiricism over interpretive pluralism.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, critical_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared measurement framework for classifying institutional and physical constraints across disciplines, replacing subjective ideological assessments with observable metrics. Solves the coordination problem of comparing constraint types across incommensurable domains (physics, economics, law, psychology) by anchoring all analysis to quantifiable features.
% TRANSFER_FUNCTION: Moves legitimacy, institutional resources, and intellectual authority from normative and interpretive methodologies toward metric-empiricist frameworks. Academic careers, funding allocations, publication prestige, and hiring decisions flow preferentially to researchers adopting the immutable-diagnostic stance. Interpretive and value-pluralist approaches lose access to these resources proportionally.
% ABSENT_VOICES: Hybrid pragmatists and critical hermeneutists are structurally excluded from the framework's self-assessment. They would testify that ε values themselves carry constructed elements—what counts as 'extractiveness' depends on prior normative commitments about legitimate beneficiaries and social purpose. Their absence means the reading never confronts its own frame-dependence from an internal perspective.
% DISAPPEARANCE_RATIONALE: If the immutable-diagnostic reading vanished, institutional resources would redistribute toward interpretive and value-pluralist methodologies; peer review standards would pluralize; and constraint classification would explicitly acknowledge its normative premises rather than hiding them behind metric formalism. The epistemic order would reorganize around transparency about frame-dependence rather than the claim to frame-transcendence.
% FOUNDING_PROBLEM: Early constraint theory was captured by competing ideological frameworks—some claimed all constraints were natural necessities, others claimed all were constructions. The immutable-diagnostic reading was built to escape this impasse by treating constraint classification as a measurement problem: if ε is observable and metrics are transparent, disagreement is disagreement about facts, not values.
% FOUNDING_PROBLEM_CORROBORATION: The immutable-diagnostic adherents attest the founding problem is live—without metric anchoring, constraint classification collapses into ideology. Critical observers and hybrid pragmatists attest the problem is mislabeled—the reading did not solve the ideological contamination; it relocated it into the definitions of 'extractiveness,' 'suppression,' and 'accessibility_collapse.' Their testimony comes from outside the metric-empiricist institutional structure.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the reading concentrates intellectual authority and institutional resources in metric-empiricist seats, while suppressing competing methodologies. Suppression is higher still (0.76) because the constraint's persistence depends actively on institutional gatekeeping: alternative framings must be kept from publication venues, hiring committees, and funding bodies. Theater is moderate (0.41) because the suppression is partly transparent—metric-empiricism is openly defended as superior methodology—but partly obscured by the claim that disagreement is resolvable through 'better observation' rather than acknowledged as a choice between incommensurable epistemic stances. The measurement series shows extractiveness and suppression rising over the interval (t=0 to t=40) as metric-empiricist hegemony in institutions consolidates, then stabilizing once alternatives are effectively marginalized. Resistance drops from initial 0.58 to final 0.58 because alternative frameworks persist but are institutionally disempowered; they mount resistance but lack the structural position to prevent enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The immutable-diagnostic adherents experience the constraint as pure coordination—a transparent framework for resolving disputes through better metrics. Practitioners of alternative methodologies experience it as extractive suppression—a framework that privileges one epistemic stance by defining all others as illegitimate. The metric-empiricist researchers and institutions compute as beneficiaries with low directionality toward extraction (they set the rules and benefit from them). Alternative methodologists compute as victims with high directionality toward extraction (they pay through marginalization and loss of institutional resources). This divergence is not an error; it is the per-seat classification the engine should compute from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ideological contamination of constraint classification) and its stated resolution (metric objectivity) sit in conflict. The critical observers and hybrid pragmatists testify that the reading did not solve the problem; it relocated it. The immutable-diagnostic reading's claim to escape ideology by appealing to 'observable metrics' is itself a normative choice—a commitment to empiricism over hermeneutics, to reductionism over interpretive pluralism. The reading suppresses this meta-level awareness by treating epistemic choices as settled facts. This is not mandatrophy in the classical sense (a constraint whose founding function has died); it is a constraint whose founding problem remains live but is redefined as solved by institutional fiat. The displacement is the extraction: the constraint buys peace by declaring alternatives out of bounds, not by proving them wrong.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metric_constructedness_ambiguity,
    'Are ε, suppression, theater_ratio, and accessibility_collapse truly discovered properties of constraints, or are they constructed through prior normative commitments about what counts as extraction, legitimacy, and alternatives?',
    'Cross-reading comparison: if the hybrid_pragmatic and rhetorical readings produce substantially different ε values for the same constraint, the metrics are frame-dependent, not discovered. If ε remains stable across readings, it is genuinely observational.',
    'If frame-dependent, the immutable-diagnostic reading''s claim to objectivity collapses, and the suppression of alternatives loses its epistemic justification—it becomes pure institutional power. The reading would downgrade from tangled_rope (with coordination) to snare (pure extraction with cover story). If stable, the reading''s claim stands and the suppression of alternatives can be defended as institutional discipline against methodological error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metric_constructedness_ambiguity, empirical, 'Whether metrics are discovered or constructed through interpretive framings.').

omega_variable(
    normative_choice_in_metric_definition,
    'The definitions of ''extractiveness,'' ''suppression,'' ''theater_ratio,'' and ''accessibility_collapse'' themselves contain normative choices: what counts as a legitimate beneficiary, what counts as an alternative, what counts as performed vs. functional. Are these choices acknowledged as choices, or are they treated as natural stipulations?',
    'Meta-analysis of metric definitions across constraint stories: track whether alternative beneficiary framings (e.g., ''who benefits'' from the perspective of different stakeholder groups) produce different ε values for the same constraint.',
    'If normative content is hidden, the immutable-diagnostic reading is engaged in false natural law—claiming discovered facts while performing normative engineering. If the content is acknowledged, the reading can maintain its epistemic authority while foregrounding its normative commitments, converting suppression from dishonest exclusion to legitimate institutional gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_choice_in_metric_definition, conceptual, 'Whether metric definitions embed hidden normative choices about legitimacy and alternatives.').

omega_variable(
    suppression_as_enforcement_vs_methodology,
    'Is the suppression of alternative frameworks (via gatekeeping, publication rejection, hiring discrimination) necessary enforcement of methodological standards, or is it illegitimate suppression of equally valid epistemic approaches?',
    'Track whether alternative methodologies (hybrid, rhetorical) produce constraint classifications that are internally coherent and empirically tractable. If they do, suppression is choice-based exclusion. If they fail on internal coherence or prediction, suppression is legitimate gatekeeping.',
    'If alternatives are coherent, the immutable-diagnostic reading is purely extractive (snare with coordination cover). If they are incoherent, the reading''s suppression of them is justified institutional discipline, and the constraint remains a tangled_rope. The empirical test is whether the sibling readings can sustain themselves without collapsing into contradiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_as_enforcement_vs_methodology, empirical, 'Whether suppression is justified methodological discipline or illegitimate power.').

omega_variable(
    frame_dependence_of_alternative_framings,
    'The immutable-diagnostic reading suppresses alternative framings by calling them ''frame-dependent.'' But is the immutable reading itself not frame-dependent on the choice to privilege observational metrics over normative judgment?',
    'Symmetry analysis: apply the immutable-diagnostic frame''s own critique of alternatives to the immutable reading itself. Does the reading hold up when subjected to its own standard of criticism?',
    'If the reading is itself frame-dependent, the asymmetric suppression of alternatives is exposed as institutional power disguised as methodological rigor. If the reading is genuinely frame-transcendent, its suppression of alternatives can be justified as protection against contamination. This is a conceptual self-reference test, not resolvable empirically—the answer depends on which meta-framework is accepted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(frame_dependence_of_alternative_framings, conceptual, 'Whether the immutable reading escapes frame-dependence or merely privileges one frame over others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(defe_tr_t0, observed).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(defe_tr_t5, observed).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(defe_tr_t10, observed).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 15, 0.34).
narrative_ontology:measurement_basis(defe_tr_t15, observed).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(defe_tr_t20, observed).
narrative_ontology:measurement(defe_tr_t25, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(defe_tr_t25, observed).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(defe_tr_t30, observed).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(defe_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(defe_be_t0, observed).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(defe_be_t5, observed).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(defe_be_t10, observed).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 15, 0.62).
narrative_ontology:measurement_basis(defe_be_t15, observed).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(defe_be_t20, observed).
narrative_ontology:measurement(defe_be_t25, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(defe_be_t25, observed).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(defe_be_t30, observed).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(defe_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(defe_su_t0, observed).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(defe_su_t5, observed).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(defe_su_t10, observed).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(defe_su_t15, observed).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(defe_su_t20, observed).
narrative_ontology:measurement(defe_su_t25, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 25, 0.74).
narrative_ontology:measurement_basis(defe_su_t25, observed).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(defe_su_t30, observed).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(defe_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__immutable_diagnostic_reading, 0.12).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel admits three distinct readings corresponding to three different claims about what the constraint typology fundamentally IS. The immutable_diagnostic_reading treats constraint classification as observational (ε discovered, not constructed); the hybrid_pragmatic_reading treats the core as observational but the contested periphery as normatively constructed; the rhetorical_scaffold_reading treats the entire typology as normative vocabulary for policy critique. Each reading instantiates different ε values, beneficiary structures, and types for the same kernel. The three stories form a family linked by their shared kernel. The immutable reading suppresses the other two through institutional gatekeeping; the hybrid reading proposes to moderate this suppression by admitting a constructed periphery; the rhetorical reading would eliminate the pretense of objectivity altogether.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__immutable_diagnostic_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
