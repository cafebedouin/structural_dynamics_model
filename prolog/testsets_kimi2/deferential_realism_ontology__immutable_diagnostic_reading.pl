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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Deferential Realism Ontology â Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism ontology under the immutable diagnostic reading
 *   treats its six-category typology as an observational instrument with
 *   fixed referents: mountains correspond to physical invariants, snares to
 *   measurable extraction mechanisms, and any disagreement about
 *   classification is treated as an observational error correctable by better
 *   measurement. This reading functions as an epistemic constraint that
 *   coordinates interdisciplinary analysis by providing a shared
 *   metric-driven vocabulary, while simultaneously suppressing constructivist
 *   and normative alternative framings by delegitimizing them as
 *   misclassification. The kernel is contested by a hybrid pragmatic reading
 *   (fixed core, contested periphery) and a rhetorical scaffold reading
 *   (normative vocabulary for policy critique). This story models the
 *   epistemic arrangement that enforces the immutable reading.
 *
 * KEY AGENTS:
 *   - diagnostic_authority (institutional/analytical/arbitrage): Adjudicates classification disputes and benefits from the authority of objective metric resolution
 *   - normative_policy_critics (moderate/constrained): Bear the cost of having their substantive normative critiques treated as observational errors
 *   - contested_perimeter_analysts (moderate/constrained): Pay through suppressed autonomy when boundary classifications are dictated rather than negotiated
 *   - constructivist_methodologists (organized/trapped): Excluded voices whose core claimâthat epsilon is constructedâcontradicts the reading's self-understanding
 *   - analytical_sociologists (analytical/analytical): Observe the framework's operation without stakes in its epistemic hierarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.78).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.88).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology â Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'df8324cd-9fd0-4bd6-b799-2e754072e930').
narrative_ontology:cs_kernel_codification('df8324cd-9fd0-4bd6-b799-2e754072e930', formalized).
narrative_ontology:cs_authority_grounding('df8324cd-9fd0-4bd6-b799-2e754072e930', expertise).
narrative_ontology:cs_reading_relation('df8324cd-9fd0-4bd6-b799-2e754072e930', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('df8324cd-9fd0-4bd6-b799-2e754072e930', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('df8324cd-9fd0-4bd6-b799-2e754072e930', foundational, epsilon_values_discoverable_not_constructed).
narrative_ontology:cs_axiom_status(epsilon_values_discoverable_not_constructed, holdable).
narrative_ontology:cs_axiom_grounding('df8324cd-9fd0-4bd6-b799-2e754072e930', epsilon_values_discoverable_not_constructed, empirically_contingent).
narrative_ontology:cs_axiom('df8324cd-9fd0-4bd6-b799-2e754072e930', secondary, classification_disputes_resolve_by_observation).
narrative_ontology:cs_axiom_status(classification_disputes_resolve_by_observation, holdable).
narrative_ontology:cs_axiom_grounding('df8324cd-9fd0-4bd6-b799-2e754072e930', classification_disputes_resolve_by_observation, instrumental).
narrative_ontology:cs_reference_frame('df8324cd-9fd0-4bd6-b799-2e754072e930', observational_kind_correspondence).
narrative_ontology:cs_drift_state('df8324cd-9fd0-4bd6-b799-2e754072e930', post_constructivist_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('df8324cd-9fd0-4bd6-b799-2e754072e930', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_authority).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, normative_policy_critics).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, contested_perimeter_analysts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adjudicates classification disputes within the DR framework by appeal to observable metrics. Maintains that epsilon values are discovered rather than constructed, and that misclassification at any typological position is an observational error correctable through better measurement. Derives epistemic authority and agenda-setting power from the claim that the six-category typology maps directly to invariant structural features of constraints.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_authority, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_authority, beneficiary).

% Use the DR vocabulary to critique policy and institutional arrangements, but find their normative classifications overridden as observational errors. Their claims about illegitimate beneficiaries in tangled_rope and snare cases are treated as misclassification correctable by better measurement rather than substantive disagreement about legitimate coordination.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, normative_policy_critics, payer,
    moderate, biographical, constrained, global).

% Work on boundary cases where rope, tangled_rope, and snare are difficult to distinguish. Their acknowledgment that classification in the contested periphery requires normative judgment about legitimate beneficiaries is suppressed; the immutable reading demands they treat these boundary disputes as measurement problems with single correct answers.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, contested_perimeter_analysts, payer,
    moderate, biographical, constrained, global).

% Argue that epsilon values are constructed by the choice of referent, observable, and reading position. Structurally excluded from authoritative discourse because the immutable reading treats this claim as a category errorâasserting that epsilon is constructed contradicts the framework's self-understanding as an observational instrument.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, constructivist_methodologists, excluded,
    organized, biographical, trapped, global).

% Study the DR framework as a social epistemic system. Neither collect from nor pay into the constraint. Track how classification disputes are resolved, whose voices are excluded, and how the immutable reading maintains its boundary against hybrid and rhetorical alternatives.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, analytical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, diagnostic_authority).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, seemingly objective vocabulary for classifying social constraints across domains, resolving interdisciplinary disputes by appeal to measurable metrics rather than normative debate.
% TRANSFER_FUNCTION: Moves epistemic authority from normative critics and constructivist theorists to empirical analysts and metric-adjudicators, by treating all classification disputes as correctable observational errors rather than substantive disagreements.
% ABSENT_VOICES: Constructivist methodologists and rhetorical theorists who see the framework as normative all the way down. They are excluded because the immutable reading treats their position as a misunderstanding of the framework's observational function rather than a legitimate alternative reading.
% DISAPPEARANCE_RATIONALE: If the immutable reading vanished, the authority structure that resolves disputes by appeal to 'better observation' would collapse. Contested classifications at the periphery would become explicit normative debates about legitimate beneficiaries. The framework would likely migrate toward the hybrid or rhetorical readings, and the current epistemic hierarchy would rearrange.
% FOUNDING_PROBLEM: The lack of a rigorous, cross-domain vocabulary for distinguishing genuine coordination from extraction in social and institutional analysis, leading to endless unresolvable normative disputes.
% FOUNDING_PROBLEM_CORROBORATION: Institutional designers and empirical analysts corroborate that the problem is live and that metric-driven classification provides needed discipline. Constructivist theorists and normative policy critics corroborate that the problem is partly solved but the immutable reading overreaches by suppressing legitimate normative contestation at the periphery. No neutral corroboration existsâthe dispute about the founding problem's status is itself the kernel contest.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.78, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.78) is high because the immutable reading extracts epistemic authority from alternative framings by treating all classification disputes as resolvable through better observation, thereby transferring the power to define legitimate contestation to metric-adjudicators. Suppression (0.88) is higher still because the persistence of this reading depends on actively excluding the rhetorical and hybrid readingsâalternative framings cannot coexist within the immutable framework. Theater_ratio (0.65) reflects a substantial performative component: the framework enacts objectivity while embedding contested normative choices (e.g., the threshold for requires_active_enforcement or the definition of victim) in its formal structure. Accessibility_collapse (0.80) is high because once the immutable reading is adopted, alternative framings become cognitively inaccessibleâthey appear not as disagreement but as error. Resistance (0.60) reflects substantial but institutionally marginalized pushback from constructivist and hybrid theorists.
 *
 * PERSPECTIVAL GAP:
 *   The diagnostic_authority seat experiences the arrangement as genuine coordinationâa rigorous empirical framework that resolves otherwise interminable debates. The payer seats (normative critics, perimeter analysts) experience the same arrangement as epistemic extraction: their substantive disagreements are recast as measurement failures, and the cost of participating in the framework is acceptance of a constructed metric as discovered fact. The engine computes this divergence from the structural asymmetry in exit options (analytical vs constrained) and directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The diagnostic_authority is the structural beneficiary: it collects epistemic authority and agenda-setting power by controlling the metric framework that resolves disputes (d near 0.0). Normative critics and perimeter analysts are structural targets: they pay through suppressed voice and delegitimized disagreement (d near 1.0). Constructivist methodologists are excluded entirelyâtheir exclusion is the enforcement mechanism that maintains the constraint's boundary. Analytical observers sit near symmetric, though their observations may be captured or ignored depending on institutional uptake.
 *
 * MANDATROPHY ANALYSIS:
 *   The arrangement prevents mislabeling by preserving a genuine coordination function: the DR framework does enable cross-domain communication and empirical constraint identification. The mandatrophy risk would be declaring it a snare (pure extraction) and erasing the real coordination value, or declaring it a rope (pure coordination) and missing the asymmetric suppression of the contested periphery. Tangled_rope is warranted because the same structure that coordinates empirical analysis also extracts epistemic rent from normative contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constructed_vs_discovered_epsilon,
    'Are epsilon values genuinely discovered through observation, or constructed by the choice of referent and measurement framework?',
    'Historical sociology of the DR framework showing how epsilon values were calibrated; cross-reading stability tests measuring whether identical constraints receive identical epsilon values across independent observers.',
    'If constructed, the immutable reading collapses into the hybrid reading; extraction is revealed as suppression of legitimate normative voice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_vs_discovered_epsilon, conceptual, 'Whether epsilon is discovered or constructed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative framings structural (institutional gatekeeping, citation networks, funding bias) or internalized (researchers self-censor to fit the observational paradigm)?',
    'Post-exit trajectory: if researchers who leave the DR framework immediately adopt constructivist framings, suppression was largely internalized; if they remain metric-driven, it was structural exclusion.',
    'If internalized, effective suppression exceeds structural measure; the constraint''s extraction is carried by the agents even after formal exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    core_periphery_boundary_stability,
    'Is the boundary between the fixed core (mountains, ropes) and contested periphery (tangled_ropes, snares) objectively stable, or does it shift under pressure to protect the immutable reading?',
    'Track historical reclassifications in the corpusâwhen anomalies appear, are peripheral types reclassified as core, or are core definitions expanded to absorb contested cases?',
    'If the boundary shifts strategically, the immutable reading operates as a snare; if stable, it supports the hybrid reading''s architecture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(core_periphery_boundary_stability, empirical, 'Whether core-periphery boundary is stable or strategic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dr_immutable_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dr_immutable_tr_t4, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(dr_immutable_tr_t8, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement(dr_immutable_tr_t12, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 12, 0.48).
narrative_ontology:measurement(dr_immutable_tr_t16, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 16, 0.58).
narrative_ontology:measurement(dr_immutable_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(dr_immutable_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dr_immutable_be_t4, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(dr_immutable_be_t8, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(dr_immutable_be_t12, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(dr_immutable_be_t16, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(dr_immutable_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(dr_immutable_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dr_immutable_su_t4, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(dr_immutable_su_t8, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(dr_immutable_su_t12, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 12, 0.75).
narrative_ontology:measurement(dr_immutable_su_t16, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 16, 0.82).
narrative_ontology:measurement(dr_immutable_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Deferential Realism ontology' conflates three structurally distinct readings. The immutable diagnostic reading claims full observational fixity; the hybrid pragmatic reading partitions the typology into fixed core and contested periphery; the rhetorical scaffold reading treats the entire framework as normative vocabulary. Each reading carries a distinct epsilon profile, distinct beneficiary/victim structure, and distinct authority grounding. They are linked as a constraint family through the shared kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
