% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Ontology â Rhetorical Scaffold Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the rhetorical_scaffold_reading of the
 *   deferential_realism_ontology kernel. Under this reading, the six-category
 *   typology (mountain, rope, tangled rope, snare, scaffold, piton) is not an
 *   observational instrument with fixed referents but a normative vocabulary
 *   for policy critique. 'Snare' is declared, not discovered, when a
 *   mechanism serves illegitimate beneficiaries. The framework's persistence
 *   and value lie in its persuasive power rather than in empirical accuracy.
 *   The reading is contested by the immutable_diagnostic_reading (which
 *   treats the typology as an observational instrument) and the
 *   hybrid_pragmatic_reading (which treats the core as fixed and the
 *   periphery as contested). This story authors the structural data for the
 *   rhetorical scaffold reading alone, per the Îµ-invariance principle.
 *
 * KEY AGENTS:
 *   - policy_critics: Primary beneficiary (organized/mobile) â gains analytical authority and cross-domain critique vocabulary
 *   - operators_of_labeled_mechanisms: Primary target (powerful/mobile) â bears delegitimization when their mechanisms are declared extractive
 *   - dr_ontology_authors: Agenda setter (moderate/mobile) â maintains the schema and classification rules
 *   - competing_framing_proponents: Excluded voice (organized/mobile) â advances quantitative and alternative framings not expressible in the six-category system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.35).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.18).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Ontology â Rhetorical Scaffold Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, 'd604a4f5-6ef1-4224-8e89-a6e397a211ca').
narrative_ontology:cs_kernel_codification('d604a4f5-6ef1-4224-8e89-a6e397a211ca', formalized).
narrative_ontology:cs_authority_grounding('d604a4f5-6ef1-4224-8e89-a6e397a211ca', distributed).
narrative_ontology:cs_reading_relation('d604a4f5-6ef1-4224-8e89-a6e397a211ca', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('d604a4f5-6ef1-4224-8e89-a6e397a211ca', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('d604a4f5-6ef1-4224-8e89-a6e397a211ca', foundational, classification_as_advocacy).
narrative_ontology:cs_axiom_status(classification_as_advocacy, holdable).
narrative_ontology:cs_axiom_grounding('d604a4f5-6ef1-4224-8e89-a6e397a211ca', classification_as_advocacy, conventional).
narrative_ontology:cs_axiom('d604a4f5-6ef1-4224-8e89-a6e397a211ca', foundational, epsilon_normatively_constructed).
narrative_ontology:cs_axiom_status(epsilon_normatively_constructed, holdable).
narrative_ontology:cs_axiom_grounding('d604a4f5-6ef1-4224-8e89-a6e397a211ca', epsilon_normatively_constructed, conventional).
narrative_ontology:cs_reference_frame('d604a4f5-6ef1-4224-8e89-a6e397a211ca', normative_critique_vocabulary).
narrative_ontology:cs_drift_state('d604a4f5-6ef1-4224-8e89-a6e397a211ca', contemporary_kernel_contest, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('d604a4f5-6ef1-4224-8e89-a6e397a211ca', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, operators_of_labeled_mechanisms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy the six-category vocabulary to frame governance mechanisms as mountains, ropes, snares, scaffolds, or pitons across institutional domains. They gain analytical authority, cross-domain portability, and a shared grammar for structural critique. They are not locked into the framework and can revert to ad hoc or disciplinary vocabularies.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, beneficiary,
    organized, biographical, mobile, global).

% Operate governance arrangements that are declared snares, tangled ropes, or pitons by critics using the typology. They bear the cost of delegitimization in policy discourse and public legitimacy but retain the capacity to reframe their mechanisms using technical, economic, or legal vocabularies outside the DR framework.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, operators_of_labeled_mechanisms, payer,
    powerful, biographical, mobile, global).

% Authored and maintain the constraint story schema, classification rules, and generation pipeline. They set the definitional boundaries of the six categories and determine what counts as structural evidence for each type. They do not directly collect material rents from the framework's use but exercise influence over the terms of policy discourse.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, dr_ontology_authors, agenda_setter,
    moderate, generational, mobile, global).

% Advance alternative analytical frameworks such as quantitative mechanism design, behavioral economics, and legal positivism that do not rely on the six-category typology. They are structurally outside the framework's coherence conditions but are not actively suppressed from public discourse.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, competing_framing_proponents, excluded,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a portable, six-category structural vocabulary that coordinates policy critics across domains, enabling them to translate local grievances into generalized claims about mechanism design and beneficiary structure.
% TRANSFER_FUNCTION: Moves analytical legitimacy and persuasive authority from the typology's formal architecture to the advocates who deploy it; transfers delegitimization costs to the operators of mechanisms declared extractive.
% ABSENT_VOICES: Quantitative mechanism designers, behavioral economists, and legal positivists who reject the six-category compression are absent from the framework's internal discourse. They would argue that the typology collapses measurable institutional heterogeneity into normative boxes.
% DISAPPEARANCE_RATIONALE: If the typology vanished overnight, policy critics would lose their shared framing grammar and would revert to ad hoc moralizing or domain-specific technical languages. The coordination of structural critique across domains would fragment, and the terms of debate would rearrange around alternative vocabularies.
% FOUNDING_PROBLEM: Policy critique lacked a portable, structural vocabulary that could travel across institutional domains and translate particular grievances into reusable claims about coordination, extraction, and mechanism design.
% FOUNDING_PROBLEM_CORROBORATION: Policy critics using the framework attest the need from their seat. Observational social scientists and mechanism designers from outside the benefiting party dispute that the problem is best solved by a normative typology rather than by improved measurement and causal identification.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the typology extracts epistemic authority and persuasive leverage for its beneficiaries, though not material rents. Suppression is low (0.18) because alternative framings are not actively suppressed â the prompt's structural delta explicitly notes low suppression. Theater ratio is moderate-high (0.48) because under this reading the typology's analytical appearance is substantially performative: it presents normative declarations as structural classifications. Accessibility collapse is low (0.22) because critics and targets alike retain access to alternative vocabularies. Resistance is moderate-low (0.28) because observational and quantitative traditions contest the framework's epistemic pretensions. The claim is scaffold because the arrangement justifies itself as transitional (a scaffold for policy critique) rather than as a permanent ontology.
 *
 * PERSPECTIVAL GAP:
 *   The policy critic seat and the target seat compute differently: critics experience the typology as a coordination device that lends their advocacy analytical discipline, while targets experience it as an asymmetric rhetorical weapon that declares their mechanisms extractive without empirical demonstration. The immutable diagnostic proponent experiences the typology as a failing observational instrument, whereas the rhetorical scaffold proponent experiences it as a successful persuasive technology. The engine computes these divergences from the same structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy critics are declared beneficiaries with mobile exit, placing them near the beneficiary end of directionality (low d). Operators of labeled mechanisms are declared victims with mobile exit, placing them nearer the target end (higher d) though their mobility dampens effective extraction. DR ontology authors are agenda setters with moderate power and mobile exit; they sit near symmetric because they maintain the framework without directly collecting its persuasive gains. The directionality derivation is straightforward and requires no override.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as scaffold rather than snare prevents misidentifying the coordination function: the typology genuinely coordinates critique across domains, which is a real coordination problem. Classifying it as scaffold rather than rope prevents ignoring its asymmetry: the typology is not neutral among parties â it is designed to delegitimize certain mechanisms and their operators. The scaffold type captures the transitional justification (the framework exists to move policy discourse, not to settle it) while acknowledging the beneficiaries who gain from its deployment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_status_of_epsilon,
    'Are epsilon values in the deferential realism typology ever discoverable through inter-subjective measurement, or are they necessarily constructed through normative judgment?',
    'Convergence test: have independent analysts with identical structural data converge on the same epsilon values and claimed types. Systematic divergence supports construction; convergence supports discoverability.',
    'If epsilon is necessarily constructed, the immutable diagnostic reading is structurally undermined and the rhetorical scaffold reading is strengthened. If epsilon is discoverable, the rhetorical scaffold reading collapses into a false account of the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_status_of_epsilon, conceptual, 'Whether the framework''s core metric is discovered or constructed').

omega_variable(
    scaffold_sunset_or_permanence,
    'Does the deferential realism typology possess a genuine sunset clause â an articulated end-state where the scaffold is dismantled â or has it become a permanent fixture of policy discourse despite its transitional justification?',
    'Examine the corpus of constraint stories and author commentary for explicit transition narratives: do authors describe the typology as a temporary stage toward better measurement, or as an enduring vocabulary?',
    'If no sunset is pursued, the scaffold claim is cover for a permanent coordination mechanism, and the constraint may compute as rope or tangled rope. A genuine sunset keeps it scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_or_permanence, empirical, 'Whether the scaffold is transitional or permanent').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the observed low suppression of alternative framings a genuine structural openness, or is it a performed tolerance that masks internalized conformity within the community of users?',
    'Track exit behavior: if critics who abandon the typology face reputational or coordination penalties within the policy-critique community, suppression is partially internalized despite low formal barriers.',
    'If internalized, effective suppression is higher than the structural measure suggests, and the constraint''s computed type may shift toward tangled rope or snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural openness versus internalized conformity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(defe_tr_t2, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 2, 0.35).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 8, 0.45).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(defe_be_t2, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 2, 0.24).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(deferential_realism_ontology__rhetorical_scaffold_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel decomposes into three structurally distinct readings. The rhetorical_scaffold_reading (this constraint) treats the typology as normative vocabulary. The immutable_diagnostic_reading treats it as observational instrument. The hybrid_pragmatic_reading treats it as fixed core plus contested periphery. Each reading carries distinct epsilon values, beneficiary structures, and epistemic warrants. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
