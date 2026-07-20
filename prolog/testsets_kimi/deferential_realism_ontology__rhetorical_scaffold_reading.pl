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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: DR Ontology as Rhetorical Scaffold
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story models the Deferential Realism ontology when read
 *   as a rhetorical scaffoldâa normative vocabulary for policy critique
 *   rather than an observational instrument. Under this reading,
 *   classifications like 'snare' are declared when a mechanism serves
 *   illegitimate beneficiaries, not discovered through measurement. The
 *   framework's value lies in persuasive power and coordination among
 *   critics. This reading stands in contrast to the immutable diagnostic
 *   reading (fixed referents, correctable misclassification) and the hybrid
 *   pragmatic reading (fixed core, contested periphery). The story authors
 *   structural data consistent with a scaffold: beneficiaries gain
 *   coordination and persuasive capital, payers (targeted institutions) lose
 *   legitimacy, and alternative framings are excluded from the discourse but
 *   not actively suppressed.
 *
 * KEY AGENTS:
 *   - policy_critics: Primary beneficiary (organized/mobile) â gains shared vocabulary and persuasive leverage
 *   - targeted_institutions: Primary payer (institutional/constrained) â bears legitimacy loss from DR classification
 *   - alternative_framing_scholars: Excluded voice (organized/mobile) â present in adjacent disciplines, absent from DR discourse
 *   - epistemological_observers: Analytical seat (analytical/analytical) â studies the framework's operation and contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.45).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.25).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "DR Ontology as Rhetorical Scaffold").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '89db931a-ea32-457d-a5fd-1d714a4cbec9').
narrative_ontology:cs_kernel_codification('89db931a-ea32-457d-a5fd-1d714a4cbec9', formalized).
narrative_ontology:cs_authority_grounding('89db931a-ea32-457d-a5fd-1d714a4cbec9', distributed).
narrative_ontology:cs_reading_relation('89db931a-ea32-457d-a5fd-1d714a4cbec9', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('89db931a-ea32-457d-a5fd-1d714a4cbec9', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('89db931a-ea32-457d-a5fd-1d714a4cbec9', foundational, classification_as_declaration).
narrative_ontology:cs_axiom_status(classification_as_declaration, holdable).
narrative_ontology:cs_axiom_grounding('89db931a-ea32-457d-a5fd-1d714a4cbec9', classification_as_declaration, conventional).
narrative_ontology:cs_axiom('89db931a-ea32-457d-a5fd-1d714a4cbec9', foundational, persuasion_over_veridicality).
narrative_ontology:cs_axiom_status(persuasion_over_veridicality, holdable).
narrative_ontology:cs_axiom_grounding('89db931a-ea32-457d-a5fd-1d714a4cbec9', persuasion_over_veridicality, instrumental).
narrative_ontology:cs_reference_frame('89db931a-ea32-457d-a5fd-1d714a4cbec9', rhetorical_critique_origin).
narrative_ontology:cs_drift_state('89db931a-ea32-457d-a5fd-1d714a4cbec9', contemporary_policy_debates, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('89db931a-ea32-457d-a5fd-1d714a4cbec9', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, targeted_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy DR categories in policy advocacy and institutional critique, gaining a shared vocabulary that coordinates opposition and confers persuasive leverage in public debates. They can switch to other critical frameworks if DR loses utility.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, beneficiary,
    organized, biographical, mobile, national).

% Publicly labeled as snares or tangled ropes by DR critics, suffering legitimacy loss and defensive costs. They cannot easily exit the label once it circulates in policy discourse; rebuttal requires engaging the DR framing on its own terms.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, targeted_institutions, payer,
    institutional, generational, constrained, national).

% Operate in economics, law, and positivist policy analysis with competing vocabularies that emphasize measurable causation over normative classification. They are not part of the DR-framed conversation and their objections are rarely engaged directly.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, alternative_framing_scholars, excluded,
    organized, biographical, mobile, global).

% Study the DR framework as an epistemological object, analyzing how its classifications function in discourse, who benefits, and whether the rhetorical scaffold reading or its siblings better capture its structural role.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, epistemological_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__rhetorical_scaffold_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, portable vocabulary for policy critics to identify and coordinate opposition to institutional mechanisms that serve illegitimate beneficiaries, lowering the cost of collective critique across disparate policy domains.
% TRANSFER_FUNCTION: Moves legitimacy away from institutions classified as snares or tangled ropes and toward the critics deploying the classification; transfers persuasive capital and discursive authority from the framework's declarative force to its users.
% ABSENT_VOICES: Alternative framing scholarsâneoclassical economists, legal formalists, positivist policy analystsâwho would argue that the DR categories obscure measurable causal mechanisms and empirical institutional analysis in favor of normative storytelling; they are present in adjacent disciplines but structurally excluded from DR-framed discourse.
% DISAPPEARANCE_RATIONALE: If the rhetorical scaffold vanished, policy critics would lose a coordinated classificatory vocabulary and the persuasive leverage it provides; targeted institutions would regain discursive legitimacy currently undermined by DR labeling; and alternative framing scholars would gain space in policy debates currently occupied by DR category deployment.
% FOUNDING_PROBLEM: The absence of an intuitively legible, portable vocabulary for critiquing institutional capture and extraction in policy debates, which left critics fragmented and unable to coordinate effective opposition to mechanisms serving illegitimate beneficiaries.
% FOUNDING_PROBLEM_CORROBORATION: Policy practitioners outside the DR framework attest to the persistent difficulty of coordinating cross-sector critique against captured institutions; however, alternative framing scholars attest that existing analytical vocabularies in economics and law already suffice, and no independent corroboration confirms that the DR framework's unique rhetorical structure is necessary to solve the problem.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.45) is moderate: the framework extracts legitimacy from targeted institutions and confers it on critics, but the source material specifies low suppression and advocacy-driven classification rather than coercive enforcement. Suppression (0.25) is low because alternative framings persist in adjacent disciplines and are not structurally barred. Theater_ratio (0.52) is moderate-high because the framework's value is explicitly persuasiveâits operation is performative and rhetorical. Accessibility_collapse (0.20) is low: understanding the framework does not eliminate alternatives; economists and legal formalists continue to operate in parallel. Resistance (0.40) is moderate: alternative framing scholars and targeted institutions actively contest the framework's epistemic authority. The claim/metric independence is maintained: the reading is claimed as scaffold (transitional coordination) while the metrics acknowledge moderate extraction and theatrical performance.
 *
 * PERSPECTIVAL GAP:
 *   The policy critic seat experiences the constraint as a valuable coordination tool and source of persuasive power; the targeted institution seat experiences it as an illegitimate legitimacy drain. The analytical observer sees the structural asymmetry: the classification is declaration from the critic's seat and extraction from the institution's seat. The engine computes this divergence from the beneficiary/payer declarations and differentiated exit options (mobile for critics, constrained for institutions).
 *
 * DIRECTIONALITY LOGIC:
 *   Policy critics are declared beneficiaries (low d): they coordinate through the framework and capture persuasive gains. Targeted institutions are the structural targets (high d): they cannot easily exit the labeling once it is applied in public discourse, and they bear the legitimacy cost. Alternative framing scholars are excluded from the discourse but not directly governed by the constraint; their directionality is not central. The engine will derive high effective extraction for institutions and low/negative extraction for critics.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification prevents mislabeling the framework as pure extraction (snare) because it identifies a genuine coordination function: critics are net beneficiaries of the shared vocabulary, and the framework is meant to be transitional (has_sunset_clause: true). However, if the scaffold never sunsets and the theater_ratio continues to rise, the framework risks mandatrophyâpersisting as a piton after its transitional justification expires. The temporal measurements show rising theater and extraction, which the lifecycle drift system can monitor for this transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffold_temporality,
    'Is the DR framework genuinely transitional (a scaffold to be dismantled after policy change), or does it tend to perpetuate itself as a permanent critical apparatus?',
    'Historical analysis of DR-influenced policy campaigns to determine whether the vocabulary is retired after institutional victories or persists indefinitely as a standing critique.',
    'If perpetual, the scaffold classification is inaccurate and the constraint may be reclassified as piton or snare; if transitional, the scaffold claim is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_temporality, empirical, 'Whether the rhetorical scaffold is actually temporary').

omega_variable(
    constructed_epsilon_tracking,
    'Do DR classifications track any measurable extraction independently of the normative judgment that a beneficiary is illegitimate?',
    'Comparative institutional analysis measuring whether DR-classified snares show higher objective extraction metrics than non-classified institutions, controlling for the classifier''s normative commitments.',
    'If classifications track independent measurement, the rhetorical scaffold reading is partially falsified and the hybrid or immutable readings gain support; if purely constructed, this reading is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constructed_epsilon_tracking, empirical, 'Whether epsilon values are constructed or measured').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(defe_tr_t6, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(defe_tr_t18, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 18, 0.52).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(defe_be_t6, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 6, 0.32).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(defe_be_t18, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 18, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(defe_su_t6, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 6, 0.18).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(defe_su_t18, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 18, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The DR ontology kernel decomposes into three structurally distinct readings: the immutable diagnostic reading treats the typology as an observational instrument with fixed referents; the hybrid pragmatic reading treats the core as fixed and periphery as contested; this rhetorical scaffold reading treats the entire typology as normative vocabulary. Their epsilon values and beneficiary structures differ, requiring separate stories per the Îµ-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
