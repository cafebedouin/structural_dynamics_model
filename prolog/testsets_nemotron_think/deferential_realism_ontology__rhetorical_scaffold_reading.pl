% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Typology as Rhetorical Scaffold (Rhetorical Scaffold Reading)
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story captures the rhetorical_scaffold_reading of the
 *   deferential realism ontology kernel. Under this reading, the six-category
 *   typology (mountain, rope, tangled_rope, snare, scaffold, piton) is not an
 *   observational instrument that discovers structural facts about
 *   constraints. Rather, it is a normative vocabulary that policy critics and
 *   advocates deploy to coordinate critique. The category 'snare' is not a
 *   measurement outcome — it is a declaration that a mechanism serves
 *   illegitimate beneficiaries. The framework's value is its persuasive
 *   power: it gives critics a ready-made grammar that signals analytical
 *   rigor and transfers justificatory burden to the labeled mechanism. This
 *   reading expects low suppression of alternative framings (critics can
 *   always switch vocabularies), advocacy-driven classification (epsilon
 *   values reflect normative judgment about legitimacy, not measured
 *   extraction), and a rope-like coordination function (shared vocabulary
 *   enabling collective critique) with minimal extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.15).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, rope).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Typology as Rhetorical Scaffold (Rhetorical Scaffold Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '24c30da5-5062-4e8d-97c4-80be5440409a').
narrative_ontology:cs_kernel_codification('24c30da5-5062-4e8d-97c4-80be5440409a', distributed).
narrative_ontology:cs_authority_grounding('24c30da5-5062-4e8d-97c4-80be5440409a', expertise).
narrative_ontology:cs_interpretation_layer_present('24c30da5-5062-4e8d-97c4-80be5440409a').
narrative_ontology:cs_reading_relation('24c30da5-5062-4e8d-97c4-80be5440409a', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('24c30da5-5062-4e8d-97c4-80be5440409a', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('24c30da5-5062-4e8d-97c4-80be5440409a', foundational, classification_is_advocacy_not_discovery).
narrative_ontology:cs_axiom_status(classification_is_advocacy_not_discovery, holdable).
narrative_ontology:cs_axiom_grounding('24c30da5-5062-4e8d-97c4-80be5440409a', classification_is_advocacy_not_discovery, deontological).
narrative_ontology:cs_axiom('24c30da5-5062-4e8d-97c4-80be5440409a', foundational, persuasive_power_is_primary_value).
narrative_ontology:cs_axiom_status(persuasive_power_is_primary_value, holdable).
narrative_ontology:cs_axiom_grounding('24c30da5-5062-4e8d-97c4-80be5440409a', persuasive_power_is_primary_value, instrumental).
narrative_ontology:cs_reference_frame('24c30da5-5062-4e8d-97c4-80be5440409a', normative_vocabulary_for_critique).
narrative_ontology:cs_drift_state('24c30da5-5062-4e8d-97c4-80be5440409a', contemporary_policy_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('24c30da5-5062-4e8d-97c4-80be5440409a', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_organizations).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, critical_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_targets_labeled_snare).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_vocabulary_enables_critique).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, classification_as_persuasive_act).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and maintain the constraint typology (mountain, rope, tangled_rope, snare, scaffold, piton) as an analytical vocabulary. They publish the framework, define its categories, and respond to critiques. Their authority derives from scholarly recognition and the framework's uptake in policy discourse. They can revise categories or abandon the project.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, framework_authors, agenda_setter,
    organized, generational, mobile, global).

% Use the typology to label contested mechanisms (regulations, market structures, institutional arrangements) as 'snare' or 'tangled_rope' in advocacy campaigns, legislative testimony, and public commentary. The vocabulary gives them a ready-made normative grammar that legitimizes critique without requiring bespoke argument each time. They can switch to other critical vocabularies if this one loses traction.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, beneficiary,
    moderate, biographical, mobile, national).

% Incorporate the typology into policy reports, amicus briefs, and media strategy. The framework's categories function as rhetorical shorthand that signals analytical rigor to journalists and policymakers. Their investment in the vocabulary (training staff, building campaigns around it) creates moderate switching costs, but alternative frameworks exist.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_organizations, beneficiary,
    organized, biographical, constrained, national).

% Cite and extend the typology in academic work, using it to structure comparative institutional analysis. They benefit from a shared vocabulary that makes their work legible across subfields. Their professional incentives reward novel applications and refinements of the framework. They can adopt competing typologies without career penalty.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, critical_scholars, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__rhetorical_scaffold_reading, critical_scholars, observer).

% Institutions or mechanisms labeled 'snare' by critics using the framework. They bear reputational and political costs when the label sticks in public discourse. They cannot easily exit the discourse — the label is applied to them externally. They must either rebut the classification on its own terms (accepting the framework's grammar) or reject the framework entirely (ceding rhetorical ground).
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_targets_labeled_snare, payer,
    institutional, biographical, constrained, national).

% Scholars and practitioners advancing alternative institutional typologies (e.g., Ostrom's IAD, Williamson's transaction cost economics, North's institutional analysis). They would object to the deferential realism typology's claim to unique analytical purchase, arguing their frameworks capture the same phenomena with different normative commitments. They are structurally excluded from the deferential realism discourse community.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, competing_framework_proponents, excluded,
    organized, generational, mobile, global).

% Meta-level analysts evaluating whether the typology functions as a diagnostic instrument, a normative vocabulary, or a hybrid. They have no stake in the framework's policy uptake but track its diffusion, its classification stability across cases, and whether its categories correspond to observable structural patterns. They can adopt any interpretive frame.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared normative vocabulary that lets diverse critics coordinate on labeling mechanisms as illegitimate without negotiating a new grammar each time. The categories (especially 'snare') function as a coordination device: when multiple parties call the same arrangement a snare, the label itself becomes a focal point for collective opposition.
% TRANSFER_FUNCTION: Moves rhetorical authority and policy salience from labeled mechanisms (which must defend against the 'snare' designation) to critics (who gain a presumption of analytical rigor by invoking the framework). The framework does not transfer material resources directly; it transfers the burden of justification in public discourse.
% ABSENT_VOICES: Policymakers and administrators who must implement or defend mechanisms labeled 'snare' are often absent from the scholarly discourse where the typology is developed. They experience the framework as an external rhetorical weapon rather than a diagnostic tool they participated in building. Affected communities whose arrangements get classified may also lack standing in the academic venues where category boundaries are negotiated.
% DISAPPEARANCE_RATIONALE: If the deferential realism typology vanished overnight, policy critics would lose a shared normative grammar that coordinates multi-actor critique campaigns. Advocacy organizations would need to build bespoke rhetorical frameworks for each target. The 'snare' label's specific persuasive power — its ability to signal 'this mechanism extracts under cover of coordination' in four syllables — would be gone, replaced by longer, less coordinated critiques. The discourse space would reorganize around competing vocabularies.
% FOUNDING_PROBLEM: Policy critique lacked a shared, structurally precise vocabulary for distinguishing legitimate coordination from illegitimate extraction. Critics talked past each other using incommensurable frameworks (market failure, regulatory capture, power analysis), making it hard to build cross-issue coalitions or hold mechanisms to a consistent standard.
% FOUNDING_PROBLEM_CORROBORATION: Framework authors attest the problem remains live — new mechanisms (algorithmic governance, platform regulation, climate policy instruments) demand fresh structural categories. Competing framework proponents (Ostromians, transaction cost economists) attest the problem was never uniquely theirs to solve — their frameworks already provided coordination/extraction distinctions, making the deferential realism typology a rhetorical innovation, not a diagnostic one. Policy practitioners attest the typology sees limited operational use; its uptake is concentrated in advocacy and academia.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.15, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low (0.15) because the framework itself extracts no material resources — its 'extraction' is purely rhetorical, the persuasive advantage it confers on critics. Suppression is very low (0.10) because no one is prevented from using competing typologies; the framework competes in an open marketplace of analytical vocabularies. Theater ratio is moderate (0.35) because the framework presents itself as diagnostic (discovering snares) while functioning as rhetorical (declaring them) — a performative gap between self-presentation and operation. Accessibility collapse is low-moderate (0.30): adopting the vocabulary makes alternative framings less salient but not inaccessible. Resistance is low (0.25): the framework faces academic critique but no organized opposition. The claimed type is 'rope' — a genuine coordination device (shared critical vocabulary) with minimal coercive overhead and no suppressed alternatives.
 *
 * PERSPECTIVAL GAP:
 *   From the framework authors' and critics' seats, the typology is a coordination success — a rope that solves the collective-action problem of 'how do we criticize mechanisms consistently?' From the policy targets' seat, the same structure operates as an asymmetric rhetorical device — they must defend against a label they had no role in constructing and cannot easily dislodge. The engine computes this divergence from the declared structural positions; the authored claim (rope) reflects the coordinator's perspective, while the metrics honestly describe low extractiveness and suppression overall.
 *
 * DIRECTIONALITY LOGIC:
 *   Framework authors (agenda_setter) sit near the beneficiary end (d ~ 0.2) — they gain scholarly recognition and framework uptake without bearing costs. Policy critics, advocacy organizations, and critical scholars (beneficiaries) sit at d ~ 0.15-0.25 — they gain rhetorical leverage with minimal cost. Policy targets labeled 'snare' (payers) sit at d ~ 0.7-0.8 — they bear reputational/political costs and cannot exit the discourse. Competing framework proponents (excluded) are structurally outside the constraint's operation — they experience it as a rival vocabulary, not as a mechanism governing them. Analytical observers (observer) sit at d = 0.5 by definition. The engine will compute per-seat effective extraction from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework's founding problem (coordinating policy critique) remains contested — critics say it's live, competing frameworks say it was already solved, practitioners say it's marginal. The typology persists not because the founding problem is dead (mandatrophy) but because it continues to provide rhetorical value to active advocacy coalitions. Its low theater ratio and low suppression distinguish it from a piton: it is actively used, not theatrically maintained. If advocacy uptake declined, it could drift toward piton (theoretical vocabulary maintained by scholars after policy relevance fades).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the deferential realism typology a single kernel with multiple readings, or are the ''readings'' actually distinct constraints that share terminology?',
    'Trace whether proponents of each reading treat the others as interpretations of the same object (same categories, same engine) or as rival frameworks. If a proponent of the immutable_diagnostic_reading would say ''the rhetorical_scaffold_reading misapplies the typology'' rather than ''the rhetorical_scaffold_reading describes a different typology'', they share a kernel.',
    'If distinct constraints, each gets its own ε and classification; the ''kernel'' is a semantic illusion. If one kernel, the readings are indexical perspectives on one constraint, and the engine''s per-seat computation should capture the divergence without needing multiple stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Whether the three declared readings constitute one kernel or three constraints.').

omega_variable(
    normative_vs_descriptive_boundary,
    'Where exactly does the rhetorical_scaffold_reading locate the normative judgment — in the category definitions, the epsilon assignment, the classification thresholds, or the decision to classify at all?',
    'Compare how the three readings would classify the same empirical case (e.g., a platform commission gate). If they agree on base metrics (extraction, suppression) but diverge on claimed_type, the normative judgment is at the classification threshold. If they diverge on base metrics, the judgment enters earlier.',
    'If normative judgment enters at epsilon assignment, the framework''s claim to structural objectivity (ε-invariance) is undermined from this reading''s perspective. If it enters only at classification, the metrics remain descriptive and the typology is a rope with a normative overlay.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_vs_descriptive_boundary, conceptual, 'Locus of normativity within the framework''s own machinery.').

omega_variable(
    persuasive_power_measurement,
    'Can the framework''s ''persuasive power'' — its primary value on this reading — be measured independently of its analytical accuracy?',
    'Natural experiment: track policy outcomes when critics use deferential realism categories vs. alternative vocabularies against the same mechanisms. Control for critic resources, mechanism type, political context.',
    'If persuasive power is measurable and separable from accuracy, the rope claim (genuine coordination) is strengthened — the framework delivers a real coordination benefit. If persuasive power correlates with analytical accuracy, the immutable_diagnostic_reading gains support. If neither, the framework may be a scaffold (transitional vocabulary) or piton (theoretical remnant).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persuasive_power_measurement, empirical, 'Whether the framework''s rhetorical efficacy is empirically distinguishable from its diagnostic validity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dr_ontology_rhetorical_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(dr_ontology_rhetorical_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(dr_ontology_rhetorical_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(dr_ontology_rhetorical_tr_t15, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(dr_ontology_rhetorical_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(dr_ontology_rhetorical_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(dr_ontology_rhetorical_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.1).
narrative_ontology:measurement(dr_ontology_rhetorical_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(dr_ontology_rhetorical_be_t15, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 15, 0.14).
narrative_ontology:measurement(dr_ontology_rhetorical_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(dr_ontology_rhetorical_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(dr_ontology_rhetorical_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.07).
narrative_ontology:measurement(dr_ontology_rhetorical_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.08).
narrative_ontology:measurement(dr_ontology_rhetorical_su_t15, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 15, 0.09).
narrative_ontology:measurement(dr_ontology_rhetorical_su_t20, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 20, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__rhetorical_scaffold_reading, 0.02).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint (rhetorical_scaffold_reading) and its two siblings form a constraint family decomposing the deferential_realism_ontology kernel. The rhetorical_scaffold_reading claims the entire typology is normative vocabulary (epsilon constructed by judgment). The immutable_diagnostic_reading claims fixed referents (epsilon discovered by measurement). The hybrid_pragmatic_reading claims a fixed core (mountains, ropes) with contested periphery (tangled_ropes, snares). Their epsilon values differ structurally: rhetorical_scaffold assigns low epsilon to the framework itself (it's a rope); immutable_diagnostic assigns near-zero epsilon (it's a mountain); hybrid_pragmatic assigns low epsilon to core, higher to periphery. They are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
