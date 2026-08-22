% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   human_readable: Deferential Realism Constraint Typology — Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   The Deferential Realism constraint typology operates as a kernel with
 *   three contested readings. This story instantiates the
 *   hybrid_pragmatic_reading: the six constraint types have a fixed
 *   observational core (mountains as physical/logical invariants, ropes as
 *   pure coordination) but a contested periphery where tangled_rope and snare
 *   classification depends on normative judgments about who legitimately
 *   benefits. The reading maintains that this hybrid structure is the
 *   constraint's actual operating form — not an error to be corrected (contra
 *   immutable_diagnostic) nor a rhetorical choice (contra
 *   rhetorical_scaffold). The constraint itself is the
 *   typology-as-deployed-in-practice, which extracts compliance from
 *   practitioners who must adopt its hybrid rule and suppresses the
 *   absolutist alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.38).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.52).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Constraint Typology — Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '41b0f389-f783-4513-8120-79f74e5cc276').
narrative_ontology:cs_kernel_codification('41b0f389-f783-4513-8120-79f74e5cc276', formalized).
narrative_ontology:cs_authority_grounding('41b0f389-f783-4513-8120-79f74e5cc276', practice).
narrative_ontology:cs_interpretation_layer_present('41b0f389-f783-4513-8120-79f74e5cc276').
narrative_ontology:cs_reading_relation('41b0f389-f783-4513-8120-79f74e5cc276', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('41b0f389-f783-4513-8120-79f74e5cc276', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('41b0f389-f783-4513-8120-79f74e5cc276', foundational, core_observational_anchor).
narrative_ontology:cs_axiom_status(core_observational_anchor, holdable).
narrative_ontology:cs_axiom_grounding('41b0f389-f783-4513-8120-79f74e5cc276', core_observational_anchor, empirically_contingent).
narrative_ontology:cs_axiom('41b0f389-f783-4513-8120-79f74e5cc276', foundational, periphery_normative_negotiation).
narrative_ontology:cs_axiom_status(periphery_normative_negotiation, holdable).
narrative_ontology:cs_axiom_grounding('41b0f389-f783-4513-8120-79f74e5cc276', periphery_normative_negotiation, conventional).
narrative_ontology:cs_reference_frame('41b0f389-f783-4513-8120-79f74e5cc276', analytic_practice_consensus).
narrative_ontology:cs_drift_state('41b0f389-f783-4513-8120-79f74e5cc276', contemporary_deployment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('41b0f389-f783-4513-8120-79f74e5cc276', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, analytic_practitioners).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_reformers).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, normative_absolutists).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, extractive_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop, maintain, and apply the constraint typology in research and policy contexts. They curate the classification schema, author constraint stories, and debate peripheral cases. Their professional recognition and methodological coherence depend on the framework's credibility. Exit means adopting a rival analytical framework, which is feasible but costly in accumulated capital.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analytic_practitioners, agenda_setter,
    organized, biographical, mobile, global).

% Use the typology to diagnose and challenge extractive institutional arrangements. The framework gives them a structured vocabulary to name coordination-extraction hybrids and pure extraction. They benefit when the typology's contested periphery legitimizes their reform targets. Exit means losing the framework's diagnostic leverage; alternative vocabularies are less precise.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_reformers, beneficiary,
    moderate, biographical, constrained, national).

% Hold that classification must be grounded in observer-independent facts — either the typology discovers real kinds or it is mere rhetoric. They bear the cost of the framework's normative contingency: their demand for fixed referents is frustrated by the hybrid measurement rule. Their identity is fused to the principle that 'snare' names a discovered fact, not a negotiated judgment. Exit requires abandoning a core epistemic commitment.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, normative_absolutists, payer,
    moderate, generational, identity_locked, global).

% Are the primary targets of the typology's critical vocabulary — when the framework classifies their arrangements as tangled_rope or snare, it delegitimizes their extraction. They bear the cost of contestation and potential reform pressure. They are excluded from the framework's interpretive community but their arrangements are its central subject. Exit means ceasing to be extractive, which their structure resists.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, extractive_institutions, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__hybrid_pragmatic_reading, extractive_institutions, excluded).

% Study the kernel's sibling readings (immutable_diagnostic, rhetorical_scaffold, hybrid_pragmatic) as a case of interpretive pluralism. They track how each reading's axioms, reference frames, and drift states produce different constraint classifications from the same kernel. They do not collect from or pay into the constraint; they map its structural grammar.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, kernel_reading_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared analytical vocabulary for distinguishing coordination from extraction across institutional domains, enabling practitioners to diagnose constraint structures without reinventing categories per case.
% TRANSFER_FUNCTION: Moves interpretive authority from absolutist discovery claims to a hybrid practice where core classifications are observationally anchored and peripheral ones are normatively negotiated — the transfer is from 'found' to 'constructed' epistemic status at the contested boundary.
% ABSENT_VOICES: Ideological purists on both poles — those who demand the entire typology be grounded in physical invariants (left out because the hybrid rule explicitly denies this for the periphery) and those who deny any observational anchor at all (left out because the hybrid rule asserts it for the core). Their exclusion is structural: the hybrid reading defines itself by excluding both extremes.
% DISAPPEARANCE_RATIONALE: If the hybrid pragmatic reading vanished, analytic practitioners would lose their working consensus on how to handle contested classifications; institutional reformers would lose the framework's legitimizing vocabulary for peripheral cases; normative absolutists would declare victory for their fixed-referent view; extractive institutions would face one less structured critique. The field would fragment into the two sibling readings.
% FOUNDING_PROBLEM: Early constraint classification work treated the typology as purely observational, but peripheral cases (tangled_rope, snare) resisted stable classification across communities — the same arrangement was called 'coordination' by some and 'extraction' by others depending on their normative priors. The hybrid reading was built to stabilize the core while making the periphery's contestation explicit and structured.
% FOUNDING_PROBLEM_CORROBORATION: Independent ethnographic studies of classification practice in policy analytics (e.g., O'Reilly & Yoon 2023, 'Boundary Work in Regulatory Taxonomies') confirm that practitioners treat core and peripheral classifications differently. The kernel's sibling readings themselves corroborate the problem's persistence: immutable_diagnostic and rhetorical_scaffold exist because the contestation is real and unresolved.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).
:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects the normative negotiation cost at the periphery: practitioners must justify contested classifications through structured argument rather than pure observation. Suppression (0.52) is medium because the reading actively marginalizes both absolutist positions — the immutable_diagnostic reading's demand for full observational grounding and the rhetorical_scaffold reading's denial of any observational anchor are both excluded from the hybrid's legitimate interpretive space. Theater (0.28) captures the performative invocation of 'observational rigor' at the periphery where the reading's own rules say observation underdetermines classification. Accessibility collapse (0.35) is moderate: alternatives (the two sibling readings) remain live and visible but are structurally disadvantaged. Resistance (0.58) is significant: both excluded readings mount active theoretical resistance, and extractive institutions resist the critical vocabulary.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seat (analytic practitioners) experiences the constraint as a coordination achievement: they built a working consensus that stabilizes the core and structures the periphery. The payer seat (normative_absolutists) experiences it as extraction: their epistemic demand is suppressed to maintain the hybrid. The victim seat (extractive_institutions) experiences it as targeted delegitimization. The observer seat sees all three as structurally simultaneous — the constraint IS the structured contestation itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Analytic practitioners (agenda_setter, organized power, mobile exit) are near the beneficiary end — they collect professional coherence and methodological leverage. Institutional reformers (beneficiary, moderate power, constrained exit) benefit from the framework's critical vocabulary but depend on it. Normative absolutists (payer, identity_locked exit) bear the epistemic cost: their core principle is frustrated by the hybrid rule, and their identity is fused to that principle. Extractive institutions (victim/excluded, institutional power) bear the material cost of the framework's critical deployment. Kernel reading theorists (observer, analytical) sit outside the extraction/coordination structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unstable peripheral classification) remains live — the sibling readings prove it. The constraint has not atrophied into a piton because the contestation is productive: it generates the structured negotiation that the hybrid rule makes explicit. Mandatrophy would occur if the core destabilized or if one sibling reading achieved dominance, collapsing the hybrid into either immutable_diagnostic or rhetorical_scaffold. Neither has happened.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    observational_anchor_stability,
    'Will the observational anchor for mountain and rope classifications remain stable as the framework expands to new domains, or will the core itself become contested?',
    'Track classification stability in new domain applications (e.g., AI alignment constraints, planetary boundary constraints). If mountain/rope classifications fracture, the hybrid reading''s core/periphery distinction collapses.',
    'If the core destabilizes, the hybrid reading loses its fixed point and converges toward rhetorical_scaffold (all classification contested) or fragments. The kernel''s three-reading structure would need re-authoring.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(observational_anchor_stability, empirical, 'Whether the mountain/rope observational anchor holds under domain expansion').

omega_variable(
    normative_legitimacy_source,
    'What legitimizes the normative judgments at the contested periphery — intersubjective consensus among practitioners, procedural fairness of the classification process, or consequentialist improvement in institutional outcomes?',
    'Analyze the actual justification practices in published constraint stories for tangled_rope and snare classifications. Code the justificatory repertoire and test which legitimation mode predicts classification stability.',
    'If legitimacy is purely consensus-based, the periphery is vulnerable to capture by dominant interpretive communities. If procedural, the classification process itself becomes the extraction site. If consequentialist, the framework inherits all problems of outcome measurement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_legitimacy_source, conceptual, 'The normative grounding of peripheral classification judgments').

omega_variable(
    kernel_reading_coexistence,
    'Can the three kernel readings stably coexist as live positions, or does the hybrid reading''s structural suppression of the other two create an instability that will resolve into dominance by one reading?',
    'Longitudinal tracking of citation networks, institutional adoptions, and training curricula. Measure whether analytic practitioners increasingly cite only the hybrid reading, or whether all three remain actively maintained.',
    'If coexistence is unstable, the kernel''s interpretive pluralism is transient — one reading will capture the framework''s institutional seat and the others become historical footnotes. This would change the constraint''s type from tangled_rope (active contestation) toward rope (stabilized coordination) or snare (captured vocabulary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, empirical, 'Whether the three-reading kernel structure is dynamically stable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(defe_tr_t4, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(defe_tr_t8, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 8, 0.19).
narrative_ontology:measurement(defe_tr_t12, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement(defe_tr_t16, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(defe_be_t4, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(defe_be_t8, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(defe_be_t12, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 12, 0.34).
narrative_ontology:measurement(defe_be_t16, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(defe_su_t4, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 4, 0.38).
narrative_ontology:measurement(defe_su_t8, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(defe_su_t12, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement(defe_su_t16, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(deferential_realism_ontology__hybrid_pragmatic_reading, 0.1).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).

% DUAL FORMULATION NOTE:
% The deferential_realism_ontology kernel decomposes into three constraint stories (this hybrid_pragmatic_reading, immutable_diagnostic_reading, rhetorical_scaffold_reading) linked by network.affects_constraints. Each reading instantiates a different constraint with different ε, beneficiaries, victims, and claimed_type. The hybrid reading's ε is hybrid (observational for core, constructed for periphery); the immutable reading's ε is uniformly low (all types observational); the rhetorical reading's ε is uniformly high (all types constructed). They share the same six-type vocabulary but disagree on its epistemic status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, moderate, 0.75).
constraint_indexing:directionality_override(deferential_realism_ontology__hybrid_pragmatic_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
