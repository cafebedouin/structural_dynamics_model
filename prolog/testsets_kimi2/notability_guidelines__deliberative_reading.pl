% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deliberative_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: WP:N Deliberative Reading â AfD as Perpetual Boundary Negotiation
 *   domain: digital commons governance / knowledge infrastructure
 *
 * SUMMARY:
 *   This constraint instantiates the deliberative_reading of the
 *   notability_guidelines kernel. In this reading, Wikipedia's Notability
 *   policy (WP:N) is not a fixed epistemic filter nor a structural
 *   gatekeeping apparatus, but a perpetual negotiation process in which
 *   encyclopedic boundaries are produced through Articles for Deletion
 *   deliberation. Notability is an output of process, not an input. The
 *   kernel is contested: the deletionist_reading treats WP:N as a necessary
 *   quality filter preventing commons degradation, while the
 *   inclusionist_reading treats it as systematic exclusion of marginalized
 *   knowledge. This reading acknowledges the genuine coordination
 *   problemâresolving scope disputes without endless edit warsâwhile
 *   treating the mechanism as provisional and revisable.
 *
 * KEY AGENTS:
 *   - Established editors (beneficiary / organized / constrained exit): primary participants in AfD who gain a structured forum for enforcing quality norms.
 *   - Wikipedia administrators (agenda_setter / institutional / identity_locked): close discussions and convert deliberative output into binding decisions.
 *   - New contributors (payer / powerless / trapped): bear the costs of deleted labor and face opacity in the deliberative process.
 *   - Readers (beneficiary / organized / mobile): receive quality-controlled content indirectly.
 *   - Marginalized knowledge advocates (excluded / moderate / constrained): structurally disadvantaged in sourcing asymmetries despite nominal access.
 *   - Academic observers (observer / analytical / analytical): study the regime without direct stakes.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.35).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.4).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "WP:N Deliberative Reading â AfD as Perpetual Boundary Negotiation").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital commons governance / knowledge infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deliberative_reading).
narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, '0e15385e-8738-4193-a084-79488d32c53b').
narrative_ontology:cs_kernel_codification('0e15385e-8738-4193-a084-79488d32c53b', distributed).
narrative_ontology:cs_authority_grounding('0e15385e-8738-4193-a084-79488d32c53b', practice).
narrative_ontology:cs_interpretation_layer_present('0e15385e-8738-4193-a084-79488d32c53b').
narrative_ontology:cs_reading_relation('0e15385e-8738-4193-a084-79488d32c53b', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('0e15385e-8738-4193-a084-79488d32c53b', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('0e15385e-8738-4193-a084-79488d32c53b', foundational, notability_emerges_from_deliberation).
narrative_ontology:cs_axiom_status(notability_emerges_from_deliberation, holdable).
narrative_ontology:cs_axiom_grounding('0e15385e-8738-4193-a084-79488d32c53b', notability_emerges_from_deliberation, conventional).
narrative_ontology:cs_axiom('0e15385e-8738-4193-a084-79488d32c53b', foundational, afd_as_sole_legitimate_boundary_forum).
narrative_ontology:cs_axiom_status(afd_as_sole_legitimate_boundary_forum, holdable).
narrative_ontology:cs_axiom_grounding('0e15385e-8738-4193-a084-79488d32c53b', afd_as_sole_legitimate_boundary_forum, conventional).
narrative_ontology:cs_reference_frame('0e15385e-8738-4193-a084-79488d32c53b', open_deliberative_commons).
narrative_ontology:cs_drift_state('0e15385e-8738-4193-a084-79488d32c53b', contemporary_wikipedia_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0e15385e-8738-4193-a084-79488d32c53b', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, established_editors).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, readers).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, new_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experienced Wikipedia editors who regularly participate in Articles for Deletion debates. They rely on the notability deliberation process to resolve content disputes, allocate attention, and enforce quality norms. Their editorial identity and social capital are tied to the project, making exit costly despite the volunteer nature of participation.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, established_editors, beneficiary,
    organized, biographical, constrained, global).

% Trusted volunteers with advanced permissions who close AfD discussions, interpret rough consensus, and enforce notability outcomes. They act as the final translators of deliberative output into binding content decisions, and their authority derives from accumulated community trust rather than external appointment.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_administrators, agenda_setter,
    institutional, generational, identity_locked, global).

% Novice editors who create new articles, often without deep familiarity with notability criteria or AfD conventions. They bear the cost of rejected labor when articles are deleted and frequently lack the social capital to effectively advocate for their content within the deliberative forum, experiencing the process as an opaque barrier rather than an open negotiation.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, new_contributors, payer,
    powerless, immediate, trapped, global).

% End users who consume Wikipedia content. They benefit indirectly from the quality control and scope discipline that AfD deliberation provides, receiving a more reliable reference work at the cost of narrower coverage and slower growth.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, readers, beneficiary,
    organized, biographical, mobile, global).

% Editors and activists who argue that notability criteria systematically disadvantage marginalized topics, non-Western subjects, and underrepresented communities due to asymmetries in mainstream sourcing. They are nominally present in AfD but find the deliberative deck stacked against topics that lack prestigious citation infrastructure.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, marginalized_knowledge_advocates, excluded,
    moderate, generational, constrained, global).

% Researchers of digital governance and platform constitutionalism who study Wikipedia's notability regime as an exemplar of crowdsourced epistemic governance. They hold no stakes in specific AfD outcomes but analyze the structural dynamics of boundary negotiation.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, academic_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, repeatable forum for resolving disputes about whether a topic merits encyclopedic coverage, converting potentially destructive edit wars and inclusion/exclusion conflicts into episodic deliberative processes with provisional closure mechanisms.
% TRANSFER_FUNCTION: Moves volunteer editorial labor and attention away from direct content creation toward boundary-maintenance deliberation; moves authority to determine encyclopedic scope from individual editorial judgment to a collective, precedent-aware process.
% ABSENT_VOICES: Subjects of biographical articles, marginalized communities with poor mainstream sourcing, and non-English knowledge traditions are structurally underrepresented in AfD debates because they lack the citation infrastructure or social capital to make their case effectively within the deliberative frame.
% DISAPPEARANCE_RATIONALE: If the notability deliberation scaffold vanished, the encyclopedia would lose its primary mechanism for resolving scope disputes. Content quality would likely degrade or fragment into factional edit wars, and the boundary between encyclopedia and indiscriminate directory would dissolve, forcing a fundamental reorganization of editorial practice.
% FOUNDING_PROBLEM: Early Wikipedia faced exponential unfiltered growth, leading to vanity pages, advertising, promotional content, and indiscriminate collections of information lacking encyclopedic focus.
% FOUNDING_PROBLEM_CORROBORATION: Early Wikipedia historians and founding editors attest to the chaos of unfiltered growth. Contemporary platform governance researchers and WMF metrics provide mixed evidence: promotional content and low-quality submissions persist, but the scale of the problem relative to total content has shifted. Corroboration from outside the established-editor beneficiary community comes from independent academic studies and WMF trust-and-safety reporting.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deliberative_reading_tests).
:- end_tests(notability_guidelines__deliberative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because the process genuinely coordinates scope disputes but extracts substantial volunteer labor and discourages new participation. Suppression is moderate (0.40): alternatives to the AfD process (forks, other wikis) exist but lack network effects, and the process suppresses unilateral inclusion. Theater ratio is modest (0.25): some bureaucratic formalism exists, but the deliberative function remains operative. Accessibility collapse (0.45) reflects that alternatives to Wikipedia's notability regime are viable but not equivalent. Resistance (0.30) captures ongoing inclusionist critique and newcomer attrition. The metrics and claimed type are authored independently: the deliberative reading claims scaffold, while the metrics acknowledge measurable extraction and enforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the established editor seat, AfD is a necessary, legitimate, and largely successful mechanism for maintaining encyclopedic quality through open deliberation. From the new contributor seat, the same process appears as an opaque, identity-bound barrier where informal norms and social capital determine outcomes more than stated guidelines. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Established editors and administrators are structural beneficiaries (low directionality): they control the process, derive social capital from it, and could not easily replicate their status outside it. New contributors are structural targets (high directionality): they bear the costs of deletion risk and lack the social capital to navigate the forum effectively. Readers are near-symmetric beneficiaries: they gain quality but do not control the process. Marginalized knowledge advocates sit at high directionality despite moderate power because the constraint's sourcing assumptions identity-lock their topics out.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this constraint as scaffold rather than rope prevents misreading the genuine enforcement costs and newcomer exclusion as pure coordination. Classifying it as scaffold rather than snare prevents ignoring the real coordination function it serves in resolving scope disputes. The founding problemâunfiltered growthâis contested but not dead, so the scaffold retains legitimacy even as its theater ratio slowly rises. If the founding problem were clearly dead and the theater ratio high, the classification would trend toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_vs_bureaucratic_drift,
    'Has AfD drifted from genuine deliberation into bureaucratic vote-counting, guideline formalism, and ritualized consensus declaration?',
    'Systematic discourse analysis of AfD threads across historical cohorts, measuring indicators of genuine deliberation (reason-giving, opinion change) versus formalistic closure (template voting, speedy keeps/deletes).',
    'If AfD has become predominantly bureaucratic, the scaffold is accruing theater and moving toward piton or snare, raising effective extraction relative to coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_vs_bureaucratic_drift, empirical, 'Whether AfD remains genuinely deliberative or has drifted into procedural formalism').

omega_variable(
    notability_sunset_condition,
    'What is the terminal condition under which the notability scaffold would no longer be needed, and is that condition attainable?',
    'Comparative analysis of mature encyclopedias and curation models; assessment of whether AI-mediated filtering or comprehensive coverage could remove the need for boundary negotiation.',
    'If no credible sunset condition exists, the scaffold risks becoming a permanent piton rather than transitional support, undermining its own justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(notability_sunset_condition, conceptual, 'Whether the notability deliberation process has a credible terminal condition').

omega_variable(
    marginalized_source_equity,
    'Can the deliberative process produce equitable outcomes for topics lacking mainstream sourcing without altering its core mechanism?',
    'Comparative outcome analysis of AfD decisions across demographic and geographic categories, controlling for source availability.',
    'If the process cannot produce equitable outcomes, the scaffold carries hidden extraction against marginalized knowledge, functioning as a tangled rope or snare for those subjects regardless of its scaffold status for the mainstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_source_equity, empirical, 'Whether AfD produces equitable outcomes for marginalized knowledge').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.25).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.34).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(notability_guidelines__deliberative_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deliberative_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
