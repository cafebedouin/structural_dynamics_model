% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deliberative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: notability_guidelines__deliberative_reading
 *   human_readable: Wikipedia Notability Guidelines: Deliberative Process Reading
 *   domain: digital_commons_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   This constraint is the deliberative reading of the Wikipedia notability
 *   guidelines kernel. It treats WP:N not as a fixed epistemic filter but as
 *   a perpetual negotiation process in which notability boundaries evolve
 *   through Articles for Deletion deliberation. The constraint is claimed as
 *   a governance scaffold: a transitional coordination mechanism whose
 *   justification is the production of legitimate boundaries through open
 *   deliberation, not the steady-state exclusion of topics. Sibling readings
 *   frame the same guideline text as either a necessary quality filter
 *   (deletionist) or structural gatekeeping (inclusionist).
 *
 * KEY AGENTS:
 *   - wikipedia_administrators (organized/identity_locked): Administer and enforce AfD outcomes; derive social capital and identity from governance role.
 *   - encyclopedia_readers (organized/mobile): Receive curated content; diffuse beneficiaries of boundary maintenance.
 *   - inclusionist_advocates (moderate/constrained): Benefit from deliberative venue but often lose individual debates.
 *   - deletionist_advocates (moderate/constrained): Benefit when deliberation yields stricter exclusion outcomes.
 *   - marginalized_topic_editors (powerless/constrained): Bear disproportionate deletion burden and erasure of contributions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deliberative_reading, 0.45).
domain_priors:suppression_score(notability_guidelines__deliberative_reading, 0.5).
domain_priors:theater_ratio(notability_guidelines__deliberative_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(notability_guidelines__deliberative_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deliberative_reading, scaffold).
narrative_ontology:human_readable(notability_guidelines__deliberative_reading, "Wikipedia Notability Guidelines: Deliberative Process Reading").
narrative_ontology:topic_domain(notability_guidelines__deliberative_reading, "digital_commons_governance/knowledge_infrastructure").

narrative_ontology:has_sunset_clause(notability_guidelines__deliberative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deliberative_reading, '8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00').
narrative_ontology:cs_kernel_codification('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', distributed).
narrative_ontology:cs_authority_grounding('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', practice).
narrative_ontology:cs_interpretation_layer_present('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00').
narrative_ontology:cs_reading_relation('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', notability_guidelines__deletionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_axiom('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', foundational, notability_is_process_output).
narrative_ontology:cs_axiom_status(notability_is_process_output, holdable).
narrative_ontology:cs_axiom_grounding('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', notability_is_process_output, conventional).
narrative_ontology:cs_axiom('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', foundational, afd_deliberation_produces_legitimate_boundaries).
narrative_ontology:cs_axiom_status(afd_deliberation_produces_legitimate_boundaries, holdable).
narrative_ontology:cs_axiom_grounding('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', afd_deliberation_produces_legitimate_boundaries, conventional).
narrative_ontology:cs_reference_frame('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', open_deliberative_commons).
narrative_ontology:cs_drift_state('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', post_bureaucratization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ed6f8e5-debf-47c5-b78e-7bd78bfa3b00', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deliberative_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, wikipedia_administrators).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, encyclopedia_readers).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, inclusionist_advocates).
narrative_ontology:constraint_beneficiary(notability_guidelines__deliberative_reading, deletionist_advocates).
narrative_ontology:constraint_victim(notability_guidelines__deliberative_reading, marginalized_topic_editors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Volunteer editors granted administrative tools who close Articles for Deletion threads, interpret notability guidelines, and enforce outcomes through page deletion and user sanctions. Their standing and identity within the project are deeply tied to this governance function; stepping away would mean abandoning years of accumulated social capital and role-specific reputation.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, wikipedia_administrators, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deliberative_reading, wikipedia_administrators, beneficiary).

% The global user base that consumes Wikipedia articles as a reference source. They receive a curated corpus whose boundaries are shaped by the AfD process but do not participate in the deliberations themselves.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, encyclopedia_readers, beneficiary,
    organized, biographical, mobile, global).

% Editors who regularly participate in AfD debates arguing for broader inclusion of topics. They value the existence of a formal deliberative venue and derive procedural legitimacy from it, even when individual debates yield outcomes they oppose.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, inclusionist_advocates, beneficiary,
    moderate, generational, constrained, global).

% Editors who regularly participate in AfD debates arguing for stricter inclusion standards and removal of insufficiently sourced articles. They benefit when the deliberative process yields outcomes that align with their epistemic preferences.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, deletionist_advocates, beneficiary,
    moderate, generational, constrained, global).

% Editors who create content about topics from underrepresented geographic regions, subcultures, or non-canonical knowledge traditions. Their articles are nominated for deletion at higher rates; they expend disproportionate labor defending notability and frequently see their contributions removed.
narrative_ontology:constraint_stakeholder(notability_guidelines__deliberative_reading, marginalized_topic_editors, payer,
    powerless, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a structured, repeatable deliberative venue (Articles for Deletion) where encyclopedic boundary questions are negotiated in public, producing evolving community consensus on what knowledge merits inclusion.
% TRANSFER_FUNCTION: Moves editorial attention and labor from marginalized or peripheral topic advocates into the central deliberation machinery, and moves definitional power over the encyclopedia's scope from individual editors to the aggregated consensus output of AfD threads.
% ABSENT_VOICES: Subjects of biographies and representatives of marginalized knowledge systems are rarely present in AfD deliberations; their exclusion is structural because they are not Wikipedians and do not participate in the policy grammar. Also, readers in the global south who rely on content that is subsequently deleted are not represented in the debate.
% DISAPPEARANCE_RATIONALE: If the notability guideline and its AfD deliberation scaffold vanished, the encyclopedia's boundary maintenance would collapse into uncoordinated individual editorial judgment or raw popularity contests; the current governance mechanism that produces incremental boundary evolution would disappear, and both deletionist and inclusionist coalitions would lose their central coordination venue.
% FOUNDING_PROBLEM: Early Wikipedia faced unbounded inclusion and potential degradation into an indiscriminate collection of trivia, promotional content, and unverifiable claims without a scalable mechanism to resolve inclusion disputes.
% FOUNDING_PROBLEM_CORROBORATION: Early Wikipedia historians and founding community members attest to the unbounded inclusion problem. Critical digital-commons scholars from outside the benefiting editor communities contest that the founding problem is now dead and the scaffold has become a permanent gatekeeping structure; their peer-reviewed work corroborates the obsolescence reading.
narrative_ontology:disappearance_verdict(notability_guidelines__deliberative_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deliberative_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deliberative_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deliberative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deliberative_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.45) is moderate because the AfD process extracts substantial editorial labor and excludes legitimate knowledge, yet it still coordinates a genuine dispute that would otherwise be resolved through edit warring. Suppression (0.50) reflects the degree to which alternative pathways for knowledge inclusion are closed off by the bureaucratic procedure. Theater ratio (0.35) captures the growing performative dimension of policy citation in AfD debates, where guideline invocation substitutes for substantive engagement. Accessibility collapse (0.55) acknowledges that once an editor understands the notability system, alternatives within Wikipedia are limited to compliance or exit. Resistance (0.55) reflects ongoing contestation by inclusionists and marginalized editors. Metrics and claim are authored independently: the scaffold claim is structural (the arrangement was built as transitional coordination), while the metrics describe a mechanism that has drifted toward greater extraction and theatricality over its interval.
 *
 * PERSPECTIVAL GAP:
 *   From the administrator and established-editor seat, the constraint is a necessary deliberative scaffold that prevents uncoordinated chaos and produces evolving consensus. From the marginalized-topic-editor seat, the same mechanism appears as an exclusionary apparatus that extracts their labor and discards their contributions under the guise of procedural fairness. The engine computes this divergence from the structural asymmetry in exit options and role declarations; the authored claim does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Wikipedia administrators sit near the beneficiary end (low d): they are structurally empowered by the mechanism, though not financially enriched. Encyclopedia readers, inclusionist advocates, and deletionist advocates also sit toward the beneficiary side because the deliberative process coordinates their competing preferences and gives them a venue. Marginalized topic editors sit toward the target end (high d): they bear the costs of the boundary-setting mechanism through deleted labor and excluded knowledge. Their constrained exit amplifies effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunbounded inclusion threatening commons qualityâis contested rather than dead. Because the problem status is contested and the mechanism still coordinates a live dispute between inclusionists and deletionists, the scaffold classification is structurally warranted despite drift. If the founding problem were clearly dead and the mechanism persisted purely by inertia, it would compute as a piton; if it had no coordination function and pure extraction, it would compute as a snare. The deliberative reading preserves the coordination claim by locating legitimacy in the process itself, not the output.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberative_vs_fixed_boundary,
    'Does the AfD deliberative process genuinely produce evolving notability boundaries, or has it stabilized into a fixed epistemic filter despite the procedural rhetoric?',
    'Longitudinal statistical analysis of AfD outcomes across decades to determine whether the acceptance rate for marginal topics trends upward, oscillates around a stable mean, or trends downward.',
    'If boundaries are stable despite procedural rhetoric, the scaffold claim fails and the constraint computes as tangled_rope or snare; if genuinely evolving, the scaffold classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberative_vs_fixed_boundary, empirical, 'Whether AfD produces evolving boundaries or stable exclusion').

omega_variable(
    sunset_mechanism_absence,
    'Is the transitional intent of the notability scaffold honored in practice, or has the mechanism become self-perpetuating in the absence of any sunset clause or termination condition?',
    'Examine policy pages and community discussions for explicit sunset provisions or serious proposals to abolish notability guidelines; assess whether any transition state has been defined.',
    'Absence of any sunset mechanics undermines the scaffold classification and pushes toward piton or snare depending on beneficiary structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_mechanism_absence, conceptual, 'Whether the scaffold lacks a genuine sunset mechanism').

omega_variable(
    committer_influence_on_siblings,
    'Does the deliberative reading''s procedural legitimacy frame primarily serve to legitimize outcomes that align with the deletionist reading?',
    'Discourse analysis of AfD arguments to measure the relative frequency of procedural fairness claims versus substantive inclusion claims, correlated with outcomes.',
    'If procedural legitimacy is deployed asymmetrically to defend exclusion, the deliberative reading functions as a cover story for the deletionist reading and should be reclassified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_influence_on_siblings, conceptual, 'Whether deliberative framing legitimately neutral or deletionist cover').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deliberative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deliberative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deliberative_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deliberative_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deliberative_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deliberative_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deliberative_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deliberative_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deliberative_reading, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deliberative_reading, base_extractiveness, 8, 0.31).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deliberative_reading, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deliberative_reading, base_extractiveness, 16, 0.42).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deliberative_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deliberative_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(nota_su_t4, notability_guidelines__deliberative_reading, suppression_requirement, 4, 0.22).
narrative_ontology:measurement(nota_su_t8, notability_guidelines__deliberative_reading, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(nota_su_t12, notability_guidelines__deliberative_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(nota_su_t16, notability_guidelines__deliberative_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deliberative_reading, suppression_requirement, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__deletionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deliberative_reading, notability_guidelines__inclusionist_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'Wikipedia Notability Guidelines' conflates three structurally distinct constraints: the deletionist reading (quality filter), the deliberative reading (negotiation scaffold), and the inclusionist reading (gatekeeping apparatus). Each reading has a distinct epsilon, stakeholder structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
