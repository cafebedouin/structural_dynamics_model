% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy_hybrid_pragmatic, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Manifesto Marriage Doctrine: Hybrid Pragmatic Reading
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   A religious institution facing federal legal pressure to reverse a core
 *   doctrinal commitment publishes the Manifesto as a prophetic revelation
 *   from authoritative religious leadership. The reversal is framed as
 *   divinely commanded rather than institutionally pragmatic, permitting the
 *   leadership to claim compliance with both federal law and theological
 *   continuity. This reading instantiates ONE of three structurally distinct
 *   interpretations of the Manifesto's legitimacy: (1)
 *   endogenous_reinterpretation_reading: the reversal is authentic divine
 *   revelation; (2) exogenous_override_reading: the reversal is federal
 *   coercion forcing pure pragmatic capitulation with no theological
 *   justification; (3) hybrid_pragmatic_reading (this constraint): the
 *   reversal deploys prophetic authority strategically to manage the
 *   contradiction between federal pressure and doctrinal preservation,
 *   preserving institutional leadership's interpretive authority while
 *   leaving the membership bearing interpretive ambiguity. The three readings
 *   have the same Manifesto as their object but different ε values, different
 *   beneficiary/victim structures, and different classifications. Each is
 *   generated as a separate constraint story linked by
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - institutional_leadership: Agenda-setter (interprets, publishes, controls the framing of the Manifesto). Primary beneficiary—gains both federal compliance and doctrinal flexibility. High power, arbitrage-grade exit (can shift interpretation framework).
 *   - rank_and_file_membership: Payer through identity-lock (must accept the reversal or leave their faith community; exit costs are social, relational, and identity-constituting). Moderate power, trapped exit through identity fusion.
 *   - doctrinal_traditionalists: Victims (their theological reading is suppressed by the leadership's prophetic framing; dissent appears as rejection of revelation). Powerful locally but constrained by institutional hierarchy.
 *   - federal_authorities: Excluded (created the crisis through legal pressure but do not participate in the theological reading; structurally necessary but not seated).
 *   - prophetic_authority_doctrine: Non-agent beneficiary (the hybrid reading instantiates and vindicates the doctrine that prophetic revelation is live authority, regardless of pragmatic motivation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.64).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Manifesto Marriage Doctrine: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '0aa771a8-d669-4eb0-9568-77424977ccb9').
narrative_ontology:cs_kernel_codification('0aa771a8-d669-4eb0-9568-77424977ccb9', fixed_text).
narrative_ontology:cs_authority_grounding('0aa771a8-d669-4eb0-9568-77424977ccb9', extraction).
narrative_ontology:cs_interpretation_layer_present('0aa771a8-d669-4eb0-9568-77424977ccb9').
narrative_ontology:cs_reading_relation('0aa771a8-d669-4eb0-9568-77424977ccb9', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('0aa771a8-d669-4eb0-9568-77424977ccb9', marriage_commitment_legitimacy__exogenous_override_reading, influences).
narrative_ontology:cs_axiom('0aa771a8-d669-4eb0-9568-77424977ccb9', foundational, prophetic_authority_deployed_strategically).
narrative_ontology:cs_axiom_status(prophetic_authority_deployed_strategically, holdable).
narrative_ontology:cs_axiom_grounding('0aa771a8-d669-4eb0-9568-77424977ccb9', prophetic_authority_deployed_strategically, deontological).
narrative_ontology:cs_axiom('0aa771a8-d669-4eb0-9568-77424977ccb9', foundational, institutional_preservation_justifies_scope_ambiguity).
narrative_ontology:cs_axiom_status(institutional_preservation_justifies_scope_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('0aa771a8-d669-4eb0-9568-77424977ccb9', institutional_preservation_justifies_scope_ambiguity, instrumental).
narrative_ontology:cs_reference_frame('0aa771a8-d669-4eb0-9568-77424977ccb9', prophetic_authority_preserving_institutional_unity).
narrative_ontology:cs_drift_state('0aa771a8-d669-4eb0-9568-77424977ccb9', contemporary_post_federal_pressure_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0aa771a8-d669-4eb0-9568-77424977ccb9', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_membership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, doctrinal_traditionalists).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because institutional leadership benefits from both-and positioning (federal compliance + doctrinal preservation) while membership bears either-or burden (authentic revelation OR betrayal). Suppression is elevated (0.64) because the constraint's persistence depends on suppressing the traditionalist reading and the rank-and-file's interpretive uncertainty. Theater ratio is elevated at midpoint (0.50+) because the prophetic framing performs a function: it permits institutional unity despite underlying contradiction. The measurement series show extraction and theater rising slightly during crisis (0-15) and plateauing as the new interpretation stabilizes (15-40). Suppression_requirement tracks similarly—initial effort to establish the framing, then maintenance suppression to prevent dissent from surfacing. The shared time grid ensures every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From institutional leadership's perspective, the Manifesto is successful crisis management preserving both federal compliance and theological authority—a genuine coordination achievement. From rank-and-file perspective, the Manifesto is ambiguous: it could be authentic revelation or pragmatic cover story, and they cannot definitively tell which. From doctrinal traditionalist perspective, the Manifesto is institutional betrayal dressed in prophetic language—extraction of their interpretive authority through suppression of dissent. The engine computes these divergences from the structural data: beneficiary (leadership) vs. victims (membership and traditionalists) sit on opposite ends of directionality; their computed types diverge directly from the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership: beneficiary of both-and positioning, arbitrage-grade exit (can shift interpretive frames without losing institutional seat), high power → low d, subsidy-ward. Rank-and-file membership: pay through identity-lock (must accept or leave community), moderate power, trapped exit → high d, extraction-ward. Doctrinal traditionalists: victims of interpretive suppression, powerful but constrained by hierarchy, dissent suppressed → high d, extraction-ward. The leadership's d is derived beneficiary status (they collects the benefit of both federal compliance and doctrinal preservation, which is the extraction mechanism's gain structure), while the membership's d is derived from victim status (they bear the interpretive uncertainty cost).
 *
 * MANDATROPHY ANALYSIS:
 *   The Manifesto appears as tangled_rope from leadership's seat (genuine coordination of crisis management + asymmetric benefit), but appears as snare from victim seats (extraction of interpretive authority through suppression of dissent, no coordination benefit to rank-and-file). The classification prevents the mistake of reading this as pure rope (genuine coordination from all seats) or pure snare (extraction from all seats). It is hybrid: coordination function exists (crisis response, institutional survival) AND extraction function exists (interpretive authority redistribution, suppression of traditionalist reading). Active enforcement is required: the leadership must continuously defend the prophetic framing against the counter-narrative that it was pragmatic capitulation. The constraint persists because institutional authority is invested in maintaining the framing, not because the membership would independently accept it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_revelation_vs_pragmatic_framing,
    'Was the Manifesto an authentic prophetic revelation from religious authority, or a strategic deployment of prophetic framing to manage pragmatic institutional needs?',
    'Post-institutional disclosure (memoirs, leaked deliberations, or historical investigation of leadership''s decision-making process before the Manifesto was published); theological analysis of whether the Manifesto''s theological content is consistent with prior prophetic claims in the tradition.',
    'If revelation: the constraint reclassifies toward genuine coordination (the reversal is theologically justified and not extraction). If pragmatic framing: the constraint remains extractive and the suppression of the traditionalist reading is itself evidence of extraction. This is the irreducible uncertainty under the hybrid reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prophetic_revelation_vs_pragmatic_framing, empirical, 'Whether the prophetic authority claim is authentic or strategic framing.').

omega_variable(
    identity_locked_suppression_persistence,
    'Is the measured suppression structural (external coercion preventing dissent from surfacing) or internalized (the membership has absorbed the framing and would defend it even after coercive authority breaks)?',
    'Post-defection trajectory: if members who leave the institution continue to defend the Manifesto''s prophetic authority, suppression is partly internalized; if they revert to the traditionalist reading after leaving, suppression was mostly structural.',
    'If internalized: the constraint''s effective suppression is higher than the authored scalar suggests; the membership carries the suppression with them after exit, and the identity-lock is deeper. If structural: exit would enable rapid reinterpretation, and the constraint depends on active enforcement more heavily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_suppression_persistence, empirical, 'Structural vs. internalized suppression mechanism in identity-locked context.').

omega_variable(
    federated_relief_vs_doctrinal_survival,
    'Did the institution''s leadership view the Manifesto primarily as a relief mechanism against federal pressure, or primarily as a doctrinal preservation mechanism that happened to satisfy federal demands?',
    'Counterfactual institutional decision-making: if federal pressure had not existed, would leadership have published the Manifesto anyway? Leadership testimony (if reliable) on what prompted the decision.',
    'If relief-primary: the constraint is more extractive and less coordinating (federal pressure is the real beneficiary of the arrangement). If doctrinal-survival-primary: the constraint is more of a genuine institutional adaptation (preserving the core function at cost of doctrinal ambiguity). This maps to directionality: leadership''s true d value depends on which motivation was primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federated_relief_vs_doctrinal_survival, empirical, 'Whether the Manifesto was primarily crisis response to exogenous pressure or primarily institutional self-preservation.').

omega_variable(
    alternative_framings_foreclosed,
    'Is the hybrid reading the only coherent framing under which institutional authority can be maintained, or could leadership have sustained authority through the endogenous (revelation) or exogenous (coercion) readings instead?',
    'Comparative institutional analysis: did other religious institutions facing similar federal pressure adopt different readings and maintain authority? What happened to institutions that explicitly rejected the pragmatic reading?',
    'If the hybrid reading is unique in permitting authority preservation: it is genuinely adaptive and somewhat coordinating. If alternative framings were viable but suppressed: the constraint is more purely extractive (leadership chose the hybrid reading to maximize their benefit, not because it was the only option).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framings_foreclosed, conceptual, 'Whether the hybrid reading was structurally necessary or strategically chosen among alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 10, 0.47).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 15, 0.5).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 40, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the marriage_commitment_legitimacy kernel. The ε-invariance principle (DP-001) requires separate constraint stories for structurally distinct readings because their ε values, beneficiary/victim structures, and classifications differ materially. The endogenous reading frames the reversal as authentic revelation (low ε, coordination-primary); the exogenous reading frames it as federal coercion (high ε, extraction-pure); this hybrid reading frames it as strategic institutional adaptation deploying prophetic authority (moderate ε, tangled_rope). The three stories are linked via network.affects_constraints so the system can model the constraint family: they share a kernel (the Manifesto text), but each story measures a different reading's structural claim. The family relationship is: exogenous_override → hybrid_pragmatic ← endogenous_reinterpretation (the hybrid reading influences both siblings by establishing a middle ground that allows institutional authority to claim both-and rather than either-or).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_legitimacy__hybrid_pragmatic_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
