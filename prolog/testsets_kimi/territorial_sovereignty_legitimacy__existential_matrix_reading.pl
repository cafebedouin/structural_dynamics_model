% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Territorial Sovereignty as Existential Zero-Sum Matrix
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This constraint story instantiates the existential_matrix_reading of the
 *   territorial_sovereignty_legitimacy kernel. It models the claim that
 *   sovereignty over the contested territory is not a juridical question
 *   resolvable by law or history, but an existential precondition: each
 *   people requires exclusive territorial control for collective survival and
 *   identity expression. Under this reading, the conflict is structurally
 *   zero-sum; compromise frameworks such as the two-state solution are
 *   unstable because any territorial concession creates existential
 *   vulnerability. The beneficiary is the coalition that achieves demographic
 *   and military dominance; the victim is the subordinate population whose
 *   existential claim to the same territory is denied.
 *
 * KEY AGENTS:
 *   - israeli_state_coalition: Agenda setter and beneficiary (institutional/identity_locked) â administers military and demographic control, fused to Zionist territorial identity
 *   - palestinian_territorial_community: Payer (powerless/trapped) â bears dispossession, fragmentation, and denial of self-determination
 *   - international_peace_architecture: Observer (institutional/analytical) â produces juridical frameworks that fail against the existential logic
 *   - civic_binational_movements: Excluded (moderate/constrained) â reject the zero-sum frame, excluded by both national camps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.85).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.82).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Territorial Sovereignty as Existential Zero-Sum Matrix").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, 'abcc9fa8-9397-498a-9b1a-501369517619').
narrative_ontology:cs_kernel_codification('abcc9fa8-9397-498a-9b1a-501369517619', implicit).
narrative_ontology:cs_authority_grounding('abcc9fa8-9397-498a-9b1a-501369517619', extraction).
narrative_ontology:cs_interpretation_layer_present('abcc9fa8-9397-498a-9b1a-501369517619').
narrative_ontology:cs_reading_relation('abcc9fa8-9397-498a-9b1a-501369517619', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('abcc9fa8-9397-498a-9b1a-501369517619', territorial_sovereignty_legitimacy__self_determination_reading, forecloses).
narrative_ontology:cs_axiom('abcc9fa8-9397-498a-9b1a-501369517619', foundational, territorial_control_existential_imperative).
narrative_ontology:cs_axiom_status(territorial_control_existential_imperative, holdable).
narrative_ontology:cs_axiom_grounding('abcc9fa8-9397-498a-9b1a-501369517619', territorial_control_existential_imperative, empirically_contingent).
narrative_ontology:cs_axiom('abcc9fa8-9397-498a-9b1a-501369517619', foundational, zero_sum_collective_survival).
narrative_ontology:cs_axiom_status(zero_sum_collective_survival, holdable).
narrative_ontology:cs_axiom_grounding('abcc9fa8-9397-498a-9b1a-501369517619', zero_sum_collective_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('abcc9fa8-9397-498a-9b1a-501369517619', existential_territorial_dominance).
narrative_ontology:cs_drift_state('abcc9fa8-9397-498a-9b1a-501369517619', post_oslo_impasse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('abcc9fa8-9397-498a-9b1a-501369517619', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_state_coalition).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_territorial_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers military occupation, settlement expansion, and demographic engineering to maintain Jewish majority and territorial control. Justifies all actions through existential security discourse. Cannot abandon territorial dominance without fracturing its own political coalition and ideological self-conception.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_state_coalition, agenda_setter,
    institutional, generational, identity_locked, national).

% Bears the costs of territorial exclusion: displacement, military rule, restricted movement, and denial of sovereign self-determination. Their collective identity is also existentially tied to the same territory, locking them into resistance within a framework that offers no viable alternative to zero-sum competition.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, palestinian_territorial_community, payer,
    powerless, biographical, trapped, national).

% Produces juridical frameworks such as the Oslo Accords and two-state models that assume sovereignty legitimacy is negotiable and divisible. Their frameworks consistently fail because the local actors operate on the existential matrix rather than the juridical one.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_peace_architecture, observer,
    institutional, generational, analytical, global).

% Advocate for shared sovereignty or a civic state not defined by ethnic dominance. They reject the zero-sum existential frame but are structurally excluded from political power by both national coalitions, who treat their proposals as existential threats.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, civic_binational_movements, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, israeli_state_coalition).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective survival and identity expression for a people by securing exclusive territorial control, ensuring demographic and military dominance as a buffer against perceived annihilation.
% TRANSFER_FUNCTION: Transfers territorial control, security, and demographic dominance from the subordinate population to the dominant existential coalition; transfers existential risk, dispossession, and political erasure in the reverse direction.
% ABSENT_VOICES: Binationalists, civic-state proponents, and non-territorial identity movements who reject the zero-sum existential frame are excluded from the political architecture; their voices are absent from both national coalitions.
% DISAPPEARANCE_RATIONALE: If the existential matrix vanished, the zero-sum character of the conflict would dissolve; juridical and compromise frameworks such as a two-state solution, confederation, or civic state would become structurally viable, and the current military-domination architecture would lose its organizing principle.
% FOUNDING_PROBLEM: The problem of collective survival in a territory where multiple claimants exist and where historical persecution or dispossession creates an existential security dilemma.
% FOUNDING_PROBLEM_CORROBORATION: Israeli historians and former security officials outside the current benefiting coalition corroborate that the existential threat has been exaggerated to justify territorial expansion. Palestinian civil society and independent international legal scholars corroborate that the problem is experienced as asymmetric domination rather than a symmetrical security dilemma. No party entirely outside the conflict attests the existential frame as objective natural law.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.85) is authored high because the existential matrix frames territorial control as zero-sum: one people's security requires the other's subordination. Suppression (0.82) is high because alternatives â binationalism, shared sovereignty, or full Palestinian self-determination â are actively suppressed by military occupation, settlement expansion, and blockade. Theater ratio (0.58) reflects the performative peace process (Oslo, Roadmap) that masked ongoing domination; it rises over the interval as negotiations became ritualized while facts on the ground hardened. Accessibility collapse (0.88) is very high because, once the existential frame is accepted, no territorial compromise appears survivable. Resistance (0.85) is high because the subordinate population mounts sustained violent and non-violent opposition to the domination architecture. The measurement series share a single time grid (1993â2023) to prevent misaligned temporal sampling.
 *
 * PERSPECTIVAL GAP:
 *   The Israeli state coalition experiences the constraint as necessary coordination for collective survival; any reduction in territorial control is read as existential annihilation. The Palestinian territorial community experiences the same structure as pure extraction enforced by military dominance. The international peace architecture experiences it as a soluble juridical dispute that local actors refuse to resolve. These divergent computed types arise from the same structural data: beneficiary/victim declarations, differentiated exit options (identity_locked vs trapped), and opposed spatial scopes.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state coalition is declared a beneficiary and agenda setter with identity_locked exit; the engine derives directionality near the full-beneficiary end (low d), so effective extraction is damped or inverted into subsidy. The Palestinian territorial community is declared a victim with trapped exit; the engine derives directionality near the full-target end (high d), amplifying effective extraction. The international peace architecture sits at analytical exit with no beneficiary/victim role, producing neutral d. Civic binational movements are excluded with constrained exit; their d is moderate but they do not sit in beneficiary or victim arrays, so they do not drive the primary effective extraction computation.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the Tangled Rope classification, this constraint could be misread as a Snare (if the coordination function for the dominant group is dismissed as cover) or a Mountain (if the existential necessity of territory is treated as natural law). The Tangled Rope gate forces both the genuine coordination function (collective survival for the beneficiary) and the asymmetric extraction (denial of survival for the victim) to be declared simultaneously. This prevents collapsing a complex structure into pure extraction or pure necessity. It is not a Scaffold because no sunset clause exists; it is not a Piton because the beneficiary actively captures the gains and enforces the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the existential_matrix_reading of the territorial_sovereignty_legitimacy kernel; the sibling covenant_continuity_reading grounds legitimacy in divine covenant and international juridical acts, while self_determination_reading grounds it in modern democratic demographic majority. Would adopting either sibling reading dissolve the zero-sum structure, or would the existential matrix subsume their juridical claims?',
    'Comparative structural analysis of whether juridical legitimacy claims (covenant or self-determination) can stabilize without converging on existential domination; historical case studies of binational or civic-state resolutions to similar conflicts.',
    'If juridical claims cannot stabilize without existential backing, this constraint is fundamental; if they can, the existential matrix is a contingent political construction that could be displaced by institutional design.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural relationship between existential reading and sibling juridical readings').

omega_variable(
    existential_claim_empirical_status,
    'Is territorial control actually an existential precondition for collective survival and identity, or is this claim empirically falsifiable by cases of dispersed or non-territorial nations that persist?',
    'Comparative ethnography and political history of diaspora nations, autonomous regions, and consociational states; security studies measuring whether territorial compromise correlates with collective annihilation or merely political change.',
    'If empirically false, the constraint''s extractiveness is based on a falsified premise and the coordination function is parasitic on fear rather than survival; if empirically true in specific conditions, the extraction may be genuinely necessary coordination for the beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_claim_empirical_status, empirical, 'Whether the existential necessity of territory is empirically sound or constructed').

omega_variable(
    active_enforcement_sustainability,
    'Can military and demographic dominance be maintained indefinitely as the enforcement mechanism for this constraint, or does it generate counter-mobilization that increases resistance and undermines the dominant coalition''s stability?',
    'Longitudinal demographic and security data tracking the cost of enforcement versus the security benefits of territorial control; analysis of resistance trajectory.',
    'If enforcement generates escalating resistance, the constraint is structurally unstable and may drift toward higher extraction and suppression or collapse; if sustainable, the tangled rope stabilizes as a permanent asymmetric arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_enforcement_sustainability, empirical, 'Long-term viability of enforcement without systemic collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(terr_tr_t5, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(terr_tr_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(terr_tr_t15, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(terr_tr_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(terr_tr_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(terr_tr_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(terr_be_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(terr_be_t5, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement(terr_be_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(terr_be_t15, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(terr_be_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(terr_be_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 25, 0.81).
narrative_ontology:measurement(terr_be_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t0, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(terr_su_t5, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(terr_su_t10, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(terr_su_t15, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement(terr_su_t20, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(terr_su_t25, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(terr_su_t30, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 30, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the territorial_sovereignty_legitimacy kernel, decomposed per the epsilon-invariance principle because the kernel conflates juridical, covenantal, and existential claims that have different structural properties and epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
