% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: CA3 Application Thresholds (State-Centric Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 (CA3) of the 1949 Geneva Conventions establishes minimum
 *   humanitarian protections for victims of 'armed conflict not of an
 *   international character.' This constraint story instantiates the
 *   STATE-CENTRIC READING: CA3 applies only when armed violence meets
 *   demonstrable intensity (sustained combat, sophisticated weaponry,
 *   duration) AND organization thresholds (command structure, insignia,
 *   territorial control). Under this reading, low-level violence, criminal
 *   armed groups, law-enforcement operations, and internal unrest remain
 *   outside CA3's scope. States retain classification authority; threshold
 *   determination is a sovereign function, not a humanitarian one. The
 *   constraint benefits states by preserving discretion to classify
 *   situations as 'law enforcement' rather than 'armed conflict,' thereby
 *   excluding humanitarian bodies from access and oversight. It extracts from
 *   irregular combatants and civilians in contested zones who fall below the
 *   threshold and thus lack humanitarian protections. This reading COEXISTS
 *   WITH two sibling readings: the expansive_human_rights_reading (CA3
 *   applies as a floor to any organized armed violence) and the
 *   icrc_customary_reading (CA3 scope flows from evolving state practice and
 *   customary international law). These are not perspectives on one
 *   constraint — they are three distinct constraints with different ε values,
 *   different beneficiary structures, and different classification outcomes.
 *   This file documents ONLY the state-centric reading as a structurally
 *   clean constraint.
 *
 * KEY AGENTS:
 *   - state_parties: institutional beneficiary, sets the threshold, controls classification authority
 *   - regular_armed_forces: institutional beneficiary, gain operational discretion below threshold
 *   - irregular_combatants: powerless payer, identity-locked, excluded from protections
 *   - civilians_in_contested_zones: powerless payers, constrained exit, dual legal status
 *   - icrc_and_humanitarian_bodies: excluded from binding decision-making, face access barriers
 *   - icj_and_courts: observer/adjudicator, determine scope retroactively in litigated cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.79).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "CA3 Application Thresholds (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '461d10db-9db8-4e2d-a93f-289ef908d59b').
narrative_ontology:cs_kernel_codification('461d10db-9db8-4e2d-a93f-289ef908d59b', fixed_text).
narrative_ontology:cs_authority_grounding('461d10db-9db8-4e2d-a93f-289ef908d59b', extraction).
narrative_ontology:cs_interpretation_layer_present('461d10db-9db8-4e2d-a93f-289ef908d59b').
narrative_ontology:cs_reading_relation('461d10db-9db8-4e2d-a93f-289ef908d59b', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('461d10db-9db8-4e2d-a93f-289ef908d59b', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('461d10db-9db8-4e2d-a93f-289ef908d59b', foundational, state_classification_authority_over_thresholds).
narrative_ontology:cs_axiom_status(state_classification_authority_over_thresholds, holdable).
narrative_ontology:cs_axiom_grounding('461d10db-9db8-4e2d-a93f-289ef908d59b', state_classification_authority_over_thresholds, deontological).
narrative_ontology:cs_axiom('461d10db-9db8-4e2d-a93f-289ef908d59b', foundational, intensity_organization_criteria_discretionary).
narrative_ontology:cs_axiom_status(intensity_organization_criteria_discretionary, overridden).
narrative_ontology:cs_axiom_grounding('461d10db-9db8-4e2d-a93f-289ef908d59b', intensity_organization_criteria_discretionary, empirically_contingent).
narrative_ontology:cs_reference_frame('461d10db-9db8-4e2d-a93f-289ef908d59b', state_sovereign_classification_authority).
narrative_ontology:cs_drift_state('461d10db-9db8-4e2d-a93f-289ef908d59b', contemporary_court_and_customary_practice, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('461d10db-9db8-4e2d-a93f-289ef908d59b', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_parties).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, regular_armed_forces).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_contested_zones).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, minority_armed_groups).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the threshold directly gates humanitarian protections — crossing it transfers obligations FROM states TO humanitarian oversight. Below threshold, states retain law-enforcement discretion and can operate without humanitarian constraints. The measurement series shows gradual rise from 0.52 to 0.68 over the interval, reflecting accumulating case law and state practice that expand the practical threshold even as states formally defend the narrow reading. Suppression is highest among the metrics (0.79) because the constraint's persistence depends on actively defending threshold discretion against humanitarian expansion. States must continually reassert classification authority, resist humanitarian access claims, and reinterpret intensity/organization language to maintain the state-centric boundary. Theater ratio rises modestly (0.25 to 0.42) as states increasingly conduct humanitarian-style operations (medical care, judicial processes) while maintaining the legal fiction that they are law enforcement, not armed conflict. The time grid is shared: every metric is authored at every time point on the interval [0, 35]. The initial metrics (t=0) represent the state-centric reading at its formal establishment (post-1949). The final metrics (t=35) represent contemporary state practice, where courts and humanitarian bodies have incrementally expanded threshold scope despite state resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties are the structural beneficiary: they set the threshold, control classification authority, and retain discretion to classify situations as law enforcement. Their directionality is low (d near 0.0, full beneficiary): the constraint subsidizes their operational freedom. Regular armed forces are also beneficiaries (d low): threshold-exclusion of irregular combatants permits asymmetric operations without triggering humanitarian protections. Irregular combatants and civilians in contested zones are the payers (d high, near 1.0): they fall below the threshold, lack prisoner-of-war status, have no guaranteed medical care, and face potential summary execution or degrading treatment. Their identity is fused to their location/allegiance and their exit options are constrained or trapped. Humanitarian bodies and human rights advocates are excluded (not authored as payer or beneficiary) — they have no structural role in the threshold function under the state-centric reading, though they actively contest it. The ICJ and courts are analytical observers: they determine scope through litigation but do not set it.
 *
 * MANDATROPHY ANALYSIS:
 *   The state-centric reading exhibits contested mandatrophy: the founding problem (clarifying the law-enforcement/armed-conflict boundary) is partly solved but partly obsolete. Modern asymmetric warfare, drone strikes, cyber operations, and gang violence all blur the intensity/organization threshold, making the CA3 boundary less functionally clear than intended. States formally defend the narrow threshold (claiming fidelity to the founding mandate) while operationally expanding it (accepting humanitarian access, conducting documented trials, observing medical neutrality — all practical CA3 behaviors). The theater_ratio rise (0.25 to 0.42) captures this drift: states increasingly perform humanitarian compliance without crossing the threshold-classification boundary. Courts and the ICRC have incrementally lowered the practical threshold through case law and opinio juris, reducing state discretion even as states formally resist. The constraint's persistence is sustained not by continued necessity (the boundary still clarifies situations) but by state inertia and institutional identity (states are accustomed to classification authority). A genuine resolution would require either: (1) states openly accepting a lower, more expansive threshold; (2) humanitarian bodies accepting state sovereignty and narrowing their access claims; or (3) a new treaty explicitly settable thresholds. The current state is unresolved mandatrophy — the founding problem is not dead (low-level violence still exists and states still need to classify it) but the original solution (state-centric threshold) is increasingly inadequate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intensity_threshold_empirical_boundary,
    'What operational metrics (duration, casualty count, weapons sophistication, territorial extent) constitute the ''intensity'' threshold, and does state practice converge on them or remain contested?',
    'Systematic analysis of state characterizations in 30+ recent conflicts (Rwanda, Syria, Myanmar, Yemen, etc.): extract the stated or implied thresholds each state used to classify situations, map the criteria, and test for convergence. ICJ case law provides formalized versions; ICRC incident reports provide operational descriptions.',
    'If convergence exists, the threshold is stabilizing and the state-centric reading gains predictability. If divergence persists, states are gaming the classification and the threshold provides discretion, not clarity — the constraint becomes purely extractive rather than coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intensity_threshold_empirical_boundary, empirical, 'Empirical convergence or divergence in state threshold definitions.').

omega_variable(
    humanitarian_access_outcome_causality,
    'Does the state-centric reading''s high suppression and threshold discretion actually prevent humanitarian bodies from providing protection, or do humanitarian actors find workarounds that bypass threshold classification?',
    'Documentary analysis of ICRC operations, NGO access agreements, and UN fact-finding missions in situations states classified as ''law enforcement'': measure how much humanitarian coverage actually occurs despite below-threshold classification. Track when humanitarian bodies invoke alternative legal frameworks (human rights law, customary IHL, domestic law) to provide protections that CA3 would grant.',
    'If humanitarian bodies routinely find functional workarounds, the suppression is theatrical and the constraint is mostly piton (inertial, theatrically maintained, functionally bypassed). If suppression actually blocks access, the constraint''s extractiveness is real and the reading''s protection denial is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_access_outcome_causality, empirical, 'Whether state threshold enforcement actually suppresses humanitarian access or merely displaces it.').

omega_variable(
    axiom_overriding_via_customary_evolution,
    'Does state practice on CA3 application in recent conflicts (post-2000) indicate that states are accepting a lower, de facto threshold even while defending the formal narrow reading?',
    'Opinio juris analysis: map state statements on CA3 application across the 2000–2025 period, identifying when states ACCEPTED (rather than resisted) humanitarian protections in situations they classified as law enforcement. Track whether acceptance has accumulated to establish customary modification of the CA3 kernel.',
    'If customary practice has lowered the threshold, the state-centric reading''s axiom (state classification authority over a high threshold) is being overridden through state action itself, and the icrc_customary_reading better describes current practice. This would suggest the state-centric reading is foreclosed by evolving state commitment, not by the expansive_human_rights reading, but by the ICRC reading (a sibling, not a rival).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(axiom_overriding_via_customary_evolution, empirical, 'Whether customary international law evolution has superseded the state-centric axiom.').

omega_variable(
    suppress_vs_internalize_mechanism,
    'Is the measured suppression (0.79) structural (states actively resist humanitarian access and court jurisdiction) or internalized (irregular combatants and below-threshold groups have incorporated state authority and do not expect protections)?',
    'Post-threshold-recognition data: track what happens when a conflict crosses into unambiguous CA3 scope — do protected groups immediately demand and receive humanitarian access (suppression was structural), or do they continue to expect law-enforcement treatment (suppression is internalized)? Compare across groups that had long periods below-threshold vs. sudden threshold crossing.',
    'If structural, the suppression is a real mechanism requiring state enforcement and vulnerable to humanitarian pressure. If internalized, the apparent suppression persists even when formal barriers fall — the constraint carries forward through target psychology, not state machinery. This determines whether humanitarian remedies (access, documentation, courts) would actually increase protections or merely surface pre-existing acceptance of exclusion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppress_vs_internalize_mechanism, empirical, 'Suppression mechanism: structural enforcement vs. internalized acceptance.').

omega_variable(
    kernel_reading_committer_ambiguity,
    'Is the state-centric reading a coherent interpretation of the CA3 kernel, or is it a constructed reading designed to preserve state discretion despite the kernel''s humanitarian intent?',
    'Hermeneutic analysis of the 1949 Geneva Conventions travaux préparatoires (preparatory documents) and the ICRC''s founding commentary on CA3. Compare what negotiators intended (did they intend intensity/organization thresholds as state tools or humanitarian floors?) vs. what state practice has made of the text. If travaux show humanitarian intent but state practice has inverted it, the reading is committer-constructed, not kernel-discovered.',
    'If the state-centric reading is merely constructed (committer-created, not kernel-discovered), it is on weaker ground than the humanitarian readings which align with stated travaux intent. This would support reclassification toward the expansive reading. If travaux actually support threshold discretion, the state-centric reading is a legitimate kernel instantiation and the humanitarian reading is an expansive reinterpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_ambiguity, conceptual, 'Whether the state-centric reading is discovered in the kernel text or constructed to protect state interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comm_tr_t5, common_article_3_scope__state_centric_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(comm_tr_t10, common_article_3_scope__state_centric_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(comm_tr_t15, common_article_3_scope__state_centric_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(comm_tr_t20, common_article_3_scope__state_centric_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(comm_tr_t25, common_article_3_scope__state_centric_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(comm_tr_t30, common_article_3_scope__state_centric_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(comm_tr_t35, common_article_3_scope__state_centric_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(comm_be_t5, common_article_3_scope__state_centric_reading, base_extractiveness, 5, 0.56).
narrative_ontology:measurement(comm_be_t10, common_article_3_scope__state_centric_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(comm_be_t15, common_article_3_scope__state_centric_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(comm_be_t20, common_article_3_scope__state_centric_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(comm_be_t25, common_article_3_scope__state_centric_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement(comm_be_t30, common_article_3_scope__state_centric_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(comm_be_t35, common_article_3_scope__state_centric_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.71).
narrative_ontology:measurement(comm_su_t5, common_article_3_scope__state_centric_reading, suppression_requirement, 5, 0.73).
narrative_ontology:measurement(comm_su_t10, common_article_3_scope__state_centric_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(comm_su_t15, common_article_3_scope__state_centric_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(comm_su_t20, common_article_3_scope__state_centric_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(comm_su_t25, common_article_3_scope__state_centric_reading, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(comm_su_t30, common_article_3_scope__state_centric_reading, suppression_requirement, 30, 0.79).
narrative_ontology:measurement(comm_su_t35, common_article_3_scope__state_centric_reading, suppression_requirement, 35, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__state_centric_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% The common_article_3_scope kernel admits three structurally distinct readings with different ε values and beneficiary/victim structures. This story (state_centric_reading) claims rope (coordination framing) but metrics describe tangled_rope (coordination + extraction + enforcement). The expansive_human_rights_reading applies CA3 as a humanitarian floor to all organized violence, removing threshold discretion (high extraction from states, low extraction from irregular combatants, different type). The icrc_customary_reading anchors CA3 scope in evolving state practice and opinio juris, making the boundary dynamic rather than fixed (different ε). Each reading is a separate constraint story linked by network.affects_constraints. The state-centric reading INFLUENCES both siblings by asserting state classification authority (upstream), which conditions what counts as state practice (customary reading) and what states can resist (human rights reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
