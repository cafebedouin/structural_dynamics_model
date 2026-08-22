% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause: Antisubordination Reading
 *   domain: constitutional_law/civil_rights
 *
 * SUMMARY:
 *   The Equal Protection Clause of the Fourteenth Amendment ('no state shall
 *   deny to any person within its jurisdiction the equal protection of the
 *   laws') is contested across three major readings, each instantiated in
 *   different constitutional eras and courts. This story instantiates the
 *   ANTISUBORDINATION READING: the clause targets caste-like subordination of
 *   historically oppressed groups, not classification per se. Under this
 *   reading, state action that entrenches hierarchy is forbidden; state
 *   action that dismantles it is permitted. The reading is challenged by the
 *   COLORBLIND READING (which categorically forbids state use of racial
 *   classifications regardless of purpose) and the REMEDIAL READING (which
 *   permits race-conscious action when narrowly tailored to remedy documented
 *   harm). These three readings do not represent neutral angles on a single
 *   fact — they represent fundamentally different commitments about what the
 *   clause TARGETS and whose interests it protects. This story models the
 *   antisubordination reading in isolation as a constraint, with its own
 *   beneficiaries, victims, and enforcement structure.
 *
 * KEY AGENTS:
 *   - historically_subordinated_castes: Benefit from remedial state action under this reading, their groups recognized as bearing documented subordination.
 *   - remedial_state_actors: Agenda-setters — courts, legislatures, executives tasked with interpreting equal protection and deploying remedies.
 *   - dominant_groups_resisting_remediation: Bear the costs of remedial measures; denied equal protection standing against them under this reading.
 *   - neutral_classification_theorists: Their foundational principle (state must be color-blind) is deprioritized in favor of subordination-targeting.
 *   - lower_courts_and_agencies: Observers and implementers tasked with diagnosing subordination — a harder epistemic task than measuring classification.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.28).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.62).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause: Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, 'c6a8bda7-9471-4fa5-bd60-57f455ac75eb').
narrative_ontology:cs_kernel_codification('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', formalized).
narrative_ontology:cs_authority_grounding('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', lineage).
narrative_ontology:cs_interpretation_layer_present('c6a8bda7-9471-4fa5-bd60-57f455ac75eb').
narrative_ontology:cs_reading_relation('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', foundational, subordination_is_constitutional_target).
narrative_ontology:cs_axiom_status(subordination_is_constitutional_target, holdable).
narrative_ontology:cs_axiom_grounding('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', subordination_is_constitutional_target, deontological).
narrative_ontology:cs_axiom('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', foundational, state_must_dismantle_not_entrench).
narrative_ontology:cs_axiom_status(state_must_dismantle_not_entrench, holdable).
narrative_ontology:cs_axiom_grounding('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', state_must_dismantle_not_entrench, deontological).
narrative_ontology:cs_reference_frame('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', antisubordination_constitutional_authority).
narrative_ontology:cs_drift_state('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', contemporary_post_2013_court, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c6a8bda7-9471-4fa5-bd60-57f455ac75eb', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, remedial_state_actors).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_groups_resisting_remediation).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, neutral_classification_theorists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, intermediate_subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, empirical_subordination_scholars).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, intermediate_subordinated_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups with documented histories of systemic exclusion and subordination (African Americans, Native Americans, Latinos in certain contexts, women in certain domains). Under this reading, they benefit from state action that dismantles the hierarchy that has entrapped them — affirmative action, reparative policies, targeted remediation. The antisubordination frame legitimizes remedies that the colorblind reading treats as impermissible classifications.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_castes, beneficiary,
    organized, generational, constrained, national).

% Courts, legislatures, and executive bodies authorized to interpret the equal protection clause and craft remedial policy. Under antisubordination, these actors have expanded authority to deploy race-conscious measures as long as the target is subordination dismantling, not subordination entrenchment. They set the interpretive agenda by deciding what counts as subordination and what remedies are appropriate.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, remedial_state_actors, agenda_setter,
    institutional, generational, mobile, national).

% Members of historically dominant groups (whites in affirmative action contexts, men in gender-equity contexts) who bear the immediate costs of remedial policies — rejection from schools, jobs, or benefits allocated preferentially to subordinated-group members. The antisubordination reading denies them equal protection standing against remedial measures because their claim rests on classification, not on subordination. They experience suppression via legal bar to challenging remedies as race-conscious.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_groups_resisting_remediation, payer,
    powerful, biographical, constrained, national).

% Legal and philosophical traditions committed to the principle that the state must treat all citizens identically regardless of race or group membership — the colorblind or formal-equality framework. The antisubordination reading subordinates (deprioritizes) their principle by making the target of scrutiny not classification itself but the direction of the classification (toward or away from subordination). Their theoretical project is challenged by a reading that renders class-neutral principle insufficient.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, neutral_classification_theorists, payer,
    analytical, generational, analytical, national).

% Groups with mixed or contested subordination status — some Latinos, some Asian Americans, immigrant and immigrant-descended communities. Under antisubordination, their classification as beneficiary or payer depends on whether courts recognize their group as subordinated. Identity fusion to subordination status determines exit: they cannot claim neutral classification without severing the group identity that constitutes their claim to remediation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, intermediate_subordinated_groups, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_kernel__antisubordination_reading, intermediate_subordinated_groups, payer).

% District courts and administrative agencies tasked with implementing equal protection doctrine. They must interpret whether a given state action entrenches or dismantles subordination. The antisubordination reading imposes an evidentiary and conceptual burden: courts must diagnose subordination rather than merely measure classification. This generates significant discretion and interpretive friction.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, lower_courts_and_agencies, observer,
    institutional, generational, constrained, national).

% Academic and empirical researchers who document historical and ongoing subordination patterns. The antisubordination reading vindicates their research as the evidentiary foundation for interpreting equal protection — subordination is real, measurable, and the appropriate target of constitutional scrutiny. Their work becomes mandatory reading for judicial interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, empirical_subordination_scholars, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, remedial_state_actors).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared understanding that state power should dismantle rather than entrench group-based hierarchy. Resolves the collective-action problem of subordinated groups unable individually to contest entrenchment while dominant groups resist remediation. The state's role shifts from neutral arbiter to active dismantler.
% TRANSFER_FUNCTION: Moves opportunity and recognition from those who would hold subordinated positions to those occupying them. Transfers judicial authority to recognize and remedy subordination as the foundation for interpreting equal protection, away from formal classification as the locus of scrutiny.
% ABSENT_VOICES: Subordinated groups historically excluded from constitutional voice (enslaved and colonized peoples, the violently disenfranchised) cannot speak contemporaneously, but their historical exclusion is the reading's evidentiary foundation. The reading incorporates them as represented through historical documentation and descendant-community testimony rather than as current seats.
% DISAPPEARANCE_RATIONALE: If this reading and its legal authority vanished, courts would revert to colorblind or formal-equality scrutiny; remedial policies would face heightened challenges; subordination as a constitutional category would lose doctrinal force; group-based remedies would be struck down as classifications. The institutional structure of remediation depends on this reading's survival.
% FOUNDING_PROBLEM: The equal protection clause itself was written and ratified in a context of massive group-based subordination (slavery, legally mandated racial caste, sex-based hierarchy). The founding problem is: can a clause designed in and by a subordinated era effectively dismantle the subordination it was written in and around? The antisubordination reading answers: yes, if we interpret it to target subordination itself rather than mere classification.
% FOUNDING_PROBLEM_CORROBORATION: Historians and civil-rights scholars document the founding problem: Reconstruction amendments were written to address subordination and were narrowly read by later courts through formal-equality framings. The colorblind reading contests whether subordination (as opposed to classification) was the target. Empirical subordination research from outside the beneficiary community corroborates that subordination persists and is measurable. The Supreme Court's own precedents in cases like Washington v. Davis (1976) and later affirmative-action decisions show judicial contestation of whether equal protection targets classification or subordination.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) because the reading confers legitimacy and remedial authority on actions whose extraction-character depends on framing: from the antisubordination seat, affirmative action is coordination (dismantling subordination); from the colorblind seat, it is extraction (taking from one group to favor another). The authored extractiveness reflects the reading's own lights, not a neutral measure. Suppression is high (0.62) because the reading requires active prevention of certain counterarguments — the colorblind claim that remediation itself constitutes classification cannot be heard; dominant-group equal protection claims are suppressed by categorical bar. Theater is moderate (0.41) and rises slightly over time because remedial action increasingly emphasizes performative inclusion and metrics-based compliance rather than structural dismantling. The measurement series shows extractiveness and suppression rising gently over the interval (indicating institutional entrenchment and hardening of counterargument-suppression) while theater plateaus (indicating stable performance-to-function ratio).
 *
 * PERSPECTIVAL GAP:
 *   From the remedial state actor's seat, the constraint is genuine coordination — disassembling hierarchy is cooperation on a shared good. From the dominant-group seat resisting remediation, it is pure extraction and suppression — opportunity is taken and counterarguments are legally barred. From the colorblind theorist's seat, the constraint is false coordination hiding extraction — remedies are classified as coordination because subordination framing is stipulated, but the actual effect is race-conscious distribution. The engine computes these divergent seats from the structural data (power, exit, role, beneficiary/victim declaration); this reading's survival depends on the remedial-actor and subordinated-caste seats out-weighing the colorblind and dominant-group seats in institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated castes have directionality near 0.0 (full beneficiary) — they receive the constraint's coordination benefit and bear no extraction cost; exit is constrained not by this constraint but by the prior subordination it aims to dismantle. Remedial state actors are near 0.5 (symmetric) — they coordinate the dismantling function but bear the cost of defending it against legal challenge and managing the evidentiary burden. Dominant groups resisting remediation are near 1.0 (full target) — they bear extraction (opportunity cost) and experience suppression (barred from colorblind equal protection claims). Neutral classification theorists are near 0.8 (primarily target) — their theoretical apparatus is deprioritized and their claims are suppressed by the subordination-targeting logic. Intermediate subordinated groups are identity-locked near 0.3 because their position depends entirely on whether courts recognize their group as subordinated; if that recognition is withdrawn, their exit options collapse.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mislabeling pure subordination-dismantling coordination as extraction by distinguishing subordination (caste-like, historically entrenched, group-targeted) from mere disadvantage (context-dependent, individual-variable, temporary). Constraining extraction to 'remedies that entrench' rather than 'remedies that target subordination' prevents false negatives on genuine coordination. However, the reading faces a mandatrophy risk: if the founding problem (can equal protection dismantle the subordination it was written within) is declared dead (subordination substantially dismantled), then the reading's authorization for remediation loses grounding. Currently the founding problem status is CONTESTED: subordinated castes argue subordination persists, colorblind theorists argue legal equality is achieved, empirical scholars document continuing gaps. The reading survives by keeping the problem contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_vs_classification_boundary,
    'Is subordination a fact that exists independently of how courts classify it, or is it constituted by the interpretive frame that recognizes it as subordination rather than mere group difference?',
    'Empirical study of pre-recognition and post-recognition subordination trajectories: does recognition (court finding of subordination) change the subordination itself, or merely the state''s response to pre-existing subordination? Careful separation of constitutive vs. evidential accounts.',
    'If subordination is pre-interpretive (exists before courts name it), then the antisubordination reading targets real structure and discrimination by courts of what to remedy is policy, not fabrication. If subordination is constituted by the frame, then the reading is more voluntaristic — courts create the very phenomenon they claim to remedy, raising mandatrophy risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subordination_vs_classification_boundary, conceptual, 'Whether subordination is discovered or constructed by the frame that recognizes it.').

omega_variable(
    intermediate_groups_classification,
    'Which groups count as subordinated for purposes of this reading? Are boundaries stable across time and jurisdictions, or are they fluid and contestable?',
    'Longitudinal doctrinal analysis: track how courts classify groups (Native Americans, Latinos, Asian Americans, immigrant groups, disability groups, sexual minorities) across cases and decades. Catalog shifts and conflicts.',
    'If boundaries are stable and pre-constitutional (discovered), subordination has determinate reference. If boundaries are fluid and court-dependent, the reading creates incentives for groups to litigate identity and status, generating extraction-like dynamics within the remediation framework. Identity-locked exit becomes a liability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intermediate_groups_classification, empirical, 'The extensionality of the subordinated-groups set and its mutability over time.').

omega_variable(
    remediation_scope_creep,
    'What prevents remedial measures authorized under subordination-dismantling logic from expanding to serve other state purposes — not subordination dismantling but preference allocation, political patronage, or rent-seeking?',
    'Doctrinal boundary analysis: how do courts distinguish permissible subordination-dismantling from impermissible group-preference that uses subordination rhetoric as cover? Case-law patterns in remediation denials.',
    'If the boundary erodes, the reading''s authorization for race-conscious measures becomes a vehicle for new extraction mechanisms disguised as subordination remedies — false coordination. If the boundary holds, the reading succeeds in targeting subordination specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_scope_creep, empirical, 'Whether remediation-authorization scope is limited to subordination-dismantling or expands to cover patronage.').

omega_variable(
    colorblind_reading_foreclosure,
    'Does the antisubordination reading logically foreclose the colorblind reading — i.e., is the core premise of antisubordination (state SHOULD dismantle subordination via race-conscious measures) incompatible with the core premise of colorblind reading (state MUST NEVER use race-conscious measures)?',
    'Formal logical analysis: can both premises be held true in the same constitutional framework? Doctrinal case study of how courts balance them.',
    'If they foreclose each other, one reading must yield; the question becomes which has greater institutional power (currently colorblind reading dominates the Supreme Court). If they merely conflict without foreclosing, both remain live — the contest continues indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_reading_foreclosure, conceptual, 'Whether the antisubordination and colorblind readings are logically incompatible.').

omega_variable(
    suppression_mechanism_structural,
    'Is the measured suppression (0.62) structural — legal doctrinal bars to colorblind equal protection claims against remediation — or internalized — dominant-group members internalize that they do not deserve protection, making the suppression self-maintaining?',
    'Post-remediation sunset analysis: if a remedial policy sunsets and dominant-group members can challenge it under colorblind doctrines (if those doctrines were restored), does the suppression persist? Measure continued deference to subordination reasoning.',
    'If internalized, the reading''s suppression outlasts the doctrinal structure that created it — the constraint becomes self-maintaining despite its removal. If structural, removing the reading removes the suppression. This affects persistence and theater dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural, empirical, 'Whether suppression of colorblind equal protection claims is structurally or internalistically maintained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_kernel__antisubordination_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(equa_tr_t10, equal_protection_kernel__antisubordination_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(equa_tr_t20, equal_protection_kernel__antisubordination_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(equa_tr_t30, equal_protection_kernel__antisubordination_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement(equa_tr_t40, equal_protection_kernel__antisubordination_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(equa_tr_t50, equal_protection_kernel__antisubordination_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_kernel__antisubordination_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(equa_be_t10, equal_protection_kernel__antisubordination_reading, base_extractiveness, 10, 0.21).
narrative_ontology:measurement(equa_be_t20, equal_protection_kernel__antisubordination_reading, base_extractiveness, 20, 0.25).
narrative_ontology:measurement(equa_be_t30, equal_protection_kernel__antisubordination_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement(equa_be_t40, equal_protection_kernel__antisubordination_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(equa_be_t50, equal_protection_kernel__antisubordination_reading, base_extractiveness, 50, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_kernel__antisubordination_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(equa_su_t10, equal_protection_kernel__antisubordination_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(equa_su_t20, equal_protection_kernel__antisubordination_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(equa_su_t30, equal_protection_kernel__antisubordination_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement(equa_su_t40, equal_protection_kernel__antisubordination_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(equa_su_t50, equal_protection_kernel__antisubordination_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(equal_protection_kernel__antisubordination_reading, 0.18).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).

% DUAL FORMULATION NOTE:
% The antisubordination reading is one of three major interpretations of the Equal Protection Clause that diverge on what the clause targets (subordination vs. classification vs. documented historical harm) and who may claim protection (historically subordinated groups vs. all citizens equally vs. remedial actors). All three readings instantiate the same kernel (the fourteenth amendment equal protection text and doctrine) but differ on its referent and implications. The antisubordination reading forecloses the colorblind reading's core premise (state must never classify by race) while coexisting with the remedial reading's narrower authorization (race-conscious measures only for remedy, not for subordination-dismantling writ large).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equal_protection_kernel__antisubordination_reading, analytical, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
