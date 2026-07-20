% ============================================================================
% CONSTRAINT STORY: war_powers_allocation__inherent_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_powers_allocation__inherent_executive_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: war_powers_allocation__inherent_executive_reading
 *   human_readable: Inherent Executive War Powers Reading
 *   domain: constitutional/law/separation_of_powers
 *
 * SUMMARY:
 *   This constraint instantiates the inherent executive reading of the
 *   constitutional war powers allocation: the claim that Article II's
 *   Commander-in-Chief Clause grants the President independent constitutional
 *   authority to deploy military force in defense of national interests
 *   without prior congressional authorization. Congressional authorization is
 *   treated as politically prudent courtesy rather than legal prerequisite;
 *   actual enforcement of legislative limits is suppressed by fait accompli
 *   deployment followed by appropriations-as-ratification. The reading
 *   coordinates rapid national security response while extracting
 *   constitutional authority from the legislative branch.
 *
 * KEY AGENTS:
 *   - Executive Branch (agenda_setter/beneficiary, institutional/arbitrage): Asserts, interprets, and deploys inherent authority; captures institutional power and operational flexibility.
 *   - Congress (payer, institutional/constrained): Bears the cost of eroded war powers; constrained by political dynamics of post-hoc funding and troop-support optics.
 *   - Armed Forces (beneficiary, institutional/constrained): Receives unified command benefit but operates without legislative mandate.
 *   - Judiciary (observer, institutional/analytical): Abstains from adjudicating, leaving the constraint's enforcement to political branches.
 *   - Antiwar Constituencies (excluded, organized/constrained): Lack formal channel to block hostilities after executive initiation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, 0.62).
domain_priors:suppression_score(war_powers_allocation__inherent_executive_reading, 0.55).
domain_priors:theater_ratio(war_powers_allocation__inherent_executive_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(war_powers_allocation__inherent_executive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_powers_allocation__inherent_executive_reading, tangled_rope).
narrative_ontology:human_readable(war_powers_allocation__inherent_executive_reading, "Inherent Executive War Powers Reading").
narrative_ontology:topic_domain(war_powers_allocation__inherent_executive_reading, "constitutional/law/separation_of_powers").

domain_priors:requires_active_enforcement(war_powers_allocation__inherent_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_powers_allocation__inherent_executive_reading, '88afdc3d-270c-483d-86e3-93642c33bd27').
narrative_ontology:cs_kernel_codification('88afdc3d-270c-483d-86e3-93642c33bd27', fixed_text).
narrative_ontology:cs_authority_grounding('88afdc3d-270c-483d-86e3-93642c33bd27', lineage).
narrative_ontology:cs_interpretation_layer_present('88afdc3d-270c-483d-86e3-93642c33bd27').
narrative_ontology:cs_reading_relation('88afdc3d-270c-483d-86e3-93642c33bd27', war_powers_allocation__congressional_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('88afdc3d-270c-483d-86e3-93642c33bd27', war_powers_allocation__functional_accommodation_reading, influences).
narrative_ontology:cs_axiom('88afdc3d-270c-483d-86e3-93642c33bd27', foundational, executive_inherent_force_prerogative).
narrative_ontology:cs_axiom_status(executive_inherent_force_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('88afdc3d-270c-483d-86e3-93642c33bd27', executive_inherent_force_prerogative, conventional).
narrative_ontology:cs_axiom('88afdc3d-270c-483d-86e3-93642c33bd27', foundational, appropriations_ratification_doctrine).
narrative_ontology:cs_axiom_status(appropriations_ratification_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('88afdc3d-270c-483d-86e3-93642c33bd27', appropriations_ratification_doctrine, conventional).
narrative_ontology:cs_reference_frame('88afdc3d-270c-483d-86e3-93642c33bd27', executive_unilateral_action_framework).
narrative_ontology:cs_drift_state('88afdc3d-270c-483d-86e3-93642c33bd27', contemporary_war_powers_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('88afdc3d-270c-483d-86e3-93642c33bd27', '').
narrative_ontology:cs_kernel_id(war_powers_allocation__inherent_executive_reading, war_powers_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:constraint_victim(war_powers_allocation__inherent_executive_reading, congress).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(war_powers_allocation__inherent_executive_reading, armed_forces).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, unitary_executive_theory).
narrative_ontology:constraint_vindicates(war_powers_allocation__inherent_executive_reading, presidential_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and deploys the Commander-in-Chief authority to initiate military operations without prior congressional approval, relying on OLC opinions and historical precedent to justify unilateral action. Benefits from expanded operational flexibility and aggrandized institutional power relative to the legislative branch.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, executive_branch, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(war_powers_allocation__inherent_executive_reading, executive_branch, beneficiary).

% Constitutional war powers authority is bypassed or treated as optional courtesy. Bears the institutional cost of eroded checks and balances while still appropriating funds for operations it did not authorize, often under political pressure not to appear to oppose troops already deployed.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, congress, payer,
    institutional, generational, constrained, national).

% Receives orders from a unified command structure without inter-branch delays, benefiting from operational clarity. Bears the physical and strategic burden of being deployed into hostilities without broad political consensus or clear legislative mandate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, armed_forces, beneficiary,
    institutional, biographical, constrained, national).

% Generally declines to adjudicate war powers disputes as non-justiciable political questions, occasionally reviewing specific executive actions but rarely enforcing a prior congressional authorization requirement.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, judiciary, observer,
    institutional, generational, analytical, national).

% Citizens and advocacy groups opposing military intervention lack formal institutional channel to block executive-initiated hostilities once underway, as authorization is treated as a retroactive formality rather than a pre-deployment gate.
narrative_ontology:constraint_stakeholder(war_powers_allocation__inherent_executive_reading, antiwar_constituencies, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_powers_allocation__inherent_executive_reading, executive_branch).
narrative_ontology:fixing_cost_class(war_powers_allocation__inherent_executive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables rapid, unified national response to security threats without deliberative delays that could compromise operational effectiveness, surprise, or diplomatic positioning.
% TRANSFER_FUNCTION: Transfers effective decision-making authority over military force initiation from the legislative branch to the executive branch, moving constitutional war powers from a shared allocation framework to executive predominance.
% ABSENT_VOICES: Antiwar legislators and constituencies who would demand prior authorization are structurally sidelined once hostilities commence; their exclusion is reinforced by the political costs of appearing to oppose troops already deployed and by the executive's fait accompli leverage.
% DISAPPEARANCE_RATIONALE: If the inherent authority claim vanished and presidents truly required prior congressional authorization for all force deployments, the tempo and geography of military operations would shift dramatically; the executive would lose fait accompli leverage and Congress would regain institutional gatekeeping capacity over war initiation.
% FOUNDING_PROBLEM: The Articles of Confederation left Congress unable to direct military operations effectively; the Constitution's Framers sought an energetic, unitary executive capable of repelling sudden attacks and conducting foreign policy with dispatch.
% FOUNDING_PROBLEM_CORROBORATION: Executive branch legal advisors (OLC) and Hamiltonian scholars attest the problem of deliberative incapacity in emergencies persists. Congressional scholars and originalist jurists attest the founding problem was solved by the Declare War Clause and that the current arrangement reflects executive aggrandizement rather than original design; no neutral institutional party corroborates the unilateral executive reading from outside the benefiting branch.
narrative_ontology:disappearance_verdict(war_powers_allocation__inherent_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(war_powers_allocation__inherent_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_powers_allocation__inherent_executive_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(war_powers_allocation__inherent_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(war_powers_allocation__inherent_executive_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_powers_allocation__inherent_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_powers_allocation__inherent_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_powers_allocation__inherent_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects a substantial but incomplete transfer of war-initiation authority; Congress retains appropriations leverage, preventing total extraction. Suppression (0.55) captures the moderate coercive force that prevents Congress from effectively blocking deployments, operating largely through political fait accompli rather than direct legal sanction. Theater ratio (0.25) is relatively low because the national security coordination function is genuine, though legal justification contains performative elements. Accessibility collapse (0.45) indicates that formal alternatives (declarations of war, strict WPR compliance) remain legally legible but are politically inaccessible. Resistance (0.60) reflects persistent but largely unsuccessful congressional pushback. The measurement series shows gradual extraction accumulation over the interval as unilateral precedents accumulate.
 *
 * PERSPECTIVAL GAP:
 *   The executive branch seat experiences the constraint as a necessary constitutional prerogative enabling effective national security management â a coordination mechanism with low directional extraction. The congressional seat experiences the same constraint as institutional displacement, where its constitutional powers are rendered structurally inert by operational precedence and political ratification dynamics. The armed forces seat sits closer to symmetric: genuine operational benefit from clear command, offset by the burden of executing politically contested deployments. The engine should compute these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch is the primary declared beneficiary (agenda_setter with arbitrage-grade exit options), placing its directionality near the full-beneficiary end. Congress is the declared victim (payer with constrained exit), placing its directionality near the full-target end. Antiwar constituencies are excluded entirely, experiencing the constraint as absolute external suppression. The judiciary's analytical exit insulates it from extraction, while the armed forces' constrained exit and beneficiary role place it near the center.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by requiring both beneficiary and victim declarations. The executive branch genuinely coordinates national defense (unitary command, rapid response), satisfying the coordination function requirement for tangled_rope. Simultaneously, Congress bears a clear asymmetric cost in eroded constitutional authority, satisfying the extraction requirement. Without the victim declaration, the constraint might be misclassified as rope (pure coordination); without the beneficiary, it might appear as snare (pure extraction). The active enforcement requirement is met by OLC opinions, legal opinions, and the structural need to maintain congressional acquiescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    appropriations_ratification_genuineness,
    'Do congressional appropriations for ongoing military operations constitute genuine legislative ratification of executive war-making, or are they structurally coerced by the political impossibility of defunding troops already in the field?',
    'Comparative legislative history analysis and natural experiment from instances where Congress has cut or conditioned funding for ongoing operations to observe whether subsequent executive behavior treats appropriations as ratification or constraint.',
    'If coerced, the constraint''s extraction from Congress is higher than surface legality suggests, reinforcing tangled_rope classification; if genuine ratification, the constraint approaches a rope with ex post coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appropriations_ratification_genuineness, conceptual, 'Whether post-hoc appropriations are authentic consent or coerced acquiescence.').

omega_variable(
    reading_boundary_stability,
    'Does the inherent executive reading collapse into the functional accommodation reading under sustained empirical pressure of prolonged operations, or does it maintain a categorical distinction rejecting any temporal limit on unilateral authority?',
    'Trace executive branch legal opinions across multi-year deployments to determine whether the inherent authority claim is consistently maintained or functionally modulated by duration and intensity of conflict.',
    'If the reading routinely accommodates functional limits, its Îµ is lower than a pure inherent claim; if it maintains categorical rejection of temporal limits, the extraction asymmetry is sharper.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_stability, conceptual, 'Stability of the inherent executive reading against operational duration.').

omega_variable(
    judicial_abdication_as_enforcement,
    'Does judicial refusal to adjudicate war powers disputes constitute an active interpretive choice that enforces the executive reading, or merely passive non-interference?',
    'Court behavior analysis: compare dismissal doctrines (standing, political question) in war powers cases against other separation-of-powers contexts to assess whether judicial silence operates as structural ratification.',
    'If judicial abdication is active enforcement, suppression is higher than measured; if passive, the constraint relies on political rather than juridical coercion.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_abdication_as_enforcement, empirical, 'Whether judicial non-justiciability actively enforces executive primacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_powers_allocation__inherent_executive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpa_inherent_exec_tr_t0, war_powers_allocation__inherent_executive_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(wpa_inherent_exec_tr_t10, war_powers_allocation__inherent_executive_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(wpa_inherent_exec_tr_t20, war_powers_allocation__inherent_executive_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(wpa_inherent_exec_tr_t30, war_powers_allocation__inherent_executive_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(wpa_inherent_exec_tr_t40, war_powers_allocation__inherent_executive_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(wpa_inherent_exec_tr_t50, war_powers_allocation__inherent_executive_reading, theater_ratio, 50, 0.27).

% Extraction over time
narrative_ontology:measurement(wpa_inherent_exec_be_t0, war_powers_allocation__inherent_executive_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(wpa_inherent_exec_be_t10, war_powers_allocation__inherent_executive_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(wpa_inherent_exec_be_t20, war_powers_allocation__inherent_executive_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(wpa_inherent_exec_be_t30, war_powers_allocation__inherent_executive_reading, base_extractiveness, 30, 0.56).
narrative_ontology:measurement(wpa_inherent_exec_be_t40, war_powers_allocation__inherent_executive_reading, base_extractiveness, 40, 0.59).
narrative_ontology:measurement(wpa_inherent_exec_be_t50, war_powers_allocation__inherent_executive_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(wpa_inherent_exec_su_t0, war_powers_allocation__inherent_executive_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(wpa_inherent_exec_su_t10, war_powers_allocation__inherent_executive_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(wpa_inherent_exec_su_t20, war_powers_allocation__inherent_executive_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(wpa_inherent_exec_su_t30, war_powers_allocation__inherent_executive_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(wpa_inherent_exec_su_t40, war_powers_allocation__inherent_executive_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(wpa_inherent_exec_su_t50, war_powers_allocation__inherent_executive_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_powers_allocation__inherent_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, congressional_primacy_reading).
narrative_ontology:affects_constraint(war_powers_allocation__inherent_executive_reading, functional_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the war_powers_allocation kernel. The kernel's constitutional text (Articles I and II) supports multiple structurally distinct allocations of authority. This reading instantiates executive predominance; sibling readings instantiate legislative primacy and contextual functionalism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
