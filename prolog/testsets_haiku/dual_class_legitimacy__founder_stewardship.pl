% ============================================================================
% CONSTRAINT STORY: dual_class_legitimacy__founder_stewardship
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dual_class_legitimacy__founder_stewardship, []).

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
 *   constraint_id: dual_class_legitimacy__founder_stewardship
 *   human_readable: Dual-Class Share Structure as Founder Stewardship Coordination
 *   domain: corporate_governance/securities_law
 *
 * SUMMARY:
 *   A founder retains concentrated voting control through dual-class shares
 *   while minority shareholders hold economic interest proportional to
 *   capital deployed. The founder's reading frames this structure as
 *   fiduciary stewardship: concentrated control enables long-horizon mission
 *   execution insulated from quarterly pressure and activist campaigns.
 *   Minority shareholders are said to benefit via superior long-term returns
 *   and mission preservation that capital-proportional governance would
 *   erode. This constraint instantiates ONE READING of the contested
 *   dual-class legitimacy kernel; sibling readings (minority_extraction and
 *   disclosure_consent) assert different structural claims about the same
 *   control arrangement. This story models the founder stewardship reading
 *   independently, with its own ε, its own stakeholder structure, and its own
 *   classification.
 *
 * KEY AGENTS:
 *   - founder_group: Agenda-setter (controls voting). Identity-locked to the mission; exit from control is exit from enterprise.
 *   - minority_shareholders_via_mission: Organized beneficiaries (benefit from mission fidelity). Mobile exit (can sell, cannot vote). Benefit is contingent on founder's stewardship performance.
 *   - institutional_investors: Powerful payers (hold significant Class B, press for governance rights). Can exit through secondary markets or proxy contestation.
 *   - public_debt_holders: Moderate payers (bear credit risk from founder agency decisions). Constrained exit (duration of debt term).
 *   - competing_activist_shareholders: Excluded (barred by control structure). Would contest if power were proportional to capital.
 *   - securities_regulators: Analytical observers. Oversee disclosure; defer to state law on control; can enforce fiduciary duty after the fact.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dual_class_legitimacy__founder_stewardship, 0.42).
domain_priors:suppression_score(dual_class_legitimacy__founder_stewardship, 0.31).
domain_priors:theater_ratio(dual_class_legitimacy__founder_stewardship, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, extractiveness, 0.42).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dual_class_legitimacy__founder_stewardship, resistance, 0.67).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dual_class_legitimacy__founder_stewardship, rope).
narrative_ontology:human_readable(dual_class_legitimacy__founder_stewardship, "Dual-Class Share Structure as Founder Stewardship Coordination").
narrative_ontology:topic_domain(dual_class_legitimacy__founder_stewardship, "corporate_governance/securities_law").

domain_priors:requires_active_enforcement(dual_class_legitimacy__founder_stewardship).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dual_class_legitimacy__founder_stewardship, '44aa9eda-93e0-4277-aa58-d70765e5e2de').
narrative_ontology:cs_kernel_codification('44aa9eda-93e0-4277-aa58-d70765e5e2de', formalized).
narrative_ontology:cs_authority_grounding('44aa9eda-93e0-4277-aa58-d70765e5e2de', lineage).
narrative_ontology:cs_interpretation_layer_present('44aa9eda-93e0-4277-aa58-d70765e5e2de').
narrative_ontology:cs_reading_relation('44aa9eda-93e0-4277-aa58-d70765e5e2de', dual_class_legitimacy__minority_extraction, coexists_with).
narrative_ontology:cs_reading_relation('44aa9eda-93e0-4277-aa58-d70765e5e2de', dual_class_legitimacy__disclosure_consent, influences).
narrative_ontology:cs_axiom('44aa9eda-93e0-4277-aa58-d70765e5e2de', foundational, founder_fiduciary_duty_override).
narrative_ontology:cs_axiom_status(founder_fiduciary_duty_override, holdable).
narrative_ontology:cs_axiom_grounding('44aa9eda-93e0-4277-aa58-d70765e5e2de', founder_fiduciary_duty_override, deontological).
narrative_ontology:cs_axiom('44aa9eda-93e0-4277-aa58-d70765e5e2de', foundational, mission_coherence_requires_control_stability).
narrative_ontology:cs_axiom_status(mission_coherence_requires_control_stability, holdable).
narrative_ontology:cs_axiom_grounding('44aa9eda-93e0-4277-aa58-d70765e5e2de', mission_coherence_requires_control_stability, instrumental).
narrative_ontology:cs_reference_frame('44aa9eda-93e0-4277-aa58-d70765e5e2de', founder_mission_protected_through_control).
narrative_ontology:cs_drift_state('44aa9eda-93e0-4277-aa58-d70765e5e2de', contemporary_activist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('44aa9eda-93e0-4277-aa58-d70765e5e2de', '').
narrative_ontology:cs_kernel_id(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, founder_group).
narrative_ontology:constraint_beneficiary(dual_class_legitimacy__founder_stewardship, minority_shareholders_via_mission).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dual_class_legitimacy__founder_stewardship, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(dual_class_legitimacy__founder_stewardship, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dual_class_legitimacy__founder_stewardship_tests).
:- end_tests(dual_class_legitimacy__founder_stewardship_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end, rising from 0.25) because the founder retains exclusive voting control while Class B shareholders bear capital risk without governance voice. This is extraction in the sense that control allocation diverges from capital allocation, yet it is moderated by (1) the founder's identity lock to the mission (alignment through reputation risk), (2) minority shareholders' ability to exit via secondary markets if stewardship fails, and (3) documented cases where founder stewardship delivers superior long-term returns. The measurement series show rising extractiveness as time progresses — as activist pressure mounts and founder must suppress alternative governance proposals — and rising theater ratio as the founder emphasizes mission rhetoric to justify control. Suppression remains lower than extraction because the constraint's persistence depends less on coercion than on founder's actual mission performance and minority shareholders' voluntary continuation of the investment. The shared time grid ensures every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The founder and the minority-shareholder seats should compute differently. From the founder's seat, control is a coordination mechanism protecting mission execution — the constraint serves all shareholders by insulating strategy from short-term pressure. From the minority-shareholder seat, the same structure is a control premium they pay, whose justification depends entirely on founder delivering the promised returns. From the activist-investor seat (excluded), dual-class is a governance barrier extracting value from capital without voice. The engine computes these seat-specific types from the structural data; the authored claim does not adjudicate which reading is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The founder is the primary beneficiary (controls the rules, sets strategy, retains upside from mission success — d near 0.0). Minority shareholders sit near symmetric (benefit from mission execution, bear capital risk, lack voting voice — d near 0.5, with a pull toward 1.0 due to the voting exclusion). Institutional investors are constrained payers (press for governance rights they cannot obtain — d near 0.65). Debt holders are moderate payers (bear agency risk from founder decisions — d near 0.55). Activist shareholders are excluded targets (barred from contesting — d near 0.9). These directionalities derive from the declared beneficiary/victim structure and exit options; no overrides are needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (founder protection against hostile takeover and activist pressure on mission) was real and live at inception. The constraint's persistence depends on whether the founding problem remains live or has been substantially solved. The founder and board claim it remains live; competing shareholders claim it is substantially dead (markets are more patient, indexed investing dominates, founder control persists as private benefit extraction). The measurement series on rising extractiveness coupled with rising theater ratio suggests the constraint's justification is drifting — if founder performance were clearly superior, rising activism would be less visible. This is the classic mandatrophy signature: founding problem status contested, constraint persists, rhetoric amplifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founder_stewardship_performance_vindication,
    'Does founder stewardship empirically deliver superior long-term returns relative to peer companies with capital-proportional governance?',
    'Long-horizon return analysis comparing dual-class founder-controlled firms to single-class and conventional institutional-governed peers, adjusted for risk, sector, and scale. Require 15+ year time horizons to capture long-term mission execution.',
    'If founder-controlled firms significantly outperform, stewardship justification is vindicated and extractiveness may be reclassified as justified coordination cost. If performance is equivalent or worse, stewardship claim is revealed as false summit and constraint reclassifies toward snare (control without demonstrated benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_stewardship_performance_vindication, empirical, 'Empirical vindication of the founder stewardship performance claim.').

omega_variable(
    mission_drift_vs_mission_fidelity,
    'Does concentrated founder control actually protect the declared mission, or does it enable mission drift according to the founder''s personal preferences?',
    'Longitudinal analysis of strategic decisions against founding mission statement; comparison of mission-aligned investment vs. personal-preference-aligned investment; post-succession analysis of whether successor founders maintain or diverge from original mission.',
    'If mission fidelity is demonstrably high, the coordination function is real and extractiveness is justified. If mission drift is substantial (founder interprets mission according to personal preference, not stakeholder preference), control is revealed as private benefit extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_drift_vs_mission_fidelity, empirical, 'Whether founder control actually protects stated mission or enables personal drift.').

omega_variable(
    identity_lock_permanence_and_succession,
    'Is founder identity-lock to the mission permanent, or does it dissolve upon founder succession? If successor is a non-founder manager or second-generation heir, do they retain the stewardship justification?',
    'Analysis of post-succession control structure and performance. Observation of whether successor founders/managers receive the same deference or face shareholder pressure to convert to single-class structure.',
    'If identity-lock is permanent (successors retain control under stewardship claim), the reading is sustainable and stewardship is a durable justification. If identity-lock is revealed as personally keyed to the founder (successors face activist pressure), stewardship is shown as a cover story for founder personal control, not a structural claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_permanence_and_succession, empirical, 'Whether identity-lock to mission persists beyond the founding generation.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Can the founder stewardship reading and the minority extraction reading coexist within the same commitment framework, or does one logically foreclose the other?',
    'Legal and fiduciary analysis of whether founder fiduciary duties admit both interpretations (stewardship of mission AND accountability to minority shareholders for fair treatment), or whether the readings assert incompatible premises about the source and scope of founder obligation.',
    'If readings coexist, the constraint is genuinely contested and both readings remain live. If one forecloses the other, the foreclosed reading is resolved and the commitment framework clarifies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the founder stewardship and minority extraction readings are logically compatible within a single fiduciary framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dual_class_legitimacy__founder_stewardship, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dual_tr_t0, dual_class_legitimacy__founder_stewardship, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dual_tr_t5, dual_class_legitimacy__founder_stewardship, theater_ratio, 5, 0.14).
narrative_ontology:measurement(dual_tr_t10, dual_class_legitimacy__founder_stewardship, theater_ratio, 10, 0.18).
narrative_ontology:measurement(dual_tr_t15, dual_class_legitimacy__founder_stewardship, theater_ratio, 15, 0.24).
narrative_ontology:measurement(dual_tr_t20, dual_class_legitimacy__founder_stewardship, theater_ratio, 20, 0.27).
narrative_ontology:measurement(dual_tr_t25, dual_class_legitimacy__founder_stewardship, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(dual_be_t0, dual_class_legitimacy__founder_stewardship, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(dual_be_t5, dual_class_legitimacy__founder_stewardship, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(dual_be_t10, dual_class_legitimacy__founder_stewardship, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(dual_be_t15, dual_class_legitimacy__founder_stewardship, base_extractiveness, 15, 0.41).
narrative_ontology:measurement(dual_be_t20, dual_class_legitimacy__founder_stewardship, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(dual_be_t25, dual_class_legitimacy__founder_stewardship, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dual_su_t0, dual_class_legitimacy__founder_stewardship, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(dual_su_t5, dual_class_legitimacy__founder_stewardship, suppression_requirement, 5, 0.22).
narrative_ontology:measurement(dual_su_t10, dual_class_legitimacy__founder_stewardship, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(dual_su_t15, dual_class_legitimacy__founder_stewardship, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(dual_su_t20, dual_class_legitimacy__founder_stewardship, suppression_requirement, 20, 0.3).
narrative_ontology:measurement(dual_su_t25, dual_class_legitimacy__founder_stewardship, suppression_requirement, 25, 0.31).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dual_class_legitimacy__founder_stewardship, identity_coordination).
narrative_ontology:boltzmann_floor_override(dual_class_legitimacy__founder_stewardship, 0.12).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__minority_extraction).
narrative_ontology:affects_constraint(dual_class_legitimacy__founder_stewardship, dual_class_legitimacy__disclosure_consent).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested dual-class legitimacy kernel. The kernel persists across three structurally distinct readings: founder_stewardship (this constraint), minority_extraction, and disclosure_consent. Each reading has its own ε, its own beneficiary/victim structure, and its own classification. The three stories are linked by network edges showing causal/legitimacy dependence. The upstream reading (founder_stewardship) is most often cited as justification for the control structure; downstream readings contest the justification. Sibling readings instantiate different ε values because they measure different constraints — what gets extracted (mission control vs. governance voice vs. informational parity) differs across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dual_class_legitimacy__founder_stewardship, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
