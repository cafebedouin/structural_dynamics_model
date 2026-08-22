% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap (Dependency Reading)
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the DEPENDENCY-TRAP reading of the contested
 *   income-support kernel: unconditional income support (typically Universal
 *   Basic Income or negative-income-tax style transfers) is presented as
 *   creating structural dependency through reduced work incentives, skill
 *   degradation, and long-term earnings penalties. The reading frames
 *   recipients as victims trapped in idleness whose skills atrophy and whose
 *   identity fuses with the dependent role, making exit progressively
 *   costlier. Taxpayers are also victims from this reading's perspective:
 *   they fund transfers that maintain idleness rather than enabling
 *   labor-market participation or skill development. This reading COEXISTS
 *   with two sibling readings: the FREEDOM-FLOOR reading (UBI decommodifies
 *   labor, enabling refusal of coercive work and improved bargaining) and the
 *   WAGE-SUBSIDY reading (UBI functions as implicit employer subsidy,
 *   allowing wage suppression while maintaining worker subsistence). All
 *   three readings interpret the same kernel (unconditional income support)
 *   but instantiate different constraints with different ε values,
 *   beneficiary/victim structures, and types. This file generates ONLY the
 *   dependency-trap reading as a clean snare; the other readings are separate
 *   constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - ubi_recipients: powerless, trapped in idleness and identity fusion with the dependent role; skill atrophy raises re-entry cost progressively
 *   - taxpayers: organized, funding transfers that (in this reading) maintain non-productive dependency rather than building human capital
 *   - policymakers: institutional agenda-setter, designing and administering the program; may benefit from controlling a dependent constituency
 *   - employers: powerful, indirect beneficiary from weakened worker bargaining position and reduced labor-market competition
 *   - labor economists: analytical observer, whose research on reduced work incentives and earnings penalties informs the reading's empirical claims
 *   - alternative-reading advocates: excluded, would dispute the reading's framing and offer freedom-floor or wage-subsidy interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.68).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap (Dependency Reading)").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'fab78950-a81e-4c97-b4b1-58da008f033d').
narrative_ontology:cs_kernel_codification('fab78950-a81e-4c97-b4b1-58da008f033d', formalized).
narrative_ontology:cs_authority_grounding('fab78950-a81e-4c97-b4b1-58da008f033d', extraction).
narrative_ontology:cs_interpretation_layer_present('fab78950-a81e-4c97-b4b1-58da008f033d').
narrative_ontology:cs_reading_relation('fab78950-a81e-4c97-b4b1-58da008f033d', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('fab78950-a81e-4c97-b4b1-58da008f033d', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('fab78950-a81e-4c97-b4b1-58da008f033d', foundational, unconditional_transfers_reduce_work_incentives).
narrative_ontology:cs_axiom_status(unconditional_transfers_reduce_work_incentives, holdable).
narrative_ontology:cs_axiom_grounding('fab78950-a81e-4c97-b4b1-58da008f033d', unconditional_transfers_reduce_work_incentives, empirically_contingent).
narrative_ontology:cs_axiom('fab78950-a81e-4c97-b4b1-58da008f033d', secondary, skill_atrophy_from_nonparticipation_is_irreversible).
narrative_ontology:cs_axiom_status(skill_atrophy_from_nonparticipation_is_irreversible, holdable).
narrative_ontology:cs_axiom_grounding('fab78950-a81e-4c97-b4b1-58da008f033d', skill_atrophy_from_nonparticipation_is_irreversible, empirically_contingent).
narrative_ontology:cs_reference_frame('fab78950-a81e-4c97-b4b1-58da008f033d', poverty_relief_mandate).
narrative_ontology:cs_drift_state('fab78950-a81e-4c97-b4b1-58da008f033d', contemporary_long_term_dependency_visible, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fab78950-a81e-4c97-b4b1-58da008f033d', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support that removes immediate survival pressure but creates structural disincentive to skill development, labor force participation, or career advancement. Over time, skill atrophy sets in, re-entry cost rises, and the recipient's identity fuses with the recipient role itself — they become defined by dependence rather than capability. Exit would require rejecting the support and entering a labor market where their skills have degraded and where they no longer see themselves as capable workers. The identity_locked exit reflects the psychological/identity barrier, not merely economic constraint.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, identity_locked, national).

% Fund unconditional income support through taxation. From the dependency-trap reading, they are victims because the transfers fund non-productive idleness rather than enabling labor-market participation or skill development. They cannot opt out of taxation without leaving the jurisdiction. Their organized power allows collective advocacy but not individual exit.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Design, fund, and administer unconditional income support programs. They set the transfer amount, eligibility criteria, and enforcement rules. Within this reading, policymakers either misunderstand the incentive effects of unconditional support, or benefit from maintaining a dependent constituency that provides political control and predictable voting blocs.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, policymakers, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit indirectly from a workforce whose outside options are constrained by skill atrophy and identity fusion with the recipient role. Lower bargaining power from workers unable or unwilling to re-enter labor markets, fearing their skills have degraded beyond recovery, makes wage suppression and working-condition degradation easier to enforce. They have high mobility and can relocate to other jurisdictions or labor-cost regimes.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, employers, beneficiary,
    powerful, biographical, arbitrage, national).

% Study incentive effects and behavioral responses to unconditional income support. The dependency-trap reading cites labor economics literature on reduced work incentives, skill degradation, and long-term earnings penalties as empirical evidence for the reading's claims. This is an analytical seat, not collecting from or bearing direct cost of the constraint.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% Proponents of freedom-floor and wage-subsidy readings are structurally excluded from this reading's kernel interpretation. They would argue that UBI decommodifies labor, enables labor refusal, or functions as employer subsidy, but the dependency-trap reading's institutional framing does not admit these alternative interpretations as legitimate within its framework. They are excluded not from the policy debate, but from this particular reading's structural picture.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, alternative_reading_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading presents unconditional income support as a failure to solve a coordination problem rather than as a solution to one. The constraint is presented as a transfer mechanism that persists by creating structural dependency, not by coordinating legitimate collective action.
% TRANSFER_FUNCTION: Moves tax revenue from organized labor and employers to individuals selected by unconditional eligibility criteria (typically non-means-tested, non-employment-contingent). From the dependency-trap reading's perspective, the transfer narrows the behavioral space of recipients (work becomes less attractive) and concentrates the constraint's burden on powerless recipients while diffusing funding cost across taxpayers.
% ABSENT_VOICES: Advocates of freedom-floor and wage-subsidy readings (alternative interpretations of the same income-support kernel) are absent from this reading's framing. They would argue UBI serves different functions — enabling labor refusal, subsidizing low-wage employment, improving bargaining position — but this reading does not engage those interpretations as legitimate structural possibilities. Their absence is structural, not accidental; the dependency-trap reading excludes them from its frame.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, recipients would be forced to re-enter labor markets (skill-degraded, but no longer identity-locked to dependency); taxpayers would reallocate resources to other uses; employers would face renewed worker bargaining power; labor-force participation would restructure. The arrangement is not natural law — it is a deliberate policy choice whose removal would trigger economic and behavioral reorganization.
% FOUNDING_PROBLEM: Poverty and economic insecurity in wealthy societies where production sufficiency exists but distribution is unequal. Unconditional income support (UBI, negative income tax) was designed to address this problem without means-testing stigma, work-requirement verification overhead, or paternalistic conditions on how recipients spend transfers.
% FOUNDING_PROBLEM_CORROBORATION: The dependency-trap reading cites longitudinal labor economics research documenting reduced work incentives, skill degradation trajectories, and long-term earnings penalties in unconditional-transfer recipients (Mulligan, Blanchard, and others). However, advocates of freedom-floor and wage-subsidy readings dispute whether these outcomes represent failure or constitute the intended effect (labor decommodification, worker bargaining improvement, employer subsidy continuation). The founding problem's status — whether it is being solved or the mechanism is creating new problems — is itself the contested kernel. No external corroborating source fully validates either reading's claim; the dispute remains unresolved.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The dependency-trap reading measures HIGH extractiveness (0.72 at interval end) because the constraint operates by reducing work incentives and making non-participation structurally attractive relative to labor-market entry. The measured increase from 0.58 to 0.72 over 25 years models the accumulation of skill degradation and identity fusion: early recipients may retain re-entry capability, but long-term exposure makes exit progressively costlier. Suppression is substantial (0.68) because the constraint persists by making recipients psychologically and materially dependent — not by external coercion alone, but through internalized belief that they cannot compete in labor markets and that their identity is constituted by the dependent role. Theater is moderate (0.41): some enforcement machinery (eligibility verification, means testing where it exists) is performative, but much serves the genuine function of maintaining recipient rolls and measuring program reach. Accessibility_collapse is moderate (0.62): alternatives (labor-market entry, skill development) theoretically exist but become progressively inaccessible as skills degrade and identity fuses with dependency. Resistance is substantial (0.58) because recipients often resist the dependency-trap framing (asserting that UBI enables choice, not traps them), and advocates of alternative readings actively contest this reading's claims. The measurement series tracks ACCUMULATING extraction over 25 years, modeling how a voluntary-entry program develops long-term dependency: initial extractiveness is moderate, but compounds as behavioral change (reduced work-seeking) becomes crystallized identity and skill depreciation becomes irreversible.
 *
 * PERSPECTIVAL GAP:
 *   From the recipients' seat (powerless, identity-locked), the constraint may feel liberating initially (survival pressure removed) but progressively restrictive (re-entry cost rising, identity fusion deepening). From policymakers' seat, the constraint may appear benign (poverty relief achieved) or beneficial (reliable constituency maintained). From taxpayers' seat, the constraint appears as resource diversion to non-productive idleness. From employers' seat, it appears as beneficial (reduced worker bargaining power). These divergent perceptions arise from the structural asymmetry of the constraint: recipients bear the long-term extraction (skill loss, identity fusion), taxpayers bear funding extraction, employers gain indirect benefit. The engine computes per-seat classification from this structural data; the dependency-trap reading claims the constraint operates as snare for the victim seats.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients: high d (near 1.0, target). They bear the extraction directly through incentive suppression and long-term harm (skill atrophy, identity fusion). Their exit is identity_locked (psychological/identity barrier, not merely economic). Taxpayers: high d (0.7–0.9, substantial target). They fund the constraint and receive no identifiable benefit within this reading (contrasting with freedom-floor reading where they benefit from macroeconomic effects). Their exit is constrained (cannot opt out of taxation without leaving the jurisdiction). Policymakers: low d (0.2–0.3, beneficiary). They design and control the program, potentially benefiting from maintaining a dependent constituency. Their exit is arbitrage (could switch policy frameworks without personal cost). Employers: very low d (0.1–0.2, beneficiary). They gain indirect benefit from weakened worker bargaining and reduced labor-market competition. Their exit is arbitrage. The directionality-derived effective extraction χ is high for powerless victims (recipients, d≈1.0) and moderate for organized victims (taxpayers, d≈0.75), amplified by the constraint's national scope and the victims' limited arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   The dependency-trap reading faces mandatrophy risk: the founding problem (poverty and insecurity) may be solved (poverty rates fall, material security improves) while the constraint (reduction of work incentives, skill degradation, long-term dependency) persists or worsens. This reading argues that the constraint's persistence is DETRIMENTAL regardless of poverty reduction because it trades immediate material security for long-term human-capital loss and identity fusion. The mandatrophy mismatch emerges when the founding problem's solution (poverty relief achieved) diverges from the constraint's function (now maintaining dependency rather than enabling transition). This reading would declare mandatrophy_resolved=true if evidence shows that recipients systematically fail to re-enter labor markets even when support is reduced or benefits expire — if the founding problem is solved but the constraint persists as pure dependency maintenance, mandatrophy is the diagnosis. The freedom-floor and wage-subsidy readings would dispute this diagnosis, claiming that the founding problem remains live or that the constraint serves different functions than the dependency trap claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causation_vs_selection,
    'Do unconditional transfers CAUSE reduced work incentives and skill atrophy, or do they REVEAL pre-existing employment barriers and capability constraints?',
    'Experimental variation in transfer amount and conditionality across similar populations; longitudinal tracking of skill development, labor-market attachment, and earnings trajectories; comparison with conditioned and means-tested programs.',
    'If causal: the dependency-trap reading''s snare classification stands; the constraint operates by removing work incentives. If selection effect: the constraint''s extractiveness lies elsewhere (in labor-market barriers themselves); the reading misdiagnoses the mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(causation_vs_selection, empirical, 'Whether dependency is caused by the constraint or revealed by it.').

omega_variable(
    identity_fusion_mechanism,
    'Is the identity fusion (recipient defined by dependence) intrinsic to unconditional support, or does it arise from surrounding stigma, institutional design, and social perception?',
    'Post-exit suppression trajectory: if recipients spontaneously re-enter work and rebuild identity when support ends, fusion was partly internalized. Cross-national comparison of identity outcomes in high-status vs. stigmatized UBI programs; ethnographic study of recipient self-perception.',
    'If internalized: the constraint''s effective suppression is higher than the structural measure; recipient carries the constraint after exit. If externally maintained: the extractiveness is in the institutional framing, not the transfer itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_mechanism, empirical, 'Whether identity fusion is structural or internalized.').

omega_variable(
    kernel_framing_contest,
    'Is unconditional income support fundamentally a dependency trap (this reading), freedom-enabling floor (sibling), or employer-subsidizing wage suppression (third sibling)? Which structural facts would prove one reading correct?',
    'Examine labor-market outcomes: (A) work incentives fall, recipients spiral into idleness → dependency-trap. (B) workers refuse bad jobs, improve bargaining position → freedom-floor. (C) employers suppress wages while UBI maintains subsistence → wage-subsidy. Requires long-term, large-scale field data comparing UBI rollout outcomes.',
    'The kernel contest determines which reading''s ε is correct (0.72 vs. 0.15–0.35 vs. 0.65) and which classification (snare vs. rope vs. snare/tangled_rope). The constraint family remains unresolved until empirical labor-market patterns adjudicate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_contest, conceptual, 'Which reading of the income-support kernel is structurally accurate.').

omega_variable(
    taxpayer_extraction_mechanism,
    'Are taxpayers genuine victims (funding non-productive idleness) or beneficiaries of macro effects (demand stimulus, employment opportunities) that outweigh marginal tax cost?',
    'Macroeconomic accounting of multiplier effects, labor-force participation response, and net revenue impact. If transfers trigger demand stimulus that increases tax base and employment offsetting marginal cost, victim designation shifts.',
    'Shifts taxpayers from victim to potential beneficiary set, refactoring the constraint''s structure and reducing extractiveness from the macro-accounting perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxpayer_extraction_mechanism, empirical, 'Whether taxpayers bear net cost or benefit from macro effects of unconditional transfers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__dependency_trap_reading, theater_ratio, 5, 0.29).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__dependency_trap_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__dependency_trap_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(inco_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__dependency_trap_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__dependency_trap_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.71).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__dependency_trap_reading, base_extractiveness, 25, 0.72).
narrative_ontology:measurement_basis(inco_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__dependency_trap_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__dependency_trap_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__dependency_trap_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(inco_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested income-support kernel. The dependency-trap reading, freedom-floor reading, and wage-subsidy reading are structurally distinct constraints arising from the same underlying arrangement (unconditional income support). They coexist because different parties and research traditions interpret the same mechanism through different frames, yielding different ε values, victim/beneficiary sets, and types. The kernel contest is unresolved: only long-term empirical labor-market outcomes can adjudicate which reading's claims are accurate. All three should be authored as separate constraint stories, linked via network.affects_constraints, with explicit omega variables documenting the reading contest in each.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, powerless, 0.92).
constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, organized, 0.74).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
