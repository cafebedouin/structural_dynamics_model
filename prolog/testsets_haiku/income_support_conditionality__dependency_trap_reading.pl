% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the DEPENDENCY-TRAP READING of the
 *   income-support-conditionality kernel: unconditional income support is
 *   framed as a snare that traps recipients in idleness, produces skill
 *   atrophy, and creates psychological identity-locking to the recipient
 *   role. The constraint persists because (in this reading) it benefits
 *   employers who receive suppressed reservation wages, satisfies
 *   administrators who believe they have solved coverage at the cost of
 *   dependency, and mobilizes political movements that oppose transfer
 *   expansion. The reading does NOT describe a genuine coordination problem
 *   being solved—it describes one-way extraction with negative externalities.
 *   A critical feature of this reading: it places UBI recipients in the
 *   VICTIM set (trapped, identity-locked, skill-degrading) rather than in the
 *   beneficiary set (as the freedom-floor reading would). This choice is the
 *   reading's core structural commitment and the source of its snare
 *   classification. The claim and metrics are authored independently and may
 *   diverge as the engine computes seats; the claim (snare) reflects the
 *   reading's core belief about the constraint's structure, while the metrics
 *   (high extractiveness, moderate suppression, rising theater as
 *   institutional justifications proliferate) reflect observable operation.
 *
 * KEY AGENTS:
 *   - UBI recipients: powerless, identity-locked, trapped in biographical time-horizon by skill atrophy and psychological fusion with recipient identity
 *   - Taxpayers funding transfers: organized, constrained exit (relocation or tax avoidance costly), bearing opportunity cost framed as uncompensated redistribution
 *   - Employers and labor demanders: organized, mobile, indirect beneficiaries of suppressed reservation wages and reduced labor-force tightness
 *   - Welfare administrators: institutional, agenda-setting, maintain the system under the belief (or institutional inertia) that unconditional support creates dependency
 *   - Labor economists and researchers: analytical observers, gatekeepers of empirical credibility for the dependency-trap narrative
 *   - Political opponents of transfer expansion: organized beneficiaries whose constituency and donor base are mobilized by the dependency-trap framing, reinforcing the constraint's persistence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.68).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'd4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400').
narrative_ontology:cs_kernel_codification('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', distributed).
narrative_ontology:cs_authority_grounding('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', extraction).
narrative_ontology:cs_interpretation_layer_present('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400').
narrative_ontology:cs_reading_relation('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', foundational, unconditional_income_produces_behavioral_dependency).
narrative_ontology:cs_axiom_status(unconditional_income_produces_behavioral_dependency, holdable).
narrative_ontology:cs_axiom_grounding('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', unconditional_income_produces_behavioral_dependency, empirically_contingent).
narrative_ontology:cs_axiom('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', foundational, work_motivation_intrinsically_necessary_for_wellbeing).
narrative_ontology:cs_axiom_status(work_motivation_intrinsically_necessary_for_wellbeing, holdable).
narrative_ontology:cs_axiom_grounding('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', work_motivation_intrinsically_necessary_for_wellbeing, deontological).
narrative_ontology:cs_reference_frame('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', active_labor_market_participation).
narrative_ontology:cs_drift_state('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', contemporary_expanded_transfer_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d4ed0f0c-6ca6-4ceb-97f9-bbf003c8f400', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers_funding_transfers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, employers_and_labor_market_demanders).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, political_movements_opposing_transfer_expansion).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, human_behavioral_response_to_incentives).
narrative_ontology:constraint_vindicates(income_support_conditionality__dependency_trap_reading, skill_capital_depreciation_under_idleness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income sufficient to meet basic needs. Initially may experience relief from labor-market coercion. Over time (months to years), face declining engagement with employment-related activity, degradation of work-relevant skills (professional competencies, network maintenance, discipline habits), and internalization of recipient identity. Psychological friction toward re-entry rises as the income floor normalizes and work re-entry becomes psychologically framed as difficult/shameful/unnecessary. Skills become dated; employer skepticism about employment gaps increases. The income support becomes a trap not through external bars but through self-reinforcing psychological and human-capital decay.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, biographical, identity_locked, national).

% Fund unconditional transfers through progressive taxation. Experience the constraint as enforced redistribution of their earned income toward recipients who (in this reading) are not working. Bear the opportunity cost: the same funds could support infrastructure, education, or remain as private income. Their voice in democratic systems can constrain further expansion, but the system itself persists because political majorities support it. Exit requires either reducing taxable income (difficult, risky) or relocating to lower-tax jurisdictions (costly, requires abandoning local ties).
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers_funding_transfers, payer,
    organized, biographical, constrained, national).

% Receive an indirect but substantial benefit: access to workers whose reservation wage is suppressed because basic income reduces the urgency of employment. Workers can wait longer, decline exploitative offers, but once the income floor becomes psychologically normal, the marginal attractiveness of entry-level wages and difficult conditions declines. Employers benefit from a larger pool of available workers at lower wage levels than would clear in a labor market without an income floor. The benefit is contingent on the narrative that recipients are trapped (not choosing) and not exercising voice in wage bargaining.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, employers_and_labor_market_demanders, beneficiary,
    organized, biographical, mobile, national).

% Design, implement, and administer the unconditional income transfer system. Set benefit levels, eligibility, payment mechanisms. Justify the system to taxpayers and to themselves as necessary for coverage and administrative efficiency. Under this reading, they operate with the belief (or under institutional inertia) that unconditional support creates dependency; they could change the design (add work requirements, time limits, skill-investment conditions) but don't, because political consensus hasn't formed or because the current system is institutionally stable. Their persistence of the constraint depends on maintaining the narrative that recipients are trapped rather than free.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Study behavioral responses to income support through experiments, longitudinal data, and comparative analysis. Their testimony determines the empirical credibility of the dependency-trap narrative. If they find strong evidence of skill atrophy and behavioral withdrawal, the snare classification stands. If they find weak evidence and substantial reallocated effort, the reading loses its empirical anchor. They are gatekeepers of what counts as evidence for or against the trap hypothesis.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists_and_poverty_researchers, observer,
    analytical, generational, analytical, global).

% Mobilize opposition to unconditional income support by framing it as dependency-creating. Gain political credibility, donor funding, and constituency engagement from this narrative. Benefit from the constraint's existence because it validates their worldview and provides evidence for their policy positions. Reinforce the constraint's persistence by maintaining the narrative that recipients are degraded by dependency, which justifies limiting transfers. Their benefit is contingent on the constraint producing outcomes they can point to (non-participation, institutional stagnation) and interpret through the trap frame.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, political_movements_opposing_transfer_expansion, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, political_movements_opposing_transfer_expansion, agenda_setter).

% A counterfactual seat representing the voice this reading excludes: recipients who use income support to pursue education, care work, artistic practice, community engagement, or political participation rather than wage labor. Under the freedom-floor reading (a sibling), they would be active speakers for the positive value of that choice and would be placed in the beneficiary set. This reading frames their non-wage-work activity as evidence of entrapment and learned helplessness, not as choice or meaningful engagement. Their silence in this reading's narrative is authored as suppression, not as preference satisfaction.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, recipients_exercising_positive_freedom, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, welfare_administrators).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading does NOT describe a coordination problem being solved. Unconditional income support is framed as extraction (transfer from taxpayers to recipients) with no reciprocal coordination benefit. The justification narratives (coverage, efficiency, equity) are treated as cover stories masking the extraction mechanism. The reading explicitly rejects the claim that the constraint coordinates anything—it describes unidirectional redistribution with negative externalities (skill atrophy, dependency, reduced work effort).
% TRANSFER_FUNCTION: Moves income from taxpayers (via the state) to recipients, framed in this reading as creating a trap: recipients come to rely on the transfer, internalize idleness as their identity, experience skill degradation, and face high psychological and human-capital costs to exit. The transfer is permanent (unconditional), so recipients' exit costs compound over time. The reading describes this as pure redistribution with negative behavioral consequences, not as coordination.
% ABSENT_VOICES: Recipients who view income support as enabling positive freedom—education, care work, artistic practice, political engagement—are structurally silenced by this reading's framing. They would argue that non-wage-work activity is meaningful choice, not entrapment. The freedom-floor reading (a sibling) authors them as active speakers; this reading attributes their voice to learned helplessness and dependency. In Q4 terms: they are excluded because the reading does not ask 'what would recipients say about their own experience' but instead asserts 'their behavior reveals their trap, not their preference.'
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared overnight, labor-force participation would rise sharply (workers re-enter because income disappears), tax burdens on remaining workers would fall, wage pressure might increase as labor supply tightens (fewer skilled workers available, skill gaps larger), and recipients would face immediate hardship. The constraint's removal would structurally reorganize work incentives, tax burdens, and labor-market dynamics—the world would visibly rearrange toward higher employment, lower redistribution, and changed wage structures.
% FOUNDING_PROBLEM: Economic precarity and coercive labor-market participation. Early justifications for unconditional income support (1960s–1980s in wealthy nations) centered on replacing means-tested welfare (which was expensive to administer, stigmatizing, and created poverty traps through implicit tax rates) and providing a floor below which no one would fall. The founding problem was hardship and bureaucratic inefficiency in welfare delivery.
% FOUNDING_PROBLEM_CORROBORATION: Welfare administrators and poverty-alleviation researchers in the 1970s–1980s endorsed unconditional support as superior to means testing—external testimony from economists and policy experts independent of the system's beneficiaries. Contemporary labor economists and fiscal-conservative policymakers attest that the founding problems (bureaucratic waste, stigma) have been substantially mitigated by digitization and streamlined administration in wealthy nations, but new problems (behavioral dependency, skill atrophy, reduced labor-force participation, intergenerational poverty) have emerged or been revealed. The foundational problem (economic precarity) remains live in poorer nations and in specific vulnerable populations. Disagreement persists about whether it is solved in wealthy nations—no external corroboration eliminates the contest.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   Extractiveness is high (0.72 at interval end) and rising over the interval (0.48→0.72, t0→t40) because the reading attributes increasing power asymmetry: recipients' skills degrade over time, making exit costlier; employers' bargaining position strengthens as the labor supply becomes more desperate. Suppression is substantial (0.68) because the constraint's persistence depends on the narrative that recipients are trapped rather than choosing—that narrative must be actively maintained against the freedom-floor reading's counterclaim. Theater ratio is moderate (0.42, rising from 0.25) because the welfare system's institutional justifications (security, coverage, equity) are partially real—the theater ratio reflects the gap between the coordination story (coverage) and the extraction reality (the reading attributes). Accessibility collapse is moderate (0.61) because exit from receipt is difficult but not impossible: recipients can re-enter labor markets, though with skills degraded and psychological friction; taxpayers can relocate. Resistance is moderate-high (0.59) because recipients, freed from labor-force coercion, may actively resist framing themselves as dependent (freedom-floor advocates), and some employers may resist lowered reservation-wage frameworks. The measurement series track rising extractiveness over 40 years as skill atrophy compounds, rising theater as the institutional justifications become more elaborate, and stabilizing suppression as the dependency narrative becomes normalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (welfare administrators) and the victims (recipients, taxpayers) should compute different types from the same structural data. The administrators author the constraint as coordinating coverage (rope or tangled-rope); the victims experience it as extraction enforced through the income floor (snare). The engine derives directionality from beneficiary/victim declarations and exit options: administrators are not beneficiaries (they don't collect); they are agenda-setters whose exit is mobile (they could leave the system). Recipients are victims (powerless, identity-locked, costly exit). Taxpayers are victims (constrained exit, funding costs). From these declarations, the engine computes: low d for administrators (they set agenda but don't extract), near-maximum d for recipients and taxpayers (powerless or constrained, bearing costs). That asymmetry drives seat-level type divergence. The admin seat might compute rope (coordination, low personal cost); the victim seats compute snare (extraction, high personal cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients are placed in the victim set and assigned d approaching 1.0 because they are powerless relative to the constraint, face identity-locking exit costs (psychological fusion with recipient identity, skill degradation), and bear the constraint's costs (time spent in idleness framed as their own choice/lack of motivation rather than structural effect). Their directionality d is amplified by their identity-locked status and by their powerlessness. Taxpayers are placed in the victim set and assigned d in the 0.7–0.85 range because they are organized (which lowers d relative to powerless recipients) but face constrained exit (relocation, tax avoidance are costly and imperfect) and bear material costs (opportunity cost of transferred funds). Employers receive an indirect benefit (suppressed reservation wages) but are not named as beneficiaries because the benefit is contingent on the constraint's narrative success and is not direct extraction to them. Administrators are the agenda-setters, setting the constraint's terms, but do not collect extraction; their role is structural (they could change the system) but their directionality should remain low because they receive no personal extraction. Political opponents of transfer expansion are named as beneficiaries (their constituency and funding base are mobilized by the dependency-trap narrative) but face the constraint indirectly—they benefit from the narrative, not from the transfers themselves. This reading's directionality choice is central: by placing recipients in the victim set (trapped, identity-locked), it endorses the snare classification and rejects the freedom-floor reading's placement of recipients as beneficiaries (freed from coercive labor markets).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (economic precarity, involuntary labor participation) was live when the constraint was established. The reading's mandatrophy claim asserts that the founding problem is now DEAD (in wealthy nations, precarity is substantially lower, labor-market institutions have evolved, digital administration has reduced bureaucratic waste). But the constraint persists because it produces secondary benefits (employers benefit from suppressed wages, administrators benefit from institutional stability, political opponents benefit from narrative credibility). The constraint is not coordinating around the original problem anymore; it is extractive around secondary benefits. However, the reading leaves mandatrophy_status as CONTESTED because poverty and precarity remain live in the global south and in specific vulnerable populations (homeless, disabled, undocumented), so some defenders of unconditional support would claim the founding problem is still live in those contexts. The mandatrophy verdict depends on geographic and demographic scope: in wealthy nations with robust labor markets, mandatrophy is substantial; in global context, the founding problem persists. The reading does not adjudicate this; the omega documents the contestation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_response_empirical_grounding,
    'Do unconditional income transfers measurably reduce work effort beyond what reservation-wage theory predicts? Do recipients'' skills actually atrophy, or do they reallocate effort to unmeasured domains (care, education, community work)?',
    'Randomized controlled trials with long-term follow-up (10+ years post-receipt); longitudinal labor-force participation data; skill assessment of recipients at entry and exit; comparison with control populations facing similar income security through other mechanisms.',
    'If behavioral atrophy is small and reallocated effort is substantial, the dependency-trap reading loses its empirical anchor and reverts toward the freedom-floor reading. If atrophy is large and persistent, the snare classification stands. The reading''s truth conditions are entirely empirical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavioral_response_empirical_grounding, empirical, 'Whether unconditional income support actually produces behavioral dependency and skill atrophy at the magnitudes the reading claims.').

omega_variable(
    kernel_reading_contest_identity_locked_vs_choice,
    'Is the measured non-participation of income-support recipients the result of (a) psychological entrapment and identity fusion (this reading''s core claim) or (b) rational preference for non-wage activities given the income floor (the freedom-floor reading''s core claim)?',
    'Post-exit suppression trajectory (per OQ-26): if recipients who lose income support report that their desire to work was internally generated (recovery of prior motivation), the psychological trap reading holds; if they report that the income floor removed their *need* to work and they would choose the same way again with the floor restored, identity-lock dissolves and preference becomes the better model.',
    'Identity-locking supports the snare classification and the suppression metric. Rational preference supports reclassification toward rope or scaffold (if temporary) and substantially lowers suppression. The reading explicitly depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_identity_locked_vs_choice, empirical, 'Whether non-participation by recipients is identity-locked psychological entrenchment or rational preference expression under a changed material constraint.').

omega_variable(
    sibling_reading_foreclosure_test,
    'Can the freedom-floor reading coexist with this dependency-trap reading within a single person''s framework, or do they logically foreclose each other?',
    'Examine whether an agent could hold both: ''Income support is freedom AND income support creates traps.'' An agent could hold this if they believe (1) the floor itself removes coercion but (2) its unconditional character invites psychological dependency, and (3) the trap can be mitigated by design (time limits, skill-investment requirements) without removing freedom. If this is logically coherent, the readings coexist. If an agent must choose one core premise and reject the other, they foreclose.',
    'The engine computes foreclosure from cs_structure.axioms + grounding_type. If the readings coexist, both siblings remain live in the corpus. If they foreclose, one reading would be marked as logically impossible once the other is accepted in a framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_test, conceptual, 'Whether the dependency-trap and freedom-floor readings logically foreclose each other or can coexist as distinct readings of the same kernel.').

omega_variable(
    taxppayers_as_victims_vs_beneficiaries,
    'Are taxpayers who fund transfers genuinely victims (bearing an uncompensated cost), or are they beneficiaries of a stability-producing redistribution system that reduces crime, political conflict, and social fragmentation?',
    'Empirical comparison of tax rates, service levels, and social stability across regimes with high vs. low unconditional transfers; measurement of crime, protest, and conflict correlates; willingness-to-pay studies asking taxpayers whether they would accept current tax levels to maintain the floor vs. lower taxes with no floor.',
    'If taxpayers are net beneficiaries of the stability the floor produces, they exit the victim set and may move to the beneficiary set; the constraint shifts toward rope or tangled-rope. This reading assumes they are pure victims; that assumption rests on denying the stability benefit or treating it as negligible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taxppayers_as_victims_vs_beneficiaries, empirical, 'Whether taxpayers funding transfers are victims of uncompensated redistribution or beneficiaries of social stability.').

omega_variable(
    kernel_contest_reading_identity,
    'This constraint instantiates ONE READING of the income_support_conditionality kernel. Are the sibling readings (freedom_floor_reading, wage_subsidy_reading) structurally distinct constraints or merely different evaluative framings of the same constraint?',
    'Apply the ε-invariance test: if measuring this constraint via the freedom-floor reading''s lights (income support as coercion removal) produces a substantially different ε from this reading''s measurement (income support as trap-creation), then they are two constraints with different ε values. If ε is the same and only the evaluative frame changes, they are one constraint viewed from different seats.',
    'The corpus currently authors them as distinct constraints (separate JSON files linked via network.affects_constraints). The omega documents whether this decomposition is justified or whether a single constraint story should be authored with multiple reading-indexed ε values. If distinct, each reading''s committer axioms live in its own cs_structure. If merged, the constraint would carry reading-indexed ε and contested axioms in a single story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_reading_identity, conceptual, 'Whether the dependency-trap, freedom-floor, and wage-subsidy readings are distinct constraints (per ε-invariance) or one constraint read from different evaluative positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(inco_tr_t0, projected).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__dependency_trap_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__dependency_trap_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__dependency_trap_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(inco_tr_t25, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__dependency_trap_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(inco_tr_t30, observed).
narrative_ontology:measurement(inco_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(inco_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(inco_be_t0, projected).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__dependency_trap_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__dependency_trap_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__dependency_trap_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(inco_be_t25, observed).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__dependency_trap_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(inco_be_t30, observed).
narrative_ontology:measurement(inco_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.72).
narrative_ontology:measurement_basis(inco_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement_basis(inco_su_t0, projected).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__dependency_trap_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__dependency_trap_reading, suppression_requirement, 15, 0.64).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__dependency_trap_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(inco_su_t25, observed).
narrative_ontology:measurement(inco_su_t30, income_support_conditionality__dependency_trap_reading, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(inco_su_t30, observed).
narrative_ontology:measurement(inco_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement_basis(inco_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__dependency_trap_reading, 0.2).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the income-support-conditionality kernel. All three share the same institutional referent (unconditional income transfers) but instantiate different constraints with different ε values and different victim/beneficiary structures. The dependency-trap reading asserts high extractiveness (ε=0.72, rising), places recipients in the victim set (trapped by skill atrophy and identity-locking), and classifies as snare. The freedom-floor reading asserts low extractiveness, places recipients in the beneficiary/excluded set (freed from labor coercion), and classifies as rope or scaffold. The wage-subsidy reading asserts high extractiveness, places recipients in the victim set (subsidizing employers), and classifies as snare via a different mechanism (worker subsidy, not idleness). Network links reflect the contest: each reading influences the others by challenging their core premises and offering alternative framings of the same arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
