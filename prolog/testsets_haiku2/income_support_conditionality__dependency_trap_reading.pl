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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint story instantiates the DEPENDENCY TRAP reading of the
 *   contested income support kernel. The standing arrangement—unconditional
 *   income support (UBI or near-equivalent)—is read here as creating a
 *   welfare trap: recipients enter a state of subsidized idleness that
 *   produces long-term skill atrophy, psychological dependency on the
 *   guarantee, and costly re-entry barriers to work. Taxpayers fund the
 *   arrangement and bear its costs. Employers benefit from a reduced labor
 *   supply that suppresses wage pressure. The state administers the program
 *   and benefits from its political convenience. This reading is ONE of three
 *   structurally distinct readings of the same institutional arrangement; the
 *   other readings (freedom_floor and wage_subsidy) would produce different
 *   constraint types, different victim sets, and different classifications
 *   from different seats. The snare classification in THIS reading reflects
 *   the dependency-trap reading's normative commitment: that unconditional
 *   income produces extractive dynamics (idleness, skill lock-in, wage
 *   suppression) that are sustained through active policy enforcement.
 *
 * KEY AGENTS:
 *   - UBI recipients: powerless, trapped in idleness; the reading classifies them as victims because the constraint induces dependency through skill atrophy and psychological anchoring.
 *   - Taxpayers: moderate power, constrained exit; fund the arrangement through taxation; classified as victims because they bear the cost of non-productive transfers.
 *   - State fiscal authorities: institutional power, analytical exit; agenda-setter administering the program; derive benefit from poverty reduction without addressing employment structure.
 *   - Employer class: powerful, arbitrage exit; derive implicit wage subsidy from labor supply reduction; classified as beneficiary because the constraint lowers their labor costs.
 *   - Labor economists: analytical observer; produce evidence about participation and skill effects; lack authority to alter the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.71).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support as Dependency Trap").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, '7bd943cb-6a27-43c1-a2ba-5971eef89b04').
narrative_ontology:cs_kernel_codification('7bd943cb-6a27-43c1-a2ba-5971eef89b04', distributed).
narrative_ontology:cs_authority_grounding('7bd943cb-6a27-43c1-a2ba-5971eef89b04', distributed).
narrative_ontology:cs_reading_relation('7bd943cb-6a27-43c1-a2ba-5971eef89b04', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('7bd943cb-6a27-43c1-a2ba-5971eef89b04', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('7bd943cb-6a27-43c1-a2ba-5971eef89b04', foundational, unconditional_income_induces_behavioral_dependency).
narrative_ontology:cs_axiom_status(unconditional_income_induces_behavioral_dependency, holdable).
narrative_ontology:cs_axiom_grounding('7bd943cb-6a27-43c1-a2ba-5971eef89b04', unconditional_income_induces_behavioral_dependency, empirically_contingent).
narrative_ontology:cs_axiom('7bd943cb-6a27-43c1-a2ba-5971eef89b04', foundational, skill_atrophy_from_nonparticipation_is_irreversible_harm).
narrative_ontology:cs_axiom_status(skill_atrophy_from_nonparticipation_is_irreversible_harm, holdable).
narrative_ontology:cs_axiom_grounding('7bd943cb-6a27-43c1-a2ba-5971eef89b04', skill_atrophy_from_nonparticipation_is_irreversible_harm, deontological).
narrative_ontology:cs_reference_frame('7bd943cb-6a27-43c1-a2ba-5971eef89b04', labor_market_participation_as_condition_of_autonomy).
narrative_ontology:cs_drift_state('7bd943cb-6a27-43c1-a2ba-5971eef89b04', mature_welfare_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7bd943cb-6a27-43c1-a2ba-5971eef89b04', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, state_fiscal_authorities).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, employer_class).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support but face structural incentives against labor market participation. As the support becomes normative, skill degradation occurs through non-use; re-entry to work requires overcoming both psychological dependency and atrophied human capital. The exit option (work at prevailing wages) is costly due to skill loss and the psychological anchor of guaranteed income; the arrangement perpetuates idleness while framing it as freedom.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_recipients, payer,
    powerless, generational, trapped, national).

% Fund the unconditional transfer through taxation. They bear the cost of both the transfer itself and the economic drag from reduced labor force participation. The arrangement frames their extraction as social obligation; exit options are constrained by citizenship and the difficulty of relocating to lower-tax jurisdictions at scale.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% Administer the unconditional income support program and set its parameters. They benefit from the arrangement's capacity to manage poverty politically without addressing structural employment failures; the program becomes a tool of governance rather than a remedy for material deprivation. The cost to the state is abstract (debt, inflation, foregone investment); the political benefit is concrete (poverty reduction without labor market disruption).
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, state_fiscal_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Benefit from a reduced labor supply when a portion of the population is subsidized to remain outside the labor market, which suppresses wage pressure. The unconditional transfer functions as an implicit wage subsidy: employers pay lower wages because workers can subsist on the guarantee, while the state bears the cost. This arrangement is structurally opaque—the employer benefit is never stated as such.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, employer_class, beneficiary,
    powerful, generational, arbitrage, global).

% Study the empirical relationship between unconditional transfers and labor force participation, skill acquisition, and long-term earnings trajectories. They produce evidence that informs the policy debate but lack enforcement authority; their findings are contested by reading communities with different normative commitments.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_market_economists, observer,
    analytical, biographical, analytical, global).

% Would advocate for conditional support (tied to work, training, or behavioral requirements) or alternative poverty-reduction mechanisms (job guarantees, wage floors, employer mandates). Their exclusion from the design process is enforced by the political coalition supporting unconditional transfers; their voice would reframe the constraint as dependency-inducing rather than liberatory.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, alternative_policy_communities, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, state_fiscal_authorities).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces poverty headcount and provides material security without requiring administrative determination of need or behavioral conditions, solving the problem of means-testing overhead and recipient privacy loss.
% TRANSFER_FUNCTION: Moves income from taxpayers to recipients, mediated by state fiscal authorities. The mechanism operates through general taxation funding a fixed per-capita payment to all eligible citizens, with no work requirement or consumption restrictions.
% ABSENT_VOICES: Workers whose wages are suppressed by the abundance of subsidized labor outside the market; families whose survival depends on unconditional work (they have no income floor and must work at any wage); employers who would compete against suppressed-wage firms but are locked out by the structural labor supply shift. Alternative policy designers (conditional support, wage guarantee) are systematically excluded from the design coalition and their proposals are pre-rejected on ideological grounds.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, labor force participation would rise sharply as material necessity returns; wage pressure would increase; employers would face higher labor costs; skill development in the population would accelerate as non-work ceases to be subsidized. The entire economic and demographic structure would reorganize around full labor-market engagement.
% FOUNDING_PROBLEM: Extreme poverty, lack of material security, inability of markets to provide subsistence for all participants, and the administrative cruelty and privacy violation of means-testing welfare systems.
% FOUNDING_PROBLEM_CORROBORATION: The policy's architects and libertarian-left advocates attest the founding problem remains live and that unconditional transfers are the only non-coercive remedy. Labor economists, employers, and fiscal conservatives attest the founding problem is substantially addressed by existing conditional programs and that unconditional transfers create new problems (dependency, idleness, skill degradation) that outweigh the elimination of means-testing friction. The empirical record shows mixed evidence; the reading community divide is not resolvable by data alone.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.68) and rising over the measurement interval (0.48 → 0.68) because the constraint's function shifts from poverty relief (early period) to dependency maintenance (later period). As recipients adapt to unconditional income, their skill atrophy and psychological dependence increase, raising the extraction cost of re-entry to work. Suppression is high (0.71) because the arrangement's persistence requires active policy maintenance: the guarantee must be large enough to sustain idleness but accompanied by narrative frames (idleness as freedom, work as oppressive) that prevent recipients from recognizing the lock-in. Theater is moderate (0.42) because a real coordination function (poverty reduction) is present, but an increasing share of the constraint's operation is performative maintenance of the idleness narrative. The measurement series track the trajectory from policy implementation (extractiveness low, coordination high) to mature welfare state (extractiveness high, dependency strong). Accessibility collapse is moderate (0.63) because alternatives do exist but are psychologically costly: recipients could return to work, but the skill loss and the identity-shift required make this exit expensive. Resistance is moderate (0.58) because labor economists and employers mount real empirical and political challenge to the reading, but the libertarian-left and state authorities have strong interest in maintaining the program's unconditional framing.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute very different types from the same structural data. From the state's and employer's seats, the arrangement is genuine coordination: poverty is reliably reduced, labor market frictions are minimized, and income security is achieved without stigmatizing means-testing. From the recipient's seat (in this reading), the arrangement is a snare: they are locked into idleness by the same forces that reduce their cost of living, and exit requires overcoming both skill loss and the psychological anchor of the guarantee. From the taxpayer's seat, the arrangement is extraction: they fund non-productive transfers and receive no direct benefit. The engine computes these divergences from the stakeholder directionality: beneficiaries have low d (low effective extraction), payers have high d (high effective extraction), and the per-seat types reflect this asymmetry. The claim (snare) is authored independently of the metrics; the snare classification emerges from the structural data, not from tuning the metrics toward a predetermined verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI recipients have directionality d ≈ 0.9 (near full target) because the constraint extracts from them through skill lock-in and psychological dependency, even though it appears to benefit them materially. The benefit (income security) is real, but the cost (long-term employability and autonomy) is higher, making their net position extractive. Taxpayers have d ≈ 0.8 (high target) because they bear the funding cost and receive no direct coordination benefit; their exit is constrained by citizenship and fiscal politics. State authorities have d ≈ 0.2 (near full beneficiary) because they derive political benefit (poverty reduction without employment disruption) from administering the constraint, and their exit cost is zero (they control it). Employer class has d ≈ 0.15 (near full beneficiary) because they derive wage suppression benefit from the reduced labor supply. The asymmetry in directionality—high d for recipients and taxpayers, low d for authorities and employers—is what makes the constraint a snare rather than a rope: the coordination function (poverty relief) is real, but it is captured by beneficiaries and paid for by victims, and the extraction persists through active enforcement (policy maintenance, narrative framing).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (extreme poverty, failure of markets to provide subsistence) was genuinely unsolved at the program's inception. The founding problem is now contested: the dependency-trap reading argues that the original problem is solved (poverty headcount is down) but the solution has created new problems (skill atrophy, labor supply reduction, wage suppression) that now constitute the mandate. The freedom-floor reading argues the founding problem remains live and that the constraint's benefit is permanent freedom from coercive work. The mandatrophy tension is unresolved: the constraint persists not because the founding problem persists, but because different reading communities have different verdicts on whether the problem is solved and what counts as harm. In the dependency-trap reading, mandatrophy is PARTIAL: the original mandate (poverty reduction) is substantially achieved, but new mandates (dependency prevention, skill maintenance) have emerged, and the constraint's current operation serves these new mandates rather than the original one. This is not a pure piton (which would have zero function), but a tangled maintenance of multiple conflicting mandates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_dependency_vs_freedom,
    'Is the same institutional arrangement—unconditional income support—a trap that induces dependency and skill atrophy, or a floor that decommodifies labor and creates genuine freedom to refuse coercive work?',
    'This omega routes to the committer frame: the two readings (dependency_trap_reading vs. freedom_floor_reading) are mutually exclusive normative postures on the same standing arrangement. No empirical data resolves which reading is structurally correct; the disagreement is in the reference frame (is idleness a trap or a liberation?) and the measurement of harm (is skill atrophy or wage suppression the correct damage metric?).',
    'Adoption of the freedom_floor_reading would reclassify the constraint from snare to rope or scaffold; UBI recipients would shift from victim to beneficiary; the constraint''s type would depend entirely on which reading''s normative commitments are endorsed. This omega documents that the snare classification in this reading is reading-indexed, not a discovery about the standing arrangement itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_dependency_vs_freedom, conceptual, 'Committer-frame ambiguity: whether unconditional income is a trap or a liberation depends on the reader''s normative framework, not on the arrangement itself.').

omega_variable(
    empirical_dependency_causation,
    'What causal pathway(s) produce skill atrophy and labor force withdrawal? Is it the income guarantee itself, or the interaction of the guarantee with other institutional features (stigma, poverty concentration, absence of training access)?',
    'Randomized controlled trials comparing unconditional transfers, conditional transfers with training access, and cash transfers with active job placement support, measured over 10+ year follow-up. Natural experiments in jurisdictions implementing selective eligibility (age bands, geographic region) to isolate the income effect from other features.',
    'If skill atrophy is intrinsic to unconditional transfers, the snare classification holds. If atrophy is context-dependent (present only when combined with absent services or stigma), the constraint might be a tangled_rope (coordination of poverty reduction coupled with extraction via skill lock-in) rather than a snare. If atrophy is negligible empirically, the entire reading collapses toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_dependency_causation, empirical, 'Whether skill degradation is caused by the income guarantee or by institutional absence (training, job matching, social integration).').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression in this constraint structural (economic necessity forces people to work despite welfare; the guarantee is never high enough to truly exit) or internalized (recipients internalize the narrative of idleness as shameful or identity-threatening)?',
    'Post-program trajectory studies: if recipients who exit the program maintain suppression-like behaviors (reluctance to work, low wage-seeking, internalized inferiority), suppression is partly internalized. Comparative studies across societies with different cultural narratives around welfare (shame-based vs. dignity-based) to isolate the internalization component.',
    'If suppression is structural, raising the guarantee and reducing work requirements would lower it; if it is internalized, the same policy changes would not reduce suppression, because recipients carry the internalized barrier with them. This affects exit-cost calculations and directionality derivation: identity-locked exit (internalized) versus trapped exit (structural) are different mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural economic necessity or internalized psychological barrier.').

omega_variable(
    wage_suppression_vs_poverty_relief_tradeoff,
    'To what degree does the employer benefit from the constraint (wage suppression from reduced labor supply) versus the poverty-relief function of the constraint (material security for recipients)? Are these separable, or is the wage suppression the mechanism by which poverty is ''relieved'' (at the cost of worker dignity)?',
    'Decompose the employer benefit: measure actual wage suppression attributable to labor supply reduction caused by unconditional transfers, using synthetic-control and instrumental-variable methods. Compare to the benefit to recipients (income security, health outcomes, child development). The tradeoff ratio reveals whether the arrangement is primarily extraction (employers capture more than recipients gain) or coordination with asymmetry.',
    'If employers capture substantially more than recipients gain, the constraint is a snare (pure extraction). If the gains are more balanced, it might be a tangled_rope (both coordination and extraction present). This omega documents that the snare classification depends on an empirical judgment about which seat''s gain exceeds the other''s cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(wage_suppression_vs_poverty_relief_tradeoff, empirical, 'Whether the constraint''s primary function is poverty relief or employer subsidy via wage suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(inco_tr_t0, projected).
narrative_ontology:measurement(inco_tr_t5, income_support_conditionality__dependency_trap_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(inco_tr_t5, projected).
narrative_ontology:measurement(inco_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__dependency_trap_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_conditionality__dependency_trap_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(inco_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(inco_be_t0, projected).
narrative_ontology:measurement(inco_be_t5, income_support_conditionality__dependency_trap_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(inco_be_t5, projected).
narrative_ontology:measurement(inco_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__dependency_trap_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_conditionality__dependency_trap_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(inco_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(inco_su_t0, projected).
narrative_ontology:measurement(inco_su_t5, income_support_conditionality__dependency_trap_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(inco_su_t5, projected).
narrative_ontology:measurement(inco_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__dependency_trap_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_conditionality__dependency_trap_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(inco_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% The income_support_conditionality kernel decomposes into three structurally distinct constraints, each a different reading of the same standing arrangement. The dependency_trap reading (this story) treats unconditional income as inducing skill atrophy and labor market withdrawal, classifying it as a snare. The freedom_floor reading treats it as decommodifying labor and enabling refusal of coercive work, classifying it as rope or scaffold. The wage_subsidy reading treats it as an implicit employer subsidy, classifying it as tangled_rope. All three readings share a referent (the unconditional income arrangement) but differ in ε (extractiveness from the reading's perspective), beneficiary/victim structure, and type. No empirical measurement resolves which reading is correct; the disagreement is normative (what counts as harm?) and epistemological (what is the true purpose of the arrangement?). Each reading carries its own constraints story with independent classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, powerless, 0.88).
constraint_indexing:directionality_override(income_support_conditionality__dependency_trap_reading, moderate, 0.79).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
