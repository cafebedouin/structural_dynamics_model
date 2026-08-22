% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Work-Disincentive Trap
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   Unconditional income support (UBI or similar arrangements) solves the
 *   founding problem of subsistence insecurity and enables workers to refuse
 *   exploitative employment — a genuine coordination function. However, this
 *   reading emphasizes the constraint's extractive dimension: the
 *   unconditional structure creates a financial incentive for exit from the
 *   labor market and does not organize around skill maintenance or
 *   retraining. Over time, this generates a victim class (remaining workers
 *   bearing rising tax burdens, poor individuals whose human capital erodes
 *   during non-participation) while the beneficiary class (those accepting
 *   the subsidy and exiting work) grows and becomes locked into dependency by
 *   the very income security that made exit rational. The founding problem is
 *   partially solved but a new problem is created: fiscal unsustainability
 *   and skill-based inequality. This reading coexists with the freedom_floor
 *   reading (which celebrates the same program's autonomy effects) and
 *   influences but does not foreclose the targeting_efficiency reading (which
 *   asks whether concentrated support on demonstrated need would achieve the
 *   same coordination function at lower extraction cost).
 *
 * KEY AGENTS:
 *   - ubi_recipients_exiting_labor_market: Gain subsistence security but lose future labor-market value as skills atrophy; trapped in dependency by the same income floor that enabled exit
 *   - remaining_working_taxpayers: Fund the program; face rising per-capita tax burden as labor force participation shrinks; constrained exit via emigration or political opposition
 *   - poor_individuals_skill_atrophy: Dual position — immediate survival benefit, long-term earning potential erosion; identity-locked into non-participation as the default state becomes normalized
 *   - state_fiscal_administrator: Agenda-setter; manages the program under fiscal pressure; faces coalition fragmentation as burden becomes visible
 *   - employers_shrinking_labor_pool: Observe labor scarcity; pass cost to consumers or relocate; affected but not party to administration
 *   - excluded_policy_advocates: Advocates for conditional support or targeting are structurally sidelined by the commitment's universality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.58).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.42).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Work-Disincentive Trap").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, 'b68ce939-7b34-41bf-886d-83f005b5edaf').
narrative_ontology:cs_kernel_codification('b68ce939-7b34-41bf-886d-83f005b5edaf', formalized).
narrative_ontology:cs_authority_grounding('b68ce939-7b34-41bf-886d-83f005b5edaf', lineage).
narrative_ontology:cs_interpretation_layer_present('b68ce939-7b34-41bf-886d-83f005b5edaf').
narrative_ontology:cs_reading_relation('b68ce939-7b34-41bf-886d-83f005b5edaf', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b68ce939-7b34-41bf-886d-83f005b5edaf', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('b68ce939-7b34-41bf-886d-83f005b5edaf', foundational, unconditional_subsistence_necessary_to_escape_wage_desperation).
narrative_ontology:cs_axiom_status(unconditional_subsistence_necessary_to_escape_wage_desperation, holdable).
narrative_ontology:cs_axiom_grounding('b68ce939-7b34-41bf-886d-83f005b5edaf', unconditional_subsistence_necessary_to_escape_wage_desperation, empirically_contingent).
narrative_ontology:cs_axiom('b68ce939-7b34-41bf-886d-83f005b5edaf', foundational, universal_distribution_prevents_stigma_and_maintains_dignity).
narrative_ontology:cs_axiom_status(universal_distribution_prevents_stigma_and_maintains_dignity, holdable).
narrative_ontology:cs_axiom_grounding('b68ce939-7b34-41bf-886d-83f005b5edaf', universal_distribution_prevents_stigma_and_maintains_dignity, deontological).
narrative_ontology:cs_reference_frame('b68ce939-7b34-41bf-886d-83f005b5edaf', unconditional_subsistence_guarantee).
narrative_ontology:cs_drift_state('b68ce939-7b34-41bf-886d-83f005b5edaf', labor_force_participation_crisis, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b68ce939-7b34-41bf-886d-83f005b5edaf', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, ubi_recipients_exiting_labor_market).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, remaining_working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income sufficient to subsist without employment. Once enrolled in the UBI regime, face a marginal income cliff: returning to work means losing the unconditional grant and accepting employment conditions in a labor market where their skills may have degraded. They become structurally dependent on the state transfer for survival; exit into employment requires overcoming both the financial disincentive and the skill/credential gap that accumulated during non-participation.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, ubi_recipients_exiting_labor_market, beneficiary,
    powerless, biographical, identity_locked, national).

% Fund the UBI program through payroll taxation and income tax. As more workers exit into the UBI subsidy, the tax base contracts while transfer costs remain fixed or grow, increasing the per-worker burden. They have constrained exit: emigration is possible but costly, reducing work hours faces its own penalties, and political opposition to the program is sporadic and ineffective.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, remaining_working_taxpayers, payer,
    organized, biographical, constrained, national).

% May initially benefit from income security but face long-term skill degradation if they remain outside the labor market. Extended non-participation erodes occupational credentials, leaves gaps in employment history that future employers interpret negatively, and atrophies domain-specific knowledge. When or if they attempt re-entry to work, they discover their earning potential has fallen and formal employment doors have narrowed. The income support solves immediate scarcity but creates a dependency path that makes future work more costly and less rewarding.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophy, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, poor_individuals_skill_atrophy, beneficiary).

% Administers and enforces the UBI program — determines eligibility, sets payment levels, collects taxation to fund it, monitors compliance with participation rules (if any). As the program operates, observes labor force participation falling, fiscal sustainability under pressure, and the political coalition supporting the program fragmenting under tax burden. Can adjust the program parameters, but faces political constraints on either raising taxes or reducing benefits.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, state_fiscal_administrator, agenda_setter,
    institutional, generational, analytical, national).

% Face a contracting labor pool as workers exit into UBI. Wage pressure rises as available workers become scarcer, but cannot easily pass this cost to consumers. Over time, some firms relocate to jurisdictions with higher labor force participation; others adjust production to lower labor intensity. They are affected by the constraint but not formally party to its administration.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, employers_shrinking_labor_pool, observer,
    organized, biographical, constrained, national).

% Advocates for targeted, means-tested support (the targeting_efficiency_reading) or for stronger labor-participation requirements are structurally sidelined by the UBI commitment. Their evidence and recommendations about skill-atrophy dynamics and fiscal sustainability are framed as hostile to the program's foundational premises and are rarely admitted to policy discourse on equal terms. They would argue for conditioning support on engagement or concentration of transfers on those unable to work, but the commitment's universality excludes these voices from shaping actual policy.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, excluded_policy_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, state_fiscal_administrator).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Removes the survival crisis from individual workers, solving the collective-action problem of subsistence security: each worker no longer faces the Hobbes choice between wage depression and destitution, enabling bargaining for better working conditions, transition support, and retraining without immediate hardship.
% TRANSFER_FUNCTION: Moves tax revenue collected from employed workers and capital holders to individuals who have exited or reduced participation in the labor market. The transfer is structured as unconditional — not tied to demonstrated need, employment effort, or skill maintenance — so it flows to both those genuinely unable to work and those choosing non-participation as a rational response to the payment structure.
% ABSENT_VOICES: Workers in declining industries who might benefit from strong retraining conditions (conditional support) rather than income subsidy are partially excluded because the unconditional framing does not organize support around skill restoration. Poor individuals fearing long-term skill loss cannot easily voice concerns without being read as 'ungrateful' for the support. Fiscal conservatives and efficiency-focused policymakers are structurally excluded by the commitment's universality and inability to be easily reformed without triggering constituency backlash.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, millions would return to labor market participation (some reluctantly, others seeking to avoid destitution), labor supply would expand sharply, wage pressure would ease, tax burdens would fall, and fiscal constraints would ease. The organizational structure around UBI administration would dissolve. The poor would face re-entry barriers (skill atrophy, credential gaps) that would require either independent resources or alternative support systems to overcome.
% FOUNDING_PROBLEM: Poverty and subsistence insecurity trap workers in wage-dependent vulnerability; employers extract rents from the desperation of those with no alternative income; workers lack bargaining power and cannot transition out of exploitative arrangements without facing immediate destitution.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocates attest the founding problem remains live and that unconditional support is necessary to solve it. Labor economists and fiscal analysts from outside the beneficiary coalition attest that the founding problem is only partly solved by the program and that new problems (skill atrophy, labor-force shrinkage, fiscal unsustainability) are created as side effects. This reading endorses the fiscal/atrophy critique as the primary evidence.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the program genuinely solves a coordination problem (subsistence security) but simultaneously creates asymmetric extraction from working taxpayers to non-participating beneficiaries. The extraction is not coercive in the sense of outright compulsion, but it is structurally enforced: workers must remain in employment to sustain the transfer, and recipients face a marginal income cliff that traps them outside employment. Suppression is lower (0.42) because the program operates through rational incentives rather than overt force — beneficiaries rationally choose non-participation given the payment structure, and workers have recourse to political opposition (though ineffective in practice). Theater is moderate-low (0.28): the program's coordination function is real, but growing administrative effort goes to justifying the program's fiscal sustainability rather than to solving the original problem. The measurement trajectory shows extractiveness rising over time as labor-force participation falls and the per-worker burden increases, while theater ratio rises as justificatory effort intensifies. Suppression remains relatively flat because the mechanism is incentive-based, not enforcement-intensive, though some suppression emerges as political opposition is marginalized. Accessibility collapse (0.52) reflects that alternatives exist (targeted programs, wage-floor mechanisms, retraining systems) but are politically foreclosed by the commitment to universality. Resistance (0.71) is moderately high because substantial constituencies (working taxpayers, fiscal conservatives, efficiency advocates) actively resist this configuration, even if their resistance is often ineffective at changing policy.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat (UBI recipients exiting labor market) experiences the arrangement as liberation — escape from wage-dependent vulnerability, freedom to refuse degrading work, dignity as a recognized community member receiving unconditional support. The agenda-setter seat (state administrator) experiences it as coordination of subsistence security on behalf of the polity. The victim seats experience it very differently: remaining workers see rising tax burdens and political powerlessness; poor individuals see initial relief but long-term skill erosion and reduced future earning capacity. From the remaining_working_taxpayers seat, the constraint computes as extractive (they pay without collecting); from the ubi_recipients seat, it computes as beneficent. The engine computes this divergence from the structural data — from who pays, who benefits, and the exit options that constrain each seat's agency. This reading's claim (tangled_rope: genuine coordination of subsistence security plus asymmetric extraction from worker to non-participant) honors that divergence rather than erasing it.
 *
 * DIRECTIONALITY LOGIC:
 *   The ubi_recipients_exiting_labor_market seat has directionality near 0.0 (full beneficiary: collects the transfer, faces no cost, and rationally exits labor market once enrolled). The remaining_working_taxpayers seat has directionality near 1.0 (full target: bears the tax burden, constrained exit, no corresponding benefit in this reading). The poor_individuals_skill_atrophy seat is complex: they have directionality near 0.5 initially (genuine subsistence benefit) but drift toward 1.0 (target) as skill atrophy accumulates and their future earning potential is damaged by present non-participation. The state_fiscal_administrator seat is near 0.5 (coordinates the system, collects some legitimacy from administering it, but faces rising fiscal pressure and political backlash). Directionality is stable for most seats except poor_individuals where the identity-locked exit becomes a trap as time accumulates. No overrides are needed; the structural data (who pays, who benefits, exit constraints) naturally derives the stated d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (subsistence insecurity, wage-dependent vulnerability) is partially solved by the program but the mandate has partly outlived its function in the sense that universality is not the only way to solve the founding problem, and the unconditional structure creates new problems (skill atrophy, fiscal pressure, labor-force shrinkage) that the original mandate did not anticipate. This is not classical mandatrophy (a tool maintaining itself after its reason for being disappears) because the founding problem partially persists — subsistence insecurity is real. Rather, it is a case where the mandate has calcified around a specific solution (universality) that creates side effects (extractive dependency) offsetting its coordination benefits. The program could be reformed to maintain subsistence security while adding retraining obligations, means-testing for those capable of work, or wage-supplementation rather than income replacement — changes that would address the side effects without abandoning the coordination function. The resistance to such reform is political, not structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_atrophy_empirical_magnitude,
    'How large is the empirical effect of extended labor-market non-participation on human capital degradation, credential obsolescence, and future earning capacity?',
    'Longitudinal studies tracking individuals who accept UBI and exit work, measuring skill retention, re-entry wage penalties, and long-term earnings trajectories. Comparison with individuals who exit for other reasons (retirement, disability, family caregiving) to isolate the effect of non-participation duration from selection bias.',
    'If atrophy effects are large, the reading''s claim about victim-class creation is empirically grounded and the extractive component of the constraint is real. If atrophy effects are small (skills persist or are easily reacquired), the reading''s framing is exaggerated and the constraint operates more as pure coordination than as masked extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_atrophy_empirical_magnitude, empirical, 'Magnitude of skill degradation in extended non-participation; critical to victim-class claim.').

omega_variable(
    fiscal_sustainability_contingency,
    'At what labor-force participation rate does the UBI program become fiscally unsustainable, and what is the program''s actual trajectory?',
    'Fiscal modeling integrating labor-supply responses to UBI payment levels, demographic changes, and tax-base dynamics. Real-world observation of jurisdictions that have attempted UBI at varying payment levels.',
    'If the program is on a trajectory toward fiscal unsustainability (declining labor force + fixed or rising transfers + fixed or declining tax base), the extraction becomes visible as the remaining working population perceives the burden, and the constraint''s persistence depends increasingly on suppression of resistance. If the fiscal trajectory is stable, the constraint can operate indefinitely without extraction becoming apparent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_sustainability_contingency, empirical, 'Whether the unconditional-support arrangement is fiscally sustainable over the long term.').

omega_variable(
    reading_boundary_with_freedom_floor,
    'Is the autonomy benefit of unconditional income support logically separable from its skill-atrophy cost, or are they aspects of the same social process?',
    'Philosophical/conceptual: If autonomy requires maintained capacity to earn (not just freedom from immediate coercion), then skill atrophy reduces autonomy even as income support increases it — the readings are not opposing but co-instantiating. If autonomy is freedom from wage dependence alone, irrespective of future earning capacity, the readings describe genuinely different states.',
    'If autonomy and skill-atrophy are logically linked, the freedom_floor_reading and this dependency_trap_reading cannot both be fully true at once — the freedom_floor reading''s liberation is shadowed by the trap_reading''s erosion. If they are separable, both readings can be fully true simultaneously (one captures autonomy gains, one captures human-capital losses) and the program genuinely has both effects. This affects whether the readings foreclose each other or merely coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_with_freedom_floor, conceptual, 'Whether autonomy and skill maintenance are conceptually connected or separable in the UBI context.').

omega_variable(
    suppression_mechanism_market_vs_political,
    'Is the measured suppression (0.42) of alternatives to the UBI regime primarily structural (market power, lack of alternatives) or internalized (beneficiaries accept the regime as necessary, taxpayers accept the burden as morally obligatory)?',
    'Post-termination trajectory: if the program were ended, would resistance persist and rebuild support (indicating internalized acceptance), or would alternatives rapidly emerge and be adopted (indicating structural suppression of alternatives rather than consensus)?',
    'If suppression is internalized, the constraint''s persistence depends on maintenance of a narrative of necessity and dignity around UBI; loss of that narrative would reduce suppression sharply. If suppression is structural (lack of political will to fund alternatives), the constraint persists despite lower narrative agreement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_market_vs_political, empirical, 'Composition of suppression: structural barriers vs. internalized acceptance.').

omega_variable(
    universal_vs_targeted_kernel_closure,
    'Does the kernel (income-support commitment) require universal distribution, or is universality contingent to this particular instantiation?',
    'Policy comparison across jurisdictions: can targeted, means-tested support solve the same founding problem (subsistence security, wage-bargaining power) at lower fiscal cost with similar autonomy outcomes? If yes, universality was not necessary to the kernel; if no, universality is intrinsic to the commitment.',
    'If universality is intrinsic, this reading and the targeting_efficiency_reading foreclose each other — only one can be true. If universality is contingent, the readings coexist and the choice between them is empirical (which configuration better solves the founding problem with lower side effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_vs_targeted_kernel_closure, empirical, 'Whether universality is essential to the income-support commitment or a policy choice that could be changed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__dependency_trap_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__dependency_trap_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(inco_tr_t25, income_support_commitment__dependency_trap_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__dependency_trap_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__dependency_trap_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(inco_be_t25, income_support_commitment__dependency_trap_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__dependency_trap_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__dependency_trap_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(inco_su_t25, income_support_commitment__dependency_trap_reading, suppression_requirement, 25, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% income_support_commitment kernel has three readings: dependency_trap_reading (this file), freedom_floor_reading (autonomy and dignity emphasis), and targeting_efficiency_reading (concentrated support on demonstrated need). Each reading instantiates a structurally distinct constraint with different ε values, victim/beneficiary structures, and types. The readings coexist as live positions in ongoing policy debate. This reading emphasizes extraction from working taxpayers and human-capital erosion; it influences but does not foreclose the targeting_efficiency reading (which asks whether the coordination function could be achieved with lower extractiveness via concentration rather than universality). See commentary.kernel_context for full framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
