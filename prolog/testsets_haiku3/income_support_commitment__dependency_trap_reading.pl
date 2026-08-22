% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Work-Disincentive and Dependency Mechanism
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   Unconditional income support systems (such as Universal Basic Income
 *   proposals or fully unconditional welfare payments) are evaluated in this
 *   reading as mechanisms that create economic disincentives to labor-market
 *   participation. The reading frames the primary effect as work reduction
 *   among recipients, leading to skill atrophy (through disuse), increased
 *   state dependence (recipients structurally reliant on continued transfers
 *   to survive), and extraction from the working taxpayer base to
 *   non-participants. This is one of three contested readings of the
 *   income_support_commitment kernel: the freedom_floor_reading emphasizes
 *   autonomy expansion and exit capacity; the targeting_efficiency_reading
 *   prioritizes concentrating support on demonstrated need. This reading
 *   instantiates a causal claim about behavioral and institutional effects
 *   specific to unconditional income support. The claim/metric gap is
 *   intentional: the constraint is authored as Tangled Rope (coordination
 *   function present, extraction from payers to beneficiaries, enforcement
 *   required) while the measured extractiveness (0.62) and theater ratio
 *   (0.41) reflect the reading's assessment that the transfer mechanism is
 *   increasingly theatricalized as skill-erosion narratives accumulate over
 *   time and the founding coordination problem (income security) becomes
 *   secondary to the persistence mechanism (maintaining dependent
 *   recipients).
 *
 * KEY AGENTS:
 *   - income_support_recipients_exiting_labor (beneficiary, powerless, identity-locked — the non-participant cohort whose skills atrophy)
 *   - working_taxpayers (payer, moderate power — bear the extraction through taxation)
 *   - workers_with_atrophied_skills (victim, powerless, constrained — second-order victims created by the mechanism)
 *   - state_welfare_administrators (agenda_setter, institutional — design and enforce the constraint)
 *   - labor_market_employers (observer, organized — witness labor-supply reduction and skill erosion)
 *   - political_opposition (excluded, organized — would argue for work requirements or targeting if heard)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.62).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.48).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Work-Disincentive and Dependency Mechanism").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '656fb714-9f5c-43f5-8914-1246e11cb533').
narrative_ontology:cs_kernel_codification('656fb714-9f5c-43f5-8914-1246e11cb533', distributed).
narrative_ontology:cs_authority_grounding('656fb714-9f5c-43f5-8914-1246e11cb533', distributed).
narrative_ontology:cs_reading_relation('656fb714-9f5c-43f5-8914-1246e11cb533', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('656fb714-9f5c-43f5-8914-1246e11cb533', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('656fb714-9f5c-43f5-8914-1246e11cb533', foundational, unconditional_support_creates_work_disincentive).
narrative_ontology:cs_axiom_status(unconditional_support_creates_work_disincentive, holdable).
narrative_ontology:cs_axiom_grounding('656fb714-9f5c-43f5-8914-1246e11cb533', unconditional_support_creates_work_disincentive, empirically_contingent).
narrative_ontology:cs_axiom('656fb714-9f5c-43f5-8914-1246e11cb533', foundational, skill_atrophy_through_disuse_irreversible_at_scale).
narrative_ontology:cs_axiom_status(skill_atrophy_through_disuse_irreversible_at_scale, holdable).
narrative_ontology:cs_axiom_grounding('656fb714-9f5c-43f5-8914-1246e11cb533', skill_atrophy_through_disuse_irreversible_at_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('656fb714-9f5c-43f5-8914-1246e11cb533', labor_market_self_sufficiency_norm).
narrative_ontology:cs_drift_state('656fb714-9f5c-43f5-8914-1246e11cb533', contemporary_universal_income_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('656fb714-9f5c-43f5-8914-1246e11cb533', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, workers_with_atrophied_skills).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income sufficient to cover basic needs without labor market participation. The reading frames this cohort as having rationally withdrawn from work in response to the transfer, creating over time a fused identity as 'non-worker' and psychological adaptation to non-participation. Skills atrophy through disuse; re-entry becomes increasingly costly.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, income_support_recipients_exiting_labor, beneficiary,
    powerless, biographical, identity_locked, national).

% Fund unconditional income support through taxation. They cannot exit the tax system without leaving the jurisdiction. The reading frames them as bearing the extraction cost of subsidizing non-participation, with the burden increasing as the recipient cohort stabilizes and becomes intergenerational.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    moderate, biographical, constrained, national).

% Initially received income support and exited the labor market. Over time, they have lost marketable skills through non-use, face employment history gaps, and experience psychological barriers to re-entry. They are now trapped in dependence despite potentially being capable of work, unable to recover lost human capital without substantial support.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, workers_with_atrophied_skills, payer,
    powerless, biographical, constrained, national).

% Design, fund, and administer the unconditional income support system. They set benefit levels, eligibility criteria, and enforcement mechanisms. The reading frames them as having institutional interests aligned with maintaining a dependent recipient population, which justifies continued administrative budgets and power.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, state_welfare_administrators, agenda_setter,
    institutional, generational, arbitrage, national).

% Observe reduced labor supply and declining average skill levels in the available workforce due to non-participation. They experience reduced bargaining power from workers but also face higher per-worker productivity demands. Their analytical position allows them to measure the labor-market externalities of the transfer system.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_market_employers, observer,
    organized, generational, arbitrage, national).

% Advocates for work requirements, skills training, or means-tested alternatives. They argue unconditional support creates dependence and that conditionality would improve outcomes. They are structurally excluded from the design of unconditional systems; if heard, they would push for mechanisms to maintain labor-market attachment.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, political_opposition_to_unconditional_support, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, state_welfare_administrators).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides collective solution to income insecurity: instead of relying on family, charity, or employment contingency, a universal transfer guarantees basic income to all residents. Solves the coordination problem of making income security a public good rather than privatized household responsibility.
% TRANSFER_FUNCTION: Moves tax revenue from employed/higher-income taxpayers to income-support recipients, with the transfer stream sustained by recipients' reduced labor-market participation. The mechanism: as recipients reduce work in response to unconditional income, they reduce their own tax contributions and increase their transfer dependence, creating a persistent extraction stream.
% ABSENT_VOICES: Voices excluded from the dependency-trap reading's design: (1) future employers of recipients, who would benefit from skilled labor and are harmed by skill atrophy; (2) family members and dependents of recipients, whose welfare depends on the recipient's eventual earning capacity; (3) communities affected by tax-base shrinkage as working populations shrink or relocate; (4) recipients themselves at the point they wish to re-enter labor and discover irreversible skill loss and discrimination. The reading does not solicit testimony from parties whose welfare depends on recipients' labor-market success.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared overnight, recipients would immediately face income loss and would either re-enter labor markets, seek alternative support (family, community, private charity, informal work), or experience severe hardship. The working taxpayer burden would lift. Labor supply would increase sharply, tax bases would recover. Communities would need to develop alternative income-security mechanisms. The long-term effects on those with severe skill atrophy would be devastating, but the economic structure would reorganize around reduced transfer dependence.
% FOUNDING_PROBLEM: Income insecurity and inadequate wages: historically, labor markets have been unreliable sources of subsistence; unemployment cycles, disability, and wage insufficiency have driven populations into poverty. Unconditional income support was designed as a way to decouple subsistence from labor-market contingency and ensure basic consumption without stigma, means-testing, or work requirements.
% FOUNDING_PROBLEM_CORROBORATION: Development economists and labor scholars (outside the beneficiary community) confirm that wage inadequacy and income insecurity remain persistent problems in many labor markets. However, economists analyzing long-term welfare effects (Acemoglu & colleagues on institutional incentives, Autor & colleagues on labor-market skill requirements) contest whether unconditional income support addresses the founding problem effectively, citing alternative mechanisms (wage floors, employment guarantees, targeted reskilling) as potentially superior on both poverty-reduction and skill-preservation dimensions. The dependency-trap reading is contested by evidence from pilot programs in some jurisdictions showing modest work-reduction effects, but acknowledged as a live risk by economists analyzing intergenerational outcomes and long-term labor-force participation.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.35 to 0.62 over the 25-year measurement interval, reflecting the reading's claim that as non-participation becomes normalized and intergenerational dependence accumulates, the constraint's extraction component grows relative to its coordination component. At t=0, the constraint functions primarily as income security (coordination); by t=25, the reading frames it as primarily financing non-participation (extraction). Theater ratio rises from 0.20 to 0.41, indicating that an increasing share of the administrative and political activity defending the system is performance (narratives of 'helping vulnerable populations,' rhetoric around 'dignity and autonomy') rather than functional provision of security — the underlying mechanism is increasingly about dependency maintenance. Suppression rises from 0.30 to 0.48 and plateaus, modeling the reading's claim that enforcement intensity (legal mechanisms, administrative surveillance, and re-entry barriers) must increase to maintain non-participation rates as the opportunity cost of non-participation rises with time and alternative exits become more attractive. Accessibility_collapse (0.52) and resistance (0.71) model the reading's claim that alternatives to unconditional support remain partly visible (means-tested programs, work requirements, private charity) but resistance from the recipient-dependent cohort is substantial and active.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (working taxpayers) and the beneficiary seat (recipients) will compute radically different types from the same structural data. From the payer perspective, this reads as Snare: extraction without beneficiary function, enforced through legal and administrative mechanisms, with suppressed alternatives (work requirements, targeting). From the recipient perspective, this reads as Rope: genuine coordination (income security is solved; collective action is replaced by guaranteed transfer) with manageable costs. From the administrator perspective, this is Piton: a past-coordination mechanism (income security WAS the founding function) now maintained through performative activity because the dependency-maintenance mechanism has become institutionally valuable and exit from administration would be costly. The engine computes these divergences from the stakeholder structural data; no seat's perception should determine the claim, and divergence is the measurement the apparatus exists to produce.
 *
 * DIRECTIONALITY LOGIC:
 *   Recipients exiting labor (beneficiary, powerless, identity-locked) sit at high directionality toward beneficiary (d ≈ 0.1): they receive income without labor, reducing their incentive to work and creating institutional dependence. Their identity as 'non-worker' becomes fused with the transfer receipt role, making exit from the system identity-threatening (loss of the transfer is experienced as loss of a core identity). Working taxpayers (payer, moderate power, constrained exit) sit at high directionality toward target (d ≈ 0.9): they fund the system through taxation with constrained exit options. The second victim class (workers_with_atrophied_skills, powerless, constrained) sits at target end (d ≈ 0.95): they bear the extraction cost through lost earning potential, reduced labor-market options, and permanent dependence. State administrators (agenda_setter, institutional, arbitrage) sit near neutral (d ≈ 0.5) on the constraint itself, but derive institutional power from administering it — their directionality is asymmetric within their institutional seat (they benefit from continued constraint, but their individual power transcends the constraint's operation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status (contested) and disappearance_verdict (world_rearranges) create a mismatch that flags mandatrophy potential. If the founding problem (income insecurity, inadequate wages) is contested — some parties claim it is solved by unconditional support, others claim it is only partially addressed or that unintended harms exceed benefits — and the world would substantially rearrange if the constraint disappeared (labor supply would recover, tax bases would stabilize, dependent recipients would face immediate crisis), then the constraint's mandate (solving income insecurity) may have outlived its functional justification. The measurement series show theater_ratio rising and base_extractiveness rising while suppression plateaus, a pattern consistent with Piton (atrophied function, performative maintenance). However, the constraint remains actively enforced (requires_active_enforcement: true) rather than merely theatrical, which is more consistent with Tangled Rope. The classification resolves to Tangled Rope because the coordination function (income security provision) is still real, the extraction from payers is real, and the enforcement is active — mandatrophy is diagnosed as the RISK of future atrophy if the founding problem's contested status and the theater ratio's rise predict a trajectory toward pure maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrophy_vs_selection_bias,
    'Does non-participation cause skill atrophy (the mechanism the reading claims), or do lower-skill individuals self-select into non-participation, with no atrophy occurring?',
    'Longitudinal skill assessment (pre-support vs. post-non-participation); control for initial skill levels; instrumental-variable analysis isolating causal effect of duration in non-participation from pre-existing skill differences.',
    'Pure selection would reframe victims as those with lower pre-existing skills; pure atrophy would validate the causal mechanism. If mixed, the magnitude of atrophy-driven extraction would be lower than the reading claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(atrophy_vs_selection_bias, empirical, 'Is skill loss mechanism-driven or selection-driven?').

omega_variable(
    reading_foreclosure_relation,
    'Does the dependency-trap reading''s core premise (unconditional support creates work-reducing extraction) logically foreclose the freedom-floor reading''s core premise (unconditional support expands autonomy), or can both remain live within a coherent evaluative framework?',
    'Conceptual analysis: Can autonomy-expansion and dependence-creation both be true of the same constraint? If autonomy means ''freedom from coerced labor'' and dependence means ''reliance on state transfer,'' both can be simultaneously true (work is optional, but exit is costly). If autonomy is defined as ''capacity to engage in chosen activity'' and dependence means ''reduced capacity for independent subsistence,'' the readings coexist contingently (autonomy now, reduced future capacity).',
    'Coexistence would mean the readings remain live alternatives and the choice between them is evaluative (trade-offs matter). Foreclosure would mean one reading''s truth entails the other''s falsity, narrowing the policy space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_relation, conceptual, 'Relationship between dependency-trap and freedom-floor readings.').

omega_variable(
    institutional_interest_in_dependence,
    'Do state welfare administrators have institutional incentives to maintain recipient dependence, or is the dependent population merely a consequence of the transfer mechanism rather than an object of administrative interest?',
    'Institutional analysis: Compare benefit-level choices, re-entry support funding, and work-requirement enforcement across jurisdictions with and without unconditional support; measure administrative budget growth relative to recipient population change; examine administrative compensation/career structures (do they expand with dependent population, contract with successful exits?).',
    'If administrators systematically prefer larger dependent populations and oppose re-entry support, the extraction becomes fully agency-driven (agenda-setter profit). If administrators are neutral on dependence magnitude and simply administer the system as designed, the extraction is mechanism-driven rather than agency-driven.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_interest_in_dependence, empirical, 'Do administrators have institutional incentives to maintain dependence?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__dependency_trap_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement_basis(inco_tr_t5, observed).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__dependency_trap_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(inco_tr_t10, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__dependency_trap_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__dependency_trap_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(inco_tr_t20, observed).
narrative_ontology:measurement(inco_tr_t25, income_support_commitment__dependency_trap_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(inco_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__dependency_trap_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement_basis(inco_be_t5, observed).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__dependency_trap_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement_basis(inco_be_t10, observed).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__dependency_trap_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__dependency_trap_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(inco_be_t20, observed).
narrative_ontology:measurement(inco_be_t25, income_support_commitment__dependency_trap_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(inco_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__dependency_trap_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement_basis(inco_su_t5, observed).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__dependency_trap_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(inco_su_t10, observed).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__dependency_trap_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__dependency_trap_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(inco_su_t20, observed).
narrative_ontology:measurement(inco_su_t25, income_support_commitment__dependency_trap_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement_basis(inco_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_commitment__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a three-reading kernel family around unconditional income support. The three readings (dependency_trap_reading, freedom_floor_reading, targeting_efficiency_reading) instantiate different causal and evaluative claims about the same standing arrangement (unconditional income transfers). The dependency-trap reading models extraction from working taxpayers to non-participants through work disincentive mechanisms. The freedom-floor reading models autonomy expansion and exit capacity. The targeting-efficiency reading prioritizes concentrating support on demonstrated need. Each reading has its own constraint_id, ε value, beneficiary/victim structure, and classification; they are linked through this network.affects_constraints field to indicate kernel family membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
