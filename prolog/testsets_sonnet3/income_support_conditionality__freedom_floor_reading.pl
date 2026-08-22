% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Exit-Option Coordination (Freedom Floor Reading)
 *   domain: political_economy/labor
 *
 * SUMMARY:
 *   This story instantiates the freedom_floor_reading of the
 *   income_support_conditionality kernel: unconditional income support is
 *   read as decommodifying labor power by giving workers a genuine,
 *   non-work-conditioned survival floor, which converts the previously
 *   coercive employment relation into one where refusal is a real option.
 *   This reading treats the constraint as substantially
 *   coordination-functioned (rope-leaning): the floor solves the collective
 *   problem of unilateral refusal risk, benefiting workers broadly while
 *   imposing real but non-extractive costs on employers who previously relied
 *   on desperation-driven compliance. This is NOT the same constraint as the
 *   dependency_trap_reading (which reads the same policy instrument as
 *   eroding work incentive and producing skill atrophy) or the
 *   wage_subsidy_reading (which reads it as employer-captured wage
 *   suppression). Each reading has a different ε, different
 *   beneficiary/victim sets, and a different claimed type; they are linked as
 *   siblings via network.affects_constraints and do not average into one
 *   classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.18).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.12).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Exit-Option Coordination (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/labor").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '123fa9cb-e0c2-43c0-9816-4c82f9ea88a3').
narrative_ontology:cs_kernel_codification('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', distributed).
narrative_ontology:cs_authority_grounding('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', distributed).
narrative_ontology:cs_reading_relation('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', income_support_conditionality__wage_subsidy_reading, influences).
narrative_ontology:cs_axiom('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', foundational, labor_power_decommodification_is_positive_freedom).
narrative_ontology:cs_axiom_status(labor_power_decommodification_is_positive_freedom, holdable).
narrative_ontology:cs_axiom_grounding('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', labor_power_decommodification_is_positive_freedom, deontological).
narrative_ontology:cs_axiom('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', secondary, unconditional_floor_removes_coercive_leverage_without_capture).
narrative_ontology:cs_axiom_status(unconditional_floor_removes_coercive_leverage_without_capture, holdable).
narrative_ontology:cs_axiom_grounding('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', unconditional_floor_removes_coercive_leverage_without_capture, empirically_contingent).
narrative_ontology:cs_reference_frame('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', commodified_labor_market_baseline).
narrative_ontology:cs_drift_state('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', post_basic_income_pilot_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('123fa9cb-e0c2-43c0-9816-4c82f9ea88a3', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_gig_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, coercive_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, well_paying_employers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, taxpayers_general).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, taxpayers_general).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Previously compelled to accept any wage or condition offered because refusal meant destitution. With an unconditional income floor, they can decline abusive scheduling, unsafe conditions, or below-subsistence wages without risking survival. Their leverage in wage negotiation rises because the credible threat of walking away is now real.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, mobile, national).

% Work platform jobs with no benefits and algorithmic discipline enforced by income desperation. The floor lets them refuse platform terms that push effective pay below a livable rate, or exit gig work entirely for training, caregiving, or organizing, without facing immediate crisis.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precarious_gig_workers, beneficiary,
    powerless, biographical, mobile, national).

% Perform socially necessary but unwaged labor (childcare, eldercare) that the market does not price. The floor recognizes this labor materially, reducing the coercive pressure to take paid work that conflicts with caregiving obligations, though exit remains constrained by care responsibilities themselves.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, generational, constrained, national).

% Firms whose labor model depended on workers' inability to refuse bad terms — poverty wages, unsafe conditions, arbitrary scheduling, retaliatory firing without consequence. The floor removes the credible threat of destitution as a management tool, forcing these employers to raise wages, improve conditions, or lose workers to exit. Their exit option is constrained: they cannot relocate the coercive leverage itself, only adapt to its absence or lobby to weaken the floor.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, coercive_employers, payer,
    organized, biographical, constrained, national).

% Firms already offering competitive wages and decent conditions face little disruption and may benefit as coercive competitors are forced to raise standards, leveling the labor-cost playing field.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, well_paying_employers, beneficiary,
    organized, biographical, mobile, national).

% Designs and funds the unconditional transfer, setting the floor level and eligibility rules (deliberately non-work-conditioned in this reading). Administers disbursement without means-testing or work requirements, which is the structural feature that converts income support from a conditional wage subsidy into a genuine exit option.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, state_administering_agency, agenda_setter,
    institutional, generational, analytical, national).

% Fund the transfer through general taxation. Many are themselves current or future beneficiaries of the floor (as workers, or through reduced downstream costs of poverty), so the payer/beneficiary line runs through the same population rather than across a clean class divide.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, taxpayers_general, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, taxpayers_general, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem in which no individual worker can unilaterally refuse coercive terms without risking destitution, even when most workers would benefit from a floor that lets everyone refuse simultaneously. The income floor coordinates a credible collective exit threat that no worker could sustain alone.
% TRANSFER_FUNCTION: Moves general tax revenue to individuals unconditionally, which in turn moves negotiating leverage away from employers who previously relied on subsistence threat as a disciplinary tool, toward workers who can now decline terms below the floor's implied reservation wage.
% ABSENT_VOICES: Coercive employers whose labor model depends on desperation-driven compliance are not credited as objecting parties in this reading's own framing, though they are the named payer seat; their objection (raised fully in the sibling wage_subsidy_reading and dependency_trap_reading) is that the floor either subsidizes low wages or erodes work ethic — this reading treats that framing as contested and located in the sibling constraints, not resolved here.
% DISAPPEARANCE_RATIONALE: If the unconditional floor vanished, workers currently able to refuse sub-subsistence terms would lose that leverage within one income cycle; coercive employers would regain the ability to extract compliance through destitution threat; unpaid caregivers would face renewed pressure to accept incompatible paid work. The labor market would re-tighten around the prior coercive equilibrium.
% FOUNDING_PROBLEM: Wage labor markets historically compel participation through the threat of destitution rather than through genuinely voluntary exchange, since most workers have no subsistence alternative to accepting whatever terms are offered.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying reservation-wage effects and randomized basic-income trials (outside both the beneficiary and payer sets) report measurable increases in job refusal rates and bargaining power following unconditional transfers, corroborating that the coercion problem is live and structurally addressed by the floor. Employer associations dispute this framing entirely, attesting instead to the dependency and wage-subsidy readings — that disagreement is the kernel contest itself, not resolved within this single reading.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because, under this reading, the floor's operation transfers negotiating leverage from coercive employers to workers without the state or any agenda-setter capturing rents from the transfer — the state administers, but does not extract. Suppression is low because eligibility is unconditional and non-work-tested; there is no enforcement apparatus disciplining recipients. Theater ratio is low and falling because the mechanism does real distributive work rather than performing it. The extractiveness series declines over the interval as the reading models the floor's coordination function maturing (employers adapt to a genuine outside option rather than being able to erode it), consistent with a rope trajectory rather than a rope-to-snare drift.
 *
 * PERSPECTIVAL GAP:
 *   From the state administering agency's seat, this is pure coordination: solving a collective refusal problem no individual could solve alone. From the coercive employer's seat, the same policy instrument removes a disciplinary tool they structurally depended on — the engine should compute a materially different seat-level type for that stakeholder even within this single reading, because their directionality is genuinely adversarial to the mechanism's function, not merely symmetric.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers, gig workers, and unpaid caregivers are beneficiaries: the floor subsidizes their exit option directly, so directionality sits near the full-beneficiary end. Coercive employers are the named victim group — the constraint removes a coercive tool they previously relied on, which the engine should register as a real cost even though it is not extraction in the rent-collection sense; they are payers of the leverage the constraint takes from them, not co-beneficiaries. Well-paying employers and the general taxpayer population sit closer to symmetric or beneficiary, since they either gain from a leveled playing field or fund a program many of them also benefit from.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading treats the founding problem (coercive dependence on wage labor absent a survival floor) as still live and substantially addressed rather than resolved-and-lingering, so mandatrophy is not declared here. The kernel contest itself — whether the founding problem is being solved (this reading), created new dependency (dependency_trap_reading), or is a captured subsidy (wage_subsidy_reading) — is the appropriate site for that dispute, not internal to any single reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_response_ambiguity,
    'Does an unconditional income floor primarily function as (a) removal of coercive leverage enabling genuine refusal of bad terms, (b) an incentive erosion producing long-term withdrawal from productive labor, or (c) a de facto subsidy captured by employers through wage suppression?',
    'Long-run randomized or natural-experiment basic income trials tracking labor force participation, wage levels at the bottom decile, job refusal/quit rates, and employer wage-setting behavior post-implementation, compared across dependency, freedom, and subsidy predictions.',
    'If (a) dominates empirically, this reading''s rope classification and low extractiveness hold. If (b) or (c) dominate, the sibling readings'' classifications (piton/tangled_rope, tangled_rope/snare respectively) are the better structural account and this reading''s ε would need revision — that revision belongs in the sibling files, not this one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_ambiguity, empirical, 'Which of the three kernel readings the empirical labor-supply response actually supports.').

omega_variable(
    employer_victim_status_ambiguity,
    'Is removing an employer''s coercive leverage over workers properly classified as the employer ''bearing a cost'' (payer/victim role) or is it more accurately a correction of a prior extractive advantage that was never legitimately theirs to hold?',
    'Normative and legal analysis of whether desperation-driven labor compliance constitutes a property-like entitlement of employers or an artifact of market failure being corrected; comparative analysis with other cases where removing a coercive advantage is or is not classified as victimization.',
    'If the coercive leverage was never a legitimate entitlement, coercive_employers should perhaps be reclassified from payer/victim to a non-victim category, which would push this reading further toward pure rope (removing the tangled element entirely). If it is treated as a real cost regardless of legitimacy, the current payer classification stands and some tangled-rope-like residue remains structurally present even in this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(employer_victim_status_ambiguity, conceptual, 'Whether loss of coercive advantage counts as victimhood for classification purposes.').

omega_variable(
    funding_incidence_ambiguity,
    'Who actually bears the tax incidence funding the unconditional floor — progressive taxation on capital and high earners, broad-based consumption taxes, or some mix — and does this shift the taxpayer stakeholder''s directionality?',
    'Fiscal incidence analysis of the specific funding mechanism used to finance the transfer in a given jurisdiction.',
    'A regressive funding mechanism would push taxpayers_general''s directionality toward payer/target, increasing measured extractiveness; a progressive mechanism funded substantially by capital or high earners would leave the current near-symmetric authoring intact or push it further toward beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_incidence_ambiguity, empirical, 'Whether the floor''s funding mechanism is progressive, regressive, or mixed, and how that affects taxpayer directionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__freedom_floor_reading, theater_ratio, 4, 0.13).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__freedom_floor_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__freedom_floor_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__freedom_floor_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__freedom_floor_reading, theater_ratio, 24, 0.1).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__freedom_floor_reading, base_extractiveness, 4, 0.27).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__freedom_floor_reading, base_extractiveness, 8, 0.24).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__freedom_floor_reading, base_extractiveness, 12, 0.21).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__freedom_floor_reading, base_extractiveness, 16, 0.19).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__freedom_floor_reading, base_extractiveness, 24, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.1).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_conditionality kernel, decomposed per the ε-invariance principle because the same policy instrument (unconditional income support) produces structurally distinct ε values depending on which causal story is read as true: freedom_floor_reading (this file, ε≈0.18, rope-leaning, employers as payers), dependency_trap_reading (ε expected higher, piton/tangled_rope-leaning, workers as victims of atrophied incentive), and wage_subsidy_reading (ε expected higher still, tangled_rope/snare-leaning, employers as beneficiaries capturing subsidized wage suppression). All three share the same underlying policy text but diverge in beneficiary/victim structure and claimed type; they are linked bidirectionally via affects_constraints rather than merged into one hedged classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
