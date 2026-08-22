% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Decommodification Floor (Freedom Floor Reading)
 *   domain: political_economy/labor/social_policy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel about
 *   unconditional income support: the freedom_floor_reading, which holds that
 *   decommodifying subsistence from wage labor gives workers a credible exit
 *   from coercive employment terms, converting the labor market from a snare
 *   (where refusal means destitution) into a rope (workers coordinate on a
 *   shared exit option that raises everyone's bargaining floor
 *   simultaneously). Before the floor exists, this reading treats the
 *   standing wage-labor arrangement (ε ≈ 0.55 at t=0, the state under
 *   contest) as substantially coercive — low-wage and hazardous-job workers
 *   accept terms below their genuine reservation price under implicit threat
 *   of destitution. As the floor is introduced and matures, extraction falls
 *   because the coercive leverage employers held is progressively removed.
 *   This is NOT the same constraint as the dependency_trap_reading (which
 *   asserts the floor itself becomes extractive by eroding work incentive and
 *   creating a benefits-trap snare) or the wage_subsidy_reading (which
 *   asserts employers capture the floor as a de facto wage subsidy, letting
 *   them suppress wages further because workers have a state-funded
 *   backstop). Each reading names a different constraint, a different
 *   beneficiary/victim set, and a different ε trajectory — they are linked
 *   here only via cs_structure.reading_relations, per the ε-invariance
 *   principle.
 *
 * KEY AGENTS:
 *   - low_wage_workers: primary beneficiary (powerless/mobile) — gains a credible exit option previously unavailable
 *   - gig_workers: secondary beneficiary (powerless/mobile) — decommodified subsistence enables platform-term refusal
 *   - low_wage_employers: primary payer (organized/constrained) — loses coercive leverage over hiring terms
 *   - labor_intensive_industry_associations: secondary payer (organized/constrained) — collective lobbying interest against the floor
 *   - state_transfer_administrator: agenda_setter (institutional/analytical) — designs and funds the floor
 *   - policy_analysts: observer (analytical) — measures contested labor-supply effects
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
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Support as Decommodification Floor (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/labor/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, '200f05be-317b-4a82-983e-84c537b3b468').
narrative_ontology:cs_kernel_codification('200f05be-317b-4a82-983e-84c537b3b468', distributed).
narrative_ontology:cs_authority_grounding('200f05be-317b-4a82-983e-84c537b3b468', distributed).
narrative_ontology:cs_reading_relation('200f05be-317b-4a82-983e-84c537b3b468', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('200f05be-317b-4a82-983e-84c537b3b468', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('200f05be-317b-4a82-983e-84c537b3b468', foundational, subsistence_threat_constitutes_coercion).
narrative_ontology:cs_axiom_status(subsistence_threat_constitutes_coercion, holdable).
narrative_ontology:cs_axiom_grounding('200f05be-317b-4a82-983e-84c537b3b468', subsistence_threat_constitutes_coercion, deontological).
narrative_ontology:cs_axiom('200f05be-317b-4a82-983e-84c537b3b468', secondary, decommodified_exit_raises_market_wide_bargaining_floor).
narrative_ontology:cs_axiom_status(decommodified_exit_raises_market_wide_bargaining_floor, holdable).
narrative_ontology:cs_axiom_grounding('200f05be-317b-4a82-983e-84c537b3b468', decommodified_exit_raises_market_wide_bargaining_floor, empirically_contingent).
narrative_ontology:cs_reference_frame('200f05be-317b-4a82-983e-84c537b3b468', wage_labor_as_baseline_freedom).
narrative_ontology:cs_drift_state('200f05be-317b-4a82-983e-84c537b3b468', post_pilot_evidence_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('200f05be-317b-4a82-983e-84c537b3b468', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, informal_caregivers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, gig_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, workers_in_hazardous_jobs).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, labor_intensive_industry_associations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently must accept coercive terms (unsafe conditions, wage theft, arbitrary scheduling) because refusal means destitution. An unconditional income floor removes the threat of immediate deprivation behind any single job offer, converting 'take it or starve' into 'take it or don't.' Their exit option shifts from trapped to mobile under this reading.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    powerless, biographical, mobile, national).

% Currently absorb platform-imposed risk (no benefits, algorithmic deactivation, unpredictable pay) because they lack a subsistence backstop. The floor gives them a credible walk-away point for the first time, letting them decline the worst platform terms without existential risk.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, gig_workers, beneficiary,
    powerless, biographical, mobile, national).

% Perform unwaged domestic and care labor that the wage economy has never priced. The floor is the first income they receive independent of market employment, recognizing that labor power was previously commodified only in one direction — into paid work, never out of it.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, informal_caregivers, beneficiary,
    powerless, generational, constrained, national).

% Take on dangerous or degrading work because refusal historically meant no income at all. With a floor, they can decline hazard pay that doesn't actually compensate for risk, forcing employers to either raise wages/safety or lose the labor supply.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, workers_in_hazardous_jobs, beneficiary,
    powerless, biographical, mobile, national).

% Have relied on the threat of destitution to secure labor at below-reservation wages and under substandard conditions without needing to compete on terms. The floor removes that leverage: they must now raise wages, improve conditions, or automate, because workers can simply decline. From this reading, employers are the ones who lose a coercive instrument they did not have to pay for directly — they are structurally a victim of the floor's introduction, not a beneficiary of it.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_employers, payer,
    organized, biographical, constrained, national).

% Lobby against unconditional transfers, arguing labor shortages will result. Under this reading, what they call a 'labor shortage' is better described as the disappearance of a captive labor supply that could previously be hired below its refusal price. Their exit option is constrained because the floor, once established, is difficult to roll back once workers have reorganized their lives around the exit option it provides.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_intensive_industry_associations, payer,
    organized, biographical, constrained, national).

% Designs and funds the unconditional transfer, sets the floor level, and decides eligibility universality. Administers the coordination mechanism that lets workers exit coercive employment without individual bargaining power.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, state_transfer_administrator, agenda_setter,
    institutional, generational, analytical, national).

% Study labor supply elasticity, wage growth, and exit rates from bad jobs following floor introduction. Their findings are contested across the three kernel readings depending on which effects they foreground.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem workers cannot solve individually: no single worker can credibly refuse coercive terms without a subsistence backstop, because unilateral refusal just means replacement by someone more desperate. The floor coordinates a market-wide exit option that raises the reservation price of labor simultaneously for everyone, which no individual bargaining act could achieve.
% TRANSFER_FUNCTION: Moves a fixed income floor from the state's general revenue base (funded broadly, including from taxed capital and higher earners) to every individual regardless of employment status. The structurally significant transfer this reading identifies is indirect: bargaining power moves from employers (who previously extracted labor below its refusal price under threat of destitution) to workers (who can now decline).
% ABSENT_VOICES: Consumers of goods and services produced by formerly-captive low-wage labor are not seated here; if wages rise and hours worked in the worst jobs fall, some price increases or service reductions would follow, and this reading does not center that cost. Employers appear as payers but their trade associations' framing (labor shortage, inflationary wage-price spiral) is treated by this reading as a contested description of the same underlying shift in bargaining leverage.
% DISAPPEARANCE_RATIONALE: If the floor were withdrawn, workers currently declining hazardous or exploitative jobs would lose the ability to do so without risking destitution; wage and condition gains attributable to increased worker bargaining leverage would likely reverse over a business cycle as employers reassert the threat of non-employment as leverage.
% FOUNDING_PROBLEM: Wage labor markets structurally coerce acceptance of employment terms below what a worker would accept absent the threat of immediate material deprivation — the 'freedom to starve' that classical political economy treats as formal freedom but that this reading treats as substantively coercive.
% FOUNDING_PROBLEM_CORROBORATION: Labor economists studying reservation wages and monopsony power in low-wage labor markets (outside any advocacy organization for basic income) document that workers facing binding subsistence constraints accept wages and conditions below competitive-market levels; this corroborates the founding problem's persistence independent of income-support advocates' own framing.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored LOW under this reading's endorsed operation (0.18 at interval end) because once the floor is fully phased in, no party is being coerced through the mechanism itself — the coordination function (a shared exit option) dominates. Per the fixed ε-referent rule, the STARTING value (0.55 at t=0) describes the pre-floor wage-labor arrangement AS THIS READING SEES IT — substantially coercive, not the floor's endorsed alternative. Suppression is low (0.12) because the floor itself uses no coercive enforcement against workers; it is a voluntary universal transfer, not a conditioned mandate. Resistance is moderately high (0.55) because organized employer interests actively contest the floor's introduction, which is exactly what this reading predicts if the floor genuinely removes coercive bargaining leverage rather than merely subsidizing it.
 *
 * PERSPECTIVAL GAP:
 *   From the low_wage_worker seat, the floor is unambiguous coordination: it solves the collective problem that no individual worker could solve alone (refusing coercive terms without dying). From the low_wage_employer seat, the same instrument looks like extraction — a policy that raises labor costs and removes a leverage point they built their business model around, without employers gaining anything the coordination story credits as a benefit. The engine computes these as different seat classifications from the same structural data; this reading does not average them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (low-wage, gig, hazardous-job workers, informal caregivers) get low derived directionality because the floor structurally subsidizes their exit capacity without extracting anything from them in return. Employers and industry associations get high derived directionality because the floor structurally removes a benefit they previously extracted (below-reservation-price labor) without compensating them — from THIS reading's lights, that removal is correctly modeled as a cost borne by a payer, not an unjust confiscation, but the structural fact of who pays is what the directionality derivation is tracking.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coercive acceptance of substandard labor terms under threat of destitution) remains live by this reading's own assessment, corroborated by labor economists outside the advocacy space studying monopsony effects in low-wage markets. This blocks a mandatrophy mislabeling in either direction: the floor cannot be dismissed as solving a dead problem (dependency_trap_reading's implicit claim), nor can its coordination function be reduced to pure employer subsidy (wage_subsidy_reading's claim) without addressing the specific mechanism — worker exit capacity — that this reading identifies as the operative change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_supply_elasticity_ambiguity,
    'Does empirical labor-supply response to unconditional income transfers show workers exiting coercive/hazardous jobs specifically (supporting this reading), or a general reduction in labor-force participation across job quality (which would lend support to the dependency_trap_reading instead)?',
    'Disaggregated labor-supply studies from basic-income pilots (Finland, Ontario, Kenya GiveDirectly, Stockton SEED) tracking which job categories see exit versus overall hours-worked decline, controlling for job quality and coercion indicators (wage theft complaints, safety violations, involuntary overtime).',
    'If exit concentrates in low-quality/coercive jobs while overall employment holds roughly steady, this corroborates the freedom_floor_reading''s mechanism. If exit is broad-based and uncorrelated with job quality, the dependency_trap_reading gains support and this reading''s ε should be revised upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_elasticity_ambiguity, empirical, 'Whether observed labor exit is coercion-selective or general, distinguishing this reading from the dependency-trap sibling.').

omega_variable(
    wage_pass_through_capture,
    'Do wages in previously coercive low-wage sectors actually rise following floor introduction (supporting this reading''s mechanism), or do employers hold wages flat and treat the floor as an implicit subsidy that lets them avoid raising pay (supporting the wage_subsidy_reading)?',
    'Sector-level wage tracking pre/post floor introduction in comparable labor markets, with attention to whether wage growth in low-wage sectors outpaces general wage growth (this reading''s prediction) or lags it (wage_subsidy_reading''s prediction).',
    'Wage growth outpacing the general trend in previously coercive sectors corroborates decommodification; flat or lagging wage growth alongside floor introduction would indicate employer capture and argue this constraint should be re-authored under the wage_subsidy_reading''s structural data instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_pass_through_capture, empirical, 'Whether the floor produces wage growth (decommodification) or wage stagnation (employer subsidy capture) — the empirical fork between this reading and its sibling.').

omega_variable(
    framing_choice_provenance,
    'Why was the freedom_floor_reading selected as the primary framing for this story rather than the dependency_trap_reading or wage_subsidy_reading, given all three are defensible readings of the same policy text?',
    'This is a conceptual/committer-structure question, not resolvable by further data alone: the choice reflects which mechanism (worker exit capacity vs. incentive erosion vs. employer capture) is treated as PRIMARY given contested theoretical priors about labor market coercion. The SCOPE manifest assigned this reading; the alternative framings are authored as sibling constraints, not as parameters of this one.',
    'Choosing a different primary framing would not change this story''s internal ε-invariance, but would produce a different constraint entirely with a different type (snare or tangled_rope rather than rope) and a different victim set (workers rather than employers).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_choice_provenance, conceptual, 'Documents that this story''s framing is one committed reading among three, per the kernel decomposition rule, not an empirically settled description.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t4, income_support_conditionality__freedom_floor_reading, theater_ratio, 4, 0.17).
narrative_ontology:measurement_basis(inco_tr_t4, projected).
narrative_ontology:measurement(inco_tr_t8, income_support_conditionality__freedom_floor_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement_basis(inco_tr_t8, projected).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__freedom_floor_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement_basis(inco_tr_t12, projected).
narrative_ontology:measurement(inco_tr_t16, income_support_conditionality__freedom_floor_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement_basis(inco_tr_t16, projected).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__freedom_floor_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement_basis(inco_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t4, income_support_conditionality__freedom_floor_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(inco_be_t4, projected).
narrative_ontology:measurement(inco_be_t8, income_support_conditionality__freedom_floor_reading, base_extractiveness, 8, 0.33).
narrative_ontology:measurement_basis(inco_be_t8, projected).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__freedom_floor_reading, base_extractiveness, 12, 0.26).
narrative_ontology:measurement_basis(inco_be_t12, projected).
narrative_ontology:measurement(inco_be_t16, income_support_conditionality__freedom_floor_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement_basis(inco_be_t16, projected).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__freedom_floor_reading, base_extractiveness, 20, 0.18).
narrative_ontology:measurement_basis(inco_be_t20, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__freedom_floor_reading, 0.15).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the income_support_conditionality kernel, each with a distinct ε, beneficiary/victim structure, and claimed type: freedom_floor_reading (this story, rope, ε≈0.18, employers as victims), dependency_trap_reading (snare, workers as victims of incentive erosion), wage_subsidy_reading (tangled_rope, workers as victims of disguised employer subsidy capture). All three describe the same policy text; none averages or supersedes the others. Link edges here point to the sibling stories for network contamination/coupling analysis, not because one reading causes another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
