% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Platform Flexible Employment as Structural Precarity (Precarity-Extraction Reading)
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   Platform-mediated flexible employment — ride-hail, delivery, micro-task,
 *   and gig contracts classifying workers as independent contractors — read
 *   here through the precarity-extraction reading of the
 *   flexible_employment_legitimacy kernel. On this reading the arrangement
 *   does solve a real matching problem, and that coordination surface is
 *   precisely what makes the cost-shift durable: algorithmic dispatch sets
 *   piece rates, ratings and deactivation supply discipline without employer
 *   obligations, and classification shifts payroll taxes, injury risk, and
 *   old-age contributions onto workers and public insurance. The claim is
 *   tangled rope: genuine coordination plus asymmetric extraction, actively
 *   enforced. Epsilon's referent is the standing flexible-employment
 *   arrangement as this reading sees it — never the formalized or
 *   fully-priced alternative the reading would prefer. The sibling readings
 *   (market_efficiency, developmental_state) are separate constraints with
 *   their own epsilon and are linked only at the network level.
 *
 * KEY AGENTS:
 *   - platform_operators: agenda-setter and primary beneficiary (institutional/arbitrage) — sets piece rates, dispatch, and deactivation rules; collects the take rate
 *   - platform_capital_investors: beneficiary (powerful/arbitrage) — holds valuations premised on labor-cost externalization
 *   - gig_platform_workers: primary target (powerless/constrained) — bears piece-rate volatility, unpaid time, injury and old-age risk
 *   - traditional_sector_employees: secondary target (organized/constrained) — bears erosion of wage floors and standards via platform competition
 *   - on_demand_service_consumers: beneficiary with partial payer position (moderate/mobile) — receives cheap on-demand service, bears socialized cost through taxation
 *   - social_insurance_systems: payer (institutional/trapped) — absorbs the contribution gap as unfunded liabilities
 *   - labor_regulators_and_courts: analytical observer (institutional/analytical) — adjudicates the classification boundary the arrangement stands on
 *   - algorithmically_deactivated_workers: excluded voice — removed by automated enforcement, no seat or appeal
 *   - future_retirees_of_gig_workforce: excluded voice — bears the old-age gap, absent from current bargaining
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.75).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.7).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, tangled_rope).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Platform Flexible Employment as Structural Precarity (Precarity-Extraction Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '68d002b9-4b01-417b-978c-73e1c26a56a4').
narrative_ontology:cs_kernel_codification('68d002b9-4b01-417b-978c-73e1c26a56a4', distributed).
narrative_ontology:cs_authority_grounding('68d002b9-4b01-417b-978c-73e1c26a56a4', distributed).
narrative_ontology:cs_reading_relation('68d002b9-4b01-417b-978c-73e1c26a56a4', flexible_employment_legitimacy__market_efficiency_reading, influences).
narrative_ontology:cs_reading_relation('68d002b9-4b01-417b-978c-73e1c26a56a4', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('68d002b9-4b01-417b-978c-73e1c26a56a4', foundational, risk_externalization_constitutes_extraction).
narrative_ontology:cs_axiom_status(risk_externalization_constitutes_extraction, holdable).
narrative_ontology:cs_axiom_grounding('68d002b9-4b01-417b-978c-73e1c26a56a4', risk_externalization_constitutes_extraction, empirically_contingent).
narrative_ontology:cs_axiom('68d002b9-4b01-417b-978c-73e1c26a56a4', foundational, algorithmic_control_without_obligation_is_discipline).
narrative_ontology:cs_axiom_status(algorithmic_control_without_obligation_is_discipline, holdable).
narrative_ontology:cs_axiom_grounding('68d002b9-4b01-417b-978c-73e1c26a56a4', algorithmic_control_without_obligation_is_discipline, empirically_contingent).
narrative_ontology:cs_reference_frame('68d002b9-4b01-417b-978c-73e1c26a56a4', standard_employment_relationship_baseline).
narrative_ontology:cs_drift_state('68d002b9-4b01-417b-978c-73e1c26a56a4', platform_scale_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('68d002b9-4b01-417b-978c-73e1c26a56a4', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_capital_investors).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_platform_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, traditional_sector_employees).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_insurance_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, monopsony_labor_market_power_thesis).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__precarity_extraction_reading, surplus_value_extraction_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and operate the apps that set piece rates through dynamic pricing, assign work through dispatch algorithms, and enforce performance through ratings and deactivation. They classify workers as independent contractors, collect a take rate on every transaction, and defend that classification with litigation and ballot campaigns. Their exit is arbitrage: the operating model is portable across jurisdictions, so any single market that reclassifies workers can be abandoned or restructured around.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, beneficiary).

% Hold equity in platforms whose valuations rest on unit economics that treat labor as a variable cost without fixed obligations. Returns arrive through appreciation and distributions; exposure to any one labor market's regulatory outcome is a portfolio position they can exit by selling.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_capital_investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Log into apps to accept individually dispatched tasks at algorithmically set prices. They supply the vehicle, phone, insurance, and the unpaid time spent waiting and repositioning, and they carry income volatility, injury risk, and retirement savings gaps with no employer contributions. They can switch apps or stop logging in, but the alternatives available to them are mostly the same or worse arrangements, and for many the alternative to logging in is no income at all.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_platform_workers, payer,
    powerless, immediate, constrained, global).

% Work under employment contracts in the same sectors — transport, delivery, care, hospitality — where platform competitors operate at lower labor cost. Wage floors, benefit standards, and bargaining gains erode where the platform model sets the reference price. Their unions litigate and lobby for classification parity; individually their exit is into the same labor market being repriced.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, traditional_sector_employees, payer,
    organized, biographical, constrained, national).

% Order rides, meals, and errands at prices below what employment-inclusive cost structures would support, with speed and convenience as the competing dimension. They also carry part of the shifted cost as taxpayers financing in-work benefits and emergency care for uncovered workers, and as recipients of service quality shaped by piece-rate time pressure.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__precarity_extraction_reading, on_demand_service_consumers, payer).

% Public unemployment, injury, and pension schemes cover what the classification leaves uncovered: emergency treatment for uninsured workplace injury, means-tested top-ups during earnings dips, and eventual old-age support for workers who contributed nothing. The schemes receive no employer contributions from the platforms that generate the liabilities and cannot decline the mandate; the shortfall lands on their financing or on other taxpayers.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Labor ministries, agencies, and courts adjudicate employment status, run presumption tests, and set minimum earnings and transparency standards. They hear evidence from every other seat and can redraw the classification line — the single rule on which the whole arrangement's cost structure turns.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_regulators_and_courts, observer,
    institutional, generational, analytical, national).

% Were removed from the apps by automated enforcement — ratings thresholds, fraud flags, or opaque policy violations — and in most jurisdictions have no human appeal and no seat where the rules that removed them are written. They would testify that the flexibility framing ends at the first algorithmic judgment against them.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, algorithmically_deactivated_workers, excluded,
    powerless, immediate, trapped, global).

% Are today's platform workers at the age when the missing contributions come due. They appear at no current table where classification, take rates, or benefit contributions are set, and would object that today's flexibility premium is being financed out of their old age.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, future_retirees_of_gig_workforce, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real matching problem: spiky, geographically dispersed demand for rides, deliveries, and tasks meets dispersed part-time labor supply; the platforms' dispatch and dynamic-pricing machinery clears that market at low search cost and lets workers fit income around other commitments.
% TRANSFER_FUNCTION: Moves money from consumers' per-transaction payments to workers at algorithmically set piece rates minus a platform take rate (commonly 20-35%), while moving risk — vehicle and insurance costs, unpaid waiting time, income volatility, injury, and old-age contributions — from the platforms' books onto workers and public social insurance; the classification converts what would be employer obligations into worker-borne costs.
% ABSENT_VOICES: Algorithmically deactivated workers have no seat in the rule-setting or appeals that govern them; future retirees of today's gig workforce bear the contribution gap but appear at no current bargaining table; workers in jurisdictions prohibiting organizing cannot voice collective terms; informal-sector workers displaced by platform entry are unrepresented. They are located outside platform governance, which has no worker representation mechanism, and outside shareholder-controlled corporate decisions.
% DISAPPEARANCE_RATIONALE: On-demand services would reprice to employment-inclusive costs or contract sharply; platform valuations built on labor-cost externalization would reset; millions of workers would need to re-enter traditional employment or unemployment systems; social insurance financing would rebalance in the other direction. The matching function would survive in some form — the cost-shift structure would not.
% FOUNDING_PROBLEM: Idle capacity against spiky demand: vehicles, tools, and part-time availability sat unused while demand for rides, deliveries, and tasks fluctuated, and post-2008 labor slack left workers needing supplemental income without fixed-schedule jobs. Flexible employment was built to clear that market without the fixed costs of the employment relationship.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: labor economists across camps — including critics of the platforms — and worker testimony confirm the matching problem is real and that many workers value schedule control; earnings and time-use studies document both the flexibility value and its uncompensated offsets. The contest is over whether the current arrangement distributes the gains of solving the problem, not over whether the problem exists.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__precarity_extraction_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.75 at interval end): take rates are set unilaterally and decoupled from marginal dispatch cost; unpaid waiting and repositioning time falls outside paid meters; classification avoids employer payroll and benefit obligations worth a substantial share of total compensation. Suppression (0.70) is structural: deactivation and ratings discipline, asymmetric information about true hourly earnings, and well-funded legal defense of the classification boundary — persistence depends on enforcement machinery, not participant preference alone. Theater (0.46) reflects the widening gap between the entrepreneurship narrative ('be your own boss', 'flexibility') and piece-rate control reality; because the coordination function is genuine, theater is moderate rather than dominant. Accessibility collapse (0.50): exit exists — stop logging in, take a job — but the alternatives are largely the same or worse precarity, and for many the alternative is no income. Resistance (0.60): coordinated log-offs and strikes, misclassification litigation, the EU Platform Work Directive, the California AB5/Prop 22 cycle. The measurement series share one time grid (T=0 is roughly 2010, T=15 roughly 2025); suppression_requirement is authored because enforcement capacity visibly built up over the interval — algorithmic management matured, deactivation systems scaled, and classification defense became a standing legal function — so this is an enforcement-intensification trajectory, not a static picture.
 *
 * PERSPECTIVAL GAP:
 *   The platform seat computes a coordination structure it built and can defend as market-clearing; the worker seat computes piece rates with discipline and no floor; the investor seat sees a cost structure that is the source of its returns; the social-insurance seat sees liabilities arriving without contributions; the regulator seat sees a classification line it can move. Same arrangement, divergent computed types per seat — the divergence is the measurement the corpus exists to take, not an error to reconcile against the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   platform_operators and platform_capital_investors sit at the beneficiary end of d: they collect the take rate and its capitalized value, and arbitrage-grade exit keeps them there. gig_platform_workers sit near the full-target end — powerless individually, constrained exit, income source fused with the app. traditional_sector_employees are targets through competitive erosion: organized power, but their exit is into the same repriced labor market. social_insurance_systems are targets with trapped exit — they cannot decline the mandate that makes them the residual claimant on the gaps. on_demand_service_consumers derive a low d from the beneficiary declaration, but the override (moderate power atom to d=0.3) corrects for their secondary payer position: the cost-shift reaches them through tax-financed top-ups and quality externalities, so they are not near-pure beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — clearing spiky demand against idle capacity — is live, so this is not a zombie mandate; the arrangement still performs its coordination function and the world would rearrange around its absence. The mandatrophy risk runs in the other direction: as the matching machinery becomes routine infrastructure, the flexibility narrative grows increasingly theatrical relative to the discipline and cost-shift it licenses — theater_ratio rising from 0.25 to 0.46 over the interval tracks that drift. If dispatch were regulated into commodity infrastructure or the classification boundary fell, the remaining structure would be bare cost-shifting plus algorithmic discipline, and the tangled rope would decompose toward its extractive pole. The classification guards against both mislabels: pure coordination would hide the uncompensated risk transfer; pure extraction would deny the real flexibility value some workers demonstrably receive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading of the flexible_employment_legitimacy kernel — what structurally changes if a sibling reading is adopted instead?',
    'Adoption of a sibling reading re-authors the constraint: the market_efficiency_reading would count risk externalization as a priced market-clearing cost and epsilon collapses toward the resource_allocation coordination floor; the developmental_state_reading would re-author the arrangement as transitional with formalization milestones, moving it scaffold-ward with a declared sunset.',
    'The computed classification flips with the reading — tangled_rope here, rope or scaffold under the siblings; cross-reading comparison is valid only at the referent level, never at the epsilon level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: which reading of the kernel this story instantiates and what the sibling readings would change structurally.').

omega_variable(
    total_compensation_offset_question,
    'When total compensation is fully accounted — unpaid time, vehicle and insurance costs, missing employer contributions, income volatility — do platform wage gains offset the transferred risk?',
    'Linked administrative and platform data matching worker earnings diaries against benefit entitlements and an employer-obligation counterfactual.',
    'If gains fully offset, this reading overstates epsilon and the market_efficiency reading gains ground; if not, the cost-shift is deeper than 0.75 and the snare boundary comes into view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(total_compensation_offset_question, empirical, 'Whether the wage gains of flexible employment compensate for the risks shifted onto workers.').

omega_variable(
    flexibility_preference_vs_necessity,
    'What share of gig workers would retain the arrangement at employment-equivalent total compensation, versus take it only because the alternatives are worse?',
    'Panel surveys combining stated-preference and revealed-preference designs; observed take-up of traditional employment offers at matched total pay.',
    'A high voluntary share enlarges the genuine coordination component and moves the classification rope-ward; a low share indicates the flexibility framing functions as cover and moves it snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_preference_vs_necessity, empirical, 'Voluntary-flexibility versus economic-necessity composition of the platform workforce.').

omega_variable(
    classification_boundary_reformability,
    'Can the employment/contractor boundary be redrawn so platforms internalize their obligations without destroying the matching function''s flexibility?',
    'Natural experiments: Spain''s rider law, the EU Platform Work Directive''s presumption mechanism, California''s AB5 versus Prop 22, UK worker-status rulings.',
    'If reformable, the constraint is a tangled rope that can be rebalanced; if every reform either collapses the flexibility value or is litigated into nullity, the structure is snare-ward with the coordination story as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_boundary_reformability, empirical, 'Whether the cost-shift is legally separable from the coordination function.').

omega_variable(
    worker_coalition_formation_capacity,
    'Can dispersed, individually powerless gig workers convert latent coalition capacity — coordinated log-offs, strikes, app-mediated organizing — into sustained bargaining power?',
    'Track strike outcomes, union recognition bids, and earnings effects following collective actions across jurisdictions.',
    'High coalition capacity raises resistance and can bargain the cost-shift down (rebalancing the tangled rope); persistent failure confirms the powerlessness atom and indicates drift toward the extractive pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_coalition_formation_capacity, empirical, 'Whether the powerless-atom victims hold coalition potential that the individual-level power atom misses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t3, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 3, 0.3).
narrative_ontology:measurement_basis(flex_tr_t3, observed).
narrative_ontology:measurement(flex_tr_t6, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 6, 0.34).
narrative_ontology:measurement_basis(flex_tr_t6, observed).
narrative_ontology:measurement(flex_tr_t9, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 9, 0.39).
narrative_ontology:measurement_basis(flex_tr_t9, observed).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 12, 0.43).
narrative_ontology:measurement_basis(flex_tr_t12, observed).
narrative_ontology:measurement(flex_tr_t15, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 15, 0.46).
narrative_ontology:measurement_basis(flex_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t3, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(flex_be_t3, observed).
narrative_ontology:measurement(flex_be_t6, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 6, 0.67).
narrative_ontology:measurement_basis(flex_be_t6, observed).
narrative_ontology:measurement(flex_be_t9, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 9, 0.71).
narrative_ontology:measurement_basis(flex_be_t9, observed).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 12, 0.74).
narrative_ontology:measurement_basis(flex_be_t12, observed).
narrative_ontology:measurement(flex_be_t15, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement_basis(flex_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t3, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 3, 0.56).
narrative_ontology:measurement_basis(flex_su_t3, observed).
narrative_ontology:measurement(flex_su_t6, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 6, 0.61).
narrative_ontology:measurement_basis(flex_su_t6, observed).
narrative_ontology:measurement(flex_su_t9, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 9, 0.65).
narrative_ontology:measurement_basis(flex_su_t9, observed).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement_basis(flex_su_t12, observed).
narrative_ontology:measurement(flex_su_t15, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement_basis(flex_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy__developmental_state_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'flexible employment' decomposes, per the epsilon-invariance principle, into a three-member constraint family over the shared kernel flexible_employment_legitimacy: this story (precarity_extraction_reading, epsilon 0.75, tangled_rope claim), the market_efficiency_reading (epsilon near the coordination floor, rope claim), and the developmental_state_reading (transitional, scaffold claim with formalization sunset). The members share one referent — the standing arrangement — and differ in reading-indexed epsilon, beneficiary/victim structure, and claimed type; the upstream efficiency reading is typically cited as cover by the platforms, which is why this reading's regulatory victories register as structural pressure on it. Each member links the other two via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__precarity_extraction_reading, moderate, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
