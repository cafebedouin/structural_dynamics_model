% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: Quantitative-Growth Reading of Performance Legitimacy (GDP-Target Regime)
 *   domain: political economy / development planning / state capitalism
 *
 * SUMMARY:
 *   A governing party-state grounds its legitimacy in delivering measured GDP
 *   growth: five-year plans set expansion targets, cadre evaluation ties
 *   official careers to jurisdictional growth rates, and the financial system
 *   is steered to fund the investment that produces the numbers. The
 *   arrangement mobilized savings into ports, rail, housing, and industrial
 *   capacity at historic speed and moved hundreds of millions of people into
 *   urban work; it also ran on transfers no participant voted on —
 *   administered deposit rates below market clearing, rural land acquired at
 *   requisition prices and resold at urban margins, and a
 *   household-registration system that kept urban labor cheap and municipal
 *   service obligations narrow. This file instantiates ONE reading of the
 *   performance_legitimacy kernel — the quantitative_growth_reading, which
 *   holds that demonstrated aggregate expansion and the employment it
 *   generates are what legitimacy must show. Per the epsilon-invariance
 *   principle, epsilon here refers to the standing growth-target arrangement
 *   as this reading assesses it (crediting growth-delivered employment,
 *   treating export dependency and overcapacity as tolerated costs); the
 *   three sibling readings — qualitative_development_reading,
 *   techno_nationalist_reading, livelihood_security_reading — are separate
 *   constraint files with their own epsilon over the same standing
 *   arrangement, linked through network.affects_constraints.
 *
 * KEY AGENTS:
 *   - central_planning_authority: agenda-setter (institutional/constrained) — sets national growth targets and designs the cadre-evaluation machinery that enforces them
 *   - gdp_measured_local_officials: local administrators (institutional/identity_locked) — run land finance, court investment, report the numbers; careers rise and fall on measured growth
 *   - industrial_export_complex: primary beneficiary (powerful/arbitrage) — receives steered credit, held-down input costs, and export channels
 *   - state_lending_institutions: intermediary beneficiary (institutional/constrained) — book the spread between repressed deposit rates and planned lending rates
 *   - household_savers: primary payer (powerless/trapped) — supply cheap loanable funds under administered rates and capital controls
 *   - land_requisitioned_rural_households: payer (powerless/trapped) — surrender land at requisition prices with no seat in the valuation
 *   - hukou_restricted_migrant_workers: payer (powerless/constrained) — supply mobile labor without portable services or bargaining rights
 *   - credit_starved_private_firms: payer (moderate/constrained) — crowded out of planned credit, pay informal-market premiums
 *   - independent_labor_organizers: excluded voice (powerless/trapped) — would bargain over the wage and service terms the model depends on holding narrow
 *   - multilateral_development_assessors: analytical observer (analytical/analytical) — audit the growth record and cost structure from outside the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.61).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.7).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.56).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "Quantitative-Growth Reading of Performance Legitimacy (GDP-Target Regime)").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political economy / development planning / state capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '6c74fba5-c15a-4eb7-a2cd-6f89e0db3976').
narrative_ontology:cs_kernel_codification('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', formalized).
narrative_ontology:cs_authority_grounding('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', extraction).
narrative_ontology:cs_interpretation_layer_present('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976').
narrative_ontology:cs_reading_relation('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', foundational, demonstrated_aggregate_expansion_constitutes_performance).
narrative_ontology:cs_axiom_status(demonstrated_aggregate_expansion_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', demonstrated_aggregate_expansion_constitutes_performance, empirically_contingent).
narrative_ontology:cs_axiom('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', foundational, employment_delivery_tracks_output_growth).
narrative_ontology:cs_axiom_status(employment_delivery_tracks_output_growth, holdable).
narrative_ontology:cs_axiom_grounding('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', employment_delivery_tracks_output_growth, empirically_contingent).
narrative_ontology:cs_axiom('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', secondary, investment_deepening_is_the_growth_engine).
narrative_ontology:cs_axiom_status(investment_deepening_is_the_growth_engine, holdable).
narrative_ontology:cs_axiom_grounding('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', investment_deepening_is_the_growth_engine, instrumental).
narrative_ontology:cs_reference_frame('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', high_speed_catchup_expansion).
narrative_ontology:cs_drift_state('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', post_2015_new_normal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6c74fba5-c15a-4eb7-a2cd-6f89e0db3976', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_lending_institutions).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, household_savers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, land_requisitioned_rural_households).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, hukou_restricted_migrant_workers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, credit_starved_private_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, investment_led_catchup_development).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, gdp_target_cadre_evaluation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the five-year plan and annual growth targets, designs the cadre-evaluation criteria that tie official advancement to delivered numbers, and adjusts the mix of targets (growth, debt, environment, stability) as conditions change. It can redefine what is measured, but it is itself bound by the legitimacy formula: a sustained miss on the headline number directly threatens the claim it governs by. Redesigning the formula means conceding, publicly, that the old demonstration no longer works.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, central_planning_authority, agenda_setter,
    institutional, generational, constrained, national).

% Run cities and provinces under tournament evaluation: mobilize land requisition and resale to fund local budgets, court investors with tax breaks and prepared sites, steer local lending, and report the resulting numbers up the hierarchy. Promotion windows open and close on measured growth; a weak print can end a career. The same target that lifts them disciplines them — opting out of the tournament means leaving the track their professional lives are built on.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, agenda_setter,
    institutional, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, gdp_measured_local_officials, payer).

% Large industrial groups, exporters, and their construction suppliers operate on credit priced below what unsteered markets would charge, on input costs held down by suppressed wages and land terms, and on export channels built by the infrastructure program. Earnings reinvest into capacity, which deepens dependence on the model continuing. Individual firms can relocate production or list abroad, but the complex as a whole has no alternative customer base at anything near current scale.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    powerful, biographical, arbitrage, global).

% Take deposits at administratively set rates, lend according to planned priorities, and book the spread. Executing the credit plan — quota allocation by sector and locality — makes them part of the machinery as well as collectors of its margin. Their balance sheets absorb the deferred cost of weak projects as evergreen loans; recognizing losses at scale would expose the model's arithmetic.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_lending_institutions, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, state_lending_institutions, agenda_setter).

% Hold the bulk of national savings in bank deposits and property because capital controls close foreign outlets and shallow markets offer little else. Administered deposit rates have often sat below inflation, so the purchasing power of savings erodes while it finances the lending machine. Saving more is the main lever available — precautionary accumulation in response to thin social insurance — which further depresses the consumption the rebalancing rhetoric calls for.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, household_savers, payer,
    powerless, generational, trapped, national).

% Villages whose collectively held land is acquired at requisition prices and resold at urban margins many times higher. Compensation is set by the acquiring government; the appreciation accrues elsewhere. Losing land ends a livelihood that cannot be resumed, and the affected hold no procedural seat in the valuation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, land_requisitioned_rural_households, payer,
    powerless, generational, trapped, regional).

% Work in the cities the model builds under a registration system that ties schooling, healthcare, and pension access to origin rather than workplace. Registration keeps labor supply elastic and wage demands low, and keeps the fiscal obligations of urbanization narrow. Moving between cities is possible; having rights follow the move is not. Returning home trades income for services.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, hukou_restricted_migrant_workers, payer,
    powerless, biographical, constrained, national).

% Smaller private enterprises sit outside planned credit priorities and borrow informally at premiums several points above the state channel, or shrink to fit internal cash flow. They compete against incumbents whose effective borrowing cost they cannot approach. Exit means liquidation; staying means paying the wedge.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, credit_starved_private_firms, payer,
    moderate, biographical, constrained, national).

% Would negotiate wage shares, hours, and service provision directly if organizing space existed. It does not: representation runs through official channels scoped to dispute mediation, not bargaining over the model's terms. Their absence from the table is a maintained condition, not an oversight.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, independent_labor_organizers, excluded,
    powerless, biographical, trapped, national).

% Article IV teams, multilateral working groups, and academic development economists audit the growth record, the household income share, local-debt stocks, and capacity utilization from outside the system. They publish assessments the planning apparatus cites when convenient and contests when not. They hold no enforcement seat.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, multilateral_development_assessors, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the capital-mobilization and objective-alignment problem of late development: pools dispersed household savings through a controlled banking channel into infrastructure and industrial capacity faster than thin private capital markets would allow, and aligns millions of local administrators on one measurable, comparable objective — jurisdictional growth — with tournament rewards.
% TRANSFER_FUNCTION: Moves purchasing power from household savers (administered deposit rates below market clearing), from rural landholders (requisition-price acquisition, urban-margin resale), and from migrant workers (wages held down by registration-bound labor supply and narrow service obligations) toward industrial borrowers, construction, and local-government balance sheets; moves career advancement to whichever officials deliver the measured numbers.
% ABSENT_VOICES: Independent labor representatives, rural land-rights defenders, and household-consumer advocates have no seat in target-setting or evaluation design; their objections surface through bounded petition channels, episodic local unrest, or technocratic critique published abroad. Their absence is load-bearing: the wage and land terms the model runs on depend on organized voice staying out.
% DISAPPEARANCE_RATIONALE: If the target-evaluation-credit complex vanished overnight, cadre incentives would scatter, land finance would lose its justification engine, planned credit allocation would unwind toward market pricing, and the industrial-export complex would face cost curves it has never paid — the entire development-model plumbing reorganizes around whatever replaced the target.
% FOUNDING_PROBLEM: A poor, capital-scarce economy with surplus agricultural labor needed to industrialize quickly, and a non-electoral government needed a legible, continuous demonstration that its rule performed.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: multilateral assessments (IMF Article IV consultations, the World Bank–DRC China 2030 study) and the development-economics literature corroborate both halves — the original capital-mobilization problem was real and substantially solved, and the cost side (declining household income share, overcapacity, local debt) is independently documented. No corroborating source attests that the founding problem remains unchanged; the contest is between 'transformed but not finished' and 'solved and now self-perpetuating.'
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.61, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.61 for the standing arrangement as this reading assesses it: the reading credits growth-delivered employment and poverty reduction against the transfer mechanisms, so epsilon sits below what a livelihood-security reading of the same arrangement would author — but the transfers themselves (rate repression, land-margin capture, wage suppression) are structural and large however they are valued. Suppression (0.70) is a raw structural property, deliberately NOT scaled by power or scope: it measures the enforcement machinery — target-linked cadre evaluation, planned credit allocation, registration barriers, capital controls, and the bounded space for independent labor voice — that holds the arrangement in place. Theater_ratio (0.34) reflects documented statistical padding and showcase-project accounting alongside genuinely functional construction and production. Accessibility_collapse (0.56): once the arrangement is understood, alternatives (consumption-led rebalancing, welfare-grounded legitimacy) are visible, but the evaluation system, local fiscal architecture, and credit plumbing all route back to the growth target. Resistance (0.52): precautionary household saving, statistical gaming by localities, central rebalancing campaigns meeting local foot-dragging, and episodic land and labor unrest. All three tracked series run on one shared seven-point grid (1994–2025) so no metric row borrows another's endpoints: base_extractiveness rises through the investment boom and plateaus after 2015 as rebalancing rhetoric meets persistent structure; theater_ratio peaks mid-2010s and recedes slightly under central statistical enforcement; suppression_requirement tracks enforcement build-up through 2012 and its redirection (rather than relaxation) afterward.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the central_planning_authority seat the arrangement is a working machine it designed and can recalibrate — coordination with a legitimacy dividend. From the gdp_measured_local_officials seat it is both instrument and treadmill: the same target that advances careers consumes them, which is why that seat carries a payer secondary role. From the payer seats — savers, dispossessed rural households, registered-out migrants, credit-starved private firms — the arrangement operates as enforced transfer with exits closed: capital controls trap savings, requisition is irreversible, registration follows the worker rather than the worker's family. The industrial_export_complex and state_lending_institutions seats experience it as opportunity. Coalition potential among the powerless payers is real on paper and blocked in practice: savers are isolated by capital controls, rural households by village-level fragmentation, migrants by registration geography — the enforcement design doubles as coalition prevention. Inter-institutionally, the center bears legitimacy risk, localities bear target pressure, banks bear deferred-loss risk, and industry collects; same-level laterally, local officials compete in a promotion tournament that makes each the other's benchmark. The engine computes per-seat classifications from these structural facts; nothing authored here adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (industrial_export_complex, gdp_measured_local_officials, state_lending_institutions) derive low directionality for those seats; victim declarations (household_savers, land_requisitioned_rural_households, hukou_restricted_migrant_workers, credit_starved_private_firms) derive high directionality, amplified by exit position — trapped savers sit nearer the full-target end than mobile actors would. The dual-positioned seats are handled structurally rather than by override: local officials carry agenda_setter with a payer secondary_role (career hostage to the target), and state lenders carry beneficiary with an agenda_setter secondary_role (they execute the credit plan). No directionality_overrides entries are authored: the derivation chain from beneficiary/victim data plus exit options already places every seat correctly, and a power-atom-keyed override would misapply one d_value across three distinct institutional seats. Receipt: the transfer's largest single terminal accumulation among the named seats lands on the industrial_export_complex (steered credit converting to capacity and retained earnings); local officials intermediate land-finance margins through their budgets and state lenders book the spread, but neither holds the terminal position — hence gain_flow names the complex. Fixing cost is prohibitive: replacing the regime requires simultaneous redesign of cadre evaluation, local fiscal architecture, credit pricing, and social insurance, each of which destabilizes the others mid-transition.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than pure extraction preserves the genuine coordination achievement the reading rests on: capital-scarce urbanization was a real collective-action problem, and the target system solved it at scale — a snare-only reading would erase the poverty-reduction and infrastructure record that gives the legitimacy claim its force. Conversely, refusing the snare label prevents the reading's own cover story ('growth is jobs is welfare') from laundering the transfers: the same structure that mobilized capital also held down the household income share that would have funded the daily-life delivery the sibling reading foregrounds. On mandatrophy: the founding problem (capital scarcity plus mass underemployment) has largely transformed into capital abundance and overcapacity, but the employment-absorption and legitimacy-demonstration functions remain contested-live, so the mandate cannot yet be declared resolved. The risk trajectory is mandate-atrophy by demographic arithmetic — a legitimacy formula keyed to a growth rate the economy may no longer be able to produce, defended by escalating enforcement rather than revised.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the quantitative_growth_reading of the performance_legitimacy kernel — what structurally changes if a sibling reading displaces it?',
    'Comparative classification across the four reading-files: track which reading''s premises the cadre-evaluation and planning documents actually operationalize, and re-run classification under the displaced reading''s beneficiary/victim structure.',
    'Under livelihood_security_reading the same arrangement authors higher epsilon (daily-life costs count fully, growth-delivered employment counts less); under techno_nationalist_reading the beneficiary set shifts toward strategic-industry champions and export dependency flips from tolerated cost to failure mode; under qualitative_development_reading overcapacity converts from tolerated cost to primary indictment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one-of-four readings of the performance_legitimacy kernel; displacement changes epsilon weighting, beneficiary set, and the tolerated-cost ledger.').

omega_variable(
    designed_vs_emergent_target_regime,
    'Is the growth-target arrangement a deliberately designed control system (cadre evaluation as authored instrument) or an emergent equilibrium of developmental-state competition that the targets merely express?',
    'Institutional genealogy: test whether removing the explicit evaluation metric collapses the growth-first behavior (designed) or whether inter-jurisdictional competition reproduces it under any metric (emergent).',
    'If designed, repair responsibility concentrates in the central_planning_authority seat and fixing is a redesign decision; if emergent, responsibility is distributed across the tournament structure and no single-seat fix exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designed_vs_emergent_target_regime, empirical, 'Whether the target regime is authored design or emergent competitive equilibrium.').

omega_variable(
    official_growth_statistics_inflation,
    'How far do officially reported growth figures diverge from independently estimable activity (nighttime luminosity, tax receipts, electricity consumption, freight volumes), and does the divergence vary by jurisdiction and period?',
    'Cross-validation of provincial series against satellite luminosity and physical-activity indicators; systematic review of statistical-law enforcement cases.',
    'Wider divergence raises the true theater_ratio above the authored 0.34, weakens the legitimacy claim''s empirical grounding, and pushes the arrangement toward performative maintenance; narrower divergence supports the reading''s own account.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(official_growth_statistics_inflation, empirical, 'Measurement-integrity gap between reported and physically corroborated growth.').

omega_variable(
    demographic_ceiling_on_demonstrable_expansion,
    'Can the reading''s foundational premise — legitimacy demonstrated through continued expansion — remain satisfiable as working-age population declines and catch-up convergence slows?',
    'Observe the legitimacy repertoire as growth settles below the historical band: whether the target system is quietly redefined (range targets, quality qualifiers) or enforcement intensity rises to defend the headline number.',
    'If the premise becomes unsatisfiable, the reading loses holdable status: expect either migration of legitimacy weight to sibling readings or substitution of coercion for performance — a rising suppression trajectory with flat extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demographic_ceiling_on_demonstrable_expansion, empirical, 'Whether demographic arithmetic caps the demonstrable-expansion premise.').

omega_variable(
    overcapacity_tolerance_boundary,
    'At what point does the reading''s tolerated-cost ledger (export dependency, chronic overcapacity) flip from accepted price of expansion to unambiguous waste — and who has standing to declare the flip?',
    'Compare social return on marginal investment against the cost of capital sector by sector; determine whether the declaration channel is internal (plan adjustment) or external (market discipline, trade retaliation).',
    'Past the boundary, the coordination half of the structure thins while the transfer half persists and the computed classification slides toward pure extraction; before it, the tolerated-cost framing holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overcapacity_tolerance_boundary, conceptual, 'Boundary condition on the reading''s tolerated-cost ledger.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 1994, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t1994, performance_legitimacy__quantitative_growth_reading, theater_ratio, 1994, 0.18).
narrative_ontology:measurement(perf_tr_t2001, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(perf_tr_t2008, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2008, 0.3).
narrative_ontology:measurement(perf_tr_t2012, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2012, 0.36).
narrative_ontology:measurement(perf_tr_t2015, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(perf_tr_t2019, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2019, 0.37).
narrative_ontology:measurement(perf_tr_t2025, performance_legitimacy__quantitative_growth_reading, theater_ratio, 2025, 0.34).

% Extraction over time
narrative_ontology:measurement(perf_be_t1994, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 1994, 0.45).
narrative_ontology:measurement(perf_be_t2001, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2001, 0.5).
narrative_ontology:measurement(perf_be_t2008, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2008, 0.58).
narrative_ontology:measurement(perf_be_t2012, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2012, 0.63).
narrative_ontology:measurement(perf_be_t2015, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(perf_be_t2019, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement(perf_be_t2025, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 2025, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t1994, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 1994, 0.55).
narrative_ontology:measurement(perf_su_t2001, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2001, 0.6).
narrative_ontology:measurement(perf_su_t2008, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2008, 0.66).
narrative_ontology:measurement(perf_su_t2012, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2012, 0.7).
narrative_ontology:measurement(perf_su_t2015, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(perf_su_t2019, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2019, 0.69).
narrative_ontology:measurement(perf_su_t2025, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% The performance-legitimacy kernel decomposes per the epsilon-invariance principle: the colloquial label 'growth-based legitimacy' conflates four structurally distinct claims about what must be demonstrated. This file carries the quantitative_growth_reading (epsilon 0.61 over the standing growth-target arrangement, assessed by this reading's own lights — growth-delivered employment offsets part of the transfer burden, and export dependency and overcapacity sit in the tolerated-cost ledger). The sibling files carry the other readings with their own epsilon, beneficiary sets, and tolerated-cost ledgers; edges run from this file to all three because this reading currently occupies the operational evaluation machinery and therefore shapes the others' operating environment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
