% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__developmental_state_reading, []).

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
 *   constraint_id: flexible_employment_legitimacy__developmental_state_reading
 *   human_readable: Flexible Employment as Managed Transition toward Formalization (Developmental State Reading)
 *   domain: labor economics/social policy
 *
 * SUMMARY:
 *   The developmental state classifies flexible employment — platform gig
 *   work, informal flexible arrangements, dispatched and outsourced labor —
 *   as a transitional form on the way to formalized standard employment, and
 *   administers it as such: guidance opinions define the category, a 12-point
 *   plan assigns protection obligations to platforms, and a 2027
 *   standardization target dates the transition's delivery, with official
 *   statistics narrating wage growth as an outcome of state steering rather
 *   than market clearing. This story instantiates ONE reading of the
 *   contested flexible_employment_legitimacy kernel. The epsilon referent is
 *   the standing arrangement under contest — the state-managed
 *   flexible-employment regime itself — assessed by this reading's own
 *   lights: it carries a genuine coordination function (sequencing a labor
 *   transition of hundreds of millions without instability) while workers
 *   bear the transition's costs indefinitely and platform operators capture
 *   the protection-cost gap. Sibling readings (market_efficiency_reading,
 *   precarity_extraction_reading) are separate constraints with their own
 *   epsilon and victim sets, linked via network.affects_constraints. Claim
 *   and metrics are authored independently: the reading's own frame claims a
 *   scaffold — transitional support with a declared endpoint — while the
 *   authored metrics describe substantial and rising extraction, structural
 *   suppression of independent voice, and a plan apparatus whose performative
 *   share grows as deadlines slip.
 *
 * KEY AGENTS:
 *   - developmental_state_bureaucracies: agenda-setter and beneficiary (institutional/identity_locked) — administers the transition, sets the 2027 target, collects governance legitimacy; bears fiscal subsidy costs and legitimacy risk
 *   - platform_operators: primary beneficiary of deferred formalization (powerful/arbitrage) — captures the gap between platform revenue and formal-employment protection costs; bears partial compliance costs under the plan
 *   - platform_gig_workers: primary target (powerless/trapped) — bear income volatility, algorithmic management, and deferred social insurance during the open-ended transition
 *   - rural_migrant_workers: secondary target (powerless/constrained) — hukou-bound; the transitional frame layers onto household-registration exclusion
 *   - local_governments: dual-positioned beneficiary/payer (institutional/constrained) — count flexible employment in stability metrics and court platform investment while funding insurance shortfalls and managing protest
 *   - formal_sector_firms: secondary beneficiary (organized/mobile) — draw on a flexible labor reserve without parity claims raising sector-wide costs
 *   - independent_labor_scholars: excluded voice (moderate/constrained) — document precarity and contest the transitional frame outside the policy process
 *   - ilo_platform_work_monitoring: analytical observer (institutional/analytical) — tracks coverage and pay floors comparatively; no enforcement seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.62).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.55).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as Managed Transition toward Formalization (Developmental State Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor economics/social policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, '831a9403-5d91-439b-aea1-0c9431fe0a29').
narrative_ontology:cs_kernel_codification('831a9403-5d91-439b-aea1-0c9431fe0a29', formalized).
narrative_ontology:cs_authority_grounding('831a9403-5d91-439b-aea1-0c9431fe0a29', extraction).
narrative_ontology:cs_interpretation_layer_present('831a9403-5d91-439b-aea1-0c9431fe0a29').
narrative_ontology:cs_reading_relation('831a9403-5d91-439b-aea1-0c9431fe0a29', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('831a9403-5d91-439b-aea1-0c9431fe0a29', flexible_employment_legitimacy__precarity_extraction_reading, influences).
narrative_ontology:cs_axiom('831a9403-5d91-439b-aea1-0c9431fe0a29', foundational, transitional_legitimacy_conditionality).
narrative_ontology:cs_axiom_status(transitional_legitimacy_conditionality, holdable).
narrative_ontology:cs_axiom_grounding('831a9403-5d91-439b-aea1-0c9431fe0a29', transitional_legitimacy_conditionality, instrumental).
narrative_ontology:cs_axiom('831a9403-5d91-439b-aea1-0c9431fe0a29', foundational, state_steering_transition_necessity).
narrative_ontology:cs_axiom_status(state_steering_transition_necessity, holdable).
narrative_ontology:cs_axiom_grounding('831a9403-5d91-439b-aea1-0c9431fe0a29', state_steering_transition_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('831a9403-5d91-439b-aea1-0c9431fe0a29', managed_transition_toward_formalization).
narrative_ontology:cs_drift_state('831a9403-5d91-439b-aea1-0c9431fe0a29', contemporary_standardization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('831a9403-5d91-439b-aea1-0c9431fe0a29', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, developmental_state_bureaucracies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, local_governments).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, formal_sector_firms).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, platform_gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, rural_migrant_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, platform_operators).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, local_governments).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, managed_transition_doctrine).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, developmental_state_performance_legitimacy).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, gradualist_welfare_sequencing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ministries and planning agencies classify flexible employment as a transitional form, issue the guidance opinions and the 12-point plan, set the 2027 standardization target, and report wage growth as an outcome of steering. Their administrative identity is built around managing the labor transition; abandoning the management frame would dissolve the mandate their apparatus is organized to execute. They absorb fiscal costs of insurance subsidies and bear legitimacy risk when worker protests surface, while collecting governance legitimacy and an employment-absorption instrument.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, developmental_state_bureaucracies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, developmental_state_bureaucracies, beneficiary).

% Operate delivery, ride-hailing, and task platforms employing tens of millions under flexible classifications. The transitional designation keeps full employer obligations deferred, preserving labor-cost flexibility, and the gap between platform revenue and the protection costs formal employment would carry accrues to them. Under the 12-point plan they bear partial compliance costs — insurance pilots, per-order pay floors, algorithm-disclosure duties — which they lobby to keep incremental, and they can shift corporate structures, reclassify workers, or relocate operations across jurisdictions if compliance tightens.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_operators, beneficiary,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, platform_operators, payer).

% Delivery riders, drivers, and task workers paid per order or task, carrying income volatility, algorithmic management, and occupational risk without full social insurance during the transitional period. Most depend on platform income for urban living costs; alternatives are other platforms on similar terms, informal work, or returning to rural origins at sharp income loss. They have no independent seat in the standardization consultations; voice runs through official union branches and episodic protest.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_gig_workers, payer,
    powerless, immediate, trapped, national).

% Hukou-bound workers who staff much of the flexible workforce; household registration excludes them from urban formal protections and services regardless of employment classification, so the transitional frame layers onto an existing exclusion. They move between construction, manufacturing, and gig work as sectors shift; returning to rural areas remains possible but at substantial income and services loss.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, rural_migrant_workers, payer,
    powerless, biographical, constrained, national).

% Municipal and provincial authorities count flexible employment in their employment-stability metrics, court platform headquarters for investment and tax base, and benefit from flexible labor markets that attract firms. They also absorb the street-level costs: managing rider protests, funding social insurance pool shortfalls, and executing standardization mandates with partial fiscal support. Official careers turn on stability metrics, which makes both strict enforcement and visible unrest costly.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, local_governments, beneficiary,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, local_governments, payer).

% Established manufacturers and service firms draw on a large flexible labor reserve in peaks without carrying equivalent fixed employment, and the transitional frame keeps gig workers from establishing formal-employment parity claims that would raise prevailing labor costs across sectors.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, formal_sector_firms, beneficiary,
    organized, biographical, mobile, national).

% Labor researchers and advocates who document platform precarity and argue the transitional designation has become a permanent arrangement; they publish where permitted, advise off the record, and hold no formal seat in the standardization process. Independent worker representation is restricted, bounding their voice to academic and media channels.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, independent_labor_scholars, excluded,
    moderate, biographical, constrained, national).

% International labor bodies track platform-work standards, publish comparative analyses of social-insurance coverage and pay floors, and engage the state through technical cooperation; they observe and document but hold no enforcement seat in the domestic standardization process.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, ilo_platform_work_monitoring, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences a massive labor-market transition — surplus agricultural and industrial labor into platform and service work — without social instability: the state supplies a legitimacy frame for flexible arrangements, times absorption against restructuring, coordinates platform growth with employment targets, and defers full formalization until absorption is complete.
% TRANSFER_FUNCTION: Moves transition risk and protection costs onto platform gig workers (income volatility, deferred social insurance, algorithmic management), while moving labor-cost flexibility to platform operators, employment-rate stability and governance legitimacy to state bureaucracies, and a disciplined labor reserve to formal-sector firms.
% ABSENT_VOICES: Platform gig workers have no independent seat in the standardization process — consultation runs through official unions and platform associations; independent labor organizers and scholars contesting the transitional frame sit outside the policy conversation; both rival readings of the kernel (market-clearing and precarity) are unrepresented in the official frame.
% DISAPPEARANCE_RATIONALE: If the management regime vanished overnight, platforms would face either a regulatory vacuum (rapid expansion on worst-case terms) or immediate formalization demands; workers' protections would be undefined mid-transition; the state would lose its employment-absorption instrument and its transitional legitimacy frame — the platform labor economy would reorganize around whichever rule-set filled the vacuum.
% FOUNDING_PROBLEM: Mass labor surplus and structural unemployment during state-sector restructuring and the platform economy's explosive growth: how to absorb hundreds of millions of workers into flexible arrangements without social instability, preserving the developmental trajectory while deferring welfare obligations the fiscal system could not yet carry.
% FOUNDING_PROBLEM_CORROBORATION: The state attests the founding problem is live, citing persistent employment pressure and incomplete formalization, with the 2027 target as proof of ongoing transition — attested only from inside the managing apparatus. Outside the beneficiary set: ILO platform-work analyses, independent labor-economics research documenting flexible/informal employment shares persisting across successive formalization deadlines, and worker-protest documentation attest that the transitional designation has outlasted its acute founding problem, indicating a permanent management arrangement; no external source attests the transition narrative.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__developmental_state_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62 because the transitional designation defers employer obligations across successive planning cycles while platform operators capture the protection-cost gap; it stays below pure extraction because the management function is real — sequencing absorption of a vast labor surplus, insurance pilots, per-order pay floors — and some standardization delivery occurs. Suppression is authored at 0.55 and is structural, not internalized: independent worker organizing is restricted, consultation runs through official union channels, and both rival readings of the kernel sit outside the official frame; the rising suppression_requirement series traces the enforcement intensification the 12-point plan represents (authority reassertion). Theater is authored at 0.44: targets are announced and re-announced, deadlines slip, and category language upgrades ('high-quality flexible employment') absorb missed deliveries, but the enforcement machinery is not wholly performative. Accessibility_collapse sits at 0.45 because alternatives persist and are re-priced rather than collapsed — informal work, inter-platform movement, agricultural fallback, formal employment where attainable. Resistance at 0.52 reflects episodic rider strikes, platform lobbying against binding obligations, local implementation drift, and scholarly contestation. All three series share one time grid (points 0-24 at intervals of 4).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the agenda-setter seat the arrangement is successful steering: employment absorbed, wages rising, formalization dated. From the worker seats the same structure is an open-ended deferral — the transition's endpoint recedes as each deadline passes, and protections arrive as pilots and floors rather than as status. From the platform seat it is a compliance horizon to be lobbied into incrementality, with arbitrage available if it binds. The 2027 target is the pivot: imminent delivery from the state seat, moving goalposts from the worker seat, regulatory weather from the platform seat. The engine computes these per-seat divergences from the structural data; the authored transitional claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real positions: platform operators capture the surplus (arbitrage exit holds them near the beneficiary end); the state bureaucracies collect legitimacy but bear fiscal and stability costs; local governments benefit from metrics and investment while funding shortfalls; formal-sector firms gain a reserve. Victim declarations map to the workers who carry transition costs with trapped or hukou-constrained exit, placing them near the full-target end. One override is declared: institutional-seat directionality is set to 0.28 because a derivation reading the bureaucracies and local governments purely from their beneficiary declarations would understate their cost-bearing — subsidy outlays, protest management, insurance-pool pressure, legitimacy risk — placing them net-beneficiary but materially cost-bearing rather than near-pure collectors.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem in its acute form — absorbing restructuring-era surplus labor without instability — was largely delivered years ago; what persists is the management apparatus and the transitional label. The transitional claim keeps the question live rather than settling it: the arrangement declares its own endpoint (2027), so the corpus can measure at the deadline whether the endpoint is honored or reset. If formalization delivery slips again, the contested founding-problem status crossed with the world_rearranges disappearance verdict should flag mandate-outlived-function, and the theater series (0.18 to 0.44) already records the drift from function toward performative maintenance. The classification guards both errors: a pure-extraction reading would erase the real coordination delivered during the acute transition; a pure-coordination reading would erase the open-ended deferral now keeping protection costs off platforms' books. Resolution is deliberately deferred — the deadline is the test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates only the developmental_state_reading of the flexible_employment_legitimacy kernel; would the same standing arrangement classify differently under the market_efficiency_reading or the precarity_extraction_reading?',
    'Generate the two sibling stories over the identical stakeholder surface and compare per-seat classifications, epsilon, and victim sets across the three readings.',
    'Under the market reading epsilon falls toward coordination cost and the victim set empties; under the precarity reading epsilon rises and the state seat is recast from manager to collector of the arrangement it administers; the 2027 target reads as a sunset clause only under this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Kernel contest: three readings of flexible employment legitimacy yield different constraints from one arrangement.').

omega_variable(
    transitional_label_permanence,
    'Is flexible employment actually transitioning toward formalization, or has the ''transitional'' designation become a permanent legitimating label for a steady-state arrangement?',
    'Track the standard-employment share and platform-worker social-insurance coverage against the 2027 target; if coverage plateaus while the label persists past successive deadlines, the designation is functioning as legitimation rather than description.',
    'If the label is permanent, the transitional claim collapses — the declared endpoint is decorative and persistence rests on the frame rather than the transition, shifting classification toward hybrid or extractive types at the 2027 checkpoint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_label_permanence, empirical, 'Whether the transition is real or the transitional label has become permanent.').

omega_variable(
    wage_growth_attribution,
    'Is wage growth in flexible employment a managed-transition outcome produced by state steering, or a market outcome of labor-supply conditions that the steering merely narrates?',
    'Compare wage trajectories across regions and periods differing in steering intensity (plan coverage, insurance-pilot participation, pay-floor adoption), and against periods where enforcement lapsed.',
    'If wages track market conditions rather than steering intensity, the authority''s performance claim weakens, the reading''s legitimacy basis erodes, and the state seat''s benefit is narrated rather than delivered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_growth_attribution, empirical, 'Attribution of wage growth: managed transition or market clearing.').

omega_variable(
    target_deadline_behavior,
    'At the 2027 standardization deadline, will the arrangement terminate or reset — is the target a genuine endpoint or a stabilization device that dates delivery while deferring it?',
    'Observe the deadline''s handling: binding delivery with enforcement, a successor plan with a later date, or reclassification of the target as aspirational.',
    'A reset converts the declared endpoint into kernel stabilization and dates a drift away from the transitional classification at the deadline; honored delivery would confirm the transitional frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(target_deadline_behavior, empirical, 'Endpoint or reset: behavior of the 2027 standardization target at its deadline.').

omega_variable(
    enforcement_bindingness,
    'Do the 12-point plan''s protection obligations bind platform operators in practice, or do they remain aspirational guidance absorbed by local implementation discretion?',
    'Audit actual social-insurance contribution coverage and pay-floor compliance among platform workforces after the plan''s implementation windows, against announced targets.',
    'If obligations are largely aspirational, the theater series understates performativity and the coordination function is thinner than the plan''s text; if they bind, part of the measured extraction is transition cost the plan is genuinely retiring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_bindingness, empirical, 'Whether plan obligations bind in practice or remain aspirational.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(flex_tr_t0, observed).
narrative_ontology:measurement(flex_tr_t4, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(flex_tr_t4, observed).
narrative_ontology:measurement(flex_tr_t8, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(flex_tr_t8, observed).
narrative_ontology:measurement(flex_tr_t12, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 12, 0.31).
narrative_ontology:measurement_basis(flex_tr_t12, observed).
narrative_ontology:measurement(flex_tr_t16, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(flex_tr_t16, observed).
narrative_ontology:measurement(flex_tr_t20, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(flex_tr_t20, observed).
narrative_ontology:measurement(flex_tr_t24, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement_basis(flex_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(flex_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(flex_be_t0, observed).
narrative_ontology:measurement(flex_be_t4, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement_basis(flex_be_t4, observed).
narrative_ontology:measurement(flex_be_t8, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(flex_be_t8, observed).
narrative_ontology:measurement(flex_be_t12, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement_basis(flex_be_t12, observed).
narrative_ontology:measurement(flex_be_t16, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(flex_be_t16, observed).
narrative_ontology:measurement(flex_be_t20, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement_basis(flex_be_t20, observed).
narrative_ontology:measurement(flex_be_t24, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(flex_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(flex_su_t0, observed).
narrative_ontology:measurement(flex_su_t4, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement_basis(flex_su_t4, observed).
narrative_ontology:measurement(flex_su_t8, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement_basis(flex_su_t8, observed).
narrative_ontology:measurement(flex_su_t12, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(flex_su_t12, observed).
narrative_ontology:measurement(flex_su_t16, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement_basis(flex_su_t16, observed).
narrative_ontology:measurement(flex_su_t20, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(flex_su_t20, observed).
narrative_ontology:measurement(flex_su_t24, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(flex_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, hukou_registration_system).

% DUAL FORMULATION NOTE:
% The natural-language label 'flexible employment legitimacy' decomposes into three structurally distinct constraints (the flexible_employment_legitimacy kernel family): this developmental-state reading (transitional form under state management, ε assessed at 0.62 with a two-group victim set), the market_efficiency_reading (legitimate market-clearing, ε near coordination cost, no victim set), and the precarity_extraction_reading (structural precarity, ε substantially higher with the state seat recast as collector). The readings share a referent arrangement but author different ε, different beneficiaries, and different types; they are linked here per the ε-invariance decomposition rule. The hukou_registration_system edge records that this regime's management of migrant labor structurally reinforces household-registration exclusion, giving that system a labor-market function it would not otherwise carry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(flexible_employment_legitimacy__developmental_state_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
