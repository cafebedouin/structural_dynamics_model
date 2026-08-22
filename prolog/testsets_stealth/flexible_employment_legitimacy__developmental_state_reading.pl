% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__developmental_state_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Flexible Employment as State-Managed Transition Toward Formalization (Developmental-State Reading)
 *   domain: labor_economics/social_policy
 *
 * SUMMARY:
 *   In economies governed by developmental-state administrations, flexible
 *   employment — app-based delivery, ride-hailing, outsourced piecework — is
 *   officially framed not as a settled market category but as a transitional
 *   form: a stage the state manages toward eventual formalization under the
 *   standard employment settlement. The arrangement under contest is that
 *   management regime itself: multi-year twelve-point plan packages, a 2027
 *   standardization target that names the transition's endpoint,
 *   occupational-injury insurance pilots, algorithm-filing requirements, and
 *   wage guidance framed as managed transition rather than market outcome.
 *   This file instantiates ONE reading of the contested kernel
 *   flexible_employment_legitimacy — the developmental-state reading — and
 *   authors epsilon for the standing managed-transition arrangement as that
 *   reading assesses it: a real but partial coordination achievement whose
 *   transition window simultaneously defers full labor costs onto flexible
 *   workers and hands platforms a lawful forbearance rent. Claim and metrics
 *   are authored independently: the reading claims scaffold (a transitional
 *   support with a declared endpoint); the metrics describe moderate
 *   extraction declining slowly, rising theater as target-setting
 *   proliferates, and a maturing enforcement apparatus. KEY AGENTS (by
 *   structural relationship): development_state_planners: agenda-setting seat
 *   (institutional/constrained) — issues plans, sets the 2027 target, runs
 *   pilots, collects mandate and performance credit; platform_companies:
 *   primary rent-receiving seat (powerful/arbitrage) — gains forbearance and
 *   deferred labor costs, pays compliance costs; flexible_platform_workers:
 *   primary cost-bearing seat (powerless/constrained) — carries volatility
 *   and coverage gaps, receives incremental protections;
 *   formal_sector_employers: cost-disadvantaged competitor seat
 *   (organized/constrained) — bears full labor costs rivals defer;
 *   service_consumers: incidental beneficiary seat (moderate/mobile) — buys
 *   below-full-cost services; labor_rights_advocates: excluded seat
 *   (moderate/trapped) — presses reclassification from outside the drafting
 *   tables; policy_research_institutes: analytical observer
 *   (analytical/analytical) — tracks coverage, injury claims, wage series.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__developmental_state_reading, 0.5).
domain_priors:suppression_score(flexible_employment_legitimacy__developmental_state_reading, 0.46).
domain_priors:theater_ratio(flexible_employment_legitimacy__developmental_state_reading, 0.39).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0.46).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0.39).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__developmental_state_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__developmental_state_reading, scaffold).
narrative_ontology:human_readable(flexible_employment_legitimacy__developmental_state_reading, "Flexible Employment as State-Managed Transition Toward Formalization (Developmental-State Reading)").
narrative_ontology:topic_domain(flexible_employment_legitimacy__developmental_state_reading, "labor_economics/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__developmental_state_reading).
narrative_ontology:has_sunset_clause(flexible_employment_legitimacy__developmental_state_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__developmental_state_reading, '9f49660c-3635-4ced-b472-bafd0a1ff49f').
narrative_ontology:cs_kernel_codification('9f49660c-3635-4ced-b472-bafd0a1ff49f', formalized).
narrative_ontology:cs_authority_grounding('9f49660c-3635-4ced-b472-bafd0a1ff49f', lineage).
narrative_ontology:cs_interpretation_layer_present('9f49660c-3635-4ced-b472-bafd0a1ff49f').
narrative_ontology:cs_reading_relation('9f49660c-3635-4ced-b472-bafd0a1ff49f', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f49660c-3635-4ced-b472-bafd0a1ff49f', flexible_employment_legitimacy__precarity_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('9f49660c-3635-4ced-b472-bafd0a1ff49f', foundational, transitional_forms_require_deliberate_management).
narrative_ontology:cs_axiom_status(transitional_forms_require_deliberate_management, holdable).
narrative_ontology:cs_axiom_grounding('9f49660c-3635-4ced-b472-bafd0a1ff49f', transitional_forms_require_deliberate_management, instrumental).
narrative_ontology:cs_axiom('9f49660c-3635-4ced-b472-bafd0a1ff49f', foundational, formalization_is_the_normative_endpoint).
narrative_ontology:cs_axiom_status(formalization_is_the_normative_endpoint, holdable).
narrative_ontology:cs_axiom_grounding('9f49660c-3635-4ced-b472-bafd0a1ff49f', formalization_is_the_normative_endpoint, conventional).
narrative_ontology:cs_axiom('9f49660c-3635-4ced-b472-bafd0a1ff49f', secondary, managed_wage_growth_over_market_clearing).
narrative_ontology:cs_axiom_status(managed_wage_growth_over_market_clearing, holdable).
narrative_ontology:cs_axiom_grounding('9f49660c-3635-4ced-b472-bafd0a1ff49f', managed_wage_growth_over_market_clearing, instrumental).
narrative_ontology:cs_reference_frame('9f49660c-3635-4ced-b472-bafd0a1ff49f', state_managed_transitional_sequence).
narrative_ontology:cs_drift_state('9f49660c-3635-4ced-b472-bafd0a1ff49f', approach_of_2027_standardization_target, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9f49660c-3635-4ced-b472-bafd0a1ff49f', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, platform_companies).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, development_state_planners).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, service_consumers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, flexible_platform_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, flexible_platform_workers).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__developmental_state_reading, platform_companies).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, developmental_state_gradualism).
narrative_ontology:constraint_vindicates(flexible_employment_legitimacy__developmental_state_reading, standard_employment_settlement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ministry and provincial planning bodies issue the twelve-point packages, publish the 2027 standardization target, enroll riders in occupational-injury pilots, and require algorithm filings. They gain administrative mandate, pilot budgets, and performance credit for coverage and wage figures. Stepping off the framework would mean conceding the field to immediate reclassification or to unmanaged expansion, so their exit runs through the very framework they administer.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, development_state_planners, agenda_setter,
    institutional, generational, constrained, national).

% Operate dispatch and contracting structures that keep couriers, drivers, and pieceworkers outside standard employment contracts. During the transition window they defer employer costs — social-insurance contributions, severance, overtime — while published timetables make the reclassification threat schedulable rather than arbitrary. They pay pilot contributions and registration costs, lobby to slow reclassification, and can restructure business models, shift jurisdictions, or automate in ways individual workers cannot.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, platform_companies, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, platform_companies, payer).

% Deliver food, drive passengers, and complete piecework under algorithmic management without employee status. They receive incremental protections — injury-pilot coverage in enrolled cities, earnings floors in some jurisdictions — while carrying income volatility, weeks without coverage, and no severance. Switching platforms offers similar terms; leaving the sector usually means lower-paid formal work, so their practical exit is narrow.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, flexible_platform_workers, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, flexible_platform_workers, beneficiary).

% Pay full social-insurance contributions and carry full labor-law obligations for comparable workforces while platform rivals defer equivalent costs during the window. They petition for accelerated standardization to level costs, supply services into the platform economy meanwhile, and expect a leveled field if formalization completes; their exit from the arrangement is essentially nil — they can only press for its acceleration.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(flexible_employment_legitimacy__developmental_state_reading, formal_sector_employers, beneficiary).

% Buy delivery, ride-hailing, and domestic services priced below what full formal-labor costing would support during the window. They organize nothing and bear none of the arrangement's costs; if formalization raised platform labor costs abruptly, their prices would rise with it.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, service_consumers, beneficiary,
    moderate, immediate, mobile, national).

% Independent organizers and legal-aid lawyers bring reclassification suits and publicize injury cases. They are not seated in plan drafting, which reserves chairs for ministry officials, platform associations, and the official union federation; their access runs through courts and publicity rather than the timetable itself.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, labor_rights_advocates, excluded,
    moderate, biographical, trapped, national).

% University and academy teams track coverage rates, injury-claim outcomes, and wage series across provinces. They publish evaluations that either corroborate the managed-transition account or document slippage between targets and instruments; they hold no enforcement power and no seat in drafting.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__developmental_state_reading, policy_research_institutes, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__developmental_state_reading, platform_companies).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__developmental_state_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences the incorporation of a new employment form into the formal labor system without destroying its job-creation function: one transition timetable coordinates ministries, provincial governments, platform firms, and the official union federation around staged insurance portability, injury-pilot enrollment, algorithm registration, and a dated standardization target.
% TRANSFER_FUNCTION: Moves regulatory forbearance and scheduling certainty to platform firms, and below-full-cost service prices to consumers, during the transition window; moves deferred protection costs, income volatility, and coverage gaps onto flexible workers; moves administrative mandate, pilot budgets, and performance credit to the planning apparatus.
% ABSENT_VOICES: Independent labor organizers, legal-aid lawyers, and rank-and-file platform workers are absent from the tables where plans and targets are drafted, which seat ministry officials, platform associations, and the official union federation. Advocates of immediate full reclassification and of deregulated expansion both stand outside the managed consensus; the timetable is set without either.
% DISAPPEARANCE_RATIONALE: Overnight removal would force an immediate choice the regime exists to defer: platforms would face abrupt reclassification litigation or unregulated expansion, millions of injury-pilot enrollments would lapse, formal-sector competitors would lose the prospect of a leveled field, and the wage-guidance channel would close — the sector's governance would reorganize around whichever force moved fastest.
% FOUNDING_PROBLEM: Platform and flexible employment grew faster than labor-law categories built for the standard employment relationship: injured riders fell between employee and contractor status with no insurance answer, earnings had no floor, and no portable account carried contributions across gigs. The regime was built to close that gap gradually, without an employment shock.
% FOUNDING_PROBLEM_CORROBORATION: Judicial decisions compensating injured gig workers, statistical reporting of social-insurance coverage gaps among new employment forms, and ILO analyses of platform-work protection gaps all attest the founding problem from outside the benefiting parties; platform firms' own petitions for clarified status corroborate it self-interestedly. No party attests the problem is closed.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__developmental_state_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__developmental_state_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__developmental_state_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(flexible_employment_legitimacy__developmental_state_reading, 'none', 1).
narrative_ontology:epsilon_provenance(flexible_employment_legitimacy__developmental_state_reading, 0.5, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.50 and gently declining (0.60 to 0.50 across the interval): by this reading's own lights the regime reduces exposure — injury pilots, earnings floors, portability steps — while the window's residual rent persists. Theater rises 0.20 to 0.39: plan issuance, target restatement, and pilot announcements grow faster than binding formalization instruments, a Goodhart drift signal in which the target increasingly substitutes for the transition it names. Suppression_requirement is authored because enforcement-capacity change is the dynamic this story traces: the interval opens with near-zero enforcement of a tolerated gray zone and closes with a mature administrative apparatus (algorithm filings, mandatory pilot enrollment, platform accountability sessions), 0.25 to 0.46. All three series share one time grid (T=0..12, mapped to 2015-2027) so no metric row borrows another's endpoints; T=12 values are marked projected because the target year lies ahead of the generation date. Accessibility_collapse 0.55: rival framings stay discursively alive while rival arrangements (immediate reclassification, deregulated expansion) are administratively foreclosed. Resistance 0.45: platforms delay and lobby, advocates litigate, workers protest episodically — all inside channels the regime itself defines. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   From the planner seat the arrangement is stewardship: a sequence that absorbed tens of millions of workers while building protection machinery that did not exist at the interval's start. From the worker seat the same sequence is indefinite deferral: every plan names an endpoint, and every endpoint arrives with the destination moved forward. From the platform seat it is purchasable predictability: compliance costs are known, the reclassification threat is scheduled rather than arbitrary, and the forbearance rent compounds annually. Formal-sector employers experience a fourth position: paying full freight beside subsidized rivals, with their remedy contingent on a completion date they do not control. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Planners declare beneficiary (mandate, pilot budgets, performance credit) and sit toward the subsidized end despite administering rather than collecting the rent. Platforms declare beneficiary with payer secondary: the forbearance rent dominates the compliance cost, placing them nearest the beneficiary end, amplified by arbitrage-grade exit. Workers declare payer with beneficiary secondary: incremental protections damp but do not reverse their position near the target end, and constrained exit holds them there. Formal-sector employers are net payers (the cost asymmetry versus platform rivals exceeds their contingent gain) with a beneficiary tail that pays out only if formalization completes. Consumers are pure incidental beneficiaries at national scope. No directionality overrides are authored: role declarations plus exit atoms already separate the seats, and a power-atom-keyed override could not distinguish the two mid-power seats from each other without distorting both.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim keeps the regime's real coordination output visible — protection machinery that did not exist in 2015 — while the temporal series tests the transition against its own declared endpoint. The R5 mismatch consumer reads founding_problem_status against disappearance_verdict: both are currently live/world_rearranges, a consistent pair — the founding coverage gap is real and attested from outside the benefiting parties, and removing the arrangement would rearrange the sector. The failure mode to watch is status flipping to dead while the verdict stays world_rearranges: a zombie transition kept alive by target restatement, which would cross-check against the rising theater_ratio path documented here. The classification prevents two opposite mislabels: reading the regime as pure extraction erases the protections actually built; reading it as accomplished coordination erases the rent the window still pays out annually.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the kernel flexible_employment_legitimacy — the developmental_state_reading, which holds flexible employment to be a transitional form manageable toward formalization. Does that framing track the arrangement''s actual structure, or does it launder persistence as process? The sibling readings locate the disagreement differently: market_efficiency_reading denies the need for management entirely; precarity_extraction_reading denies that the form is transitional at all, treating it as a durable extraction structure.',
    'Compare the compiled classifications and transfer-flow data across the three sibling stories at matched time points; if this reading''s epsilon diverges sharply from observed flows (rents accruing without convergence to formalized status), the transitional premise is doing cover work rather than descriptive work.',
    'If the transitional premise fails, this constraint collapses toward the precarity reading''s structure (victim-set widening, enforcement reframed as maintenance of extraction) or toward the market reading''s (management reframed as overhead); the 2027 target stops functioning as a sunset and starts functioning as authorization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Whether the developmental-state framing is a faithful structural description or a legitimizing frame over a persistent arrangement.').

omega_variable(
    sunset_clause_genuineness,
    'Is the 2027 standardization target a genuine transition endpoint — after which the transitional arrangement gives way to binding formal rules — or a legitimizing device that stabilizes the kernel while the transition extends?',
    'Observe the instrument mix as the target year approaches: enactment of binding formalization measures (mandatory insurance conversion, reclassification triggers with dates) versus restatement, postponement, or replacement of the target with new interim targets.',
    'If the target functions as indefinite authorization, the declared sunset is theatrical, the scaffold decays toward inertial or extractive operation, and the has_sunset_clause declaration should be re-authored as performative rather than operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_genuineness, empirical, 'Whether the declared endpoint binds or merely stabilizes the managed-transition narrative.').

omega_variable(
    wage_growth_causality,
    'This reading attributes observed wage growth among flexible workers to managed transition — policy outcome, not market outcome. Is that attribution correct, or is wage growth driven by labor-supply dynamics (aging cohorts, delivery-labor shortages) that would have occurred without the guidance channel?',
    'Difference-in-differences across jurisdictions with differing intervention intensity, and event studies around plan announcements; if wage trajectories parallel in low-intervention jurisdictions, the managed-outcome claim fails.',
    'If wages are market-clearing outcomes, the reading''s vindicated proposition (developmental management produces the gains) loses its evidentiary base and the planning apparatus''s performance credit is misattributed — weakening the authority claim that distinguishes this reading from the market reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_growth_causality, empirical, 'Whether wage growth under the regime is a policy product or a market product the regime claims credit for.').

omega_variable(
    transition_cost_incidence,
    'Who ultimately bears the transition''s deferred-protection costs, and does the formalization the regime promises arrive in time to compensate the cohort that paid them — or do transition generations age out unprotected while the endpoint recedes?',
    'Longitudinal cohort tracking of flexible workers through formalization events: coverage acquisition rates, injury-compensation outcomes, and pension-account accumulation by entry cohort.',
    'If cohorts exit the workforce before formalization reaches them, the transitional framing functions as inter-cohort transfer — the victim set widens from current workers to the transition generations specifically, and the reading''s own lights can no longer certify the arrangement as fair sequencing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transition_cost_incidence, empirical, 'Whether transition costs are repaid to the cohorts that bore them or silently shifted forward.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__developmental_state_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(felm_dev_tr_t0, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(felm_dev_tr_t0, observed).
narrative_ontology:measurement(felm_dev_tr_t2, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 2, 0.24).
narrative_ontology:measurement_basis(felm_dev_tr_t2, observed).
narrative_ontology:measurement(felm_dev_tr_t4, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(felm_dev_tr_t4, observed).
narrative_ontology:measurement(felm_dev_tr_t6, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(felm_dev_tr_t6, observed).
narrative_ontology:measurement(felm_dev_tr_t8, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 8, 0.34).
narrative_ontology:measurement_basis(felm_dev_tr_t8, observed).
narrative_ontology:measurement(felm_dev_tr_t10, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(felm_dev_tr_t10, observed).
narrative_ontology:measurement(felm_dev_tr_t12, flexible_employment_legitimacy__developmental_state_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(felm_dev_tr_t12, projected).

% Extraction over time
narrative_ontology:measurement(felm_dev_be_t0, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement_basis(felm_dev_be_t0, observed).
narrative_ontology:measurement(felm_dev_be_t2, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement_basis(felm_dev_be_t2, observed).
narrative_ontology:measurement(felm_dev_be_t4, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(felm_dev_be_t4, observed).
narrative_ontology:measurement(felm_dev_be_t6, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 6, 0.53).
narrative_ontology:measurement_basis(felm_dev_be_t6, observed).
narrative_ontology:measurement(felm_dev_be_t8, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(felm_dev_be_t8, observed).
narrative_ontology:measurement(felm_dev_be_t10, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement_basis(felm_dev_be_t10, observed).
narrative_ontology:measurement(felm_dev_be_t12, flexible_employment_legitimacy__developmental_state_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(felm_dev_be_t12, projected).

% Suppression requirement over time
narrative_ontology:measurement(felm_dev_su_t0, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(felm_dev_su_t0, observed).
narrative_ontology:measurement(felm_dev_su_t2, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 2, 0.3).
narrative_ontology:measurement_basis(felm_dev_su_t2, observed).
narrative_ontology:measurement(felm_dev_su_t4, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 4, 0.35).
narrative_ontology:measurement_basis(felm_dev_su_t4, observed).
narrative_ontology:measurement(felm_dev_su_t6, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(felm_dev_su_t6, observed).
narrative_ontology:measurement(felm_dev_su_t8, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(felm_dev_su_t8, observed).
narrative_ontology:measurement(felm_dev_su_t10, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(felm_dev_su_t10, observed).
narrative_ontology:measurement(felm_dev_su_t12, flexible_employment_legitimacy__developmental_state_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(felm_dev_su_t12, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__developmental_state_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__market_efficiency_reading).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__developmental_state_reading, flexible_employment_legitimacy__precarity_extraction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'flexible employment' covers three structurally distinct claims about the same arrangement, decomposed per the epsilon-invariance principle. This story authors epsilon for the managed-transition regime as the developmental-state reading assesses it (moderate, declining); the market_efficiency_reading story authors epsilon for the same labor market read as self-coordinating exchange (near-negligible intended extraction); the precarity_extraction_reading story authors epsilon for the platform labor process read as durable extraction structure (high). The upstream/downstream structure runs through this reading: the state-management regime is the institutional fact both siblings argue about — the market reading treats its interventions as distortion to be measured against, the precarity reading treats its timetable as cover to be exposed. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
