% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
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
 *   human_readable: Unconditional Income Support as Dependency Mechanism (Dependency Trap Reading)
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   A polity operates unconditional income support: transfers arrive
 *   regardless of work status, financed by compulsory taxation of employed
 *   households. This story instantiates the DEPENDENCY TRAP READING of that
 *   standing arrangement: the transfer raises recipients' reservation wages
 *   above declining market productivity, labor supply contracts, skills
 *   depreciate during non-employment, and dependence deepens across household
 *   generations — while the tax base funds the non-participation it
 *   subsidizes. The reading concedes a genuine floor function (destitution is
 *   prevented) and locates the defect in what the floor does to capacity over
 *   time. Per the epsilon-invariance principle, the colloquial label
 *   'unconditional income support' decomposes into three structurally
 *   distinct claims held by different parties; this file authors only the
 *   dependency-trap instantiation, with epsilon assessed for the standing
 *   unconditional arrangement as THIS reading sees it (reading-indexed value
 *   over a fixed referent — never the reading's endorsed alternative). The
 *   sibling readings are separate constraint files linked through the
 *   network. KEY AGENTS (by structural relationship): -
 *   labor_supply_reducing_recipients: Primary beneficiary-turned-bearer
 *   (powerless/trapped) — collects the transfer, sheds labor supply, bears
 *   skill depreciation - transfer_dependent_households: Multi-generation
 *   beneficiary unit (powerless/trapped) — household budgets equilibrate
 *   around the transfer - net_taxpaying_workers: Primary target
 *   (organized/constrained) — funds non-participation through compulsory
 *   taxation - children_in_workless_households: Involuntary bearer
 *   (powerless/trapped) — inherits the dependence trajectory without voice -
 *   transfer_program_legislators: Agenda setter (institutional/mobile) —
 *   enacts, adjusts, and could sunset the arrangement -
 *   low_wage_sector_employers: Secondary bearer (organized/mobile) — faces
 *   contracted low-wage labor supply - labor_economists_and_evaluators:
 *   Analytical observer (analytical/analytical) — runs the experiments all
 *   three readings argue from
 *
 * KEY AGENTS:
 *   - labor_supply_reducing_recipients: primary beneficiary seat that simultaneously bears the atrophy cost — dual-listed in beneficiaries and victims because the reading's mechanism pays this population and corrodes it through the same channel
 *   - transfer_dependent_households: beneficiary seat at generational horizon — the unit across which dependence compounds
 *   - net_taxpaying_workers: primary target seat — compulsory contributors with no opt-out from the taxing jurisdiction
 *   - children_in_workless_households: involuntary bearer seat — no voice, no consent, longest exposure
 *   - transfer_program_legislators: agenda-setter seat — holds the dial that could condition, taper, or sunset the arrangement
 *   - low_wage_sector_employers: secondary bearer seat — experiences the labor-supply contraction at the bottom of the market
 *   - labor_economists_and_evaluators: analytical observer seat — produces the elasticity and atrophy evidence on which every reading of the kernel depends
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.6).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.38).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support as Dependency Mechanism (Dependency Trap Reading)").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "economic/political/social").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, 'fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296').
narrative_ontology:cs_kernel_codification('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', formalized).
narrative_ontology:cs_authority_grounding('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', distributed).
narrative_ontology:cs_reading_relation('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', foundational, unconditional_transfer_erodes_work_capacity).
narrative_ontology:cs_axiom_status(unconditional_transfer_erodes_work_capacity, holdable).
narrative_ontology:cs_axiom_grounding('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', unconditional_transfer_erodes_work_capacity, empirically_contingent).
narrative_ontology:cs_axiom('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', secondary, contribution_reciprocity_is_owed).
narrative_ontology:cs_axiom_status(contribution_reciprocity_is_owed, holdable).
narrative_ontology:cs_axiom_grounding('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', contribution_reciprocity_is_owed, deontological).
narrative_ontology:cs_reference_frame('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', conditional_reciprocity_baseline).
narrative_ontology:cs_drift_state('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', contemporary_post_pilot_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa5dbcb9-ce11-4290-ad6e-c64dd3fd4296', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, labor_supply_reducing_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, transfer_dependent_households).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, net_taxpaying_workers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, labor_supply_reducing_recipients).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, children_in_workless_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, transfer_dependent_households).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, low_wage_sector_employers).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, labor_supply_elasticity_doctrine).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, welfare_dependency_hypothesis).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, human_capital_depreciation_theory).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, reservation_wage_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Working-age adults who receive the unconditional transfer and respond by reducing hours, declining low-wage offers, or leaving the labor market altogether. The transfer arrives without conditions, so nothing obliges job search; meanwhile each year of non-employment lowers the wage employers will offer and lets skills and contacts depreciate, so the gap between the transfer and achievable market earnings widens over time. Leaving the arrangement would mean accepting a wage now well below the transfer plus lost leisure — an exit that gets more expensive the longer the arrangement is used.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_supply_reducing_recipients, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, labor_supply_reducing_recipients, payer).

% Households in which transfer receipt has become the stable budgetary baseline across adult generations. Work attachment weakens cohort by cohort: parents model non-employment, local networks contain fewer employed contacts, and the household's spending, housing, and time patterns equilibrate around the guaranteed payment. The household collects the income and simultaneously absorbs the slow loss of the habits, references, and expectations that market re-entry would require.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, transfer_dependent_households, beneficiary,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, transfer_dependent_households, payer).

% Employed households whose taxes finance the transfers. They cannot opt out of the taxing jurisdiction short of emigration, and their marginal rates rise as the recipient population grows. On this reading's account they fund not only subsistence but voluntary non-participation, and they carry the political burden of contesting an arrangement that is insulated by entitlement status. Their coalition power is real — they vote, lobby, and occasionally win work-requirement legislation — but the arrangement's core flows survive each contest.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, net_taxpaying_workers, payer,
    organized, biographical, constrained, national).

% Children raised in households where adult work is largely absent. They had no voice in the arrangement that shapes their environment: the neighborhood role models, parental work norms, school engagement, and expectation-setting they absorb all form around transfer-supported non-employment. Their exposure is the longest in the system and their outcomes — adult earnings, employment, and eventual program receipt — unfold decades after the decisions that shaped them were made by others.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, children_in_workless_households, payer,
    powerless, generational, trapped, national).

% Legislative majorities and budget committees that enact eligibility rules, set benefit levels, and could condition, taper, or sunset the arrangement. They face electoral cycles shorter than the dependence dynamics they administer, so the visible costs of tightening (immiserating current recipients) arrive before the visible benefits (restored labor supply), while expansion buys immediate constituency support. Individually they bear little of the arrangement's cost and can leave office before its long-run consequences mature.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, transfer_program_legislators, agenda_setter,
    institutional, immediate, mobile, national).

% Firms relying on entry-level and low-wage labor, which face thinner applicant pools and upward wage pressure as transfer-supported workers decline offers below their new reservation wage. They bear a second-order cost of the arrangement — a contracted labor supply at the bottom of the market — and respond with automation, relocation, or restructuring rather than political defense of any particular labor-market configuration. They sit outside the transfer arrangement's design conversation while absorbing one of its market consequences.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, low_wage_sector_employers, payer,
    organized, biographical, mobile, national).

% Researchers who design and evaluate the experiments — the negative income tax trials, the Finnish basic income evaluation, dividend and cash-transfer studies — estimating labor-supply responses, skill trajectories, and intergenerational outcomes. Every reading of the income-support commitment argues from their output, and their findings are contested, re-analyzed, and selectively cited by all factions. They collect no transfer and pay no tax into the arrangement; their stake is interpretive authority over what the evidence shows.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labor_economists_and_evaluators, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, labor_supply_reducing_recipients).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a guaranteed income floor decoupled from employment status: people whose market earnings fall below subsistence — or who exit the market entirely — are supported without means-testing investigations, work-search bureaucracy, or the stigma machinery of conditional poor relief. The coordination problem solved is subsistence security at minimal administrative overhead.
% TRANSFER_FUNCTION: Moves purchasing power from net-taxpaying employed households to transfer recipients regardless of work status. On this reading's account the arrangement also moves something else over time: labor-market attachment, skills, and work norms drain out of the recipient population and across household generations, while the contributor side finances the drainage.
% ABSENT_VOICES: Children in recipient households would object if consulted — they bear the longest exposure with zero voice in the design. Future generations bearing compounded dependence and a thinned contributor base are structurally absent (no seat exists for them). Taxpayers in high-contribution brackets participate only through blunt electoral instruments, and low-wage employers experience the labor-supply contraction without a seat in the arrangement's design conversation.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, recipient household budgets would break immediately — with atrophied skills and stale references, most long-term recipients could not replace the transfer with wages at comparable speed, producing a destitution spike the arrangement's critics concede. Low-wage labor supply would loosen over subsequent years, contributor tax burdens would fall, and the political economy of the welfare state would reorganize around whatever conditional or targeted replacement was erected. Household formation, migration, and local labor markets in high-dependence regions would all rearrange.
% FOUNDING_PROBLEM: Conditional poor relief humiliated applicants, invaded households, and trapped recipients between inadequate benefits and punitive work tests; unconditional income support was proposed (from Friedman's negative income tax onward) to guarantee subsistence security without surveillance, stigma, or the poverty traps that phase-out rates built into means-tested systems.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the historical record on conditional poor relief (poor-law scholarship documenting the surveillance and trap effects of earlier systems) and by the fiscal and evaluative literature (VATT's Finnish basic income evaluation, SIME/DIME follow-ups, actuarial analyses by independent fiscal authorities). No corroborator attests the founding problem is simply solved: destitution risk persists in attenuated forms while this reading attests the solution now regenerates the dependence it was designed to avoid, and the freedom-floor reading attests the opposite — the status is genuinely disputed between camps, with the evaluation record itself the disputed terrain.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.60 (moderate-substantial) because, on this reading's assessment, a significant share of transfer-financed non-participation is disincentive-driven rather than incapacity-driven: real productive capacity is drawn out of the market and replaced by taxed transfers, and the recipient population pays a second, non-monetary cost in depreciated skills. Suppression is authored LOW relative to extraction (0.38) and is deliberately NOT scaled by anything — it is a raw structural property. Pure unconditionality coerces no one into taking the transfer and builds no benefit cliff; the suppressive content sits almost entirely on the contributor side (compulsory taxation with no exit from the jurisdiction) and in political entrenchment (entitlement status that insulates the arrangement from ordinary revision). Theater ratio 0.30: the floor function is real, but a growing share of activity is performative — pilot programs launched and evaluated without scaling decisions, administrative rituals maintained past usefulness, pilot findings cited selectively by every faction. Accessibility collapse is low (0.30): work-conditioned support, earned-income subsidies, and targeted transfers remain politically available alternatives; nothing about the arrangement forecloses them. Resistance is substantial (0.55): fiscal-conservative coalitions, work-requirement legislation, and taxpayer mobilization persistently contest the arrangement, and the rising suppression_requirement series tracks the growing enforcement effort needed to hold the arrangement against that resistance — the narrative here specifically traces enforcement-capacity change, which is why the third series is authored at all. The three series share one time grid (T=0..54 at nine-unit steps, roughly 1970–2024: negative income tax experiments through the contemporary pilot era); every tracked metric is authored at every shared point, and the interval-end values equal the base_properties scalars. The drift is monotonic rather than cyclical: expansion, evidence, partial retrenchment, renewed expansion — each cycle ratcheting the baseline slightly higher, which is why the series trend rather than oscillate.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the finding. From the agenda-setter seat (legislators) the arrangement is a policy dial: adjustable, reversible, one line item among many — low personal stakes, mobile exit via electoral turnover. From the primary target seat (net taxpayers) the same arrangement is enforced transfer: compulsory, unconsented, with no jurisdictional exit at acceptable cost. From the beneficiary seat (recipients) the arrangement is security that quietly reprices re-entry — each year out of work lowers the wage the market will pay relative to the transfer, so the seat that collects the cash is also the seat being locked in. From the observer seat the arrangement is a live empirical dispute whose parameters (elasticities, atrophy rates, transmission coefficients) determine which of the three kernel readings is even coherent. The engine computes per-seat classifications from the structural data; this story does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations pull labor_supply_reducing_recipients and transfer_dependent_households toward the beneficiary pole; their trapped exit status pushes back toward the target pole — and that tension is exactly the reading's thesis, so the derived mid-range directionality for the recipient seats is the honest output, not an artifact to be overridden. The dual listing of labor_supply_reducing_recipients in both beneficiaries and victims encodes the same ambivalence structurally. Net_taxpaying_workers and children_in_workless_households are declared victims with constrained or trapped exit, placing them near the full-target end; children sit furthest because their exposure is longest and wholly unchosen. Legislators derive near-symmetric institutional directionality (they administer what they neither significantly pay into nor collect). Employers and economists are peripheral: employers bear diffuse second-order costs, economists hold the analytical seat with no material flow. No directionality overrides are authored — the structural data (declarations plus exit atoms) already produces the relationships this reading asserts, and the guidance reserves overrides for cases the derivation gets wrong.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding warrant — a subsistence floor WITHOUT the traps of conditional poor relief — is, on this reading, half-eclipsed: destitution prevention remains live, but the arrangement now manufactures the dependence it was designed to avoid, so mandatrophy is partial and contested rather than resolved (the resolved flag is therefore left undeclared; the R5 fields carry the genealogy). Classifying as tangled_rope rather than snare preserves what the reading itself concedes: the floor genuinely protects people who cannot work, and erasing the arrangement would immiserate them — which is also why fixing_cost is prohibitive. Refusing the rope classification registers what the reading insists on: the same structure that coordinates the floor moves resources from productive to non-productive use and corrodes the recipients' capacity, with identifiable bearers on both sides. The temporal series guards against the two mislabelings in opposite directions: if theater_ratio kept climbing while the floor function hollowed out, the arrangement would drift toward theatrical maintenance of a commitment nobody efficiently defends; if the gains ever concentrated behind an administering seat rather than diffusing to recipient households, the capture signature would surface. Coalition dynamics matter on the target side: net taxpayers already constitute a voting bloc (organized power), which is why resistance is high; the recipient seats lack equivalent coalition capacity, which is precisely the asymmetry the dependence mechanism exploits.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (dependency_trap_reading) of the income_support_commitment kernel; which structural features of the arrangement would change under the sibling readings freedom_floor_reading and targeting_efficiency_reading?',
    'Author the sibling stories as separate epsilon-invariant constraints and compare extractedness, victim sets, and computed types across the family; the disagreement is located in the sign and magnitude of the labor-supply/atrophy mechanism and in whether unconditionality or universality is the operative defect.',
    'Under freedom_floor_reading the same standing arrangement computes with low extraction and reversed polarity (recipients as pure beneficiaries, no atrophy victims); under targeting_efficiency_reading the defect relocates from unconditionality to universality, changing the agenda-setter seat and the candidate fix. Classification of THIS file is valid only for this reading''s seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story instantiates one of three live readings of the income-support kernel.').

omega_variable(
    labor_supply_response_magnitude,
    'How large are the labor-supply reductions attributable to unconditional transfers, relative to the security value of the income floor?',
    'Long-horizon randomized and quasi-experimental evidence: negative income tax experiment follow-ups, the Finnish basic income evaluation, Alaska Permanent Fund dividend studies, GiveDirectly village trials.',
    'Near-zero persistent responses collapse this reading toward the freedom_floor reading and drive measured extraction toward the coordination-cost floor; large persistent responses confirm elevated extraction and activate the atrophy mechanism as a distinct harm channel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_magnitude, empirical, 'Magnitude of the disincentive effect that carries this reading''s core claim.').

omega_variable(
    atrophy_vs_adverse_selection,
    'Does observed non-participation reflect transfer-caused erosion of skills and work capacity, or adverse selection of people already inclined toward non-work?',
    'Exogenous variation in benefit generosity paired with longitudinal measures of wages, hours, and skill proxies, distinguishing within-person trajectories from cross-sectional composition.',
    'Causal atrophy makes the arrangement an active corrosive agent and raises the recipient seat''s effective burden despite its beneficiary position; pure selection shifts responsibility off the arrangement and lowers the recipient-side directionality toward the beneficiary pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrophy_vs_adverse_selection, empirical, 'Whether the arrangement corrodes capacity or merely reveals pre-existing preferences.').

omega_variable(
    intergenerational_transmission_channel,
    'Does growing up in a transfer-dependent household causally depress children''s later labor-market attachment and earnings, or does childhood income security improve their outcomes?',
    'Sibling and cross-cohort comparisons around program expansions and contractions, tracking adult employment, earnings, and program receipt of exposed children.',
    'Confirmed transmission makes children_in_workless_households full targets with high directionality and hardens the state-dependence claim; null results flip that seat toward incidental beneficiary and weaken the intergenerational leg of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_channel, empirical, 'Whether dependence transmits across generations or childhood security is protective.').

omega_variable(
    fiscal_scale_stability,
    'Is the transfer-and-tax equilibrium stable as population aging shrinks the contributor base relative to the recipient population?',
    'Actuarial projection of contribution and payout streams under demographic aging scenarios, with sensitivity to labor-force participation assumptions.',
    'Instability would intensify the enforcement burden on the contributor side (holding taxpayers in the arrangement as their numbers thin) and push the arrangement toward crisis-driven restructuring rather than steady-state drift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fiscal_scale_stability, empirical, 'Long-run sustainability of the contributor base beneath the transfer commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(inco_tr_t9, income_support_commitment__dependency_trap_reading, theater_ratio, 9, 0.16).
narrative_ontology:measurement(inco_tr_t18, income_support_commitment__dependency_trap_reading, theater_ratio, 18, 0.19).
narrative_ontology:measurement(inco_tr_t27, income_support_commitment__dependency_trap_reading, theater_ratio, 27, 0.21).
narrative_ontology:measurement(inco_tr_t36, income_support_commitment__dependency_trap_reading, theater_ratio, 36, 0.24).
narrative_ontology:measurement(inco_tr_t45, income_support_commitment__dependency_trap_reading, theater_ratio, 45, 0.27).
narrative_ontology:measurement(inco_tr_t54, income_support_commitment__dependency_trap_reading, theater_ratio, 54, 0.3).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(inco_be_t9, income_support_commitment__dependency_trap_reading, base_extractiveness, 9, 0.44).
narrative_ontology:measurement(inco_be_t18, income_support_commitment__dependency_trap_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement(inco_be_t27, income_support_commitment__dependency_trap_reading, base_extractiveness, 27, 0.51).
narrative_ontology:measurement(inco_be_t36, income_support_commitment__dependency_trap_reading, base_extractiveness, 36, 0.54).
narrative_ontology:measurement(inco_be_t45, income_support_commitment__dependency_trap_reading, base_extractiveness, 45, 0.57).
narrative_ontology:measurement(inco_be_t54, income_support_commitment__dependency_trap_reading, base_extractiveness, 54, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(inco_su_t9, income_support_commitment__dependency_trap_reading, suppression_requirement, 9, 0.31).
narrative_ontology:measurement(inco_su_t18, income_support_commitment__dependency_trap_reading, suppression_requirement, 18, 0.33).
narrative_ontology:measurement(inco_su_t27, income_support_commitment__dependency_trap_reading, suppression_requirement, 27, 0.34).
narrative_ontology:measurement(inco_su_t36, income_support_commitment__dependency_trap_reading, suppression_requirement, 36, 0.36).
narrative_ontology:measurement(inco_su_t45, income_support_commitment__dependency_trap_reading, suppression_requirement, 45, 0.37).
narrative_ontology:measurement(inco_su_t54, income_support_commitment__dependency_trap_reading, suppression_requirement, 54, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file (dependency_trap_reading) authors epsilon ~0.60 for the standing arrangement as seen from the dependency-trap seat: beneficiaries are the voluntarily non-participating recipient class, victims are contributing workers and capacity-eroded recipients. The freedom_floor sibling authors LOW epsilon over the SAME referent with reversed polarity (no atrophy victims; recipients as unambiguous beneficiaries) — the upstream/downstream structure runs through the shared empirical record, where each reading cites the same trials (negative income tax experiments, Finnish basic income evaluation) as vindication. The targeting_efficiency sibling changes the defect locus rather than the polarity: its victims are the misallocated dollars and the excluded needy, not contributors as such. All three files link one another through affects_constraints; no single file hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
