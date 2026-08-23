% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legalization_reading, []).

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
 *   constraint_id: substance_control_legitimacy__legalization_reading
 *   human_readable: Adult Autonomy Boundary over Substance Use (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the legalization reading of the substance-control
 *   legitimacy kernel: competent adults hold autonomy over their own use, and
 *   state authority reaches only conduct that threatens others. The
 *   constraint under examination is that legitimacy boundary as it operates
 *   in adopting jurisdictions — the standing arrangement of
 *   legalization-with-regulation, not any rival arrangement the reading might
 *   prefer. Assumptions stated plainly: interval t=0 approximates the early
 *   post-adoption transition and t=24 a mature regime at roughly two-year
 *   steps; the arrangement is treated as jurisdiction-generic across adopting
 *   polities. Per the epsilon-invariance principle this is one member of a
 *   constraint family: the prohibition reading (users inside the victim set,
 *   high epsilon against the criminalized arrangement) and the harm-reduction
 *   reading (population-harm minimization criterion) are separate files
 *   linked through network.affects_constraints. Epsilon here (0.41) is
 *   authored for THIS arrangement as THIS reading assesses it: the autonomy
 *   core contributes near-zero extraction, while the corporate market layer
 *   and residual third-party harm exposure carry the measurable extraction.
 *   Claimed_type and metrics were authored independently: I claim
 *   tangled_rope because the structure shows both a genuine coordination
 *   function and asymmetric extraction under active enforcement, and I
 *   authored metrics at descriptively honest values without tuning either to
 *   predicted engine output.
 *
 * KEY AGENTS:
 *   - state_regulators_and_courts: agenda-setting administrator (institutional/constrained) — wields the enforcement apparatus while being fenced by the same boundary it polices
 *   - licensed_commercial_producers: primary beneficiary and receipt seat (powerful/arbitrage) — captures market gains, lobbies the boundary's levers
 *   - competent_adult_users: protected autonomous class (moderate/constrained) — exited the punishable set entirely
 *   - impaired_road_users: primary third-party payer (organized/trapped) — bears uncompensated crash-risk exposure
 *   - household_secondhand_exposure_bearers: unprotected third-party payer (powerless/trapped) — absorbs lawful-use externalities at home
 *   - taxpayers_funding_treatment_systems: diffuse payer (moderate/constrained) — covers externality costs beyond excise receipts
 *   - relatives_lacking_compulsory_treatment_channel: excluded voice (moderate/trapped) — no seat in drawing or administering the line
 *   - constitutional_policy_analysts: analytical observer (analytical/analytical) — comparative tracking, no stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__legalization_reading, 0.41).
domain_priors:suppression_score(substance_control_legitimacy__legalization_reading, 0.35).
domain_priors:theater_ratio(substance_control_legitimacy__legalization_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, extractiveness, 0.41).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(substance_control_legitimacy__legalization_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__legalization_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__legalization_reading, "Adult Autonomy Boundary over Substance Use (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__legalization_reading, '2667a32a-c473-4857-b3bf-7e8835f43d0d').
narrative_ontology:cs_kernel_codification('2667a32a-c473-4857-b3bf-7e8835f43d0d', distributed).
narrative_ontology:cs_authority_grounding('2667a32a-c473-4857-b3bf-7e8835f43d0d', distributed).
narrative_ontology:cs_reading_relation('2667a32a-c473-4857-b3bf-7e8835f43d0d', substance_control_legitimacy__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('2667a32a-c473-4857-b3bf-7e8835f43d0d', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('2667a32a-c473-4857-b3bf-7e8835f43d0d', foundational, state_may_not_punish_self_regarding_conduct).
narrative_ontology:cs_axiom_status(state_may_not_punish_self_regarding_conduct, holdable).
narrative_ontology:cs_axiom_grounding('2667a32a-c473-4857-b3bf-7e8835f43d0d', state_may_not_punish_self_regarding_conduct, deontological).
narrative_ontology:cs_axiom('2667a32a-c473-4857-b3bf-7e8835f43d0d', secondary, paternalist_coercion_is_illegitimate).
narrative_ontology:cs_axiom_status(paternalist_coercion_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('2667a32a-c473-4857-b3bf-7e8835f43d0d', paternalist_coercion_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('2667a32a-c473-4857-b3bf-7e8835f43d0d', millian_self_other_boundary).
narrative_ontology:cs_drift_state('2667a32a-c473-4857-b3bf-7e8835f43d0d', contemporary_commercial_market_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2667a32a-c473-4857-b3bf-7e8835f43d0d', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__legalization_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, competent_adult_users).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__legalization_reading, licensed_commercial_producers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, impaired_road_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, household_secondhand_exposure_bearers).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, taxpayers_funding_treatment_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__legalization_reading, licensed_commercial_producers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Licenses producers and retailers, sets impairment thresholds and potency rules, runs compliance inspections, and prosecutes impaired driving. Collects earmarked excise revenue and routes it to treatment and safety programs. The same statutes that create this administration also fence it off from private consumption choices: the private act of a competent adult is outside its writ, and courts police that fence. Its discretion is bounded by the very line it administers; stepping past the line costs it legitimacy in the courts.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, state_regulators_and_courts, agenda_setter,
    institutional, generational, constrained, national).

% Buy and consume legal products behind age gates, purchase limits, and public-use bans. The private act itself requires no permission and carries no criminal exposure. Some users develop dependency that narrows their practical ability to walk away from the market; the settled majority simply lives under the arrangement. Moving to a jurisdiction with different rules is possible but costly.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, competent_adult_users, beneficiary,
    moderate, biographical, constrained, national).

% Grow, manufacture, brand, and retail inside license caps and track-and-trace requirements. Profits scale with market breadth and product potency; advertising limits, tax rates, and license availability are the levers they lobby hardest on. Compliance spending, licensing fees, and excise taxes flow out of gross margins. Multi-jurisdiction structures let them shift capital and product lines when any single government tightens.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, licensed_commercial_producers, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__legalization_reading, licensed_commercial_producers, payer).

% Share roads with drivers whose impairment is chemically real but imperfectly testable at the roadside. They bear crash risk generated by other people's private consumption choices, made whole only after the fact through liability and prosecution. Organized advocacy groups push checkpoint funding, per se limits, and better testing technology; nobody can opt out of the shared road.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, impaired_road_users, payer,
    organized, biographical, trapped, national).

% Children and non-using partners breathe smoke and vapor in homes where use is lawful, and absorb caregiving instability when a householder's use escalates. They consented to none of it and can invoke no remedy against the lawful private act itself; state intervention arrives only at demonstrated danger to the child. Leaving means leaving home.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, household_secondhand_exposure_bearers, payer,
    powerless, generational, trapped, local).

% Fund emergency care, dependency treatment, and family-services caseloads from general revenue, while earmarked excise receipts cover only part of the load. Their exposure scales with market breadth and potency trends. Voice runs through budget politics and ballot initiatives; there is no individual opt-out short of emigration.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, taxpayers_funding_treatment_systems, payer,
    moderate, generational, constrained, national).

% Watch adult kin decline into dependency with no lawful lever to compel assessment or treatment, because the boundary reserves that decision to the user alone. Their available instruments are persuasion, guardianship in narrow incapacity cases, and waiting for a crisis that triggers an emergency hold. They were not in the room when the line was drawn and have no institutional seat in its administration.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, relatives_lacking_compulsory_treatment_channel, excluded,
    moderate, biographical, trapped, local).

% Track how courts, legislatures, and ballot processes allocate authority between private choice and state protection across adopting jurisdictions, and publish comparative assessments of outcomes. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__legalization_reading, constitutional_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__legalization_reading, licensed_commercial_producers).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__legalization_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a publicly adjudicable line telling state institutions when coercive intervention in substance use is warranted: only against conduct that threatens others. This lets courts settle police-power disputes consistently, lets enforcement budgets concentrate on impaired driving and distribution violations instead of possession cases, and gives producers a stable licensing frame in place of an illicit market.
% TRANSFER_FUNCTION: Moves enforcement capacity away from policing private consumption and toward third-party harm enforcement; moves market profits to licensed commercial operators; moves excise revenue from purchasers to public treasuries; and leaves residual third-party harm costs — crashes, secondhand exposure, treatment loads — with uninvolved bystanders and general-revenue budgets.
% ABSENT_VOICES: Relatives who want a compelled-assessment channel for dependent kin would object that the boundary treats their households as the price of autonomy; they sit outside legislative hearings, which are dominated by industry coalitions and civil-liberties coalitions. Communities that absorbed the previous enforcement regime, and international drug-control bodies committed to supply reduction, likewise have no seat in adopting jurisdictions.
% DISAPPEARANCE_RATIONALE: If the autonomy boundary vanished overnight, criminal justice would reabsorb millions of possession cases, licensed markets would collapse back into illicit supply chains, invested capital would flee or convert, excise-dependent budgets would lose their streams, and users would rearrange acquisition and consumption around enforcement evasion.
% FOUNDING_PROBLEM: The overcriminalization of private moral conduct: prohibition filled prisons with possession cases, enriched illicit suppliers with adulterated product, concentrated enforcement on poor and minority communities, and failed to suppress use itself — a legitimacy crisis for punishing self-regarding acts.
% FOUNDING_PROBLEM_CORROBORATION: Arrest and incarceration datasets compiled by sentencing-reform researchers, civil-liberties litigation records, and academic criminology outside the benefiting parties attest that the founding overcriminalization problem was real and that adopting jurisdictions reduced it. Public-health agencies and traffic-safety researchers dispute the companion claim that the problem is closed: they document rising presentation of high-potency products and persistent impaired-driving fatalities, so no outside source attests that the founding problem is fully dead.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__legalization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__legalization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__legalization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__legalization_reading, 0.41, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__legalization_reading_tests).
:- end_tests(substance_control_legitimacy__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction 0.41: the autonomy core is a shield, not a pump, but the legal market converts a formerly informal activity into a taxed, branded, potency-escalating industry whose externality tail (treatment loads, crash costs, secondhand exposure) lands outside the transaction. Suppression 0.35 reflects raw structural coercion — DUI checkpoints, age gates, advertising bans, license revocation — in an arrangement where user-side alternatives otherwise stay wide open; suppression is authored as an unscaled structural property, and the engine owns any context scaling. Theater 0.22: licensing, testing, and enforcement are functional, but warning labels and responsibility campaigns increasingly serve liability positioning. Accessibility_collapse 0.38: rivals to the arrangement remain politically alive and home-production or abstention remain available, so alternatives are narrowed, not closed. Resistance 0.48: prohibitionist movements, public-health hardliners, and industry anti-tax lobbying all contest pieces of it. Measurements run on one shared grid (points 0,4,8,12,16,20,24) for all tracked metrics, per the alignment rule. suppression_requirement is authored deliberately because enforcement capacity genuinely changed shape over the interval: a rapid regulatory and roadside-testing build-out in the first half, plateauing once the apparatus matured — this is an enforcement-infrastructure trajectory, not merely shifting extraction. Trajectories are monotonic; no cyclical reinforcement mechanism is asserted.
 *
 * PERSPECTIVAL GAP:
 *   The producer seat experiences the boundary as an enabling framework — the thing that converted risk into a bankable asset class. The road-user and household seats experience the same boundary as a decision that their exposure is the accepted price of strangers' autonomy, compensable only after injury. The regulator seat experiences a bounded mandate: empowered to act exactly where the line permits, disabled where it does not. The analyst seat sees all three simultaneously. These seats will classify differently from the same structural data, and the divergence is the finding, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low d: users (autonomy subsidized, no transfer paid) and producers (market access granted). Payers map to high d: road users and household exposure bearers carry uncompensated harm, taxpayers carry the externality tail. Two directionality overrides are declared where the derivation chain would err. First, institutional -> 0.45: the agenda-setter derives toward beneficiary because it administers the system and collects excise revenue, but the substantive content of this constraint is a LIMIT on that same agent's authority — the state surrenders intervention prerogatives as the arrangement's core payment — leaving it near-symmetric rather than subsidized. Only the regulator seat carries the institutional atom, so the override lands on it alone. Second, organized -> 0.70: organized victims (road users) derive toward the full-target pole, but DUI enforcement, per se limits, and safety funding return part of the burden as protection, damping them below full-target while keeping them clearly on the paying side. Household exposure bearers (powerless) get no such damping and correctly derive near full-target.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both directions of mislabeling. Reading the arrangement as pure rope would hide the corporate layer — a legal market whose gains concentrate with license holders while crash risk, secondhand exposure, and treatment loads socialize; the tangled-rope structure keeps both faces visible and demands the enforcement fact (active licensing, testing, and prosecution machinery) be named. Reading it as a snare would misplace the victim set: under this reading's own structure, users have exited the punishable class entirely, and naming them victims would import a rival reading's referent into this file. On genealogy: the founding problem (overcriminalization) is contested rather than dead — substantially addressed in adopting jurisdictions per outside corroboration, unresolved elsewhere and in potency trends — so the arrangement's center of gravity has shifted from decriminalization toward market governance without its mandate expiring. The status-contested x world-rearranges pairing signals a living, depended-upon arrangement rather than a zombie mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_substance_legitimacy,
    'Is the autonomy/harm-to-others criterion the correct characterization of the operative legitimacy structure for substance control, or do the prohibition_reading (inherent-harm criminalization) and harm_reduction_reading (population harm-minimization) better describe how authority actually allocates?',
    'Cross-family comparison: compile the sibling stories'' epsilon, victim sets, and computed classifications, and test which reading''s predicted enforcement perimeter matches observed statute and case-law allocation across adopting jurisdictions.',
    'If the prohibition reading captures the operative structure, the victim set re-expands to users themselves and measured extraction of the standing arrangement rises sharply; if the harm-reduction reading does, the boundary migrates to aggregate-harm management and compulsory-intervention tools re-enter the legitimate set. This story''s classification holds only under the autonomy criterion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_substance_legitimacy, conceptual, 'Committer-frame omega: this constraint is one reading of the substance_control_legitimacy kernel; sibling readings relocate the victim set and redraw the state-authority perimeter.').

omega_variable(
    corporate_layer_rent_vs_coordination_cost,
    'How much of the corporate layer''s margin is genuine rent capture versus the necessary price of a regulated market (testing, track-and-trace, compliance staffing, license scarcity as quality control)?',
    'Compare legal-market price premia and margins against comparable unregulated adjacent goods, and audit regulatory-cost pass-through in licensee financial disclosures obtained through oversight processes.',
    'If margins exceed auditable compliance costs by a wide factor, the corporate layer is extraction riding on the autonomy settlement and tighter tax or advertising remedies are warranted; if margins roughly track compliance cost, part of the measured extraction is the coordination price itself and the tangled-rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corporate_layer_rent_vs_coordination_cost, empirical, 'Whether the licensed market''s gains are rents or the cost of regulation.').

omega_variable(
    net_third_party_harm_trajectory,
    'Does the legalized regime actually reduce net third-party harm — traffic fatalities adjusted for exposure, child exposure incidents, treatment-system overflow — relative to prohibition-era baselines, or does availability and potency growth offset enforcement gains?',
    'Panel natural experiments across staggered adopting jurisdictions with matched controls, separating substitution effects (alcohol displacement) from additive use.',
    'If net third-party harm rose, the reading''s own warrant — authority limited to protecting others — is undermined on its own terms and pressure builds toward the harm-reduction sibling''s criterion; if it fell, the boundary''s legitimacy consolidates and the current classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_third_party_harm_trajectory, empirical, 'Whether the arrangement performs on the outcome that grounds its own legitimacy.').

omega_variable(
    involuntary_treatment_boundary_ambiguity,
    'Should the absence of any compelled-assessment channel for dependent adults count as an unregistered harm the boundary imposes on families, or as the correct and intended respect for autonomy?',
    'Preference aggregation through deliberative processes including affected relatives, weighed against clinical evidence on coerced-treatment outcomes; not resolvable by observational data alone.',
    'If families'' predicament registers as imposed harm, the victim set widens beyond third-party physical exposure and extraction rises; if it registers as intended design, the boundary stands as authored and the excluded seat stays commentary-grade.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(involuntary_treatment_boundary_ambiguity, preference, 'Whether the excluded relatives'' predicament is a cost of the arrangement or its point.').

omega_variable(
    coercion_level_resolution_uncertainty,
    'Are the authored level-resolved grid endpoints (individual, organizational, class, structural) faithful to the arrangement, given that several cells rest on conservative judgments where the record is thin?',
    'Enforcement-statistics disaggregation by level (arrest types, inspection counts, litigation volume, initiative spending) at matched time points in adopting jurisdictions.',
    'Material divergence at the structural level would change the entrenchment picture and hence lifecycle-drift dating; divergence at the class level would change the equity assessment of the enforcement refocus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_level_resolution_uncertainty, empirical, 'Uncertainty attached to the leveled coercion-grid endpoint judgments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__legalization_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__legalization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(subs_tr_t4, substance_control_legitimacy__legalization_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(subs_tr_t8, substance_control_legitimacy__legalization_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__legalization_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(subs_tr_t16, substance_control_legitimacy__legalization_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(subs_tr_t20, substance_control_legitimacy__legalization_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__legalization_reading, theater_ratio, 24, 0.22).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__legalization_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(subs_be_t4, substance_control_legitimacy__legalization_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(subs_be_t8, substance_control_legitimacy__legalization_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__legalization_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(subs_be_t16, substance_control_legitimacy__legalization_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement(subs_be_t20, substance_control_legitimacy__legalization_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__legalization_reading, base_extractiveness, 24, 0.41).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__legalization_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(subs_su_t4, substance_control_legitimacy__legalization_reading, suppression_requirement, 4, 0.27).
narrative_ontology:measurement(subs_su_t8, substance_control_legitimacy__legalization_reading, suppression_requirement, 8, 0.31).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__legalization_reading, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(subs_su_t16, substance_control_legitimacy__legalization_reading, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(subs_su_t20, substance_control_legitimacy__legalization_reading, suppression_requirement, 20, 0.35).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__legalization_reading, suppression_requirement, 24, 0.35).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=24
narrative_ontology:measurement(subs_grid_01, substance_control_legitimacy__legalization_reading, accessibility_collapse(class), 0, 0.3).
narrative_ontology:measurement(subs_grid_02, substance_control_legitimacy__legalization_reading, accessibility_collapse(class), 24, 0.34).
narrative_ontology:measurement(subs_grid_03, substance_control_legitimacy__legalization_reading, accessibility_collapse(individual), 0, 0.2).
narrative_ontology:measurement(subs_grid_04, substance_control_legitimacy__legalization_reading, accessibility_collapse(individual), 24, 0.25).
narrative_ontology:measurement(subs_grid_05, substance_control_legitimacy__legalization_reading, accessibility_collapse(organizational), 0, 0.4).
narrative_ontology:measurement(subs_grid_06, substance_control_legitimacy__legalization_reading, accessibility_collapse(organizational), 24, 0.58).
narrative_ontology:measurement(subs_grid_07, substance_control_legitimacy__legalization_reading, accessibility_collapse(structural), 0, 0.18).
narrative_ontology:measurement(subs_grid_08, substance_control_legitimacy__legalization_reading, accessibility_collapse(structural), 24, 0.36).
narrative_ontology:measurement(subs_grid_09, substance_control_legitimacy__legalization_reading, resistance(class), 0, 0.5).
narrative_ontology:measurement(subs_grid_10, substance_control_legitimacy__legalization_reading, resistance(class), 24, 0.42).
narrative_ontology:measurement(subs_grid_11, substance_control_legitimacy__legalization_reading, resistance(individual), 0, 0.08).
narrative_ontology:measurement(subs_grid_12, substance_control_legitimacy__legalization_reading, resistance(individual), 24, 0.1).
narrative_ontology:measurement(subs_grid_13, substance_control_legitimacy__legalization_reading, resistance(organizational), 0, 0.55).
narrative_ontology:measurement(subs_grid_14, substance_control_legitimacy__legalization_reading, resistance(organizational), 24, 0.62).
narrative_ontology:measurement(subs_grid_15, substance_control_legitimacy__legalization_reading, resistance(structural), 0, 0.55).
narrative_ontology:measurement(subs_grid_16, substance_control_legitimacy__legalization_reading, resistance(structural), 24, 0.38).
narrative_ontology:measurement(subs_grid_17, substance_control_legitimacy__legalization_reading, stakes_inflation(class), 0, 0.4).
narrative_ontology:measurement(subs_grid_18, substance_control_legitimacy__legalization_reading, stakes_inflation(class), 24, 0.3).
narrative_ontology:measurement(subs_grid_19, substance_control_legitimacy__legalization_reading, stakes_inflation(individual), 0, 0.15).
narrative_ontology:measurement(subs_grid_20, substance_control_legitimacy__legalization_reading, stakes_inflation(individual), 24, 0.28).
narrative_ontology:measurement(subs_grid_21, substance_control_legitimacy__legalization_reading, stakes_inflation(organizational), 0, 0.5).
narrative_ontology:measurement(subs_grid_22, substance_control_legitimacy__legalization_reading, stakes_inflation(organizational), 24, 0.68).
narrative_ontology:measurement(subs_grid_23, substance_control_legitimacy__legalization_reading, stakes_inflation(structural), 0, 0.25).
narrative_ontology:measurement(subs_grid_24, substance_control_legitimacy__legalization_reading, stakes_inflation(structural), 24, 0.6).
narrative_ontology:measurement(subs_grid_25, substance_control_legitimacy__legalization_reading, suppression(class), 0, 0.38).
narrative_ontology:measurement(subs_grid_26, substance_control_legitimacy__legalization_reading, suppression(class), 24, 0.24).
narrative_ontology:measurement(subs_grid_27, substance_control_legitimacy__legalization_reading, suppression(individual), 0, 0.12).
narrative_ontology:measurement(subs_grid_28, substance_control_legitimacy__legalization_reading, suppression(individual), 24, 0.2).
narrative_ontology:measurement(subs_grid_29, substance_control_legitimacy__legalization_reading, suppression(organizational), 0, 0.38).
narrative_ontology:measurement(subs_grid_30, substance_control_legitimacy__legalization_reading, suppression(organizational), 24, 0.52).
narrative_ontology:measurement(subs_grid_31, substance_control_legitimacy__legalization_reading, suppression(structural), 0, 0.12).
narrative_ontology:measurement(subs_grid_32, substance_control_legitimacy__legalization_reading, suppression(structural), 24, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__legalization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, substance_control_legitimacy__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__legalization_reading, dui_enforcement_and_impairment_standards).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'drug policy legitimacy' under the epsilon-invariance principle. The single public label conflates three structurally distinct constraints — one per reading of the substance_control_legitimacy kernel — with different victim sets, different enforcement perimeters, and different epsilon referents. This file (legalization_reading) authors epsilon 0.41 for the legalized-with-regulation arrangement as the autonomy reading assesses it: users sit outside the victim set, and extraction concentrates in the corporate market layer and residual third-party harm exposure. The prohibition_reading sibling authors a high epsilon against a criminalizing arrangement whose victim set includes users; the harm_reduction_reading sibling authors epsilon against a population-harm-management arrangement. Each member of the family links the others via affects_constraints; the downstream dui_enforcement_and_impairment_standards node carries the third-party-harm machinery this reading relies on for its legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, institutional, 0.45).
constraint_indexing:directionality_override(substance_control_legitimacy__legalization_reading, organized, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
