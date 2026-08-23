% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__prohibition_reading, []).

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
 *   constraint_id: substance_control_kernel__prohibition_reading
 *   human_readable: Criminal Prohibition of Substance Use — Moral-Transgression/Punitive Reading
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the prohibition reading of the substance-control
 *   kernel: the commitment that substance use is a moral transgression and
 *   that state punishment is the fitting response, defended as protection of
 *   social order. Under this reading the standing arrangement criminalizes
 *   use and possession, runs users through arrest and sentencing, funds an
 *   enforcement sector whose budgets and forfeiture receipts depend on
 *   continued enforcement, and generates an illegal supply chain whose
 *   disputes resolve violently because courts are unavailable to
 *   participants. The epsilon referent is the standing punitive arrangement
 *   itself, described as it actually operates — not the health-based or
 *   liberty-based arrangements the sibling readings would install. KEY AGENTS
 *   (by structural relationship): - criminalized_substance_users: Primary
 *   target (powerless/trapped) — bears arrest, incarceration, and a portable
 *   lifelong record - drug_enforcement_agencies: Primary beneficiary and
 *   agenda setter (institutional/arbitrage) — collects budgets and forfeited
 *   assets, sets enforcement policy - private_prison_operators: Secondary
 *   beneficiary (powerful/arbitrage) — collects per-diem revenue on the
 *   sentenced population - order_campaign_officeholders: Political
 *   beneficiary (institutional/immediate horizon) — converts punitive posture
 *   into electoral capital - overpoliced_minority_neighborhoods: Concentrated
 *   target (moderate/constrained) — bears saturation policing and household
 *   separation - black_market_violence_exposed_residents: Externality-bearing
 *   target (moderate/constrained) — bears the violence the illegal market
 *   generates - harm_reduction_practitioners: Excluded voice
 *   (moderate/constrained) — holds suppressed evidence-based alternative -
 *   public_health_epidemiologists: Analytical observer
 *   (institutional/analytical) — documents harms, commands no agenda power -
 *   court_ordered_treatment_industry: Incidental beneficiary
 *   (organized/constrained) — receives the referral stream punishment creates
 *   - international_narcotics_control_bodies: Global agenda setter
 *   (institutional/arbitrage) — enforces the treaty-level punitive baseline
 *
 * KEY AGENTS:
 *   - criminalized_substance_users: Primary target (powerless/trapped) — bears arrest, incarceration, and a portable lifelong record
 *   - drug_enforcement_agencies: Primary beneficiary and agenda setter (institutional/arbitrage) — collects budgets and forfeited assets, sets enforcement policy
 *   - private_prison_operators: Secondary beneficiary (powerful/arbitrage) — collects per-diem revenue on the sentenced population
 *   - order_campaign_officeholders: Political beneficiary (institutional/immediate horizon) — converts punitive posture into electoral capital
 *   - overpoliced_minority_neighborhoods: Concentrated target (moderate/constrained) — bears saturation policing and household separation
 *   - black_market_violence_exposed_residents: Externality-bearing target (moderate/constrained) — bears the violence the illegal market generates
 *   - harm_reduction_practitioners: Excluded voice (moderate/constrained) — holds suppressed evidence-based alternative
 *   - public_health_epidemiologists: Analytical observer (institutional/analytical) — documents harms, commands no agenda power
 *   - court_ordered_treatment_industry: Incidental beneficiary (organized/constrained) — receives the referral stream punishment creates
 *   - international_narcotics_control_bodies: Global agenda setter (institutional/arbitrage) — enforces the treaty-level punitive baseline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, 0.8).
domain_priors:suppression_score(substance_control_kernel__prohibition_reading, 0.83).
domain_priors:theater_ratio(substance_control_kernel__prohibition_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, suppression_requirement, 0.83).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(substance_control_kernel__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_kernel__prohibition_reading, "Criminal Prohibition of Substance Use — Moral-Transgression/Punitive Reading").
narrative_ontology:topic_domain(substance_control_kernel__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__prohibition_reading, 'e60a8e61-8e65-4266-9ab3-247747f9eaf3').
narrative_ontology:cs_kernel_codification('e60a8e61-8e65-4266-9ab3-247747f9eaf3', fixed_text).
narrative_ontology:cs_authority_grounding('e60a8e61-8e65-4266-9ab3-247747f9eaf3', extraction).
narrative_ontology:cs_interpretation_layer_present('e60a8e61-8e65-4266-9ab3-247747f9eaf3').
narrative_ontology:cs_reading_relation('e60a8e61-8e65-4266-9ab3-247747f9eaf3', substance_control_kernel__harm_reduction_reading, influences).
narrative_ontology:cs_reading_relation('e60a8e61-8e65-4266-9ab3-247747f9eaf3', substance_control_kernel__legalization_reading, forecloses).
narrative_ontology:cs_axiom('e60a8e61-8e65-4266-9ab3-247747f9eaf3', foundational, self_regarding_vice_warrants_state_punishment).
narrative_ontology:cs_axiom_status(self_regarding_vice_warrants_state_punishment, holdable).
narrative_ontology:cs_axiom_grounding('e60a8e61-8e65-4266-9ab3-247747f9eaf3', self_regarding_vice_warrants_state_punishment, deontological).
narrative_ontology:cs_axiom('e60a8e61-8e65-4266-9ab3-247747f9eaf3', secondary, punishment_deters_use_and_preserves_social_order).
narrative_ontology:cs_axiom_status(punishment_deters_use_and_preserves_social_order, holdable).
narrative_ontology:cs_axiom_grounding('e60a8e61-8e65-4266-9ab3-247747f9eaf3', punishment_deters_use_and_preserves_social_order, empirically_contingent).
narrative_ontology:cs_reference_frame('e60a8e61-8e65-4266-9ab3-247747f9eaf3', moralized_punitive_prohibition).
narrative_ontology:cs_drift_state('e60a8e61-8e65-4266-9ab3-247747f9eaf3', post_legalization_wave_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e60a8e61-8e65-4266-9ab3-247747f9eaf3', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__prohibition_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, drug_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, order_campaign_officeholders).
narrative_ontology:constraint_beneficiary(substance_control_kernel__prohibition_reading, court_ordered_treatment_industry).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, criminalized_substance_users).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, overpoliced_minority_neighborhoods).
narrative_ontology:constraint_victim(substance_control_kernel__prohibition_reading, black_market_violence_exposed_residents).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, moral_transgression_doctrine).
narrative_ontology:constraint_vindicates(substance_control_kernel__prohibition_reading, punitive_deterrence_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Federal and local agencies that schedule substances, set interdiction and arrest priorities, and run the pipeline from street-level stops to federal prosecution. Staffing, budgets, and institutional size are justified by enforcement activity counts, and seized assets returned through forfeiture supplement appropriations. If the punitive framework were replaced, personnel and capabilities would transfer to other enforcement domains — the institution is not bound to this particular mandate.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, drug_enforcement_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Companies contracted to house sentenced prisoners at per-diem rates. Revenue scales with the sentenced population; operators lobby legislatures for occupancy guarantees and minimum-sentence provisions. Capital and contracts are mobile — the business model attaches to whatever population the justice system supplies.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, private_prison_operators, beneficiary,
    powerful, biographical, arbitrage, national).

% Legislators and executives who campaign on restoring or defending public order. Punitive substance platforms reliably convert into votes and donations; the officeholder bears none of the frontline costs and can pivot messaging when electorates shift.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, order_campaign_officeholders, beneficiary,
    institutional, immediate, arbitrage, national).

% People who use scheduled substances. Exposure runs from stop-and-search through arrest, sentencing, and a permanent record that blocks employment, housing, and in some jurisdictions the vote. Physiological dependence makes simply stopping unreliable, and relocating beyond the framework's reach is rarely feasible; the record follows across state lines.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, criminalized_substance_users, payer,
    powerless, biographical, trapped, national).

% Residential areas where enforcement activity concentrates regardless of local offense rates. Residents absorb frequent stops, household separation through incarceration, and the chilling effects of saturation patrol. Housing costs and economic ties limit moving away; neighborhood associations can petition but do not set enforcement policy.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, overpoliced_minority_neighborhoods, payer,
    moderate, generational, constrained, regional).

% People living where illegal distribution operates. Because suppliers cannot use courts to settle disputes, disagreements resolve through gunfire; residents bear shootings, stray rounds, and displacement. They chose neither to sell nor to make the trade illegal, and relocation is constrained by income and housing.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, black_market_violence_exposed_residents, payer,
    moderate, biographical, constrained, regional).

% Workers running syringe exchange, naloxone distribution, and supervised consumption services. Across much of the jurisdiction their supplies are treated as contraband and their clients as arrest targets; several programs operate under legal threat. They hold operational evidence about what reduces overdose deaths but have no seat where scheduling and sentencing are decided.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, local).

% Researchers measuring overdose mortality, infection rates, and the health effects of incarceration. Their publications feed legislative testimony and reform litigation, but they command no enforcement or appropriation authority, and enforcement agencies publicly contest findings that undercut the punitive frame.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, public_health_epidemiologists, observer,
    institutional, generational, analytical, global).

% Licensed providers receiving clients through diversion sentences and drug courts. Payment follows the referral pipeline that punishment creates; providers deliver genuine clinical care while their census depends on continued arrest volume. Losing the referral stream would force wholesale business restructuring.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, court_ordered_treatment_industry, beneficiary,
    organized, biographical, constrained, national).

% Treaty secretariats and monitoring boards administering the global narcotics conventions. They review national compliance, censure deviation toward supervised-consumption rooms or legal cannabis markets, and their institutional continuity presupposes the punitive baseline remaining in force.
narrative_ontology:constraint_stakeholder(substance_control_kernel__prohibition_reading, international_narcotics_control_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_kernel__prohibition_reading, drug_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_kernel__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a uniform, enforceable public standard that scheduled substance use is socially unacceptable: employers, parents, schools, and police share one sanctioned norm, and international partners coordinate interdiction through common treaty categories. Whatever else it does, the arrangement does solve the problem of expressing and enforcing a collective moral boundary at scale.
% TRANSFER_FUNCTION: Moves liberty, years of life, and lifetime earnings from convicted users and their households to the enforcement sector (appropriations, staffing, forfeited assets) and to contractors paid per housed prisoner; moves electoral capital to officeholders who campaign on order; moves decision rights over substance policy to enforcement agencies and treaty bodies rather than to users or health authorities.
% ABSENT_VOICES: Current users, formerly incarcerated people (disenfranchised in several jurisdictions), and harm-reduction practitioners have no formal seat in scheduling or sentencing decisions; legislative hearings are dominated by enforcement agencies, prosecutors, and treatment vendors whose revenue rides on the existing design. The people bearing the heaviest costs are structurally furthest from the table that keeps the arrangement in place.
% DISAPPEARANCE_RATIONALE: If the punitive framework vanished overnight, the enforcement sector would shed mandates and staff, hundreds of thousands of sentenced people would move to health-system or liberty tracks, illegal supply chains would transform as disputes became court-adjudicable, treaty bodies would convene emergency revision, and officeholder coalitions built on order politics would scramble for new platforms. Arrangements across policing, courts, prisons, treatment, and foreign policy are organized around this framework's continuation.
% FOUNDING_PROBLEM: Early twentieth-century industrial cities showed visible public intoxication, patent medicines had addicted large numbers to opiates, and political movements fused drug fear with racial panic about minority labor and immigrant opium dens; the arrangement was built to suppress that perceived disorder and defend a moralized social order through criminal sanction.
% FOUNDING_PROBLEM_CORROBORATION: Historians of narcotics control (archival scholarship on the Harrison Act era and the racialized panics behind the early statutes) attest the founding problem from outside the benefiting parties; epidemiologists corroborate that disorder-shaped problems persist in mutated form (fentanyl-era overdose mortality) while disputing that punishment is the remedy. Enforcement agencies also attest a live problem, but they sit inside the beneficiary set, so their attestation cannot serve as independent corroboration.
narrative_ontology:disappearance_verdict(substance_control_kernel__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_kernel__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__prohibition_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_kernel__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_kernel__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.80) because the arrangement's costs concentrate on identifiable classes — sentenced users, saturated neighborhoods, violence-exposed residents — while its gains pool in the enforcement sector and its contractors; the expected structural delta for this reading (users entering the punishable set, enforcement becoming primary beneficiary) is exactly what the number encodes. Suppression (0.83) reflects that persistence runs through active machinery — arrest, sentencing, treaty censure, funding bans on alternatives — not participant preference; suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope (only extractiveness is scaled, by the engine, through directionality and scope). Theater (0.38) is moderate: core enforcement is functionally real, but a growing share of activity is rhetorical display and metric performance. Accessibility_collapse (0.58) sits mid-range: alternatives are visible (Portuguese decriminalization, state cannabis markets) but face treaty and federal barriers, so they collapse only partially. Resistance (0.70) is high and continuous: ballot initiatives, reform litigation, and over-policing backlash meet the framework throughout the interval. All three temporal series share one seven-point grid (interval units roughly 1970–2025), so no metric borrows another's timeline; end-state values equal the base_properties scalars. The trajectories show an enforcement ratchet peaking around year 40 with mild reform-era softening after — a ratchet with late attrition rather than an oscillation, so no intermittent-reinforcement reading is claimed.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. From the enforcement seat the arrangement is a necessary defense of social order it is charged with providing — experienced extraction is low and the framework reads as legitimate governance. From the user and neighborhood seats the same machinery is experienced as domination: punishment without consent, record without remedy, violence without recourse. From the epidemiological seat it is an iatrogenic system producing measurable mortality and incarceration morbidity. The engine derives these divergences from the declared directionalities and exit atoms; the authored snare claim adjudicates nothing about which seat is right — it records that the structure sustains itself through coercion against identifiable losers while paying identifiable winners.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (enforcement agencies, prison contractors, order-platform officeholders, court-ordered treatment vendors) derive low d — the arrangement subsidizes them. Declared victim classes (users, saturated neighborhoods, violence-exposed residents) derive high d — it taxes them. Exit atoms modulate within sides: enforcement agencies and prison operators hold arbitrage-grade exit (capabilities and capital transfer to other mandates and populations), placing them nearest the beneficiary pole; users are trapped (physiological dependence plus a record that follows across jurisdictions), and neighborhood residents are constrained (housing costs and economic ties bind them), pushing both toward the full-target pole. Officeholders benefit indirectly through electoral rents rather than direct receipts, which the derivation prices as low-but-not-minimal d. Epidemiologists hold the analytical seat and feed no extraction arithmetic. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms reproduce the true relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem has mutated rather than died: patent-medicine addiction and the specific racialized panics of the early twentieth century are gone, but overdose mortality and visible street disorder persist in new chemical forms, so the mandate's status is contested rather than dead. Because status is contested and the disappearance verdict is world_rearranges, the dead-mandate mismatch flag does not fire — correctly, since the arrangement still organizes real activity across policing, courts, prisons, treatment, and foreign policy. The drift signal instead appears in the temporal series: theater_ratio climbing from 0.22 to 0.38 marks rhetoric and display (school-program curricula of demonstrated ineffectiveness, seizure press events, quota-driven sweeps) substituting for the original protective function, while extractiveness plateaus at a high level sustained by the enforcement sector's self-funding loop. Mandatrophy is prevented from mislabeling in both directions: the arrangement is not coordination wearing an extraction costume (the victim classes are real and concentrated), nor is it extraction with no coordination residue (a uniform moral norm is genuinely expressed and some deterrent price and availability effects are measurable). The snare reading stands because the coordination story functions as justification while persistence runs through coercion and the active suppression of alternatives — harm-reduction programs barred, legalization experiments preempted, researchers defunded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the substance_control_kernel should govern substance policy — prohibition (this constraint), harm reduction, or legalization?',
    'Jurisdiction-level policy adoption and persistence: observe which reading''s arrangements survive electoral contest, treaty review, and outcome comparison over time.',
    'Sibling readings change the victim set (users become patients or rights-holders), move beneficiary status from the enforcement apparatus to service providers, and drop epsilon sharply; this story''s classification holds only for the punitive instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame indexical: this constraint is one reading of a contested kernel.').

omega_variable(
    punitive_deterrence_efficacy,
    'Does criminal punishment materially reduce substance prevalence or harm, net of black-market substitution effects?',
    'Natural experiments: Portuguese decriminalization, US state cannabis legalization, alcohol prohibition repeal; difference-in-differences on prevalence, youth access, and overdose mortality.',
    'If deterrence is negligible, the arrangement''s coordination-function leg collapses and the classification hardens toward pure extraction; if material, residual hybrid-coordination texture strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(punitive_deterrence_efficacy, empirical, 'Empirical foundation of the punitive axiom.').

omega_variable(
    enforcement_self_funding_causality,
    'Is enforcement intensity driven by the apparatus''s self-funding loop (forfeiture receipts, metric-justified budgets) or by genuine constituent demand for order?',
    'Compare jurisdictions matched on surveyed order-demand but differing in forfeiture-retention rules and budget insulation from enforcement counts.',
    'If the self-funding loop drives intensity, receipt-of-gains is causal and reform requires severing forfeiture; if demand-driven, receipt is symptomatic and electoral change suffices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_self_funding_causality, empirical, 'Whether the enforcement apparatus''s capture of gains causes the arrangement''s persistence.').

omega_variable(
    black_market_violence_attribution,
    'Is black-market violence intrinsic to criminalized supply (contracts unenforceable in court) or an exogenous feature of illicit demand?',
    'Cross-regime comparison of distribution-violence rates for identical substances under different legal statuses; historical alcohol-prohibition violence series.',
    'If intrinsic, violence enters the arrangement''s own cost ledger and raises the burden borne by exposed residents; if exogenous, it is background condition rather than product of the arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(black_market_violence_attribution, empirical, 'Attribution of the secondary externality named in this reading''s structural delta.').

omega_variable(
    user_side_suppression_internalization,
    'Is user-side suppression wholly structural (record, incarceration, surveillance) or partially internalized (stigma that suppresses treatment-seeking even after sanctions lift)?',
    'Post-decriminalization treatment-uptake trajectories: if help-seeking lags sanction removal by years, an internalized component is confirmed.',
    'If internalized, removing criminal sanctions will not promptly restore health-system contact; effective suppression exceeds the structural measure and persists past formal reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_side_suppression_internalization, empirical, 'Structural versus internalized composition of user-side suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__prohibition_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_kernel__prohibition_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(subs_tr_t0, observed).
narrative_ontology:measurement(subs_tr_t10, substance_control_kernel__prohibition_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(subs_tr_t10, observed).
narrative_ontology:measurement(subs_tr_t20, substance_control_kernel__prohibition_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(subs_tr_t20, observed).
narrative_ontology:measurement(subs_tr_t30, substance_control_kernel__prohibition_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(subs_tr_t30, observed).
narrative_ontology:measurement(subs_tr_t40, substance_control_kernel__prohibition_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(subs_tr_t40, observed).
narrative_ontology:measurement(subs_tr_t50, substance_control_kernel__prohibition_reading, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(subs_tr_t50, observed).
narrative_ontology:measurement(subs_tr_t55, substance_control_kernel__prohibition_reading, theater_ratio, 55, 0.38).
narrative_ontology:measurement_basis(subs_tr_t55, observed).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_kernel__prohibition_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(subs_be_t0, observed).
narrative_ontology:measurement(subs_be_t10, substance_control_kernel__prohibition_reading, base_extractiveness, 10, 0.67).
narrative_ontology:measurement_basis(subs_be_t10, observed).
narrative_ontology:measurement(subs_be_t20, substance_control_kernel__prohibition_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(subs_be_t20, observed).
narrative_ontology:measurement(subs_be_t30, substance_control_kernel__prohibition_reading, base_extractiveness, 30, 0.79).
narrative_ontology:measurement_basis(subs_be_t30, observed).
narrative_ontology:measurement(subs_be_t40, substance_control_kernel__prohibition_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(subs_be_t40, observed).
narrative_ontology:measurement(subs_be_t50, substance_control_kernel__prohibition_reading, base_extractiveness, 50, 0.81).
narrative_ontology:measurement_basis(subs_be_t50, observed).
narrative_ontology:measurement(subs_be_t55, substance_control_kernel__prohibition_reading, base_extractiveness, 55, 0.8).
narrative_ontology:measurement_basis(subs_be_t55, observed).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_kernel__prohibition_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(subs_su_t0, observed).
narrative_ontology:measurement(subs_su_t10, substance_control_kernel__prohibition_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(subs_su_t10, observed).
narrative_ontology:measurement(subs_su_t20, substance_control_kernel__prohibition_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement_basis(subs_su_t20, observed).
narrative_ontology:measurement(subs_su_t30, substance_control_kernel__prohibition_reading, suppression_requirement, 30, 0.83).
narrative_ontology:measurement_basis(subs_su_t30, observed).
narrative_ontology:measurement(subs_su_t40, substance_control_kernel__prohibition_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement_basis(subs_su_t40, observed).
narrative_ontology:measurement(subs_su_t50, substance_control_kernel__prohibition_reading, suppression_requirement, 50, 0.85).
narrative_ontology:measurement_basis(subs_su_t50, observed).
narrative_ontology:measurement(subs_su_t55, substance_control_kernel__prohibition_reading, suppression_requirement, 55, 0.83).
narrative_ontology:measurement_basis(subs_su_t55, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_kernel__prohibition_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% Colloquial 'drug policy' conflates three structurally distinct claims decomposed per the epsilon-invariance principle: the punitive/prohibition reading (this file — users in the punishable set, enforcement apparatus as primary beneficiary, high epsilon), the harm-reduction reading (users as patients, service providers as beneficiaries, low epsilon), and the legalization reading (users as rights-holders, state limited to third-party-harm regulation, near-zero punitive epsilon). The prohibition reading currently dominates resource allocation and structurally shapes its siblings' operating environment: it criminalizes harm-reduction tools and workers (an influences edge) and its foundational axiom directly contradicts the legalization reading's harm-principle axiom within any single normative framework (a forecloses edge recorded in cs_structure.reading_relations). Each member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
