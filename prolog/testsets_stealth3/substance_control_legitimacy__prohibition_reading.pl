% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__prohibition_reading, []).

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
 *   constraint_id: substance_control_legitimacy__prohibition_reading
 *   human_readable: Criminalization-Based Substance Control Regime (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   SUMMARY: This story instantiates ONE reading of the
 *   substance_control_legitimacy kernel - the prohibition reading, under
 *   which substance use is held inherently harmful and state authority
 *   derives from a moral duty to prevent that harm through criminal law. The
 *   standing arrangement under contest is the criminalization regime itself
 *   (scheduling, user prosecution, supply interdiction, carceral processing);
 *   epsilon is authored for THAT arrangement as this reading assesses it,
 *   never for the health-framed or autonomy-framed alternatives the sibling
 *   readings would install. KEY AGENTS (by structural relationship):
 *   law_enforcement_agencies - agenda-setter and principal collector
 *   (institutional/constrained), anchors budgets and forfeiture on
 *   enforcement volume; elected_prohibition_politicians - agenda-setter with
 *   mobile positions (powerful/mobile); private_prison_operators -
 *   beneficiary via per-diem occupancy (powerful/arbitrage);
 *   illicit_trafficking_organizations - paradoxical beneficiary subsidized by
 *   the scarcity the regime creates, bearing priced-in enforcement losses
 *   (organized/arbitrage); substance_users - primary target, entering the
 *   target set through criminalization itself (powerless/trapped);
 *   overpoliced_low_income_communities - concentrated secondary target
 *   (moderate/trapped); black_market_violence_bystanders - externality
 *   bearers of the market the regime manufactures (powerless/trapped);
 *   harm_reduction_practitioners - excluded seat, barred from the operative
 *   conversation; taxpayers - diffuse fiscal bearers. The claimed_type
 *   (snare) and the metrics are independent authored facts: the claim records
 *   my structural judgment that the coordination story now functions
 *   predominantly as cover for an enforcement economy with concentrated
 *   collectors and identifiable targets; the metrics record the arrangement's
 *   observed operation. Where computed per-seat classifications diverge from
 *   this claim, that divergence is data, not error.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, 0.81).
domain_priors:suppression_score(substance_control_legitimacy__prohibition_reading, 0.84).
domain_priors:theater_ratio(substance_control_legitimacy__prohibition_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, suppression_requirement, 0.84).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(substance_control_legitimacy__prohibition_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_legitimacy__prohibition_reading, "Criminalization-Based Substance Control Regime (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_legitimacy__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__prohibition_reading, '0d955680-6a35-4953-8b80-49cba9d177c0').
narrative_ontology:cs_kernel_codification('0d955680-6a35-4953-8b80-49cba9d177c0', formalized).
narrative_ontology:cs_authority_grounding('0d955680-6a35-4953-8b80-49cba9d177c0', lineage).
narrative_ontology:cs_interpretation_layer_present('0d955680-6a35-4953-8b80-49cba9d177c0').
narrative_ontology:cs_reading_relation('0d955680-6a35-4953-8b80-49cba9d177c0', substance_control_legitimacy__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d955680-6a35-4953-8b80-49cba9d177c0', substance_control_legitimacy__legalization_reading, forecloses).
narrative_ontology:cs_axiom('0d955680-6a35-4953-8b80-49cba9d177c0', foundational, inherent_harm_grounds_preventive_criminalization_duty).
narrative_ontology:cs_axiom_status(inherent_harm_grounds_preventive_criminalization_duty, holdable).
narrative_ontology:cs_axiom_grounding('0d955680-6a35-4953-8b80-49cba9d177c0', inherent_harm_grounds_preventive_criminalization_duty, empirically_contingent).
narrative_ontology:cs_axiom('0d955680-6a35-4953-8b80-49cba9d177c0', secondary, user_culpability_presumption).
narrative_ontology:cs_axiom_status(user_culpability_presumption, holdable).
narrative_ontology:cs_axiom_grounding('0d955680-6a35-4953-8b80-49cba9d177c0', user_culpability_presumption, deontological).
narrative_ontology:cs_reference_frame('0d955680-6a35-4953-8b80-49cba9d177c0', duty_based_paternalist_authority).
narrative_ontology:cs_drift_state('0d955680-6a35-4953-8b80-49cba9d177c0', contemporary_overdose_and_liberalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0d955680-6a35-4953-8b80-49cba9d177c0', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__prohibition_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, private_prison_operators).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, elected_prohibition_politicians).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__prohibition_reading, illicit_trafficking_organizations).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, substance_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, overpoliced_low_income_communities).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, black_market_violence_bystanders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, illicit_trafficking_organizations).
narrative_ontology:constraint_victim(substance_control_legitimacy__prohibition_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the enforcement side of drug control: narcotics units, multi-jurisdiction task forces, interdiction programs. Drug enforcement anchors budget lines, staffing levels, federal grant eligibility, and asset-forfeiture revenue; seizure totals feed annual reporting and congressional testimony. Agencies lobby legislatures to maintain scheduling structures and sentence lengths. Exit would mean re-tasking specialized units, unwinding headcount-based union agreements, and surrendering forfeiture revenue - possible in principle, organizationally expensive, so agencies defend the current frame in every budget cycle.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies, beneficiary).

% Sponsor scheduling bills, mandatory-minimum statutes, and enforcement appropriations; campaign on seizure statistics and enforcement records, drawing endorsements and contributions from law-enforcement unions. Electoral cycles keep horizons short, and demonstrated position reversals after voter shifts on cannabis show the seat can change stances at low personal cost - the arrangement is defended while it polls well, not on principle.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, elected_prohibition_politicians, agenda_setter,
    powerful, biographical, mobile, national).

% Sell bed capacity to corrections systems under per-diem contracts; drug offenses supply a large share of occupancy. Revenue scales with admissions and sentence length, creating contractual interest in enforcement intensity; operators lobby for sentencing legislation and against early-release measures. Capital is mobile - contract portfolios can shift toward immigration detention or other lines if drug enforcement shrinks.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, private_prison_operators, beneficiary,
    powerful, biographical, arbitrage, national).

% Supply prohibited markets. Prohibition sets their price structure: artificial scarcity yields margins unavailable in licit commodity trade, and enforcement removes rivals and raises entry barriers, concentrating market share among surviving organizations. Leadership imprisonment is a recurring cost, absorbed through succession pipelines. Organizations diversify across substances and corridors and relocate production faster than any government party adapts, giving them the widest maneuvering room of anyone in the arrangement. They have documented interests against liberalization that would collapse premium pricing.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, illicit_trafficking_organizations, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__prohibition_reading, illicit_trafficking_organizations, payer).

% Possess and consume controlled substances. Exposure runs through arrest, prosecution, conviction records, custodial sentences including felony classifications, probation supervision and testing regimens, then record-based exclusion from housing, employment, licensing, and child custody proceedings. Dependence narrows practical options further; the criminal category travels with the person across jurisdictions through records databases. Ceasing use is medically difficult for dependent users, and record-clearing pathways are slow and partial.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, substance_users, payer,
    powerless, biographical, trapped, national).

% Live where enforcement concentrates: elevated stop rates, raid frequency, and loitering and curfew enforcement layered on top of drug statutes. Costs include removal of working-age residents to incarceration, lost household income, record-based exclusion of returning members, and degraded police-community trust that suppresses cooperation against violent crime. Residential relocation is limited by income and by loss of support networks; collective voice runs through congregations and advocacy groups with modest legislative traction.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, overpoliced_low_income_communities, payer,
    moderate, generational, trapped, regional).

% Reside along distribution corridors and consume unregulated supply. Exposure includes crossfire during market disputes, wrong-address raids, and fatal poisoning from contaminated product (synthetic-opioid-adulterated stimulants) sold with no labeling, dosage standard, or recourse. They chose neither the market nor its policing and have no channel to either.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, black_market_violence_bystanders, payer,
    powerless, immediate, trapped, regional).

% Operate or propose syringe services, naloxone distribution, drug-checking, and supervised consumption facilities. Core tools are criminalized in many jurisdictions through paraphernalia statutes and site bans; federal authorities have blocked or delayed facility authorization. Practitioners testify and litigate but sit outside the operative policy conversation wherever the prohibition frame holds office - their outcome evidence is received as anecdote rather than standard.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, harm_reduction_practitioners, excluded,
    moderate, biographical, constrained, continental).

% Fund the apparatus through appropriations: policing budgets, court dockets, jail and prison per-diems, probation systems, and drug-court programs, receiving the promised protection in return. Fiscal exposure is diffuse and involuntary; oversight runs through budget cycles in which enforcement line items rarely lose to proposed alternatives.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__prohibition_reading, taxpayers, payer,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__prohibition_reading, law_enforcement_agencies).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels society's response to genuinely dangerous substances through a single criminal-legal apparatus: defines prohibited substance classes, centralizes supply interdiction, sets uniform boundaries of lawful conduct around intoxicants, and provides a legal instrument against trafficking organizations that operate through violence.
% TRANSFER_FUNCTION: Moves liberty, money (fines, fees, forfeiture), labor (incarcerated workforces), and civic standing (records) from users, overpoliced communities, and corridor residents into enforcement budgets, per-diem contractor revenue, political capital, and black-market price premiums.
% ABSENT_VOICES: The people the policy governs - current and former users, returning citizens, corridor residents - are largely absent from drafting tables; harm-reduction clinicians are formally excluded from many legislatures by rule or by refusal of standing. Their objections (recorded in testimony archives and litigation filings) exist but carry no vote.
% DISAPPEARANCE_RATIONALE: Overnight repeal would rearrange the landscape: black-market premiums would collapse, dissolving the violence economics that accompany artificial scarcity; hundreds of thousands of open cases and supervisions would evaporate; carceral budgets and contractor portfolios would shrink, forcing reallocation fights; treatment and housing demand would surge faster than current capacity; and international treaty partners would confront a rewritten compliance environment.
% FOUNDING_PROBLEM: Late-nineteenth-century patent-medicine opiates and unregulated cocaine markets produced visible addiction, poisoning deaths, and elite alarm that intoxication was dissolving industrial discipline and family order; the temperance-to-Harrison-Act-to-Controlled-Substances-Act line of arrangements was built to suppress supply and mark the user as a culpable actor whom criminal process could correct.
% FOUNDING_PROBLEM_CORROBORATION: CDC and WHO mortality series corroborate from outside the enforcement complex that substance-related harm is real and currently worsening (overdose deaths); historians of the temperance and Harrison Act eras document the moral-panic component of the founding genealogy; The Lancet's public-health commissions and subsequent epidemiology dispute that criminalization addresses the harm. No source outside the beneficiary set attests that criminalization specifically is solving the founding problem - efficacy claims originate almost entirely from the enforcing institutions themselves.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__prohibition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__prohibition_reading, 0.81, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.81: even granting this reading's own premise (use is harmful, prevention a duty), the operating arrangement transfers from governed parties to collecting institutions far beyond what duty-fulfillment requires - user prosecution, forfeiture, per-diem contracting, and premium-priced black markets all route value to identifiable seats, and the reading's credited offset (prevented harm) is empirically thin against overdose and violence series that worsened under enforcement escalation. Suppression 0.84: criminalization is definitionally coercive (arrest, custody, supervision) and the regime additionally suppresses rival approaches - paraphernalia statutes against syringe services, bans on supervised consumption sites, federal pressure against state legalization - so the figure covers both direct penal force and exit-blocking; suppression is authored as a raw structural property, unscaled, per the framework rule. Theater_ratio 0.42: a substantial minority of activity is performative - school assemblies with weak evidence bases, staged seizure displays, drug-free-zone signage, announcement-driven operations - alongside genuinely functional interdiction and treatment; the end-state scalar equals the final series point. Accessibility_collapse 0.48: alternatives have NOT fully collapsed - dozens of jurisdictions run legal cannabis markets, Portugal decriminalized in 2001, and reform ballots repeatedly pass - but individual exit from the criminalized category remains hard and jurisdiction-level exit draws federal counterpressure, including the 2024 recriminalization drift in Oregon. Resistance 0.62: sustained and partly successful (sentencing reform legislation, clemency waves, medical-body consensus statements, legalization referenda), short of dominant. MEASUREMENT GRID: interval year 0 = 1970 (Controlled Substances Act); t=11 ~ 1981 escalation onset; t=22 ~ 1992 post-crack-era statutory peak; t=33 ~ 2003 forfeiture expansion and incarceration plateau; t=44 ~ 2014 reform wave; t=55 ~ 2025 synthetic-opioid-era hardening. All three tracked series share this one grid; the suppression_requirement series is authored deliberately (not defaulted) because the story's narrative specifically tracks enforcement-capacity change: ratchet (1970-2003), partial rollback under reform (to 2014), renewed hardening (to present). Extractiveness dips at t=44 with reform-era relief and rises again as fentanyl-era enforcement intensifies; theater peaks mid-interval when spectacle enforcement dominated and settles slightly as functional interdiction regained share.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute different constraint types from identical structure. From the agenda-setter seat (agencies, politicians), the arrangement presents as duty faithfully administered: every arrest is a harm prevented, budgets are the cost of protection - a coordination frame. From the payer seats, the same operations present as targeted coercion: the user experiences the criminal category as a life-long status; the corridor resident experiences the market's violence and the policing of it as twin impositions nobody asked them about. From the excluded seat (harm-reduction clinicians), the arrangement presents as evidence-suppression: functioning alternatives barred so the dominant frame need not compete. From the paradoxical beneficiary seat (trafficking organizations), the regime is a subsidy program with occasional audits. The engine computes these divergences from power, exit, and directional position; this story authors the structure and declines to reconcile the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive derivation: law_enforcement_agencies, private_prison_operators, elected_prohibition_politicians, and illicit_trafficking_organizations sit near the beneficiary pole (low d, damped or inverted effective extraction); substance_users, overpoliced_low_income_communities, and black_market_violence_bystangers... (see corrected name below) sit near the target pole, with trapped exit pushing users and communities toward the full-target end since neither records nor residence can be exited. One override is authored: illicit_trafficking_organizations derive near-pure-beneficiary directionality from their beneficiary declaration (roughly 0.1), which understates their position - leadership imprisonment is a real recurring loss - so an override at the organized power atom sets d to 0.3, keeping them net-subsidized (the scarcity premium dwarfs enforcement losses) while registering the borne cost. Taxpayers are left to derivation rather than override because an override at the moderate atom would misstate the other moderate seats; their diffuse fiscal bearing is recorded in commentary instead. Vindicated propositions (inherent_harm_doctrine, criminal_deterrence_of_use_hypothesis) are listed separately from beneficiaries: doctrines collect no rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits: its moral-panic component (intoxication as dissolving sin requiring marked culprits) is dead as a defensible formulation - historians and public-health bodies attest this from outside the beneficiary set - while its material component (real, worsening overdose harm) is tragically live. The arrangement persists on the second while drawing legitimacy from the first. Classification guards against two opposite errors: reading the regime as pure rope launders the enforcement economy as care (the coordination story is now majority cover, per the theater trajectory), and reading it as extraction-with-zero-function ignores the slim irreducible core (violent-organization interdiction, youth-access control) that would survive user-decriminalization - tracked as a live omega rather than asserted. Mandatrophy is NOT declared resolved: the enforcement mandate is still actively executed, not merely maintained theatrically; what has outlived function is the moral-panic justification, not the apparatus. The R5 status (contested) times the Q5 verdict (world_rearranges) correctly: the world depends on the arrangement, and the parties dispute whether that dependence still buys what it was built to buy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This story is one reading (prohibition_reading) of the substance_control_legitimacy kernel; what structurally changes under the sibling readings harm_reduction_reading and legalization_reading, and where exactly does the disagreement bite?',
    'Compile the sibling files from the same kernel and diff the structural outputs: victim-set membership (users criminalized here, patient/rights-bearer status there), the legitimacy-source axiom (affirmative preventive duty vs harm-minimization mandate vs third-party-harm limit), and resulting per-seat classifications.',
    'Sibling authorships will assign users out of the victim set and likely lower reading-indexed epsilon over the same referent; cross-reading chi comparison separates how much measured extraction is referent-intrinsic versus reading-indexed, and tests whether the forecloses edge to legalization_reading computes as the engine predicts from axiom contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: this file is one of three sibling readings; disagreement located in the source-of-authority axiom and consequent victim-set membership.').

omega_variable(
    governance_core_separability,
    'Is there an irreducible governance core - interdiction against violent trafficking organizations, youth-access limits, control over catastrophically potent supply chains - separable from criminalization of users?',
    'Compare regimes that decriminalized users while retaining supply controls (Portugal 2001 onward, Dutch tolerance policy): if overdose, youth-access, and organized-violence outcomes hold while user-side carceral load collapses, the core is separable from user criminalization.',
    'Separable: the user-criminalization layer is removable without losing the coordination function, and the arrangement reads as a slim-core coordination wrapped in a large extraction economy. Inseparable: part of the measured extraction is the irreducible price of governing genuinely dangerous supply, and the snare reading overstates by that share.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_core_separability, empirical, 'Whether the coordination function survives removal of user criminalization - the tangled-rope boundary question.').

omega_variable(
    inherent_harm_naturality,
    'Is ''substance use is inherently harmful'' a uniform natural-kind fact, or a heterogeneous dose-, substance-, and context-dependent profile presented as fixed nature?',
    'Pharmacological and epidemiological synthesis across the harm spectrum: lethal-dose ratios and dependence profiles span orders of magnitude from caffeinated beverages to synthetic opioids; if the profile is heterogeneous, uniform criminal authority over ''use'' as a class lacks its factual foundation.',
    'Heterogeneity collapses the naturality premise that legitimizes the regime''s mountain-like self-presentation, exposing it as constructed policy serving identifiable enforcers (the false-summit direction); a confirmed uniform-harm finding would stabilize the reading''s legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_harm_naturality, empirical, 'Whether the foundational harm premise is natural fact or naturalized heterogeneity.').

omega_variable(
    counterfactual_regime_epsilon,
    'How much of the measured extraction is attributable to the criminalization instrument itself rather than to substance harm under any governance regime?',
    'Natural experiments: Portugal 2001 decriminalization, US state cannabis legalization from 2014, Canada 2018 - tracking carceral load, overdose mortality, market violence, and street prices against matched non-reform jurisdictions.',
    'Large deltas attribute extraction to the instrument (supporting the snare reading and raising confidence in the classification); small deltas attribute it to substances generally, supporting the reading''s own defense that costs reflect the harm being prevented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_regime_epsilon, empirical, 'Instrument-attributable versus substance-intrinsic share of measured extraction.').

omega_variable(
    violence_externality_attribution,
    'Is black-market violence intrinsic to artificially scarce prohibited markets (the alcohol-prohibition pattern) or intrinsic to the pharmacology of the substances themselves?',
    'Cross-substance comparison under constant prohibition intensity: if violence rates track market premium and enforcement pressure rather than pharmacological class, the externality is manufactured by the arrangement; if class predicts violence independent of market structure, it is not.',
    'Prohibition-side attribution books the violence externality into the arrangement''s own account, raising effective extraction on target-adjacent seats; pharmacological attribution lets the reading credit the regime with containing violence that would otherwise occur.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_externality_attribution, empirical, 'Attribution of the black-market violence externality between the arrangement and the substances.').

omega_variable(
    victim_coalition_feasibility,
    'Users, overpoliced communities, corridor residents, and offenders'' families hold interests opposed to every collector seat; why has no durable counter-coalition formed, and under what conditions could one?',
    'Organizational analysis of the fragmentation mechanisms: stigma split between ''criminal'' and ''patient'' identities, felony disenfranchisement removing the most affected voters, geographic dispersal of concentrated harm, and foundation funding substituting for member-financed organization.',
    'A durable coalition converts dispersed payer positions into organized countervailing power, raising achievable resistance and lowering sustainable suppression - potentially dating a type transition; persistent fragmentation locks the current configuration in place indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_coalition_feasibility, empirical, 'Feasibility conditions for coalition power among the regime''s dispersed targets.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__prohibition_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__prohibition_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subs_tr_t11, substance_control_legitimacy__prohibition_reading, theater_ratio, 11, 0.33).
narrative_ontology:measurement(subs_tr_t22, substance_control_legitimacy__prohibition_reading, theater_ratio, 22, 0.43).
narrative_ontology:measurement(subs_tr_t33, substance_control_legitimacy__prohibition_reading, theater_ratio, 33, 0.47).
narrative_ontology:measurement(subs_tr_t44, substance_control_legitimacy__prohibition_reading, theater_ratio, 44, 0.44).
narrative_ontology:measurement(subs_tr_t55, substance_control_legitimacy__prohibition_reading, theater_ratio, 55, 0.42).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__prohibition_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(subs_be_t11, substance_control_legitimacy__prohibition_reading, base_extractiveness, 11, 0.66).
narrative_ontology:measurement(subs_be_t22, substance_control_legitimacy__prohibition_reading, base_extractiveness, 22, 0.76).
narrative_ontology:measurement(subs_be_t33, substance_control_legitimacy__prohibition_reading, base_extractiveness, 33, 0.8).
narrative_ontology:measurement(subs_be_t44, substance_control_legitimacy__prohibition_reading, base_extractiveness, 44, 0.74).
narrative_ontology:measurement(subs_be_t55, substance_control_legitimacy__prohibition_reading, base_extractiveness, 55, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__prohibition_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(subs_su_t11, substance_control_legitimacy__prohibition_reading, suppression_requirement, 11, 0.68).
narrative_ontology:measurement(subs_su_t22, substance_control_legitimacy__prohibition_reading, suppression_requirement, 22, 0.82).
narrative_ontology:measurement(subs_su_t33, substance_control_legitimacy__prohibition_reading, suppression_requirement, 33, 0.86).
narrative_ontology:measurement(subs_su_t44, substance_control_legitimacy__prohibition_reading, suppression_requirement, 44, 0.72).
narrative_ontology:measurement(subs_su_t55, substance_control_legitimacy__prohibition_reading, suppression_requirement, 55, 0.84).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__prohibition_reading, legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'drug policy legitimacy' decomposes, per the epsilon-invariance principle, into three reading-stories of the substance_control_legitimacy kernel. Each file carries ONE stable epsilon over the FIXED referent (the standing criminalization arrangement) with reading-indexed values: this prohibition reading authors 0.81, crediting partial duty-fulfillment offsets that the siblings do not credit; harm_reduction_reading strips the credited benefit and reframes targets as patients; legalization_reading weights the autonomy violation. The upstream/downstream pressure between readings runs through scheduling precedent and treaty commitments, which the prohibition reading froze and the siblings must amend. Edges here carry cross-reading comparison; no reading's contest is folded inside another file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(substance_control_legitimacy__prohibition_reading, organized, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
