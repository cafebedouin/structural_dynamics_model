% ============================================================================
% CONSTRAINT STORY: substance_control_legitimacy__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_legitimacy__harm_reduction_reading, []).

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
 *   constraint_id: substance_control_legitimacy__harm_reduction_reading
 *   human_readable: Harm-Reduction Substance Governance Regime
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This story instantiates the harm_reduction_reading of the
 *   substance_control_legitimacy kernel: the state claims authority over
 *   substance use as a public-health stewardship duty, exercising it through
 *   service provision, monitored treatment, and residual supply enforcement
 *   rather than user criminalization. The standing arrangement under contest
 *   — and therefore the referent of epsilon — is THIS regime as it actually
 *   operates: real service infrastructure delivering measurable mortality
 *   reductions, wrapped around compulsory-treatment machinery and a
 *   health-surveillance apparatus, with an illicit market persisting because
 *   supply remains criminalized. Per the family discipline, the colloquial
 *   label 'drug policy' decomposes into three structurally distinct
 *   constraints (one per reading), each with its own stable epsilon: this
 *   reading's epsilon is moderate and mandate-centered (0.52); the
 *   prohibition reading authors a carceral arrangement with sharply higher
 *   epsilon and a different victim set; the legalization reading authors an
 *   autonomy-bounded arrangement whose mandate surface does not exist. The
 *   three files link through network.affects_constraints; nothing of the
 *   contest is averaged into this one. Claim/metric independence holds:
 *   tangled_rope is stated because the structure carries both a genuine
 *   coordination function and asymmetric extraction under active enforcement,
 *   and the metrics are authored as descriptive facts — neither was tuned
 *   toward a predicted engine output.
 *
 * KEY AGENTS:
 *   - - public_health_agencies: Agenda-setter (institutional/constrained) — writes mandate criteria, funds the network, operates surveillance; budget grows with the mandate perimeter
 *   - - people_who_use_drugs: Primary target (moderate/trapped) — receives life-sustaining services under continuous monitoring; exit means abandoning medications
 *   - - treatment_provider_networks: Primary beneficiary (institutional/constrained) — per-episode revenue scales with referrals; helps draft the criteria that generate them
 *   - - illicit_supply_participants: Residual target (organized/arbitrage) — bears continuing supply enforcement; rotates faster than enforcement adapts
 *   - - general_resident_population: Diffuse beneficiary (moderate/mobile) — cleaner streets, lower transmission, pays the levies
 *   - - families_of_dependent_users: Dual-positioned (powerless/constrained) — conscripted as monitors, protected from bereavement
 *   - - drug_user_unions: Excluded voice (moderate/constrained) — demands voluntariness, holds no mandate-design seat
 *   - - judicial_rights_reviewers: Analytical observer (institutional/analytical) — reviews commitment orders after the fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, 0.52).
domain_priors:suppression_score(substance_control_legitimacy__harm_reduction_reading, 0.42).
domain_priors:theater_ratio(substance_control_legitimacy__harm_reduction_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_legitimacy__harm_reduction_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_legitimacy__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_legitimacy__harm_reduction_reading, "Harm-Reduction Substance Governance Regime").
narrative_ontology:topic_domain(substance_control_legitimacy__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_legitimacy__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_legitimacy__harm_reduction_reading, '56cb6663-3139-4770-86da-10af6f44291c').
narrative_ontology:cs_kernel_codification('56cb6663-3139-4770-86da-10af6f44291c', distributed).
narrative_ontology:cs_authority_grounding('56cb6663-3139-4770-86da-10af6f44291c', expertise).
narrative_ontology:cs_interpretation_layer_present('56cb6663-3139-4770-86da-10af6f44291c').
narrative_ontology:cs_reading_relation('56cb6663-3139-4770-86da-10af6f44291c', substance_control_legitimacy__prohibition_reading, coexists_with).
narrative_ontology:cs_reading_relation('56cb6663-3139-4770-86da-10af6f44291c', substance_control_legitimacy__legalization_reading, influences).
narrative_ontology:cs_axiom('56cb6663-3139-4770-86da-10af6f44291c', foundational, nonpunitive_harm_minimization_duty).
narrative_ontology:cs_axiom_status(nonpunitive_harm_minimization_duty, holdable).
narrative_ontology:cs_axiom_grounding('56cb6663-3139-4770-86da-10af6f44291c', nonpunitive_harm_minimization_duty, instrumental).
narrative_ontology:cs_axiom('56cb6663-3139-4770-86da-10af6f44291c', foundational, least_restrictive_intervention_principle).
narrative_ontology:cs_axiom_status(least_restrictive_intervention_principle, holdable).
narrative_ontology:cs_axiom_grounding('56cb6663-3139-4770-86da-10af6f44291c', least_restrictive_intervention_principle, deontological).
narrative_ontology:cs_reference_frame('56cb6663-3139-4770-86da-10af6f44291c', public_health_stewardship_framework).
narrative_ontology:cs_drift_state('56cb6663-3139-4770-86da-10af6f44291c', contemporary_mandate_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('56cb6663-3139-4770-86da-10af6f44291c', '').
narrative_ontology:cs_kernel_id(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, treatment_provider_networks).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, general_resident_population).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, illicit_supply_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_beneficiary(substance_control_legitimacy__harm_reduction_reading, families_of_dependent_users).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, general_resident_population).
narrative_ontology:constraint_victim(substance_control_legitimacy__harm_reduction_reading, families_of_dependent_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the criteria that convert flagged substance use into compulsory assessment, contract and fund the service network, and operate the reporting systems that track people across programs. Budget and regulatory remit grow with the mandate perimeter; unwinding the framework would require repealing statutory duties the agencies themselves administer.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Obtain sterile supplies, opioid agonist therapy, and supervised consumption without fear of arrest — the regime's headline deliverance. But a positive screen or an overdose presentation can trigger mandatory assessment, monitored dispensing, and in some jurisdictions court-ordered treatment lasting months. Staying in the system means accepting continuous monitoring; walking away means surrendering the medications and supplies that keep them alive, so pharmacological dependence and service dependence arrive fused.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs, payer,
    moderate, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, people_who_use_drugs, beneficiary).

% Deliver counseling, prescribing, and residential capacity under per-episode reimbursement tied to mandate referrals. Every widening of compulsory-assessment criteria enlarges the referred caseload, and clinical leadership sits on the advisory boards that draft those criteria. Revenue depends on public contracts, so the network's fortunes rise and fall with the mandate perimeter it helps define.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, treatment_provider_networks, beneficiary,
    institutional, biographical, constrained, national).

% Continue moving product through channels the state never authorized, because the reading decriminalizes possession by users while leaving production and distribution offenses fully intact. They absorb interdiction losses, rotate compounds and trafficking routes faster than enforcement adapts, and pass adulteration risk down to the buyers the regime claims to protect.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, illicit_supply_participants, payer,
    organized, biographical, arbitrage, global).

% Live with less open injecting, fewer discarded needles, and slower blood-borne-disease transmission in shared spaces, and vote on the levies that fund the service network. They carry no direct compliance burden beyond taxation and retain the option of moving away from service siting disputes.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, general_resident_population, beneficiary,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, general_resident_population, payer).

% Are enlisted as monitors under mandated-treatment plans — reporting missed doses, hosting home visits — and absorb relapse emergencies inside the household. The same arrangements demonstrably reduce overdose deaths among their kin, so the burden and the benefit land on the same people through the same door.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, families_of_dependent_users, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(substance_control_legitimacy__harm_reduction_reading, families_of_dependent_users, beneficiary).

% Organize current and former users to demand fully voluntary services and peer-run programs. They testify at hearings and occasionally win pilot funding, but hold no seat on the bodies that set mandate criteria — and their members are precisely the population those criteria bind.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, drug_user_unions, excluded,
    moderate, biographical, constrained, national).

% Review involuntary-treatment orders for proportionality and consent-standard violations, usually hearing appeals months after commitments begin. Their docket shapes how far agencies dare extend the mandate perimeter, and their rulings are the main external brake on administrative discretion.
narrative_ontology:constraint_stakeholder(substance_control_legitimacy__harm_reduction_reading, judicial_rights_reviewers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_legitimacy__harm_reduction_reading, treatment_provider_networks).
narrative_ontology:fixing_cost_class(substance_control_legitimacy__harm_reduction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Centralizes management of substance-related externalities: sterile-supply distribution, supervised consumption, and opioid agonist therapy are provided once through a coordinated network instead of improvised per person; referral pathways between emergency departments, primary care, and social services are standardized; and the state acquires a single evidentiary apparatus for tracking overdoses and infections.
% TRANSFER_FUNCTION: Moves public funds from taxpayers to contracted provider networks per treatment episode; moves decision-making authority over users' bodies and regimens from users to clinicians and tribunals wherever mandate criteria fire; moves the risk of criminal sanction off users and onto whoever remains in the unauthorized supply chain.
% ABSENT_VOICES: Current and former drug users are structurally underrepresented: drug_user_unions testify but do not sit on the mandate-designing bodies, and the people whose bodies the compulsory-assessment criteria bind were not consulted on their terms. Illicit supply participants are wholly absent by construction. Communities subjected to concentrated service siting and policing spillovers enter late, after parameters are set.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, supervised-consumption sites and agonist-therapy programs close within days, overdose and infection mortality climbs back toward the pre-regime baseline, thousands of stabilized patients lose prescribed medication abruptly, provider networks shed contracted capacity, and the vacuum is filled either by returning user prosecution or by unmanaged open markets — the surrounding institutional arrangements visibly reorganize.
% FOUNDING_PROBLEM: Prohibition had produced mass incarceration of users, overdose deaths driven by adulterated and unpredictable supply, and exploding blood-borne-disease epidemics, while failing durably to reduce use. The arrangement was built to reduce death, disease, and public disorder from drug markets without carrying the carceral apparatus — to solve the harm problem directly rather than through punishment.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiology attests the founding problem is live: overdose mortality series, blood-borne-disease surveillance, and coroner reporting all document ongoing mass harm independent of any government's framing. International review bodies and academic evaluators corroborate that the problem persists; user unions attest it from the injured seat. No corroborating source claims the founding problem is solved — agreement on liveness crosses the beneficiary boundary.
narrative_ontology:disappearance_verdict(substance_control_legitimacy__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_legitimacy__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_legitimacy__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(substance_control_legitimacy__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_legitimacy__harm_reduction_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_legitimacy__harm_reduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_legitimacy__harm_reduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_legitimacy__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52 is moderate by design of the reading: the service layer delivers real goods (medications, sterile supplies, supervision) that users actively want, but the mandate layer converts care-seeking into a compliance relationship — flagged use can trigger compulsory assessment, monitored dispensing, and court-ordered treatment — and the surveillance layer converts every clinical contact into a tracked record. Suppression 0.42 is below the carceral readings': no user goes to jail for use, yet coercion is structural (commitment orders, dosing conditions tied to housing and benefits) plus continuing supply-side enforcement. Theater 0.25 and rising: needle exchanges and supervised consumption are stubbornly functional, but evaluation rituals, ribbon-cuttings, and consultation processes that never alter mandate criteria increasingly pad the activity profile. Accessibility_collapse 0.40: the rival readings remain politically alive — prohibition retrenchment and full legalization both stay on the ballot everywhere this regime operates, so understanding the arrangement does not erase its alternatives. Resistance 0.45: user unions contest mandates, civil-liberties litigation tests commitment orders, suppliers adapt operationally, and residents fight siting. Temporal shape: the series run on ONE shared nine-point grid (t=0..24, step 3) so every metric is authored at every examined point. The dynamics are cyclical-with-ratchet: an overdose crisis forces emergency mandate expansion (extractiveness jumps at t=9..12), fiscal retrenchment trims it back (t=15 dip), the next crisis repeats — but each cycle leaves the mandate layer permanently thicker, so troughs rise. The oscillation is partly an extraction mechanism in itself: crisis justifies coercion that calm periods never fully retract (intermittent reinforcement at institutional scale). Enforcement-capacity change tracks the same cycle, hence the suppression_requirement series; the flat-between-crises segments reflect stable machinery, not missing data.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the treatment_provider_networks seat the arrangement is a sustainable practice: patients arrive referred, revenue is predictable, and mandate expansion looks like reaching the people who won't come voluntarily — a functioning coordination they help steer. From the people_who_use_drugs seat the identical structure is conditional liberty: the medication keeping them alive is dispensed through a relationship in which noncompliance is reportable and refusal can end in a tribunal order. The general_resident_population seat sees neither — it sees fewer needles in parks and pays taxes. The engine computes this per-seat divergence from power, exit, and declared position; the divergence between the provider seat and the user seat is the sharpest signal in the story, and it is structural, not rhetorical.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries map to the low-d end: public_health_agencies collect budget and remit growth (agenda-setter AND beneficiary — the arrangement subsidizes its own administrator); treatment_provider_networks collect per-episode revenue that scales with mandate width; general_resident_population receive externality relief and pay only taxes, with mobile exit damping their exposure further. Declared victims map to the high-d end: people_who_use_drugs bear mandates, monitoring, and commitment risk with trapped exit (dependence fuses them to the service relationship — identity-adjacent lock-in through the body itself), and illicit_supply_participants bear the residual enforcement the reading deliberately retained, though their arbitrage mobility moderates chi relative to trapped users. Families_of_dependent_users straddle: formally payers (monitoring burdens, household emergencies) with real offsetting benefit, so their derived d should land mid-range rather than at either pole. No directionality overrides are authored: beneficiary/victim declarations plus exit atoms already place every seat correctly, and the dual-positioned seats are handled by their declared roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure modes this domain invites. Reading the regime as pure rope — 'services for the sick, nothing extracted' — erases the mandate asymmetry: a real coordination function coexisting with a transfer of bodily authority from users to tribunals, which is exactly the tangled-rope signature (coordination AND extraction through the same structure, held up by active enforcement). Reading it as pure snare — 'therapy as punishment' — erases the mortality evidence: the service layer saves lives users demonstrably want saved, so the arrangement is not coercion wearing a health costume. The founding problem (mass harm without working solutions) is corroborated live by sources outside the beneficiary set, so this is not a zombie mandate: mandatrophy is unresolved, and the theater-ratio trajectory is the leading indicator to watch. If mandate effectiveness omega resolves against mandates, the extraction component detaches from any function and the structure slides snare-ward; if black-market endogeneity resolves as self-inflicted, the residual payer seat thins and the structure cleans toward rope. The classification is thus provisional in a disciplined way: the engine recomputes as the omegas resolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of kernel substance_control_legitimacy (reading: harm_reduction_reading). At which structural element do the sibling readings (prohibition_reading, legalization_reading) diverge from this one?',
    'Comparative structural analysis across the three reading-stories: locate the disputed element in the source of state authority over users (stewardship duty owed to the user herself vs. an autonomy boundary limited to third-party harm vs. a moral-prevention duty exercised through criminalization), and observe which victim sets and transfer flows each reading produces.',
    'Adopting prohibition_reading replaces this reading''s payer set with criminally sanctioned users and drives epsilon sharply upward; adopting legalization_reading dissolves the treatment-mandate extraction surface entirely (no paternalist authority over competent adults) and removes this reading''s primary payer seat. The disagreement is located specifically in whether the state owes the user protection against her own choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame record: this story instantiates one reading of a three-way kernel contest; siblings are separate constraint files.').

omega_variable(
    mandate_effectiveness_uncertainty,
    'Do compulsorily initiated treatment episodes actually outperform voluntary initiation on retention and health outcomes, once selection effects are controlled?',
    'Controlled cohort comparison of mandated versus voluntarily enrolled patients matched on severity, with long-follow-up retention and mortality endpoints.',
    'If mandates confer no outcome advantage, their extractive component is pure overhead riding on the service network, and the regime''s classification slides from tangled_rope toward snare; if mandates measurably retain patients who would otherwise die, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_effectiveness_uncertainty, empirical, 'Whether treatment mandates are therapeutically productive or extractive overlay.').

omega_variable(
    black_market_endogeneity,
    'Is the persistent illicit market a property of this reading, or an artifact of partial adoption — the reading decriminalizes users but leaves production and distribution criminalized, manufacturing the residual victim group it then enforces against?',
    'Natural experiment across jurisdictions applying the reading''s logic consistently versus partially: if fully legalized supply collapses the violent-market externalities the reading cites as justification for supply enforcement, the residual market is endogenous to incomplete application.',
    'If endogenous, the illicit_supply_participants payer seat is transitional and self-created, the regime converges toward a cleaner coordination profile, and this reading''s own axioms push it toward the legalization_reading position; if exogenous, supply-side enforcement is a durable structural feature and the payer seat persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(black_market_endogeneity, conceptual, 'Whether the persistent black market is intrinsic to the reading or produced by its half-applied form.').

omega_variable(
    medicalization_as_control_ambiguity,
    'Is medicalized authority over users substantively less coercive than carceral authority, or a relabeled form of social control wearing clinical vocabulary?',
    'Compare rights profiles across regime types: consent standards before commitment, appeal latency, maximum order durations, and discharge rates. If medicalized orders match carceral ones on duration and contestability, the label changed but the control did not.',
    'If relabeling, the authored suppression figure understates the regime''s true coercive content and the user seat''s effective position is worse than the structural data suggests; if substantively milder, the reading''s central claim survives scrutiny.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medicalization_as_control_ambiguity, conceptual, 'Szasz-line ambiguity: therapeutic vs. carceral control beneath medical vocabulary.').

omega_variable(
    surveillance_data_flow_direction,
    'Does the health-system surveillance built around monitored dosing and screening feed primarily care coordination, or does it leak into enforcement-adjacent and benefit-sanctioning uses?',
    'Audit of data-sharing agreements and request logs between health authorities, police, and welfare agencies, with user-level tracing of downstream consequences of flagged records.',
    'Enforcement leakage would raise the effective burden on the user seat above what the mandate structure alone implies, increasing measured asymmetry between the user and provider seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_data_flow_direction, empirical, 'Direction of surveillance data flow: care coordination vs. enforcement leakage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_legitimacy__harm_reduction_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t0, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(subs_tr_t3, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 3, 0.1).
narrative_ontology:measurement(subs_tr_t6, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 6, 0.13).
narrative_ontology:measurement(subs_tr_t9, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 9, 0.16).
narrative_ontology:measurement(subs_tr_t12, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(subs_tr_t15, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(subs_tr_t18, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 18, 0.21).
narrative_ontology:measurement(subs_tr_t21, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 21, 0.23).
narrative_ontology:measurement(subs_tr_t24, substance_control_legitimacy__harm_reduction_reading, theater_ratio, 24, 0.25).

% Extraction over time
narrative_ontology:measurement(subs_be_t0, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(subs_be_t3, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(subs_be_t6, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(subs_be_t9, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 9, 0.46).
narrative_ontology:measurement(subs_be_t12, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(subs_be_t15, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(subs_be_t18, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(subs_be_t21, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 21, 0.49).
narrative_ontology:measurement(subs_be_t24, substance_control_legitimacy__harm_reduction_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t0, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(subs_su_t3, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 3, 0.34).
narrative_ontology:measurement(subs_su_t6, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(subs_su_t9, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 9, 0.39).
narrative_ontology:measurement(subs_su_t12, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(subs_su_t15, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(subs_su_t18, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 18, 0.43).
narrative_ontology:measurement(subs_su_t21, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 21, 0.41).
narrative_ontology:measurement(subs_su_t24, substance_control_legitimacy__harm_reduction_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_legitimacy__harm_reduction_reading, resource_allocation).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_legitimacy__harm_reduction_reading, substance_control_legitimacy__legalization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial concept 'drug policy' fails the epsilon-invariance test as a single story — evaluating it as carceral policy yields high extraction, evaluating it as service provision yields low, and the difference is the observable, not the constraint. Decomposed into three kernel readings, each with its own stable epsilon, victim set, and classification: this file (harm_reduction_reading, moderate mandate-centered epsilon), prohibition_reading (carceral, high epsilon, users as victims), legalization_reading (autonomy-bounded, mandate surface absent). Upstream/downstream structure: harm-reduction outcome evidence creates structural pressure on both siblings — it erodes prohibition's legitimacy conditions and supplies legalization's argumentative base — hence the affects_constraints edges from this story to both. Each sibling reciprocally links back in its own file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
