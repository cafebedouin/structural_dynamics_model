% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__severity_carve_out_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__severity_carve_out_reading, []).

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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Severity Carve-Out Reading: Beta Designation Barred in Critical Systems
 *   domain: legal/technological/consumer_protection
 *
 * SUMMARY:
 *   In the jurisdictions where this reading holds, courts and regulators
 *   treat a vendor's beta, investigational, or experimental designation as
 *   void for liability purposes whenever the software operates in a
 *   life-safety, financial, or other critical domain — regardless of how
 *   mature the testing program is, how prominent the disclosure, or how
 *   explicitly the purchaser acknowledged risk. The rule keeps expected
 *   catastrophic-loss bearing inside the vendor perimeter in exactly the
 *   settings where users cannot evaluate readiness and harm is irreversible.
 *   This story instantiates ONE reading of the beta_designation_doctrine
 *   kernel; the expansive_shield_reading and narrow_warning_reading are
 *   separate constraint stories with their own epsilon values, beneficiary
 *   structures, and classifications, linked through
 *   network.affects_constraints. The epsilon referent here is the standing
 *   carve-out arrangement itself, assessed by this reading's own lights —
 *   never the waiver regime the siblings would institute. Claim and metrics
 *   are authored independently: the claim states tangled_rope because the
 *   arrangement demonstrably carries both a genuine internalization function
 *   and asymmetric, categorical costs; the metrics describe the arrangement's
 *   actual operation without being tuned to any predicted engine output.
 *
 * KEY AGENTS:
 *   - - sectoral_safety_regulators: Agenda-setting administrator (institutional/constrained) — draws the critical-domain boundary, runs certification and surveillance, enforces the bar
 *   - - tort_law_judiciary: Co-agenda-setter (institutional/constrained) — adjudicates label-vs-liability case by case; precedent accretes into standing doctrine
 *   - - critical_system_end_users: Protected beneficiary (powerless/trapped) — receives only deployments whose maker bears full liability
 *   - - certified_incumbent_vendors: Moat beneficiary (powerful/arbitrage) — collects the pricing protection the categorical bar affords
 *   - - premarket_software_vendors: Primary payer (moderate/constrained) — bears categorical liability and delayed deployment regardless of testing merit
 *   - - early_adopter_institutions: Net payer with partial relief (institutional/constrained) — foregone pilots on one side of the ledger, avoided fallout on the other
 *   - - patients_denied_early_access: Excluded voice (powerless/trapped) — bears foregone-access costs with no seat in rulemaking
 *   - - software_liability_researchers: Analytical observer (analytical/analytical) — measures whether the bar protects or entrenches
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.38).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.58).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Severity Carve-Out Reading: Beta Designation Barred in Critical Systems").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "legal/technological/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, 'ee5166cf-883d-406b-9c23-a1aa3c958849').
narrative_ontology:cs_kernel_codification('ee5166cf-883d-406b-9c23-a1aa3c958849', distributed).
narrative_ontology:cs_authority_grounding('ee5166cf-883d-406b-9c23-a1aa3c958849', distributed).
narrative_ontology:cs_reading_relation('ee5166cf-883d-406b-9c23-a1aa3c958849', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('ee5166cf-883d-406b-9c23-a1aa3c958849', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('ee5166cf-883d-406b-9c23-a1aa3c958849', foundational, harm_severity_overrides_vendor_designation).
narrative_ontology:cs_axiom_status(harm_severity_overrides_vendor_designation, holdable).
narrative_ontology:cs_axiom_grounding('ee5166cf-883d-406b-9c23-a1aa3c958849', harm_severity_overrides_vendor_designation, deontological).
narrative_ontology:cs_axiom('ee5166cf-883d-406b-9c23-a1aa3c958849', foundational, consent_asymmetry_defeats_disclosure_waivers).
narrative_ontology:cs_axiom_status(consent_asymmetry_defeats_disclosure_waivers, holdable).
narrative_ontology:cs_axiom_grounding('ee5166cf-883d-406b-9c23-a1aa3c958849', consent_asymmetry_defeats_disclosure_waivers, empirically_contingent).
narrative_ontology:cs_reference_frame('ee5166cf-883d-406b-9c23-a1aa3c958849', severity_dominant_liability_allocation).
narrative_ontology:cs_drift_state('ee5166cf-883d-406b-9c23-a1aa3c958849', contemporary_ai_deployment_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('ee5166cf-883d-406b-9c23-a1aa3c958849', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, certified_incumbent_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, premarket_software_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, early_adopter_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, early_adopter_institutions).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, certified_incumbent_vendors).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, severity_proportional_liability_principle).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, meaningful_consent_precondition_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the certification and post-market surveillance systems for software in medicine, aviation, and finance. They decide which product categories count as critical, publish guidance on when pre-release deployment is permissible, and investigate incidents. Their budget authority and political standing depend on being seen as the gate catastrophic failures do not slip past; they collect no fee tied to how strictly the bar is drawn.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, sectoral_safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Courts hearing product-liability and contract disputes decide whether a vendor's beta or investigational label excuses it from paying for harm its software caused. Where this reading holds, they treat such labels as void in critical domains no matter how prominent the disclosure or how extensive the attached testing record. Precedent accumulates case by case; individual judges cannot opt out of the doctrine their colleagues set.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, tort_law_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Patients with implanted or bedside software-governed devices, passengers in vehicles running driver-assistance stacks, and account holders whose funds move through automated trading and payment rails. They cannot personally evaluate whether a system is ready; they rely on the fact that anything operating in these domains reached them only after its maker accepted full liability for it. When a certified system injures them, compensation channels exist that a signed beta acknowledgment would have closed.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users, beneficiary,
    powerless, biographical, trapped, global).

% Established makers of approved medical devices, avionics, and banking infrastructure that completed expensive validation and approval cycles years ago. Every competitor barred from skipping that cycle under a beta banner protects the pricing that recoups their sunk compliance spending. They advocate vigorously for strict categorical application, and they also pay claims under the same liability regime when their own approved products fail.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, certified_incumbent_vendors, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, certified_incumbent_vendors, payer).

% Developers of diagnostic algorithms, surgical robotics, autonomous-driving stacks, and trading engines that want real-world data before their systems are fully mature. The bar means they cannot ship into hospitals, vehicles, or markets under a disclaimer; they must fund complete validation first, buy liability coverage sized to worst-case harm, or postpone deployment and watch the state of the art move ahead. The rule charges them the same price whether their internal testing is meticulous or careless.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, premarket_software_vendors, payer,
    moderate, biographical, constrained, global).

% Hospital networks, broker-dealers, and fleet operators that would gain clinical or commercial advantage from piloting promising pre-release systems. They are barred from deploying such systems in critical functions under beta terms, so they bear integration delays, pay for interim manual workarounds, and sometimes watch rivals in laxer jurisdictions move first. On the other side of the ledger they are spared the operational disruption and reputational damage of failed experiments, and their insurers price their deployments without a waiver-shaped hole.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, early_adopter_institutions, payer,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, early_adopter_institutions, beneficiary).

% People whose conditions might be treated months or years sooner by an experimental diagnostic or therapeutic system that manufacturers will not release outside full approval under this reading. They are not consulted when the bar is drawn: rulemaking dockets fill with industry filings and safety-advocate testimony, and no mechanism elicits whether they would accept documented risk in exchange for earlier access. Some obtain gray-market or offshore access instead, outside any liability framework at all.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, patients_denied_early_access, excluded,
    powerless, biographical, trapped, global).

% Academics, actuarial analysts, and policy institutes compiling incident databases, insurance-loss series, and cross-jurisdiction comparisons of innovation rates in critical software. They produce the evidence that determines whether the categorical bar is doing protective work or mainly defending incumbents, and they hold no position in the liability chain themselves.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_liability_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, certified_incumbent_vendors).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns deployment incentives in domains where users cannot evaluate software readiness and failures are catastrophic and irreversible: it assigns expected-loss bearing to the party positioned to test, validate, patch, and insure — the vendor — closing the disclosure-asymmetry loophole through which waiver-by-label would otherwise race critical domains to the bottom.
% TRANSFER_FUNCTION: Moves expected catastrophic-loss bearing from end users (and the public backstop that absorbs uncompensated harm) to software vendors and their liability insurers; secondarily moves time-to-market cost onto vendors and foregone-access cost onto patients and adopting institutions.
% ABSENT_VOICES: Risk-tolerant patients who would trade documented uncertainty for earlier access have no seat: rulemaking and precedent are populated by vendors, safety advocates, regulators, and institutional purchasers. Small premarket vendors are similarly under-represented relative to incumbents whose compliance spending the categorical bar protects.
% DISAPPEARANCE_RATIONALE: If the bar vanished overnight, beta-labeled deployments would enter hospitals, vehicles, and financial rails within quarters, priced on disclaimers instead of liability; incident and uncompensated-loss rates would climb until insurers refused cover or repriced catastrophically, procurement contracts would be rewritten, and litigation would rebuild some approximation of the doctrine case by case — the arrangement the world currently relies on to keep liability inside the vendor perimeter would have to be reconstructed from incident wreckage.
% FOUNDING_PROBLEM: Vendors deploying immature software into life-safety and financial settings under beta, investigational, or experimental labels that purport to disclaim liability — leaving patients, passengers, and account holders bearing uncompensated catastrophic harm they never meaningfully agreed to: the pattern behind radiation-therapy overdoses attributed to software faults, early driver-assistance fatalities written off as user error during public testing programs, and trading-engine failures absorbed as known experimental risk.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: regulator incident databases (device-event reporting systems, transportation-safety investigation records, market-regulator post-mortems of trading disruptions), court records declining waiver defenses in critical-domain cases, reinsurer loss series on software-caused bodily injury and financial harm, and the human-factors literature on disclosure non-comprehension. Certified incumbents also attest the problem is live, but their testimony is self-interested; the databases and case records stand without them.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).
:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.38 decomposes into three components net of the internalization credit: the above-internalization wedge of the categorical rule (vendors whose testing is already sound pay the same as careless ones), the incumbent pricing protection the bar affords, and the diffuse foregone-access cost borne by patients and adopting institutions. Suppression 0.58 is the raw, unscaled structural force of the bar — a contracting practice is legally unavailable across whole domains, enforced by courts and regulators — deliberately narrower than a snare's suppression because adjacent exits remain open: post-approval deployment, insured pilots under supervisory pathways, and non-critical markets. Theater 0.24 reflects validation regimes whose engineering substance still dominates their documentation layer, with the checkbox share growing as compliance scales. Accessibility collapse 0.72: the specific suppressed pathway — waiver-by-label in critical domains — closes completely once the doctrine is understood, which is its defining feature, while the surrounding option space stays navigable, so the aggregate sits well below mountain-grade collapse. Resistance 0.48: sustained industry litigation and lobbying, forum shopping toward expansive-shield jurisdictions, and periodic legislative pushback — real but not regime-threatening. The three temporal series share one seven-point grid (1990-2026) so every metric is authored at every examined time point; all trajectories are monotone, reflecting enforcement codification and scope creep rather than oscillation, so no cyclical machinery is invoked. The rising suppression_requirement series traces a genuine enforcement-capacity ratchet: case-by-case judicial discretion hardened into standing doctrine as statutes and precedents accumulated.
 *
 * PERSPECTIVAL GAP:
 *   From the premarket vendor seat the arrangement computes as enforced extraction: a price charged categorically, collected regardless of merit, with exit costing the entire critical-market segment. From the end-user seat it computes as protection no individual could purchase alone. From the incumbent seat it computes as an earned moat that recoups sunk compliance spending. From the regulator seat it computes as mandate fulfilled. The engine derives these divergent per-seat classifications from the structural data — role, power, exit, scope — and the divergence between the payer seats and the beneficiary/agenda-setter seats is the perspectival fact this story exists to register; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical_system_end_users are declared beneficiaries with trapped exit: the derivation places them near the full-beneficiary pole (d near 0.0), and their trapped status amplifies the protection they receive rather than any extraction. Certified_incumbent_vendors are beneficiaries with arbitrage-grade exit — near the beneficiary end among economic actors (d roughly 0.1-0.2), since the moat premium accrues to them precisely because they can reallocate while entrants cannot. Premarket_software_vendors are declared victims with constrained exit (d roughly 0.8): they bear the categorical charge, and their alternative — abandoning critical markets — forfeits the segment where their technology compounds. Early_adopter_institutions are victims with a secondary beneficiary position (d roughly 0.55-0.65 net): foregone pilots dominate avoided fallout. The regulator and judiciary seats sit near symmetric (d roughly 0.4-0.5): they administer the bar without collecting from it. No directionality overrides are authored: the override surface is keyed by power atom, and this story contains multiple structurally opposed seats sharing power atoms (three institutional seats with opposed relationships; two powerless seats on opposite sides of the arrangement) — a per-atom override would cross-contaminate them, so the derivation chain is left to run on the beneficiary/victim declarations, which already separate the seats correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — waiver-by-label externalizing catastrophic harm in domains where consent cannot be meaningful — is live and corroborated by incident databases and case records from outside the beneficiary set, so no mandatrophy is declared and the mandate has not outlived its function. The classification work this story performs is boundary-keeping in both directions: reading the carve-out as pure rope would erase the incumbent-moat and categorical-overcharge components that the payer seats demonstrably bear; reading it as snare would erase the internalization function that gives the arrangement its coordination warrant and its world_rearranges verdict. The tangled_rope claim holds both halves simultaneously. The R5 mismatch consumer finds founding_problem_status=live paired with disappearance_verdict=world_rearranges — the consistent cell, flagging no zombie and no capture drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_disagreement_location,
    'Within the beta_designation_doctrine kernel, where exactly do the three readings disagree — on waiver duration, on software domain, or on whether disclosure can ever substitute for liability?',
    'Comparative statutory and case-law mapping: identify which jurisdictions condition waiver on a genuine testing phase (narrow_warning_reading), which accept designation-based waiver wholesale (expansive_shield_reading), and which bar it by domain severity (this reading); the operative axis is whichever variable actually separates the frameworks.',
    'If the operative axis is domain severity, this reading is the emerging canonical form and the siblings persist only as legacy contracting boilerplate; if the axis is duration or disclosure quality, this reading narrows to a special case and its categorical character is contingent rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_disagreement_location, conceptual, 'Locates the structural axis on which the kernel''s three readings actually diverge.').

omega_variable(
    sibling_structural_delta,
    'What would change structurally if the expansive_shield_reading governed instead of this reading?',
    'Author and compile the sibling stories; compare victim sets, beneficiary sets, and computed per-seat classifications across the three files of the kernel family.',
    'Under the expansive reading the victim set expands to every user of any beta-labeled software, end-user protection vanishes, and this reading''s beneficiary seats flip to victims; the cross-file comparison is the measurement, and this story''s epsilon is fixed to the carve-out arrangement only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_structural_delta, conceptual, 'Committer-frame record of the structural delta the sibling readings would produce.').

omega_variable(
    categorical_vs_risk_calibrated_necessity,
    'Is categorical unavailability load-bearing, or would risk-calibrated liability scaled to demonstrated testing quality achieve the same internalization without the categorical overcharge?',
    'Natural experiments from jurisdictions or sectors piloting tiered or sandboxed liability for critical software: compare incident rates, insurance pricing, and deployment volumes against categorical-bar jurisdictions.',
    'If tiering holds incident rates flat, the categorical component is excess extraction and this reading''s epsilon drifts downward toward a calibrated variant; if tiering correlates with incident spikes, categoricality is the coordination itself and the current epsilon sits near its floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_risk_calibrated_necessity, empirical, 'Tests whether the categorical form of the bar is necessary or merely administratively convenient.').

omega_variable(
    incumbent_moat_share,
    'What fraction of the bar''s benefit distribution is protective (accruing to end users as uncompensated-harm prevention) versus rent-preserving (accruing to certified incumbents as pricing protection against uncertified entrants)?',
    'Entry-rate, price, and margin analysis in critical-software markets before and after carve-out adoption, controlling for underlying certification-cost trends.',
    'A dominant moat share pushes the arrangement toward the capture end of its hybrid range and eventually toward snare-flavored computation; a dominant protective share supports the coordination-first reading of the carve-out.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbent_moat_share, empirical, 'Decomposes the bar''s benefit flow between protection and entrenchment.').

omega_variable(
    foregone_access_cost_magnitude,
    'How much measurable health and financial benefit is destroyed by delaying critical-software deployment until full approval — the cost borne by the denied-access population?',
    'Comparator analysis against expanded-access and regulatory-sandbox pathways that permit conditional deployment with liability retained: measure outcomes for patients and institutions served under each regime.',
    'A large foregone-benefit figure raises the effective burden on the excluded seat and strengthens the case for supervised-pilot variants of this reading; a small figure confirms the current calibration prices the risk correctly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foregone_access_cost_magnitude, empirical, 'Sizes the opportunity cost imposed on populations denied early access to experimental critical software.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 1990, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t1990, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 1990, 0.14).
narrative_ontology:measurement_basis(beta_tr_t1990, observed).
narrative_ontology:measurement(beta_tr_t1996, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 1996, 0.16).
narrative_ontology:measurement_basis(beta_tr_t1996, observed).
narrative_ontology:measurement(beta_tr_t2002, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2002, 0.18).
narrative_ontology:measurement_basis(beta_tr_t2002, observed).
narrative_ontology:measurement(beta_tr_t2008, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement_basis(beta_tr_t2008, observed).
narrative_ontology:measurement(beta_tr_t2014, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement_basis(beta_tr_t2014, observed).
narrative_ontology:measurement(beta_tr_t2020, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2020, 0.23).
narrative_ontology:measurement_basis(beta_tr_t2020, observed).
narrative_ontology:measurement(beta_tr_t2026, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 2026, 0.24).
narrative_ontology:measurement_basis(beta_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(beta_be_t1990, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement_basis(beta_be_t1990, observed).
narrative_ontology:measurement(beta_be_t1996, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 1996, 0.24).
narrative_ontology:measurement_basis(beta_be_t1996, observed).
narrative_ontology:measurement(beta_be_t2002, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2002, 0.27).
narrative_ontology:measurement_basis(beta_be_t2002, observed).
narrative_ontology:measurement(beta_be_t2008, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement_basis(beta_be_t2008, observed).
narrative_ontology:measurement(beta_be_t2014, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2014, 0.33).
narrative_ontology:measurement_basis(beta_be_t2014, observed).
narrative_ontology:measurement(beta_be_t2020, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2020, 0.36).
narrative_ontology:measurement_basis(beta_be_t2020, observed).
narrative_ontology:measurement(beta_be_t2026, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 2026, 0.38).
narrative_ontology:measurement_basis(beta_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t1990, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement_basis(beta_su_t1990, observed).
narrative_ontology:measurement(beta_su_t1996, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 1996, 0.44).
narrative_ontology:measurement_basis(beta_su_t1996, observed).
narrative_ontology:measurement(beta_su_t2002, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2002, 0.48).
narrative_ontology:measurement_basis(beta_su_t2002, observed).
narrative_ontology:measurement(beta_su_t2008, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2008, 0.52).
narrative_ontology:measurement_basis(beta_su_t2008, observed).
narrative_ontology:measurement(beta_su_t2014, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2014, 0.55).
narrative_ontology:measurement_basis(beta_su_t2014, observed).
narrative_ontology:measurement(beta_su_t2020, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement_basis(beta_su_t2020, observed).
narrative_ontology:measurement(beta_su_t2026, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 2026, 0.58).
narrative_ontology:measurement_basis(beta_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, resource_allocation).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'beta designation doctrine' conflates three structurally distinct liability rules and is decomposed per the epsilon-invariance principle into a three-story constraint family. Expansive_shield_reading is historically upstream: vendor-drafted boilerplate asserted designation-based waiver universally, and its low-contest era produced the baseline against which the others define themselves. Narrow_warning_reading occupies the middle: it accepts designation as a genuine-testing disclosure while preserving baseline liability. This severity_carve_out_reading is downstream codification pressure: once regulators and courts began barring waiver by domain severity, the upstream expansive claim became indefensible in critical domains, and the narrow warning reading was pushed toward severity-conditioned variants. Each story carries its own stable epsilon; measuring the family requires all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
