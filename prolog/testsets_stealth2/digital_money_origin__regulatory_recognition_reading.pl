% ============================================================================
% CONSTRAINT STORY: digital_money_origin__regulatory_recognition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__regulatory_recognition_reading, []).

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
 *   constraint_id: digital_money_origin__regulatory_recognition_reading
 *   human_readable: Regulatory-Recognition Boundary on Monetary Status (Digital Money Origin Kernel)
 *   domain: economic/institutional/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the digital_money_origin kernel:
 *   the claim that digital money emerged when monetary authorities formally
 *   incorporated it into statistical aggregates and regulatory frameworks.
 *   The constraint under examination is the standing arrangement that reading
 *   describes — the recognition boundary whereby an instrument counts as
 *   money only upon official incorporation. Per the epsilon-referent rule,
 *   extractiveness is authored for that standing arrangement as this reading
 *   assesses it, never for any preferred alternative. The sibling readings
 *   (became_thinkable_reading, first_held_reading) are separate constraint
 *   files, not parts of this one; the committer structure is routed to omegas
 *   and commentary.kernel_context. KEY AGENTS (by structural relationship):
 *   monetary_authorities: agenda-setter (institutional/arbitrage) — draws and
 *   administers the perimeter; incumbent_commercial_banks: primary
 *   beneficiary (powerful/constrained) — recognized balances, charter
 *   protection, shielded market; licensed_nonbank_issuers: edge-of-perimeter
 *   beneficiary paying compliance costs (moderate/constrained);
 *   unregulated_innovators: primary target (moderate/constrained) — bears
 *   exclusion and legal uncertainty; alternative_currency_projects: target
 *   with excluded voice (organized/identity_locked); household_money_holders:
 *   diffuse beneficiary carrying indirect costs (organized/constrained);
 *   monetary_economists: analytical observer (analytical/analytical).
 *   Expected structural delta honored: latest origin date of the three
 *   readings (interval opens 1980, after conception-era and first-holding-era
 *   windows), a constraint set dominated by legal/regulatory barriers,
 *   incumbent financial institutions as beneficiaries, unregulated innovators
 *   as victims.
 *
 * KEY AGENTS:
 *   - monetary_authorities: Agenda-setter (institutional/arbitrage) — defines the categories, licenses issuers, draws the perimeter
 *   - incumbent_commercial_banks: Primary beneficiary (powerful/constrained) — recognized balances, charter protection, shielded market
 *   - licensed_nonbank_issuers: Edge-of-perimeter beneficiary bearing compliance costs (moderate/constrained)
 *   - unregulated_innovators: Primary target (moderate/constrained) — bears exclusion and legal uncertainty
 *   - alternative_currency_projects: Target with excluded voice (organized/identity_locked)
 *   - household_money_holders: Diffuse beneficiary with indirect costs (organized/constrained)
 *   - monetary_economists: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, 0.63).
domain_priors:suppression_score(digital_money_origin__regulatory_recognition_reading, 0.68).
domain_priors:theater_ratio(digital_money_origin__regulatory_recognition_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(digital_money_origin__regulatory_recognition_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__regulatory_recognition_reading, tangled_rope).
narrative_ontology:human_readable(digital_money_origin__regulatory_recognition_reading, "Regulatory-Recognition Boundary on Monetary Status (Digital Money Origin Kernel)").
narrative_ontology:topic_domain(digital_money_origin__regulatory_recognition_reading, "economic/institutional/technological").

domain_priors:requires_active_enforcement(digital_money_origin__regulatory_recognition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__regulatory_recognition_reading, '2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa').
narrative_ontology:cs_kernel_codification('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', distributed).
narrative_ontology:cs_authority_grounding('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', practice).
narrative_ontology:cs_interpretation_layer_present('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa').
narrative_ontology:cs_reading_relation('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', digital_money_origin__first_held_reading, forecloses).
narrative_ontology:cs_reading_relation('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', digital_money_origin__became_thinkable_reading, influences).
narrative_ontology:cs_axiom('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', foundational, official_act_constitutes_moneyness).
narrative_ontology:cs_axiom_status(official_act_constitutes_moneyness, holdable).
narrative_ontology:cs_axiom_grounding('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', official_act_constitutes_moneyness, conventional).
narrative_ontology:cs_axiom('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', secondary, aggregate_measurement_requires_official_boundary).
narrative_ontology:cs_axiom_status(aggregate_measurement_requires_official_boundary, holdable).
narrative_ontology:cs_axiom_grounding('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', aggregate_measurement_requires_official_boundary, instrumental).
narrative_ontology:cs_reference_frame('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', official_statistical_perimeter).
narrative_ontology:cs_drift_state('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', post_stablecoin_proliferation, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('2aa4f17b-dbbd-4ddf-8d3c-cd437f5bd1aa', '').
narrative_ontology:cs_kernel_id(digital_money_origin__regulatory_recognition_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, incumbent_commercial_banks).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, licensed_nonbank_issuers).
narrative_ontology:constraint_beneficiary(digital_money_origin__regulatory_recognition_reading, household_money_holders).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, unregulated_innovators).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, alternative_currency_projects).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, licensed_nonbank_issuers).
narrative_ontology:constraint_victim(digital_money_origin__regulatory_recognition_reading, household_money_holders).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, chartalist_state_theory_of_money).
narrative_ontology:constraint_vindicates(digital_money_origin__regulatory_recognition_reading, official_aggregate_policy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft and maintain the statistical definitions of money, license or refuse issuers, and decide which instruments enter the aggregates their policy runs on. They convene the committees where the perimeter is drawn, coordinate definitions internationally through BIS and IMF forums, and can revise categories by administrative decision at low cost to themselves.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold the deposit balances that populate the official money stock, operate under charters that presuppose the recognized categories, and enjoy deposit-insurance eligibility and central-bank settlement access tied to recognized status. New payment entrants must either fit the recognized molds or compete from outside the banking rail. Leaving recognized status would mean forfeiting the settlement and insurance ties their balance sheets are built on.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, incumbent_commercial_banks, beneficiary,
    powerful, biographical, constrained, global).

% Issue electronic money under dedicated licensing regimes created once authorities carved out a category for them. Recognition brought legal standing, passporting rights, and merchant trust; it also brought capital requirements, safeguarding duties, and reporting obligations that scale with their outstanding float. Their category lives at the edge of the perimeter and can be narrowed by the same committees that drew it.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, licensed_nonbank_issuers, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, licensed_nonbank_issuers, payer).

% Build payment instruments and value-transfer systems that function like money but sit outside the recognized categories: they struggle to open and keep bank accounts, face legal uncertainty over whether their instruments are securities, e-money, or nothing in particular, and are invisible in the aggregates that anchor policy debate. Relocation to permissive jurisdictions is possible but fragments their user base and rarely restores banking-rail access.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, unregulated_innovators, payer,
    moderate, biographical, constrained, global).

% Run ledgers and issue units whose stated purpose is to operate independently of state monetary institutions; their founding documents treat official recognition as beside the point or as capture. They bear denial of banking access and hostile treatment in supervisory rhetoric, and their communities would resist seeking recognition even where it would relieve those pressures.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, alternative_currency_projects, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, alternative_currency_projects, excluded).

% Use the recognized unit for wages, prices, and savings, and are protected by insured deposits and supervised payment systems. They also absorb the arrangement's indirect costs: slower arrival of cheaper payment methods, fees that reflect limited competition, and the quiet exclusion of alternatives that might have served them better.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, household_money_holders, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(digital_money_origin__regulatory_recognition_reading, household_money_holders, payer).

% Study the aggregates, advise committees, and publish on measurement and policy transmission; their empirical infrastructure depends on the official series the perimeter produces, and several of the discipline's standard results presuppose the recognized categories.
narrative_ontology:constraint_stakeholder(digital_money_origin__regulatory_recognition_reading, monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__regulatory_recognition_reading, incumbent_commercial_banks).
narrative_ontology:fixing_cost_class(digital_money_origin__regulatory_recognition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives monetary authorities a bounded, measurable object: a shared official definition of what counts as money lets aggregates be computed, monetary policy calibrated, payment-system risks supervised, and a common unit of account stabilized across the economy.
% TRANSFER_FUNCTION: Moves legal legitimacy, market access, and supervisory protection to institutions inside the recognized perimeter; moves compliance burdens, legal uncertainty, and exclusion from settlement and banking rails onto issuers left outside it.
% ABSENT_VOICES: Alternative-currency communities and unrecognized issuers are not seated where definitions are drafted; users of informal digital value transfer (remittance corridors, in-game economies) have no representation. Their objection — that use, not recognition, makes money — enters only as litigation or advocacy from outside the room.
% DISAPPEARANCE_RATIONALE: If the recognition boundary vanished overnight, monetary aggregates would lose their object (authorities could no longer state what the money stock contains), supervision would fragment into ad hoc instrument-by-instrument rulings, incumbents would lose the perimeter that shields their franchise, and monetary status would be renegotiated usage-by-usage — a wholesale rearrangement of the monetary-institutional landscape.
% FOUNDING_PROBLEM: Once balances dematerialized onto mainframes and interbank networks, central banks could no longer count money by looking at vaults and notes; they needed an administratively tractable definition of money in order to conduct policy and monitor payment-system risk.
% FOUNDING_PROBLEM_CORROBORATION: Independent attestation exists outside the beneficiary set: BIS and IMF statistical working papers, academic monetary economics, and national statistical offices all attest the measurement-and-supervision problem remains live (stablecoin and CBDC debates reopen it continuously). Fintech industry bodies corroborate that the problem is live while disputing the boundary's current placement.
narrative_ontology:disappearance_verdict(digital_money_origin__regulatory_recognition_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__regulatory_recognition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__regulatory_recognition_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(digital_money_origin__regulatory_recognition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__regulatory_recognition_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__regulatory_recognition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_money_origin__regulatory_recognition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(digital_money_origin__regulatory_recognition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.63: the recognition boundary performs real measurement and supervision work while transferring legitimacy, settlement access, and market protection asymmetrically toward recognized incumbents; the transfer is decoupled from marginal service cost in the way monopoly-perimeter arrangements typically are. Suppression 0.68: persistence depends on actively maintained legal machinery — licensing regimes, AML/KYC obligations, settlement-access gating, debanking of unrecognized issuers — not on participant preference; the suppression_requirement series rises across the interval because the story specifically tracks enforcement-capacity build-up (from light-touch statistical monitoring in the 1980s to mature licensing and exclusion machinery by 2020). Theater_ratio 0.28: the statistical function is genuine, but a growing share of activity defends the perimeter rather than measures it (consultation responses, litigation defense, rhetorical delegitimation of outside instruments). Accessibility_collapse 0.45: alternatives persist — crypto networks, offshore issuance, community currencies — but recognized-channel access collapses completely for the unrecognized, so collapse is real yet partial. Resistance 0.55: sustained fintech lobbying, crypto-community refusal, jurisdictional arbitrage, and academic critique meet the boundary continuously. Claim/metric independence maintained: claimed_type tangled_rope is asserted from the structural reading (genuine coordination function + asymmetric incidence + active enforcement); the metrics are authored as descriptive facts without tuning toward any predicted engine verdict. All three tracked series run on one shared six-point grid (1980/1988/1996/2004/2012/2020) so no metric row borrows another's endpoints.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the boundary is an epistemic necessity — one cannot measure an undefined aggregate, so the arrangement reads as indispensable coordination the authorities themselves must maintain. From the incumbent-beneficiary seat it reads as earned institutional position: recognized status, insurance, and settlement access that outsiders simply failed to qualify for. From the payer seats the same structure reads as a moat: their instruments function as money in use while official definitions convert that functioning into legal risk. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (incumbent_commercial_banks, licensed_nonbank_issuers, household_money_holders) derive low directionality — the boundary subsidizes their positions, strongly for incumbents (constrained exit binds them to recognized status they dominate) and weakly for households (genuine benefit offset by indirect cost, sitting nearer symmetric). Declared victims (unregulated_innovators, alternative_currency_projects) derive high directionality, amplified by constrained and identity_locked exits respectively: the innovators cannot self-grant the recognition their products need, and the alternative-currency communities will not seek it even to relieve pressure. Monetary_authorities are deliberately NOT listed as beneficiaries — they collect institutional remit and data control rather than rents — so their seat takes the canonical fallback near the low-symmetric end; no directionality_overrides are authored because the beneficiary/victim declarations plus exit options already capture every seat's relationship, and an override keyed to the institutional power atom would misfire across both the authorities and the banks.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy declaration: the founding problem (needing an administratively tractable definition of money after dematerialization) is live, corroborated from outside the beneficiary set, and the R5 mismatch consumer finds status=live paired with verdict=world_rearranges — no zombie flag. The classification work this story performs is preventing mislabeling in BOTH directions: a pure-snare reading would erase the real coordination function (official aggregates genuinely guide policy and supervision; abolishing the boundary would not leave a free lunch but a measurement vacuum), while a pure-rope reading would erase the asymmetric incidence (the perimeter's placement was shaped under incumbent influence, and its costs fall concentrated on the unrecognized while its protections concentrate on the chartered). Tangled_rope holds both facts. The piton test also fails honestly: a concentrated capturer exists (incumbent banks), so the arrangement is not an inertial leftover nobody profits from.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates the regulatory_recognition_reading of the digital_money_origin kernel — is the latest-date, official-act criterion the correct constitutive account, and what would adopting a sibling reading change structurally?',
    'Comparative evaluation across the three sibling stories against the documentary record: whichever reading''s structural delta (origin date, beneficiary/victim set, enforcement profile) best fits the archival evidence re-weights the family.',
    'Adopting became_thinkable_reading moves the origin decades earlier and empties the victim set (no enforcement machinery yet exists to bear on anyone); adopting first_held_reading makes instrument-holders the relevant agents and removes incumbent-bank benefit from the structure. This reading uniquely generates the incumbent-beneficiary/innovator-victim configuration.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one of three rival readings of the digital_money_origin kernel; sibling adoption swaps the structural delta.').

omega_variable(
    state_theory_vs_market_theory_of_money,
    'Is monetary status constituted by official act (chartalism, this reading''s operative premise) or by market usage converging on a medium of exchange (Mengerian)?',
    'Historical and comparative analysis of durable unofficial monies — crypto adoption curves, prisoner-of-war camps, local exchange systems — testing whether official recognition tracks already-established use or precedes and creates it.',
    'If usage constitutes moneyness, this reading misdates the origin and misdescribes its victim set: unrecognized issuers would be money-producers rather than pre-monetary innovators, and the boundary''s profile shifts toward pure gatekeeping riding on a definition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_theory_vs_market_theory_of_money, conceptual, 'The deep theoretical disagreement underlying the kernel contest: state theory versus market theory of money''s origin.').

omega_variable(
    perimeter_absorption_capacity,
    'Can the recognition perimeter absorb stablecoins, tokenized deposits, and CBDC-adjacent instruments without the reading''s referent dissolving into activity-based regulation?',
    'Track whether forthcoming regimes (MiCA-style frameworks, stablecoin statutes, CBDC legislation) preserve a categorical monetary-status boundary or replace it with activity-based licensing that no longer turns on what money IS.',
    'If the categorical boundary dissolves, this constraint decays toward transitional or inertial territory (category retained, constitutive function migrated); if it holds, extraction continues along current lines with the perimeter redrawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(perimeter_absorption_capacity, empirical, 'Whether the recognition boundary survives the crypto-era instrument wave intact.').

omega_variable(
    measurement_necessity_vs_incumbent_moat,
    'Is the official boundary a neutral epistemic necessity of aggregate measurement, or a moat whose placement was shaped by incumbent financial institutions?',
    'Archival study of aggregate-definition revision episodes (who was consulted, which candidate definitions survived) combined with counterfactual analysis of how well usage-based or transaction-based aggregate constructions would perform for policy purposes.',
    'If moat-dominated, effective extraction on the payer seats is understated by the structural derivation and the coordination-function credit shrinks; if necessity-dominated, the tangled_rope reading softens toward rope and the incumbent-benefit attribution weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_necessity_vs_incumbent_moat, conceptual, 'Whether the boundary''s placement reflects measurement requirements or incumbent influence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__regulatory_recognition_reading, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1980, 0.08).
narrative_ontology:measurement_basis(digi_tr_t1980, observed).
narrative_ontology:measurement(digi_tr_t1988, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1988, 0.11).
narrative_ontology:measurement_basis(digi_tr_t1988, observed).
narrative_ontology:measurement(digi_tr_t1996, digital_money_origin__regulatory_recognition_reading, theater_ratio, 1996, 0.15).
narrative_ontology:measurement_basis(digi_tr_t1996, observed).
narrative_ontology:measurement(digi_tr_t2004, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2004, 0.19).
narrative_ontology:measurement_basis(digi_tr_t2004, observed).
narrative_ontology:measurement(digi_tr_t2012, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement_basis(digi_tr_t2012, observed).
narrative_ontology:measurement(digi_tr_t2020, digital_money_origin__regulatory_recognition_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement_basis(digi_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1980, 0.4).
narrative_ontology:measurement_basis(digi_be_t1980, observed).
narrative_ontology:measurement(digi_be_t1988, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1988, 0.46).
narrative_ontology:measurement_basis(digi_be_t1988, observed).
narrative_ontology:measurement(digi_be_t1996, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 1996, 0.52).
narrative_ontology:measurement_basis(digi_be_t1996, observed).
narrative_ontology:measurement(digi_be_t2004, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2004, 0.57).
narrative_ontology:measurement_basis(digi_be_t2004, observed).
narrative_ontology:measurement(digi_be_t2012, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2012, 0.6).
narrative_ontology:measurement_basis(digi_be_t2012, observed).
narrative_ontology:measurement(digi_be_t2020, digital_money_origin__regulatory_recognition_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement_basis(digi_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1980, 0.3).
narrative_ontology:measurement_basis(digi_su_t1980, observed).
narrative_ontology:measurement(digi_su_t1988, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1988, 0.38).
narrative_ontology:measurement_basis(digi_su_t1988, observed).
narrative_ontology:measurement(digi_su_t1996, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 1996, 0.47).
narrative_ontology:measurement_basis(digi_su_t1996, observed).
narrative_ontology:measurement(digi_su_t2004, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2004, 0.55).
narrative_ontology:measurement_basis(digi_su_t2004, observed).
narrative_ontology:measurement(digi_su_t2012, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2012, 0.62).
narrative_ontology:measurement_basis(digi_su_t2012, observed).
narrative_ontology:measurement(digi_su_t2020, digital_money_origin__regulatory_recognition_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(digi_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_money_origin__regulatory_recognition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, became_thinkable_reading).
narrative_ontology:affects_constraint(digital_money_origin__regulatory_recognition_reading, first_held_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when did digital money emerge' decomposes, per the epsilon-invariance principle, into three structurally distinct claims: conception (became_thinkable_reading), private holding (first_held_reading), and official incorporation (this file). Each carries its own epsilon, beneficiary/victim set, and interval; forcing one story to span all three would make epsilon observable-dependent, which is the signature of a mis-decomposed label. Edges run earliest-to-latest because upstream readings are cited as evidence by downstream ones (thinkability enables holding; established holding precedes recognition). This reading's epsilon is the highest-extraction member of the family: its referent is the enforcement-bearing boundary itself, whereas the siblings' referents (a concept's availability; a holding practice) carry lighter enforcement loads.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
