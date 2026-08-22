% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__public_health_flexibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__public_health_flexibility_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__public_health_flexibility_reading
 *   human_readable: TRIPS Public Health Flexibilities — Broad Compulsory Licensing and Parallel Import Reading
 *   domain: international_trade_law/public_health/intellectual_property
 *
 * SUMMARY:
 *   Under this reading, the TRIPS Agreement is construed as embedding broad,
 *   standing member rights to issue compulsory licenses and permit parallel
 *   imports of patented medicines, with public health access as an
 *   interpretive priority the text itself supports. The arrangement this
 *   reading instantiates constrains pharmaceutical patent holders' ability to
 *   enforce uniform global market exclusivity: governments retain lawful
 *   tools to authorize generic production at negotiated remuneration and to
 *   import cheaper legitimately-marketed equivalents. The reading was
 *   affirmed politically at Doha (2001), institutionalized through the
 *   Article 31bis amendment (adopted 2003, in force 2017), and stress-tested
 *   during the COVID-19 waiver debate (2020-2022). This story generates ONLY
 *   this reading as a clean, epsilon-invariant constraint; the
 *   strong-exclusivity and dispute-settlement-authority readings of the same
 *   kernel are separate constraint files linked through
 *   network.affects_constraints, and the contest between readings is routed
 *   to omega variables rather than hedged into the metrics. KEY AGENTS (by
 *   structural relationship): - pharmaceutical_patent_holders: Primary target
 *   (institutional/arbitrage) — bears pricing erosion and exclusivity loss
 *   wherever flexibilities are invoked -
 *   generic_pharmaceutical_manufacturers: Primary beneficiary
 *   (organized/mobile) — gains licensed production space and negotiating
 *   leverage - health_ministries_developing_countries:
 *   Beneficiary-administrator (moderate/constrained) — invokes the licenses,
 *   absorbs the diplomatic pressure - low_income_country_patients: Ultimate
 *   beneficiary (powerless/trapped) — receives access; holds no seat in trade
 *   fora - wto_membership_trips_council: Agenda-setter
 *   (institutional/arbitrage) — administers the treaty, codified Doha and
 *   Article 31bis - developed_country_trade_agencies: Boundary-policing
 *   agenda-setter (institutional/arbitrage) — administers the TRIPS-plus
 *   counterpressure that narrows effective scope -
 *   civil_society_health_advocates: Campaign beneficiary (organized/mobile) —
 *   collects standing and policy wins; forced the Doha agenda - Analytical
 *   observer: sees the full three-reading kernel structure and the divergence
 *   between seats
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.47).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.62).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "TRIPS Public Health Flexibilities — Broad Compulsory Licensing and Parallel Import Reading").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__public_health_flexibility_reading, "international_trade_law/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__public_health_flexibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'da1e0e76-f845-450e-998d-096c6d8206e6').
narrative_ontology:cs_kernel_codification('da1e0e76-f845-450e-998d-096c6d8206e6', formalized).
narrative_ontology:cs_authority_grounding('da1e0e76-f845-450e-998d-096c6d8206e6', lineage).
narrative_ontology:cs_interpretation_layer_present('da1e0e76-f845-450e-998d-096c6d8206e6').
narrative_ontology:cs_reading_relation('da1e0e76-f845-450e-998d-096c6d8206e6', trips_agreement_interpretive_kernel__strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('da1e0e76-f845-450e-998d-096c6d8206e6', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('da1e0e76-f845-450e-998d-096c6d8206e6', foundational, public_health_primacy_in_trips_interpretation).
narrative_ontology:cs_axiom_status(public_health_primacy_in_trips_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('da1e0e76-f845-450e-998d-096c6d8206e6', public_health_primacy_in_trips_interpretation, deontological).
narrative_ontology:cs_axiom('da1e0e76-f845-450e-998d-096c6d8206e6', secondary, exhaustion_policy_member_autonomy).
narrative_ontology:cs_axiom_status(exhaustion_policy_member_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('da1e0e76-f845-450e-998d-096c6d8206e6', exhaustion_policy_member_autonomy, conventional).
narrative_ontology:cs_reference_frame('da1e0e76-f845-450e-998d-096c6d8206e6', doha_affirmed_health_flexibility_baseline).
narrative_ontology:cs_drift_state('da1e0e76-f845-450e-998d-096c6d8206e6', contemporary_post_covid_waiver_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('da1e0e76-f845-450e-998d-096c6d8206e6', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__public_health_flexibility_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_country_patients).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__public_health_flexibility_reading, civil_society_health_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Originator firms holding patents on essential medicines. Wherever a compulsory license issues or parallel imports flow, they lose monopoly pricing and market exclusivity in that market, receiving only Article 31(h) remuneration. They cannot exit the treaty system, but they mitigate: launch sequencing into strong-IP markets first, voluntary licensing offered to preempt involuntary licenses, lobbying for TRIPS-plus chapters in bilateral trade agreements, and home-country trade pressure against invoking governments.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, pharmaceutical_patent_holders, payer,
    institutional, generational, arbitrage, global).

% Producers of off-patent and licensed-generic medicines. The flexibility space lets them manufacture and export patented molecules under compulsory license, serve markets originators decline to price for, and negotiate voluntary licenses from a position of demonstrated capability. Their gains are market share and negotiating leverage; they can redeploy production lines to other products and markets if the space closes.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, generic_pharmaceutical_manufacturers, beneficiary,
    organized, biographical, mobile, global).

% Government health authorities that invoke compulsory licensing and parallel importation to secure affordable drug supply, and that write the national implementing legislation determining how usable the flexibilities are. Each invocation carries diplomatic and trade-retaliation risk from trading partners. Forgoing the flexibilities means paying monopoly prices, rationing treatment, or depending on donor programs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__public_health_flexibility_reading, health_ministries_developing_countries, agenda_setter).

% People living with HIV, tuberculosis, hepatitis C, and other conditions treated by medicines whose price depends on whether generic competition is legally possible. They receive access when the flexibility space is used and bear untreated disease when it is narrowed or left unused. They hold no seat in trade negotiations and cannot exit illness or the price structure.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, low_income_country_patients, beneficiary,
    powerless, biographical, trapped, regional).

% The collective membership administering the treaty through the TRIPS Council. It adopted the Doha Declaration on TRIPS and Public Health affirming members' right to protect public health, waived and then permanently amended Article 31 to create the Paragraph 6 export mechanism (Article 31bis), and adjudicates the boundary between legitimate flexibilities and regime erosion. It can amend, waive, or reinterpret — it reshapes the constraint rather than sitting under it.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, wto_membership_trips_council, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Home-country trade bureaucracies of major pharmaceutical-producing states. They police the flexibility space's boundaries from outside the multilateral text: special-watch-list designations against invoking countries, TRIPS-plus intellectual property chapters inserted into bilateral and regional trade agreements, and diplomatic pressure campaigns. They administer the countervailing arrangement that narrows this one's effective scope.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, developed_country_trade_agencies, agenda_setter,
    institutional, biographical, arbitrage, global).

% Access-to-medicines campaign organizations that forced public health onto the Doha agenda and monitor flexibility use since. They collect standing, funding relevance, and policy wins from the framework's existence; they run no part of the licensing machinery but shape when ministries dare to invoke it.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__public_health_flexibility_reading, civil_society_health_advocates, beneficiary,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__public_health_flexibility_reading, diffuse).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__public_health_flexibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem in medicine access: creates a shared multilateral legal framework under which any government can secure affordable generic supply during health crises without unilaterally defying the international IP regime and inviting isolated retaliation. The flexibilities coordinate health exceptions across jurisdictions so no single country bears the cost of defection alone.
% TRANSFER_FUNCTION: Moves pricing power and market exclusivity from patent-holding pharmaceutical firms to invoking governments and generic producers; moves affordable medicines to populations that could not obtain them at monopoly prices; moves negotiating leverage from originator firms to importing states, who can now credibly threaten licensed generic entry.
% ABSENT_VOICES: Patients themselves have no seat in TRIPS negotiations — they appear only vicariously through health ministries and advocacy organizations. Least-developed countries without domestic manufacturing capacity were effectively voiceless until Article 31bis gave them an import route, and even now lack the administrative apparatus to invoke it. Future R&D funders whose incentive structure erodes if flexibilities broaden are spoken for by industry associations rather than seated themselves.
% DISAPPEARANCE_RATIONALE: If the flexibility space vanished overnight, generic supply chains built around licensed ARV, TB, and HCV regimens would collapse, treatment prices in dependent countries would spike toward monopoly levels, national treatment programs would break, and invoking-or-defying crises would erupt immediately as health ministries confronted unpayable invoices — the access architecture of global public health would reorganize around donor charity and bilateral bargaining.
% FOUNDING_PROBLEM: The arrangement-through-this-reading was consolidated to solve the problem that the 1994 uniform minimum IP standards threatened to price essential medicines out of reach during the HIV/AIDS pandemic: governments joining the trade regime needed assurance that patent obligations would not strip them of health-emergency tools, and generic suppliers needed legal cover to serve markets originators would not price for.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the WHO Commission on Intellectual Property Rights, Innovation and Public Health (2006) and the UN Secretary-General's High-Level Panel on Access to Medicines (2016) — bodies staffed independently of generic industry and health ministries — attest both the founding problem and its recurrence with each new therapeutic class and pandemic. Industry-funded analyses attest the opposite pole (that the emergency passed and residual flexibilities deter innovation), which is itself evidence the status is contested rather than dead.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__public_health_flexibility_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__public_health_flexibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__public_health_flexibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__public_health_flexibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.47 at interval end): the arrangement genuinely strips monopoly rents from patent holders in markets where licenses issue or parallel imports flow, but the stripping is bounded — Article 31(h) remuneration is owed, most markets still honor exclusivity, and the threat value of a compulsory license often settles into voluntary licensing rather than outright displacement. Suppression (0.62) is a raw structural property, unscaled by power or scope: the constraint's persistence depends on actively suppressing the rival exclusivity-expansion program — unilateral watch-listing, FTA TRIPS-plus insertion — and it removes patent holders' litigation alternatives wherever a lawful license stands. Theater ratio (0.30) reflects real functional use (post-Doha ARV scale-up, Thai and Indian licenses, COVID-era invocations) shadowed by genuinely theatrical machinery: the Paragraph 6 export mechanism has been used exactly once (Rwanda/Canada, 2007) and proved too cumbersome for COVID-era deployment. Accessibility collapse is low (0.35) because alternatives to the flexibility route persist and are actively cultivated — voluntary licensing, tiered pricing, donor programs, patent pools — so the constraint coexists with rather than extinguishes its substitutes. Resistance is high (0.70): the arrangement is the most actively contested feature of the IP regime. The measurement series run on ONE shared time grid (t = 0..27, mapping 1995..2022: t0 = TRIPS entry into force, t6 = Doha, t12 = first Paragraph 6 use, t21 = UN High-Level Panel, t24-t27 = the COVID waiver fight and partial outcome), with every tracked metric authored at every point. Extractiveness oscillates rather than drifting monotonically: crisis cycles (ARV era, HCV, COVID) drive invocation waves, and the oscillation is driven by exogenous epidemiology, not intermittent reinforcement — the base_properties scalars report the end-state (t27) values. Gain-flow check performed before authoring 'diffuse': generic producers' margins compress under intra-generic competition, ministry savings pass through budgets to patients, and patient surplus is dispersed consumption — no named seat durably captures the transferred value, so 'diffuse' is the affirmative finding, not a default.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute materially different types from identical structural data. From the pharmaceutical_patent_holders seat, the arrangement is enforced confiscation of lawfully granted exclusivity — extraction with a coordination alibi. From the low_income_country_patients and health_ministries seats, the same structure is the only workable coordination mechanism for survival-critical access — coordination with an incidental cost to a wealthy industry. From the wto_membership seat it is a balance-maintenance device whose value lies precisely in being contested: the flexibilities must be real enough to be credible and bounded enough to keep producing states inside the regime. The engine derives this divergence from the declared beneficiary/victim structure and exit asymmetries; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: generic manufacturers (beneficiary, mobile exit) sit near the full-beneficiary end; health ministries (beneficiary, constrained exit — they need the medicines and cannot leave the treaty) sit low but not zero, since invocation carries real diplomatic cost; patients (beneficiary, trapped) are subsidized by the arrangement despite having no leverage; pharmaceutical patent holders (victim, institutional power, arbitrage exit) sit near the full-target end. No directionality overrides are authored: the structural declarations plus exit atoms already differentiate every seat correctly, and an override keyed on the institutional power atom would misapply across the three distinct institutional seats (target, multilateral administrator, bilateral policeman) that share it. Patent holders' arbitrage capacity legitimately damps their EFFECTIVE extraction — launch sequencing and FTA lobbying blunt what they actually experience — without changing their structural position as the arrangement's target.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Reading the arrangement as pure rope ignores that a named, concentrated party (patent holders) pays through the same structure that coordinates everyone else — that is the tangled-rope signature, not frictionless coordination. Reading it as pure snare ignores that the coordination function is primary and verified: patients receive medicines they otherwise would not, and the arrangement survives scrutiny of its stated purpose. On the genealogy interview, the founding problem is CONTESTED rather than dead — the HIV-era emergency matured, but each new therapeutic class and pandemic reopens it — so the arrangement has not outlived its mandate and no zombie flag is warranted; the (contested-status x world_rearranges-verdict) pairing is coherent. The persistent risk is drift toward piton: if TRIPS-plus bilateral erosion continues outrunning multilateral codification, the flexibilities could survive as paper rights maintained theatrically — the theater_ratio series and the Article 31bis usability omega exist to catch exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_structural_delta,
    'This constraint is one reading of the trips_agreement_interpretive_kernel — what structurally changes if the strong_exclusivity_reading is adopted instead?',
    'Adoption of the sibling reading (through panel jurisprudence, treaty amendment, or coalition shift) would flip the beneficiary and victim sets: pharmaceutical patent holders move to beneficiary, generic manufacturers and health ministries move to victims, compulsory licensing contracts to narrow emergency exceptions, and epsilon recomputes over a different standing arrangement.',
    'Every classification output for this story is conditional on this reading holding; under the sibling reading the same treaty text yields a different constraint with inverted directionality and substantially higher extraction from the access side.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Committer structure: the structural delta this reading carries relative to its kernel sibling.').

omega_variable(
    interpretive_authority_location,
    'Where does binding interpretive authority over the TRIPS text actually sit — with dispute panels wielding retaliation, with ministerial consensus declarations, or with member autonomy — and does the answer survive the next major panel ruling touching compulsory licensing?',
    'Observe the next WTO panel or Appellate-body-era disposition that adjudicates a health flexibility measure, and whether membership responds by codifying around it (as with Article 31bis) or deferring to it.',
    'If binding authority consolidates in panels hostile to broad construction, this reading''s flexibilities become contingent on litigation outcomes and the arrangement drifts toward the sibling reading''s world; if ministerial codification remains the decisive register, the reading''s authority structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_location, conceptual, 'Location of interpretive authority — the axis contested by the third kernel reading.').

omega_variable(
    compulsory_license_remuneration_calibration,
    'Does Article 31(h) remuneration actually compensate patent holders for foregone exclusivity, or does the authored epsilon overstate (or understate) their net loss once royalties paid on issued licenses are counted?',
    'Audit of royalty terms across all issued compulsory licenses (Thai, Indian, Malaysian, Indonesian cases) against counterfactual monopoly pricing in the same markets.',
    'If remuneration approaches monopoly returns, effective extraction from patent holders falls and the arrangement reads closer to rope; if remuneration is nominal, the extraction component is larger than authored and the payer seat''s burden is understated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_license_remuneration_calibration, empirical, 'Calibration of the extraction component against actual license royalty terms.').

omega_variable(
    trips_plus_erosion_vs_codification_race,
    'Will bilateral TRIPS-plus provisions continue narrowing the effective flexibility space faster than multilateral codification (Doha-class declarations, Article 31bis, future waivers) expands it?',
    'Comparative tracking of FTA intellectual-property chapters against TRIPS Council decisions over the coming decade, measuring net effective scope of compulsory licensing and parallel import rights.',
    'If erosion wins, the arrangement drifts toward piton — paper rights maintained theatrically while effective scope shrinks; if codification wins, the arrangement consolidates as a stable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_plus_erosion_vs_codification_race, empirical, 'The race between bilateral narrowing and multilateral entrenchment that determines lifecycle direction.').

omega_variable(
    art31bis_export_mechanism_usability,
    'Is the Article 31bis Paragraph 6 export mechanism structurally usable by importing countries lacking manufacturing capacity, or is it procedural theater?',
    'Observe the next serious invocation attempt: if transaction costs (notification chains, tripartite contracting, anti-diversion requirements) again prevent use under real crisis conditions, the mechanism is performance; a streamlined successful use would establish function.',
    'If the mechanism is theater, the theater_ratio is understated for the export component and the least-developed-country beneficiary claim weakens substantially; if usable, the arrangement''s coordination function extends to the countries that need it most.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(art31bis_export_mechanism_usability, empirical, 'Usability of the sole-to-date-unused-in-crisis export flexibility machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__public_health_flexibility_reading, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(trip_tr_t3, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 3, 0.2).
narrative_ontology:measurement(trip_tr_t6, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(trip_tr_t9, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 9, 0.26).
narrative_ontology:measurement(trip_tr_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(trip_tr_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(trip_tr_t18, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 18, 0.36).
narrative_ontology:measurement(trip_tr_t21, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 21, 0.34).
narrative_ontology:measurement(trip_tr_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement(trip_tr_t27, trips_agreement_interpretive_kernel__public_health_flexibility_reading, theater_ratio, 27, 0.3).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(trip_be_t3, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 3, 0.28).
narrative_ontology:measurement(trip_be_t6, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement(trip_be_t9, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 9, 0.44).
narrative_ontology:measurement(trip_be_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement(trip_be_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 15, 0.39).
narrative_ontology:measurement(trip_be_t18, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 18, 0.37).
narrative_ontology:measurement(trip_be_t21, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 21, 0.43).
narrative_ontology:measurement(trip_be_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement(trip_be_t27, trips_agreement_interpretive_kernel__public_health_flexibility_reading, base_extractiveness, 27, 0.47).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement(trip_su_t3, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 3, 0.33).
narrative_ontology:measurement(trip_su_t6, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(trip_su_t9, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 9, 0.56).
narrative_ontology:measurement(trip_su_t12, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(trip_su_t15, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 15, 0.57).
narrative_ontology:measurement(trip_su_t18, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 18, 0.59).
narrative_ontology:measurement(trip_su_t21, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 21, 0.61).
narrative_ontology:measurement(trip_su_t24, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(trip_su_t27, trips_agreement_interpretive_kernel__public_health_flexibility_reading, suppression_requirement, 27, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__public_health_flexibility_reading, resource_allocation).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__public_health_flexibility_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the trips_agreement_interpretive_kernel. The colloquial label 'the TRIPS Agreement' conflates three structurally distinct claims: who holds binding interpretive authority over the text (dispute_settlement_interpretive_authority), what the text mandates about patent strength (strong_exclusivity_reading), and what the text permits for public health (this file). Each reading instantiates a different constraint with its own epsilon, beneficiary/victim structure, and classification; forcing them into one story would make epsilon observer-dependent, violating epsilon-invariance. The authority reading is upstream (it determines whose interpretation binds), and this reading both competes with the exclusivity reading (coexists_with — opposing coalitions hold both live) and exerts structural pressure on the authority reading (influences — the Doha/Article 31bis codification path shifted legitimacy away from exclusive panel adjudication without eliminating it).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
