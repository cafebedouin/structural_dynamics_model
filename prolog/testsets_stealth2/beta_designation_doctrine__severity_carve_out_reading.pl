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
 *   constraint_id: beta_designation_doctrine__severity_carve_out_reading
 *   human_readable: Beta Designation Doctrine — Severity Carve-Out Reading (Critical-Systems Liability Floor)
 *   domain: legal/technological
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   beta_designation_doctrine kernel: the severity carve-out reading, under
 *   which the beta label is categorically unavailable as a liability shield
 *   for life-safety, financial, and other critical systems, regardless of how
 *   genuine the testing phase or how candid the disclosure. Per the
 *   epsilon-invariance principle, the colloquial label 'beta designation'
 *   covers three structurally distinct liability allocations, authored as
 *   three linked stories: the expansive shield reading (waiver everywhere,
 *   indefinite), the narrow warning reading (time-bounded disclosure with
 *   base liability preserved), and this severity carve-out. Each has its own
 *   epsilon, its own beneficiary/victim structure, and its own
 *   classification; this file authors only the carve-out reading and links to
 *   its siblings through network edges. EPSILON REFERENT: the standing
 *   arrangement under contest is the severity carve-out rule itself, as
 *   instantiated where it governs critical-software deployment (court
 *   refusals to enforce beta disclaimers in medical, aviation, and financial
 *   contexts; software-inclusive product-liability codifications), assessed
 *   by this reading's own lights — the pro-safety-floor position — and not
 *   scored against either sibling reading or against the reading's endorsed
 *   ideal.
 *
 * KEY AGENTS:
 *   - - critical_software_vendors: Primary target (powerful/constrained) — bears the liability exposure the rule compels in its core market
 *   - - critical_market_entrants: Secondary target (moderate/mobile) — bears exclusion from critical markets they cannot insure into
 *   - - critical_system_end_users: Primary beneficiary (powerless/trapped) — holds the recourse floor and safer-deployment incentives
 *   - - injured_critical_system_claimants: Compensation recipients (powerless/trapped) — receive the transferred recoveries case by case
 *   - - plaintiffs_bar: Fee collector (organized/mobile) — takes a percentage of every preserved recovery
 *   - - liability_insurers: Near-symmetric intermediary (organized/mobile) — collects premiums and bears payouts under the same regime
 *   - - product_safety_regulators, liability_courts, product_liability_legislatures: Agenda-setting seats (institutional/constrained) — administer, adjudicate, and legislate the rule
 *   - - early_adopter_institutions and open_source_maintainers: Excluded voices — would bargain or seek safe harbors but are outside the doctrinal conversation
 *   - - software_liability_scholars: Analytical observer — maps the allocation without holding a stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, 0.52).
domain_priors:suppression_score(beta_designation_doctrine__severity_carve_out_reading, 0.55).
domain_priors:theater_ratio(beta_designation_doctrine__severity_carve_out_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(beta_designation_doctrine__severity_carve_out_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__severity_carve_out_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__severity_carve_out_reading, "Beta Designation Doctrine — Severity Carve-Out Reading (Critical-Systems Liability Floor)").
narrative_ontology:topic_domain(beta_designation_doctrine__severity_carve_out_reading, "legal/technological").

domain_priors:requires_active_enforcement(beta_designation_doctrine__severity_carve_out_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__severity_carve_out_reading, '3738cde6-a811-46bc-8cf9-fc1564d39bc5').
narrative_ontology:cs_kernel_codification('3738cde6-a811-46bc-8cf9-fc1564d39bc5', distributed).
narrative_ontology:cs_authority_grounding('3738cde6-a811-46bc-8cf9-fc1564d39bc5', practice).
narrative_ontology:cs_interpretation_layer_present('3738cde6-a811-46bc-8cf9-fc1564d39bc5').
narrative_ontology:cs_reading_relation('3738cde6-a811-46bc-8cf9-fc1564d39bc5', beta_designation_doctrine__expansive_shield_reading, forecloses).
narrative_ontology:cs_reading_relation('3738cde6-a811-46bc-8cf9-fc1564d39bc5', beta_designation_doctrine__narrow_warning_reading, influences).
narrative_ontology:cs_axiom('3738cde6-a811-46bc-8cf9-fc1564d39bc5', foundational, harm_severity_overrides_contractual_allocation).
narrative_ontology:cs_axiom_status(harm_severity_overrides_contractual_allocation, holdable).
narrative_ontology:cs_axiom_grounding('3738cde6-a811-46bc-8cf9-fc1564d39bc5', harm_severity_overrides_contractual_allocation, deontological).
narrative_ontology:cs_axiom('3738cde6-a811-46bc-8cf9-fc1564d39bc5', secondary, beta_label_no_defense_in_critical_systems).
narrative_ontology:cs_axiom_status(beta_label_no_defense_in_critical_systems, holdable).
narrative_ontology:cs_axiom_grounding('3738cde6-a811-46bc-8cf9-fc1564d39bc5', beta_label_no_defense_in_critical_systems, conventional).
narrative_ontology:cs_reference_frame('3738cde6-a811-46bc-8cf9-fc1564d39bc5', severity_override_safety_floor).
narrative_ontology:cs_drift_state('3738cde6-a811-46bc-8cf9-fc1564d39bc5', contemporary_post_pld_revision, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3738cde6-a811-46bc-8cf9-fc1564d39bc5', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, injured_critical_system_claimants).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, plaintiffs_bar).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__severity_carve_out_reading, liability_insurers).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, critical_software_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, critical_market_entrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(beta_designation_doctrine__severity_carve_out_reading, liability_insurers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, harm_severity_overrides_contractual_allocation).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__severity_carve_out_reading, deployment_decision_internalization).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build and sell the software that runs hospital records, trading and payment rails, aircraft and industrial controls. Where this rule applies they carry full product-liability exposure for defects in those deployments: the beta label protects them not at all, so they fund pre-release assurance, insurance, and reserve accounts, and they sometimes hold features back until maturity. Escaping the exposure would mean leaving the critical-systems market, which for most of them is their core business.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_software_vendors, payer,
    powerful, biographical, constrained, global).

% Startups and smaller firms trying to bring new tools into hospitals, banks, and infrastructure operators. Full liability exposure at their scale is difficult or impossible to insure, so access to these markets effectively requires capital reserves or partnerships they rarely have. Many respond by selling first into non-critical niches and entering critical markets later, if at all.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_market_entrants, payer,
    moderate, immediate, mobile, global).

% Patients whose care runs on clinical software, customers whose money moves through bank software, passengers flying on aircraft controlled by certified code. They cannot inspect, test, or negotiate the terms of the software their lives and savings depend on, and they cannot opt out of the hospital or the banking system. The rule keeps a floor of recourse available to them when deployed software causes harm.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, critical_system_end_users, beneficiary,
    powerless, biographical, trapped, global).

% People already harmed by defective software in a critical setting. Through the preserved liability channel they can pursue compensation from the vendor whose product injured them; recoveries arrive after the harm, case by case, and almost none of them anticipated litigation. They do not organize around or administer the arrangement; they encounter it only as injured parties.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, injured_critical_system_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Law firms and attorneys who take critical-system injury cases on contingency. Every recovery under the preserved liability channel carries a percentage fee to them, so the volume of viable claims shapes their practice economics. They can and do shift into other practice areas when liability channels narrow.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, plaintiffs_bar, beneficiary,
    organized, biographical, mobile, national).

% Underwrite errors-and-omissions and product-liability coverage priced against this liability regime. Premium inflows rise with vendor exposure; payout obligations rise with the same events. Their net position sits near break-even plus loading, and they can withdraw from the line entirely if pricing turns adverse.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, liability_insurers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__severity_carve_out_reading, liability_insurers, payer).

% Sector agencies — medical device, banking, aviation authorities — that police software deployment in their domains and increasingly treat vendor testing claims skeptically. They issue guidance, require post-market surveillance, and can block deployments. They operate inside statutory mandates and budgets set elsewhere.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, product_safety_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Judges deciding whether a beta disclaimer bars a critical-system injury claim. Under this reading they refuse to enforce such disclaimers in life-safety and financial contexts regardless of how candid the disclosure was. They are bound by precedent and statute, and courts in different jurisdictions reach opposite results on similar facts.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, liability_courts, agenda_setter,
    institutional, generational, constrained, national).

% Enact and revise the liability statutes the rule lives in — most recently extending product-liability regimes to cover software explicitly. Vendor associations lobby them to preserve contractual freedom; consumer and patient-safety groups lobby for the floor. Reversal is procedurally straightforward and politically expensive.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, product_liability_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Hospitals, trading firms, and infrastructure operators that would accept documented risk in exchange for early, cheaper access to promising but unproven tools. The rule closes that bargain: courts will not enforce waivers covering critical-system harm, so the discount-for-risk deal they would offer is unavailable. They press their case in comment letters and pilot programs rather than courtrooms.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, early_adopter_institutions, excluded,
    powerful, biographical, constrained, global).

% Volunteer developers and maintainers whose code ends up, sometimes without their knowledge, inside critical commercial systems. They contribute without payment and without negotiating deployment terms; proposals to extend strict liability toward upstream contributors land on them with no revenue to insure against. They argue for safe harbors and are heard, if at all, in standards bodies and legislative comment periods.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, open_source_maintainers, excluded,
    moderate, biographical, mobile, global).

% Legal academics and policy researchers who map how liability doctrines allocate software risk across jurisdictions. They publish comparative analyses of disclaimer enforceability, advise law-reform commissions, and supply the empirical baselines the other seats argue over. They hold no stake in any outcome beyond the argument itself.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__severity_carve_out_reading, software_liability_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__severity_carve_out_reading, injured_critical_system_claimants).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__severity_carve_out_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the risk-allocation problem of high-stakes software deployment: individual users of hospital, banking, aviation, and infrastructure software cannot inspect, test, or negotiate liability terms, so the rule assigns residual harm risk to the deploying vendor and preserves a common floor of recourse, making deployment decisions carry their own harm costs.
% TRANSFER_FUNCTION: Moves liability-backed compensation from vendors deploying software in critical domains to users harmed by defects, along with the burden of pre-deployment assurance; incidentally moves percentage fees to litigation intermediaries and premium volume to insurers.
% ABSENT_VOICES: Early-adopter institutions that would trade documented risk acceptance for cheaper early access, open-source maintainers facing upstream liability exposure with no revenue, and patients who would accept informed risk for faster access to experimental clinical software. They stand outside the courtrooms and legislative hearings where the doctrine is argued, which are dominated by vendor associations on one side and consumer and patient-safety organizations on the other.
% DISAPPEARANCE_RATIONALE: Vendor counsel would reinstate beta disclaimers across critical deployments within a quarter; hospitals, banks, and operators would face contract-and-warranty-only recourse after software harm; under-tested releases would reach critical settings faster because the assurance costs the rule compels become optional; and uncompensated harm would migrate to users, their insurers, and public payers.
% FOUNDING_PROBLEM: Vendors began shipping visibly unfinished software into hospitals, trading floors, and infrastructure under a beta label and enforcing the label as a disclaimer of responsibility for foreseeable harm, leaving injured users with no recovery path.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: published court opinions refusing to enforce beta disclaimers in medical and financial contexts, regulator incident reporting such as health-IT safety-event databases, and the legislative findings accompanying software-inclusive product-liability reforms. No source outside the benefiting parties attests that the founding problem is resolved.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__severity_carve_out_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__severity_carve_out_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__severity_carve_out_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__severity_carve_out_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__severity_carve_out_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__severity_carve_out_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__severity_carve_out_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. CLAIMED TYPE tangled_rope comes from structure: the rule solves a genuine allocation problem (users of critical systems cannot negotiate or assess software risk, so residual harm risk needs a default bearer), AND identifiable parties pay through the same structure (vendors bearing exposure, entrants bearing exclusion), AND the rule holds only through active enforcement (courts must refuse disclaimers; regulators must police labeling; legislatures must sustain statutes). METRICS are authored descriptively. Extractiveness 0.52: under the reading's own lights the vendor-to-claimant transfer is largely corrective — it tracks harm the vendor's product caused — but it carries plainly non-corrective components: non-fault strict-liability exposure, litigation overhead, insurance loading, and the market-exclusion cost borne by entrants, which yields a moderate rather than low epsilon. Suppression 0.55 is a RAW structural property, unscaled by power or scope (only extractiveness is scaled, by directionality and scope, in the engine): the rule forecloses one specific exit — the disclaimer — and backs the foreclosure with active multi-forum enforcement, while leaving vendors substantive alternatives (mature-before-release, insurance, contractual indemnities, non-critical niches). Theater_ratio 0.22: the rule's activity is mostly functional; the residual ceremony sits in conformity paperwork and certification rituals that signal assurance without always producing it. Accessibility_collapse 0.45: alternatives remain workable once the rule is understood, so it is far from a natural-law profile. Resistance 0.55: sustained industry lobbying, amicus campaigns, and jurisdiction shopping meet the rule continuously. MEASUREMENTS run on one shared time grid (points 0-24, roughly 2001-2025, quarterly-century of software-liability jurisprudence): extractiveness creeps up as liability deepens and codifies; theater declines as scattered dicta consolidate into operative doctrine; suppression_requirement rises as enforcement machinery matures — courts hardening, regulators staffing surveillance, statutes absorbing software explicitly. The rising suppression series is authored because the story specifically traces enforcement-capacity buildup, not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical structural data. From the vendor seat the rule is a compelled transfer it cannot contract around — high directionality, constrained exit, extractive-heavy experience. From the end-user and claimant seats the same rule is the only thing standing between them and a disclaimer — protective, near-subsidy. From the courts and regulators the rule is ordinary administration of a settled allocation. From the entrants it is a barrier to entry dressed as consumer protection. From plaintiffs_bar it is practice economics. The engine computes this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. critical_software_vendors and critical_market_entrants are declared victims and sit near the full-target end — vendors because their exit is constrained (leaving means abandoning their core market), entrants less so because they retain a mobile exit into non-critical niches. critical_system_end_users and injured_critical_system_claimants are declared beneficiaries with trapped exit — the constraint subsidizes them and they cannot arbitrage it. plaintiffs_bar is a beneficiary whose collections scale with the liability flow (low directionality). liability_insurers are dual-positioned: declared beneficiary (premium volume) with a payer secondary role (payout obligations); their true net position is near-symmetric, and the derivation likely understates their directionality — a known limitation accepted here rather than papered over with a coarse power-atom-level override, since any override keyed to 'organized' would also distort plaintiffs_bar. Agenda-setting seats (regulators, courts, legislatures) are neither beneficiaries nor victims; their directionality follows the power-atom fallback. No directionality_overrides are authored: the declared structural data produces the right qualitative ordering, and the one imprecise case (insurers) is documented instead of forced.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Read without its coordination function, the rule looks like pure extraction from a hated industry — a snare whose victims are vendors; read without its payer structure, it looks like a pure consumer boon — a rope with no costs. The tangled_rope framing keeps both visible: the risk-internalization function is real and load-bearing, and the vendor/entrant payments are real and partly non-corrective. On obsolescence: the founding problem remains live (under-tested software still reaches critical settings, and disclaimer attempts continue), so no mandatrophy is declared. The forward risk is different — if formal verification and simulation mature to the point that pre-release testing genuinely exhausts the failure space, the carve-out's justification decays into ritual while its costs persist; the declining theater series and the corrective-versus-rent omega are the instruments that would catch that transition. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: consistent, no zombie flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the beta_designation_doctrine kernel; what exactly would each sibling reading change structurally, and where precisely is the disagreement located?',
    'Comparative classification of the three readings as separate stories: expansive_shield_reading concentrates costs on users and drives vendor-side epsilon toward zero; narrow_warning_reading makes protection duration-contingent so post-release defects fall outside it; this reading makes protection domain-contingent. The disagreement is located in whether harm severity can override contractual allocation at all.',
    'If the expansive reading prevails, critical-system users lose the recourse floor and this story''s beneficiary structure inverts; if the narrow-warning reading prevails, protection becomes a function of release timing rather than harm severity. This story''s classification holds only while the severity-override premise is the operative one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: this file is one of three readings of the beta-designation kernel; siblings are separate constraints.').

omega_variable(
    criticality_boundary_underdetermination,
    'Which systems count as ''life-safety, financial, or other critical,'' and who decides the boundary?',
    'Accumulation of case law and regulatory enumerations — sectoral classification lists, product-liability annexes, medical-device categorizations — converging on a administrable boundary.',
    'A wide boundary extends the liability floor to ordinary business software and raises vendor-side costs across the economy; a narrow boundary confines the rule to avionics-class systems and leaves most deployments shieldable. The effective scope of the extraction the rule performs scales with this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(criticality_boundary_underdetermination, conceptual, 'The categorical trigger of the carve-out is under-determined at its edges.').

omega_variable(
    corrective_vs_rent_decomposition,
    'Is the money moved from vendors to the claimant side corrective (tracking harm actually caused) or rent-generating (litigation overhead, non-fault exposure, and intermediary fees exceeding the value of services rendered)?',
    'Actuarial and court-record decomposition of recovery flows: compensation share versus contingency fees, defense costs, and insurance loading, benchmarked against comparable non-software product-liability lines.',
    'If the rent share dominates, the arrangement drifts toward enforced transfer with a weakening coordination justification and the computed classification slides toward the extractive pole; if compensation dominates, the coordination reading strengthens and the moderate epsilon is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corrective_vs_rent_decomposition, empirical, 'Whether the vendor-side payment stream is primarily corrective or primarily rent.').

omega_variable(
    innovation_path_suppression_tradeoff,
    'Does closing the beta route in critical domains destroy a valuable controlled-testing pathway, delaying beneficial deployment of improving software in hospitals and finance?',
    'Difference-in-difference on deployment rates and adverse-event rates across jurisdictions that adopt or reject severity carve-outs, controlling for sector and firm size.',
    'Large chilling effects would shrink the coordination benefit and raise the net-extraction reading of vendor costs; negligible effects would confirm the floor as low-cost protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_path_suppression_tradeoff, empirical, 'Whether the suppressed beta pathway carried real value the rule destroys.').

omega_variable(
    jurisdictional_instantiation_variance,
    'The reading is instantiated unevenly across forums — courts split by state, the EU codifying software into product liability, sectoral regulators diverging. Which instantiation does the authored epsilon describe?',
    'Forum-by-forum sub-stories or scoped variants joined by network edges, each with its own enforcement picture and epsilon.',
    'Epsilon and suppression vary materially by forum; a global-scope classification overstates uniformity and misweights enforcement intensity, potentially dating type transitions incorrectly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_instantiation_variance, empirical, 'Cross-forum variance in how the severity carve-out actually operates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__severity_carve_out_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(beta_tr_t0, observed).
narrative_ontology:measurement(beta_tr_t4, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement_basis(beta_tr_t4, observed).
narrative_ontology:measurement(beta_tr_t8, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement_basis(beta_tr_t8, observed).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(beta_tr_t12, observed).
narrative_ontology:measurement(beta_tr_t16, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement_basis(beta_tr_t16, observed).
narrative_ontology:measurement(beta_tr_t20, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(beta_tr_t20, observed).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__severity_carve_out_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement_basis(beta_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(beta_be_t0, observed).
narrative_ontology:measurement(beta_be_t4, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement_basis(beta_be_t4, observed).
narrative_ontology:measurement(beta_be_t8, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 8, 0.44).
narrative_ontology:measurement_basis(beta_be_t8, observed).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement_basis(beta_be_t12, observed).
narrative_ontology:measurement(beta_be_t16, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement_basis(beta_be_t16, observed).
narrative_ontology:measurement(beta_be_t20, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(beta_be_t20, observed).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__severity_carve_out_reading, base_extractiveness, 24, 0.52).
narrative_ontology:measurement_basis(beta_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(beta_su_t0, observed).
narrative_ontology:measurement(beta_su_t4, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 4, 0.34).
narrative_ontology:measurement_basis(beta_su_t4, observed).
narrative_ontology:measurement(beta_su_t8, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 8, 0.39).
narrative_ontology:measurement_basis(beta_su_t8, observed).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(beta_su_t12, observed).
narrative_ontology:measurement(beta_su_t16, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement_basis(beta_su_t16, observed).
narrative_ontology:measurement(beta_su_t20, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(beta_su_t20, observed).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__severity_carve_out_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement_basis(beta_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__severity_carve_out_reading, resource_allocation).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__expansive_shield_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__severity_carve_out_reading, beta_designation_doctrine__narrow_warning_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'beta designation doctrine' per the epsilon-invariance principle. The single natural-language concept covers three structurally distinct liability allocations with materially different epsilon values and different victim sets: expansive_shield_reading (waiver everywhere; epsilon concentrated on users of all software), narrow_warning_reading (time-bounded disclosure; epsilon duration-dependent), and this severity_carve_out_reading (domain-categorical unavailability; epsilon moderate, borne by critical-domain vendors and entrants). The expansive reading is historically upstream — the older, broader position from which the other two depart — and its decline supplies the revival pressure recorded in this reading's drift_state. All three files link one another through affects_constraints; none is evaluable via another's observables without changing epsilon, which is why they are separate stories rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
