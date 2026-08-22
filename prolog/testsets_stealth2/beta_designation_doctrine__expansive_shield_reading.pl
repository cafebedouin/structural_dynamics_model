% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Beta Designation Doctrine — Expansive Shield Reading
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   This story instantiates the expansive_shield_reading of the
 *   beta_designation_doctrine kernel: the claim that attaching a beta label
 *   legally displaces developer liability — comprehensively, for any defect,
 *   for an unlimited duration, in every software context including
 *   life-safety deployments. Operationally, vendors attach standardized
 *   clickwrap terms declaring the product beta; courts enforcing assent treat
 *   the label as a complete waiver; users and deploying organizations bear
 *   every resulting cost, from data loss and breaches to outages and physical
 *   harm. The claim/metric relationship is deliberately unreconciled:
 *   claimed_type tangled_rope reflects the genuine early-access coordination
 *   core the label still performs, while the authored metrics describe
 *   heavily extractive operation that has been drifting toward pure
 *   cost-shifting as the label's informational content decays. Per the
 *   epsilon-invariance principle this file authors ONE reading with one
 *   stable epsilon; the sibling readings are separate constraints linked
 *   through network.affects_constraints, and the kernel contest is routed to
 *   omegas rather than hedged inside this story.
 *
 * KEY AGENTS:
 *   - major_software_vendors: agenda-setter and primary beneficiary (institutional/arbitrage) — drafts the terms, controls the label, collects the avoided liability
 *   - independent_developers: secondary beneficiary (moderate/mobile) — rides the normalized waiver without drafting leverage
 *   - beta_software_users: primary target (powerless/constrained) — bears consumer-scale defect costs under non-negotiable clickwrap
 *   - enterprise_licensees: institutional target (powerful/constrained) — absorbs outage and integration costs despite bargaining power, because the liability clause itself is off the table
 *   - safety_critical_operators: extreme target (organized/trapped) — carries catastrophic-harm costs in life-safety contexts with no substitute and no recourse
 *   - consumer_protection_agencies: analytical observer (institutional/analytical) — the seat whose remedies would alter which terms survive enforcement
 *   - liability_insurers: excluded voice (institutional/mobile) — the risk-pricing disciplinarian the waiver designs out of the conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.8).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.63).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, tangled_rope).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation Doctrine — Expansive Shield Reading").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, '1e22881a-427f-49ce-b253-a5ea124a3954').
narrative_ontology:cs_kernel_codification('1e22881a-427f-49ce-b253-a5ea124a3954', formalized).
narrative_ontology:cs_authority_grounding('1e22881a-427f-49ce-b253-a5ea124a3954', extraction).
narrative_ontology:cs_interpretation_layer_present('1e22881a-427f-49ce-b253-a5ea124a3954').
narrative_ontology:cs_reading_relation('1e22881a-427f-49ce-b253-a5ea124a3954', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('1e22881a-427f-49ce-b253-a5ea124a3954', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('1e22881a-427f-49ce-b253-a5ea124a3954', foundational, beta_label_discharges_all_defect_liability).
narrative_ontology:cs_axiom_status(beta_label_discharges_all_defect_liability, holdable).
narrative_ontology:cs_axiom_grounding('1e22881a-427f-49ce-b253-a5ea124a3954', beta_label_discharges_all_defect_liability, conventional).
narrative_ontology:cs_axiom('1e22881a-427f-49ce-b253-a5ea124a3954', secondary, waiver_unbounded_by_duration_or_severity).
narrative_ontology:cs_axiom_status(waiver_unbounded_by_duration_or_severity, holdable).
narrative_ontology:cs_axiom_grounding('1e22881a-427f-49ce-b253-a5ea124a3954', waiver_unbounded_by_duration_or_severity, conventional).
narrative_ontology:cs_reference_frame('1e22881a-427f-49ce-b253-a5ea124a3954', comprehensive_waiver_baseline).
narrative_ontology:cs_drift_state('1e22881a-427f-49ce-b253-a5ea124a3954', contemporary_consumer_protection_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1e22881a-427f-49ce-b253-a5ea124a3954', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, major_software_vendors).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, independent_developers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, beta_software_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, enterprise_licensees).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, safety_critical_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts the standardized beta terms and clickwrap licenses, decides which products carry the label and for how long, and enforces the waiver through accepted-contract doctrine and mandatory arbitration clauses. Avoided liability shows up as retained revenue and shifted risk; the same firms can reposition a product as released, or re-badge a released product as beta, whenever the label's legal posture is favorable.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, major_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, major_software_vendors, beneficiary).

% Ships software under the same label conventions without carrying product-liability insurance, relying on the normalized practice to make experimentation affordable. Benefits from the arrangement but did not draft its terms; moving to paid, warranted products means absorbing insurance and support costs most small shops cannot carry.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, independent_developers, beneficiary,
    moderate, biographical, mobile, global).

% Accepts the license terms by clicking through, without negotiation or realistic comprehension of the waiver's breadth, to reach software that work, school, or social life runs on. Bears data loss, account compromise, and outage costs directly; the practical alternative to the terms is abstaining from widely adopted tools.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, beta_software_users, payer,
    powerless, immediate, constrained, global).

% Deploys beta-tagged business platforms under vendor paper negotiated at scale, yet the liability clause itself is rarely negotiable. Absorbs integration failures, downtime, and remediation costs; deep ecosystem lock-in and migration expense make switching a multi-year project rather than an available remedy.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, enterprise_licensees, payer,
    powerful, biographical, constrained, continental).

% Runs software carrying the beta label inside hospital, utility, and industrial control workflows where a defect can injure or kill. Certification cycles, vendor monocultures, and procurement timelines mean no realistic substitute exists; under the expansive reading the operator carries the full cost of catastrophic defects with no recourse against the supplier.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, safety_critical_operators, payer,
    organized, generational, trapped, national).

% Investigates whether blanket waivers attached to a label are enforceable against consumers, takes testimony from the other seats, and in some jurisdictions is extending product-liability regimes to software by rulemaking. Its remedies would alter which terms survive enforcement.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_agencies, observer,
    institutional, generational, analytical, continental).

% Would otherwise price software defect risk through premiums and exclusions, channeling investment toward safer engineering. Comprehensive waivers make the risk contractually invisible, so the insurer's disciplining voice is designed out of the conversation; it objects from outside a process it cannot currently enter.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, liability_insurers, excluded,
    institutional, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, major_software_vendors).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The beta label coordinates expectations between suppliers and early users: it signals that the product is unfinished, recruits field testing and bug reports, and prices access (free or cheap) against tolerance of defects. Under this reading the same label additionally settles, in advance, who pays when the defects arrive.
% TRANSFER_FUNCTION: Moves defect costs — data loss, security breaches, outages, integration failures, and physical harm in embedded contexts — from the software supplier to everyone using the labeled product, and converts user troubleshooting and workaround labor into unpaid quality-assurance work flowing back to the supplier.
% ABSENT_VOICES: End users who never read or understand the waiver; safety-critical operators without procurement leverage over vendor paper; insurers whose risk-pricing function the waiver renders moot; and future victims of defects in software kept permanently beta. None of these seats is represented where the terms are drafted.
% DISAPPEARANCE_RATIONALE: If the comprehensive waiver vanished overnight, suppliers would reprice risk through insurance, slower release cadences, and faster patching; litigation volume would surge until a new liability equilibrium formed; insurers would re-enter software risk markets; and release practices would reorganize around genuine testing phases. The software economy as currently arranged depends on the waiver holding.
% FOUNDING_PROBLEM: Early commercial software shipped with inevitable defects under tort regimes that threatened ruinous liability for any failure; beta designation emerged to let developers distribute genuinely experimental software and gather field feedback without unlimited exposure.
% FOUNDING_PROBLEM_CORROBORATION: Academic legal scholarship on adhesion contracts and clickwrap enforceability, judicial opinions declining to enforce blanket waivers, and consumer-protection rulemaking in the US and EU — all outside the benefiting parties — attest that the original narrow problem (liability chill on genuinely experimental releases) persists only at the margin while the doctrine now shields production software indefinitely. Vendor trade associations alone attest that the founding problem remains fully live.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.80 at interval end) because the waiver is comprehensive and unconditional: every defect cost lands on the using party regardless of negligence, severity, or how long the 'testing' phase nominally lasts. Suppression (0.63) is authored as a raw structural property — enforced-standard-form contracting, mandatory arbitration, and the absence of any negotiated alternative — and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by the engine, through directionality and scope. Theater_ratio (0.45) tracks the growing share of the label's operation that is performative: a 'beta' badge on production infrastructure performs ongoing testing while functioning as a permanent legal instrument. Accessibility_collapse (0.50) is moderate because consumer-segment alternatives partly persist while enterprise and embedded segments approach closure. Resistance (0.55) reflects sustained but so far unsuccessful pushback: unconscionability litigation, consumer-protection rulemaking, and the EU's extension of product liability toward software. The measurement series run on one shared grid (every tracked metric authored at t=0,4,8,12,16,20,24) so no metric's end-state leaks backward into earlier rows; the trajectories are monotonic rather than cyclical — enforcement machinery hardened and the label's legal function grew steadily over the interval, with no oscillation phase to model.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the vendor seat the arrangement is earned contract freedom: disclosed terms, accepted consideration, innovation protected from ruinous tort exposure. From the user and operator seats the same structure is unilateral risk imposition entered through a click nobody reads, with the exit door bricked by lock-in and certification cycles. Courts occupy an intermediate seat, enforcing assent as such while occasionally refusing particular clauses. Note the coalition asymmetry: the natural coalition channel for powerless users is class litigation, and the expansive waiver travels in practice alongside arbitration and class-action-waiver clauses that close precisely that channel — the engine should register the users' low power as durable rather than transient. The authored claim does not adjudicate among these seats; the per-seat classifications are computed from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Major software vendors sit at the beneficiary pole (d near 0.0): they collect the avoided liability and hold arbitrage-grade exit, able to relabel or restructure around the doctrine at will. Independent developers derive low d as incidental beneficiaries. Beta software users sit high on the target axis (constrained exit, no negotiation). Enterprise licensees are the instructive case of power without exit: globally powerful organizations whose d nonetheless approaches the full-target end because the specific clause is non-negotiable and switching costs are prohibitive. Safety-critical operators sit nearest full target (d near 1.0): trapped by certification cycles and vendor monocultures into bearing the largest absolute harms. Agencies and insurers hold analytical and excluded positions respectively — neither pays nor collects through the arrangement's operation. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — letting genuinely experimental software reach users without destroying the developer — survives only at the margin, and the parties dispute even that. Classifying this as tangled_rope rather than snare prevents erasing the real coordination the label still performs for authentic early-access exchanges; simultaneously, the rising theater_ratio and extractiveness series document the mandate decoupling from operation, as the waiver persists long after the 'testing' it nominally enables has become indefinite. The R5 mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: the world does depend on the arrangement, but the parties dispute whether the problem it was built for is still the one it solves — no clean zombie flag fires, and none should. The severity_carve_out_reading sibling exists precisely because the mandate's failure is starkest where harm is catastrophic; that decomposition, not a forced single verdict, is how the classification apparatus keeps the hybrid honest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the expansive_shield_reading of the beta_designation_doctrine kernel; what structural changes would adoption of a sibling reading produce?',
    'Author and compile the sibling stories (beta_designation_doctrine__narrow_warning_reading, beta_designation_doctrine__severity_carve_out_reading) and compare computed per-seat classifications and epsilon across the family.',
    'Under narrow_warning_reading the affected-user set shrinks to harms during a genuine testing phase and base product liability returns afterward, pulling epsilon toward ordinary exchange; under severity_carve_out_reading safety_critical_operators exit the cost-bearing set entirely, removing the highest-severity component of the measured burden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Kernel contest: sibling readings redraw the victim set and epsilon of the beta-label arrangement.').

omega_variable(
    clickwrap_consent_fiction,
    'Does click-through assent to a comprehensive beta waiver constitute informed consent, or a procedural fiction that no user reads or understands?',
    'Comprehension studies and behavioral data on license-term reading rates; the litigation record on unconscionability and enforceability findings across jurisdictions.',
    'If assent is fictional, the measured suppression understates the arrangement''s coercive character and the structure trends toward pure imposed risk; if assent is meaningful, part of the cost transfer is priced exchange the users knowingly bought.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clickwrap_consent_fiction, empirical, 'Whether the waiver rests on real consent or procedural formality.').

omega_variable(
    label_signal_vs_legal_instrument,
    'Under the expansive reading, does the beta label retain genuine informational value as an unfinished-software signal, or does it operate purely as a liability instrument?',
    'User-expectation surveys comparing what ''beta'' conveys on perpetually-beta production products versus genuinely experimental releases, correlated against observed defect rates under each usage.',
    'If the signal is dead, the arrangement''s coordination story is cover and the constraint sits at the snare boundary; if the signal is live, the hybrid coordination-plus-cost-shift reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(label_signal_vs_legal_instrument, conceptual, 'Whether the label still coordinates expectations or only waives liability.').

omega_variable(
    internalized_defect_normalization,
    'Is user acquiescence to bearing defect costs structural (no negotiated alternative exists) or internalized (a normalized belief that software is unavoidably buggy and recourse pointless)?',
    'Post-reform trajectory in a jurisdiction where blanket waivers become unenforceable: if users immediately demand liability-backed products, acquiescence was structural; if tolerance persists unchanged, it was internalized.',
    'Internalization raises effective suppression above the structural measure and blunts the resistance metric; structural-only suppression predicts a rapid demand shift upon reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_defect_normalization, empirical, 'Structural versus internalized source of user acquiescence to defect costs.').

omega_variable(
    market_segment_collapse_heterogeneity,
    'How completely do alternatives actually collapse once the waiver practice is understood — do liability-bearing or non-beta competitors remain reachable in each market segment?',
    'Segment-by-segment audit of competitor license terms and measured switching feasibility across consumer applications, enterprise platforms, and embedded control systems.',
    'Near-total collapse in embedded and enterprise segments concentrates the burden on trapped seats and amplifies their effective position; reachable alternatives in consumer segments keep parts of the arrangement nearer voluntary exchange.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_segment_collapse_heterogeneity, empirical, 'Heterogeneity of alternative collapse across market segments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_expansive_shield_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(beta_expansive_shield_tr_t4, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(beta_expansive_shield_tr_t8, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(beta_expansive_shield_tr_t12, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 12, 0.33).
narrative_ontology:measurement(beta_expansive_shield_tr_t16, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(beta_expansive_shield_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(beta_expansive_shield_tr_t24, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 24, 0.45).

% Extraction over time
narrative_ontology:measurement(beta_expansive_shield_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(beta_expansive_shield_be_t4, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 4, 0.6).
narrative_ontology:measurement(beta_expansive_shield_be_t8, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(beta_expansive_shield_be_t12, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(beta_expansive_shield_be_t16, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(beta_expansive_shield_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(beta_expansive_shield_be_t24, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 24, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(beta_expansive_shield_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(beta_expansive_shield_su_t4, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(beta_expansive_shield_su_t8, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(beta_expansive_shield_su_t12, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 12, 0.56).
narrative_ontology:measurement(beta_expansive_shield_su_t16, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 16, 0.59).
narrative_ontology:measurement(beta_expansive_shield_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(beta_expansive_shield_su_t24, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 24, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, information_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the beta_designation_doctrine kernel per the epsilon-invariance principle: the colloquial label 'beta disclaimer' conflates three structurally distinct claims — comprehensive indefinite universal waiver (this story), time-bounded testing disclosure preserving base liability (narrow_warning_reading), and categorical unavailability for life-safety and financial systems (severity_carve_out_reading). Each carries its own epsilon, beneficiary/victim structure, and classification. The narrow warning reading is the upstream, higher-confidence claim historically cited as justification for the expansive one; the severity carve-out reading attacks the expansive reading at its highest-harm application. Edges here run from this reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
