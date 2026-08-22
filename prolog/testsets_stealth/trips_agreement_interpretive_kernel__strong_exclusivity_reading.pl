% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__strong_exclusivity_reading, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__strong_exclusivity_reading
 *   human_readable: TRIPS Strong Exclusivity Reading: Uniform Patent Protection Mandate with Narrowly Construed Flexibilities
 *   domain: international_trade_law/public_health_policy/intellectual_property
 *
 * SUMMARY:
 *   The TRIPS Agreement (1994) fixes a multilateral minimum standard for
 *   intellectual property protection binding on all WTO members. This story
 *   instantiates the strong_exclusivity_reading of the
 *   trips_agreement_interpretive_kernel: the treaty is read as mandating
 *   high, uniform patent protection - twenty-year product patents, narrowly
 *   gated Article 30/31 exceptions, compulsory licensing available only under
 *   tight conditions - with pharmaceutical innovation as the justifying end.
 *   Under this reading the standing arrangement coordinates a genuine
 *   collective-action problem (standards fragmentation, free-riding,
 *   IP-driven trade friction) while simultaneously extracting heavily from
 *   low-income states, their patients, and generic producers through monopoly
 *   pricing and constrained policy space. Per the kernel-reading epsilon
 *   rule, the referent of extractiveness is the strong-exclusivity
 *   arrangement itself as this reading holds it - the arrangement the story
 *   is about - never the sibling reading's endorsed flexibility regime. The
 *   sibling readings are separate constraint files, not hedges inside this
 *   one. Interval mapping: t=0 is 1995 (entry into force), t=30 is 2025; the
 *   Doha Declaration lands at t=6, the pharmaceutical transition expiry for
 *   developing countries at t=10, and the Appellate Body crisis and COVID
 *   waiver fight at t=24-27.
 *
 * KEY AGENTS:
 *   - pharmaceutical_patent_holders: Primary beneficiary (institutional/arbitrage) - collects patent-term exclusivity rents across all WTO jurisdictions; the receipt seat for the arrangement's gains
 *   - innovator_states: Beneficiary and enforcement driver (institutional/mobile) - initiate disputes, negotiate TRIPS-plus chapters, block waiver proposals
 *   - wto_dispute_settlement_bodies: Agenda setter (institutional/constrained) - adjudicate the fixed text; narrow construction operationalizes this reading
 *   - low_income_states: Primary target (moderate/trapped) - bear higher medicine costs, constrained policy space, retaliation risk
 *   - patients_in_developing_countries: Ultimate target (powerless/trapped) - bear monopoly pricing as foregone treatment; no standing anywhere
 *   - generic_manufacturers: Target with residual coordination benefit (organized/constrained) - export markets narrow but rule predictability persists
 *   - middle_income_states: Secondary target with partial exit (organized/constrained) - coalition leverage and successful compulsory-licensing use
 *   - access_to_medicines_movement: Excluded voice (organized/constrained) - no standing in dispute settlement; contests the reading from outside the forum
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading: Uniform Patent Protection Mandate with Narrowly Construed Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health_policy/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '2129a59f-3931-4ee0-b487-f8ce4588121c').
narrative_ontology:cs_kernel_codification('2129a59f-3931-4ee0-b487-f8ce4588121c', fixed_text).
narrative_ontology:cs_authority_grounding('2129a59f-3931-4ee0-b487-f8ce4588121c', lineage).
narrative_ontology:cs_interpretation_layer_present('2129a59f-3931-4ee0-b487-f8ce4588121c').
narrative_ontology:cs_reading_relation('2129a59f-3931-4ee0-b487-f8ce4588121c', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_reading_relation('2129a59f-3931-4ee0-b487-f8ce4588121c', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, coexists_with).
narrative_ontology:cs_axiom('2129a59f-3931-4ee0-b487-f8ce4588121c', foundational, uniform_minimum_standards_mandatory).
narrative_ontology:cs_axiom_status(uniform_minimum_standards_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('2129a59f-3931-4ee0-b487-f8ce4588121c', uniform_minimum_standards_mandatory, conventional).
narrative_ontology:cs_axiom('2129a59f-3931-4ee0-b487-f8ce4588121c', foundational, exclusivity_incentivizes_pharmaceutical_innovation).
narrative_ontology:cs_axiom_status(exclusivity_incentivizes_pharmaceutical_innovation, holdable).
narrative_ontology:cs_axiom_grounding('2129a59f-3931-4ee0-b487-f8ce4588121c', exclusivity_incentivizes_pharmaceutical_innovation, instrumental).
narrative_ontology:cs_reference_frame('2129a59f-3931-4ee0-b487-f8ce4588121c', uniform_protection_narrow_exceptions).
narrative_ontology:cs_drift_state('2129a59f-3931-4ee0-b487-f8ce4588121c', post_doha_31bis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2129a59f-3931-4ee0-b487-f8ce4588121c', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovator_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, middle_income_states).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovation_incentive_theory).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, uniform_minimum_standards_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collect patent-term exclusivity across every WTO jurisdiction simultaneously: monopoly pricing power on patented medicines, licensing revenue, and the ability to block generic entry for twenty years per product. Fund and staff the enforcement coalition that defends the arrangement, and can re-portfolio R&D, re-price by market, or relocate production in response to any single jurisdiction's policy changes.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, beneficiary,
    institutional, generational, arbitrage, global).

% Home jurisdictions of the patent-holding industry. Capture the tax base, export revenue, and trade leverage that exclusivity rents generate. Initiate dispute settlement proceedings against members perceived to under-protect, negotiate bilateral IP chapters that ratchet protection beyond the treaty text, and coordinate blocking positions against waiver proposals in the TRIPS Council.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovator_states, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovator_states, agenda_setter).

% Panels and the Appellate Body adjudicate TRIPS disputes and their narrow construction of Articles 30-31 and 31bis operationalizes this reading in practice. They cannot initiate enforcement themselves; their rulings authorize retaliation by complaining members. Their own interpretive discretion is bounded by the fixed treaty text and by the coalition that staffs and resources the system.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Accepted TRIPS minimums as the price of WTO membership and market access. Bear higher public procurement costs for patented medicines, loss of domestic generic supply options, and retaliation risk when they attempt to use the flexibilities this reading construes narrowly. Exit would mean leaving the multilateral trading system entirely; LDC transition periods provide partial but expiring shelter.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_states, payer,
    moderate, generational, trapped, national).

% Bear the ultimate cost of monopoly pricing as foregone treatment: medicines priced beyond household and health-system reach. Have no standing in any forum where the arrangement is contested, cannot switch to a cheaper jurisdiction, and their access depends on flexibilities that only states can invoke on their behalf.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_developing_countries, payer,
    powerless, biographical, trapped, national).

% Indian and other generic producers face product-patent barriers in their export markets under this reading, narrowing their addressable volume. They retain process-patent workarounds, benefit from the predictability of uniform rules when operating legally, and supplied the low-cost medicines that built the access-to-medicines case; their core markets contract as the strong reading extends product patents into middle-income jurisdictions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_manufacturers, beneficiary).

% Brazil, India, South Africa, Thailand and peers face the same minimums as low-income members but hold coalition leverage: they led the Doha bloc, have issued compulsory licenses successfully, and host growing pharmaceutical markets that innovator firms do not want to lose. They absorb TRIPS-plus pressure in bilateral negotiations while retaining more usable countermeasures than low-income members.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, middle_income_states, payer,
    organized, generational, constrained, national).

% Public-health NGOs, treatment activists, and humanitarian medical organizations lack formal standing in WTO dispute settlement. They shaped the Doha Declaration through political pressure and continue to contest the strong reading through documentation campaigns, litigation support in national courts, and waiver advocacy, but remain outside the enforcement forum where the arrangement is actually adjudicated.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, access_to_medicines_movement, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Harmonizes minimum IP standards across 160+ trading nations: solves the collective-action problem of standards competition and jurisdictional free-riding, gives knowledge-good producers predictable multi-jurisdiction protection, and provides a single enforceable forum for IP disputes that would otherwise fragment into bilateral trade conflicts.
% TRANSFER_FUNCTION: Moves pricing power and licensing revenue from patients, public health systems, and generic producers - concentrated in low- and middle-income importing countries - to patent-holding pharmaceutical firms headquartered in innovator states; simultaneously moves policy discretion over medicine access from national legislatures into a multilateral enforcement arena where invoking it carries retaliation risk.
% ABSENT_VOICES: Patients in low-income countries and the health ministries that procure for them have no standing in WTO dispute settlement; the access-to-medicines movement participates only through political pressure and amicus channels; future patients whose treatments depend on R&D incentives are represented solely by the industry's own incentive claims. The unanimity behind the strong reading in enforcement settings partly reflects that the seats with the most direct objections were never in the room.
% DISAPPEARANCE_RATIONALE: If the strong-exclusivity configuration vanished overnight, patent-term pricing would collapse in newly contestable markets, TRIPS-plus FTA chapters would lose their multilateral anchor, generic supply chains would reorganize around expanded middle-income production, and innovator firms would re-portfolio R&D toward markets that still pay - the global pharmaceutical economy would visibly rearrange rather than continue as before.
% FOUNDING_PROBLEM: Pre-TRIPS fragmentation: national IP regimes varied wildly, innovations developed anywhere could be copied freely in weak-protection jurisdictions, rights-holders faced unremedied 'piracy' with no enforceable recourse, and trade friction over intellectual property was rising with no multilateral discipline to contain it.
% FOUNDING_PROBLEM_CORROBORATION: Trade economists and the WTO secretariat attest from outside the beneficiary set that the standards-fragmentation problem was real and that TRIPS addressed it. On whether the strong-exclusivity configuration specifically remains necessary: WHO, public-health scholars, and access-to-medicines researchers attest that the incentive problem is substantially met at current exclusivity margins and that the arrangement now functions predominantly as rent protection; independent empirical work on pharmaceutical R&D responsiveness to additional exclusivity (disease-specific market studies, priority-review natural experiments) corroborates the contested rather than live status.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__strong_exclusivity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the arrangement's pricing effect is decoupled from R&D marginal cost and concentrates on the agents least able to bear it. Suppression is high (0.72) because persistence depends on active machinery - dispute settlement, retaliation authorization, TRIPS-plus chapters - not on participant preference; suppression is authored as a raw structural property and is not scaled by power or scope (the engine scales only extractiveness, by directionality and spatial scope). Theater is moderate (0.42): the incentive function is real - exclusivity does fund some R&D - but a growing share of enforcement activity defends existing rent structures rather than producing new innovation, especially in markets too poor to fund R&D under any exclusivity level; the t=25 spike reflects incentive rhetoric intensifying during the COVID waiver fight while technology transfer was resisted. Accessibility_collapse is moderate (0.50): alternatives - compulsory licensing, parallel importation, LDC waivers - persist but are narrowed rather than eliminated under this reading. Resistance is high (0.75): Doha, the Article 31bis amendment, the COVID waiver campaign, and middle-income compulsory licensing constitute sustained, partially successful resistance. Claim and metrics are independent authored facts: I claim tangled_rope because the structure holds both a genuine coordination function (standards harmonization that even developing-country exporters use) and asymmetric extraction flowing through the same enforcement machinery; the metrics describe actual operation without being tuned to any predicted engine verdict. The measurement series run on one shared grid (t=0,3,6,10,15,20,25,30) with every metric authored at every point; the Doha dip at t=6 appears in both extractiveness and suppression, and the suppression_requirement series is authored because this story specifically tracks enforcement-capacity build-up (TRIPS-plus ratchet) and strain (Appellate Body crisis), not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats the arrangement is a rules-based system that makes global R&D investment bankable; extraction is invisible because it presents as 'market price.' From the trapped payer seats the same structure operates as a constraint on survival-critical access enforced by trade retaliation. Middle-income states straddle the gap: they have both invoked flexibilities against the reading and benefited from the predictability the uniform rules provide, which is why their computed seat should differ from both poles. Coalition dynamics matter for the powerless seats individually but not as a class: patients cannot coordinate, but middle-income state coalitions (the Doha bloc) demonstrably moved the arrangement - the engine should see that resistance is carried by organized states, not by the ultimate victims. The divergence between the institutional beneficiary seats and the powerless payer seats is the measurement this story exists to take; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Patent holders and innovator states sit near the beneficiary end: the constraint subsidizes their pricing power, they administer the enforcement agenda, and their exit is arbitrage-grade (re-portfolio, re-price, relocate in response to any single jurisdiction). Low-income states and their patients sit near the full-target end: trapped by WTO membership obligations and by disease respectively, with no meaningful exit. Generic manufacturers derive a high but not maximal d: the victim listing dominates, but their residual coordination benefit (predictable rules, legal process-patent workarounds) and organized power pull them slightly off the target pole - hence the secondary beneficiary role. Middle-income states sit between: payer position with constrained-but-real countermeasures. The dispute bodies administer without collecting, placing them near symmetric. No directionality overrides are declared: the beneficiary/victim declarations plus exit options produce the correct d for every seat, so the structural derivation chain suffices.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - standards fragmentation, free-riding, and IP-driven trade friction - remains partly live: harmonization still solves a real coordination problem that developing-country exporters also consume, so this is not a piton and not a dead-mandate zombie. But the strong-exclusivity configuration now does more work protecting existing rent structures than solving the founding problem at the margin, which is why the classification is tangled_rope rather than rope: the same machinery that coordinates (uniform standards, dispute settlement) extracts (monopoly pricing, narrowed flexibilities). Mislabeling risk runs both ways: reading the arrangement as pure snare erases the real coordination function and the genuine benefits middle-income exporters draw from it; reading it as pure rope erases the measured extraction from the least-resourced seats. The R5 mismatch check does not fire a zombie flag because founding_problem_status is contested rather than dead - but the contested status combined with a rising theater_ratio marks where the drift is heading: if the incentive justification continues to decouple from observed R&D yield, the arrangement migrates toward snare; if enforcement machinery decays faster than extraction (the Appellate Body trajectory), the enforcement layer drifts toward piton dynamics while the substantive constraint persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical,
    'This constraint is one reading of the trips_agreement_interpretive_kernel - the strong_exclusivity_reading. What would change structurally if the sibling public_health_flexibility_reading were instantiated instead?',
    'The readings are separate constraint files regardless of resolution; institutional resolution runs through dispute settlement jurisprudence, ministerial declarations, and treaty amendment (Doha 2001, Article 31bis 2017, the 2022 partial waiver).',
    'The sibling reading would move patients_in_developing_countries and low_income_states toward the beneficiary side, shrink the extraction surface, and reclassify compulsory licensing as an affirmative right rather than a tightly-gated exception; this file''s epsilon, beneficiary/victim structure, and classification apply only to the strong-exclusivity arrangement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexical, conceptual, 'Committer structure: this file is one reading of the TRIPS kernel; sibling readings instantiate different constraints from the same text.').

omega_variable(
    flexibility_interpretive_status_disagreement,
    'Where in the kernel''s structure is the reading contest located?',
    'Panel and Appellate Body jurisprudence on Articles 30-31 and 31bis, Doha Declaration paragraphs 4-6, and member practice on compulsory licensing and parallel importation.',
    'If the flexibilities are institutionally construed as broad affirmative rights, this reading''s victim set contracts and its enforcement burden rises, migrating the constraint toward the sibling''s structure; if construed as narrow exceptions, this file''s structure holds as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flexibility_interpretive_status_disagreement, conceptual, 'The contest is located in the interpretive status of the flexibilities architecture, not in the baseline protection mandate itself.').

omega_variable(
    marginal_innovation_incentive,
    'Does the marginal exclusivity this reading adds beyond a moderate baseline produce additional pharmaceutical R&D responsive to low-income disease burden?',
    'Disease-specific R&D studies, natural experiments (priority review vouchers, push-funding comparisons, market-size shocks), and innovator-firm portfolio analysis.',
    'If marginal yield is low, the instrumental justification for the strong reading collapses, the arrangement trends toward snare classification, and the live portion of the founding problem shrinks to standards harmonization alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginal_innovation_incentive, empirical, 'Empirical test of the reading''s incentive axiom: whether the means (strong exclusivity) actually produces the end (responsive innovation).').

omega_variable(
    trips_plus_ratchet_boundary,
    'Do TRIPS-plus FTA chapters narrow effective flexibility beyond the treaty text, and does that narrowing fall inside this constraint''s epsilon or constitute a distinct bilateral constraint layer?',
    'Comparative text analysis of FTA IP chapters against TRIPS baselines, plus access-to-medicines index data on flexibility use in TRIPS-plus signatory states.',
    'If the ratchet is inside this constraint''s epsilon, measured extraction is higher than treaty text alone implies and the addressable fixing seats are bilateral negotiators rather than the WTO membership collectively; if outside, this file understates the total enforcement web''s extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trips_plus_ratchet_boundary, empirical, 'Boundary question separating the treaty-level constraint from the bilateral ratchet layer that enforces it in practice.').

omega_variable(
    appellate_body_enforcement_decay,
    'The Appellate Body''s paralysis since 2019 degrades the multilateral enforcement machinery this reading depends on - does enforcement migrate to unilateral retaliation, decay into inertia, or reconstitute through the MPIA?',
    'Track dispute filings, appeal-into-the-void rates, unilateral enforcement actions (Section 301-style investigations), and MPIA adoption by members.',
    'Enforcement decay would flatten the suppression trajectory and push the enforcement layer toward inertia-maintained persistence (piton dynamics in the machinery while the substantive constraint persists); migration to unilateral retaliation would concentrate suppression in innovator-state seats and raise effective extraction on trapped targets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appellate_body_enforcement_decay, empirical, 'Enforcement-machinery trajectory question governing the future shape of the suppression series.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trip_tr_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(trip_tr_t0, observed).
narrative_ontology:measurement(trip_tr_t3, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(trip_tr_t3, observed).
narrative_ontology:measurement(trip_tr_t6, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 6, 0.3).
narrative_ontology:measurement_basis(trip_tr_t6, observed).
narrative_ontology:measurement(trip_tr_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(trip_tr_t10, observed).
narrative_ontology:measurement(trip_tr_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(trip_tr_t15, observed).
narrative_ontology:measurement(trip_tr_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(trip_tr_t20, observed).
narrative_ontology:measurement(trip_tr_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 25, 0.46).
narrative_ontology:measurement_basis(trip_tr_t25, observed).
narrative_ontology:measurement(trip_tr_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(trip_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(trip_be_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(trip_be_t0, observed).
narrative_ontology:measurement(trip_be_t3, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 3, 0.62).
narrative_ontology:measurement_basis(trip_be_t3, observed).
narrative_ontology:measurement(trip_be_t6, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 6, 0.57).
narrative_ontology:measurement_basis(trip_be_t6, observed).
narrative_ontology:measurement(trip_be_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 10, 0.68).
narrative_ontology:measurement_basis(trip_be_t10, observed).
narrative_ontology:measurement(trip_be_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement_basis(trip_be_t15, observed).
narrative_ontology:measurement(trip_be_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(trip_be_t20, observed).
narrative_ontology:measurement(trip_be_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement_basis(trip_be_t25, observed).
narrative_ontology:measurement(trip_be_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(trip_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(trip_su_t0, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement_basis(trip_su_t0, observed).
narrative_ontology:measurement(trip_su_t3, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 3, 0.64).
narrative_ontology:measurement_basis(trip_su_t3, observed).
narrative_ontology:measurement(trip_su_t6, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(trip_su_t6, observed).
narrative_ontology:measurement(trip_su_t10, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(trip_su_t10, observed).
narrative_ontology:measurement(trip_su_t15, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(trip_su_t15, observed).
narrative_ontology:measurement(trip_su_t20, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(trip_su_t20, observed).
narrative_ontology:measurement(trip_su_t25, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 25, 0.69).
narrative_ontology:measurement_basis(trip_su_t25, observed).
narrative_ontology:measurement(trip_su_t30, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(trip_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% The TRIPS text is a contested kernel (trips_agreement_interpretive_kernel) that decomposes into structurally distinct constraint stories per the epsilon-invariance principle: this file (strong_exclusivity_reading), public_health_flexibility_reading, and dispute_settlement_interpretive_authority. The epsilon values differ across readings because the beneficiary/victim structures differ: the flexibility reading moves patients and low-income states toward the beneficiary side and shrinks the extraction surface, while this reading holds flexibilities as narrow exceptions and concentrates extraction on them. This reading is upstream of the flexibility sibling in enforcement practice: the TRIPS-plus ratchet and narrow panel construction change the sibling's operating environment without logically eliminating it. All three files are linked as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
