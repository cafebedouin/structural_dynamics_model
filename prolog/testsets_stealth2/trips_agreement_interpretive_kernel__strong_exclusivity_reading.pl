% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__strong_exclusivity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   human_readable: TRIPS Strong Exclusivity Reading — Uniform Patent Protections with Narrowly Construed Flexibilities
 *   domain: international_trade_law/public_health/intellectual_property
 *
 * SUMMARY:
 *   The TRIPS Agreement is a contested kernel: a single treaty text that
 *   competing coalitions read differently, each reading instantiating a
 *   different constraint. This story instantiates the
 *   strong_exclusivity_reading — the arrangement in which TRIPS mandates
 *   high, uniform minimum patent protections with flexibilities (compulsory
 *   licensing, parallel imports) construed narrowly, justified as the
 *   incentive structure for pharmaceutical innovation. Under this reading the
 *   arrangement coordinates a global minimum-standards floor (a genuine
 *   collective-action function) while the same structure transfers monopoly
 *   pricing power from patients and importing states to patent-holding firms,
 *   held in place by active enforcement: dispute settlement with authorized
 *   retaliation, plus bilateral ratchets (unilateral watch lists, TRIPS-plus
 *   FTA chapters). Beneficiaries: pharmaceutical patent holders and
 *   technology-exporting states. Victims: low-income importing states, their
 *   patients, and generic producers facing the narrow construction. This is
 *   one reading only; the public_health_flexibility_reading and the
 *   dispute_settlement_interpretive_authority reading are separate
 *   constraints with their own beneficiary/victim structures and their own ε,
 *   linked through the network. Per the ε-referent rule, ε is authored for
 *   the standing strong-exclusivity arrangement as THIS reading assesses it —
 *   the reading holds the transfer is the justified price of innovation,
 *   which is why its ε sits below what a flexibility-reading story would
 *   author for its own arrangement.
 *
 * KEY AGENTS:
 *   - pharmaceutical_patent_holders: Primary beneficiary (powerful/arbitrage) — collects monopoly rents on patented medicines across all WTO markets
 *   - technology_exporting_states: Secondary beneficiary (institutional/arbitrage) — collects IP-intensive trade surpluses and wields the bilateral enforcement ratchet
 *   - wto_dispute_settlement_body: Agenda setter (institutional/constrained) — administers the arrangement; panel rulings have construed flexibilities narrowly and retaliation authorization is the enforcement backstop
 *   - low_income_importing_states: Primary target (organized/trapped) — bears high medicine prices and constrained regulatory autonomy; exit costs more than compliance
 *   - patients_in_low_income_states: Deepest target (powerless/trapped) — bears the health consequence of monopoly pricing directly
 *   - generic_pharmaceutical_producers: Target with secondary benefit (organized/constrained) — excluded from new molecules by the narrow construction but holds the legacy generic export market
 *   - public_health_ngo_coalition: Excluded voice (moderate/constrained) — forced the 2001 declaration but its broad-flexibility reading remains outside operative construction
 *   - ip_innovation_economists: Analytical observer (analytical/analytical) — sees both the standards floor and the price paid for it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.65).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.7).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__strong_exclusivity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "TRIPS Strong Exclusivity Reading — Uniform Patent Protections with Narrowly Construed Flexibilities").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__strong_exclusivity_reading, "international_trade_law/public_health/intellectual_property").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__strong_exclusivity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__strong_exclusivity_reading, '4385e91e-621f-49ec-b550-123ff4600c56').
narrative_ontology:cs_kernel_codification('4385e91e-621f-49ec-b550-123ff4600c56', fixed_text).
narrative_ontology:cs_authority_grounding('4385e91e-621f-49ec-b550-123ff4600c56', extraction).
narrative_ontology:cs_interpretation_layer_present('4385e91e-621f-49ec-b550-123ff4600c56').
narrative_ontology:cs_reading_relation('4385e91e-621f-49ec-b550-123ff4600c56', trips_agreement_interpretive_kernel__public_health_flexibility_reading, influences).
narrative_ontology:cs_reading_relation('4385e91e-621f-49ec-b550-123ff4600c56', trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, influences).
narrative_ontology:cs_axiom('4385e91e-621f-49ec-b550-123ff4600c56', foundational, exclusive_rights_are_pharmaceutical_innovation_prerequisite).
narrative_ontology:cs_axiom_status(exclusive_rights_are_pharmaceutical_innovation_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('4385e91e-621f-49ec-b550-123ff4600c56', exclusive_rights_are_pharmaceutical_innovation_prerequisite, empirically_contingent).
narrative_ontology:cs_axiom('4385e91e-621f-49ec-b550-123ff4600c56', foundational, flexibilities_are_narrowly_construed_exceptions).
narrative_ontology:cs_axiom_status(flexibilities_are_narrowly_construed_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('4385e91e-621f-49ec-b550-123ff4600c56', flexibilities_are_narrowly_construed_exceptions, conventional).
narrative_ontology:cs_reference_frame('4385e91e-621f-49ec-b550-123ff4600c56', uniform_strong_minimum_standards_baseline).
narrative_ontology:cs_drift_state('4385e91e-621f-49ec-b550-123ff4600c56', post_doha_covid_waiver_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4385e91e-621f-49ec-b550-123ff4600c56', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, technology_exporting_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_importing_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_pharmaceutical_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_pharmaceutical_producers).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__strong_exclusivity_reading, innovation_incentive_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the treaty's dispute process: hears complaints over patent-protection shortfalls, rules on how the flexibility clauses may be used, and authorizes retaliatory suspension of trade concessions against non-complying members. Its rulings have construed compulsory licensing narrowly and its retaliation authorization is what gives the standards floor practical force. Its interpretive latitude is bounded by member consensus and the appellate vacancy crisis; it cannot exit the system it administers.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, constrained, global).

% Hold twenty-year product patents across all WTO markets and collect monopoly pricing margins on patented medicines. Shaped the treaty's terms through domestic trade-policy channels and continue to gain from bilateral chapters that lock standards above the treaty floor. They can relocate R&D, shift between bilateral and multilateral enforcement venues, and price-discriminate across markets; leaving any single market is cheap because the arrangement spans all of them.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Export IP-intensive goods and collect the trade surpluses the uniform standards underwrite. They maintain unilateral watch lists and negotiate bilateral chapters that exceed the multilateral floor. They fund the multilateral system and absorb occasional retaliation exposure on non-IP goods; their domestic generic sectors are small enough that strong standards impose little internal cost.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, technology_exporting_states, beneficiary,
    institutional, generational, arbitrage, global).

% Import nearly all patented medicines and pay monopoly prices through public health budgets and out-of-pocket spending. They accepted the standards as part of the broader trade package that purchased market access, and leaving the trading system would cost more than compliance. They coordinate in WTO groupings and secured the 2001 declaration affirming health flexibilities, but each actual use of a flexibility invites bilateral designation and retaliation risk.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, low_income_importing_states, payer,
    organized, generational, trapped, global).

% Bear the health consequences of monopoly pricing directly: treatment courses priced far above generic cost, rationing by budget rather than by need. They hold no seat in trade negotiation and have no market exit; their only channel is advocacy filtered through their governments and NGOs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, patients_in_low_income_states, payer,
    powerless, immediate, trapped, global).

% Supply the world's off-patent medicines and built a large export industry during the treaty's transition periods. The narrow construction of compulsory licensing bars them from new molecules except through cumbersome case-by-case processes, and bilateral chapters extend exclusivity further. They profit from the legacy generic market while being shut out of the patented segment — a genuinely dual position.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_pharmaceutical_producers, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__strong_exclusivity_reading, generic_pharmaceutical_producers, beneficiary).

% Treatment-access and public-health organizations that forced the 2001 ministerial declaration and drove the waiver campaign. Under the operative narrow construction their broad-flexibility reading carries no interpretive weight in dispute rulings; their leverage is political — ministerial moments, public campaigns — and is exercised from outside the interpretive process rather than within it.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, public_health_ngo_coalition, excluded,
    moderate, biographical, constrained, global).

% Study whether uniform strong standards raise innovation output or mainly extend and defend rents, and what the arrangement costs importing health systems. They see both the standards floor's coordination value and the price paid for it; their findings feed the beneficiary coalition's justification and the flexibility coalition's critique alike.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__strong_exclusivity_reading, ip_innovation_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(trips_agreement_interpretive_kernel__strong_exclusivity_reading, pharmaceutical_patent_holders).
narrative_ontology:fixing_cost_class(trips_agreement_interpretive_kernel__strong_exclusivity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform minimum floor of intellectual property protection across all WTO members, solving a collective-action problem: without a common floor, states face pressure to under-protect (free-riding on others' R&D), innovators face fragmented and unpredictable protection across markets, and IP conflicts become bilateral trade fights. The treaty coordinates one predictable standards regime.
% TRANSFER_FUNCTION: Moves monopoly pricing power over patented medicines from patients, public health systems, and generic producers in importing states to patent-holding pharmaceutical firms concentrated in technology-exporting states; moves interpretive and regulatory autonomy over health measures from importing states to the dispute-settlement process.
% ABSENT_VOICES: Patients without treatment access and the health ministries of low-income states were largely absent from the 1986–1994 negotiation table, where IP was negotiated as a trade issue by trade ministries. Generic producers and treatment-access advocates entered only after implementation began, and under the operative narrow construction their flexibility arguments sit outside the interpretive process that decides how the text is applied.
% DISAPPEARANCE_RATIONALE: If the strong-exclusivity construction and its enforcement vanished overnight, generic producers would supply patented molecules at marginal cost in importing states within months, pharmaceutical R&D incentives would reprice (with contested effects on innovation rates), bilateral IP pressure would lose its treaty anchor, and the dispute system would lose one of its most-litigated dockets. The global pharmaceutical trade regime, WTO accession terms, and FTA IP chapters all depend on the arrangement persisting.
% FOUNDING_PROBLEM: Pre-treaty fragmentation: weak or absent pharmaceutical patent protection across much of the world meant innovating firms could not recoup R&D across markets, free-riding was systematic, and IP conflicts were handled through unilateral trade pressure rather than agreed rules.
% FOUNDING_PROBLEM_CORROBORATION: Innovation economists outside the patent-holder beneficiary set attest the free-riding problem was real and remains real for some drug classes (notably antibiotics and neglected-disease therapeutics). The 2001 Doha Declaration on the TRIPS Agreement — adopted by the full WTO membership and therefore attestable from outside the beneficiary set — affirms that public-health flexibilities are integral to the text, corroborating that the founding problem's proper solution is disputed rather than settled. No party outside the beneficiary set attests that the strong-uniform construction is the uniquely required solution.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__strong_exclusivity_reading, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__strong_exclusivity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

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
 *   Claim: tangled_rope — the arrangement possesses both a genuine coordination function (a uniform minimum-standards floor solving free-riding and fragmentation) and asymmetric extraction through the same structure (monopoly pricing flowing to patent holders, paid by patients and importing states), and it requires active enforcement to hold. Metrics are authored independently of the claim. Extractiveness 0.65 reflects this reading's own assessment: substantial transfer, acknowledged by the reading itself as the incentive price, so below the ceiling a flexibility-reading story would author. Suppression 0.70 is a raw structural property, unscaled by power or scope: persistence depends on actively deterring flexibility use through retaliation threats, bilateral designations, and FTA lock-in, not on participant preference. Theater 0.35: the harmonization function is real, but a growing share of enforcement activity defends the narrow construction itself rather than any health or innovation end (evergreening disputes; waiver negotiations that consumed two years and produced a narrow, partially-implemented instrument). Accessibility_collapse 0.55: alternatives partly persist — states may invoke flexibilities, seek waivers, or negotiate — but each carries retaliation risk and lock-in, so alternatives are costly rather than closed. Resistance 0.60: the Doha Declaration, the Africa Group coalition, compulsory-licensing use by middle-income states, and the COVID waiver campaign are real, organized resistance that dented but did not displace the construction. The measurement series run on one shared grid (1995–2025, seven points, all three metrics at every point) and show extraction accumulating as TRIPS-plus layers ratcheted onto the treaty floor, enforcement capacity intensifying through the 2010s, and theater rising as waiver politics produced symbolic rather than structural relief — the 2021 dip in extractiveness marks the waiver's real but partial effect.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (the dispute-settlement body, backed by the patent-holding exporter coalition) experiences the arrangement as a lawful incentive architecture it administers: rules are rules, flexibilities are exceptions, enforcement is neutral application. The payer seats experience the same text as enforced extraction: a compulsory-licensing provision that cannot practically be used is not a flexibility. The generic-producer seat straddles the two — shut out of new molecules, enriched by the legacy generic trade. The engine computes per-seat classifications from these structural positions; the divergence between the agenda-setter's coordination experience and the payers' extraction experience is the measurement the corpus exists to take, not an inconsistency to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the beneficiary seats toward the low-d end: pharmaceutical patent holders sit nearest 0.0 (pure rent collection with arbitrage-grade exit through venue-shifting and R&D relocation); technology-exporting states sit slightly higher (~0.15) because they bear enforcement costs and occasional retaliation exposure on non-IP goods. Victim declarations drive the target seats high: patients (trapped, powerless) sit nearest 1.0; low-income importing states near ~0.85 (they accepted the standards inside the broader trade package, a small offsetting benefit); generic producers around ~0.65–0.7 — victims of the narrow construction but holders of a large generic export market, so not full targets. No directionality overrides are authored: the override surface is power-atom-granular, and this story's same-power seats (low-income importing states and generic producers are both organized) need different d values that only the engine's role-plus-exit derivation can supply; a power-atom override would misapply to both.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — fragmentation and free-riding depriving innovators of returns — is partially live: innovation economists attest it persists for some drug classes, but the parties dispute whether the uniform-strong construction is its solution or its overshoot. Mandatrophy is therefore resolved as contested rather than resolved. If the founding problem were fully dead and the arrangement persisted on inertia alone, the classification would trend piton; the enforcement series shows the opposite — active, intensifying maintenance. If the coordination function were pure cover, the classification would trend snare; but the standards floor does real work (removing it would rearrange the pharmaceutical trade, and every party including the flexibility coalition accepts a floor exists). The tangled_rope classification holds both truths and prevents mislabeling in both directions: it stops access advocates from reading the whole treaty as pure extraction, and it stops the beneficiary coalition from reading it as pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of the trips_agreement_interpretive_kernel: where exactly in the text''s structure is the disagreement between this strong-exclusivity reading and the public-health-flexibility reading located?',
    'Doctrinal analysis of which structural element the readings diverge on: the construal of Article 31 (compulsory-licensing conditions), Article 6 (exhaustion and parallel imports), and whether the Doha Declaration carries interpretive authority or merely political weight. If the divergence sits in drafting-repairable provisos, the readings could in principle be reconciled; if it sits in the innovation-incentive premise itself, they cannot.',
    'If the disagreement is located in interpretive method rather than normative premise, the dispute-settlement sibling could resolve the contest; if located in the foundational premise, the readings remain structurally separate constraints indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Locates where in the kernel''s structure the reading contest actually sits.').

omega_variable(
    innovation_incentive_empirical_support,
    'Does the strong-uniform exclusivity arrangement actually raise pharmaceutical innovation rates, or does it primarily extend and defend monopoly rents on existing molecules (evergreening, patent thicketing)?',
    'Natural experiments and cross-country study: India''s 2005 product-patent introduction, the 2021–2022 waiver episode, and new-molecule approval rates against rent metrics under varying exclusivity strength, controlling for market size and disease burden.',
    'If the innovation effect is weak or concentrated in rent extension, the coordination function is substantially cover and the arrangement trends from tangled_rope toward snare; if strong, part of the measured extraction is the genuine price of the coordination good.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_empirical_support, empirical, 'Whether the incentive justification is empirically borne out or functions as cover.').

omega_variable(
    enforcement_path_bilateral_vs_multilateral,
    'Is the narrow construction held by the multilateral dispute-settlement machinery, or by bilateral power (unilateral watch-list designations, TRIPS-plus FTA chapters)?',
    'Compare flexibility-use outcomes for states facing formal panel rulings versus states facing only bilateral pressure; track whether FTA IP chapters exceed the multilateral floor and whether compulsory-licensing use correlates with bilateral designation rather than multilateral violation.',
    'If bilateral power is the operative enforcement, this arrangement is less a multilateral coordination structure and more a bilateral ratchet with a treaty facade — classification shifts toward snare and the multilateral seats lose their agenda-setter character.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_path_bilateral_vs_multilateral, empirical, 'Which enforcement path actually holds the narrow construction in place.').

omega_variable(
    doha_rebalancing_durability,
    'Does the 2001 Doha Declaration (and the 2021 partial waiver) represent a durable rebalancing in which the two readings stably coexist, or a temporary concession that TRIPS-plus erosion is progressively reversing?',
    'Track the ratio of TRIPS-plus FTA IP chapters to flexibility-affirming instruments over time; monitor whether compulsory-licensing use rises post-waiver or whether bilateral lock-in forecloses it.',
    'If erosion is reversing Doha, the drift state moves from substantial toward severe and the arrangement trends snare; if Doha holds, the readings coexist and the tangled_rope classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doha_rebalancing_durability, empirical, 'Durability of the Doha rebalancing against TRIPS-plus erosion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__strong_exclusivity_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_strong_exclusivity_tr_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement_basis(trips_strong_exclusivity_tr_t1995, observed).
narrative_ontology:measurement(trips_strong_exclusivity_tr_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2001, 0.23).
narrative_ontology:measurement_basis(trips_strong_exclusivity_tr_t2001, observed).
narrative_ontology:measurement(trips_strong_exclusivity_tr_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2005, 0.27).
narrative_ontology:measurement_basis(trips_strong_exclusivity_tr_t2005, observed).
narrative_ontology:measurement(trips_strong_exclusivity_tr_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement_basis(trips_strong_exclusivity_tr_t2010, observed).
narrative_ontology:measurement(trips_strong_exclusivity_tr_t2016, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2016, 0.32).
narrative_ontology:measurement_basis(trips_strong_exclusivity_tr_t2016, observed).
narrative_ontology:measurement(trips_strong_exclusivity_tr_t2021, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2021, 0.35).
narrative_ontology:measurement_basis(trips_strong_exclusivity_tr_t2021, observed).
narrative_ontology:measurement(trips_strong_exclusivity_tr_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, theater_ratio, 2025, 0.35).
narrative_ontology:measurement_basis(trips_strong_exclusivity_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(trips_strong_exclusivity_be_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement_basis(trips_strong_exclusivity_be_t1995, observed).
narrative_ontology:measurement(trips_strong_exclusivity_be_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement_basis(trips_strong_exclusivity_be_t2001, observed).
narrative_ontology:measurement(trips_strong_exclusivity_be_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement_basis(trips_strong_exclusivity_be_t2005, observed).
narrative_ontology:measurement(trips_strong_exclusivity_be_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement_basis(trips_strong_exclusivity_be_t2010, observed).
narrative_ontology:measurement(trips_strong_exclusivity_be_t2016, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2016, 0.66).
narrative_ontology:measurement_basis(trips_strong_exclusivity_be_t2016, observed).
narrative_ontology:measurement(trips_strong_exclusivity_be_t2021, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2021, 0.64).
narrative_ontology:measurement_basis(trips_strong_exclusivity_be_t2021, observed).
narrative_ontology:measurement(trips_strong_exclusivity_be_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, base_extractiveness, 2025, 0.65).
narrative_ontology:measurement_basis(trips_strong_exclusivity_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(trips_strong_exclusivity_su_t1995, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement_basis(trips_strong_exclusivity_su_t1995, observed).
narrative_ontology:measurement(trips_strong_exclusivity_su_t2001, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement_basis(trips_strong_exclusivity_su_t2001, observed).
narrative_ontology:measurement(trips_strong_exclusivity_su_t2005, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2005, 0.63).
narrative_ontology:measurement_basis(trips_strong_exclusivity_su_t2005, observed).
narrative_ontology:measurement(trips_strong_exclusivity_su_t2010, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement_basis(trips_strong_exclusivity_su_t2010, observed).
narrative_ontology:measurement(trips_strong_exclusivity_su_t2016, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2016, 0.69).
narrative_ontology:measurement_basis(trips_strong_exclusivity_su_t2016, observed).
narrative_ontology:measurement(trips_strong_exclusivity_su_t2021, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement_basis(trips_strong_exclusivity_su_t2021, observed).
narrative_ontology:measurement(trips_strong_exclusivity_su_t2025, trips_agreement_interpretive_kernel__strong_exclusivity_reading, suppression_requirement, 2025, 0.7).
narrative_ontology:measurement_basis(trips_strong_exclusivity_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__strong_exclusivity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__strong_exclusivity_reading, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% DUAL FORMULATION NOTE:
% The TRIPS Agreement is one kernel — a single treaty text — that decomposes into at least three structurally distinct constraints under different readings. This story authors the strong-exclusivity reading: its ε (0.65), beneficiary set (patent holders, exporting states), and victim set (importing states, patients, generic producers) describe the narrow-construction arrangement as this reading assesses it. The public-health-flexibility reading instantiates a different constraint over the same text — broad flexibilities as integral protections, with the rent structure as the extraction object — and authors its own ε, beneficiaries, and victims. The dispute-settlement reading constrains the institutional machinery both others depend on. Each story carries one stable ε per the ε-invariance principle; the reading contest itself lives in the omega variables, not in hedged metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
