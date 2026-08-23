% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable upon Unilateral Bad-Faith Determination (Transactional-Provisional Reading)
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   A multilateral exchange concluded in 2015 trades verified limits on the
 *   Iranian enrichment program for calibrated sanctions relief, with the
 *   entire instrument resting on continuous reciprocal performance rather
 *   than formal legal obligation. Read as a provisional transactional
 *   framework, the arrangement binds no party beyond its ongoing consent: any
 *   participant may declare counterparty bad faith by nationally defined
 *   criteria and cease performance, and the 2018 cessation by one party did
 *   exactly that, reimposing the pre-deal penalty architecture on the
 *   remaining performers. The structural center of gravity is the asymmetry
 *   between the two legs of the exchange — the disarmament leg (stockpile
 *   shipment, centrifuge removal, inspection access) is physically
 *   irreversible once performed, while the relief leg (market access, asset
 *   release, sanctions easing) is revocable by unilateral declaration. The
 *   instrument also carries literal sunset provisions (several restrictions
 *   lapse at ten and fifteen years), declared via has_sunset_clause. The
 *   claim and the metrics are independent authored facts: the claimed type is
 *   stated from the structure (genuine exchange plus asymmetric legs plus
 *   active enforcement), and the metrics describe observed operation without
 *   being tuned to any predicted output.
 *
 * KEY AGENTS:
 *   - us_withdrawal_administration: agenda-setting seat (institutional power, arbitrage exit) — makes the bad-faith determination, ceases performance, reimposes the penalty architecture; the seat the arrangement's gains demonstrably accrued to
 *   - signatory_state_executives: beneficiary class (institutional, arbitrage) — hold the standing unilateral exit option the frame deliberately preserves
 *   - anti_deal_domestic_coalitions: beneficiary (organized, mobile) — legislative and advocacy opposition collecting a policy victory without treaty-breach cost
 *   - iranian_nuclear_establishment: primary payer (moderate, trapped) — performed the irreversible leg of the exchange
 *   - reliant_european_commercial_entrants: payer (powerful, constrained) — absorbed reimposition losses on re-entry capital
 *   - e3eu_deal_stewards: payer (institutional, identity_locked) — maintained the frame past the point of reciprocity to protect a constructed diplomatic identity
 *   - iranian_hardline_factions: excluded voice (organized) — objected from outside the frame's governance; vindicated by the cessation
 *   - nonparty_regional_opponents: excluded voice (powerful, mobile) — lobbied for dissolution from outside the negotiation
 *   - iaea_verification_body: analytical observer (institutional, analytical) — supplies the verification that makes the exchange possible; holds no seat in the validity determination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.72).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.6).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable upon Unilateral Bad-Faith Determination (Transactional-Provisional Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:has_sunset_clause(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e16f62f6-fc43-45ec-a296-806d82b797ed').
narrative_ontology:cs_kernel_codification('e16f62f6-fc43-45ec-a296-806d82b797ed', formalized).
narrative_ontology:cs_authority_grounding('e16f62f6-fc43-45ec-a296-806d82b797ed', self_enforcing).
narrative_ontology:cs_reading_relation('e16f62f6-fc43-45ec-a296-806d82b797ed', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('e16f62f6-fc43-45ec-a296-806d82b797ed', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('e16f62f6-fc43-45ec-a296-806d82b797ed', foundational, unilateral_bad_faith_voidance_right).
narrative_ontology:cs_axiom_status(unilateral_bad_faith_voidance_right, holdable).
narrative_ontology:cs_axiom_grounding('e16f62f6-fc43-45ec-a296-806d82b797ed', unilateral_bad_faith_voidance_right, conventional).
narrative_ontology:cs_axiom('e16f62f6-fc43-45ec-a296-806d82b797ed', foundational, performance_continuity_bindingness).
narrative_ontology:cs_axiom_status(performance_continuity_bindingness, holdable).
narrative_ontology:cs_axiom_grounding('e16f62f6-fc43-45ec-a296-806d82b797ed', performance_continuity_bindingness, instrumental).
narrative_ontology:cs_reference_frame('e16f62f6-fc43-45ec-a296-806d82b797ed', provisional_transactional_equilibrium).
narrative_ontology:cs_drift_state('e16f62f6-fc43-45ec-a296-806d82b797ed', post_unilateral_exit_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e16f62f6-fc43-45ec-a296-806d82b797ed', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, signatory_state_executives).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, anti_deal_domestic_coalitions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_nuclear_establishment).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, reliant_european_commercial_entrants).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, e3eu_deal_stewards).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, us_withdrawal_administration).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, national_self_judgment_of_counterparty_bad_faith).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, political_commitment_instrument_not_legally_binding).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determined in May 2018 that the counterparty side of the exchange was acting in bad faith by nationally defined criteria, announced cessation of participation, and reimposed the full pre-deal penalty architecture, including extraterritorial penalties on third-country firms transacting with the remaining parties. Sets which determinations count, administers the reimposed penalties, and collected three years of verified restraint from counterparts before exercising cessation at no formal-legal cost. Exit consisted of a public declaration; no tribunal, commission, or counterparty could block it.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, us_withdrawal_administration, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, us_withdrawal_administration, beneficiary).

% Executives of the signatory governments hold, at all times, the standing option to declare counterparty bad faith and cease performance. The frame's design preserves that option deliberately: nothing in the instrument obliges continuation, so each executive retains full discretion over future policy. Several exercised lesser versions of the option — suspending portions of performance — without triggering formal breach proceedings.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, signatory_state_executives, beneficiary,
    institutional, generational, arbitrage, national).

% Legislative majorities and allied advocacy networks opposed the arrangement from signature onward. Because the instrument was framed as a political commitment rather than a ratified treaty, opposing and ultimately dissolving it carried no treaty-breach stigma; the coalitions gained a policy victory and the reimposition lever without paying an international-law price. Their advocacy activity continued throughout the frame's operation and culminated in the 2018 cessation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, anti_deal_domestic_coalitions, beneficiary,
    organized, biographical, mobile, national).

% Performed the physically irreversible side of the exchange: shipped the bulk of its enriched-uranium stockpile out of the country, removed and disabled thousands of centrifuges, capped enrichment level and stockpile size, and submitted to intrusive continuous monitoring. The consideration it received — sanctions relief and market access — was extended by instruments any counterparty could revoke by unilateral declaration. After the 2018 cessation and reimposed penalties, restoring the surrendered capability requires years of reconstruction under intensified financial pressure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_nuclear_establishment, payer,
    moderate, generational, trapped, national).

% Major industrial, energy, and shipping firms re-entered the Iranian market on the strength of the frame's apparent durability, committing capital to contracts, offices, insurance structures, and financing lines. When a non-party government's national determination reimposed extraterritorial penalties, these firms faced a choice between abandoning the market and losing access to the US financial system; nearly all withdrew at substantial booked losses.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, reliant_european_commercial_entrants, payer,
    powerful, biographical, constrained, continental).

% The European signatories staked diplomatic credibility on the frame's continuance and built their nonproliferation posture around it. After the 2018 cessation they maintained the instrument's machinery — commission sessions, reporting, attempted channels for legitimate trade — while absorbing the cost of upholding a bargain whose counterpart consideration had been revoked. Abandoning the frame would repudiate the diplomatic identity they had constructed around it, so they continued performing past the point of reciprocity.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, e3eu_deal_stewards, payer,
    institutional, generational, identity_locked, continental).

% Opposed the arrangement from before signature, arguing that surrendering hardware and stockpiles for revocable relief was structurally unsound. Never seated in the framework's governance or its dispute-resolution bodies, they gained influence as the cessation vindicated their core objection, and directed the staged performance reductions after 2019.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_hardline_factions, excluded,
    organized, generational, trapped, national).

% Regional governments excluded from the negotiation objected to its terms — principally the legitimation of enrichment capacity and the sunset provisions — and campaigned inside the withdrawing party's domestic coalition for dissolution. Their exclusion from the table left the frame without defenders among the actors most able to influence the deciding electorate.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, nonparty_regional_opponents, excluded,
    powerful, generational, mobile, regional).

% Operates the monitoring system that makes the exchange verifiable: inspections, surveillance equipment, and material accounting. Reports findings factually to the parties and the board of governors but holds no seat in the bad-faith determinations that govern the frame's validity — its certifications of Iranian compliance were issued in the same period the cessation determination was made.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_verification_body, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, us_withdrawal_administration).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a mutual-distrust spiral — enrichment expansion answered by sanctions tightening — into a sequenced, verifiable exchange: the Iranian side accepts caps, stockpile limits, and continuous inspection; the counterpart sides extend calibrated sanctions relief and reintegration. Verification substitutes for trust, allowing parties that deny each other's good faith to trade observable performance instead of intentions.
% TRANSFER_FUNCTION: Moves enriched-uranium stockpiles, centrifuge capacity, and inspection access from the Iranian nuclear establishment to the verifying parties, and moves market access, released assets, and sanctions relief from the sanctioning parties toward Iran — with the relief leg revocable by any party's unilateral determination and the disarmament leg effectively irreversible once performed.
% ABSENT_VOICES: Iranian hardline factions and the non-party regional governments would object if seated — the former that irreversible dismantlement is poor consideration against revocable relief, the latter that the frame legitimizes enrichment capacity at all. Both stood outside the negotiating room and the framework's governance bodies; the deciding electorate in the withdrawing party heard chiefly from the latter.
% DISAPPEARANCE_RATIONALE: If the frame vanished overnight, enrichment levels and stockpiles would resume their pre-2015 trajectory, the penalty architecture would harden back to its pre-deal form, the commercial reintegration would unwind, and regional proliferation hedging would restart — the parties' entire 2015–2018 equilibrium depended on the exchange operating.
% FOUNDING_PROBLEM: Between 2003 and 2015 the Iranian enrichment program expanded under progressively tighter penalties, presenting the opposing parties with a worsening choice between military action and acquiescence in latent breakout capability; the frame was built to trade verified program limits for economic reintegration and thereby dissolve that dilemma.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: IAEA reporting documents the program expansion that motivated the frame; UN Security Council Resolution 2231's endorsing text recites the breakout concern; and the frame's own opponents — hardline factions and non-party regional governments — attest the underlying problem was real even while rejecting the chosen solution. No corroborating source attests that the problem is solved; enrichment has since exceeded the frame's limits.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72 at interval end) because the exchange's legs are asymmetrically revocable: one side's consideration became unrecoverable on delivery while the counterpart side was withdrawn by declaration, and post-cessation the remaining performers continued delivering against revoked consideration. Suppression (0.60) is authored as a raw structural property, unscaled by power or scope — only extractiveness is scaled in the engine's computation — and reflects the post-2018 shift of the compliance mechanism from reciprocity to extraterritorial financial coercion applied to third-country firms. Theater ratio (0.55) reflects machinery that after 2018 largely performed continuity — commission meetings, reporting cycles, workaround schemes — without the core exchange operating. Accessibility collapse is moderate-low (0.40) because under this reading alternatives remain visible and usable: exit, renegotiation, and reversion to the safeguards baseline all stay open, which is precisely the frame's design intent. Resistance (0.65) is substantial: staged performance reductions after 2019, prolonged domestic legislative conflict inside the ceasing party, and sustained regional lobbying against the frame. The temporal series run on one single shared grid (2015–2021, all three metrics at all seven points) with monotone rather than cyclical trajectories; the inflection at 2018 marks the cessation. The measurement endpoints match the scalar base_properties by construction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat compute differently from the same structure. From the ceasing party's seat, the frame is a revocable commitment exercised exactly as designed — cessation upon national bad-faith determination is the reserved right, not a violation. From the first-performing payer's seat, the same structure operates as an exchange in which its own consideration is unrecoverable while the counterpart's was withdrawn by announcement. From the steward seat, the frame persisted past reciprocity as an identity commitment. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structural positions: signatory_state_executives hold the standing exit option (arbitrage-grade exit places them near the beneficiary end of directionality), and anti_deal_domestic_coalitions collected policy value from the frame's low bindingness (mobile exit, low d). Victim declarations map likewise: iranian_nuclear_establishment performed irreversibly with no recovery path (trapped, near full-target), reliant_european_commercial_entrants bore reimposition losses on sunk capital (constrained, high d), and e3eu_deal_stewards bear diffuse ongoing costs with identity-locked exit (high d despite institutional power). The receipt surface names us_withdrawal_administration as the seat the gains demonstrably accrued to — three years of verified counterpart restraint followed by cost-free exit and reimposition leverage — which is a receipt claim, distinct from its beneficiary-role designation. The excluded seats (hardline factions, regional opponents) shaped the frame's fate without collecting from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a widening enrichment program under tightening sanctions presenting a war-or-acquiescence dilemma — remains live and is corroborated from outside the beneficiary set, so no mandate-atrophy is declared. The classification guards against both mislabels: the genuine verified-exchange function blocks a pure-extraction reading of the frame, while the revocability asymmetry blocks a pure-coordination reading. The theater-ratio trajectory after 2018 is the drift signal to watch: if the frame's machinery persists long after the exchange stopped operating, sustained theatrical dominance would indicate inertial maintenance of a dead mandate rather than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story instantiates the transactional_provisional_reading of the jcpoa_treaty_bindingness kernel; would adopting a sibling reading (binding_multilateral_reading or graduated_compliance_reading) change the constraint''s structural identity rather than merely its evaluation?',
    'Compile the sibling stories over the same referent and compare victim sets, exit structures, and computed types across readings; locating the divergence in epsilon versus in victim-set composition distinguishes readings that differ in degree from readings that differ in kind.',
    'If the binding_multilateral_reading is adopted, unilateral exit constitutes breach rather than exercise of a reserved right — measured suppression rises, the payer set expands to include the withdrawing party''s own successors, and the foreclosure edge computed from this story''s axioms inverts direction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a contested bindingness kernel; sibling readings instantiate different constraints over the same instrument.').

omega_variable(
    bad_faith_determination_standard,
    'Is the bad faith that warrants voidance determined by national self-certification, or does it require externally verifiable breach?',
    'Compare the 2018 cessation determination — invoked while IAEA reporting certified Iranian compliance — against the framework''s dispute-resolution text, and survey state practice on self-judged material breach.',
    'Purely national self-certification keeps exit cost near zero and the frame near-voluntary; an external-verification requirement raises exit cost substantially and converges this reading toward the graduated-compliance structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_determination_standard, empirical, 'Whether the voidance trigger is self-judged or externally certified.').

omega_variable(
    consideration_recoverability_asymmetry,
    'How reversible is each side''s performance — does the first-performing party''s dismantled enrichment capability constitute sunk consideration?',
    'Technical assessment of centrifuge redeployment timelines, enriched-uranium stockpile regeneration rates, and retention of inspector knowledge and institutional memory.',
    'High recoverability collapses the extraction asymmetry toward a balanced reciprocal exchange; low recoverability entrenches the asymmetric structure and strengthens the payer-seat classifications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consideration_recoverability_asymmetry, empirical, 'Whether the two legs of the exchange are symmetrically revocable.').

omega_variable(
    coalition_benefit_timing,
    'Do the anti-deal domestic coalitions collect during the frame''s operation, or only upon its dissolution?',
    'Trace coalition positions and advocacy-resource flows across the frame''s life: ratification-era opposition, activity during operation, and post-cessation policy capture.',
    'If benefit accrues only at dissolution, the coalition seat''s derived directionality overstates ongoing collection and should damp toward symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_benefit_timing, conceptual, 'Timing of beneficiary collection for the domestic-opposition seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 2015, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2015, 0.18).
narrative_ontology:measurement_basis(jcpo_tr_t2015, observed).
narrative_ontology:measurement(jcpo_tr_t2016, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2016, 0.15).
narrative_ontology:measurement_basis(jcpo_tr_t2016, observed).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2017, 0.17).
narrative_ontology:measurement_basis(jcpo_tr_t2017, observed).
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2018, 0.34).
narrative_ontology:measurement_basis(jcpo_tr_t2018, observed).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2019, 0.44).
narrative_ontology:measurement_basis(jcpo_tr_t2019, observed).
narrative_ontology:measurement(jcpo_tr_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2020, 0.51).
narrative_ontology:measurement_basis(jcpo_tr_t2020, observed).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2021, 0.55).
narrative_ontology:measurement_basis(jcpo_tr_t2021, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement_basis(jcpo_be_t2015, observed).
narrative_ontology:measurement(jcpo_be_t2016, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2016, 0.47).
narrative_ontology:measurement_basis(jcpo_be_t2016, observed).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2017, 0.49).
narrative_ontology:measurement_basis(jcpo_be_t2017, observed).
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement_basis(jcpo_be_t2018, observed).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement_basis(jcpo_be_t2019, observed).
narrative_ontology:measurement(jcpo_be_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2020, 0.7).
narrative_ontology:measurement_basis(jcpo_be_t2020, observed).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2021, 0.72).
narrative_ontology:measurement_basis(jcpo_be_t2021, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement_basis(jcpo_su_t2015, observed).
narrative_ontology:measurement(jcpo_su_t2016, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2016, 0.27).
narrative_ontology:measurement_basis(jcpo_su_t2016, observed).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2017, 0.29).
narrative_ontology:measurement_basis(jcpo_su_t2017, observed).
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2018, 0.42).
narrative_ontology:measurement_basis(jcpo_su_t2018, observed).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2019, 0.51).
narrative_ontology:measurement_basis(jcpo_su_t2019, observed).
narrative_ontology:measurement(jcpo_su_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2020, 0.57).
narrative_ontology:measurement_basis(jcpo_su_t2020, observed).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement_basis(jcpo_su_t2021, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, resource_allocation).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the JCPOA' conflates structurally distinct claims about the instrument's bindingness. Per the epsilon-invariance principle, the family decomposes into three readings, each with its own epsilon, beneficiary/victim structure, and type: binding_multilateral_reading (upstream — the frame's asserted legal character, cited as evidence by the other readings), graduated_compliance_reading, and this transactional_provisional_reading (downstream — the reading under which the 2018 cessation was exercised as a reserved right rather than committed as breach). Each story links the others via affects_constraints; the upstream reading's higher-confidence legal characterization is what the downstream readings accept, modify, or reject.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
