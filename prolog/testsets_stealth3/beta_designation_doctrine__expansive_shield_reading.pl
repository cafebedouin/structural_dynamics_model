% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Beta Designation as Comprehensive Liability Waiver (Expansive Shield Reading)
 *   domain: technology_law/software_liability/consumer_protection
 *
 * SUMMARY:
 *   Under the expansive shield reading, a vendor's application of a 'beta'
 *   label to any software, for any duration, in any context, discharges the
 *   vendor of all liability for defects — data loss, security breaches,
 *   downtime, consequential damages, and personal injury included. The
 *   designation is untethered from any actual testing phase: it may persist
 *   indefinitely on shipping products and may wrap systems deployed in
 *   life-safety and financial contexts. The arrangement is presented as
 *   freedom of contract and early-access economics; its operation is the
 *   wholesale externalization of defect costs onto users and onto third
 *   parties who never assented to anything. This file instantiates ONE
 *   reading of the beta_designation_doctrine kernel; the
 *   narrow_warning_reading and severity_carve_out_reading are separate
 *   constraint files linked via network.affects_constraints, not positions
 *   averaged into this one. The claim/metric gap is deliberate: claimed_type
 *   states the structure I believe true of this reading (snare — the
 *   testing-disclosure story is cover for enforced cost-shifting with
 *   suppressed exits), while the metrics describe its observed operation
 *   independently; the engine computes per-seat classifications from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - commercial_software_vendors: agenda_setter and primary beneficiary (institutional/arbitrage) — drafts the standardized waiver, controls the label, collects the avoided liability cost
 *   - consumer_end_users: primary payer (powerless/constrained) — bear all defect costs under non-negotiable adhesion terms
 *   - dependent_third_parties: payer and structurally excluded (powerless/trapped) — injured by beta-based systems they never licensed and could not exit
 *   - enterprise_deployers: dual-positioned payer/beneficiary (powerful/constrained) — capture cheap early access, internalize outage and breach costs
 *   - contract_enforcement_courts: observer (institutional/analytical) — sustain or refuse the waiver's enforceability at the margins
 *   - consumer_protection_regulators: observer (organized/analytical) — contest the reading's scope through unfair-terms enforcement
 *   - arbitration_providers: secondary beneficiary (organized/mobile) — collect fee volume from disputes the waiver machinery routes away from class courts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.82).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.7).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Beta Designation as Comprehensive Liability Waiver (Expansive Shield Reading)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability/consumer_protection").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, 'a143131c-f6a5-46b7-ba51-a5398e85ea4a').
narrative_ontology:cs_kernel_codification('a143131c-f6a5-46b7-ba51-a5398e85ea4a', formalized).
narrative_ontology:cs_authority_grounding('a143131c-f6a5-46b7-ba51-a5398e85ea4a', extraction).
narrative_ontology:cs_interpretation_layer_present('a143131c-f6a5-46b7-ba51-a5398e85ea4a').
narrative_ontology:cs_reading_relation('a143131c-f6a5-46b7-ba51-a5398e85ea4a', beta_designation_doctrine__narrow_warning_reading, forecloses).
narrative_ontology:cs_reading_relation('a143131c-f6a5-46b7-ba51-a5398e85ea4a', beta_designation_doctrine__severity_carve_out_reading, forecloses).
narrative_ontology:cs_axiom('a143131c-f6a5-46b7-ba51-a5398e85ea4a', foundational, beta_designation_discharges_all_defect_liability).
narrative_ontology:cs_axiom_status(beta_designation_discharges_all_defect_liability, holdable).
narrative_ontology:cs_axiom_grounding('a143131c-f6a5-46b7-ba51-a5398e85ea4a', beta_designation_discharges_all_defect_liability, conventional).
narrative_ontology:cs_axiom('a143131c-f6a5-46b7-ba51-a5398e85ea4a', secondary, waiver_operation_independent_of_actual_testing).
narrative_ontology:cs_axiom_status(waiver_operation_independent_of_actual_testing, holdable).
narrative_ontology:cs_axiom_grounding('a143131c-f6a5-46b7-ba51-a5398e85ea4a', waiver_operation_independent_of_actual_testing, conventional).
narrative_ontology:cs_reference_frame('a143131c-f6a5-46b7-ba51-a5398e85ea4a', comprehensive_waiver_baseline).
narrative_ontology:cs_drift_state('a143131c-f6a5-46b7-ba51-a5398e85ea4a', contemporary_consumer_protection_era, gap(repudiation_pressure, minor, false)).
narrative_ontology:cs_created_at('a143131c-f6a5-46b7-ba51-a5398e85ea4a', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, commercial_software_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, consumer_end_users).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, dependent_third_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, enterprise_deployers).
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, arbitration_providers).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, enterprise_deployers).
narrative_ontology:constraint_vindicates(beta_designation_doctrine__expansive_shield_reading, freedom_of_contract_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts standardized license terms declaring beta-labeled software provided as-is with all liability disclaimed, applies the label across product lines for indefinite periods, and enforces acceptance through clickwrap gates on installation and update. Collects the avoided liability cost directly as margin and pricing freedom. Controls both the designation and its legal interpretation; exit means nothing because the vendor can relabel, reprice, or restructure terms unilaterally at any time.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, commercial_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Install and run beta-labeled software under terms they did not negotiate and largely do not read. Bear the full cost of defects: lost data, breach remediation, downtime, and consequential losses. Declining a given product is possible, but comparable channels carry equivalent terms and workplaces or institutions often require specific software, so exit is costly and the terms themselves are never open to bargaining.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_end_users, payer,
    powerless, biographical, constrained, global).

% Passengers in vehicles running beta autonomy stacks, patients whose records move through beta health IT, occupants of buildings on beta-labeled control systems. They never saw a license, assented to nothing, and bear injury and damage costs when the underlying software fails. Their only theoretical recourse runs through tort claims that the waiver machinery is designed to deflect; there is no contractual relationship to exit because they were never parties to one.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, dependent_third_parties, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, dependent_third_parties, excluded).

% Deploy beta-labeled software in production environments to capture cost and speed advantages: no license fees, no vendor support obligations, early feature access. When defects land, they internalize outage, security, and remediation costs that a warrantied product would have transferred back to the vendor. Large deployers can negotiate bespoke indemnification; the mid-market cannot.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, enterprise_deployers, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(beta_designation_doctrine__expansive_shield_reading, enterprise_deployers, beneficiary).

% Decide whether blanket beta waivers survive unconscionability scrutiny, whether clickwrap assent suffices for total liability transfers, and where gross-negligence exceptions bite. General deference to assent-based contracting sustains the arrangement; occasional refusals at the severity margins are absorbed by the vendors' interpretation layer through redrafted clauses and favorable choice-of-law routing.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, contract_enforcement_courts, observer,
    institutional, generational, analytical, national).

% Enforce unfair-contract-terms regimes against standardized liability waivers, issue guidance treating perpetual beta labels as marketing rather than testing status, and propose boundary legislation. Their remedies reach particular markets or clauses; none has yet displaced the reading's core operation across its home jurisdiction.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_regulators, observer,
    organized, generational, analytical, continental).

% Administer the individual arbitrations that beta-license terms mandate in place of court and class-action channels. Fee volume flows from the waiver machinery's dispute-routing function; the providers neither set the terms nor bear defect costs, but their revenue depends on the routing the enforcement layer maintains.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, arbitration_providers, beneficiary,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, commercial_software_vendors).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The beta label nominally coordinates expectations between vendor and user: it signals unfinished software, recruits bug reports, and sets reliability expectations for early access. Under this reading the signal persists nominally but is untethered from any bounded testing relationship — it attaches to shipping products indefinitely and waives liability whether or not any testing occurs.
% TRANSFER_FUNCTION: Moves the entire cost of software defects — data loss, breach remediation, downtime, consequential damages, and personal injury — from vendors to users and to uninvolved third parties, priced implicitly into free or discounted early access.
% ABSENT_VOICES: Dependent third parties harmed by beta-based systems were never parties to any license and hold no seat anywhere in the arrangement's formation; future users are bound by perpetual waiver terms drafted before they existed; consumers in jurisdictions whose courts adopted enforceability doctrines had no representation when the defaults were set.
% DISAPPEARANCE_RATIONALE: If the comprehensive waiver vanished overnight, vendors would need to price liability risk explicitly — insurance, warranties, staged rollouts — beta channels would shrink to genuine test cohorts, software pricing would shift to reflect assumed liability, and deployment of beta-labeled software in safety-critical and financial contexts would halt pending hardening. Release practices, procurement, and the liability-insurance market for software would all reorganize.
% FOUNDING_PROBLEM: Pre-release software needed external testing, and testers needed clear notice that unstable code carries risk — a bounded disclosure for a bounded phase, so that early adopters understood what they were accepting and vendors could recruit informed feedback.
% FOUNDING_PROBLEM_CORROBORATION: Vendor trade groups attest the problem is still live on the theory that software is never finished; corroborating sources outside the benefiting parties — court opinions declining to enforce perpetual waivers, consumer-protection scholarship, and regulatory guidance treating beta labels as marketing rather than testing status — attest that the founding problem no longer describes the arrangement's operation, which now runs on waiver economics detached from any testing phase.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.82 at interval end) because the waiver transfers the entire defect-cost stack — including consequential and injury harms — with no temporal decay and no severity floor; the vendor's avoided liability is a direct, recurring gain. Suppression (0.70) reflects the enforcement machinery rather than physical coercion: standardized clickwrap presented on a take-it-or-leave-it basis, progressively hardened with mandatory arbitration and class-action waivers that specifically dismantle the coalition mechanism (class litigation) available to otherwise-powerless users. Theater_ratio reaches 0.50 because roughly half the designation's activity is performative — 'beta program,' 'feedback community,' 'testing phase' framing — while the operative function is the waiver itself; the label increasingly appears on products no one is meaningfully testing. Accessibility_collapse (0.55) is moderate: supported releases, paid SLAs, and open-source alternatives exist, but industry-wide adoption of the practice means understanding the waiver yields no practical exit — every comparable channel carries equivalent terms. Resistance (0.55) is real and sustained — unconscionability challenges, unfair-terms enforcement in consumer-protection regimes, legislative proposals — but has so far dented the practice only at the margins. The measurement series run on one shared time grid (points 0-30 at step 6) with all three metrics authored at every point; the trajectory is a monotonic ratchet, not a cycle — each enforcement layer (arbitration, class-action waiver, choice-of-law routing) locks in the previous layer's gains, so no oscillation is modeled and none should be inferred.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent types from identical structural facts. From the vendor seat, the arrangement presents as legitimate coordination it authored: voluntary early access, disclosed risk, mutual gain — a rope-shaped world justified by freedom of contract. From the consumer seat, the same structure operates as pure extraction: non-negotiable terms, no duration limit, no recourse. From the dependent-third-party seat it is worse than extraction — injury without even nominal assent. The engine computes this divergence from power and exit data; the authored snare claim is this story's own structural judgment, not an adjudication of the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial software vendors sit at the beneficiary end: they collect the avoided liability cost directly, control both the label and the license text, and hold arbitrage-grade exit (relabel, reprice, restructure terms unilaterally). Consumer end users sit near the full-target end: they bear the transferred costs under constrained exit — declining a product is possible, but industry-wide term uniformity and workplace or institutional dependence on specific software make exit costly and terms impossible to negotiate. Dependent third parties occupy the extreme target position: trapped with no contractual relationship at all, bearing injury costs the waiver machinery is designed to deflect into tort thickets. Enterprise deployers derive d mid-to-high: genuine benefit from cheap access offset by internalized defect costs. No directionality_overrides are authored: the beneficiary/victim declarations plus exit atoms produce the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification prevents the arrangement's genuine historical coordination function — bounded testing-phase disclosure that recruited informed testers — from laundering its current operation, in which the designation is deliberately severed from any testing relationship. Conversely, it prevents overcorrection: the vestigial signal still conveys some information, which is why theater_ratio is 0.50 rather than higher. The founding mandate (bounded disclosure for a bounded phase) is dead under this reading — the arrangement persists on waiver economics, not testing economics — and the R5 interview records that obsolescence; the mismatch between dead founding problem and a world that rearranges without the arrangement is the capture/zombie signature this reading exhibits relative to the kernel's origin. A piton reading is rejected because extraction here has a concentrated capturer (vendors) actively maintaining the machinery — inertia is not what holds it up; enforcement is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint instantiates the expansive_shield_reading of the beta_designation_doctrine kernel; how would adopting a sibling reading change the structural classification?',
    'Comparative classification across the three reading-files of the kernel: if narrow_warning_reading computes as rope or tangled_rope while this reading computes as snare, the kernel''s extraction is located precisely in the boundary removals this reading performs.',
    'A sibling reading shrinks the victim set to genuine test cohorts, restores base product liability, and drops epsilon toward coordination-cost levels. The disagreement is located in three structural elements: temporal bound (none versus bounded testing phase), preserved base liability (none versus preserved), and severity boundary (none versus categorical life-safety/financial carve-out).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this file is one reading of a contested kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    cross_jurisdiction_enforceability,
    'Does the comprehensive beta waiver actually survive enforcement when challenged across jurisdictions, or do courts routinely refuse it?',
    'Systematic tracking of litigation outcomes involving beta-label exculpatory clauses: enforceability rates by jurisdiction, clause type, and harm severity.',
    'If courts systematically refuse enforcement, the arrangement''s effective extraction collapses toward a theatrical threat (piton-like profile despite the aggressive paper terms); if enforcement is routine, the snare classification is confirmed with the measured epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_jurisdiction_enforceability, empirical, 'Gap between the waiver''s paper scope and its realized enforceability.').

omega_variable(
    clickwrap_assent_authenticity,
    'Does click-through assent to a beta waiver constitute consent capable of legitimately carrying a total, perpetual liability transfer?',
    'Comprehension studies of license-term readership combined with judicial treatment distinguishing clickwrap from browsewrap assent.',
    'If assent is nominal, the waiver''s conventional legitimacy basis fails, suppression reads as unilateral imposition rather than contracted risk allocation, and user directionality moves nearer the full-target end than the derivation alone would place it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clickwrap_assent_authenticity, conceptual, 'Whether the arrangement''s legitimacy rests on genuine assent or fabricated consent.').

omega_variable(
    gross_negligence_background_carveout,
    'Does background doctrine voiding exculpatory clauses for gross negligence or willful misconduct already bound the supposedly comprehensive waiver?',
    'Survey of appellate treatments of software-context exculpatory clauses where conduct rose to gross negligence.',
    'A reliable background carve-out lowers epsilon somewhat and narrows the structural distance to severity_carve_out_reading; its absence confirms this reading''s comprehensiveness claim as operative rather than merely asserted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gross_negligence_background_carveout, empirical, 'Whether background tort doctrine silently bounds the waiver this reading declares unlimited.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(beta_tr_t6, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement(beta_tr_t12, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(beta_tr_t18, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(beta_tr_t24, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 24, 0.44).
narrative_ontology:measurement(beta_tr_t30, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 30, 0.5).

% Extraction over time
narrative_ontology:measurement(beta_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(beta_be_t6, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(beta_be_t12, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(beta_be_t18, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 18, 0.73).
narrative_ontology:measurement(beta_be_t24, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(beta_be_t30, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 30, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(beta_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(beta_su_t6, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(beta_su_t12, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(beta_su_t18, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 18, 0.63).
narrative_ontology:measurement(beta_su_t24, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(beta_su_t30, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beta_designation_doctrine__expansive_shield_reading, information_standard).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__narrow_warning_reading).
narrative_ontology:affects_constraint(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine__severity_carve_out_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'beta designation doctrine' decomposes into three structurally distinct readings of one kernel, per the epsilon-invariance principle. This file (expansive_shield_reading) authors the reading with no temporal bound, no preserved base liability, and no severity carve-out — hence the highest epsilon of the family. narrow_warning_reading restores the bounded-testing-phase structure (lower epsilon, coordination-dominant); severity_carve_out_reading removes only the life-safety/financial exposure (intermediate epsilon concentrated in consumer contexts). Each file carries its own beneficiaries, victims, and claimed_type; the upstream expansive reading influences the downstream contest because vendors cite its enforceability precedent against both sibling readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
