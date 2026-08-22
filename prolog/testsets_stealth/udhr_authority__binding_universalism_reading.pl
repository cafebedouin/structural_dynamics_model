% ============================================================================
% CONSTRAINT STORY: udhr_authority__binding_universalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__binding_universalism_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: udhr_authority__binding_universalism_reading
 *   human_readable: UDHR Binding Universalism — Justiciable Individual Rights Enforceable Against States Regardless of Consent
 *   domain: international law / political philosophy / human rights doctrine
 *
 * SUMMARY:
 *   This story instantiates one reading of a contested kernel. The kernel is
 *   the authority of the Universal Declaration of Human Rights; this file
 *   authors the binding-universalist reading: that the Declaration
 *   establishes justiciable individual rights enforceable against states
 *   regardless of consent. The arrangement under contest — the one the story
 *   is about — is the non-consent-based justiciable rights regime itself:
 *   tribunals holding coercive interpretive authority over sovereign
 *   governments, state autonomy subordinated to an individual-rights baseline
 *   no state can opt out of. Epsilon is authored for that arrangement,
 *   assessed by this reading's own lights: the reading endorses the regime
 *   and still authors its burden on state autonomy as high, because the
 *   regime's defining move is stripping the consent protection on which state
 *   autonomy rests. The sibling readings are separate constraints with
 *   separate epsilon values — aspirational sovereignty (consent-gated,
 *   near-zero non-consensual burden) and customary emergence (burden present
 *   but legitimacy sourced in state practice) — and are not averaged here;
 *   the epsilon values differ because the constraints differ. The
 *   claim/metric independence rule applies: claimed_type states what this
 *   seat believes structurally true; the metrics state what is descriptively
 *   true of the regime's operation.
 *
 * KEY AGENTS:
 *   - international_rights_tribunals: agenda setter and primary beneficiary (institutional/identity_locked) — administers the regime, writes its doctrine, and collects jurisdiction, docket, and standing with every assertion of authority
 *   - small_and_middle_power_states: primary target (organized/constrained) — bear compliance, policy revision, and reparations without the leverage to resist or renegotiate
 *   - great_powers: nominal target with arbitrage exit (powerful/arbitrage) — authority is claimed over them, but non-ratification, reservations, and institutional position keep effective exposure selective
 *   - individual_rights_claimants: primary beneficiary (powerless/trapped) — receive forum, judgment, and occasional remedy against the state they live under
 *   - democratic_electorates: dual-positioned seat (moderate/constrained) — fund and implement compliance they never voted for, and invoke the same protections when their own state turns on them
 *   - human_rights_advocacy_organizations: secondary beneficiary (organized/mobile) — receive standing, venue, and relevance from the arrangement they litigate through
 *   - non_consenting_state_populations: excluded voice (powerless/trapped) — governed by rulers the reading claims authority over, with no seat in any authorizing or petitioning process
 *   - international_law_scholars: analytical observer (analytical/analytical) — maps the authority claims and supplies the doctrinal defenses and critiques both sides cite
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, 0.7).
domain_priors:suppression_score(udhr_authority__binding_universalism_reading, 0.72).
domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__binding_universalism_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__binding_universalism_reading, "UDHR Binding Universalism — Justiciable Individual Rights Enforceable Against States Regardless of Consent").
narrative_ontology:topic_domain(udhr_authority__binding_universalism_reading, "international law / political philosophy / human rights doctrine").

domain_priors:requires_active_enforcement(udhr_authority__binding_universalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__binding_universalism_reading, 'e2408940-1f1c-40ad-8749-3cf920efdf4b').
narrative_ontology:cs_kernel_codification('e2408940-1f1c-40ad-8749-3cf920efdf4b', fixed_text).
narrative_ontology:cs_authority_grounding('e2408940-1f1c-40ad-8749-3cf920efdf4b', lineage).
narrative_ontology:cs_interpretation_layer_present('e2408940-1f1c-40ad-8749-3cf920efdf4b').
narrative_ontology:cs_reading_relation('e2408940-1f1c-40ad-8749-3cf920efdf4b', udhr_authority__aspirational_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('e2408940-1f1c-40ad-8749-3cf920efdf4b', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('e2408940-1f1c-40ad-8749-3cf920efdf4b', foundational, rights_bind_without_consent).
narrative_ontology:cs_axiom_status(rights_bind_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('e2408940-1f1c-40ad-8749-3cf920efdf4b', rights_bind_without_consent, deontological).
narrative_ontology:cs_axiom('e2408940-1f1c-40ad-8749-3cf920efdf4b', foundational, individuals_hold_justiciable_standing).
narrative_ontology:cs_axiom_status(individuals_hold_justiciable_standing, holdable).
narrative_ontology:cs_axiom_grounding('e2408940-1f1c-40ad-8749-3cf920efdf4b', individuals_hold_justiciable_standing, deontological).
narrative_ontology:cs_reference_frame('e2408940-1f1c-40ad-8749-3cf920efdf4b', universal_justiciable_rights_baseline).
narrative_ontology:cs_drift_state('e2408940-1f1c-40ad-8749-3cf920efdf4b', contemporary_sovereignty_backlash, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e2408940-1f1c-40ad-8749-3cf920efdf4b', '').
narrative_ontology:cs_kernel_id(udhr_authority__binding_universalism_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, international_rights_tribunals).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, human_rights_advocacy_organizations).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, small_and_middle_power_states).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, great_powers).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, democratic_electorates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, democratic_electorates).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, individual_rights_supremacy_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, universal_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__binding_universalism_reading, erga_omnes_rights_enforcement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who bring claims against their own or other governments in international and regional fora. What flows to them is a hearing, a judgment, occasionally reparations — often years after the harm and enforceable only through continued political pressure on their state. Leaving the arrangement is not available to them in any meaningful sense: the alternative forum is the domestic system they are suing.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, individual_rights_claimants, beneficiary,
    powerless, biographical, trapped, global).

% Regional courts, treaty-monitoring bodies, and international criminal institutions that receive petitions, issue binding judgments, and monitor state compliance. Their docket, budget, and institutional standing grow with every government brought within reach. They write the interpretive doctrine that defines what the rights require, and they cannot relinquish that role without dissolving; their identity is the mandate.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, international_rights_tribunals, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, international_rights_tribunals, beneficiary).

% Governments with limited geopolitical leverage that have accepted treaty jurisdiction or been brought within it through customary-claim arguments. They bear judgment compliance, policy revision, and reparations with little capacity to resist or renegotiate. Denunciation carries reputational, financing, and alliance costs — and under this reading, denunciation does not release them from the rights themselves.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, small_and_middle_power_states, payer,
    organized, generational, constrained, global).

% Large states with Security Council standing, major economies, or strategic indispensability. The same authority is claimed over them, but they can decline ratification, enter reservations, shield allies through institutional position, or comply selectively without the isolation a smaller state would face. Their exposure tends to arrive indirectly — through allies, companies, or citizens litigating abroad.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, great_powers, payer,
    powerful, generational, arbitrage, global).

% Voting publics whose elected governments implement judgments they never voted for — franchise rulings, immigration decisions, sentencing reform. They can change their government but cannot, by voting, alter the tribunal's jurisdiction or doctrine. The same arrangement also protects them: when their own state turns coercive, these are the rights and the forum they invoke.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, democratic_electorates, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, democratic_electorates, beneficiary).

% NGOs, legal clinics, and bar networks that litigate test cases, file shadow reports, and mobilize compliance pressure. The arrangement supplies their standing, their strategic venue, and much of their public relevance; funding and recruitment track its caseload. They can redirect to other strategies at will — nothing binds them to this forum.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, human_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% People living under governments that never accepted the arrangement. The reading claims enforcement power over their rulers regardless, but they took no part in authorizing it, most cannot petition the tribunals directly, and they experience it mainly as external pressure on their state — conditionality, sanctions language, public reporting — rather than as a forum available to them.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, non_consenting_state_populations, excluded,
    powerless, biographical, trapped, global).

% Academic lawyers and political theorists who map the arrangement's authority claims, write the doctrinal defenses and the critiques, and staff the inquiries both governments and tribunals cite. They collect no compliance and pay no judgments; their stake is interpretive authority over what the arrangement means.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, international_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__binding_universalism_reading, international_rights_tribunals).
narrative_ontology:fixing_cost_class(udhr_authority__binding_universalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a forum above the state for claims by individuals against their own or other governments, and maintains a common baseline of treatment across jurisdictions. Without it, the state alleged to have violated the right is also the final adjudicator of the claim, and no floor of treatment binds across borders.
% TRANSFER_FUNCTION: Moves adjudicative authority from state governments to international courts, treaty bodies, and criminal tribunals; moves enforcement standing to individuals and advocacy organizations; moves compliance costs — policy revision, reparations, institutional reform — onto state budgets and, through taxation and policy change, onto domestic electorates.
% ABSENT_VOICES: Populations of states that never consented: the reading claims enforcement authority over their governors without their participation in any authorizing act, and most cannot petition the tribunals directly. Also absent: domestic sovereigntist movements, which contest tribunal supremacy but had no seat in the treaty-body interpretive process; and the 1948 abstaining delegations (Soviet bloc, Saudi Arabia, South Africa), whose objections to specific articles were never resolved — only outvoted.
% DISAPPEARANCE_RATIONALE: Individual petition systems would close and pending cases would lose their forum; domestic courts would lose the supranational interpretive anchor they cite against their own governments; advocacy litigation strategies would dissolve; and states would regain final authority over rights questions. The post-war settlement's central innovation — rights positioned above sovereignty — would revert to unenforceable aspiration, and the rearrangement would be large and immediate.
% FOUNDING_PROBLEM: The Holocaust showed that a state could persecute its own population with complete legal immunity: sovereignty barred outsiders, and domestic law was the persecutor's instrument. The Declaration was drafted to assert rights no state could legitimately deny; this reading operationalizes that assertion as justiciable, non-consent-based enforcement against states.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the payer states themselves — the Declaration's preamble was adopted without dissent among voting states, and the post-war constitutions of Germany and Japan entrenched rights precisely to bind their own sovereigns; historians of the Nuremberg record and the drafting travaux attest the atrocity problem; and sovereigntist critics concede the historical problem while disputing this remedy. No seat inside the regime is the source of the attestation.
narrative_ontology:disappearance_verdict(udhr_authority__binding_universalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__binding_universalism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__binding_universalism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_authority__binding_universalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__binding_universalism_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__binding_universalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_authority__binding_universalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.70 at interval end) because the reading removes consent as a defense: compliance costs — judgment implementation, policy revision, reparations, institutional reform — land on state budgets and, through them, on electorates, while the regime's authority grows with each assertion. Suppression (0.72) is the reading's defining structural feature: 'regardless of consent' is precisely the foreclosure of the exit international law otherwise affords states, and the enforcement machinery — individual petition, binding judgments, compliance monitoring — exists to make that foreclosure operative. Suppression is authored as a raw structural property, unscaled; only extractiveness is context-scaled downstream. Theater (0.33) is moderate-low: adjudication is real and consequential, but a documented share of regime activity — universal periodic review cycles, resolutions, shadow-reporting — is declaratory with limited bite, and that share has crept up since the mid-2000s as procedural venues multiplied. Accessibility_collapse (0.60): the reading's logic forecloses the pure-sovereignty alternative in principle, but in practice alternatives persist — great-power non-ratification, reservations, denunciation — so alternatives are narrowed, not eliminated. Resistance (0.62): sustained sovereigntist pushback, court-curbing, selective compliance, and open political attack on tribunal supremacy in several democracies. The temporal series share one time grid (1948/1959/1976/1993/2002/2012/2018/2024) so no metric's value is imputed at another's points: base_extractiveness rises with institutional hardening (individual petition mechanisms, the ad hoc tribunals, the permanent criminal court, consolidation thereafter); theater_ratio falls as real adjudication displaced declaratory aspiration, then creeps up with procedural proliferation; suppression_requirement rises monotonically as the enforcement infrastructure matured — this is the story's enforcement-history trace, and it is authored because enforcement-capacity change is exactly what this interval records. End-state values match base_properties.
 *
 * PERSPECTIVAL GAP:
 *   The tribunal seat and the payer seats should compute different types from the same structural data. From the bench, the arrangement is the coordination machinery it staffs: petitions heard, judgments issued, a rights baseline maintained. From a small state's seat, the same structure operates as authority asserted over it without its consent and enforced through reputational and financial pressure it cannot match. Great powers and small states hold the same nominal role — state under claimed authority — but their exits differ structurally, so the same regime lands on them as different arrangements. Electorates sit dual: they fund compliance they never voted for and invoke the same regime's protections when their own state turns on them. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: individual claimants (receive forum and remedy; d near the beneficiary end), advocacy organizations (receive standing, venue, relevance; mobile exit), and tribunals (receive jurisdiction, docket, doctrinal authority — they are also the agenda setters, so their derived d sits near the beneficiary end despite running the machinery). Targets: small and middle-power states bear the burden without leverage and, under a reading that denies consent-based release, their exit is the most foreclosed in the system — d near the full-target end. Great powers are nominally targeted but their arbitrage exit keeps effective burden well below small states'. Democratic electorates sit near symmetric. Two directionality overrides are authored because the derivation would misread two seats: great powers' arbitrage-grade exit would be read as beneficiary-grade d, but escaping a regime is not collecting from it — override to 0.45; electorates appear in the victim declaration, which alone would drive d toward the full-target end, but their dual position — they hold the rights too — moderates it — override to 0.55. Receipt surface: the gains — jurisdiction, docket, doctrinal authority — demonstrably accrue to the tribunal seat, so gain_flow names it rather than 'diffuse'; fixing is prohibitive because the arrangement is now embedded in domestic constitutional orders and incorporated conventions that would cost the fixers more to dismantle than the autonomy regained.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — sovereignty as immunity for atrocity — is live, not dead: state abuse of populations persists and the regime's docket grows. There is no sunset and no atrophied function, so the mandatrophy machinery should not fire; the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no zombie flag. The tangled_rope claim is what prevents mislabeling in both directions: a pure-coordination reading would erase the non-consent burden that is the regime's defining structural feature; a pure-extraction reading would erase the genuine coordination function — without a forum above the state, rights violations have no remedy and no common floor across jurisdictions. Both are present in the same structure, enforced by the same machinery, which is the tangled-rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_udhr_authority,
    'This constraint is one reading of the udhr_authority kernel. Does the victim and beneficiary structure survive under the sibling readings — and what exactly changes if the aspirational-sovereignty or customary-emergence reading is adopted in tribunal practice?',
    'Track whether regional courts and treaty bodies continue to assert non-consent-based jurisdiction (this reading), retreat to consent-plus-practice grounding (customary reading), or reframe the Declaration as guidance (aspirational reading); treaty revision or a binding ICJ advisory opinion would resolve it formally.',
    'Under the aspirational reading this constraint''s victim set empties: without non-consensual binding there is no extraction to measure and the arrangement collapses into consent-priced treaty relations. Under the customary reading the burden on states persists but its legitimacy source shifts to state conduct, moderating the non-consent objection and changing which seats are trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_udhr_authority, conceptual, 'Which reading of UDHR authority is instantiated changes the constraint''s victim structure entirely.').

omega_variable(
    justiciability_textual_grounding,
    'Does the Declaration''s text and drafting history ground justiciable, non-consent-based enforceability, or is that structure a judicial construction layered onto a deliberately declaratory instrument that contains no enforcement machinery and was adopted as a General Assembly resolution rather than a treaty?',
    'Systematic reading of the travaux préparatoires, drafting-committee records, and early state-practice record against the interpretive claims in leading tribunal judgments; the 1948 vote itself is the central datum.',
    'If construction, the regime''s authority rests on institutional self-extension — the tribunals'' authority grows precisely by preventing revision of the reading — and the burden profile hardens. If textual, the reading is a discovery the drafters under-implemented, and the burden on states is the price of completing the founding design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justiciability_textual_grounding, empirical, 'Whether justiciable non-consent bindingness is grounded in the instrument or constructed by its interpreters.').

omega_variable(
    great_power_arbitrage_asymmetry,
    'Does the regime''s effective burden concentrate on small and middle-power states while great powers arbitrage — never ratifying, entering reservations, shielding allies — converting a universal arrangement into a selectively enforced one?',
    'Comparative exposure and compliance data across state size and power: judgment rates, compliance rates, ratification gaps, and Security Council referral patterns by great-power alignment.',
    'If concentration holds, the same nominal regime operates as different arrangements for different victims — the coordination claim weakens toward selective enforcement, and the trapped-seat burden intensifies relative to the arbitrage seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(great_power_arbitrage_asymmetry, empirical, 'Whether the burden on states is universal or concentrated on those without exit.').

omega_variable(
    democratic_override_legitimacy,
    'Is the regime''s override of domestic majoritarian decisions — prisoner franchise, immigration, sentencing — a cost this reading''s own commitments can absorb (rights trump process), or does accumulated override erode the democratic legitimacy the reading needs to hold?',
    'Not resolvable by data alone: it depends on whether the reading''s deontological premise is accepted. Track override frequency, legislative pushback, and whether domestic polities re-authorize jurisdiction through statute or referendum.',
    'If override is absorbed, the electorate seat''s burden is the price of the coordination function; if it erodes legitimacy, the reading''s authority base contracts and voluntary compliance decays into enforcement, raising the effective burden on every payer seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_override_legitimacy, preference, 'Whether tribunal override of democratic process is a justified cost or a legitimacy-eroding overreach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__binding_universalism_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__binding_universalism_reading, theater_ratio, 1948, 0.65).
narrative_ontology:measurement(udhr_tr_t1959, udhr_authority__binding_universalism_reading, theater_ratio, 1959, 0.58).
narrative_ontology:measurement(udhr_tr_t1976, udhr_authority__binding_universalism_reading, theater_ratio, 1976, 0.48).
narrative_ontology:measurement(udhr_tr_t1993, udhr_authority__binding_universalism_reading, theater_ratio, 1993, 0.4).
narrative_ontology:measurement(udhr_tr_t2002, udhr_authority__binding_universalism_reading, theater_ratio, 2002, 0.32).
narrative_ontology:measurement(udhr_tr_t2012, udhr_authority__binding_universalism_reading, theater_ratio, 2012, 0.3).
narrative_ontology:measurement(udhr_tr_t2018, udhr_authority__binding_universalism_reading, theater_ratio, 2018, 0.31).
narrative_ontology:measurement(udhr_tr_t2024, udhr_authority__binding_universalism_reading, theater_ratio, 2024, 0.33).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__binding_universalism_reading, base_extractiveness, 1948, 0.3).
narrative_ontology:measurement(udhr_be_t1959, udhr_authority__binding_universalism_reading, base_extractiveness, 1959, 0.38).
narrative_ontology:measurement(udhr_be_t1976, udhr_authority__binding_universalism_reading, base_extractiveness, 1976, 0.47).
narrative_ontology:measurement(udhr_be_t1993, udhr_authority__binding_universalism_reading, base_extractiveness, 1993, 0.56).
narrative_ontology:measurement(udhr_be_t2002, udhr_authority__binding_universalism_reading, base_extractiveness, 2002, 0.63).
narrative_ontology:measurement(udhr_be_t2012, udhr_authority__binding_universalism_reading, base_extractiveness, 2012, 0.67).
narrative_ontology:measurement(udhr_be_t2018, udhr_authority__binding_universalism_reading, base_extractiveness, 2018, 0.69).
narrative_ontology:measurement(udhr_be_t2024, udhr_authority__binding_universalism_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t1948, udhr_authority__binding_universalism_reading, suppression_requirement, 1948, 0.25).
narrative_ontology:measurement(udhr_su_t1959, udhr_authority__binding_universalism_reading, suppression_requirement, 1959, 0.33).
narrative_ontology:measurement(udhr_su_t1976, udhr_authority__binding_universalism_reading, suppression_requirement, 1976, 0.43).
narrative_ontology:measurement(udhr_su_t1993, udhr_authority__binding_universalism_reading, suppression_requirement, 1993, 0.55).
narrative_ontology:measurement(udhr_su_t2002, udhr_authority__binding_universalism_reading, suppression_requirement, 2002, 0.62).
narrative_ontology:measurement(udhr_su_t2012, udhr_authority__binding_universalism_reading, suppression_requirement, 2012, 0.67).
narrative_ontology:measurement(udhr_su_t2018, udhr_authority__binding_universalism_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(udhr_su_t2024, udhr_authority__binding_universalism_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__binding_universalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, customary_emergence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'UDHR authority' decomposes into three structurally distinct constraints, per the epsilon-invariance principle: this binding-universalist reading (justiciable, non-consent-based enforcement; high burden on state autonomy), the aspirational-sovereignty reading (moral guidance; consent-gated; no non-consensual burden to measure), and the customary-emergence reading (binding via state practice and opinio juris; burden present but legitimacy sourced in state conduct). Their epsilon values differ because they are different constraints, not one constraint measured differently. The upstream reading (this one) supplies the tribunal practice that the customary reading must answer to; each story links the others here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, powerful, 0.45).
constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
