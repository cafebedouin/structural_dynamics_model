% ============================================================================
% CONSTRAINT STORY: imposition_mechanism_kernel__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_mechanism_kernel__endogenous_climb_reading, []).

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
 *   constraint_id: imposition_mechanism_kernel__endogenous_climb_reading
 *   human_readable: Endogenous Climb of Norm Legitimacy (State Mandate as Ratification)
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This story instantiates one reading of how new norms acquire legitimacy
 *   during state formation: the endogenous_climb_reading, in which legitimacy
 *   is achieved bottom-up and the state's mandate arrives as ratification of
 *   an already-accomplished social fact. The standing arrangement under
 *   assessment is the codification episode itself — diffusion decade,
 *   enactment, and the settling decades — read by this reading's own lights:
 *   adoption outruns the statute, enforcement machinery stays thin,
 *   resistance is scattered, and the state coordinates and certifies rather
 *   than manufactures compliance. The interval runs T=0 (onset of organized
 *   diffusion) to T=30 (three decades on), with enactment at T=10. The claim
 *   and the metrics are independent authored facts: the claim is rope
 *   (net-beneficiary coordination with minimal coercive overhead); the metric
 *   series describes what this reading takes the record to show — low
 *   extraction concentrated on a small residual population, a one-time
 *   theatrical pulse at enactment, and enforcement that builds briefly and
 *   then decays as normalization completes.
 *
 * KEY AGENTS:
 *   - central_state_administration: agenda-setter and beneficiary (institutional/arbitrage) — surveys practice, drafts the mandate to match it, collects the reform's legitimacy at low cost
 *   - adopting_communities: primary beneficiary (moderate/constrained) — their prior practice becomes the legal standard
 *   - norm_entrepreneurs: beneficiary (organized/mobile) — diffusion campaigners vindicated by codification
 *   - norm_dissenters: residual target (powerless/trapped) — bound by a statute they never accepted
 *   - local_custom_holders: marginal target with offsetting benefit (moderate/constrained) — variants superseded, uniformity gained
 *   - historical_sociologists: analytical observer — date adoption curves against enactment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).
domain_priors:suppression_score(imposition_mechanism_kernel__endogenous_climb_reading, 0.12).
domain_priors:theater_ratio(imposition_mechanism_kernel__endogenous_climb_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_mechanism_kernel__endogenous_climb_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_mechanism_kernel__endogenous_climb_reading, rope).
narrative_ontology:human_readable(imposition_mechanism_kernel__endogenous_climb_reading, "Endogenous Climb of Norm Legitimacy (State Mandate as Ratification)").
narrative_ontology:topic_domain(imposition_mechanism_kernel__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_mechanism_kernel__endogenous_climb_reading, 'c832f41c-97b6-4027-957e-e570de3a5bb7').
narrative_ontology:cs_kernel_codification('c832f41c-97b6-4027-957e-e570de3a5bb7', distributed).
narrative_ontology:cs_authority_grounding('c832f41c-97b6-4027-957e-e570de3a5bb7', expertise).
narrative_ontology:cs_interpretation_layer_present('c832f41c-97b6-4027-957e-e570de3a5bb7').
narrative_ontology:cs_reading_relation('c832f41c-97b6-4027-957e-e570de3a5bb7', imposition_mechanism_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('c832f41c-97b6-4027-957e-e570de3a5bb7', imposition_mechanism_kernel__hybrid_legitimation_reading, coexists_with).
narrative_ontology:cs_axiom('c832f41c-97b6-4027-957e-e570de3a5bb7', foundational, legitimacy_precedes_legal_mandate).
narrative_ontology:cs_axiom_status(legitimacy_precedes_legal_mandate, holdable).
narrative_ontology:cs_axiom_grounding('c832f41c-97b6-4027-957e-e570de3a5bb7', legitimacy_precedes_legal_mandate, empirically_contingent).
narrative_ontology:cs_axiom('c832f41c-97b6-4027-957e-e570de3a5bb7', secondary, state_role_is_ratification_not_generation).
narrative_ontology:cs_axiom_status(state_role_is_ratification_not_generation, holdable).
narrative_ontology:cs_axiom_grounding('c832f41c-97b6-4027-957e-e570de3a5bb7', state_role_is_ratification_not_generation, empirically_contingent).
narrative_ontology:cs_reference_frame('c832f41c-97b6-4027-957e-e570de3a5bb7', popular_consensus_precedes_mandate).
narrative_ontology:cs_drift_state('c832f41c-97b6-4027-957e-e570de3a5bb7', contemporary_revisionist_scholarship, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('c832f41c-97b6-4027-957e-e570de3a5bb7', '').
narrative_ontology:cs_kernel_id(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, adopting_communities).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, central_state_administration).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, norm_dissenters).
narrative_ontology:constraint_victim(imposition_mechanism_kernel__endogenous_climb_reading, local_custom_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_mechanism_kernel__endogenous_climb_reading, local_custom_holders).
narrative_ontology:constraint_vindicates(imposition_mechanism_kernel__endogenous_climb_reading, endogenous_norm_legitimacy_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Practiced the norm for a generation before any statute mentioned it — in households, markets, and parish registers. When the state codified it, their existing conduct became the legal standard: courts began enforcing what they already did, and laggard regions were pulled up to their practice. Their costs are small alignments where local usage differed from the statutory wording. Leaving the norm remains legally possible but would cut them off from contracts, marriage registration, and neighborly standing, and almost none want to leave.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, adopting_communities, beneficiary,
    moderate, generational, constrained, national).

% Campaigned for the norm through voluntary associations, newspapers, and pulpits during the diffusion decade, absorbing ridicule and occasional prosecution for advocating what was then unconventional. Codification vindicated their cause, made their arguments citable authority, and raised their standing. Many immediately redirect their organizing energy to the next cause; their attachment to this particular arrangement ends at victory.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_entrepreneurs, beneficiary,
    organized, biographical, mobile, national).

% Surveyed existing practice through commissions and statistical bureaus, found convergence already far advanced, and drafted the mandate to match what the majority already did. Promulgation cost it little: no mass persuasion campaign, no large enforcement establishment. It collects the reform's credit, records the statute as its achievement, and keeps discretion to amend or quietly shelve provisions because its sunk investment was ratification rather than manufacture of consent.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, central_state_administration, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, central_state_administration, beneficiary).

% Declined the norm before codification on religious conviction, occupational necessity, or simple habit. The statute turned their conduct from eccentricity into illegality. Prosecutions are sparse and unevenly distributed, but fines, registry refusals, and social censure fall on them, and every jurisdiction inside the state enforces the same standard, so there is nowhere to relocate short of emigration.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, norm_dissenters, payer,
    powerless, biographical, trapped, national).

% Accepted the norm's substance but practiced recognized regional or occupational variants — different forms, dates, or procedures serving the same function. The statutory standard superseded their variants in court and administration. They gain uniform legal treatment and the end of inter-jurisdictional dispute, and they lose the standing and convenience of their own forms. Their ledger has entries on both sides.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, local_custom_holders, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(imposition_mechanism_kernel__endogenous_climb_reading, local_custom_holders, beneficiary).

% Date adoption against enactment using parish registers, court dockets, market records, probate inventories, and press archives, stratified by region and class. Their sequence findings — whether practice preceded statute broadly or only among elites — are the evidential ground on which this account of the arrangement stands or falls. They collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(imposition_mechanism_kernel__endogenous_climb_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_mechanism_kernel__endogenous_climb_reading, central_state_administration).
narrative_ontology:fixing_cost_class(imposition_mechanism_kernel__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the tail-end of a standardization problem: once a population has largely converged on a norm informally, codification makes the convergent practice uniform, legible to courts and registries, and enforceable at the margins, replacing patchwork local expectation and inter-jurisdictional dispute with a single rule.
% TRANSFER_FUNCTION: Moves little in money or goods. It moves definitional authority over the norm from dispersed social practice to the state, moves enforcement obligation onto residual dissenters and variant-holders, and moves reform credit to the ratifying government.
% ABSENT_VOICES: Dissenters and variant-holders lived under the statute but were absent from the codifying conversation: the commissions surveyed practice, not objections, and the mandate ratified a consensus they were never consulted within. Their objection — that agreement among the aligned overstated acceptance — appears in petitions and trial records but entered no drafting room.
% DISAPPEARANCE_RATIONALE: The underlying norm would survive repeal — it preceded the statute — but legal uniformity would not: local variants would re-diverge, courts would lose the single standard they now administer, dissenters' obligations would lapse, and the state's ratification credit would evaporate. Administrative arrangements built on the codified form depend on it.
% FOUNDING_PROBLEM: Divergent local practice generated disputes that courts could not resolve consistently, transactions crossing jurisdictions carried unpredictable terms, and registries could not certify status uniformly. Codification promised one legible rule where there had been many customs.
% FOUNDING_PROBLEM_CORROBORATION: Pre-enactment provincial court dockets and municipal petitions record the dispute volume and inconsistency the codification addressed; these originate with local judiciaries and petitioners, outside the ratifying ministry's self-account. What no outside source attests is the stronger claim that the mandate was unnecessary — the record corroborates a real coordination problem, not the irrelevance of the statute.
narrative_ontology:disappearance_verdict(imposition_mechanism_kernel__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_mechanism_kernel__endogenous_climb_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_mechanism_kernel__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imposition_mechanism_kernel__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_mechanism_kernel__endogenous_climb_reading, 0.18, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).
:- end_tests(imposition_mechanism_kernel__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.18 at interval end) because the mandate transfers little material value: its yield is standardization for adopters and legitimacy for the state, with residual extraction falling on dissenters and variant-holders. Suppression is low (0.12) because the arrangement does not depend on enforcement to hold — compliance is largely voluntary, the enforcement establishment stays thin, and the series shows suppressive effort peaking modestly around T=15 (selective prosecution of dissenters) and decaying thereafter. Theater peaks at 0.30 exactly at enactment (T=10), when the promulgation ceremony performs authorship of a change society had already made, then decays to 0.18 as the statute settles into ordinary administration — a one-time ceremonial pulse, not cyclical intermittent reinforcement and not ratcheting proxy-maintenance. Accessibility_collapse (0.62) reflects that once codified, non-adoption collapses as a practical option for most purposes — courts, contracts, and registries all presume the standard — while dissent and variant practice persist at the margins, well short of the near-total collapse of a natural law. Resistance (0.18) is scattered noncompliance and petitioning, no organized movement. All three series share one time grid (T=0,5,10,15,20,25,30) so no metric is ever sampled against another's end-state value.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same statute. From the state's seat the arrangement is cheap ratification of achieved consensus — something it built for almost nothing and could amend at will. From the dissenter's seat the identical statute is binding law backed by fine and registry refusal, imposed without their consent. From the variant-holders' seat it is a mixed ledger — uniformity gained, standing lost. The engine derives these per-seat classifications from the structural data; the authored rope claim describes the aggregate structure and does not adjudicate the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Adopting communities, entrepreneurs, and the state are declared beneficiaries and derive low directionality — the arrangement subsidizes them (their practice becomes law; their cause wins; the state collects credit). Dissenters are declared victims with trapped exit and derive high directionality — the statute extracts compliance they never sold. Variant-holders are declared victims but sit nearer symmetric than a naive victim-plus-constrained-exit derivation would place them: they receive uniformity and dispute-resolution benefits that offset much of the loss of their variants. The override surface keys on power atom rather than agent, so an override for this one moderate agent would also re-price the adopting communities; the offsetting benefit is therefore left qualitative here rather than forced through a mis-keyed override. National scope mildly amplifies effective extraction on the trapped dissenter seat, which is the intended pricing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — inconsistent, illegible local practice — remains live, and the codification still performs its function in courts and registries, so no mandatrophy declaration is made. The classification guards against two mislabels: reading the low enforcement cost as absence of coordination function (it is the signature of a norm doing its own work), and reading the enactment-theater pulse as degraded performative maintenance (the series decays after T=10 rather than ratcheting, and the function it decorates is real). If later measurements showed theater rising while enforcement decayed toward zero and the statute survived only as ceremony, the mandatrophy question would open; the current trajectory does not support it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_attribution,
    'This constraint is one reading (endogenous_climb_reading) of the imposition_mechanism_kernel; what structurally changes if a sibling reading is adopted instead?',
    'Comparative sequence analysis across codification episodes: class-stratified adoption-curve dating against enactment dates, adjudicated in the historiographical literature.',
    'Adopting exogenous_override_reading raises epsilon sharply (mandate as coercion), shifts the computed type toward enforced extraction, and reverses the beneficiary/target map; adopting hybrid_legitimation_reading splits the mechanism, raising epsilon moderately and adding the state''s symbolic apparatus as a partial agenda-setting beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_attribution, conceptual, 'Reading-index ambiguity within the imposition-mechanism kernel: epsilon and type are properties of the reading, not the topic.').

omega_variable(
    mass_versus_elite_adoption,
    'Does pre-enactment acceptance reflect broad mass adoption, or an elite-and-official cascade that the state''s own personnel led — leaving the mandate to do real work on everyone else?',
    'Class-stratified adoption series from parish registers, court dockets, probate inventories, and market records, compared against the social composition of the drafting commissions.',
    'If acceptance was elite-confined, the statute performed genuine coercive conversion of the majority: epsilon and suppression rise, resistance rises, and the computed type drifts toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mass_versus_elite_adoption, empirical, 'Whether the popular acceptance preceding the mandate was mass or elite adoption.').

omega_variable(
    enforcement_shadow_versus_consent,
    'Is the thin enforcement establishment evidence of voluntary compliance, or of a credible-threat equilibrium in which enforcement is rarely needed because the sanction is believed?',
    'Natural experiments where the sanction lapsed or was amnestied: if noncompliance surges when the threat lifts, the observed calm was deterrence, not consent.',
    'If deterrence-driven, the authored suppression understates effective suppression and the coordinator framing covers a standing threat; effective extraction on the dissenter and variant-holder seats rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_shadow_versus_consent, empirical, 'Whether low observed enforcement reflects consent or an enforcement shadow.').

omega_variable(
    archival_survivorship_selection,
    'Does archival survivorship inflate the appearance of endogeneity — states codify what converged (records preserved, celebrated), while imposed norms that failed left thinner, differently shaped records?',
    'Compare the documentary footprint of codifications that succeeded against matched episodes attempted and abandoned, counting records per episode rather than reading surviving records at face value.',
    'Correcting for survivorship raises measured resistance and enforcement cost across the family and narrows the gap between this reading and its siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archival_survivorship_selection, empirical, 'Survivorship bias in the historical record as a confounder of the sequence evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_mechanism_kernel__endogenous_climb_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endo_climb_tr_t0, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(endo_climb_tr_t0, observed).
narrative_ontology:measurement(endo_climb_tr_t5, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement_basis(endo_climb_tr_t5, observed).
narrative_ontology:measurement(endo_climb_tr_t10, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(endo_climb_tr_t10, observed).
narrative_ontology:measurement(endo_climb_tr_t15, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(endo_climb_tr_t15, observed).
narrative_ontology:measurement(endo_climb_tr_t20, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(endo_climb_tr_t20, observed).
narrative_ontology:measurement(endo_climb_tr_t25, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 25, 0.2).
narrative_ontology:measurement_basis(endo_climb_tr_t25, observed).
narrative_ontology:measurement(endo_climb_tr_t30, imposition_mechanism_kernel__endogenous_climb_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(endo_climb_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(endo_climb_be_t0, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(endo_climb_be_t0, observed).
narrative_ontology:measurement(endo_climb_be_t5, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement_basis(endo_climb_be_t5, observed).
narrative_ontology:measurement(endo_climb_be_t10, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 10, 0.17).
narrative_ontology:measurement_basis(endo_climb_be_t10, observed).
narrative_ontology:measurement(endo_climb_be_t15, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 15, 0.21).
narrative_ontology:measurement_basis(endo_climb_be_t15, observed).
narrative_ontology:measurement(endo_climb_be_t20, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 20, 0.2).
narrative_ontology:measurement_basis(endo_climb_be_t20, observed).
narrative_ontology:measurement(endo_climb_be_t25, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 25, 0.19).
narrative_ontology:measurement_basis(endo_climb_be_t25, observed).
narrative_ontology:measurement(endo_climb_be_t30, imposition_mechanism_kernel__endogenous_climb_reading, base_extractiveness, 30, 0.18).
narrative_ontology:measurement_basis(endo_climb_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(endo_climb_su_t0, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(endo_climb_su_t0, observed).
narrative_ontology:measurement(endo_climb_su_t5, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement_basis(endo_climb_su_t5, observed).
narrative_ontology:measurement(endo_climb_su_t10, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 10, 0.16).
narrative_ontology:measurement_basis(endo_climb_su_t10, observed).
narrative_ontology:measurement(endo_climb_su_t15, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement_basis(endo_climb_su_t15, observed).
narrative_ontology:measurement(endo_climb_su_t20, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 20, 0.16).
narrative_ontology:measurement_basis(endo_climb_su_t20, observed).
narrative_ontology:measurement(endo_climb_su_t25, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 25, 0.14).
narrative_ontology:measurement_basis(endo_climb_su_t25, observed).
narrative_ontology:measurement(endo_climb_su_t30, imposition_mechanism_kernel__endogenous_climb_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(endo_climb_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_mechanism_kernel__endogenous_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__exogenous_override_reading).
narrative_ontology:affects_constraint(imposition_mechanism_kernel__endogenous_climb_reading, imposition_mechanism_kernel__hybrid_legitimation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'how new norms gained legitimacy' decomposes into three mechanism-attribution claims with distinct epsilon over the same referent (the codification episode). This member (endogenous_climb_reading) authors low epsilon; exogenous_override_reading authors high epsilon; hybrid_legitimation_reading authors intermediate. Each story links the others. The endogenous reading sits upstream in citation practice — its sequence findings are cited as supporting evidence by the hybrid reading and contested head-on by the exogenous reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
