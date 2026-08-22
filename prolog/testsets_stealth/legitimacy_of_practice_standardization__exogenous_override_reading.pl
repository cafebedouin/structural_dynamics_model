% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__exogenous_override_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__exogenous_override_reading
 *   human_readable: State-Decreed Practice Standardization (Exogenous Override Reading)
 *   domain: political_history/modernization_studies
 *
 * SUMMARY:
 *   Interwar modernizing states decreed wholesale changes in calendrical,
 *   sartorial, and public practice — Gregorian adoption and hat laws in
 *   Turkey (1925-26), dress-code campaigns in Iran, anti-veiling campaigns in
 *   Soviet Central Asia — citing modernization, fiscal stability, and
 *   international alignment. Each case shows the same structural signature:
 *   abrupt legal imposition, dedicated enforcement machinery, rapid surface
 *   compliance, and persistent underground practice; rural populations kept
 *   the lunar calendar for decades, and the 'double life' stabilized rather
 *   than faded. This file instantiates ONE reading of the
 *   legitimacy_of_practice_standardization kernel — the
 *   exogenous_override_reading, under which such decree-led change is
 *   legitimate — and authors the standing arrangement that reading is about:
 *   the decree-and-enforce apparatus and its double-life product. Sibling
 *   readings are separate constraints. The claim/metric gap is deliberate:
 *   the reading CLAIMS tangled_rope (real coordination, real extraction)
 *   while the metrics describe substantially extractive, actively enforced
 *   operation whose compliance is increasingly performative — the engine
 *   measures the divergence; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - centralizing_state_bureaucracy: agenda-setter (institutional/arbitrage) — decrees, enforces, collects
 *   - urban_modernist_elites: primary beneficiary (powerful/mobile) — reforms ratify their existing life
 *   - international_alignment_partners: secondary beneficiary (institutional/arbitrage) — schedulable counterpart, zero domestic cost
 *   - rural_practice_keeping_populations: primary target (powerless/trapped) — bears the double-life costs indefinitely
 *   - traditional_clerical_authorities: target with partial offset (organized/identity_locked) — loses public monopoly, converts pressure into moral authority
 *   - veiled_women_targeted_by_campaigns: target (powerless/identity_locked) — pincer between state penalty and community sanction
 *   - enforcement_inspectorate: administering agent (moderate/constrained) — career-dependent on citation counts
 *   - modernization_historians: analytical observer — sees the full structure including the compliance/practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62).
domain_priors:suppression_score(legitimacy_of_practice_standardization__exogenous_override_reading, 0.76).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__exogenous_override_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__exogenous_override_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__exogenous_override_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__exogenous_override_reading, "State-Decreed Practice Standardization (Exogenous Override Reading)").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__exogenous_override_reading, "political_history/modernization_studies").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__exogenous_override_reading, 'b8266611-23d4-471a-8f29-99d0daebf0e6').
narrative_ontology:cs_kernel_codification('b8266611-23d4-471a-8f29-99d0daebf0e6', formalized).
narrative_ontology:cs_authority_grounding('b8266611-23d4-471a-8f29-99d0daebf0e6', extraction).
narrative_ontology:cs_interpretation_layer_present('b8266611-23d4-471a-8f29-99d0daebf0e6').
narrative_ontology:cs_reading_relation('b8266611-23d4-471a-8f29-99d0daebf0e6', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('b8266611-23d4-471a-8f29-99d0daebf0e6', legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, influences).
narrative_ontology:cs_axiom('b8266611-23d4-471a-8f29-99d0daebf0e6', foundational, state_decree_confers_practice_legitimacy).
narrative_ontology:cs_axiom_status(state_decree_confers_practice_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b8266611-23d4-471a-8f29-99d0daebf0e6', state_decree_confers_practice_legitimacy, conventional).
narrative_ontology:cs_axiom('b8266611-23d4-471a-8f29-99d0daebf0e6', foundational, collective_benefit_overrides_practice_autonomy).
narrative_ontology:cs_axiom_status(collective_benefit_overrides_practice_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('b8266611-23d4-471a-8f29-99d0daebf0e6', collective_benefit_overrides_practice_autonomy, instrumental).
narrative_ontology:cs_reference_frame('b8266611-23d4-471a-8f29-99d0daebf0e6', state_as_sole_practice_legitimator).
narrative_ontology:cs_drift_state('b8266611-23d4-471a-8f29-99d0daebf0e6', post_double_life_ethnography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b8266611-23d4-471a-8f29-99d0daebf0e6', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__exogenous_override_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, centralizing_state_bureaucracy).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, urban_modernist_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_partners).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, rural_practice_keeping_populations).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_clerical_authorities).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__exogenous_override_reading, veiled_women_targeted_by_campaigns).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_clerical_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_inspectorate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and promulgates the decrees adopting the state calendar, dress code, and public practice standards; builds the inspectorates that enforce them; collects fines and reaps the fiscal-legibility and treaty-alignment gains. It can amend, suspend, or quietly stop enforcing any provision at will, so its exit from any particular rule is redefinition of the rule itself.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, centralizing_state_bureaucracy, agenda_setter,
    institutional, generational, arbitrage, national).

% Already live on the state calendar and in the mandated dress; the decrees ratify their existing life and open ministry, military, and professional careers conditional on conformity. Their adjustment cost is near zero and their mobility between cities, professions, and posts is high.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, urban_modernist_elites, beneficiary,
    powerful, biographical, mobile, national).

% Foreign governments, lenders, and trading counterparts gain a single schedulable counterpart: treaties, loan amortizations, tariff schedules, and shipping timetables keyed to one calendar. They bear none of the domestic adjustment cost and can redirect engagement to other states if alignment lapses.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, international_alignment_partners, beneficiary,
    institutional, civilizational, arbitrage, global).

% Keep the lunar calendar for ritual and agrarian timing while using the state calendar for taxes, courts, schools, and markets; maintain a second set of clothing for inspection-prone settings; pay fines when the two lives visibly collide. Tied to land, kin, and congregation, they cannot relocate, and the double bookkeeping passes intact to each generation.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, rural_practice_keeping_populations, payer,
    powerless, generational, trapped, regional).

% Lose the public monopoly on time reckoning and sartorial norms as the state calendar and dress code occupy ground the ritual establishment formerly governed; they continue setting ritual time privately, and state pressure converts into moral authority among their constituencies. Leaving the role would mean abandoning the vocation that constitutes them, so they absorb the loss inside it.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_clerical_authorities, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, traditional_clerical_authorities, beneficiary).

% Targeted by unveiling campaigns and street-level dress enforcement. Compliance exposes them to sanction from family and community; refusal invites state penalty — a pincer with no neutral option. The garment carries piety, honor, and kinship standing, so removing it is not a wardrobe decision but a renegotiation of identity conducted under inspection.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, veiled_women_targeted_by_campaigns, payer,
    powerless, biographical, identity_locked, regional).

% Salaried inspectors and municipal police who conduct dress checks, audit shops and offices for calendar conformity, and collect fines. Promotion depends on citation counts, giving each officer a career stake in finding violations; many privately regard parts of the code as arbitrary but cannot refuse assignments without losing position.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_inspectorate, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_practice_standardization__exogenous_override_reading, enforcement_inspectorate, beneficiary).

% Reconstruct the reform record from legislative archives, court dockets, fine ledgers, ethnographies, and oral histories; measure the gap between published compliance statistics and practiced life. Hold no stake in either the modernizing coalition's success narrative or the traditionalist grievance narrative.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__exogenous_override_reading, modernization_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_practice_standardization__exogenous_override_reading, centralizing_state_bureaucracy).
narrative_ontology:fixing_cost_class(legitimacy_of_practice_standardization__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifying time reckoning, public dress, and official practice under a single administrable standard lets a central fiscal state budget, tax, conscript, contract, and schedule internationally without translating among local calendars and customs; the standard is solved once, centrally, rather than negotiated per transaction.
% TRANSFER_FUNCTION: Moves compliance labor, fine revenue, and the costs of double-bookkeeping from rural and traditional populations to the center; moves symbolic recognition — whose time and whose dress count as official — from traditional authorities to the state bureaucracy.
% ABSENT_VOICES: Rural practice-keepers and clerical authorities were not seated in the councils that drafted the decrees; women targeted by unveiling campaigns had no representative channel at all. Their objections enter the record only as riot, petition, flight, or the quiet persistence the enforcement apparatus was built to prevent.
% DISAPPEARANCE_RATIONALE: If the decree-and-enforce apparatus vanished overnight, fiscal administration would fragment back into plural calendars, treaty and loan schedules would require renegotiation, the inspectorate's livelihoods would dissolve, and the public/private boundary that currently partitions official from practiced life would reorganize around whatever settlement the parties reached next.
% FOUNDING_PROBLEM: A fiscally illegible polity: multiple calendars made tax collection, conscription, and treaty scheduling error-prone, and elite opinion read sartorial and ritual plurality as evidence of backwardness blocking international credit and standing.
% FOUNDING_PROBLEM_CORROBORATION: Fiscal archives, treaty texts, and lender correspondence corroborate the legibility problem from outside the benefiting parties. No source outside the modernizing coalition attests that the resulting double-life equilibrium was an intended or acceptable endpoint; ethnographic and oral-history scholarship — produced independently of the state — documents the persistence the compliance statistics obscure.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__exogenous_override_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__exogenous_override_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_practice_standardization__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_practice_standardization__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62: the fiscal-legibility coordination is genuine (budgets, treaties, and courts do need one calendar), but the adjustment burden falls almost entirely on populations who did not consent and receive little of the gain. Suppression is 0.76 as a raw structural property — dedicated inspectorates, fine schedules, street-level dress enforcement, and criminalized refusal — and is deliberately NOT scaled by power or scope here; only extractiveness is scaled downstream. Theater is 0.58 and rising through the interval: surface compliance is this constraint's signature product, the performed conformity that lets enforcement statistics report success while practiced life continues unchanged. Accessibility_collapse is 0.38 — unusually low for an enforced construct, because the alternative (traditional practice) never collapses; it is expelled from public space and survives privately, which is precisely why enforcement must continue. Resistance is 0.58: open episodes (dress-law riots, regional revolts) plus the quieter, more durable resistance of persistence itself. Measurement series run on one shared grid (1925-1955, six points) so every tracked metric is authored at every examined time point; suppression_requirement is tracked because enforcement capacity is the story's dynamic — built up through the campaign era, peaking circa 1937, decaying thereafter.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat the arrangement is coordination it built and administers: the same decree that reads as liberation from fiscal chaos in the ministry reads as confiscation of time and dress in the village. From the trapped payer seats the identical structure operates as indefinite enforced extraction with identity-locked exits. From the arbitrage beneficiary seats (international partners) it is a free dividend. The engine derives these divergences from the structural data; the authored claim adjudicates nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the urban elite and international partner seats toward the subsidy end (low d): the mandated practice matches their existing life or costs them nothing domestically. The state bureaucracy sits at the agenda-setting pole with arbitrage exit — it defines rather than faces the rules. Victim declarations drive rural practice-keepers and targeted women toward the full-target end (high d), amplified by trapped and identity_locked exits: their inability to leave or to shed the fused identity places them near maximal exposure. The single override corrects the clerical seat: victim-classed with identity_locked exit, the derivation would place clerics near full target (~0.95), but they partially convert suppression into moral authority and retain private-domain control, so d is overridden to 0.78 for the organized power atom this seat occupies. The inspectorate's dual position (administers and collects salary, bears career dependence) nets to a moderately low d consistent with its agenda_setter/beneficiary roles.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. A pure-snare reading would erase the real coordination function — fiscal legibility and treaty schedulability were genuine collective-action problems that plural calendars genuinely caused, and the unified standard solved them. A pure-rope reading would erase the asymmetry: the same structure that coordinates the treasury extracts double-bookkeeping labor, fines, and identity cost from populations with no exit, indefinitely. Tangled rope holds both facts. On the genealogy axis, the founding problem (fiscal illegibility) is largely solved — corroborated by fiscal archives outside the benefiting parties — yet the arrangement persists with rising theater and undiminished double life: the founding_problem_status x disappearance_verdict mismatch flags exactly the zombie-risk this reading's own tradition will not acknowledge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    persistence_attribution_across_readings,
    'Is the decades-long double-life equilibrium a property of decree-imposed standardization specifically, or of standardization as such — would voluntary adoption show the same underground persistence?',
    'Comparative study of endogenous adoption cases (calendar and dress shifts adopted without decree) for equivalent persistence signatures; cross-reading comparison across the sibling constraint stories.',
    'If persistence appears only under decree, the extraction measured here is attributable to the override mechanism and this reading bears it; if it appears under voluntary adoption too, part of the burden belongs to standardization itself and the sibling readings inherit it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_attribution_across_readings, conceptual, 'Whether the double-life outcome distinguishes this reading from its siblings or is common to the whole kernel.').

omega_variable(
    decree_displacement_premise,
    'Does state decree actually displace practice, or merely relocate it from public to private space?',
    'Longitudinal ethnography tracking practice maintenance across three-plus generations after decree; comparison of private-practice prevalence at t0 versus tn of this interval.',
    'If decree relocates rather than displaces, the reading''s legitimacy claim rests on an empirically failing premise — the arrangement''s coordination cover weakens and its computed classification shifts toward extraction with coordination as pretext.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decree_displacement_premise, empirical, 'The empirical premise underneath the reading''s normative claim.').

omega_variable(
    collective_benefit_distribution,
    'Who is ''the collective'' whose benefit justifies the override — does the fiscal and international gain accrue broadly, or concentrate in administrative centers?',
    'Distributional analysis of reform benefits: tax-incidence studies, trade-gain allocation, administrative employment patterns by region and class.',
    'If benefits concentrate where costs do not fall, the ''collective benefit'' clause functions as cover, effective extraction rises for the paying seats, and the tangled-rope balance tilts toward its snare pole.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_distribution, empirical, 'Whether the justification clause tracks a real broad benefit or a concentrated one.').

omega_variable(
    enforcement_decay_meaning,
    'Does the post-1937 decay in suppression requirement reflect deliberate liberalization or simple attrition of enforcement capacity — and does the double life survive either way?',
    'Archival distinction between policy reversals (rescinded provisions, amnesties) and budgetary/personnel collapse in the inspectorate; oral histories on whether private practice contracted when enforcement eased.',
    'If capacity attrition, the arrangement persists by inertia with rising theater — a piton-drift signal inside a tangled-rope shell; if liberalization, the double life is a negotiated equilibrium and the extraction is bargained rather than imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_meaning, empirical, 'What the falling suppression trajectory actually represents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__exogenous_override_reading, 1925, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lops_exo_tr_t1925, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1925, 0.42).
narrative_ontology:measurement(lops_exo_tr_t1931, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1931, 0.5).
narrative_ontology:measurement(lops_exo_tr_t1937, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1937, 0.55).
narrative_ontology:measurement(lops_exo_tr_t1943, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1943, 0.57).
narrative_ontology:measurement(lops_exo_tr_t1949, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1949, 0.58).
narrative_ontology:measurement(lops_exo_tr_t1955, legitimacy_of_practice_standardization__exogenous_override_reading, theater_ratio, 1955, 0.58).

% Extraction over time
narrative_ontology:measurement(lops_exo_be_t1925, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1925, 0.7).
narrative_ontology:measurement(lops_exo_be_t1931, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1931, 0.68).
narrative_ontology:measurement(lops_exo_be_t1937, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1937, 0.65).
narrative_ontology:measurement(lops_exo_be_t1943, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1943, 0.64).
narrative_ontology:measurement(lops_exo_be_t1949, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1949, 0.63).
narrative_ontology:measurement(lops_exo_be_t1955, legitimacy_of_practice_standardization__exogenous_override_reading, base_extractiveness, 1955, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(lops_exo_su_t1925, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1925, 0.58).
narrative_ontology:measurement(lops_exo_su_t1931, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1931, 0.74).
narrative_ontology:measurement(lops_exo_su_t1937, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1937, 0.78).
narrative_ontology:measurement(lops_exo_su_t1943, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1943, 0.73).
narrative_ontology:measurement(lops_exo_su_t1949, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1949, 0.66).
narrative_ontology:measurement(lops_exo_su_t1955, legitimacy_of_practice_standardization__exogenous_override_reading, suppression_requirement, 1955, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_practice_standardization__exogenous_override_reading, information_standard).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, endogenous_displacement_reading).
narrative_ontology:affects_constraint(legitimacy_of_practice_standardization__exogenous_override_reading, dual_practice_equilibrium_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'practice standardization legitimacy' decomposes into three structurally distinct constraints sharing one kernel. This file instantiates ONLY the exogenous_override_reading: the decree-and-enforce arrangement and its double-life outcome, with a single stable epsilon assessed over that standing arrangement. The endogenous_displacement_reading (voluntary utility-driven adoption) and the dual_practice_equilibrium_reading (domain-partitioned legitimacy) are separate stories with their own beneficiaries, victims, and epsilon values; they are linked here via network edges, not folded into this classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimacy_of_practice_standardization__exogenous_override_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
