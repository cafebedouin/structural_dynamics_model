% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__parliamentary_sovereignty_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__parliamentary_sovereignty_reading
 *   human_readable: Parliamentary Sovereignty Reading — Legislative Final Interpretive Authority over the Basic Law
 *   domain: constitutional/political/institutional_design
 *
 * SUMMARY:
 *   In Westminster-lineage and comparable parliamentary systems, the standing
 *   arrangement under contest is that the elected legislature's reading of
 *   the basic law is final: courts interpret provisionally, but an act of
 *   Parliament or an express override clause displaces any judicial
 *   construction. The arrangement solves a real coordination problem — a
 *   polity needs one operative constitutional answer between elections —
 *   while the same structure binds rights minorities to interpretations they
 *   cannot veto and cannot exit, and keeps the judiciary's doctrine
 *   provisional at the pleasure of the governing majority. This file authors
 *   ONLY the parliamentary-sovereignty reading of the shared kernel; the
 *   judicial-supremacy and popular-constitutionalism readings are separate
 *   constraint stories linked through network.affects_constraints. Claim and
 *   metrics are authored independently: the claimed type (tangled_rope)
 *   states what I believe structurally true — genuine coordination plus
 *   asymmetric extraction held by active enforcement — while the metrics
 *   describe the arrangement's observed operation over 1965-2025.
 *
 * KEY AGENTS:
 *   - elected_legislature: Primary beneficiary and agenda-setter (institutional/arbitrage) — holds final interpretive authority, renews it electorally, and can rewrite the allocation rules themselves
 *   - governing_majority_voters: Secondary beneficiary (organized/mobile) — collect the policy content of the majority's readings
 *   - opposition_parliamentarians: Same-level payer/beneficiary (powerful/mobile) — bear exclusion from the pen today while competing to hold it tomorrow
 *   - rights_minorities: Primary target (moderate/trapped) — bound by adverse readings with no exit from citizenship's binding force
 *   - independent_judiciary: Secondary target and day-to-day administrator (institutional/constrained) — interprets provisionally and bears override, jurisdiction-limiting, and appointment pressure
 *   - civil_rights_advocates: Excluded (organized/constrained) — no formal seat in the interpretive hierarchy they litigate against
 *   - constitutional_scholars: Analytical observer (analytical/analytical) — maps the contest, collects nothing, bears nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52).
domain_priors:suppression_score(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.58).
domain_priors:theater_ratio(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__parliamentary_sovereignty_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "Parliamentary Sovereignty Reading — Legislative Final Interpretive Authority over the Basic Law").
narrative_ontology:topic_domain(basic_law_interpretive_authority__parliamentary_sovereignty_reading, "constitutional/political/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '50486bee-72ad-4b46-b6bf-a526a067ad50').
narrative_ontology:cs_kernel_codification('50486bee-72ad-4b46-b6bf-a526a067ad50', fixed_text).
narrative_ontology:cs_authority_grounding('50486bee-72ad-4b46-b6bf-a526a067ad50', practice).
narrative_ontology:cs_interpretation_layer_present('50486bee-72ad-4b46-b6bf-a526a067ad50').
narrative_ontology:cs_reading_relation('50486bee-72ad-4b46-b6bf-a526a067ad50', basic_law_interpretive_authority__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('50486bee-72ad-4b46-b6bf-a526a067ad50', basic_law_interpretive_authority__popular_constitutionalism_reading, forecloses).
narrative_ontology:cs_axiom('50486bee-72ad-4b46-b6bf-a526a067ad50', foundational, representative_accountability_grounds_finality).
narrative_ontology:cs_axiom_status(representative_accountability_grounds_finality, holdable).
narrative_ontology:cs_axiom_grounding('50486bee-72ad-4b46-b6bf-a526a067ad50', representative_accountability_grounds_finality, deontological).
narrative_ontology:cs_axiom('50486bee-72ad-4b46-b6bf-a526a067ad50', secondary, no_unelected_interpretive_veto).
narrative_ontology:cs_axiom_status(no_unelected_interpretive_veto, holdable).
narrative_ontology:cs_axiom_grounding('50486bee-72ad-4b46-b6bf-a526a067ad50', no_unelected_interpretive_veto, deontological).
narrative_ontology:cs_reference_frame('50486bee-72ad-4b46-b6bf-a526a067ad50', electoral_mandate_finality).
narrative_ontology:cs_drift_state('50486bee-72ad-4b46-b6bf-a526a067ad50', contemporary_rights_review_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50486bee-72ad-4b46-b6bf-a526a067ad50', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_voters).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, independent_judiciary).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parliamentarians).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parliamentarians).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, representative_accountability_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_authority__parliamentary_sovereignty_reading, democratic_mandate_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts, amends, and where necessary expressly overrides judicial readings of the basic law; renews its interpretive finality through elections; and can rewrite the very rules that allocate interpretive authority, so no particular constraint on it tends to survive a determined parliamentary majority for long.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature, beneficiary).

% Collect the policy content of the majority's constitutional readings — the statutes, overrides, and entrenchments they voted for. They can switch allegiance between elections and bear little of the override cost unless they become the outvoted side on some future question.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, governing_majority_voters, beneficiary,
    organized, immediate, mobile, national).

% Vote against and publicly denounce adverse readings while out of office, but the finality structure is the prize they are competing to hold: their objection is to who wields the pen, not to the pen existing. Their exit is rotation into government, not departure from the arrangement.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parliamentarians, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, opposition_parliamentarians, beneficiary).

% Depend on constitutional readings for protection of the interests that make them vulnerable. When the legislature overrides a protective ruling or entrenches an adverse meaning, their recourse is the next election — in which they remain outnumbered on precisely the dimensions that matter to them. Citizenship gives them no exit from the binding force of what is enacted.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, rights_minorities, payer,
    moderate, generational, trapped, national).

% Develops constitutional doctrine case by case and sets much of the practical interpretive agenda, but its readings stand only until the legislature displaces them. Override invocations, jurisdiction-limiting bills, and appointment politics are the recurring costs of the seat; resignation or accommodation are the only exits.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, independent_judiciary, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__parliamentary_sovereignty_reading, independent_judiciary, agenda_setter).

% Litigate, campaign, and document the harms of override and entrenchment, but hold no formal seat in the interpretive hierarchy they contest; their access runs entirely through the institutions whose finality they dispute.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, civil_rights_advocates, excluded,
    organized, generational, constrained, national).

% Map the allocation-of-final-authority debate comparatively across systems and generations; collect no rents from the arrangement and bear none of its binding force; produce the analyses the other seats cite against one another.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__parliamentary_sovereignty_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__parliamentary_sovereignty_reading, elected_legislature).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles, through an accountable elected body, the otherwise-unending question of what the basic law requires — giving ministries, courts, and citizens a single operative constitutional answer between elections and keeping governance continuous during interpretive conflict.
% TRANSFER_FUNCTION: Moves final interpretive authority — and with it the policy content of constitutional meaning — from courts and extra-institutional contestation to the elected legislature; correspondingly moves the incidence of adverse readings onto rights minorities and onto the judiciary's institutional standing.
% ABSENT_VOICES: Rights minorities bound by adverse readings, future generations affected by entrenched interpretations, and civil-rights advocates hold no formal seat; judges reduced to provisional voices speak only to be overridden. Present, they would object that finality without their consent converts democratic self-rule into rule by the temporarily numerous.
% DISAPPEARANCE_RATIONALE: If legislative finality vanished overnight, constitutional politics would immediately reorganize around whoever absorbed the vacancy — courts asserting supremacy or popular movements claiming constituent power — and every branch's strategy, along with every minority's litigation posture, would recalibrate within a political cycle.
% FOUNDING_PROBLEM: After monarchical and colonial rule, the design problem was where ultimate legal authority should rest in a democracy: someone must be able to settle what the basic law means, yet settling it unelected reproduces the old subordination. This arrangement answers: the elected chamber, accountable at the ballot box.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: comparative constitutional scholarship treats the allocation of final interpretive authority as an unsolved design problem across systems; judicial opinions in rights-dialogue systems reason explicitly about the legitimacy of legislative override; and the continued vitality of the sibling readings is itself external attestation that the founding question remains open. No attestation comes only from the legislature.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__parliamentary_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__parliamentary_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (epsilon = 0.52 at interval end) is moderate: the arrangement's binding force falls asymmetrically on minorities and the judiciary, but minorities remain voting members of the demos and overrides are episodic rather than constant, so the extraction is real yet bounded. Suppression (0.58) reflects the enforcement machinery — override clauses, jurisdiction limits, appointment politics — needed to hold finality against judicial counter-claims; it is a raw structural property, unscaled, distinct from extractiveness, which the engine scales by directionality and scope. Theater (0.28) is low-moderate: elections genuinely constrain, but the accountability linkage attenuates as party discipline thickens. Accessibility collapse (0.42) is low for a human construct: the sibling readings remain live alternatives and several systems have partially migrated toward them. Resistance (0.60) is substantial — courts test limits, minorities litigate, advocates contest. All three tracked series share one time grid (decade points 1965-2025): base_extractiveness rises as rights instruments made legislative finality bite harder on identifiable groups; suppression_requirement jumps in the charter era when override machinery was codified, then plateaus; theater_ratio climbs slowly as the accountability mechanism weakens.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently despite identical nominal membership in one constitutional order. From the legislature's seat the arrangement is the democratic mandate functioning: it wrote the rules and can rewrite them, and arbitrage-grade exit makes almost nothing look extractive. From the judiciary's seat the same structure is provisional authority plus recurring override risk. From the minorities' seat it is binding without voice — the vote they cast is always outnumbered on the dimensions that matter to them. Opposition parliamentarians occupy the sharpest divergence: they denounce the pen while treating possession of it as the prize, so their measured grievance concerns incumbency rather than structure. The engine computes these per-seat types from role, power, and exit data; the divergence is the finding, not noise.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation toward the subsidy pole: elected_legislature (agenda-setter with arbitrage exit) sits nearest the beneficiary end — the constraint subsidizes its institutional authority — and governing_majority_voters collect policy wins with mobile exit. Victim declarations drive the opposite pole: rights_minorities (trapped; citizenship is not exitable) and independent_judiciary (constrained; resignation or compliance are the only exits) sit near the full-target end, so effective extraction amplifies for them and damps toward subsidy for the legislature. Opposition parliamentarians derive a mid-range directionality: the payer role pulls up, mobile exit and their beneficiary secondary-role pull down. No directionality overrides were needed — the beneficiary/victim plus exit data produce the correct relationships directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification keeps both faces visible. Reading the arrangement as pure rope would erase the minorities and judiciary who pay through the same structure that coordinates; reading it as pure snare would erase the genuine terminal-decision function that no polity dispenses with. Mandatrophy status: the founding problem — where ultimate legal authority rests in a democracy — is live, so no resolved-mandatrophy flag is declared. The drift watch is theater_ratio: if the accountability mechanism becomes fully theatrical while override machinery persists, the arrangement's justification hollows and the trajectory bends toward snare (should a concentrated capturer harden around the extraction) or piton (should enforcement persist without anyone meaningfully profiting). The measured series show mild drift in that direction, not arrival.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the parliamentary-sovereignty reading of the basic-law-interpretive-authority kernel — would the sibling readings (judicial supremacy, popular constitutionalism) restructure the beneficiary and victim sets so thoroughly that no cross-reading epsilon comparison is meaningful?',
    'Author each reading as its own epsilon-invariant story (as done here) and compare classifications across files rather than averaging within one.',
    'Merging readings would average incompatible beneficiary/victim structures and fabricate a middle epsilon belonging to no actual arrangement; keeping them separate preserves the divergence as the measurable signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification over a shared kernel; cross-reading comparison happens between files, never within one.').

omega_variable(
    accountability_congruence_ambiguity,
    'Is the representative accountability that legitimates final authority empirically real — do legislators'' constitutional votes track constituent preferences — or substantially mediated by party discipline, whips, and safe seats?',
    'Congruence studies linking district opinion to roll-call behavior on constitutional questions; natural experiments where party leadership and district preferences diverge.',
    'If accountability is largely theatrical, the democratic-mandate justification thins, theater_ratio understates performance, and the arrangement drifts toward snare-like extraction justified by a fiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_congruence_ambiguity, empirical, 'Whether the legitimating accountability mechanism is substantively operative or performative.').

omega_variable(
    override_target_stability,
    'Are the targets of legislative override a rotating set (ordinary losers in fair competition) or persistent groups (minorities who predictably lose on the dimensions that matter to them)?',
    'Longitudinal coding of override invocations and adverse entrenchments by affected group, with persistence analysis across decades.',
    'Persistent targets raise rights_minorities'' effective directionality toward the full-target end and support a snare-drift reading; rotating targets support the tangled-rope reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(override_target_stability, empirical, 'Stability of the population that bears the override burden.').

omega_variable(
    override_chill_vs_invocation,
    'Does the override mechanism operate mainly through rare invocation, or through the standing threat that chills judicial doctrine-making in advance?',
    'Compare doctrinal trajectories before and after override powers were codified, controlling for bench-composition changes; interview-based studies of judicial self-restraint.',
    'If chill dominates, suppression is higher than invocation counts suggest and the judiciary''s effective directionality sits nearer the full-target end than its formal override rate implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(override_chill_vs_invocation, empirical, 'Latent versus exercised force of the override mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__parliamentary_sovereignty_reading, 1965, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(blia_parl_sov_tr_t1965, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement_basis(blia_parl_sov_tr_t1965, observed).
narrative_ontology:measurement(blia_parl_sov_tr_t1975, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1975, 0.17).
narrative_ontology:measurement_basis(blia_parl_sov_tr_t1975, observed).
narrative_ontology:measurement(blia_parl_sov_tr_t1985, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement_basis(blia_parl_sov_tr_t1985, observed).
narrative_ontology:measurement(blia_parl_sov_tr_t1995, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 1995, 0.22).
narrative_ontology:measurement_basis(blia_parl_sov_tr_t1995, observed).
narrative_ontology:measurement(blia_parl_sov_tr_t2005, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 2005, 0.24).
narrative_ontology:measurement_basis(blia_parl_sov_tr_t2005, observed).
narrative_ontology:measurement(blia_parl_sov_tr_t2015, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 2015, 0.26).
narrative_ontology:measurement_basis(blia_parl_sov_tr_t2015, observed).
narrative_ontology:measurement(blia_parl_sov_tr_t2025, basic_law_interpretive_authority__parliamentary_sovereignty_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(blia_parl_sov_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(blia_parl_sov_be_t1965, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement_basis(blia_parl_sov_be_t1965, observed).
narrative_ontology:measurement(blia_parl_sov_be_t1975, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1975, 0.4).
narrative_ontology:measurement_basis(blia_parl_sov_be_t1975, observed).
narrative_ontology:measurement(blia_parl_sov_be_t1985, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1985, 0.44).
narrative_ontology:measurement_basis(blia_parl_sov_be_t1985, observed).
narrative_ontology:measurement(blia_parl_sov_be_t1995, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 1995, 0.47).
narrative_ontology:measurement_basis(blia_parl_sov_be_t1995, observed).
narrative_ontology:measurement(blia_parl_sov_be_t2005, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 2005, 0.49).
narrative_ontology:measurement_basis(blia_parl_sov_be_t2005, observed).
narrative_ontology:measurement(blia_parl_sov_be_t2015, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement_basis(blia_parl_sov_be_t2015, observed).
narrative_ontology:measurement(blia_parl_sov_be_t2025, basic_law_interpretive_authority__parliamentary_sovereignty_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(blia_parl_sov_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(blia_parl_sov_su_t1965, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement_basis(blia_parl_sov_su_t1965, observed).
narrative_ontology:measurement(blia_parl_sov_su_t1975, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement_basis(blia_parl_sov_su_t1975, observed).
narrative_ontology:measurement(blia_parl_sov_su_t1985, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1985, 0.5).
narrative_ontology:measurement_basis(blia_parl_sov_su_t1985, observed).
narrative_ontology:measurement(blia_parl_sov_su_t1995, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement_basis(blia_parl_sov_su_t1995, observed).
narrative_ontology:measurement(blia_parl_sov_su_t2005, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 2005, 0.57).
narrative_ontology:measurement_basis(blia_parl_sov_su_t2005, observed).
narrative_ontology:measurement(blia_parl_sov_su_t2015, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 2015, 0.56).
narrative_ontology:measurement_basis(blia_parl_sov_su_t2015, observed).
narrative_ontology:measurement(blia_parl_sov_su_t2025, basic_law_interpretive_authority__parliamentary_sovereignty_reading, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(blia_parl_sov_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__parliamentary_sovereignty_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who interprets the constitution' decomposes, per the epsilon-invariance principle, into three structurally distinct arrangements — legislative finality, judicial finality, and no-terminal-adjudication — each with its own epsilon, beneficiary/victim sets, and classification. This file authors only the parliamentary-sovereignty reading; the siblings are separate stories linked here as a constraint family. The three compete as peer readings of one kernel rather than as upstream/downstream dependencies; each file's network edge records the family membership the decomposition rule requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
