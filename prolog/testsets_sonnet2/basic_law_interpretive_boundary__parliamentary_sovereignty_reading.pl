% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_boundary__parliamentary_sovereignty_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: basic_law_interpretive_boundary__parliamentary_sovereignty_reading
 *   human_readable: Knesset Sovereignty over Basic Law Interpretation and Amendment
 *   domain: constitutional_law/comparative_constitutionalism
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-sovereignty reading of the
 *   contested basic_law_interpretive_boundary kernel: the Knesset, as
 *   directly elected sovereign, holds final interpretive and amending
 *   authority over Basic Laws by simple majority, and may override or
 *   legislate around adverse judicial rulings, including
 *   reasonableness-doctrine review. This is one of three declared readings
 *   (the others — judicial_supremacy_reading and
 *   balanced_contestation_reading — are separate constraint stories with
 *   their own ε and stakeholder structures, per the ε-invariance
 *   decomposition rule). Under THIS reading's own lights, extraction is
 *   comparatively low for ordinary majoritarian policy: the coordination
 *   problem of final-authority ambiguity is genuinely resolved by locating
 *   the tiebreaker in the electorally accountable body. Extraction and
 *   suppression rise, however, where the majority uses its unconstrained
 *   position against minorities who previously depended on judicial review as
 *   their only durable remedy — this is where the reading's own metrics
 *   register cost, assessed by its own lights, not by the judicial-supremacy
 *   reading's lights.
 *
 * KEY AGENTS:
 *   - knesset_majority_coalition: agenda_setter/beneficiary (institutional/arbitrage) — sets and can rewrite the interpretive boundary itself
 *   - electoral_majority_voters: beneficiary (organized/mobile) — benefits from unconstrained translation of votes into policy
 *   - parliamentary_minority_blocs: payer (organized/constrained) — loses judicial backstop, retains only electoral recourse
 *   - unrepresented_arab_citizen_minority: payer (powerless/trapped) — most exposed, cannot exit the state, historically minority-favoring rulings become removable by simple majority
 *   - civil_society_petitioners: excluded (moderate/constrained) — formal petition right survives, binding remedy does not
 *   - supreme_court_justices: excluded (institutional/analytical) — repositioned from adjudicator to advisor under this reading
 *   - comparative_constitutional_scholars: observer (analytical/analytical) — cross-system comparison, no direct stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.31).
domain_priors:suppression_score(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.42).
domain_priors:theater_ratio(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, rope).
narrative_ontology:human_readable(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "Knesset Sovereignty over Basic Law Interpretation and Amendment").
narrative_ontology:topic_domain(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, "constitutional_law/comparative_constitutionalism").

domain_priors:requires_active_enforcement(basic_law_interpretive_boundary__parliamentary_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, '86c56b35-c8a5-4302-a034-d34c09438533').
narrative_ontology:cs_kernel_codification('86c56b35-c8a5-4302-a034-d34c09438533', distributed).
narrative_ontology:cs_authority_grounding('86c56b35-c8a5-4302-a034-d34c09438533', practice).
narrative_ontology:cs_interpretation_layer_present('86c56b35-c8a5-4302-a034-d34c09438533').
narrative_ontology:cs_reading_relation('86c56b35-c8a5-4302-a034-d34c09438533', basic_law_interpretive_boundary__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('86c56b35-c8a5-4302-a034-d34c09438533', basic_law_interpretive_boundary__balanced_contestation_reading, influences).
narrative_ontology:cs_axiom('86c56b35-c8a5-4302-a034-d34c09438533', foundational, electoral_accountability_confers_final_interpretive_authority).
narrative_ontology:cs_axiom_status(electoral_accountability_confers_final_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('86c56b35-c8a5-4302-a034-d34c09438533', electoral_accountability_confers_final_interpretive_authority, conventional).
narrative_ontology:cs_axiom('86c56b35-c8a5-4302-a034-d34c09438533', foundational, unelected_judicial_review_lacks_democratic_legitimacy_to_bind_simple_majority).
narrative_ontology:cs_axiom_status(unelected_judicial_review_lacks_democratic_legitimacy_to_bind_simple_majority, holdable).
narrative_ontology:cs_axiom_grounding('86c56b35-c8a5-4302-a034-d34c09438533', unelected_judicial_review_lacks_democratic_legitimacy_to_bind_simple_majority, deontological).
narrative_ontology:cs_axiom('86c56b35-c8a5-4302-a034-d34c09438533', secondary, override_power_preserves_political_recourse_as_sufficient_remedy).
narrative_ontology:cs_axiom_status(override_power_preserves_political_recourse_as_sufficient_remedy, holdable).
narrative_ontology:cs_axiom_grounding('86c56b35-c8a5-4302-a034-d34c09438533', override_power_preserves_political_recourse_as_sufficient_remedy, instrumental).
narrative_ontology:cs_reference_frame('86c56b35-c8a5-4302-a034-d34c09438533', pre_1992_undefined_final_authority_equilibrium).
narrative_ontology:cs_drift_state('86c56b35-c8a5-4302-a034-d34c09438533', post_2023_judicial_reform_contest, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('86c56b35-c8a5-4302-a034-d34c09438533', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, basic_law_interpretive_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electoral_majority_voters).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_minority_blocs).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, unrepresented_arab_citizen_minority).
narrative_ontology:constraint_victim(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_petitioners).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, majoritarian_democratic_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds a simple-majority governing coalition that can pass, amend, or entrench Basic Laws, and — under this reading — can override or bypass judicial invalidation of ordinary legislation and, where it chooses, of Basicraws themselves. Exit from this arrangement is not needed because the coalition IS the arrangement; it can rewrite the boundary that would otherwise constrain it.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition, beneficiary).

% Cast votes that translate directly into legislative power without an unelected court able to permanently veto the resulting policy program. Their preferences, once they command a majority, face no counter-majoritarian check under this reading — they can vote out the coalition if they dislike outcomes, but cannot be blocked from getting what a majority wants in the interim.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, electoral_majority_voters, beneficiary,
    organized, biographical, mobile, national).

% Opposition parties who previously relied on judicial review as a check against majoritarian legislation affecting their constituents. Under this reading, their only recourse is winning future elections or negotiating within the coalition system; the courts offer no durable backstop. They remain inside the political system (no geographic or civic exit) but their structural leverage against majority action is removed.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, parliamentary_minority_blocs, payer,
    organized, biographical, constrained, national).

% A demographic and political minority historically under-represented in ruling coalitions, whose rights protections (land, family unification, religious status matters, electoral participation) have at times depended on judicial review of majoritarian legislation. Under unconstrained parliamentary sovereignty, protections that survive only via court intervention become removable by ordinary majority vote; this population cannot exit the state and has limited coalition leverage.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, unrepresented_arab_citizen_minority, payer,
    powerless, generational, trapped, national).

% NGOs, individual litigants, and advocacy groups who historically petitioned the Supreme Court (via standing rules and reasonableness review) to challenge legislative or executive action. Under this reading their petitions become advisory at most; the Knesset can proceed regardless of the Court's judgment. They retain the formal right to petition but the remedy is structurally hollowed out.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, civil_society_petitioners, excluded,
    moderate, biographical, constrained, national).

% Under this reading the Court retains a voice — it may still rule — but its rulings on Basic Law compatibility or reasonableness do not bind the Knesset, which may override, reenact, or amend around any adverse decision by simple majority. The Court is repositioned from adjudicator to advisor within the arrangement this reading describes.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, supreme_court_justices, excluded,
    institutional, generational, analytical, national).

% Study the Israeli case as a live instance of the parliamentary-sovereignty-vs-judicial-supremacy debate found in the UK, Canada notwithstanding clause, and other uncodified or semi-codified systems. They compare institutional designs but hold no stake in the outcome themselves.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, knesset_majority_coalition).
narrative_ontology:fixing_cost_class(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the question of final interpretive authority over Basic Laws by locating it in a single, electorally accountable body, avoiding the coordination failure of two co-equal authorities (Knesset and Court) each claiming final say with no tiebreaker — a genuine problem in any system lacking a codified, judicially enforceable constitution.
% TRANSFER_FUNCTION: Moves final decision-making power over the content and durability of rights protections from the judiciary (previously willing to invalidate majoritarian legislation under limited Basic Law review and reasonableness doctrine) to the sitting legislative majority, and correspondingly moves the practical remedy available to minorities and civil-society litigants from binding judicial relief to political advocacy within the majoritarian process.
% ABSENT_VOICES: The Arab citizen minority and civil society petitioners who relied on judicial review as their primary durable check would object most strongly; they are structurally present in the polity but underrepresented in ruling coalitions, and their formal right to petition the Court is preserved on paper while its binding force is removed under this reading. Comparative scholars from judicial-supremacy systems would also object procedurally but hold no direct stake.
% DISAPPEARANCE_RATIONALE: If unconstrained Knesset interpretive sovereignty were replaced overnight by binding judicial supremacy, previously overridable Basic Law amendments would become subject to substantive judicial invalidation, coalition legislative strategy would need to build in constitutional-compliance review at every step, and minority-protective litigation would regain a durable remedy — a substantial reorganization of how legislative power is actually exercised, not a cosmetic change.
% FOUNDING_PROBLEM: Israel's Basic Laws were enacted piecemeal without a single ratified constitution, leaving unresolved which institution — the elected Knesset or the appointed Supreme Court — holds final interpretive authority when a Basic Law's meaning or a law's compatibility with it is contested. This reading was built to resolve that gap by locating final authority in the body directly accountable to voters.
% FOUNDING_PROBLEM_CORROBORATION: Knesset majority coalitions and their legal advisors attest the problem (undefined final authority) remains live and requires legislative resolution via override mechanisms. Independent of the benefiting coalition, a substantial body of Israeli constitutional scholars, sitting and retired Supreme Court justices, and international comparative-law commentators attest that the 'gap' framing is itself contested — they hold that limited judicial review of Basic Laws had already functionally settled the question in favor of a bounded, reviewable sovereignty, and that this reading represents a reopening of a settled arrangement for majoritarian advantage rather than resolution of a genuine unresolved founding problem.
narrative_ontology:disappearance_verdict(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.31, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).
:- end_tests(basic_law_interpretive_boundary__parliamentary_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.31 at interval end) because, under this reading, the arrangement genuinely resolves a real institutional coordination failure (undefined final authority) for the bulk of ordinary majoritarian policy — the ε referent is the standing arrangement (unconstrained Knesset interpretive sovereignty) as this reading itself sees it, not the judicial-supremacy alternative it forecloses. Suppression is authored higher and rising (0.18 to 0.42) because maintaining the override power against a resistant judiciary and civil society requires active political and sometimes legislative maintenance (override clauses, reasonableness-standard amendments) — this is a raw structural property, not scaled by directionality. Accessibility collapse is moderate (0.35): political recourse (elections, coalition negotiation) remains genuinely available, distinguishing this from a mountain-grade collapse. Resistance is comparatively high (0.58) reflecting sustained mass protest, judicial pushback, and civil-society mobilization opposing the override framework as it has been operationalized — this reading does not deny that resistance exists; it asserts the resistance does not defeat the sovereignty claim's legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   From the knesset_majority_coalition seat, this constraint should compute close to rope/tangled_rope-adjacent — genuine coordination (settling final authority) with the extraction concentrated on identifiable payer seats. From the unrepresented_arab_citizen_minority and civil_society_petitioners seats, the same structural data should compute closer to tangled_rope or snare-adjacent, since their situation descriptions show high dependence, low exit, and removal of a previously available durable remedy. The engine computes these seat-level divergences from the authored power/exit/scope data; this story does not pre-resolve which seat's computed type is 'correct' — that is exactly the divergence the corpus exists to register.
 *
 * DIRECTIONALITY LOGIC:
 *   knesset_majority_coalition and electoral_majority_voters are declared beneficiaries: they collect the practical value of unconstrained majoritarian policy-making, so their derived directionality sits near the beneficiary end. parliamentary_minority_blocs, unrepresented_arab_citizen_minority, and civil_society_petitioners are declared victims/payers: they bear the cost of a removed judicial backstop, with directionality pushed toward the target end — most severely for the powerless, trapped minority, least severely for the organized, constrained minority blocs who retain some coalition leverage. No directionality override is used: the derivation chain (beneficiary/victim declarations + power + exit) already captures the asymmetry between an organized opposition bloc that can still contest power electorally and a powerless minority that cannot.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview captures the mandatrophy tension directly: this reading holds the founding problem (undefined final interpretive authority) is live and requires resolution, while outside corroborators (scholars, justices, comparative commentators) hold the problem was already functionally settled by limited judicial review and that this reading reopens a settled arrangement for majoritarian advantage. The classification does not resolve this dispute — it registers founding_problem_status as contested with corroboration drawn from outside the benefiting coalition, which is the structurally honest way to represent a live constitutional argument without smuggling in a verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the correct reading of the basic_law_interpretive_boundary kernel the parliamentary_sovereignty_reading (this story), the judicial_supremacy_reading, or the balanced_contestation_reading — and is that a matter Israeli constitutional practice has actually settled, or does it remain genuinely open?',
    'Track whether a stable, cross-coalition institutional equilibrium emerges (e.g., a durable override-clause supermajority threshold accepted by successive governments of different political composition) versus continued oscillation between readings tracking which bloc holds power. Comparative analysis against UK parliamentary sovereignty and Canadian notwithstanding-clause equilibria as reference cases.',
    'If the sovereignty reading becomes the durable cross-partisan equilibrium, this story''s classification is stable and near-Rope for majoritarian policy. If a future court or constitutional convention reasserts binding judicial review, the arrangement this story describes ceases to exist as authored and the judicial_supremacy_reading becomes the operative constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading is or will become the operative one is unresolved and is precisely the contest the three sibling stories exist to represent separately.').

omega_variable(
    minority_protection_erosion_trajectory,
    'Does removing judicial review as a durable backstop for minority protections (e.g., for the Arab citizen minority) produce actual legislative erosion of those protections, or does electoral and international pressure substitute as an effective check?',
    'Track legislative outcomes affecting minority-protective Basic Law provisions over the interval following override-power consolidation; compare rate and severity of minority-affecting legislation pre- and post- the reading''s operationalization.',
    'If erosion materializes, the victim-side extractiveness for unrepresented_arab_citizen_minority is understated at 0.31 and should be authored higher in a subsequent story revision; if substitute checks prove effective, the moderate ε is vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minority_protection_erosion_trajectory, empirical, 'Whether removing the judicial backstop causes measurable harm to minority protections or is offset by other checks.').

omega_variable(
    international_treaty_carveout_stability,
    'Does the near-zero extraction for international treaty obligations (per the expected structural delta) hold stably, or does Knesset sovereignty eventually extend to override treaty-derived commitments as well?',
    'Monitor whether override legislation is ever extended to conflict with binding international obligations (e.g., trade agreements, human rights conventions Israel has ratified) versus remaining confined to domestic Basic Law disputes.',
    'If the carve-out erodes, the reading''s claimed boundary (unconstrained only domestically) is itself unstable and the ε figure authored here would need revision upward to reflect broader-scope extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_treaty_carveout_stability, empirical, 'Whether the international-obligations exception to unconstrained sovereignty remains structurally stable over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(basi_tr_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(basi_tr_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 16, 0.13).
narrative_ontology:measurement(basi_tr_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(basi_tr_t32, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(basi_be_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 8, 0.17).
narrative_ontology:measurement(basi_be_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 16, 0.21).
narrative_ontology:measurement(basi_be_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(basi_be_t32, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 32, 0.29).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, base_extractiveness, 40, 0.31).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(basi_su_t8, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(basi_su_t16, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 16, 0.29).
narrative_ontology:measurement(basi_su_t24, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(basi_su_t32, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 32, 0.39).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_boundary__parliamentary_sovereignty_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, judicial_supremacy_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_boundary__parliamentary_sovereignty_reading, balanced_contestation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'who has final authority over Basic Laws in Israel' per the ε-invariance principle: measuring this arrangement through the parliamentary-sovereignty lens yields a low-to-moderate, rising ε (0.14 to 0.31) because the reading treats the arrangement as a resolved coordination problem with concentrated, bounded costs on identifiable minorities; measuring the same underlying contest through the judicial_supremacy_reading lens would yield a structurally different ε (the standing arrangement under THAT reading is the override/bypass of binding judicial review, assessed as illegitimate constitutional circumvention by that reading's own lights, which that story authors independently). The balanced_contestation_reading sits structurally between, authoring its own ε for a bounded, mutually constrained arrangement. All three are linked here rather than merged into one story with an observable parameter, per the BGS-pattern authoring rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
