% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: 1890 Manifesto as Endogenous Revelatory Reversal of Plural Marriage Doctrine
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   In 1890, Church President Wilford Woodruff issued the Manifesto, publicly
 *   advising members against contracting new plural marriages, an act he
 *   later described as prompted by a September vision showing the
 *   consequences of continued resistance to federal law. This reading treats
 *   that vision as the operative mechanism: revelation resolved an apparent
 *   conflict between an eternal doctrinal commitment (plural marriage per
 *   Doctrine and Covenants Section 132) and existential institutional threat,
 *   by reframing God's will as having changed for the present circumstance
 *   rather than admitting the earlier doctrine was wrong or that the change
 *   was coerced. The reading foregrounds who benefits from this framing
 *   (leadership's continuing interpretive authority, institutional survival)
 *   and who pays (families whose marriages were destabilized, members left to
 *   reconcile an eternal principle's apparent revision) without collapsing
 *   into the sibling readings that treat the event as pure external coercion
 *   or as leaving an unresolved doctrine-practice gap.
 *
 * KEY AGENTS:
 *   - wilford_woodruff: agenda_setter (institutional/analytical) — issues the revelatory reversal
 *   - church_institutional_leadership: beneficiary (institutional/arbitrage) — retains interpretive authority and institutional continuity
 *   - plural_wives_and_children_post_manifesto: payer (powerless/trapped) — bear disruption of the reversal's uneven application
 *   - rank_and_file_members_expected_to_reconcile_doctrine: payer (moderate/constrained) — must internalize the changed-will framing
 *   - federal_government: excluded (institutional/analytical) — proximate coercive cause omitted from the revelatory narrative
 *   - historians_and_theologians: observer (analytical/analytical) — assess independence of the revelation from political necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.52).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.44).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "1890 Manifesto as Endogenous Revelatory Reversal of Plural Marriage Doctrine").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'fd2e1eeb-3098-4fa3-840c-0c401c6bee94').
narrative_ontology:cs_kernel_codification('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', fixed_text).
narrative_ontology:cs_authority_grounding('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', lineage).
narrative_ontology:cs_interpretation_layer_present('fd2e1eeb-3098-4fa3-840c-0c401c6bee94').
narrative_ontology:cs_reading_relation('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', foundational, continuing_revelation_supersedes_prior_revealed_practice).
narrative_ontology:cs_axiom_status(continuing_revelation_supersedes_prior_revealed_practice, holdable).
narrative_ontology:cs_axiom_grounding('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', continuing_revelation_supersedes_prior_revealed_practice, theological).
narrative_ontology:cs_axiom('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', secondary, prophetic_office_is_sole_legitimate_interpreter_of_changed_divine_will).
narrative_ontology:cs_axiom_status(prophetic_office_is_sole_legitimate_interpreter_of_changed_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', prophetic_office_is_sole_legitimate_interpreter_of_changed_divine_will, conventional).
narrative_ontology:cs_reference_frame('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', section_132_eternal_requirement_doctrine).
narrative_ontology:cs_drift_state('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', manifesto_declaration_1890, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fd2e1eeb-3098-4fa3-840c-0c401c6bee94', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_hierarchy_continuity).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_wives_and_children_post_manifesto).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_members_expected_to_reconcile_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, continuing_revelation_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__endogenous_reinterpretation_reading, prophetic_interpretive_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As Church President, issues the September 23-25, 1889/1890 revelatory statement (the Manifesto) publicly advising against future plural marriages, framed as a vision/revelation resolving conflict between prior doctrine (Section 132, sealed for eternity) and mounting federal pressure. His authority to receive and declare revelation is the mechanism by which the reversal is legitimated rather than treated as capitulation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, wilford_woodruff, agenda_setter,
    institutional, civilizational, analytical, national).

% The First Presidency and Quorum of Twelve retain interpretive control over doctrine and property (temples, church assets, incorporation) that federal seizure under the Edmunds-Tucker Act threatened. The revelation framing lets the institution survive intact, preserve its claim to continuing prophetic authority, and avoid admitting the practice was abandoned under duress rather than divine change of will.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership, beneficiary,
    institutional, civilizational, arbitrage, national).

% Existing plural wives and their children bear the practical and social cost of a doctrine reversed on paper but only partially enforced in practice for years afterward; some families are quietly maintained, others abandoned or forced into legal and social limbo, with no institutional mechanism to redress the disruption caused by the reversal or its uneven application.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, plural_wives_and_children_post_manifesto, payer,
    powerless, biographical, trapped, local).

% Ordinary members are asked to accept that a practice previously taught as an eternal requirement for the highest degree of celestial glory has now been suspended by revelation, without a doctrinal explanation of why an eternal principle changed. They must either internalize the framing (God's will adapted to circumstance) or carry unresolved cognitive dissonance; exit from the community carries high social and familial cost.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, rank_and_file_members_expected_to_reconcile_doctrine, payer,
    moderate, generational, constrained, national).

% The actual proximate cause of the reversal — escheatment threats, disenfranchisement, and prosecution under the Edmunds-Tucker Act — is structurally excluded from the revelatory narrative. Its coercive role is real but is not part of the story the institution tells about why the practice ended.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, biographical, analytical, national).

% Examine the documentary record (Woodruff's journal, the timing relative to federal legal pressure, subsequent semi-covert continuation of plural marriages after 1890) to assess whether the revelation was a genuine independent theological event or a legitimating narrative for an externally forced policy change.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, historians_and_theologians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides the institution and its membership a theologically coherent off-ramp from a practice that had become existentially threatening to the church's survival, allowing continued collective religious identity and leadership legitimacy without an admission of doctrinal error or capitulation to outside coercion.
% TRANSFER_FUNCTION: Moves the cost of doctrinal inconsistency and family disruption onto plural wives, their children, and ordinary members who must reconcile the reversal, while the interpretive authority and institutional continuity accrue to church leadership and its ongoing legitimacy claim.
% ABSENT_VOICES: Plural wives whose marriages were rendered legally and socially ambiguous had essentially no voice in the declaration's drafting or timing; federal officials whose coercive pressure was the proximate cause are excluded from the revelatory account entirely, as are members who privately doubted the vision's independence from political necessity.
% DISAPPEARANCE_RATIONALE: Had Woodruff not issued a revelatory reversal (or had the reversal been openly framed as pure capitulation to federal threat rather than revelation), the church's claim to continuous prophetic authority would have been directly damaged, statehood negotiations for Utah would have taken a different shape, and the doctrine-practice gap around Section 132 would have had to be resolved through open acknowledgment of error rather than narrative continuity — the institution's self-understanding and its relationship to the state would look substantially different.
% FOUNDING_PROBLEM: The church faced an existential conflict between a doctrine taught as an eternal requirement (plural marriage, per Section 132) and federal legal pressure (Edmunds-Tucker Act) that threatened to dissolve the church corporation, seize temple properties, and imprison or disenfranchise practicing members — a problem that endangered institutional survival itself, not merely policy preference.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians (documented in church-external and mixed archival scholarship on the Manifesto period) and legal historians of the Edmunds-Tucker Act corroborate that the practical threat to church property and incorporation was resolved by the early 1890s and was not renewed; the church's own institutional voice continues to frame the resolution primarily as revelatory rather than legally compelled, which is the asymmetry this reading is built to name.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.52 rather than high: within this reading's own terms, the revelation genuinely resolves a real coordination problem (institutional survival, doctrinal continuity) and is not pure pretext — but it also demonstrably transfers costs onto families and members without their voice in the decision, and it entrenches leadership's interpretive monopoly going forward. Theater ratio is authored higher (0.58, rising sharply from 0.35 to ~0.6 around the Manifesto's issuance and settling there) because a substantial share of subsequent institutional activity (temple recommend questions, official statements, later semi-covert continuation of plural marriages by some leaders after 1890 followed by the 1904 Second Manifesto) reflects performative reaffirmation of the reversal's completeness rather than the underlying reality, which remained messier for over a decade. Suppression falls over the measured interval (0.55 to 0.44) as the acute federal legal threat that had driven raids and prosecutions receded after statehood in 1896, reducing the active coercive backdrop even as the revelatory framing itself remained fixed doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Church institutional leadership sits near the beneficiary end: the revelation framing lets the presidency retain its unique channel to ongoing revelation as the mechanism of legitimate change, insulating the institution from a narrative of doctrinal failure or capitulation. Plural wives and their children sit near the full-target end: trapped by existing marital and family commitments, powerless to influence the declaration's terms or timing, and bearing the practical fallout of a reversal implemented unevenly. Rank-and-file members occupy an intermediate position — moderate power, constrained exit (leaving the church carries steep social and familial cost) — required to do interpretive work reconciling an 'eternal' doctrine's apparent change without any institutional acknowledgment of the coercive backdrop.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents two mislabeling failures. First, treating the Manifesto purely as extraction (a snare) would ignore the real coordination benefit: without some resolution, the church risked dissolution as a legal and religious entity, which would have been worse for essentially all members, not just leadership. Second, treating it purely as legitimate revelatory coordination (a rope) would erase the asymmetric costs borne by plural families and the members left without adequate account of why an 'eternal' principle changed on a convenient legal timeline. Tangled Rope captures both: genuine coordination function (institutional survival) coexisting with asymmetric extraction (interpretive authority concentrated upward, disruption costs concentrated on the powerless), sustained by active enforcement (temple recommend interviews, excommunication threats for continued plural marriage) rather than by pure voluntary consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_independence_from_coercion,
    'Was Woodruff''s September vision a genuinely independent theological event, or a legitimating narrative constructed contemporaneously with (or after) the decision to comply with federal pressure?',
    'Close documentary analysis of the timing and content of Woodruff''s journal entries relative to legal developments (Edmunds-Tucker enforcement, the Idaho test oath case, threatened dissolution proceedings against the church corporation), cross-referenced with the drafting history of the public Manifesto text and its several redrafts before publication.',
    'If the vision demonstrably followed and was shaped by legal counsel''s advice on how to frame a forced policy change, this reading collapses toward the exogenous_override_reading''s account and this constraint''s beneficiary-legitimacy structure would need re-authoring as closer to pure extraction; if the vision genuinely precedes and is independent of legal strategizing, this reading''s moderate-extraction, genuine-coordination framing is better supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_independence_from_coercion, empirical, 'Whether the endogenous revelation was theologically prior to or constructed in response to external coercion.').

omega_variable(
    why_gods_will_changed,
    'If Section 132 declared plural marriage a requirement for the highest degree of celestial glory as an eternal, unchanging principle, what theological account explains its reversal for present circumstances without undermining the doctrine''s claimed eternality?',
    'Comparative analysis of official church statements from 1890 through the present addressing this tension (or its conspicuous absence), and how church theology has subsequently handled the doctrinal status of celestial plural marriage versus its earthly practice.',
    'A coherent, sustained theological account would support treating this as genuine doctrinal development; persistent avoidance or inconsistency across a century of statements would support reading the revelation framing as primarily a legitimacy-preserving device layered over an otherwise unresolved contradiction — closer to the practice_doctrine_gap sibling reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(why_gods_will_changed, conceptual, 'The unresolved theological tension between an eternal principle and its practical suspension.').

omega_variable(
    cs_framing_kernel_vs_authority_narrative,
    'Should the kernel here be understood as the doctrinal text (Section 132) or as the institution''s meta-claim to continuing revelatory authority that can reinterpret any text? Under the first framing, the Manifesto reads as amendment to a fixed_text kernel; under the second, it reads as an exercise of a distributed, self-renewing authority claim where the ''kernel'' is the authority mechanism itself, not any single revealed text.',
    'Examine whether subsequent official pronouncements treat Section 132 as still-binding canonical text (fixed_text framing) or treat the continuing-revelation principle as the operative and prior authority (implicit/practice framing), by tracking how the church''s own canon and correlated curriculum characterize the relationship between the two.',
    'Under the fixed_text framing, this constraint''s authority_grounding would lean toward lineage (continuity of an authorized interpretive chain reading a stable text); under the distributed-authority framing, it would lean toward extraction (the authority to reinterpret at will is what preserves institutional power, and the specific text becomes secondary) — this choice changes which authority_grounding value best fits and was resolved here in favor of lineage because the church''s own self-account foregrounds continuity of prophetic office over discontinuity of doctrine, but the alternative framing remains live.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_authority_narrative, conceptual, 'Alternative CS framings of what the kernel actually is: a fixed doctrinal text versus the self-renewing authority to reinterpret it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 5, 0.55).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 10, 0.62).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 15, 0.6).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(marr_tr_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 15, 0.5).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(marr_be_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 30, 0.44).
narrative_ontology:measurement(marr_su_t40, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 40, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.08).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the marriage_commitment_reversal kernel, decomposed per the ε-invariance principle because the natural-language label 'the 1890 Manifesto' conflates structurally distinct claims about mechanism (internal revelation vs. external coercion) and about doctrinal status (resolved vs. left ambiguous). endogenous_reinterpretation_reading (this story) authors moderate extraction under a genuine-coordination-plus-legitimacy-preservation account; exogenous_override_reading authors the same event as coerced capitulation with Section 132 doctrinally untouched, implying higher suppression and a different victim framing (theological consistency itself as casualty); practice_doctrine_gap authors the persistent doctrine/practice ambiguity as the structural fact rather than adjudicating mechanism at all. Each carries its own ε and stakeholder set; they are linked here because a purity or contamination signal on one plausibly propagates to the others via their shared documentary record and shared institutional actor (church_institutional_leadership).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
