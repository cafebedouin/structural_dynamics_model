% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Existential-Risk Index (Objective-Index Reading)
 *   domain: science communication / normative epistemology / risk governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained since 1947 by the Bulletin of the Atomic
 *   Scientists, is set each year by an invited Science and Security Board
 *   whose methodology connecting empirical indicators to the clock's minutes
 *   has never been published. This story instantiates the
 *   objective_index_reading of the doomsday_clock_metric kernel: the claim
 *   that the setting tracks measurable existential-risk levels through expert
 *   synthesis of empirical indicators. Per the epsilon-invariance principle,
 *   the colloquial label 'the Doomsday Clock' decomposes into three
 *   structurally distinct claims — this objective-index claim, a
 *   performative-tool claim, and a hybrid-legitimacy claim — authored as
 *   separate linked stories; this file authors ONLY the objective-index
 *   reading, with epsilon's referent fixed to the standing arrangement under
 *   contest: the clock-setting practice as it actually operates, assessed by
 *   this reading's own lights. Even on that reading's own terms, the
 *   arrangement concentrates interpretive authority over civilizational risk
 *   in an unaccountable board and suppresses the normative framings that any
 *   single risk number embeds; the beneficiary and victim declarations below
 *   record that concentration rather than averaging it away.
 *
 * KEY AGENTS:
 *   - science_security_board: agenda-setting beneficiary (institutional / identity_locked) — sets the minutes, administers the unpublished methodology, collects the interpretive authority the arrangement confers
 *   - bulletin_institution: beneficiary (institutional / mobile) — hosts the clock, collects attention, donations, and brand relevance
 *   - expert_risk_assessment_community: secondary beneficiary (institutional / mobile) — its collective authority is vindicated by the arrangement's operation
 *   - democratic_public: primary payer (moderate / constrained) — bears the loss of contestable normative deliberation over existential risk
 *   - normative_dissent_voices: excluded (moderate / constrained) — ethicists, STS scholars, religious and community framings structurally outside the methodology's scope
 *   - media_organizations: incidental beneficiary (powerful / mobile) — collects the annual news cycle the unveiling generates
 *   - risk_governance_scholars: analytical observer (analytical / analytical) — maps the arrangement's epistemic structure from outside, collects and bears nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.7).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.72).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Existential-Risk Index (Objective-Index Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science communication / normative epistemology / risk governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '93f0305c-219a-4fe9-9aec-6f77ac56795d').
narrative_ontology:cs_kernel_codification('93f0305c-219a-4fe9-9aec-6f77ac56795d', implicit).
narrative_ontology:cs_authority_grounding('93f0305c-219a-4fe9-9aec-6f77ac56795d', expertise).
narrative_ontology:cs_interpretation_layer_present('93f0305c-219a-4fe9-9aec-6f77ac56795d').
narrative_ontology:cs_reading_relation('93f0305c-219a-4fe9-9aec-6f77ac56795d', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_reading_relation('93f0305c-219a-4fe9-9aec-6f77ac56795d', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_axiom('93f0305c-219a-4fe9-9aec-6f77ac56795d', foundational, aggregate_existential_risk_empirically_measurable).
narrative_ontology:cs_axiom_status(aggregate_existential_risk_empirically_measurable, holdable).
narrative_ontology:cs_axiom_grounding('93f0305c-219a-4fe9-9aec-6f77ac56795d', aggregate_existential_risk_empirically_measurable, empirically_contingent).
narrative_ontology:cs_axiom('93f0305c-219a-4fe9-9aec-6f77ac56795d', foundational, normative_weighting_separable_from_measurement).
narrative_ontology:cs_axiom_status(normative_weighting_separable_from_measurement, holdable).
narrative_ontology:cs_axiom_grounding('93f0305c-219a-4fe9-9aec-6f77ac56795d', normative_weighting_separable_from_measurement, empirically_contingent).
narrative_ontology:cs_reference_frame('93f0305c-219a-4fe9-9aec-6f77ac56795d', calibrated_empirical_risk_index).
narrative_ontology:cs_drift_state('93f0305c-219a-4fe9-9aec-6f77ac56795d', contemporary_portfolio_expansion_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('93f0305c-219a-4fe9-9aec-6f77ac56795d', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_institution).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_risk_assessment_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_public).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, normative_dissent_voices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, media_organizations).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, value_free_measurement_ideal).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, expert_risk_synthesis_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% An invited group of scientists and policy scholars that meets annually to decide the clock's setting. It selects which risks to weigh, how much each counts, and how far the hands move, and presents the result at a public briefing. Members serve in rotating cohorts drawn from the same professional networks; the procedure connecting indicators to minutes is not published. Leaving the board would mean publicly stepping away from the institution's signature activity, which almost no member has done.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_security_board, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, science_security_board, beneficiary).

% The nonprofit that publishes the Bulletin of the Atomic Scientists and owns the clock. The annual setting drives its visibility, donations, and press coverage each year, and the clock is by a wide margin its most-cited output. It could de-emphasize the clock and pivot to other publications, and has diversified, but the clock remains its brand.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_institution, beneficiary,
    institutional, generational, mobile, global).

% The broader network of physical scientists, climate researchers, and security-studies scholars from which board members are drawn and to which the clock's judgments are attributed. The annual presentation presents their collective judgment as the source of the clock's numbers, reinforcing the standing of quantitative expert assessment in public life.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_risk_assessment_community, beneficiary,
    institutional, generational, mobile, global).

% The publics in whose name the clock speaks and whose collective fate the setting describes. They receive the annual number through media coverage with no channel to contest how it was computed, which risks were counted, or how they were weighted. Their recourse is limited to accepting the number, ignoring it, or generically distrusting it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_public, payer,
    moderate, generational, constrained, global).

% Ethicists, science-and-technology-studies scholars, religious leaders, and community advocates who hold that a single number about civilizational danger embeds contestable value choices. They are outside the board's composition and outside the methodology's declared scope; their objections surface in op-eds and journals the setting process does not consult.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, normative_dissent_voices, excluded,
    moderate, biographical, constrained, global).

% News outlets that cover the annual announcement as a reliable recurring story. The clock gives them a ready-made visual, a quotable number, and an authority to cite; their amplification carries the setting to mass audiences. Their attention is portable — if the clock lost newsworthiness they would drop it without cost.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, media_organizations, beneficiary,
    powerful, biographical, mobile, global).

% Researchers in risk governance and epistemic-injustice scholarship who study the clock's history, procedure, and public function. They reconstruct setting decisions from archival and testimonial sources and publish analyses of how the arrangement's authority is maintained; they collect no benefit from the clock and bear no cost from its setting.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, risk_governance_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, science_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates distributed expert assessments across nuclear, climate, biological, and technological hazard domains into a single publicly legible signal, giving journalists, policymakers, and publics a common reference point for how close civilizational catastrophe is judged to be.
% TRANSFER_FUNCTION: Moves interpretive authority over existential risk from open democratic deliberation to the Science and Security Board's unpublished synthesis, and moves public attention and policy salience toward whichever hazards the board's annual statement selects.
% ABSENT_VOICES: Normative dissenters — ethicists, STS scholars, religious and community leaders, and lay publics — would contest the value weightings any single number embeds; they sit outside the board's composition and its methodology's scope. Communities bearing hazards the board declines to add to the portfolio likewise have no seat in the setting.
% DISAPPEARANCE_RATIONALE: If the clock and its annual setting vanished overnight, the risk-communication field would reorganize within a few news cycles: rival indices and direct expert testimony would fill the signal gap, the Bulletin would lose its primary brand asset, and board members would lose the platform their authority currently rides on. The democratic public would lose a shared reference point, for better or worse, and normative contestation over civilizational risk would lose its single most visible target.
% FOUNDING_PROBLEM: After 1945, the destructiveness of nuclear weapons was invisible to publics and unrepresented in public deliberation; the clock was built to give an inattentive public a single, legible signal of how close nuclear catastrophe might be.
% FOUNDING_PROBLEM_CORROBORATION: Arms-control historians and physicists outside the Bulletin corroborate that the original communicative gap was real in 1947. The Bulletin's board attests the problem is live and widening (annual statements cite arsenals, climate, and artificial intelligence). From outside the beneficiary set, STS scholars and media critics attest the arrangement's center of function has shifted toward authority maintenance and advocacy; no external party attests that a single expert-set index currently tracks an aggregate existential-risk quantity. Corroboration for the founding problem's present status is split along the same lines as the kernel contest itself.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.70 at interval end: the arrangement converts contestable normative judgment into apparently objective output, and the interpretive authority it moves is real and concentrated even though what moves is epistemic rather than material. Suppression is 0.72 as a raw structural property — unscaled by power or scope; only extractiveness is scaled — maintained by methodology opacity, boundary work against critics, and media amplification, with a smaller internalized component of public epistemic deference (the split is carried by an omega, not resolved by the scalar). Theater is 0.48: the deliberation behind each setting is real work, but the seconds-precision format implies a measurement resolution the synthesis cannot support, and the annual unveiling and portfolio-expansion announcements are substantially ritual. Accessibility collapse is 0.55 — once the objective framing is accepted, alternative framings of what the setting is lose standing but do not vanish; resistance is 0.55, sustained by STS critique, journalistic scrutiny, and intra-scientific dissent. All three series share one time grid; the 1984-to-1991 dip is externally driven (Cold War thaw), not internal reform — the underlying trend in all three series is upward, with the suppression series tracking the growth of the machinery defending the objective framing (portfolio expansion into politically contested domains, the unilateral move to seconds-level display, hardened refusal to publish weighting criteria).
 *
 * PERSPECTIVAL GAP:
 *   The board seat and the public seat should compute differently. From the board's position the arrangement is a public service it staffs with its best judgment: it experiences the structure as the work of getting the number right and itself as the party bearing that burden. From the public's position the same structure is an unaccountable monopoly over a question — how safe are we? — that is constitutively normative, experienced as a number that cannot be contested, recomputed, or appealed. The excluded dissenters' seat is sharper still: they experience the methodology's opacity not as privacy but as the mechanism that keeps their objections out of scope. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries — the board, the Bulletin, the wider expert community — derive low directionality (the arrangement subsidizes them), with the board dual-positioned as the seat that both runs the arrangement and collects from it. Declared victims — the democratic public and normative dissenters — derive high directionality, amplified by constrained exit: there is no opting out of a risk discourse the clock anchors, and the dissenters' framings are excluded rather than merely outvoted. Media organizations benefit incidentally with near-arbitrage exit (attention is portable), placing them nearest the beneficiary end despite their amplification role. Global spatial scope amplifies effective extraction on the target seats because the methodology's opacity is verifiable by essentially no one outside the board — at global scope, verification failure is the default condition.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope claim prevents two opposite mislabelings. Reading the arrangement as pure extraction would erase its genuine coordination function: aggregating distributed expert judgment into a common public signal solves a real collective problem, and the clock's history includes episodes (1991) where the signal moved against the setters' prior positioning, which a pure cover story would not permit. Reading it as pure coordination would launder the extraction: the same structure that aggregates information also concentrates interpretive authority, and the founding problem — communicating nuclear danger to an inattentive public — has been stretched across a portfolio (climate, artificial intelligence, biology) whose weighting decisions are irreducibly normative. The founding-problem interview records that status as contested rather than dead; the receipt surface names the board as the seat the gains accrue to, with fixing priced prohibitive for the fixer because publishing the weighting methodology would surrender the very authority the arrangement confers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Is the clock-setting arrangement best modeled by this reading (objective index tracking measurable risk) or by the sibling readings of the doomsday_clock_metric kernel (strategically chosen mobilization device; irreducible scientific-normative entanglement)?',
    'Comparative structural test across the three authored stories: does the setting''s variance track documented indicator movements, advocacy and mobilization milestones, or neither separably? The three readings make different predictions about which covariates drive setting changes.',
    'Adopting the performative reading reclassifies the theater_ratio as the arrangement''s primary function and raises effective extraction on the public seat; adopting the hybrid reading dissolves this reading''s separability axiom entirely and reclassifies the coordination function as inherently normative, collapsing the objective-index claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the doomsday_clock_metric kernel correctly models the arrangement''s determinant structure.').

omega_variable(
    setting_indicator_correlation,
    'Does the clock setting actually correlate with any validated aggregate measure of existential risk, or does no such validated metric exist for the setting to track?',
    'Retrospective correlation study of setting changes against documented indicator movements (deployed warhead counts, warming trajectories, bio-risk indices) with controls for board-composition changes and advocacy campaigns; plus expert elicitation on whether any aggregate existential-risk quantity is measurable at all.',
    'A null or board-composition-driven correlation would collapse this reading''s foundational premise: the arrangement would stand exposed as authority performance rather than measurement, and the classification would move sharply toward pure extraction with the coordination function demoted to cover.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(setting_indicator_correlation, empirical, 'Whether the setting empirically tracks anything measurable, the empirical core of this reading''s claim.').

omega_variable(
    normative_weighting_locus,
    'Where do the irreducibly normative judgments enter the setting — in indicator selection, in portfolio weighting across hazard domains, or in the threshold choice of what counts as proximity to catastrophe?',
    'Methodology disclosure by the board, or reconstruction of setting decisions from archival minutes and member testimony, mapping each decision node to its empirical versus evaluative content.',
    'If normative judgment concentrates at a single threshold node, a bounded fix (explicit, contestable thresholds) could separate the coordination function from the extraction; if it permeates the synthesis, the objective reading fails wholesale and no bounded reform recovers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_weighting_locus, conceptual, 'The location of value judgments inside the unpublished synthesis procedure.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of normative framing structural (board gatekeeping, methodology opacity, media amplification of the objective framing) or internalized (public epistemic deference to scientific authority that would persist even if the gatekeeping lifted)?',
    'Natural experiments from periods when the Bulletin''s amplification capacity dropped (funding and ownership disruptions) — did normative contestation resurge? — plus survey data on deference to expert risk numbers held independent of the Bulletin''s activity.',
    'If the internalized share is large, effective suppression exceeds the structural measure and persists under any reform of the board''s composition; the constraint would survive its own gatekeepers. If structural, opening the methodology would release the suppressed framings quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized split in the suppression of normative framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1947, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__objective_index_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement_basis(doom_tr_t1947, observed).
narrative_ontology:measurement(doom_tr_t1963, doomsday_clock_metric__objective_index_reading, theater_ratio, 1963, 0.2).
narrative_ontology:measurement_basis(doom_tr_t1963, observed).
narrative_ontology:measurement(doom_tr_t1984, doomsday_clock_metric__objective_index_reading, theater_ratio, 1984, 0.28).
narrative_ontology:measurement_basis(doom_tr_t1984, observed).
narrative_ontology:measurement(doom_tr_t1991, doomsday_clock_metric__objective_index_reading, theater_ratio, 1991, 0.22).
narrative_ontology:measurement_basis(doom_tr_t1991, observed).
narrative_ontology:measurement(doom_tr_t2007, doomsday_clock_metric__objective_index_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement_basis(doom_tr_t2007, observed).
narrative_ontology:measurement(doom_tr_t2018, doomsday_clock_metric__objective_index_reading, theater_ratio, 2018, 0.42).
narrative_ontology:measurement_basis(doom_tr_t2018, observed).
narrative_ontology:measurement(doom_tr_t2026, doomsday_clock_metric__objective_index_reading, theater_ratio, 2026, 0.48).
narrative_ontology:measurement_basis(doom_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement_basis(doom_be_t1947, observed).
narrative_ontology:measurement(doom_be_t1963, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1963, 0.45).
narrative_ontology:measurement_basis(doom_be_t1963, observed).
narrative_ontology:measurement(doom_be_t1984, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1984, 0.55).
narrative_ontology:measurement_basis(doom_be_t1984, observed).
narrative_ontology:measurement(doom_be_t1991, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1991, 0.48).
narrative_ontology:measurement_basis(doom_be_t1991, observed).
narrative_ontology:measurement(doom_be_t2007, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2007, 0.58).
narrative_ontology:measurement_basis(doom_be_t2007, observed).
narrative_ontology:measurement(doom_be_t2018, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2018, 0.66).
narrative_ontology:measurement_basis(doom_be_t2018, observed).
narrative_ontology:measurement(doom_be_t2026, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2026, 0.7).
narrative_ontology:measurement_basis(doom_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1947, 0.25).
narrative_ontology:measurement_basis(doom_su_t1947, observed).
narrative_ontology:measurement(doom_su_t1963, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1963, 0.35).
narrative_ontology:measurement_basis(doom_su_t1963, observed).
narrative_ontology:measurement(doom_su_t1984, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1984, 0.45).
narrative_ontology:measurement_basis(doom_su_t1984, observed).
narrative_ontology:measurement(doom_su_t1991, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1991, 0.3).
narrative_ontology:measurement_basis(doom_su_t1991, observed).
narrative_ontology:measurement(doom_su_t2007, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement_basis(doom_su_t2007, observed).
narrative_ontology:measurement(doom_su_t2018, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement_basis(doom_su_t2018, observed).
narrative_ontology:measurement(doom_su_t2026, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2026, 0.72).
narrative_ontology:measurement_basis(doom_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Doomsday Clock' conflates three structurally distinct claims about one artifact: that it is an objective risk index (this story), that it is a strategic mobilization device (performative_tool_reading), and that it embodies irreducible scientific-normative entanglement (hybrid_legitimacy_reading). Each claim has its own epsilon, beneficiary/victim structure, and classification, so per the epsilon-invariance principle they are authored as separate stories linked by network edges. This objective-index reading is the upstream member: as the Bulletin's official self-understanding, it supplies the legitimacy conditions under which the other two readings operate as critiques — the performative critique must overcome the presumption of measurement, and the hybrid critique must deny the possibility this reading asserts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
