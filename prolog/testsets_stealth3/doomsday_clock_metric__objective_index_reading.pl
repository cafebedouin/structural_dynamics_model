% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index (Objective-Index Reading)
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   Since 1947 the Bulletin of the Atomic Scientists' Science and Security
 *   Board has issued an annual setting of the Doomsday Clock, presented as a
 *   measure of humanity's proximity to civilization-ending catastrophe. This
 *   story authors the objective-index reading of that practice: the standing
 *   arrangement under which the setting is produced and received as an expert
 *   synthesis of empirical indicators, with epsilon assessed for that
 *   arrangement by this reading's own lights. The colloquial label 'the
 *   Doomsday Clock' covers three structurally distinct claims — that the
 *   setting tracks measurable risk (this file), that it is chosen for
 *   mobilization effect (performative_tool_reading), and that scientific and
 *   normative judgment are irreducibly entangled in it
 *   (hybrid_legitimacy_reading); per the epsilon-invariance principle each is
 *   a separate story, linked through the network. The claimed type and the
 *   metrics are authored independently: the claim states what this reading
 *   holds the arrangement to be; the metrics state how it observably
 *   operates.
 *
 * KEY AGENTS:
 *   - - bulletin_science_security_board: agenda setter and primary beneficiary seat (institutional / identity_locked) — administers the annual setting and collects the interpretive authority it confers
 *   - - existential_risk_expert_community: beneficiary (organized / constrained) — supplies the indicator literature and receives agenda-setting returns
 *   - - lay_public_risk_interpreters: primary target (powerless / constrained) — receives finished judgments with no channel to contest their construction
 *   - - democratic_oversight_bodies: target (institutional / trapped) — holds formal authority over risk policy without interpretive standing over the risk number
 *   - - science_journalists: dual-positioned amplifier and target (moderate / mobile) — trades access for amplification
 *   - - normative_frame_theorists: excluded critic (moderate / constrained) — contests the reading's premise from outside the process
 *   - - sts_scholars: analytical observer (analytical / analytical) — maps how the arrangement's authority is produced
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.66).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.72).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index (Objective-Index Reading)").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '13557273-ebc6-4810-8620-2ddbaaa4fcc2').
narrative_ontology:cs_kernel_codification('13557273-ebc6-4810-8620-2ddbaaa4fcc2', formalized).
narrative_ontology:cs_authority_grounding('13557273-ebc6-4810-8620-2ddbaaa4fcc2', expertise).
narrative_ontology:cs_interpretation_layer_present('13557273-ebc6-4810-8620-2ddbaaa4fcc2').
narrative_ontology:cs_reading_relation('13557273-ebc6-4810-8620-2ddbaaa4fcc2', doomsday_clock_metric__performative_tool_reading, forecloses).
narrative_ontology:cs_reading_relation('13557273-ebc6-4810-8620-2ddbaaa4fcc2', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_axiom('13557273-ebc6-4810-8620-2ddbaaa4fcc2', foundational, risk_setting_is_pure_measurement).
narrative_ontology:cs_axiom_status(risk_setting_is_pure_measurement, holdable).
narrative_ontology:cs_axiom_grounding('13557273-ebc6-4810-8620-2ddbaaa4fcc2', risk_setting_is_pure_measurement, empirically_contingent).
narrative_ontology:cs_axiom('13557273-ebc6-4810-8620-2ddbaaa4fcc2', foundational, normative_weighting_excludable_from_methodology).
narrative_ontology:cs_axiom_status(normative_weighting_excludable_from_methodology, holdable).
narrative_ontology:cs_axiom_grounding('13557273-ebc6-4810-8620-2ddbaaa4fcc2', normative_weighting_excludable_from_methodology, conventional).
narrative_ontology:cs_axiom('13557273-ebc6-4810-8620-2ddbaaa4fcc2', secondary, expert_competence_warrants_interpretive_authority).
narrative_ontology:cs_axiom_status(expert_competence_warrants_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('13557273-ebc6-4810-8620-2ddbaaa4fcc2', expert_competence_warrants_interpretive_authority, instrumental).
narrative_ontology:cs_reference_frame('13557273-ebc6-4810-8620-2ddbaaa4fcc2', neutral_indicator_aggregation_instrument).
narrative_ontology:cs_drift_state('13557273-ebc6-4810-8620-2ddbaaa4fcc2', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('13557273-ebc6-4810-8620-2ddbaaa4fcc2', '2026-08-10T14:32:00Z').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, existential_risk_expert_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, lay_public_risk_interpreters).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_oversight_bodies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, science_journalists).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, science_journalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes twice a year, commissions indicator briefings across nuclear, climate, biological, and technological domains, and issues one annual number behind closed doors. Decides which indicators enter the packet and how they are weighed; publishes the setting and a rationale but not the weighting scheme. Members' standing as risk authorities is bound up with the institution they steward, and internal dissent has historically exited through quiet resignation rather than external contest.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board, beneficiary).

% Supply the peer-reviewed literature the briefings draw on and receive agenda-setting returns: grant salience, media citation, and a canonical reference point that keeps catastrophic risk on institutional agendas. Engagement with the annual cycle is voluntary but career-relevant; abstaining costs visibility inside the field.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, existential_risk_expert_community, beneficiary,
    organized, generational, constrained, global).

% Receive the annual number as a finished judgment with no access to the weights behind it. Any individual can accept, ignore, or mock the figure, but there is no procedural channel to contest how it was computed or to advance a rival weighting; no counter-public with standing has formed.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, lay_public_risk_interpreters, payer,
    powerless, biographical, constrained, global).

% Legislatures and civic institutions that must position policy relative to catastrophic risk. The clock's cultural authority sets the terms they respond to; commissioning a rival assessment invites accusations of politicizing risk, so most ratify or cite the expert figure rather than produce independent interpretations. Formal authority over risk policy coexists with deference on risk interpretation.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_oversight_bodies, payer,
    institutional, generational, trapped, national).

% Cover the annual announcement as a dependable news event and depend on the board for access and quotable authority, amplifying the setting's reach in exchange. Press queries about the weighting scheme go unanswered, and outlets that recast the clock as advocacy find the access window narrows.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, science_journalists, payer,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, science_journalists, beneficiary).

% Philosophers, ethicists, and decision theorists who argue that catastrophic-risk evaluation cannot be divorced from value judgments about which harms count and what probabilities justify alarm. They publish in academic venues but hold no seat in the setting process, and board communications do not engage their objections.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, normative_frame_theorists, excluded,
    moderate, generational, constrained, global).

% Researchers who trace how the clock's authority is produced — credentialing, ritual, media amplification — and publish analyses of its governance. They neither set the number nor campaign against it; their seat is analytical.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, sts_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, bulletin_science_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates heterogeneous technical indicators — warhead counts and postures, climate trajectories, biosecurity developments, AI capability trends — into a single annually updated, publicly legible signal of proximity to civilization-ending catastrophe, solving the problem that dispersed indicators are illegible to non-specialists.
% TRANSFER_FUNCTION: Moves interpretive authority over existential risk from the public and from democratic institutions to the expert board; moves public attention and agenda priority toward the risks the board weights; confers epistemic legitimacy on the board's annual judgment.
% ABSENT_VOICES: Normative theorists and democratic representatives are absent from the setting room; the decisions they would contest — which risks count, how heavily each weighs, what counts as 'seconds to midnight' — are value-laden choices made without them and published only as conclusions.
% DISAPPEARANCE_RATIONALE: Risk communication would reorganize around the vacancy: legislatures and newsrooms would commission or adopt rival indices, competing expert coalitions would publish competing numbers, and the weighting of catastrophic risks would become an openly contested political question rather than an annually revealed expert fact.
% FOUNDING_PROBLEM: After Hiroshima, scientists who understood what nuclear weapons meant faced a public that could not read the technical indicators of peril; the clock was built to translate expert alarm into a civic-legible signal urgent enough to matter politically.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the archival record of the Chicago scientists' stated intent in 1947, published histories of the atomic scientists' movement, and the contemporary risk-communication literature all attest that translating expert catastrophic-risk knowledge into public-legible form remains an unsolved live problem. The Bulletin's own attestations agree but are not the basis; no source examined attests the problem is dead.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness (0.66 at interval end) prices the transfer of interpretive authority: the public and democratic institutions receive a finished number whose construction they cannot inspect, and the weighting decisions that determine which catastrophes dominate attention are made privately. Suppression (0.72) is the active maintenance of the objectivity claim — undisclosed weights, non-engagement with normative critics, framing of every setting as measurement rather than judgment. Theater ratio (0.40) reflects the growth of the announcement ritual relative to the analytic work beneath it: the meetings and indicator reviews are real, but a rising share of the arrangement's activity is the staged reveal and its media circuit. Accessibility collapse (0.52) is moderate: rival indices and democratic co-determination remain constructible, but none has acquired comparable standing, so alternatives are crowded out rather than impossible. Resistance (0.58) is sustained — journalistic skepticism, STS analysis, periodic public challenges — but never crystallizes into a rival institution. Coordination type is information_standard: the arrangement's primary function is encoding dispersed technical indicators into a single public unit of risk proximity. The three series share one time grid (eight points, 1947-2024). The 1991 dip in all three marks the post-Cold War rollback, which briefly made the arrangement look responsive to the world rather than to its own momentum. The suppression_requirement series is authored deliberately: this story's dynamic is enforcement intensification — as public deference to experts declined after 1968 and critiques of the clock as advocacy spread after 2000, holding the objectivity claim required progressively more active defense, culminating in the current posture of methodological opacity. Base extractiveness dips and resumes across the same span, rising net by roughly two-thirds of its starting value.
 *
 * PERSPECTIVAL GAP:
 *   From the board's seat the arrangement is custodianship: a hard-won communicative instrument built by people who witnessed the first nuclear tests, maintained against trivialization and against capture alike; the engine should compute a low-extraction, coordination-forward classification for that seat. From the public's and the oversight bodies' seats the same structure is a number they cannot audit, issued by a room they cannot enter, weighted toward risks they did not rank; the engine should compute materially higher effective extraction there. The divergence is structural — identical metrics, opposite directionalities — and the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The board sits nearest the beneficiary pole: it defines the inputs, performs the synthesis, and collects the authority the output confers; its identity lock means exit would cost it the very standing the arrangement provides. The expert community collects second-order returns and sits low-moderate. Lay interpreters and democratic oversight bodies sit nearest the target pole: both bear the cost of uncontestable judgment, the oversight bodies more acutely because their formal power is unusable on precisely the question the number answers — which is why they are authored trapped where the public is merely constrained. Journalists straddle: paid in access, paying in amplification they cannot condition on scrutiny. No directionality_overrides are authored: the two institutional-power seats sit at opposite poles, and an override keys to the power atom alone, so it could not separate them — the role-based derivation from beneficiaries, victims, and exit options is what distinguishes them, and it is left to stand.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — translating expert knowledge of catastrophic risk into a civic-legible signal — is still live, corroborated by sources outside the benefiting parties, so no zombie flag is expected from the genealogy interview. The type distinction does real work here: calling the whole arrangement a snare would erase the genuine aggregation service (dispersed indicators really are illegible to non-specialists, and something like this synthesis would be rebuilt); calling it a rope would erase the interpretive monopoly (weighting civilizations' risks is a value-laden act performed privately and received as measurement). Tangled rope holds both facts. The receipt surface sharpens the picture: the gains accrue to a single named seat, and self-reform is prohibitive for that seat because publishing the weighting scheme would dissolve the objectivity claim that constitutes its authority — the arrangement cannot fix itself from inside.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the doomsday_clock_metric kernel — the objective_index_reading. What changes structurally if a sibling reading (performative_tool_reading or hybrid_legitimacy_reading) is the better account of the standing arrangement?',
    'Comparative classification across the three linked reading-stories: whichever reading best predicts the observable record (setting behavior, methodology disclosure or its absence, board rhetoric) prevails; the corpus holds all three as separate files with separate epsilon values.',
    'Under the performative reading the arrangement reclassifies around strategic communication, with a different beneficiary and victim structure; under the hybrid reading the suppression of normative framing becomes the central extractive mechanism and epsilon rises further. This file''s classification holds only within the objective-index reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three mutually exclusive constitutive readings of the clock kernel.').

omega_variable(
    normative_content_separability,
    'Is the weighting of indicators in the setting actually separable from value judgment, as the objective-index claim requires?',
    'Disclosure of the weighting scheme followed by replication attempts: if independent analysts given the same indicator packet converge on the board''s setting, separability holds; persistent divergence locates the normative residue.',
    'If inseparable, the objective reading collapses toward the hybrid reading, the suppression metric measures denial rather than maintenance, and the arrangement''s coordination claim weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_content_separability, empirical, 'Whether value judgment can be excised from the setting methodology.').

omega_variable(
    methodology_opacity_verification,
    'Does the published setting correlate with any reproducible composite of the cited indicators, given that the weighting scheme has never been disclosed?',
    'Retrospective reconstruction: fit candidate weightings to the historical indicator record and test predictive accuracy against subsequent settings.',
    'A successful reconstruction supports the index claim and lowers effective extraction; failure establishes that the number is not derivable from its stated inputs, the strongest available evidence for the rival readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(methodology_opacity_verification, empirical, 'Testability of the objectivity claim under current methodological opacity.').

omega_variable(
    soft_construal_relation_shift,
    'The declared foreclosure of hybrid_legitimacy_reading assumes the strict construal of this reading (normative content excluded from the setting). Under a softer construal — measurement with normative inputs disclosed — does the relation to the hybrid sibling downgrade?',
    'Conceptual analysis of which construal the board''s own communications commit to: rhetoric of pure measurement sustains foreclosure; rhetoric of informed judgment downgrades the relation.',
    'Under the soft construal this reading and the hybrid reading could coexist within one framework, changing the kernel''s structure from a partition into a spectrum and altering computed foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(soft_construal_relation_shift, conceptual, 'Framing under-determination in this reading''s relation to its hybrid sibling.').

omega_variable(
    epistemic_deference_trajectory,
    'What happens to the arrangement as general deference to expert institutions continues to decline?',
    'Track reception metrics: media framing of the annual announcements, citation of the setting in official documents, survey trust in the issuing body.',
    'If deference collapse continues, the arrangement drifts toward inertial maintenance — announced ritually, believed by fewer, changed by no one — shifting its classification toward the degraded end without any change in the board''s behavior.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_deference_trajectory, empirical, 'Persistence of the arrangement under declining epistemic deference.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doomsday_objective_index_tr_t1947, doomsday_clock_metric__objective_index_reading, theater_ratio, 1947, 0.15).
narrative_ontology:measurement(doomsday_objective_index_tr_t1958, doomsday_clock_metric__objective_index_reading, theater_ratio, 1958, 0.18).
narrative_ontology:measurement(doomsday_objective_index_tr_t1968, doomsday_clock_metric__objective_index_reading, theater_ratio, 1968, 0.22).
narrative_ontology:measurement(doomsday_objective_index_tr_t1980, doomsday_clock_metric__objective_index_reading, theater_ratio, 1980, 0.26).
narrative_ontology:measurement(doomsday_objective_index_tr_t1991, doomsday_clock_metric__objective_index_reading, theater_ratio, 1991, 0.24).
narrative_ontology:measurement(doomsday_objective_index_tr_t2002, doomsday_clock_metric__objective_index_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(doomsday_objective_index_tr_t2015, doomsday_clock_metric__objective_index_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(doomsday_objective_index_tr_t2024, doomsday_clock_metric__objective_index_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(doomsday_objective_index_be_t1947, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1947, 0.4).
narrative_ontology:measurement(doomsday_objective_index_be_t1958, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1958, 0.46).
narrative_ontology:measurement(doomsday_objective_index_be_t1968, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1968, 0.53).
narrative_ontology:measurement(doomsday_objective_index_be_t1980, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1980, 0.59).
narrative_ontology:measurement(doomsday_objective_index_be_t1991, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1991, 0.53).
narrative_ontology:measurement(doomsday_objective_index_be_t2002, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2002, 0.57).
narrative_ontology:measurement(doomsday_objective_index_be_t2015, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(doomsday_objective_index_be_t2024, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2024, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(doomsday_objective_index_su_t1947, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1947, 0.35).
narrative_ontology:measurement(doomsday_objective_index_su_t1958, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1958, 0.38).
narrative_ontology:measurement(doomsday_objective_index_su_t1968, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1968, 0.45).
narrative_ontology:measurement(doomsday_objective_index_su_t1980, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1980, 0.52).
narrative_ontology:measurement(doomsday_objective_index_su_t1991, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(doomsday_objective_index_su_t2002, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2002, 0.56).
narrative_ontology:measurement(doomsday_objective_index_su_t2015, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(doomsday_objective_index_su_t2024, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'the Doomsday Clock' into a three-story constraint family per the epsilon-invariance principle: the objective-index claim (this file, epsilon 0.66), the performative-tool claim, and the hybrid-legitimacy claim carry different epsilon values, different beneficiary/victim structures, and different failure modes, so measuring one observable for all three would average away exactly the structure the corpus exists to take. The objective-index claim functions upstream: it is the account the board officially professes, and both sibling readings define themselves against it, which is why this file links to both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
