% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock Setting as Deliberately Hybrid Expert-Normative Instrument
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained since 1947 by the Bulletin of the Atomic
 *   Scientists, converts expert deliberation about civilizational catastrophe
 *   into a single annual image: minutes to midnight. This story instantiates
 *   the hybrid-legitimacy reading of that instrument — the claim that the
 *   setting irreducibly fuses empirical judgment with normative stakes, and
 *   that this deliberate ambiguity is the source of its public authority.
 *   Under this reading the clock solves a real coordination problem (giving a
 *   fragmented risk landscape a shared focal point) while generating a
 *   growing accountability void (a number that looks precise, cannot be
 *   scored, and concentrates attention, authority, and donations on its
 *   keeper). The colloquial label 'the Doomsday Clock' covers structurally
 *   distinct claims; per the epsilon-invariance principle this file authors
 *   only the hybrid reading, with the objective-index and performative-tool
 *   readings as linked sibling constraints carrying their own epsilon values.
 *   KEY AGENTS (by structural relationship): -
 *   bulletin_atomic_scientists_board: agenda-setter and principal collector
 *   (organized/identity_locked) — sets the clock, hosts the announcement,
 *   absorbs the attention, authority, and donor flows -
 *   clock_sponsor_network: credibility lender (organized/mobile) — scientists
 *   whose association underwrites the signal - policy_agenda_community:
 *   institutional borrower (institutional/mobile) — anchors arguments to the
 *   clock's number - news_media_cycle: amplifier-beneficiary
 *   (organized/arbitrage) — receives a reliable annual visual peg -
 *   attentive_global_public: dual-positioned audience (moderate/mobile) —
 *   gains a shared shorthand, absorbs mild epistemic distortion -
 *   quantitative_risk_researchers: excluded rivals (moderate/constrained) —
 *   produce falsifiable assessments crowded out in salience -
 *   risk_governance_analysts: analytical observer (analytical/analytical) —
 *   studies how hybrid instruments hold legitimacy
 *
 * KEY AGENTS:
 *   - bulletin_atomic_scientists_board: agenda-setter and principal collector — administers the annual setting and is the seat where attention, authority, and donor support demonstrably accrue
 *   - clock_sponsor_network: beneficiary — lends scientific standing, participates voluntarily, exits costlessly
 *   - policy_agenda_community: beneficiary — borrows a ready-made urgency marker for speeches, hearings, and negotiations
 *   - news_media_cycle: beneficiary — receives a predictable annual spectacle it may amplify or ignore
 *   - attentive_global_public: beneficiary with secondary payer position — gains a common reference point, absorbs a number that presents judgment calls with the look of precision
 *   - quantitative_risk_researchers: excluded — build falsifiable probabilistic assessments that compete for the same attention the clock occupies, with no seat in the setting process
 *   - risk_governance_analysts: observer — evaluates the instrument against epistemic and governance criteria without operating it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.52).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.12).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Setting as Deliberately Hybrid Expert-Normative Instrument").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '2cf19142-63d4-4eac-b8cb-a037dbc4c08a').
narrative_ontology:cs_kernel_codification('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', implicit).
narrative_ontology:cs_authority_grounding('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', practice).
narrative_ontology:cs_interpretation_layer_present('2cf19142-63d4-4eac-b8cb-a037dbc4c08a').
narrative_ontology:cs_reading_relation('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', foundational, normative_entanglement_irreducible).
narrative_ontology:cs_axiom_status(normative_entanglement_irreducible, holdable).
narrative_ontology:cs_axiom_grounding('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', normative_entanglement_irreducible, empirically_contingent).
narrative_ontology:cs_axiom('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', foundational, deliberate_ambiguity_preserves_legitimacy).
narrative_ontology:cs_axiom_status(deliberate_ambiguity_preserves_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', deliberate_ambiguity_preserves_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', hybrid_expert_normative_instrument).
narrative_ontology:cs_drift_state('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', contemporary_multi_risk_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2cf19142-63d4-4eac-b8cb-a037dbc4c08a', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, clock_sponsor_network).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, policy_agenda_community).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, news_media_cycle).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, attentive_global_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, attentive_global_public).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, hybrid_expertise_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deliberates annually behind closed doors, weighs testimony from sponsor scientists and invited experts, and announces a new setting with a public statement. The announcement draws global press coverage, sustains the organization's profile, and anchors its fundraising. The board cannot step away from the clock without dissolving the institution's central public asset; nearly eight decades of continuity have made the clock and the organization effectively inseparable.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists_board, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists_board, beneficiary).

% Lend names and disciplinary standing to the enterprise through board service and public endorsement. Association confers visibility and a channel for scientists to address mass audiences; disassociation is easy and costless, which keeps participation voluntary.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, clock_sponsor_network, beneficiary,
    organized, biographical, mobile, global).

% Cites the clock's setting in speeches, hearings, and negotiations as a ready-made urgency marker. It obtains a shared rhetorical anchor without commissioning analysis, and can adopt or drop the reference as convenience dictates.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_agenda_community, beneficiary,
    institutional, generational, mobile, global).

% Receives a predictable annual news event with a striking visual. Coverage amplifies the setting to mass audiences; editors can cover or ignore it at will, and no competitor offers an equivalent annual spectacle.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, news_media_cycle, beneficiary,
    organized, immediate, arbitrage, global).

% Encounters the clock as a shorthand for how dangerous the world is, requiring no technical background. It gains a common reference point for civic conversation; it also absorbs a number that presents judgment calls with the appearance of precision, and it has no lever over how the number is produced.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, attentive_global_public, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, attentive_global_public, payer).

% Build probabilistic, falsifiable assessments of catastrophic and existential risks through forecasting tournaments, structured elicitation, and modeling. Their outputs compete for the same public and policy attention the clock occupies; informal consultation is their only channel into the setting process, and their calls for calibrated alternatives struggle against an established annual ritual.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, quantitative_risk_researchers, excluded,
    moderate, biographical, constrained, global).

% Study how instruments that fuse empirical assessment with normative urgency acquire and keep public trust. They evaluate the clock against epistemic criteria such as calibration and falsifiability and against governance criteria such as accountability and agenda effects, without operating it.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, risk_governance_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_atomic_scientists_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__hybrid_legitimacy_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates dispersed, contested expert judgments about civilizational catastrophe risk into a single annually renewed public signal that synchronizes media attention, policy rhetoric, and philanthropic priority-setting across a domain that otherwise lacks any shared focal point.
% TRANSFER_FUNCTION: Moves public attention and agenda priority toward existential-risk issues; moves epistemic authority and donor support toward the Bulletin of the Atomic Scientists; moves a measure of definitional power over 'how close we are to catastrophe' from distributed experts and affected publics to the setting board.
% ABSENT_VOICES: Quantitative risk modelers producing falsifiable probabilistic assessments are outside the setting room except as informal consultees; communities bearing concrete site-specific risks — downwind populations, frontline climate-exposed regions — have no seat and might object that abstract aggregation dilutes their stakes; advocates for retiring the clock entirely are heard only as external criticism.
% DISAPPEARANCE_RATIONALE: The annual announcement is a fixed node in the science-media calendar. Its overnight removal would scatter attention across competing indices, force policy actors to commission or cite specific analyses rather than gesture at a dial, and leave the Bulletin without its core public asset; the attention architecture around civilizational risk would reorganize within a few annual cycles.
% FOUNDING_PROBLEM: After 1945, the scientists who built the atomic bomb confronted a danger the public and policymakers could not intuit: destruction at civilizational scale arriving without visible cues. The clock was created in 1947 to translate that invisible peril into an immediately legible image — minutes to midnight — that a mass audience could grasp without technical background.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the founders' contemporaneous statements and early Bulletin editorials are documented in the historical record; independent historians of science attest the postwar communicative gap the clock addressed; declassified government records show policymakers tracked the setting as a barometer. No external party attests that the original nuclear-specific formulation remains adequate — historians note the mandate has been progressively widened to climate, biology, and technology, a widening the board itself acknowledges.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio (0.48 at interval end) is high by design under this reading: the annual unveiling, the seconds-to-midnight dramaturgy, and the curated board statement are constitutive of the instrument's legitimacy rather than degenerate residue — roughly half the observable activity maintains the hybrid mystique, half conveys substantive assessment. Extraction (0.52) reflects the attention, authority, and donor flows accruing to the board, grown from a modest founding-era level as media saturation intensified; the referent is the standing clock arrangement assessed by this reading's own lights — the reading endorses the ambiguity and still records the flows it generates. Suppression (0.12) is authored as a raw structural property and left unscaled: nothing coerces participation, and mild conformity pressure on risk discourse is the only suppressive trace. Accessibility collapse is low (0.15) because alternatives — international scientific assessments, forecasting tournaments, structured expert elicitations — remain fully available; resistance (0.25) registers recurring scientific criticism and periodic calls to retire the clock. The temporal series runs on one shared nine-point grid (both metrics authored at every point) and traces a long-wave salience cycle: Cold War crises inflated rents and dramatization, the post-1991 lull deflated both, and the post-2007 multi-risk revival rebuilt them. The oscillation is partly the mechanism itself — crisis dramatization intermittently reinforces public attention, which is what the board collects — so the cycle is documented here rather than treated as noise.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the board's chair, the annual setting is a hard-won synthesis whose ambiguity is its virtue; from the excluded quantitative-modeling seat, the same number is an unfalsifiable incumbent occupying attention that calibrated work cannot buy; from the public seat it is a useful shorthand whose imprecision is invisible; from the policy seat it is a free rhetorical anchor. Same artifact, four different lived arrangements — the engine computes these per-seat classifications from the structural data, and the authored rope claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (board, sponsors, policy community, media, public) derive low directionality; the board sits nearest the beneficiary pole because it both administers and collects, and its identity lock makes the benefit sticky — the organization has become its instrument. The attentive public carries a declared secondary payer position (mild epistemic distortion), pulling it toward symmetric. Quantitative risk researchers declare no formal victim status, consistent with this reading's no-clear-victim-structure delta, but their constrained exit — salience competition against an entrenched annual ritual — places them near the target pole through the exit-modulation term; no directionality override was needed because the derivation separates the two moderate-power agents by exit options alone. Spatial scope is global, which scales effective extraction modestly upward for the paying seats.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy is declared: the founding problem — communicating civilizational-scale danger to a public that cannot intuit it — remains live, and the genealogy interview records live status with external historical corroboration. The classification work here is boundary-keeping rather than obsolescence detection: labeling the clock a pure coordination device would erase the growing asymmetry between the board's collected attention and the diffuse epistemic costs; labeling it a capture instrument would erase the genuine, voluntary, widely used coordination service. The temporal series shows the asymmetry widening with media saturation; the omega variables hold that boundary open for adjudication rather than resolving it by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This story instantiates the hybrid_legitimacy_reading of the doomsday_clock_metric kernel; how would classification shift under the sibling readings — objective_index_reading (the setting as expert synthesis of measurable indicators) or performative_tool_reading (the setting as strategic impact maximization)?',
    'Cross-read the sibling constraint files in the same family; convergence on beneficiary structure and epsilon across readings indicates the kernel''s structure dominates the reading, while divergence localizes the contest to the legitimacy-source question.',
    'Under the objective reading, epsilon should fall toward the coordination floor and the accountability-void costs thin out; under the performative reading, the board''s collected attention and funds become deliberate strategy, raising effective extraction and making agenda management the operative function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the clock-kernel governs classification.').

omega_variable(
    accountability_void_crowding_out,
    'Does the clock''s unfalsifiable-but-authoritative number measurably crowd out calibrated risk assessments, imposing real epistemic costs on the wider risk-assessment ecosystem?',
    'Salience and citation analysis comparing clock coverage with quantitative risk indices around announcement windows; policy-document tracing of which instrument decision-makers actually cite when allocating attention and funds.',
    'Substantial crowding-out raises effective extraction above the authored epsilon and pressures the classification toward a hybrid coordination/extraction shape with quantitative_risk_researchers as the paying seat; negligible crowding-out confirms a near-pure coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_void_crowding_out, empirical, 'Size of the epistemic cost imposed by an authoritative but unscoreable risk number.').

omega_variable(
    ambiguity_load_bearing_vs_posthoc,
    'Is the setting''s deliberate ambiguity genuinely constitutive of the clock''s legitimacy, as this reading holds, or a post-hoc rationalization of board discretion?',
    'Historical comparison of settings against contemporaneous justifications; board-member interviews and internal deliberation records; counterfactual evidence from episodes where the board published detailed methodology — did legitimacy or reach suffer?',
    'If post-hoc, this reading collapses toward the performative sibling and the constraint reclassifies around strategic agenda management; if load-bearing, the ambiguity is functional and the coordination reading stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_load_bearing_vs_posthoc, conceptual, 'Whether the hybrid ambiguity is constitutive or rationalized.').

omega_variable(
    beneficiary_structure_clarity,
    'Is the board a net beneficiary collecting attention and donor flows, or a custodian whose maintenance costs exceed what it collects — the structural delta for this reading declares no clear beneficiary/victim structure?',
    'Attention-flow and financial audit of the Bulletin: staffing, budget, and donation trends attributable to the clock versus the journal and other programs.',
    'If custodian-only, the board''s directionality sits near symmetric and the arrangement reads as pure coordination; if rent-collecting, the board is the capture seat and asymmetric-extraction pressure rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_clarity, empirical, 'Whether the keeping seat nets a gain or merely carries the instrument.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 1947, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1947, 0.2).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t1947, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t1955, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1955, 0.24).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t1955, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t1962, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1962, 0.34).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t1962, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t1975, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1975, 0.29).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t1975, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t1985, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1985, 0.37).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t1985, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t1995, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1995, 0.27).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t1995, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2007, 0.35).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t2007, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t2020, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_tr_t2025, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.28).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t1947, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t1955, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1955, 0.36).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t1955, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t1962, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1962, 0.44).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t1962, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t1975, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1975, 0.39).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t1975, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t1985, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1985, 0.47).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t1985, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t1995, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1995, 0.32).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t1995, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t2007, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2007, 0.41).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t2007, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t2020, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2020, 0.49).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t2020, observed).
narrative_ontology:measurement(ddc_hybrid_legitimacy_be_t2025, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(ddc_hybrid_legitimacy_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(doomsday_clock_metric__hybrid_legitimacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Doomsday Clock' decomposes into three structurally distinct constraints sharing one kernel: this hybrid-legitimacy reading (irreducible expert-normative entanglement; moderate collected flows, no coercive structure), the objective-index reading (pure measurement convention; extraction near the coordination floor), and the performative-tool reading (strategic advocacy instrument; concentrated flows, deliberate agenda management). Each carries its own epsilon, beneficiaries, and classification; they are linked here as a constraint family. Direction of influence: the objective reading supplies the epistemic warrant this hybrid reading borrows; the performative reading explains the strategic behavior this reading tolerates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
