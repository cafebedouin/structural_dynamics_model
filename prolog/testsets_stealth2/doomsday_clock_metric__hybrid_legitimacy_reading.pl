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
 *   human_readable: Doomsday Clock Setting — Hybrid Legitimacy Reading
 *   domain: science_communication/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock expresses proximity to civilizational catastrophe as
 *   minutes-to-midnight, reset annually by the Bulletin of the Atomic
 *   Scientists' Science and Security Board. This file instantiates ONE
 *   reading of that kernel — the hybrid_legitimacy_reading: the setting is a
 *   good-faith entanglement of empirical assessment and normative valuation,
 *   and that deliberate under-determination is the source of the clock's
 *   authority and usability. Per the epsilon-invariance principle, this story
 *   authors epsilon ONLY for this instantiation; the objective-index and
 *   performative-tool instantiations are separate constraints with their own
 *   epsilon, beneficiary structures, and classifications, linked through the
 *   network. Conflating the three would make epsilon observer-dependent. KEY
 *   AGENTS (by structural relationship): - bulletin_science_security_board:
 *   agenda-setting seat (institutional/identity_locked) — renders the annual
 *   judgment, collects deference and salience -
 *   existential_risk_policy_community: beneficiary (organized/mobile) — uses
 *   the signal as shared shorthand for agenda and funding -
 *   concerned_global_publics: beneficiary carrying diffuse costs
 *   (powerless/constrained) — receives the signal, grants unaudited deference
 *   - science_journalists_media: beneficiary (organized/arbitrage) —
 *   amplifies the annual announcement - national_governments: addressee
 *   receiving both barometer value and rhetorical pressure (powerful/mobile)
 *   - methodological_critics: excluded (moderate/mobile) — demand published
 *   criteria, hold no seat - rival_risk_indicators: excluded
 *   (organized/mobile) — compete for the same communicative niche
 *
 * KEY AGENTS:
 *   - bulletin_science_security_board: agenda-setter and collector of deference (institutional power, identity_locked exit)
 *   - existential_risk_policy_community: beneficiary using the signal as shared shorthand (organized power, mobile exit)
 *   - concerned_global_publics: beneficiary bearing diffuse deference costs (powerless, constrained exit)
 *   - science_journalists_media: beneficiary amplifying the annual event (organized power, arbitrage exit)
 *   - national_governments: addressee gaining barometer value, absorbing rhetorical pressure (powerful, mobile exit)
 *   - methodological_critics: excluded voices demanding auditable criteria (moderate power, mobile exit)
 *   - rival_risk_indicators: excluded competitors for the communicative niche (organized power, mobile exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.32).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.13).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.13).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock Setting — Hybrid Legitimacy Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, 'f7075a45-fefd-41a1-bdea-b33499b80cb4').
narrative_ontology:cs_kernel_codification('f7075a45-fefd-41a1-bdea-b33499b80cb4', implicit).
narrative_ontology:cs_authority_grounding('f7075a45-fefd-41a1-bdea-b33499b80cb4', practice).
narrative_ontology:cs_interpretation_layer_present('f7075a45-fefd-41a1-bdea-b33499b80cb4').
narrative_ontology:cs_reading_relation('f7075a45-fefd-41a1-bdea-b33499b80cb4', doomsday_clock_metric__objective_index_reading, forecloses).
narrative_ontology:cs_reading_relation('f7075a45-fefd-41a1-bdea-b33499b80cb4', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('f7075a45-fefd-41a1-bdea-b33499b80cb4', foundational, normative_entanglement_constitutive_of_legitimacy).
narrative_ontology:cs_axiom_status(normative_entanglement_constitutive_of_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('f7075a45-fefd-41a1-bdea-b33499b80cb4', normative_entanglement_constitutive_of_legitimacy, instrumental).
narrative_ontology:cs_axiom('f7075a45-fefd-41a1-bdea-b33499b80cb4', secondary, expert_discretion_over_unformalizable_risk).
narrative_ontology:cs_axiom_status(expert_discretion_over_unformalizable_risk, holdable).
narrative_ontology:cs_axiom_grounding('f7075a45-fefd-41a1-bdea-b33499b80cb4', expert_discretion_over_unformalizable_risk, conventional).
narrative_ontology:cs_reference_frame('f7075a45-fefd-41a1-bdea-b33499b80cb4', hybrid_expert_judgment_convention).
narrative_ontology:cs_drift_state('f7075a45-fefd-41a1-bdea-b33499b80cb4', contemporary_multidomain_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f7075a45-fefd-41a1-bdea-b33499b80cb4', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_policy_community).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, concerned_global_publics).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, science_journalists_media).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, national_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, concerned_global_publics).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, national_governments).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, deliberate_ambiguity_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Meets regularly to weigh nuclear arsenals and doctrine, climate trajectory, biological capability, cyber and AI development, and the state of great-power diplomacy; issues a reasoned public statement and moves the minute hand. Its members serve as the public face of atomic-age scientific conscience, and the act of rendering the judgment is the body's defining function — stepping outside that role, or reducing it to a published formula, would end the body as constituted. Each setting accrues salience, funding leverage, and standing to the board itself.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, beneficiary).

% Researchers, think tanks, and advocates working on catastrophic-risk reduction cite the clock in briefings, funding cases, and curricula as a shared shorthand for urgency. They did not create it and cannot revise it, but their agendas gain a recognizable anchor from it; switching to other indicators is possible at some professional cost in recognizability.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_policy_community, beneficiary,
    organized, generational, mobile, global).

% Encounter the clock through annual coverage as the most widely recognized symbol of how close catastrophe may be. They gain a digestible answer to an otherwise technical question and extend their trust to the board's number without access to the reasoning's inputs. Ignoring the clock costs them little day to day, but no comparably legible alternative carries the same recognition, and they were never consulted on how the number is produced.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, concerned_global_publics, beneficiary,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, concerned_global_publics, payer).

% Receive a dependable annual news event with built-in stakes, imagery, and quotable experts, and amplify the new setting worldwide each cycle. Coverage is voluntary and reversible — outlets profit from the news peg and owe the clock nothing between announcements.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, science_journalists_media, beneficiary,
    organized, immediate, arbitrage, global).

% Are the clock's principal addressees: the setting signals how the expert body grades their stewardship of planetary risk. They gain a compact external barometer of expert alarm and absorb rhetorical pressure whenever the hand moves against them; they can and do disregard the number without material penalty.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, national_governments, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, national_governments, payer).

% Methodologists, philosophers of science, and former advisers who argue the setting should publish its criteria, quantify its weights, or abandon the pretense of precision. They publish critiques and open letters but hold no seat in the deliberation that produces the number, and the board responds by defending the genre rather than by admitting them.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, methodological_critics, excluded,
    moderate, biographical, mobile, global).

% Competing risk dashboards, structured expert surveys, and index projects seeking the same communicative niche. The clock's incumbency crowds the space: funders and editors gravitate to the established symbol, leaving rivals to narrower professional audiences despite comparable or better methodological transparency.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, rival_risk_indicators, excluded,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__hybrid_legitimacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, annually updated, globally recognized focal point that condenses expert assessment of catastrophic risk into a form lay publics, journalists, and policymakers can share — solving the problem that existential risk knowledge is otherwise dispersed across technical literatures no public can integrate.
% TRANSFER_FUNCTION: Moves epistemic deference and public attention toward the Science and Security Board's judgment: publics and policymakers grant trust to an unaudited synthesis, and the board receives agenda-setting power, institutional salience, and funding leverage in return for a legible signal.
% ABSENT_VOICES: Methodological critics and advocates of published criteria are outside the setting process; rival indicator projects are crowded out of the niche the clock occupies; and the global publics who grant the deference have no seat or consultation channel — the number is issued about everyone, by no one answerable to them.
% DISAPPEARANCE_RATIONALE: Media would lose their annual news peg within one cycle; the policy community would scatter across rival indices; and no existing artifact holds equivalent name recognition, so public attention to catastrophic risk would fragment before re-coalescing around a successor symbol.
% FOUNDING_PROBLEM: In 1947 the Manhattan Project scientists who founded the Bulletin faced a public unable to grasp nuclear peril from technical literature; they designed the clock to translate expert dread of nuclear war into a civic image legible without physics.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the atomic scientists and the Bulletin's own archives attest the nuclear-origin account, and science-communication scholarship from outside the benefiting community corroborates the founding design. For the post-2007 multi-domain remit, attestation comes almost entirely from the board and its client community; critics explicitly dispute that the expanded mandate serves the founding problem — that corroboration asymmetry is itself the signal.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).
:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.32: the clock transfers real epistemic deference to an unaudited synthesizer, but the transfer is bounded, revocable in attention, and repaid in a legible signal — there is no identifiable victim class bearing the cost. Suppression 0.13 is a raw, unscaled structural property: nothing is coerced; the small value reflects mild boundary-keeping around the methodology (declining to publish weights, framing critics as misunderstanding the genre). Theater 0.42: the annual unveiling is now a staged media and social-media event, yet the deliberation behind it is real and documented — performativity is the delivery mechanism, not a substitute for function. Accessibility_collapse 0.15: rival indices and raw assessments remain fully available; understanding the clock opens rather than closes exits. Resistance 0.28: steady methodological criticism and occasional calls to retire the clock, but no abolition movement. All three series share one nine-point time grid (t=0..32, mapping to 1993-2025); trajectories are monotonic, driven by domain expansion (climate added 2007, later bio, cyber, AI) widening the board's discretionary scope, and by the announcement's growth into a media spectacle. The board's exit is identity_locked through institutional identity fusion: the body has become its function, and publishing a formula would dissolve the hybrid character its members hold constitutive — which is why the accountability void is stable by construction and why fixing_cost is prohibitive despite the fixer being the beneficiary.
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute different constraints from the same artifact. From the board's seat the arrangement is legitimate self-governance by qualified judgment — the ambiguity is craft. From the publics' seat it is a trusted signal answering an unanswerable question. From the critics' seat it is an unauditable black box whose operators grade their own homework. From the rival indicators' seat it is an incumbent occupying a niche their transparency cannot dislodge. The engine computes per-seat classifications from power, exit, and role; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party declares benefit; the costs — epistemic deference, rhetorical pressure — are diffuse rather than borne by any named victim group. Derived directionality therefore sits near the beneficiary end for all agents, and effective extraction stays modest even as base epsilon rises. The board's identity_locked exit pins its own seat nearest the subsidy end while simultaneously making it the only seat positioned to reform the arrangement: the accountability void persists because its sole potential fixer is its principal beneficiary. Excluded seats (critics, rival indicators) feed no directionality — their grievance is structural absence, not payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested rather than dead: communicating nuclear proximity remains a live need, while the expanded multi-domain remit strains the original form. Holding the classification open as a coordination arrangement with a contested mandate keeps two mislabels apart. The critics' reading — opaque discretion serving insiders — would compute as enforced extraction and push toward snare or tangled_rope; the skeptics' reading — an annual ritual outliving its function — would push toward piton. Both are seat-relative computations the engine owns. This story's structural data (no enforcement machinery, no identifiable victim, broad diffuse benefit, identity-fused administrator) supports a coordination reading whose mandate is contested, not resolved: founding_problem_status=contested crossed with disappearance_verdict=world_rearranges yields no zombie flag, correctly refusing to declare the mandate dead while the world still rearranges around the artifact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Which reading of the doomsday_clock_metric kernel correctly models the setting''s constitution — irreducible entangled judgment (this file), indicator-trackable measurement (objective_index_reading), or impact-maximizing strategy (performative_tool_reading)?',
    'Cross-reading comparison within the constraint family: audit whether historical settings correlate with retrospective quantitative risk assessments (favoring the objective sibling), track advocacy windows and media cycles (favoring the performative sibling), or resist both reductions while tracking the board''s stated reasoning (supporting this reading).',
    'Each reading fixes a different epsilon and beneficiary structure: the objective sibling would make the setting auditable and likely lower extraction; the performative sibling would raise theater toward degraded-ritual profiles; this reading holds ambiguity as functional and keeps the coordination classification available.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which of three sibling readings the clock''s setting instantiates.').

omega_variable(
    accountability_void_benignity,
    'Is the absence of published criteria and weights benign interpretive latitude proper to synthetic judgment, or unaccountable agenda-setting power over a global risk narrative?',
    'Retrospective audit: score each historical setting against ex-post expert consensus on the underlying risks; systematic divergence or lag indicates discretion functioning as error-cover, convergence indicates benign latitude.',
    'If discretion is error-cover, the deference transfer becomes uncompensated extraction and the classification slides toward enforced extraction; if benign, the current profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accountability_void_benignity, empirical, 'Whether the unaudited character of the setting is functional latitude or unchecked power.').

omega_variable(
    legitimacy_source_locus,
    'Does the clock''s authority derive from the setters'' scientific credentials (transferable to any formalized index) or from the hybrid ambiguity itself (dissolving under formalization)?',
    'Counterfactual uptake test: introduce a fully specified, published-criteria shadow index with comparable institutional backing and observe whether public and media attention migrates.',
    'If credentials suffice, formalization reforms the arrangement at low cost and the cost-to-fix drops; if ambiguity is the source, formalization destroys the function and the prohibition on fixing is constitutive rather than incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_locus, conceptual, 'Where the clock''s authority actually resides — credentials or constitutive ambiguity.').

omega_variable(
    public_benefit_realism,
    'Do global publics receive usable, decision-relevant orientation from the clock, or only the feeling of being informed?',
    'Behavioral trace studies linking clock movements to subsequent public opinion, donation, electoral, or policy-attention data; absence of any behavioral signature indicates attention capture without orientation value.',
    'If no behavioral trace exists, the beneficiary set contracts to elites and media, the coordination story weakens, and the deference flow looks increasingly one-way toward the board.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_benefit_realism, empirical, 'Whether public benefit from the clock is substantive or felt-only.').

omega_variable(
    multidomain_scalar_coherence,
    'Can a single minutes-to-midnight scalar coherently aggregate heterogeneous catastrophes (nuclear, climate, biological, cyber, AI), or did domain expansion break the metric''s internal validity?',
    'Decomposition test: elicit board-internal domain weightings and check whether the published number is reproducible from domain sub-assessments under any stable weighting; year-to-year weighting instability indicates incoherence.',
    'If incoherent, the setting''s meaning degrades to board sentiment, strengthening the performative sibling''s case and eroding this reading''s claim that the number tracks a unified judgment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multidomain_scalar_coherence, conceptual, 'Whether one scalar can carry multi-domain risk meaning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(doom_tr_t0, observed).
narrative_ontology:measurement(doom_tr_t4, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(doom_tr_t4, observed).
narrative_ontology:measurement(doom_tr_t8, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 8, 0.26).
narrative_ontology:measurement_basis(doom_tr_t8, observed).
narrative_ontology:measurement(doom_tr_t12, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(doom_tr_t12, observed).
narrative_ontology:measurement(doom_tr_t16, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement_basis(doom_tr_t16, observed).
narrative_ontology:measurement(doom_tr_t20, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(doom_tr_t20, observed).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement_basis(doom_tr_t24, observed).
narrative_ontology:measurement(doom_tr_t28, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 28, 0.39).
narrative_ontology:measurement_basis(doom_tr_t28, observed).
narrative_ontology:measurement(doom_tr_t32, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement_basis(doom_tr_t32, observed).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(doom_be_t0, observed).
narrative_ontology:measurement(doom_be_t4, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement_basis(doom_be_t4, observed).
narrative_ontology:measurement(doom_be_t8, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(doom_be_t8, observed).
narrative_ontology:measurement(doom_be_t12, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement_basis(doom_be_t12, observed).
narrative_ontology:measurement(doom_be_t16, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement_basis(doom_be_t16, observed).
narrative_ontology:measurement(doom_be_t20, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement_basis(doom_be_t20, observed).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 24, 0.3).
narrative_ontology:measurement_basis(doom_be_t24, observed).
narrative_ontology:measurement(doom_be_t28, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 28, 0.31).
narrative_ontology:measurement_basis(doom_be_t28, observed).
narrative_ontology:measurement(doom_be_t32, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 32, 0.32).
narrative_ontology:measurement_basis(doom_be_t32, observed).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(doom_su_t0, observed).
narrative_ontology:measurement(doom_su_t4, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 4, 0.07).
narrative_ontology:measurement_basis(doom_su_t4, observed).
narrative_ontology:measurement(doom_su_t8, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 8, 0.08).
narrative_ontology:measurement_basis(doom_su_t8, observed).
narrative_ontology:measurement(doom_su_t12, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 12, 0.09).
narrative_ontology:measurement_basis(doom_su_t12, observed).
narrative_ontology:measurement(doom_su_t16, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 16, 0.09).
narrative_ontology:measurement_basis(doom_su_t16, observed).
narrative_ontology:measurement(doom_su_t20, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 20, 0.1).
narrative_ontology:measurement_basis(doom_su_t20, observed).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 24, 0.11).
narrative_ontology:measurement_basis(doom_su_t24, observed).
narrative_ontology:measurement(doom_su_t28, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 28, 0.12).
narrative_ontology:measurement_basis(doom_su_t28, observed).
narrative_ontology:measurement(doom_su_t32, doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 32, 0.13).
narrative_ontology:measurement_basis(doom_su_t32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, performative_tool_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'the Doomsday Clock' (kernel doomsday_clock_metric). The label spans three structurally distinct claims: (1) this file, hybrid_legitimacy_reading — the setting is irreducibly entangled judgment whose ambiguity is the legitimacy source; (2) objective_index_reading — the setting tracks measurable risk through indicator synthesis, auditable in principle; (3) performative_tool_reading — the setting is strategically chosen for mobilization effect. Their epsilon values differ: this reading authors moderate-low extraction with diffuse benefit; the objective sibling would author low extraction with an auditability premium; the performative sibling would author high theater and strategic-direction effects. Each story carries its own beneficiaries, metrics, and classification; they are linked here so contamination and cross-reading analysis can traverse the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
