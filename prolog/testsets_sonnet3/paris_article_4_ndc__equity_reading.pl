% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__equity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__equity_reading, []).

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
 *   constraint_id: paris_article_4_ndc__equity_reading
 *   human_readable: Paris Article 4 NDC Regime Read Through Common But Differentiated Responsibilities
 *   domain: international climate governance / treaty law / political economy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Paris Agreement Article 4 NDC
 *   kernel — the equity reading, under which Nationally Determined
 *   Contributions must be interpreted through the Common But Differentiated
 *   Responsibilities and Respective Capabilities (CBDR-RC) principle carried
 *   forward from the 1992 UNFCCC. Under this reading, developed and
 *   developing states face structurally different obligations: developed
 *   states carry binding mitigation floors plus finance and
 *   technology-transfer duties tied to historical emissions responsibility,
 *   while developing states retain broader policy space and their pledges are
 *   read as contingent on delivered support. This reading is in live contest
 *   with two siblings not authored here: a sovereigntist reading (NDCs as
 *   voluntary, self-determined pledges preserving energy sovereignty for all
 *   states equally) and a supranational reading (NDCs as binding commitments
 *   on a uniform ratcheting trajectory with independent international
 *   accountability). Each sibling reading has its own epsilon and its own
 *   beneficiary/victim structure and is authored as a separate constraint
 *   story linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - developing_state_coalitions: coordinate the differentiation reading in negotiating text (organized/constrained)
 *   - vulnerable_island_states: bear the underlying physical risk the equity reading exists to address (powerless/trapped)
 *   - developed_state_treasuries: bear the binding finance and transfer obligations under this reading (institutional/constrained)
 *   - developed_state_energy_sectors: absorb domestic compliance costs of binding mitigation floors (powerful/constrained)
 *   - equity_coalition_negotiators: administer and defend the differentiation language procedurally (organized/constrained)
 *   - supranational_enforcement_advocates: structurally checked by the coalition veto this reading grants (institutional/excluded-in-effect)
 *   - treaty_law_scholars: analytical observers assessing textual defensibility of the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, 0.48).
domain_priors:suppression_score(paris_article_4_ndc__equity_reading, 0.42).
domain_priors:theater_ratio(paris_article_4_ndc__equity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(paris_article_4_ndc__equity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__equity_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__equity_reading, "Paris Article 4 NDC Regime Read Through Common But Differentiated Responsibilities").
narrative_ontology:topic_domain(paris_article_4_ndc__equity_reading, "international climate governance / treaty law / political economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__equity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__equity_reading, '2868e8d5-bfe1-45cd-8d0b-109778d0e2b4').
narrative_ontology:cs_kernel_codification('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', fixed_text).
narrative_ontology:cs_authority_grounding('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', distributed).
narrative_ontology:cs_reading_relation('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', paris_article_4_ndc__supranational_reading, influences).
narrative_ontology:cs_axiom('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', foundational, historical_emissions_ground_differentiated_obligation).
narrative_ontology:cs_axiom_status(historical_emissions_ground_differentiated_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', historical_emissions_ground_differentiated_obligation, deontological).
narrative_ontology:cs_axiom('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', secondary, respective_capabilities_justify_asymmetric_binding_force).
narrative_ontology:cs_axiom_status(respective_capabilities_justify_asymmetric_binding_force, holdable).
narrative_ontology:cs_axiom_grounding('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', respective_capabilities_justify_asymmetric_binding_force, instrumental).
narrative_ontology:cs_reference_frame('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', unfccc_1992_annex_differentiation).
narrative_ontology:cs_drift_state('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', post_paris_self_differentiation_era, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('2868e8d5-bfe1-45cd-8d0b-109778d0e2b4', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__equity_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, developing_state_coalitions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__equity_reading, vulnerable_island_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_treasuries).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, developed_state_energy_sectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(paris_article_4_ndc__equity_reading, domestic_taxpayers_in_donor_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Blocs such as the G77 and Like-Minded Developing Countries invoke CBDR-RC in negotiating rounds to preserve differentiated treatment: their NDCs are read as aspirational and contingent on finance, while developed-country NDCs are read as binding floors. They set negotiating agenda language jointly with allies but cannot unilaterally impose the reading; their leverage is coalition voice inside the UNFCCC process, not enforcement power over any single state.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developing_state_coalitions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__equity_reading, developing_state_coalitions, agenda_setter).

% Small island and low-lying states face existential exposure to warming they did not cause. The equity reading gives them a normative claim on adaptation and loss-and-damage transfers from historically high emitters. They have essentially no exit from the physical exposure and limited individual leverage; their gains from this reading come entirely through coalition solidarity, not independent bargaining power.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, vulnerable_island_states, beneficiary,
    powerless, civilizational, trapped, global).

% Historically high-emitting states are read, under CBDR-RC, as bearing binding mitigation floors plus finance and technology-transfer obligations (climate finance pledges, Green Climate Fund contributions, loss-and-damage fund commitments). They can renegotiate contribution levels domestically and can slow-walk disbursement, but cannot simply exit the differentiation framework without abandoning the treaty regime itself, which carries reputational and diplomatic costs.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_treasuries, payer,
    institutional, biographical, constrained, global).

% Domestic fossil-fuel and heavy-industry sectors in Annex-I-type states absorb the compliance costs of binding mitigation trajectories justified by historical-responsibility reasoning. They can lobby to soften domestic implementing legislation but cannot escape the international differentiation architecture their governments have accepted; their exit is political contestation, not withdrawal from the framework.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, developed_state_energy_sectors, payer,
    powerful, biographical, constrained, national).

% Technical and diplomatic staff who draft and defend the CBDR-RC interpretive language in COP decision texts and NDC guidance. They administer the differentiation reading procedurally — pushing for annexes, differentiated reporting formats, and finance conditionality — and can shift its application through negotiated text, though they cannot compel compliance from any state that resists.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, equity_coalition_negotiators, agenda_setter,
    organized, generational, constrained, global).

% Actors who favor uniform, binding ratchet mechanisms with independent compliance review are structurally checked by the equity reading: differentiation language gives developing-state coalitions an effective veto over harmonized enforcement design, so this constituency's preferred architecture cannot be adopted without their consent being negotiated around.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, supranational_enforcement_advocates, excluded,
    institutional, generational, constrained, global).

% Ultimately fund the climate finance and transfer obligations their governments accept under the equity reading through general revenue or earmarked levies. They have no direct voice in the international negotiation and can only express preference through domestic elections, which operate on a much slower and blunter cycle than treaty commitments.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, domestic_taxpayers_in_donor_states, payer,
    moderate, biographical, trapped, national).

% Assess whether the CBDR-RC differentiation reading is a defensible interpretation of Article 4's text and preamble or a strategic gloss adopted by coalition negotiators. They publish competing accounts of the treaty's ordinary meaning and negotiating history without being party to the outcome.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__equity_reading, treaty_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(paris_article_4_ndc__equity_reading, diffuse).
narrative_ontology:fixing_cost_class(paris_article_4_ndc__equity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: without some differentiation principle, states with vastly different historical emissions, capacity, and development needs would face identical formal obligations, making near-universal treaty participation politically infeasible. CBDR-RC lets low-capacity and low-historical-emission states join a global regime without accepting parity of burden with historically high emitters.
% TRANSFER_FUNCTION: Moves compliance burden, finance, and technology-transfer obligations from historically high-emitting developed states toward developing states and vulnerable states, and moves negotiating leverage over enforcement design from would-be supranational compliance bodies toward developing-state coalitions who can invoke differentiation to resist harmonized binding review.
% ABSENT_VOICES: Future generations in developing states bear the compounding cost if finance transfers under-deliver, but have no seat; domestic constituencies in developing states who might prefer faster unilateral decarbonization over finance-conditional pledges are underrepresented relative to coalition negotiators who speak for the bloc; supranational enforcement advocates are present in the process but structurally out-voted on differentiation questions.
% DISAPPEARANCE_RATIONALE: Developed-state treasuries and energy sectors would argue the world barely rearranges — mitigation trajectories would simply become formally uniform, removing a negotiating friction. Developing-state coalitions and vulnerable island states would argue the world rearranges substantially: without the differentiation reading, finance and technology-transfer claims lose their normative anchor, and lower-capacity states face pressure toward parity obligations they cannot meet, likely triggering non-participation or weakened pledges. The verdict differs by seat, which is why it is authored as contested rather than resolved.
% FOUNDING_PROBLEM: The 1992 UNFCCC and later the 2015 Paris Agreement needed near-universal participation from states with radically unequal historical emissions and radically unequal capacity to pay for mitigation, while acknowledging that industrialized states built their wealth substantially on the emissions now driving the crisis.
% FOUNDING_PROBLEM_CORROBORATION: Developing-state coalitions and allied legal scholars attest the founding problem (unequal historical responsibility and capacity) remains fully live and CBDR-RC remains the correct operative reading. Independent treaty-law academics outside both coalitions note that Paris's shift from the binary Annex I/non-Annex I structure toward self-differentiated NDCs was itself a negotiated retreat from strict CBDR-RC, suggesting the doctrine's textual anchor in the current instrument is weaker than in the original Convention — corroboration from outside the beneficiary coalition is mixed rather than confirmatory.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__equity_reading, contested).
narrative_ontology:founding_problem_status(paris_article_4_ndc__equity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__equity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(paris_article_4_ndc__equity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__equity_reading, 0.48, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__equity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__equity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__equity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 at interval end) rather than high because CBDR-RC's transfer obligations are real but partially unenforced in practice — pledged climate finance has chronically underdelivered against commitments, which caps how much the reading actually extracts from developed-state treasuries relative to its nominal claims. Suppression is moderate (0.42): the reading is maintained through negotiating leverage and coalition solidarity rather than binding legal coercion, since NDCs remain nationally determined and non-compliance carries reputational rather than legal sanction. Theater ratio is meaningful (0.40) because a substantial share of the differentiation apparatus — differentiated reporting formats, transparency framework flexibilities, finance pledge architecture — persists as declaratory structure with weak delivery mechanisms behind it. Accessibility collapse is low-moderate (0.35): states can and do contest the CBDR-RC reading in COP negotiations each cycle, and the sovereigntist and supranational readings remain live alternatives fought over in real time. Resistance is moderate-high (0.55) precisely because developed-state actors actively resist the binding-floor implications of this reading in domestic politics and in negotiating rounds.
 *
 * PERSPECTIVAL GAP:
 *   From developed-state treasury and energy-sector seats, this reading operates as an actively enforced extraction of finance and mitigation burden justified by history rather than present capacity or fault — a tangled rope where real coordination (near-universal treaty participation) is bundled with asymmetric cost. From developing-state coalition and vulnerable-state seats, the identical structure is read as the treaty's only defensible allocation of burden given unequal historical contribution — coordination with fair burden-sharing, not extraction. The engine computes both per-seat readings from the same structural data; this story does not adjudicate which seat is 'right,' only authors the structure both seats are reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Developing-state coalitions and vulnerable island states are structural beneficiaries: the reading gives them a normative and procedural claim on finance, technology transfer, and differentiated review that they would lack under a formally symmetric regime, so their directionality sits toward the beneficiary end despite their generally lower raw power. Developed-state treasuries and energy sectors are targets: the reading assigns them binding floors and outbound transfer obligations tied to historical responsibility, pushing their directionality toward the target end even though their raw institutional and economic power is high — this is exactly the case where power and directionality diverge, because the reading's whole point is to impose asymmetric obligation on the historically high-power actor. Equity coalition negotiators occupy a dual position: they administer the reading (agenda-setting) on behalf of a beneficiary bloc without personally capturing transfers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unequal historical responsibility and unequal capacity to decarbonize — remains substantively live for many low-capacity states, which argues against reflexively classifying this as pure mandatrophy. But the specific mechanism (Paris's shift to self-differentiated, bottom-up NDCs from the Convention's binary Annex I/non-Annex I structure) has already partially decoupled the CBDR-RC textual anchor from binding legal force, meaning the equity reading persists more through negotiating leverage and normative appeal than through a hardened legal instrument. This supports the tangled_rope claim over either mountain (it is not natural law) or pure snare (there is a genuine coordination function enabling near-universal participation) — but the moderate theater_ratio and contested founding-problem corroboration flag that some of the apparatus may be drifting toward declaratory performance as delivery lags pledges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cbdr_textual_anchor_strength,
    'Does the equity reading''s differentiation claim rest on a strong textual anchor in Paris Article 4 itself, or primarily on the 1992 UNFCCC''s preamble and general principles, with Paris''s self-differentiated NDC structure representing a partial retreat from binding differentiation?',
    'Close textual and negotiating-history analysis of Article 4 drafting records (2015 Paris negotiations) compared against UNFCCC Article 3/4 CBDR-RC language, plus tracking of subsequent COP decision texts that either reaffirm or dilute differentiated treatment.',
    'A weak textual anchor in Paris itself would suggest the equity reading is increasingly a negotiating-coalition construct layered onto a text that no longer clearly requires it, strengthening the case that this reading''s persistence depends more on coalition leverage than on treaty law — pushing the classification toward a more contested, less legally settled tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cbdr_textual_anchor_strength, conceptual, 'Whether CBDR-RC differentiation is strongly anchored in Paris Article 4''s actual text or is a legacy principle imported from the 1992 Convention onto a structurally more symmetric instrument.').

omega_variable(
    finance_pledge_delivery_gap,
    'What share of the climate finance and technology-transfer obligations the equity reading assigns to developed states is actually delivered, versus pledged and not delivered?',
    'Track disbursement data against pledges (e.g., the $100bn/year commitment and its successors) across multiple reporting cycles; compare pledged versus disbursed figures from independent monitoring bodies (OECD, Oxfam finance tracking, UNFCCC finance reports).',
    'A large and persistent delivery gap would support reading a substantial share of the constraint''s coordination function as theater — the differentiation architecture exists on paper while extraction from developing states'' policy space (via conditionality, delay) continues without the compensating transfer materializing, pushing the classification toward snare-adjacent territory for the specific finance-transfer sub-claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finance_pledge_delivery_gap, empirical, 'Whether the equity reading''s transfer obligations are substantially delivered or substantially theatrical.').

omega_variable(
    coalition_veto_representativeness,
    'Do the negotiators who invoke CBDR-RC on behalf of the developing-state coalition adequately represent the interests of the most vulnerable states within that coalition, or do larger, more industrially advanced developing states (which also benefit from differentiation) capture the coalition''s negotiating position?',
    'Compare negotiating positions and outcomes across coalition sub-groups (e.g., AOSIS/small island states versus large emerging economies within G77) to see whether differentiation benefits are distributed proportionally to vulnerability or captured by higher-capacity developing states.',
    'If capture by higher-capacity developing states is substantial, the beneficiary group as authored (developing_state_coalitions) should be split into a genuinely vulnerable sub-group and a higher-capacity sub-group whose interests partially diverge — this would be a decomposition signal under the epsilon-invariance principle, not a metric adjustment to this single story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_veto_representativeness, conceptual, 'Whether the developing-state beneficiary coalition is internally homogeneous or conceals a capture dynamic among its own members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__equity_reading, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pari_tr_t2015, paris_article_4_ndc__equity_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(pari_tr_t2019, paris_article_4_ndc__equity_reading, theater_ratio, 2019, 0.34).
narrative_ontology:measurement(pari_tr_t2023, paris_article_4_ndc__equity_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(pari_tr_t2027, paris_article_4_ndc__equity_reading, theater_ratio, 2027, 0.4).
narrative_ontology:measurement(pari_tr_t2031, paris_article_4_ndc__equity_reading, theater_ratio, 2031, 0.4).
narrative_ontology:measurement(pari_tr_t2035, paris_article_4_ndc__equity_reading, theater_ratio, 2035, 0.4).

% Extraction over time
narrative_ontology:measurement(pari_be_t2015, paris_article_4_ndc__equity_reading, base_extractiveness, 2015, 0.32).
narrative_ontology:measurement(pari_be_t2019, paris_article_4_ndc__equity_reading, base_extractiveness, 2019, 0.37).
narrative_ontology:measurement(pari_be_t2023, paris_article_4_ndc__equity_reading, base_extractiveness, 2023, 0.44).
narrative_ontology:measurement(pari_be_t2027, paris_article_4_ndc__equity_reading, base_extractiveness, 2027, 0.47).
narrative_ontology:measurement(pari_be_t2031, paris_article_4_ndc__equity_reading, base_extractiveness, 2031, 0.48).
narrative_ontology:measurement(pari_be_t2035, paris_article_4_ndc__equity_reading, base_extractiveness, 2035, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(pari_su_t2015, paris_article_4_ndc__equity_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(pari_su_t2019, paris_article_4_ndc__equity_reading, suppression_requirement, 2019, 0.34).
narrative_ontology:measurement(pari_su_t2023, paris_article_4_ndc__equity_reading, suppression_requirement, 2023, 0.38).
narrative_ontology:measurement(pari_su_t2027, paris_article_4_ndc__equity_reading, suppression_requirement, 2027, 0.4).
narrative_ontology:measurement(pari_su_t2031, paris_article_4_ndc__equity_reading, suppression_requirement, 2031, 0.42).
narrative_ontology:measurement(pari_su_t2035, paris_article_4_ndc__equity_reading, suppression_requirement, 2035, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__equity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__equity_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__equity_reading, paris_article_4_ndc__supranational_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the paris_article_4_ndc kernel, decomposed per the epsilon-invariance principle because the natural-language label 'NDCs under Article 4' covers structurally distinct claims with different epsilon values: the equity_reading (this file, moderate epsilon ~0.48, asymmetric distribution favoring developing states), the sovereigntist_reading (expected lower epsilon, symmetric voluntarism), and the supranational_reading (expected higher epsilon for developing states under uniform binding ratchet, since it removes the differentiation this reading grants). The three readings are mutually exclusive as governing interpretive frames but coexist as live positions held by different negotiating blocs within the same treaty regime.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
