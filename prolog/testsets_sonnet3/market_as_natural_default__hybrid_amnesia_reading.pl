% ============================================================================
% CONSTRAINT STORY: market_as_natural_default__hybrid_amnesia_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_market_as_natural_default__hybrid_amnesia_reading, []).

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
 *   constraint_id: market_as_natural_default__hybrid_amnesia_reading
 *   human_readable: Market-as-Natural-Default (Hybrid Amnesia Reading)
 *   domain: political economy / ideology studies / economic history
 *
 * SUMMARY:
 *   This story instantiates the hybrid_amnesia_reading of the
 *   market_as_natural_default kernel: a two-stage account in which the
 *   1930s-1970s decline of comparative-institutional economic memory was
 *   genuine forgetting, not designed closure, but the resulting vacated
 *   terrain was subsequently discovered and actively defended by organized
 *   beneficiaries from roughly 1980 onward. The measured extractiveness
 *   therefore starts low (0.20, reflecting the innocent-forgetting phase
 *   where no one was extracting from the amnesia because no one had yet
 *   organized around it) and rises to 0.45 by the interval's end (reflecting
 *   the defensive-rationalization phase where incumbent intermediaries, the
 *   professoriate, and lobbying apparatus actively weaponize the inherited
 *   default). This is distinct from the lapsed_alternative_reading (which
 *   holds the whole history is forgetting, with no active beneficiary defense
 *   — a story that would show flat, low extraction throughout) and from the
 *   beneficiary_maintained_reading (which holds the closure was actively
 *   engineered from the start by beneficiaries — a story that would show
 *   extraction elevated from time_point 0). The hybrid reading's ε referent
 *   throughout is the standing arrangement of market-default framing as it
 *   actually operated in each phase, assessed from this reading's own lights:
 *   genuinely benign in phase one, genuinely extractive in phase two.
 *
 * KEY AGENTS:
 *   - incumbent_market_intermediaries: institutional beneficiary that inherited (did not create) the amnesia and now defends it
 *   - market_economics_professoriate: organized beneficiary reproducing the default through pedagogy and career incentives
 *   - financial_sector_lobbying_apparatus: agenda-setting institutional actor doing the active defensive work from the 1980s forward
 *   - cooperative_and_mutualist_movements: powerless, trapped payer bearing the cost of an erased institutional vocabulary
 *   - post_industrial_displaced_workers: powerless, trapped payer whose policy options were foreclosed by the narrowed imagination
 *   - policy_publics_lacking_institutional_memory: diffuse payer inheriting the frame without ever encountering its contested history
 *   - economic_historians_of_institutional_alternatives: excluded analytical voice whose corroborating scholarship rarely reaches policy discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(market_as_natural_default__hybrid_amnesia_reading, 0.38).
domain_priors:suppression_score(market_as_natural_default__hybrid_amnesia_reading, 0.42).
domain_priors:theater_ratio(market_as_natural_default__hybrid_amnesia_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(market_as_natural_default__hybrid_amnesia_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(market_as_natural_default__hybrid_amnesia_reading, tangled_rope).
narrative_ontology:human_readable(market_as_natural_default__hybrid_amnesia_reading, "Market-as-Natural-Default (Hybrid Amnesia Reading)").
narrative_ontology:topic_domain(market_as_natural_default__hybrid_amnesia_reading, "political economy / ideology studies / economic history").

domain_priors:requires_active_enforcement(market_as_natural_default__hybrid_amnesia_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(market_as_natural_default__hybrid_amnesia_reading, '45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a').
narrative_ontology:cs_kernel_codification('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', implicit).
narrative_ontology:cs_authority_grounding('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', extraction).
narrative_ontology:cs_interpretation_layer_present('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a').
narrative_ontology:cs_reading_relation('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', market_as_natural_default__lapsed_alternative_reading, influences).
narrative_ontology:cs_reading_relation('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', market_as_natural_default__beneficiary_maintained_reading, influences).
narrative_ontology:cs_axiom('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', foundational, amnesia_precedes_and_enables_capture).
narrative_ontology:cs_axiom_status(amnesia_precedes_and_enables_capture, holdable).
narrative_ontology:cs_axiom_grounding('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', amnesia_precedes_and_enables_capture, empirically_contingent).
narrative_ontology:cs_axiom('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', secondary, beneficiary_intent_is_inherited_not_originary).
narrative_ontology:cs_axiom_status(beneficiary_intent_is_inherited_not_originary, holdable).
narrative_ontology:cs_axiom_grounding('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', beneficiary_intent_is_inherited_not_originary, empirically_contingent).
narrative_ontology:cs_reference_frame('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', mixed_economy_institutional_pluralism_midcentury).
narrative_ontology:cs_drift_state('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', contemporary_post_2008_policy_debate, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('45a678c3-88e4-4ab3-9a0e-7c6fa3c80c4a', '').
narrative_ontology:cs_kernel_id(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate).
narrative_ontology:constraint_beneficiary(market_as_natural_default__hybrid_amnesia_reading, financial_sector_lobbying_apparatus).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, cooperative_and_mutualist_movements).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, post_industrial_displaced_workers).
narrative_ontology:constraint_victim(market_as_natural_default__hybrid_amnesia_reading, policy_publics_lacking_institutional_memory).
narrative_ontology:constraint_vindicates(market_as_natural_default__hybrid_amnesia_reading, market_allocation_as_default_mechanism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Investment banks, brokerages, and large market-making firms did not create the original forgetting of mid-century mixed-economy and mutualist arrangements, but from the 1980s onward they discovered that the vacated cultural memory could be filled with a naturalized market default that served their position. They fund think tanks, endow business-school chairs, and sponsor curricula that treat market allocation as the default state of affairs rather than one institutional choice among historically available others. They did not do the initial forgetting; they inherited it and built defenses around it.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries, agenda_setter).

% Departments and journals whose prestige, funding, and paradigm depend on market-centric models teach the naturalized default forward, with declining institutional memory of the alternatives (cooperative federations, municipal ownership, sectoral planning boards) that were live policy options through the 1950s-60s. Individual economists face real career costs for reviving comparative-institutional frameworks; the discipline's incentive structure, not any single actor's malice, reproduces the amnesia.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, market_economics_professoriate, beneficiary,
    organized, generational, constrained, national).

% From roughly 1980 forward, organized lobbying and litigation campaigns actively defended market-default policy premises against periodic revival attempts (e.g., post-2008 calls for public banking, post-2020 industrial-policy proposals). This is the defensive-rationalization stage: the apparatus does not need to have caused the original forgetting to have every incentive to entrench it once discovered, and it deploys real political capital to keep the closure in place.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, financial_sector_lobbying_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Successor organizations to mid-century cooperative banking, mutual insurance, and municipal-ownership movements find that the institutional vocabulary and legal templates that once made their models legible to policymakers have atrophied. Reviving a cooperative alternative now requires re-explaining a framework treated as historically settled rather than contested, a burden the original movements never had to bear.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, cooperative_and_mutualist_movements, payer,
    powerless, generational, trapped, regional).

% Workers displaced by market-mediated deindustrialization and restructuring confront policy debate in which non-market or mixed responses (sectoral wage boards, public employment guarantees, worker ownership conversion) are treated as fringe rather than as the mainstream mid-century options they once were. They bear the material cost of a foreclosed policy imagination they did not participate in foreclosing.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, post_industrial_displaced_workers, payer,
    powerless, biographical, trapped, national).

% The general public inherits a policy discourse in which market allocation appears as the default backdrop against which all interventions are judged as deviations, rather than as itself one institutional arrangement that had to be actively constructed and is now actively maintained. Most members of the public never encounter the historical fact that this was contested terrain.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, policy_publics_lacking_institutional_memory, payer,
    powerless, generational, constrained, national).

% Scholars who document the mid-century range of institutional alternatives (Karl Polanyi's successors, comparative-institutionalist economic historians) publish work establishing that the market default was not inevitable and that its current defense is doing real ideological work — but their findings circulate in specialist venues and rarely reach the policy discourse their work would reframe.
narrative_ontology:constraint_stakeholder(market_as_natural_default__hybrid_amnesia_reading, economic_historians_of_institutional_alternatives, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(market_as_natural_default__hybrid_amnesia_reading, incumbent_market_intermediaries).
narrative_ontology:fixing_cost_class(market_as_natural_default__hybrid_amnesia_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The market-default frame does provide real coordination value where it functions honestly: a shared baseline expectation that reduces negotiation costs across a large, plural economy, letting disparate actors coordinate around price signals without re-litigating institutional design at every transaction.
% TRANSFER_FUNCTION: In its current, defensively-rationalized form the frame moves policy attention, legitimacy, and resulting resource allocation away from cooperative, mutualist, and mixed-economy alternatives and toward incumbent market intermediaries and the institutions (academic, financial, media) that certify market allocation as default rather than as choice.
% ABSENT_VOICES: Successor cooperative and mutualist organizations, and the economic historians who document the foreclosed alternatives, would object that the 'naturalness' framing erases a real historical contest — but neither group holds the institutional platform (endowed chairs, lobbying budgets, media access) that the beneficiary institutions hold, so their objection rarely enters mainstream policy debate.
% DISAPPEARANCE_RATIONALE: If the naturalized-default framing vanished overnight, incumbent beneficiaries would lose a legitimacy shield and would have to defend market allocation on comparative-institutional merits rather than by default status — a real rearrangement for them. But policy publics might see little immediate change, since the underlying market institutions themselves would persist even if their justificatory framing were exposed as historically contingent; the contest is over whether the framing is load-bearing or merely decorative on top of institutions that would persist anyway.
% FOUNDING_PROBLEM: There was no single founding act for this reading: the mid-century decline of comparative-institutional economic education and public memory of cooperative/mixed-economy alternatives (1930s-1970s) was not designed to benefit anyone in particular — it reflects Cold War-era professionalization of economics around formal market models, postwar prosperity reducing salience of institutional alternatives, and pedagogical drift. Only later (1980s onward) did organized beneficiaries recognize the vacated terrain and begin actively defending market-default framing against periodic revival attempts.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of institutional alternatives (outside the beneficiary set) corroborate that mid-century policy discourse genuinely included a wider range of live institutional options, and that the narrowing was not originally designed as an extraction strategy. However, the same scholars, plus antitrust and financial-regulation historians, corroborate that from the 1980s forward the defense of the market-default frame became demonstrably organized and self-interested. No account of the current defensive phase is offered by a source outside the financial-sector lobbying apparatus and the professoriate that benefits from it; the genealogy is corroborated for stage one but is asserted mainly by the beneficiaries themselves for stage two.
narrative_ontology:disappearance_verdict(market_as_natural_default__hybrid_amnesia_reading, contested).
narrative_ontology:founding_problem_status(market_as_natural_default__hybrid_amnesia_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(market_as_natural_default__hybrid_amnesia_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(market_as_natural_default__hybrid_amnesia_reading, 'none', 1).
narrative_ontology:epsilon_provenance(market_as_natural_default__hybrid_amnesia_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(market_as_natural_default__hybrid_amnesia_reading_tests).
:- end_tests(market_as_natural_default__hybrid_amnesia_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The theater_ratio rises sharply between time_point 30 and 45 (0.22 to 0.38), marking the transition from genuine pedagogical drift to defensive rationalization: after this point, an increasing share of market-default advocacy is performative reassurance (op-eds, think-tank white papers, textbook framing devices) rather than substantive economic argument, because the substantive argument for treating markets as default rather than choice was never really made and does not need to be remade once the frame is entrenched. Suppression_requirement is tracked because the story's central claim is that active enforcement (lobbying, funding capture, curriculum gatekeeping) had to be BUILT UP over the interval — it was near-zero when the amnesia was passive and rises to meaningfully entrench the closure once beneficiaries recognized what they had inherited. All three metrics share the same seven-point time grid (0, 15, 30, 45, 60, 75, 90) so no metric's value is silently substituted from a story-level scalar at an earlier point.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent market intermediaries, the professoriate, and the lobbying apparatus sit near the full-beneficiary end: they collect legitimacy, funding, and policy deference from the naturalized frame without having to justify market allocation on comparative merits. Cooperative movements, displaced workers, and policy publics sit near the full-target end: they bear the cost of a foreclosed alternative-institutional vocabulary and inherit a policy discourse that treats their preferred arrangements as historically settled losers rather than live options. The professoriate is powerful but constrained in exit — individual economists cannot easily leave the paradigm without career cost, which is why its exit_options is constrained rather than arbitrage despite organized power.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mandatrophy mislabeling in both directions. Reading it as pure innocent forgetting (the lapsed_alternative sibling) would understate the real, organized extraction happening in the 1980s-present defensive phase — mislabeling active capture as historical accident lets beneficiaries off the hook for choices they are currently making. Reading it as pure engineered closure from the start (the beneficiary_maintained sibling) would overstate coordination and intent in the 1930s-1970s phase, when the evidence better supports genuine pedagogical and professional drift with no organized beneficiary yet in place. The hybrid reading's tangled_rope classification captures both: a real coordination function (shared allocative baseline, reduced negotiation cost) coexists with organized, actively-enforced extraction that developed only after beneficiaries discovered the vacated terrain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    phase_transition_dating,
    'Precisely when did the transition from genuine institutional forgetting to organized beneficiary defense occur, and is a single transition point even the right model, or did different sectors (banking, academia, media) capture the frame at different times?',
    'Archival research tracing think-tank founding dates, curriculum reform timelines, and lobbying expenditure records against the decline of comparative-institutional economics coursework; a genuinely staggered transition across sectors would argue for a more granular multi-phase model than the single-transition hybrid used here.',
    'If the transition was staggered rather than a clean 1980 inflection, the theater_ratio and suppression_requirement trajectories authored here (sharp rise at time_point 30-45) may misdate individual sectors even if the aggregate trend is correct; this would refine but not overturn the hybrid classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phase_transition_dating, empirical, 'Uncertainty about the precise timing and sectoral staggering of the forgetting-to-capture transition.').

omega_variable(
    counterfactual_naturalness_test,
    'Would a comparable naturalization dynamic have occurred even without any beneficiary capture — i.e., is some degree of institutional forgetting about superseded policy alternatives simply what happens to any settled arrangement over two generations, regardless of beneficiary intent?',
    'Comparative case study: examine whether other settled policy defaults with no organized beneficiary lobby (e.g., certain public-health protocols, or now-obscure superseded regulatory regimes with no current rent-collecting constituency) show similar amnesia curves without the corresponding extraction rise.',
    'If similar amnesia curves appear even absent beneficiary capture, part of what this reading attributes to active extraction may be a baseline feature of institutional memory decay, meaning the true beneficiary-driven extraction component is smaller than the authored 0.20-to-0.45 delta suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_naturalness_test, conceptual, 'Whether the extraction rise is fully attributable to beneficiary capture or partly a generic institutional-memory-decay baseline.').

omega_variable(
    corroboration_asymmetry,
    'The founding-problem corroboration is strong for stage one (independent economic historians confirm genuine forgetting) but weak for stage two (the defensive-rationalization account rests mainly on inference from lobbying/funding patterns rather than direct testimony from outside the beneficiary set) — how much should this asymmetry discount confidence in the stage-two extraction estimate?',
    'Seek testimony or documentary evidence (internal think-tank memos, lobbying disclosure filings, whistleblower accounts) from within or adjacent to the beneficiary institutions themselves that would corroborate or disconfirm intentional defensive rationalization as opposed to good-faith advocacy.',
    'If stronger outside corroboration emerges confirming intentional, coordinated defense, confidence in the 0.45 terminal extractiveness value increases; if no such corroboration is found and the defensive activity looks more like uncoordinated parallel self-interest, the classification might drift toward a milder rope-with-drift rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corroboration_asymmetry, empirical, 'Asymmetric evidentiary support between the two stages of the hybrid account.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(market_as_natural_default__hybrid_amnesia_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mark_tr_t0, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(mark_tr_t0, observed).
narrative_ontology:measurement(mark_tr_t15, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement_basis(mark_tr_t15, observed).
narrative_ontology:measurement(mark_tr_t30, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(mark_tr_t30, observed).
narrative_ontology:measurement(mark_tr_t45, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 45, 0.38).
narrative_ontology:measurement_basis(mark_tr_t45, observed).
narrative_ontology:measurement(mark_tr_t60, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 60, 0.47).
narrative_ontology:measurement_basis(mark_tr_t60, observed).
narrative_ontology:measurement(mark_tr_t75, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 75, 0.52).
narrative_ontology:measurement_basis(mark_tr_t75, observed).
narrative_ontology:measurement(mark_tr_t90, market_as_natural_default__hybrid_amnesia_reading, theater_ratio, 90, 0.55).
narrative_ontology:measurement_basis(mark_tr_t90, observed).

% Extraction over time
narrative_ontology:measurement(mark_be_t0, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(mark_be_t0, observed).
narrative_ontology:measurement(mark_be_t15, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 15, 0.21).
narrative_ontology:measurement_basis(mark_be_t15, observed).
narrative_ontology:measurement(mark_be_t30, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement_basis(mark_be_t30, observed).
narrative_ontology:measurement(mark_be_t45, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 45, 0.29).
narrative_ontology:measurement_basis(mark_be_t45, observed).
narrative_ontology:measurement(mark_be_t60, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement_basis(mark_be_t60, observed).
narrative_ontology:measurement(mark_be_t75, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 75, 0.41).
narrative_ontology:measurement_basis(mark_be_t75, observed).
narrative_ontology:measurement(mark_be_t90, market_as_natural_default__hybrid_amnesia_reading, base_extractiveness, 90, 0.45).
narrative_ontology:measurement_basis(mark_be_t90, observed).

% Suppression requirement over time
narrative_ontology:measurement(mark_su_t0, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement_basis(mark_su_t0, observed).
narrative_ontology:measurement(mark_su_t15, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 15, 0.14).
narrative_ontology:measurement_basis(mark_su_t15, observed).
narrative_ontology:measurement(mark_su_t30, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(mark_su_t30, observed).
narrative_ontology:measurement(mark_su_t45, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 45, 0.28).
narrative_ontology:measurement_basis(mark_su_t45, observed).
narrative_ontology:measurement(mark_su_t60, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement_basis(mark_su_t60, observed).
narrative_ontology:measurement(mark_su_t75, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 75, 0.4).
narrative_ontology:measurement_basis(mark_su_t75, observed).
narrative_ontology:measurement(mark_su_t90, market_as_natural_default__hybrid_amnesia_reading, suppression_requirement, 90, 0.42).
narrative_ontology:measurement_basis(mark_su_t90, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(market_as_natural_default__hybrid_amnesia_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(market_as_natural_default__hybrid_amnesia_reading, 0.1).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__lapsed_alternative_reading).
narrative_ontology:affects_constraint(market_as_natural_default__hybrid_amnesia_reading, market_as_natural_default__beneficiary_maintained_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the market_as_natural_default kernel, decomposed per the epsilon-invariance principle because the three readings assign structurally different extractiveness trajectories to what a single natural-language label ('market naturalization') would otherwise conflate. lapsed_alternative_reading holds a flat, low ε throughout (pure forgetting, no organized beneficiary). beneficiary_maintained_reading holds an elevated ε from time_point 0 (active engineered closure from the outset). This hybrid_amnesia_reading holds a rising ε from 0.20 to 0.45, structurally distinct from both because it is the only reading with a genuine phase transition — an early period with no active-enforcement requirement followed by a later period where enforcement is actively built up. All three stories are linked bidirectionally via affects_constraints; each carries its own claimed_type, its own beneficiary/victim structure, and its own metric profile, per the authoring rule that a natural-language concept covering multiple structurally distinct claims must be decomposed rather than measured on a hidden observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
