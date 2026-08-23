% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__musk_cult_believer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__musk_cult_believer, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: valuation_legitimacy__musk_cult_believer
 *   human_readable: Valuation Legitimacy from Musk's Impossible-Goal Track Record
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'Musk cult believer' reading of the
 *   valuation_legitimacy kernel: the claim that Musk's track record of
 *   achieving 'impossible' goals (reusable rockets, EV mass adoption,
 *   satellite internet profitability) legitimates valuations that dwarf
 *   traditional DCF outputs. The constraint operates as a tangled rope — it
 *   genuinely coordinates capital toward high-variance technological bets
 *   that traditional frameworks undervalue (the coordination function), while
 *   simultaneously extracting from skeptics, short-sellers, and minority
 *   shareholders through governance-free narrative premium (the extraction
 *   function). The 1B performance shares vesting on Mars colony milestones
 *   epitomize the hybrid: a coordination mechanism aligning founder
 *   incentives with civilizational goals, but also a governance-free transfer
 *   of upside from minority holders to Musk personally.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, 0.65).
domain_priors:suppression_score(valuation_legitimacy__musk_cult_believer, 0.45).
domain_priors:theater_ratio(valuation_legitimacy__musk_cult_believer, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, extractiveness, 0.65).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__musk_cult_believer, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__musk_cult_believer, "Valuation Legitimacy from Musk's Impossible-Goal Track Record").
narrative_ontology:topic_domain(valuation_legitimacy__musk_cult_believer, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__musk_cult_believer).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__musk_cult_believer, '4de676fa-9997-4077-8ae1-fa19202dbc78').
narrative_ontology:cs_kernel_codification('4de676fa-9997-4077-8ae1-fa19202dbc78', implicit).
narrative_ontology:cs_authority_grounding('4de676fa-9997-4077-8ae1-fa19202dbc78', lineage).
narrative_ontology:cs_interpretation_layer_present('4de676fa-9997-4077-8ae1-fa19202dbc78').
narrative_ontology:cs_reading_relation('4de676fa-9997-4077-8ae1-fa19202dbc78', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('4de676fa-9997-4077-8ae1-fa19202dbc78', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('4de676fa-9997-4077-8ae1-fa19202dbc78', valuation_legitimacy__governance_skeptic, influences).
narrative_ontology:cs_axiom('4de676fa-9997-4077-8ae1-fa19202dbc78', foundational, execution_history_legitimates_narrative_valuation).
narrative_ontology:cs_axiom_status(execution_history_legitimates_narrative_valuation, holdable).
narrative_ontology:cs_axiom_grounding('4de676fa-9997-4077-8ae1-fa19202dbc78', execution_history_legitimates_narrative_valuation, empirically_contingent).
narrative_ontology:cs_axiom('4de676fa-9997-4077-8ae1-fa19202dbc78', foundational, financial_metrics_are_lagging_indicators_for_paradigm_shifts).
narrative_ontology:cs_axiom_status(financial_metrics_are_lagging_indicators_for_paradigm_shifts, holdable).
narrative_ontology:cs_axiom_grounding('4de676fa-9997-4077-8ae1-fa19202dbc78', financial_metrics_are_lagging_indicators_for_paradigm_shifts, empirically_contingent).
narrative_ontology:cs_axiom('4de676fa-9997-4077-8ae1-fa19202dbc78', secondary, governance_concerns_irrelevant_when_founder_uniquely_capable).
narrative_ontology:cs_axiom_status(governance_concerns_irrelevant_when_founder_uniquely_capable, holdable).
narrative_ontology:cs_axiom_grounding('4de676fa-9997-4077-8ae1-fa19202dbc78', governance_concerns_irrelevant_when_founder_uniquely_capable, deontological).
narrative_ontology:cs_reference_frame('4de676fa-9997-4077-8ae1-fa19202dbc78', pre_musk_valuation_paradigm).
narrative_ontology:cs_drift_state('4de676fa-9997-4077-8ae1-fa19202dbc78', post_2020_narrative_peak, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4de676fa-9997-4077-8ae1-fa19202dbc78', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__musk_cult_believer, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_cult_believers).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_founded_entities).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, short_sellers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, valuation_skeptics).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, minority_shareholders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, founder_led_innovation_premium).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, execution_history_trumps_dcf).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, vertical_integration_creates_compounding_optionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retail and institutional investors who structure their portfolio identity around Musk's track record. They interpret bankruptcy warnings as negotiating tactics, Mars colonization timelines as credible commitments, and governance concerns as irrelevant. Their exit is identity-locked — selling would mean admitting the narrative was wrong. They benefit from share price appreciation driven by narrative momentum.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_cult_believers, beneficiary,
    organized, biographical, identity_locked, global).

% Tesla, SpaceX, X, xAI, Neuralink, The Boring Company — the constellation of Musk-led enterprises. They set the valuation narrative through product announcements, timeline projections, and capital allocation decisions. Musk's 82.4% voting control across entities lets him enforce the narrative. They benefit from low-cost capital raised on narrative-driven valuations.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_founded_entities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, musk_founded_entities, beneficiary).

% Hedge funds and quantitative strategies betting against Musk-led companies based on fundamental metrics. They have lost billions cumulatively. Their exit is constrained — they can cover positions but face unlimited upside risk on narrative-driven squeezes. They pay the extraction through mark-to-market losses and borrowing costs.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, short_sellers, payer,
    powerful, immediate, constrained, global).

% Analysts, journalists, and academics who apply traditional valuation frameworks (DCF, comparables) and conclude Musk-led companies are overvalued. They pay through reputational costs when proven wrong, opportunity cost from avoiding winning trades, and credibility erosion with clients who see them as 'missing the story.' They can exit by changing methodology or coverage universe.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, valuation_skeptics, payer,
    moderate, biographical, mobile, global).

% Institutional and retail holders who own equity but have no governance voice due to dual-class structures and Musk's voting control. They bear downside risk if narrative collapses but cannot influence strategy. Their exit is constrained — selling locks in losses if narrative holds, holding exposes them to governance-free downside. They pay through dilution from performance-share grants and asymmetric risk allocation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, minority_shareholders, payer,
    powerless, biographical, constrained, global).

% Regulators, index providers, academic finance researchers, and market structure analysts who study whether narrative-driven valuation represents a durable paradigm shift or a bubble. They neither collect nor pay directly but their frameworks shape the legitimacy of the constraint.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, capital_markets_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation toward high-variance, long-horizon technological bets that traditional DCF frameworks systematically undervalue — reusable rockets, satellite internet constellations, full self-driving, brain-computer interfaces, Mars colonization. The narrative creates a permission structure for capital to flow to projects that look unjustifiable on near-term cash flows.
% TRANSFER_FUNCTION: Moves capital from skeptics (short-sellers covering, fundamental investors avoiding) to believers (buying and holding) and to Musk-founded entities (raising at narrative premia). The performance-share grants (1B shares vesting on Mars colony milestones) transfer upside from minority shareholders to Musk personally. Governance extraction transfers control from shareholders to founder.
% ABSENT_VOICES: Future generations who would bear costs if Mars colony fails; employees at Musk companies who face burnout culture but cannot easily exit due to equity lockups and mission identity; taxpayers who subsidize SpaceX/Starlink contracts but have no say in governance; communities affected by Tesla/X operations with no board representation.
% DISAPPEARANCE_RATIONALE: If the narrative constraint vanished overnight, Musk-led entities would lose their narrative premium — cost of capital would rise sharply, performance-share grants would become worthless, capital allocation would shift to DCF-justifiable projects, governance reforms would be demanded. The $1.75T+ aggregate valuation would compress toward fundamental cash-flow multiples. Capital markets would reprice 'founder-led innovation premium' across the board.
% FOUNDING_PROBLEM: Traditional valuation frameworks (DCF, comparables) systematically undervalue founder-led companies pursuing 'impossible' technological leaps because they cannot price optionality on未proven paradigms. The market needed a coordination mechanism to fund civilizational-scale bets that look like bad investments on near-term metrics.
% FOUNDING_PROBLEM_CORROBORATION: Believers cite Tesla's 2019-2020 profitability inflection, SpaceX's Falcon 9 reusability, Starlink's cash-flow positivity as proof the founding problem was real and the solution works. Skeptics cite SolarCity bailout, Hyperloop vaporware, FSD missed deadlines, Twitter/X value destruction as proof the coordination function has degraded into extraction. Independent academic work (e.g., Stanford Graduate School of Business case studies on Tesla valuation) documents both the genuine coordination achievement and the subsequent narrative detachment.
narrative_ontology:disappearance_verdict(valuation_legitimacy__musk_cult_believer, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__musk_cult_believer, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__musk_cult_believer, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(valuation_legitimacy__musk_cult_believer, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__musk_cult_believer, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__musk_cult_believer_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__musk_cult_believer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the large and growing gap between narrative-driven valuation and fundamental cash-flow justification across Musk-led entities. The performance-share grants, dual-class governance, and 'funding secured' episodes represent transfers that lack reciprocal value creation for non-founder stakeholders. Suppression (0.45) is moderate — the constraint doesn't prevent dissent (short-sellers operate, skeptics publish), but it makes dissent expensive through unlimited upside risk on narrative squeezes and reputational costs for 'missing the story.' Theater ratio (0.25) is low but rising — early Tesla/SpaceX execution was genuinely coordination-heavy; recent X/Twitter acquisition and Mars timeline shifts show more performative maintenance of the narrative. Accessibility collapse (0.35) is moderate — alternative valuation frameworks (DCF, real options) remain available and used by many participants. Resistance (0.55) is significant — short-sellers, governance activists, and fundamental investors actively contest the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, this is a rope — genuine coordination solving the 'funding impossible bets' problem. From the payer seats (short-sellers, minority shareholders), it reads as a snare — extraction enforced through governance capture and narrative control. From the believer seat (identity_locked beneficiary), it reads as a mountain — the track record feels like natural law. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the authoring seat's assessment that both coordination and extraction are real and substantial.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk-founded entities (agenda_setter) sit at d ≈ 0.1 (full beneficiary) — they set the narrative, control governance, and raise capital at narrative premia. Musk cult believers (beneficiary, identity_locked) sit at d ≈ 0.2 — they collect narrative-driven gains but are structurally trapped by identity fusion. Short-sellers (payer, constrained exit) sit at d ≈ 0.85 — they bear the extraction directly through mark-to-market losses. Valuation skeptics (payer, mobile) sit at d ≈ 0.6 — they pay reputational/opportunity costs but can exit by changing methodology. Minority shareholders (payer, constrained) sit at d ≈ 0.75 — they bear asymmetric downside with no governance voice. Capital markets observers (observer, analytical) sit at d = 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (funding civilizational-scale bets that DCF undervalues) was live in 2010-2018 and the constraint genuinely solved it — Tesla and SpaceX execution validates the coordination function. Post-2020, the constraint's coordination function has atrophied (X/Twitter destruction, FSD delays, Mars timeline slips) while extraction has intensified (performance shares, governance entrenchment). The mandatrophy is contested because believers argue the coordination function is just entering its most ambitious phase (Mars, Optimus, xAI) while skeptics argue it has become a vehicle for personal enrichment. The constraint persists because the agenda-setter controls the narrative and governance, and believers' identity-lock prevents exit-driven correction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Where does the genuine coordination function (funding high-variance technological bets) end and pure narrative extraction begin? Is the Mars-colony performance-share grant a coordination mechanism or a governance-free transfer?',
    'Counterfactual analysis: if Musk-led entities raised capital at DCF-justifiable valuations post-2020, would they still pursue Mars/Starlink/FSD at the same pace? If yes, coordination function is independent of narrative premium. If no, narrative premium is load-bearing for the coordination.',
    'If coordination function is independent of narrative premium, the constraint is a snare with a coordination cover story. If narrative premium is load-bearing, it is a genuine tangled rope where extraction funds coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Whether the extraction component is necessary for the coordination function or merely parasitic on it.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds believers to the constraint? Is it financial (sunk cost), ideological (civilizational mission), social (community belonging), or professional (career capital tied to narrative)?',
    'Survey/interview studies of long-term Musk-entity shareholders measuring identity centrality, exit intentions under counterfactual scenarios (narrative collapse, Musk departure, governance reform), and correlation with financial vs. non-financial identity markers.',
    'If identity lock is primarily financial, exit becomes more likely under sustained drawdown. If ideological/social, exit is unlikely regardless of financial outcomes — the constraint is more persistent. If professional, exit correlates with career-stage exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'The mechanism of identity-locked exit for believer stakeholders.').

omega_variable(
    governance_extraction_causality,
    'Does Musk''s 82.4% voting control with 42% equity enable the coordination function (rapid decision-making for impossible bets) or does it primarily enable extraction (performance shares, related-party deals, X acquisition)?',
    'Governance event study: compare decision speed/quality and shareholder returns for Musk-controlled vs. non-Musk-controlled high-variance tech companies. Analyze whether dual-class structure correlates with better execution on long-horizon bets or with founder enrichment.',
    'If governance enables coordination, the constraint''s tangled rope character is strengthened. If governance enables extraction, the constraint trends toward snare. The performance-share grants (1B shares for Mars milestones) are a key test case — they require governance control to structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_extraction_causality, empirical, 'Whether governance concentration serves coordination or extraction.').

omega_variable(
    kernel_framing_underdetermination,
    'Does the ''valuation_legitimacy'' kernel frame the contest at the right level of abstraction? An alternative framing: ''founder_legitimacy'' — whether Musk personally legitimates ventures vs. whether each venture''s fundamentals legitimates its valuation. The kernel choice changes which structural elements are held constant across readings.',
    'Analyze whether sibling readings share a common referent (valuation methodology for Musk-led entities) or whether they talk past each other (dcf_fundamentalist talks about Tesla cash flows; governance_skeptic talks about board structure; musk_cult_believer talks about Musk''s biography). If they talk past each other, the kernel is a false unity.',
    'If kernel is a false unity, each reading should be a separate constraint story without network links. If kernel is genuine, network links and cs_structure reading_relations are warranted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the declared kernel captures a genuine shared referent across readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__musk_cult_believer, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2010, valuation_legitimacy__musk_cult_believer, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(valu_tr_t2013, valuation_legitimacy__musk_cult_believer, theater_ratio, 2013, 0.08).
narrative_ontology:measurement(valu_tr_t2016, valuation_legitimacy__musk_cult_believer, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(valu_tr_t2019, valuation_legitimacy__musk_cult_believer, theater_ratio, 2019, 0.18).
narrative_ontology:measurement(valu_tr_t2021, valuation_legitimacy__musk_cult_believer, theater_ratio, 2021, 0.22).
narrative_ontology:measurement(valu_tr_t2023, valuation_legitimacy__musk_cult_believer, theater_ratio, 2023, 0.24).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__musk_cult_believer, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(valu_be_t2010, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2010, 0.15).
narrative_ontology:measurement(valu_be_t2013, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2013, 0.25).
narrative_ontology:measurement(valu_be_t2016, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2016, 0.35).
narrative_ontology:measurement(valu_be_t2019, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2019, 0.45).
narrative_ontology:measurement(valu_be_t2021, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2021, 0.58).
narrative_ontology:measurement(valu_be_t2023, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2023, 0.62).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2010, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(valu_su_t2013, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2013, 0.15).
narrative_ontology:measurement(valu_su_t2016, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2016, 0.25).
narrative_ontology:measurement(valu_su_t2019, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2019, 0.35).
narrative_ontology:measurement(valu_su_t2021, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2021, 0.42).
narrative_ontology:measurement(valu_su_t2023, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2023, 0.44).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__musk_cult_believer, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__musk_cult_believer, 0.18).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This story decomposes the 'valuation_legitimacy' kernel into four structurally distinct readings. The musk_cult_believer reading has the highest base extractiveness (0.65) and claims tangled_rope (genuine coordination + extraction). The dcf_fundamentalist reading claims mountain (low extraction, natural law of finance). The real_options_technologist reading claims rope (coordination via option pricing). The governance_skeptic reading claims snare (pure extraction via governance capture). They are linked via affects_constraints because the musk_cult_believer narrative premium directly affects the operating environment for the other three readings — it changes the cost of capital, the short-selling risk, and the governance reform pressure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, institutional, 0.1).
constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, organized, 0.2).
constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, powerful, 0.85).
constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, moderate, 0.6).
constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, powerless, 0.75).
constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
