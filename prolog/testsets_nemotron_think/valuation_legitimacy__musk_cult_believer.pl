% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__musk_cult_believer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   human_readable: Musk Track Record Valuation Legitimacy
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This constraint story captures the 'Musk cult believer' reading of the
 *   valuation_legitimacy kernel. The reading asserts that Elon Musk's
 *   historical achievement of 'impossible' goals (reusable orbital rockets
 *   via SpaceX, profitable EV mass production via Tesla, Starlink deployment)
 *   constitutes a valid epistemological basis for valuing his current and
 *   future ventures at extreme multiples. Financial metrics (DCF, earnings
 *   multiples) are dismissed as lagging indicators that fail to capture the
 *   optionality of vertical integration and first-principles engineering. The
 *   reading explicitly rejects governance concerns (dual-class shares, board
 *   independence, related-party transactions) as irrelevant when the founder
 *   has demonstrated unique capability. The beneficiary set is believers who
 *   buy and hold; the victim set is short-sellers and skeptical analysts who
 *   have consistently lost money betting against Musk. The claimed type is
 *   'rope' — a genuine coordination mechanism that directs capital to
 *   high-variance, high-mean ventures — but the metrics show moderate
 *   extractiveness and rising theater, suggesting the engine may compute a
 *   different per-seat classification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, 0.6).
domain_priors:suppression_score(valuation_legitimacy__musk_cult_believer, 0.3).
domain_priors:theater_ratio(valuation_legitimacy__musk_cult_believer, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, extractiveness, 0.6).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__musk_cult_believer, rope).
narrative_ontology:human_readable(valuation_legitimacy__musk_cult_believer, "Musk Track Record Valuation Legitimacy").
narrative_ontology:topic_domain(valuation_legitimacy__musk_cult_believer, "corporate_finance/technology_governance/space_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__musk_cult_believer, '5774fa2b-7e0c-40aa-8b87-87a57cb43f1b').
narrative_ontology:cs_kernel_codification('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', distributed).
narrative_ontology:cs_authority_grounding('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', lineage).
narrative_ontology:cs_interpretation_layer_present('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b').
narrative_ontology:cs_reading_relation('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', valuation_legitimacy__governance_skeptic, forecloses).
narrative_ontology:cs_axiom('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', foundational, founder_genius_justifies_valuation).
narrative_ontology:cs_axiom_status(founder_genius_justifies_valuation, holdable).
narrative_ontology:cs_axiom_grounding('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', founder_genius_justifies_valuation, empirically_contingent).
narrative_ontology:cs_axiom('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', secondary, governance_irrelevant_for_unique_founders).
narrative_ontology:cs_axiom_status(governance_irrelevant_for_unique_founders, holdable).
narrative_ontology:cs_axiom_grounding('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', governance_irrelevant_for_unique_founders, deontological).
narrative_ontology:cs_reference_frame('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', musk_impossible_achievements_lineage).
narrative_ontology:cs_drift_state('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', post_twitter_acquisition, gap(authority_erosion, minor, false)).
narrative_ontology:cs_created_at('5774fa2b-7e0c-40aa-8b87-87a57cb43f1b', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__musk_cult_believer, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, musk_believers).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, tesla_long_term_shareholders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, spacex_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, short_sellers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, skeptical_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, tesla_long_term_shareholders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, founder_led_innovation_superiority).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, first_principles_thinking_works).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, vertical_integration_creates_compounding_optionality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the vision and 'impossible' goals (reusable rockets, Mars colony, FSD). His track record is the evidence base for the valuation model. He controls voting power and narrative. Exit is arbitrary — he can leave but chooses to stay.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, elon_musk, agenda_setter,
    institutional, generational, arbitrage, global).

% Retail and institutional investors who buy and hold based on belief in Musk's track record. They benefit from valuation appreciation. Exit is selling shares — liquid but costly if conviction is high.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_believers, beneficiary,
    organized, biographical, mobile, global).

% Institutional holders who benefit from stock appreciation but bear governance risk (lack of independent board, key-person risk). Exit is constrained by fund mandates and liquidity.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, tesla_long_term_shareholders, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__musk_cult_believer, tesla_long_term_shareholders, payer).

% Private investors in SpaceX who accept illiquidity for access to the Mars colony optionality. Exit is extremely constrained (secondary markets limited, lock-ups).
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, spacex_investors, beneficiary,
    powerful, generational, constrained, global).

% Hedge funds and traders betting against Musk's companies. They have lost billions historically. Exit is trapped — covering shorts at high prices causes further losses; staying short risks unlimited downside.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, short_sellers, payer,
    powerful, immediate, trapped, global).

% Equity researchers and commentators who apply traditional valuation (DCF, multiples) and flag governance risks. They are structurally excluded from the valuation narrative — their models are dismissed as 'lagging indicators'.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, skeptical_analysts, excluded,
    moderate, biographical, mobile, global).

% Corporate governance experts, proxy advisors, and minority shareholder advocates who argue that 82.4% voting control with 42% equity is extraction. They are excluded because the reading declares governance irrelevant for unique founders.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, governance_advocates, excluded,
    moderate, biographical, mobile, national).

% Board members tasked with oversight. In practice, they have limited influence due to Musk's voting control. They observe the constraint but cannot change it.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, independent_directors, observer,
    moderate, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Directs capital to high-risk, high-reward ventures led by proven founders who pursue seemingly impossible goals, bypassing traditional risk-averse allocation mechanisms that would starve such ventures of funding.
% TRANSFER_FUNCTION: Moves capital from skeptics/short-sellers to believers and Musk's companies via market mechanisms: short squeezes, capital raises at high valuations, and optionality pricing that rewards conviction over current cash flows.
% ABSENT_VOICES: Skeptical analysts, governance advocates, short sellers, and risk-averse institutional investors are structurally excluded from the valuation narrative. They would argue that governance matters, that DCF is not a 'lagging indicator' but a discipline, and that survivor bias inflates the track record.
% DISAPPEARANCE_RATIONALE: If the belief that Musk's track record justifies premium valuations vanished, capital allocation would revert to DCF-based models. Tesla and SpaceX would face dramatically higher cost of capital, likely forcing downsizing of Mars/FSD ambitions and reducing Musk's ability to attract talent and capital for 'impossible' goals.
% FOUNDING_PROBLEM: Traditional valuation methods (DCF, comparable multiples) systematically undervalue founder-led, vertically integrated, first-principles companies that pursue seemingly impossible goals because they cannot price optionality on unproven technologies.
% FOUNDING_PROBLEM_CORROBORATION: Venture capitalists specializing in hard tech (e.g., Founders Fund, a16z) corroborate that traditional metrics undervalue deep tech; however, mainstream finance academia, regulatory bodies (SEC), and corporate governance institutions do not accept this reading.
narrative_ontology:disappearance_verdict(valuation_legitimacy__musk_cult_believer, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__musk_cult_believer, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__musk_cult_believer, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__musk_cult_believer, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__musk_cult_believer, 0.6, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness 0.6 reflects that the constraint transfers substantial wealth from skeptics (shorts) to believers via market mechanisms, but the reading argues this is fair compensation for risk-bearing. Suppression 0.3 reflects narrative dismissal of alternative valuation frameworks, not legal coercion. Theater_ratio 0.2 captures performative aspects (Twitter polls, product unveilings as marketing) but the reading sees these as genuine signaling. Accessibility_collapse 0.5: for believers, DCF alternatives are cognitively collapsed; for outsiders, they remain viable. Resistance 0.5: ongoing short interest, SEC scrutiny, and governance lawsuits constitute active resistance. Measurements show extractiveness rising 2010-2020 then plateauing; theater and suppression rising then plateauing — consistent with a coordination mechanism that has matured but not degraded.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter (Musk) and beneficiary seats experience this as a rope — genuine coordination that solves a capital allocation problem for impossible ventures. The payer seats (shorts, skeptics) experience it as a snare — a narrative that extracts via forced covering and optionality pricing. The engine will compute this divergence from the structural data. The reading's own claim (rope) matches the agenda_setter/beneficiary experience but not the payer experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Elon Musk (agenda_setter) sits at d≈0.0 (full beneficiary): he controls the narrative, voting power, and goal-setting. Believers (beneficiary) sit at d≈0.2: they gain from appreciation but bear mark-to-market risk. Long-term shareholders (beneficiary/payer) sit at d≈0.4: they gain but bear governance risk. SpaceX investors (beneficiary) sit at d≈0.1: illiquid but high conviction. Short sellers (payer) sit at d≈0.9: trapped, unlimited downside. Skeptical analysts and governance advocates (excluded) sit at d≈0.7: they bear reputational/career costs for being wrong but have mobile exit. Independent directors (observer) sit at d≈0.5: analytical seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (traditional finance undervalues founder-led impossible ventures) remains live per the reading. However, the governance_skeptic reading argues the problem is dead (Musk's companies are now mature, governance matters). The mismatch (status=live vs. disappearance_verdict=world_rearranges) flags potential mandatrophy: if the founding problem is actually dead but the arrangement persists, it becomes a piton. The corroboration from hard-tech VCs supports live; the absence of mainstream finance corroboration supports dead. This tension is the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does this reading''s structural relationship to the valuation_legitimacy kernel affect its classification, given sibling readings dcf_fundamentalist, real_options_technologist, and governance_skeptic?',
    'Cross-reading comparison of ε values, beneficiary/victim sets, and directionality profiles. If sibling readings show systematically different extraction profiles for the same referent, the kernel is confirmed as contested and each reading is a distinct constraint.',
    'Confirms that valuation_legitimacy is a kernel with multiple structurally distinct constraints, not a single constraint with measurement ambiguity. Validates the ε-invariance principle decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Committee frame: this constraint is one reading of a contested kernel; structural deltas to siblings are irreducible.').

omega_variable(
    survivor_bias_vs_predictive_track_record,
    'Is Musk''s track record genuinely predictive of future success, or is it survivor bias / selection effect (only the successes are visible; failures like SolarCity, Hyperloop, Twitter acquisition are downplayed)?',
    'Out-of-sample test: track record of Musk-led ventures founded after 2020 (Neuralink, The Boring Company, xAI, Twitter/X) against predefined milestones. Also, counterfactual analysis of ventures that failed or were restructured.',
    'If survivor bias, the coordination function is illusory — the constraint is a snare extracting from believers who buy at inflated valuations. If genuinely predictive, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survivor_bias_vs_predictive_track_record, empirical, 'Whether the track record evidence base is epistemically sound or contaminated by selection bias.').

omega_variable(
    governance_foreclosure_legitimacy,
    'Does the reading''s foreclosure of governance_skeptic (governance declared irrelevant) constitute a structural foreclosure within a single framework, or do the readings merely coexist in public discourse?',
    'Analyze whether a single institutional framework (e.g., Delaware corporate law, SEC disclosure regime) can simultaneously uphold both readings. If Delaware courts enforce governance duties regardless of founder track record, the readings coexist in discourse but foreclose in legal framework.',
    'If foreclosure in legal framework, the governance_skeptic reading is structurally foreclosed by existing institutions, making this reading''s claim of irrelevance a power claim, not a logical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(governance_foreclosure_legitimacy, conceptual, 'Whether the reading_relations forecloses edge to governance_skeptic is legally/institutionally valid or merely rhetorical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__musk_cult_believer, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(val_musk_believer_tr_t0, valuation_legitimacy__musk_cult_believer, theater_ratio, 0, 0.1).
narrative_ontology:measurement(val_musk_believer_tr_t2, valuation_legitimacy__musk_cult_believer, theater_ratio, 2, 0.12).
narrative_ontology:measurement(val_musk_believer_tr_t4, valuation_legitimacy__musk_cult_believer, theater_ratio, 4, 0.15).
narrative_ontology:measurement(val_musk_believer_tr_t6, valuation_legitimacy__musk_cult_believer, theater_ratio, 6, 0.18).
narrative_ontology:measurement(val_musk_believer_tr_t8, valuation_legitimacy__musk_cult_believer, theater_ratio, 8, 0.2).
narrative_ontology:measurement(val_musk_believer_tr_t10, valuation_legitimacy__musk_cult_believer, theater_ratio, 10, 0.2).
narrative_ontology:measurement(val_musk_believer_tr_t12, valuation_legitimacy__musk_cult_believer, theater_ratio, 12, 0.2).
narrative_ontology:measurement(val_musk_believer_tr_t14, valuation_legitimacy__musk_cult_believer, theater_ratio, 14, 0.2).

% Extraction over time
narrative_ontology:measurement(val_musk_believer_be_t0, valuation_legitimacy__musk_cult_believer, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(val_musk_believer_be_t2, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(val_musk_believer_be_t4, valuation_legitimacy__musk_cult_believer, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(val_musk_believer_be_t6, valuation_legitimacy__musk_cult_believer, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(val_musk_believer_be_t8, valuation_legitimacy__musk_cult_believer, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(val_musk_believer_be_t10, valuation_legitimacy__musk_cult_believer, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(val_musk_believer_be_t12, valuation_legitimacy__musk_cult_believer, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(val_musk_believer_be_t14, valuation_legitimacy__musk_cult_believer, base_extractiveness, 14, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(val_musk_believer_su_t0, valuation_legitimacy__musk_cult_believer, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(val_musk_believer_su_t2, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2, 0.2).
narrative_ontology:measurement(val_musk_believer_su_t4, valuation_legitimacy__musk_cult_believer, suppression_requirement, 4, 0.25).
narrative_ontology:measurement(val_musk_believer_su_t6, valuation_legitimacy__musk_cult_believer, suppression_requirement, 6, 0.28).
narrative_ontology:measurement(val_musk_believer_su_t8, valuation_legitimacy__musk_cult_believer, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(val_musk_believer_su_t10, valuation_legitimacy__musk_cult_believer, suppression_requirement, 10, 0.3).
narrative_ontology:measurement(val_musk_believer_su_t12, valuation_legitimacy__musk_cult_believer, suppression_requirement, 12, 0.3).
narrative_ontology:measurement(val_musk_believer_su_t14, valuation_legitimacy__musk_cult_believer, suppression_requirement, 14, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__musk_cult_believer, resource_allocation).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__musk_cult_believer, 0.15).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, valuation_legitimacy__governance_skeptic).

% DUAL FORMULATION NOTE:
% This constraint is the musk_cult_believer reading of the valuation_legitimacy kernel. It differs from dcf_fundamentalist in ε (0.6 vs ~0.1), beneficiary set (believers vs. fundamental analysts), and directionality (founder-centric vs. cash-flow-centric). It differs from real_options_technologist in grounding (person-specific track record vs. abstract optionality). It forecloses governance_skeptic on the axiom that governance is irrelevant for unique founders.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(valuation_legitimacy__musk_cult_believer, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
