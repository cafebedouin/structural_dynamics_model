% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__musk_cult_believer
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: valuation_legitimacy__musk_cult_believer
 *   human_readable: Musk Track Record Valuation Legitimacy (Believer Reading)
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This constraint story instantiates the musk_cult_believer reading of the
 *   valuation_legitimacy kernel. In this reading, legitimacy derives from a
 *   founder's demonstrated capacity to achieve objectives that expert
 *   consensus deems impossible or imprudent. Financial metrics and governance
 *   standards are treated as lagging indicators or irrelevant obstacles
 *   rather than legitimate valuation inputs. The constraint coordinates
 *   capital toward high-risk frontier ventures but simultaneously extracts
 *   from skeptics, shorts, and minority governance interests through
 *   narrative-driven valuation inflation and suppression of alternative
 *   appraisal methodologies. The kernel has four contested readings; this is
 *   the one that treats founder track record as supreme.
 *
 * KEY AGENTS:
 *   - musk_founder: Primary agenda setter (powerful/arbitrage) â sets narrative, receives performance awards, controls voting structure
 *   - mission_believer_investors: Primary beneficiaries (organized/identity_locked) â hold equity through volatility, identity fused with mission
 *   - short_sellers: Primary targets (organized/constrained) â bear losses from narrative-driven price action, delegitimized
 *   - skeptic_analysts: Secondary targets (moderate/constrained) â face professional marginalization for conventional analysis
 *   - governance_advocates: Excluded voices (moderate/constrained) â argue for minority protections, dismissed as irrelevant
 *   - financial_economists: Analytical observers (analytical/analytical) â document deviation from market norms without direct stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__musk_cult_believer, 0.75).
domain_priors:suppression_score(valuation_legitimacy__musk_cult_believer, 0.8).
domain_priors:theater_ratio(valuation_legitimacy__musk_cult_believer, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, extractiveness, 0.75).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(valuation_legitimacy__musk_cult_believer, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__musk_cult_believer, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__musk_cult_believer, "Musk Track Record Valuation Legitimacy (Believer Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__musk_cult_believer, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__musk_cult_believer).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__musk_cult_believer, '614f9784-fe6c-4e2c-beb2-47637b2288a6').
narrative_ontology:cs_kernel_codification('614f9784-fe6c-4e2c-beb2-47637b2288a6', implicit).
narrative_ontology:cs_authority_grounding('614f9784-fe6c-4e2c-beb2-47637b2288a6', extraction).
narrative_ontology:cs_interpretation_layer_present('614f9784-fe6c-4e2c-beb2-47637b2288a6').
narrative_ontology:cs_reading_relation('614f9784-fe6c-4e2c-beb2-47637b2288a6', valuation_legitimacy__dcf_fundamentalist, forecloses).
narrative_ontology:cs_reading_relation('614f9784-fe6c-4e2c-beb2-47637b2288a6', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('614f9784-fe6c-4e2c-beb2-47637b2288a6', valuation_legitimacy__governance_skeptic, forecloses).
narrative_ontology:cs_axiom('614f9784-fe6c-4e2c-beb2-47637b2288a6', foundational, founder_track_record_supremacy).
narrative_ontology:cs_axiom_status(founder_track_record_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('614f9784-fe6c-4e2c-beb2-47637b2288a6', founder_track_record_supremacy, empirically_contingent).
narrative_ontology:cs_axiom('614f9784-fe6c-4e2c-beb2-47637b2288a6', foundational, financial_metrics_are_lagging_indicators).
narrative_ontology:cs_axiom_status(financial_metrics_are_lagging_indicators, holdable).
narrative_ontology:cs_axiom_grounding('614f9784-fe6c-4e2c-beb2-47637b2288a6', financial_metrics_are_lagging_indicators, empirically_contingent).
narrative_ontology:cs_reference_frame('614f9784-fe6c-4e2c-beb2-47637b2288a6', founder_execution_supremacy).
narrative_ontology:cs_drift_state('614f9784-fe6c-4e2c-beb2-47637b2288a6', post_governance_challenges_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('614f9784-fe6c-4e2c-beb2-47637b2288a6', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__musk_cult_believer, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__musk_cult_believer, mission_believer_investors).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, short_sellers).
narrative_ontology:constraint_victim(valuation_legitimacy__musk_cult_believer, skeptic_analysts).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, narrative_driven_valuation).
narrative_ontology:constraint_vindicates(valuation_legitimacy__musk_cult_believer, founder_led_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the narrative around corporate milestones, product timelines, and valuation benchmarks through direct public communication. Receives performance-based equity awards tied to market capitalization and operational targets. Controls voting power disproportionate to economic ownership. Benefits directly from valuation premiums attached to his personal brand and track record.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, musk_founder, agenda_setter,
    powerful, generational, arbitrage, global).

% Allocate capital based on founder track record and mission narrative rather than discounted cash flow analysis. Hold equity through volatility, benefiting from narrative-driven appreciation and short squeezes. Their investment identity is fused with technological optimism and anti-establishment market participation.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, mission_believer_investors, beneficiary,
    organized, biographical, identity_locked, global).

% Take positions betting on fundamental mean-reversion or governance-driven downside. Bear concentrated losses during narrative-driven price appreciation and short squeezes. Their market function of price discovery is delegitimized by the dominant narrative framing them as malicious rather than analytical.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, short_sellers, payer,
    organized, biographical, constrained, global).

% Issue research based on conventional financial metrics or governance concerns. Face professional marginalization, reduced access to management, and ridicule from retail investor communities. Their analytical outputs are systematically discounted relative to narrative-aligned commentary.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, skeptic_analysts, payer,
    moderate, biographical, constrained, global).

% Argue that concentrated voting control and weak board independence undermine minority shareholder value. Are excluded from effective influence by the prevailing narrative that governance structures are irrelevant when the founder is uniquely capable.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, governance_advocates, excluded,
    moderate, generational, constrained, national).

% Observe the deviation from efficient market benchmarks and governance norms. Document the premium attached to founder narrative versus fundamentals. Do not participate in the capital allocation directly.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__musk_cult_believer, financial_economists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__musk_cult_believer, musk_founder).
narrative_ontology:fixing_cost_class(valuation_legitimacy__musk_cult_believer, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates capital allocation toward high-risk frontier technology venturesâreusable rockets, electric vehicle scale-up, satellite constellationsâthat conventional discounted cash flow analysis would reject due to negative near-term cash flows and high bankruptcy risk.
% TRANSFER_FUNCTION: Moves wealth from short sellers and skeptical capital to long-believer investors and the founder through narrative-driven valuation inflation; moves governance control from minority shareholders to the founder by rendering governance concerns illegitimate; moves professional credibility from conventional analysts to narrative-aligned commentators.
% ABSENT_VOICES: Discounted cash flow value investors, governance scholars emphasizing minority shareholder protections, former employees with critical operational data, and academic efficient-market theorists are structurally excluded from valuation conversations; their frameworks are dismissed as lagging or malicious.
% DISAPPEARANCE_RATIONALE: If the constraint vanished overnight, capital would flee frontier-tech ventures lacking near-term cash flows, valuations would compress to conventional metrics, the founder's compensation and control structures would face immediate challenge, and the retail investor base organized around mission narratives would disperse.
% FOUNDING_PROBLEM: Capital markets systematically underprice or refuse to fund capital-intensive frontier technology with long development horizons and high technical risk, creating a coordination failure between breakthrough-seeking entrepreneurs and risk-capital providers.
% FOUNDING_PROBLEM_CORROBORATION: Innovation economists and venture capital scholars outside the benefiting parties attest that founder narratives and mission-driven capital pools genuinely solve a market failure in frontier-tech funding. Governance scholars and value investors outside the benefiting parties attest that the founding problem is substantially solvedâTesla and SpaceX are mature cash-flow entitiesâand the arrangement now primarily serves extraction and control consolidation.
narrative_ontology:disappearance_verdict(valuation_legitimacy__musk_cult_believer, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__musk_cult_believer, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__musk_cult_believer, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__musk_cult_believer, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__musk_cult_believer, 0.75, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.75) is high because the valuation framework systematically transfers wealth from skeptics to believers and founder through inflated equity values decoupled from near-term cash flows. Suppression (0.80) is higher because the constraint's persistence depends on actively dismissing DCF analysis, governance critique, and short-selling research as illegitimate or malicious. Theater ratio (0.60) reflects that a substantial share of narrative activity (Mars colony timelines, robotaxi promises) serves to maintain valuation rather than report verifiable operational progress. Accessibility collapse (0.70) is high because once an investor accepts the founder-supremacy framework, conventional valuation alternatives collapse as relevant inputs. Resistance (0.45) is moderate because short interest and skeptical research persist but are structurally disadvantaged by narrative-driven price action and social delegitimization.
 *
 * PERSPECTIVAL GAP:
 *   The believer seat experiences the constraint as genuine coordination solving a market failure in frontier-tech funding; the payer seats (shorts, skeptics, minority governance) experience it as extraction enforced by narrative control. The founder seat experiences both coordination and capture. The engine computes this divergence from structural data: believers have identity_locked exit and beneficiary role; payers have constrained exit and payer role.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (mission_believer_investors) derive low directionality because the constraint subsidizes their positions through narrative-driven appreciation and short squeezes. The founder agenda setter derives very low directionality because the constraint is custom-built to inflate his compensation and control. Payers (short_sellers, skeptic_analysts) derive high directionality because the constraint extracts directly through price action and professional marginalization. Excluded governance advocates sit near the target end but are outside the active transfer mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâfunding frontier tech rejected by conventional metricsâwas genuine. The constraint prevents mislabeling by documenting that the problem is contested: innovation economists say live; governance scholars say dead. If dead, the constraint is mandatrophic. The temporal measurements show rising extractiveness and theater ratio even as the underlying businesses mature, supporting the obsolescence hypothesis without resolving it. The T17 abductive trigger would fire on the mountain-extraction-accumulation pattern if this were misclassified as a mountain, but it is properly authored as tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dcf_foreclosure_ambiguity,
    'Does the musk_cult_believer reading logically foreclose the dcf_fundamentalist reading within any single valuation framework, or can an investor hold both premises simultaneously?',
    'Examine whether any market participant simultaneously assigns primary legitimacy to founder track record AND to discounted cash flow; if none exist, forecloses is structurally valid.',
    'If foreclosed, the constraint actively delegitimizes alternative valuation and raises suppression; if coexisting, it is one paradigm among many.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dcf_foreclosure_ambiguity, conceptual, 'Whether musk_cult_believer forecloses dcf_fundamentalist or coexists with it.').

omega_variable(
    frontier_funding_genuineness,
    'Is the frontier-technology funding coordination function structurally separable from the founder-cult narrative, or are they inseparable?',
    'Compare capital efficiency for analogous ventures with and without charismatic-founder valuation premia; examine whether the narrative persists after positive cash flows are achieved.',
    'If separable, the constraint may degrade to snare; if inseparable, the tangled rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(frontier_funding_genuineness, empirical, 'Whether coordination and extraction components are structurally separable.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (platform control, analyst intimidation, media access) or internalized (investor identity fusion with the mission, self-censorship by analysts)?',
    'Post-narrative-break suppression trajectory: if skepticism remains professionally costly after structural barriers are removed, reclassify as partially internalized.',
    'If internalized, effective suppression exceeds structural measure and the constraint operates partly through identity coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    founding_problem_live_or_dead,
    'Has the founding problemâfunding frontier tech rejected by conventional metricsâbeen solved by the maturation of Tesla and SpaceX, or does it remain live for Mars colonization and AI?',
    'Cash flow analysis of the underlying entities independent of narrative premium; expert assessment from innovation economists versus governance scholars.',
    'If dead, the constraint is mandatrophic and piton classification should be evaluated; if live, the coordination function remains genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_live_or_dead, empirical, 'Whether the founding coordination problem is still live or obsolescent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__musk_cult_believer, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vl_mcb_tr_t0, valuation_legitimacy__musk_cult_believer, theater_ratio, 0, 0.2).
narrative_ontology:measurement(vl_mcb_tr_t2, valuation_legitimacy__musk_cult_believer, theater_ratio, 2, 0.25).
narrative_ontology:measurement(vl_mcb_tr_t4, valuation_legitimacy__musk_cult_believer, theater_ratio, 4, 0.32).
narrative_ontology:measurement(vl_mcb_tr_t6, valuation_legitimacy__musk_cult_believer, theater_ratio, 6, 0.42).
narrative_ontology:measurement(vl_mcb_tr_t8, valuation_legitimacy__musk_cult_believer, theater_ratio, 8, 0.52).
narrative_ontology:measurement(vl_mcb_tr_t10, valuation_legitimacy__musk_cult_believer, theater_ratio, 10, 0.56).
narrative_ontology:measurement(vl_mcb_tr_t12, valuation_legitimacy__musk_cult_believer, theater_ratio, 12, 0.6).

% Extraction over time
narrative_ontology:measurement(vl_mcb_be_t0, valuation_legitimacy__musk_cult_believer, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vl_mcb_be_t2, valuation_legitimacy__musk_cult_believer, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(vl_mcb_be_t4, valuation_legitimacy__musk_cult_believer, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(vl_mcb_be_t6, valuation_legitimacy__musk_cult_believer, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(vl_mcb_be_t8, valuation_legitimacy__musk_cult_believer, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(vl_mcb_be_t10, valuation_legitimacy__musk_cult_believer, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(vl_mcb_be_t12, valuation_legitimacy__musk_cult_believer, base_extractiveness, 12, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(vl_mcb_su_t0, valuation_legitimacy__musk_cult_believer, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vl_mcb_su_t2, valuation_legitimacy__musk_cult_believer, suppression_requirement, 2, 0.38).
narrative_ontology:measurement(vl_mcb_su_t4, valuation_legitimacy__musk_cult_believer, suppression_requirement, 4, 0.48).
narrative_ontology:measurement(vl_mcb_su_t6, valuation_legitimacy__musk_cult_believer, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(vl_mcb_su_t8, valuation_legitimacy__musk_cult_believer, suppression_requirement, 8, 0.74).
narrative_ontology:measurement(vl_mcb_su_t10, valuation_legitimacy__musk_cult_believer, suppression_requirement, 10, 0.78).
narrative_ontology:measurement(vl_mcb_su_t12, valuation_legitimacy__musk_cult_believer, suppression_requirement, 12, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__musk_cult_believer, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__musk_cult_believer, governance_skeptic).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the valuation_legitimacy kernel. The kernel decomposes into four structurally distinct claims about the source of valuation legitimacy, each with different beneficiary/victim structures and epsilon values. This reading (musk_cult_believer) has high extractiveness and suppression; the dcf_fundamentalist reading would have low extractiveness but high accessibility_collapse for proven cash flows; the governance_skeptic reading would identify victims as minority shareholders; the real_options_technologist reading would have moderate extractiveness tied to optionality value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
