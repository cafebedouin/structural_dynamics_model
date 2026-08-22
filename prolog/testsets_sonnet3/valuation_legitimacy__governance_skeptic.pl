% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_valuation_legitimacy__governance_skeptic, []).

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
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Musk Dual-Class Control Structure — Governance Skeptic Reading
 *   domain: corporate_finance/technology_governance/space_economics
 *
 * SUMMARY:
 *   This story instantiates the governance-skeptic reading of the
 *   valuation-legitimacy kernel as applied to a Musk-controlled frontier
 *   technology entity valued near $1.75T. Under this reading, valuation
 *   legitimacy is conditioned on governance structures that protect minority
 *   shareholders; the reading holds that the observed 82.4% voting control
 *   from 42% economic ownership, absence of independent compensation and
 *   nominating committees under controlled-company exemptions, charter
 *   renunciation of corporate opportunities, and unresolved multi-company
 *   time-allocation conflicts (Terafab benefiting Tesla/SpaceX with no
 *   arm's-length allocation process) together constitute extraction dressed
 *   as founder-execution coordination. This is ONE of four declared readings
 *   of the valuation_legitimacy kernel; the DCF-fundamentalist,
 *   real-options-technologist, and Musk-cult-believer readings are separate
 *   constraints with their own ε values and are not blended into this one.
 *
 * KEY AGENTS:
 *   - elon_musk: agenda_setter/beneficiary (institutional/arbitrage) — sets governance terms, controls opportunity allocation across affiliated companies
 *   - class_a_public_shareholders: payer (powerless/mobile) — bear economic risk without governance voice
 *   - early_class_b_holders: beneficiary (organized/arbitrage) — aligned voting power without proportionate accountability exposure
 *   - tesla_spacex_boards: excluded (institutional/constrained) — materially interested in opportunity allocation but structurally absent from it
 *   - sec_and_delaware_courts: observer (institutional/analytical) — slow-moving external check on related-party dealing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.79).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.71).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.79).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Musk Dual-Class Control Structure — Governance Skeptic Reading").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "corporate_finance/technology_governance/space_economics").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, 'f49a3340-8dd2-437c-9a4e-03ed501c4fb5').
narrative_ontology:cs_kernel_codification('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', distributed).
narrative_ontology:cs_authority_grounding('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', distributed).
narrative_ontology:cs_reading_relation('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_axiom('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', foundational, governance_protection_is_valuation_precondition).
narrative_ontology:cs_axiom_status(governance_protection_is_valuation_precondition, holdable).
narrative_ontology:cs_axiom_grounding('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', governance_protection_is_valuation_precondition, conventional).
narrative_ontology:cs_axiom('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', foundational, voting_control_disproportionate_to_equity_is_extraction_absent_accountability).
narrative_ontology:cs_axiom_status(voting_control_disproportionate_to_equity_is_extraction_absent_accountability, holdable).
narrative_ontology:cs_axiom_grounding('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', voting_control_disproportionate_to_equity_is_extraction_absent_accountability, empirically_contingent).
narrative_ontology:cs_reference_frame('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', shareholder_primacy_governance_norm).
narrative_ontology:cs_drift_state('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', post_ipo_multi_entity_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f49a3340-8dd2-437c-9a4e-03ed501c4fb5', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, elon_musk).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, minority_institutional_investors).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, controlled_company_governance_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds 82.4% of voting power via 10:1 Class B shares against 42% economic ownership. Sets board composition, blocks independent compensation and nominating committees under controlled-company exemptions, and allocates his own time and attention across five-plus affiliated companies (Terafab, Tesla, SpaceX, xAI, Neuralink, The Boring Company) without a board process that can override him. The charter renounces corporate opportunities on his behalf, meaning ventures he originates can be routed to whichever entity he chooses. His exit from any accountability mechanism is functionally unconstrained — no vote of Class A shareholders can remove him or restructure the vote ratio.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, elon_musk, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, elon_musk, beneficiary).

% Hold the bulk of the economic capital but a small fraction of the vote. Can sell shares (mobile in the market sense) but cannot exercise governance voice — no seat on compensation or nominating committees, no credible proxy path to challenge related-party allocation decisions between Musk's companies. Their only real recourse is exit via sale, which does nothing to correct the governance structure itself; it just prices the extraction into whoever holds the shares next.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    powerless, biographical, mobile, global).

% A small pre-IPO cohort holding super-voting Class B shares alongside Musk. Their votes are structurally aligned with his and their economic upside compounds with the valuation premium his control commands, without themselves bearing the accountability costs imposed on Class A holders.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    organized, generational, arbitrage, global).

% Sister-company boards have a legitimate interest in how Musk's attention, IP, and opportunity allocation are divided across ventures (Terafab's outputs plausibly benefit Tesla and SpaceX), but have no formal seat in Terafab's governance to negotiate or contest that allocation. They are structurally excluded from a decision that materially affects their own shareholders.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_spacex_boards, excluded,
    institutional, biographical, constrained, global).

% Regulatory and judicial bodies that can, in principle, scrutinize related-party transactions, disclosure adequacy, and fiduciary duty under controlled-company doctrine. Their enforcement has historically been slow and post-hoc relative to the speed of capital allocation decisions inside Musk-controlled entities.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, sec_and_delaware_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, elon_musk).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A controlling founder-operator can allocate capital and technical talent quickly across a multi-company frontier-technology portfolio without seeking case-by-case shareholder ratification, which genuinely accelerates execution on capital-intensive, long-horizon bets that dispersed shareholder voting would slow down.
% TRANSFER_FUNCTION: Moves control premium and private benefits of control (favorable related-party allocation of Musk's time, IP, and corporate opportunities) from Class A public shareholders — who hold the bulk of the economic risk — to Musk and the early Class B cohort, who hold the bulk of the voting power without proportionate economic exposure.
% ABSENT_VOICES: Class A shareholders as a class have no seat on compensation or nominating committees and cannot compel disclosure of how corporate opportunities are allocated between Musk's companies; sister-company boards (Tesla, SpaceX) that bear real interest in that allocation have no negotiating seat at all inside this entity's governance.
% DISAPPEARANCE_RATIONALE: If the dual-class structure and controlled-company exemptions disappeared overnight — one-share-one-vote, independent committees — capital allocation decisions would require board and shareholder processes that could reroute opportunities away from Musk-favored allocations, compensation would be renegotiated at arm's length, and the valuation premium attributable to unconstrained founder control would compress; the governance skeptic reading holds a meaningful share of the $1.75T valuation is a control premium, not a coordination dividend, and would evaporate with the structure.
% FOUNDING_PROBLEM: Founders raising capital for capital-intensive, high-failure-rate frontier technology ventures argued dispersed public shareholders would second-guess necessary long-horizon bets, so voting control was concentrated to protect execution speed against short-termist shareholder pressure.
% FOUNDING_PROBLEM_CORROBORATION: Musk and the early Class B cohort attest the structure remains necessary to execute the technology roadmap without short-term shareholder interference. Independent governance researchers, proxy advisory firms (ISS, Glass Lewis), and Delaware Chancery Court rulings on related-party compensation matters (outside the benefiting parties) have documented that the accountability gap has widened past what the original execution-speed rationale requires, particularly given the multi-entity time-allocation conflicts that did not exist at founding.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.79, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(valuation_legitimacy__governance_skeptic_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(valuation_legitimacy__governance_skeptic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.79) is authored high because the value transferred — control premium, favorable time/IP allocation, unreviewable related-party decisions — runs structurally from dispersed economic owners to the concentrated voting bloc, and this transfer is decoupled from any per-transaction coordination benefit. Suppression (0.71) reflects that alternatives (proxy contests, committee independence, one-share-one-vote) are foreclosed by the charter and controlled-company exemptions, not merely disfavored. Theater ratio (0.42) captures that some governance apparatus exists (board meetings, disclosures, say-on-pay votes) but functions performatively given the controlling vote overrides any adverse outcome. Accessibility collapse (0.62) is moderate-high: exit via share sale remains available (this is not a trapped-employee scenario) but voice-based correction is essentially foreclosed once the dual-class structure is understood. Resistance (0.58) reflects real but historically unsuccessful shareholder litigation and proxy advisory pushback.
 *
 * DIRECTIONALITY LOGIC:
 *   Musk and early Class B holders sit at the beneficiary end of directionality — the constraint structurally subsidizes their control and optionality at the expense of proportional accountability. Class A public shareholders sit at the target end: they bear the governance costs (foreclosed voice, unreviewable related-party allocation) while holding most of the economic exposure. Sister-company boards are neither clean beneficiaries nor victims of THIS constraint directly, but are structurally excluded from a decision that affects their own shareholders — hence their role is excluded rather than payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting long-horizon capital-intensive execution from short-termist shareholder interference — was live at founding when the venture was single-purpose and thinly capitalized. Under this reading, the problem has become substantially dead as the entity has matured, gone public, and the founder's attention has fragmented across five-plus companies with resulting allocation conflicts the original structure never anticipated; the arrangement nonetheless persists and hardens (rising suppression_requirement, rising theater_ratio), which is the mandatrophy signature the tangled_rope classification is meant to catch rather than mislabeling this as pure coordination or pure natural-law inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    control_premium_vs_execution_value,
    'Is the valuation premium attributable to concentrated control best explained as compensation for execution-speed coordination value, or as capitalized private benefits of control extracted from minority shareholders?',
    'Event-study analysis of valuation changes around governance-structure announcements (charter amendments, committee independence changes, related-party disclosures) compared to peer companies with conventional one-share-one-vote governance in the same sector.',
    'If the premium tracks execution announcements rather than control-entrenchment events, the coordination story strengthens and the constraint drifts toward scaffold or rope; if it tracks entrenchment and related-party events, the extraction reading strengthens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(control_premium_vs_execution_value, empirical, 'Whether valuation premium reflects execution coordination value or capitalized control extraction.').

omega_variable(
    multi_entity_allocation_arbitration,
    'Is there any de facto arbitration mechanism (formal or informal) governing how Musk''s attention, IP, and corporate opportunities are allocated across his affiliated companies, even absent a formal cross-board process?',
    'Discovery in shareholder derivative litigation, SEC comment letters, or voluntary disclosure of any informal allocation framework or side agreements between the boards.',
    'Evidence of a substantive informal arbitration mechanism would reduce the asymmetric-extraction reading''s force even without formal governance; its absence corroborates the governance-skeptic reading that allocation is effectively unilateral.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multi_entity_allocation_arbitration, empirical, 'Whether informal cross-company allocation discipline exists despite the absence of formal governance structures.').

omega_variable(
    kernel_framing_choice,
    'The alternative framing to this reading treats the vote/equity split as a compensation instrument (paying Musk in control rights for irreplaceable execution capacity) rather than as a governance-legitimacy failure; both framings are coherent given the same facts, but they produce different classifications (tangled_rope here vs. something closer to scaffold or rope under the compensation framing).',
    'Compare against the real_options_technologist and dcf_fundamentalist sibling readings'' classifications once authored, and examine whether shareholder votes ratifying the compensation-instrument framing were made with full information about the accountability tradeoffs.',
    'If shareholders ratified the structure with genuine understanding and viable alternative options, the compensation framing gains force and the constraint may be better read as a negotiated (if lopsided) rope; if ratification was structurally coerced by controlled-company voting mechanics, the governance-skeptic framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether the vote/equity gap is better framed as compensation-for-execution or as governance-legitimacy failure — a framing choice this reading resolves toward the latter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__governance_skeptic, theater_ratio, 0, 0.2).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__governance_skeptic, theater_ratio, 4, 0.24).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__governance_skeptic, theater_ratio, 8, 0.29).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__governance_skeptic, theater_ratio, 12, 0.33).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__governance_skeptic, theater_ratio, 16, 0.36).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__governance_skeptic, theater_ratio, 20, 0.39).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__governance_skeptic, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__governance_skeptic, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__governance_skeptic, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__governance_skeptic, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__governance_skeptic, base_extractiveness, 12, 0.7).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__governance_skeptic, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__governance_skeptic, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__governance_skeptic, base_extractiveness, 24, 0.79).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__governance_skeptic, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__governance_skeptic, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__governance_skeptic, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__governance_skeptic, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__governance_skeptic, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__governance_skeptic, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__governance_skeptic, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(valuation_legitimacy__governance_skeptic, 0.08).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% This story is one of four constraint stories decomposing the natural-language claim 'the valuation is legitimate/illegitimate' per the ε-invariance principle. Each reading (dcf_fundamentalist, governance_skeptic, real_options_technologist, musk_cult_believer) evaluates a structurally distinct claim about what grounds valuation legitimacy, and each authors its own ε, beneficiary/victim structure, and classification against the same underlying entity. This reading (governance_skeptic) treats the dual-class/controlled-company structure itself as the extractive mechanism; it forecloses musk_cult_believer's premise (that track record alone substitutes for governance accountability) because the two cannot be jointly held — either governance protections are a precondition for legitimacy or personal track record substitutes for them, not both. It coexists with dcf_fundamentalist and real_options_technologist because those readings dispute the VALUATION METHOD question (cash flows vs. option value) which is orthogonal to and can be held alongside a governance-accountability critique.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
