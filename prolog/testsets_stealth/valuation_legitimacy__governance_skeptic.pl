% ============================================================================
% CONSTRAINT STORY: valuation_legitimacy__governance_skeptic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: valuation_legitimacy__governance_skeptic
 *   human_readable: Dual-Class Control Wedge Priced into Mega-Cap Valuation (Governance-Skeptic Reading)
 *   domain: economic/corporate governance/technology
 *
 * SUMMARY:
 *   This file instantiates the governance_skeptic reading of the
 *   valuation_legitimacy kernel. The standing arrangement under assessment —
 *   the epsilon referent — is the dual-class control structure of a mega-cap
 *   electric/autonomy manufacturer: 10:1 supervoting shares concentrating
 *   82.4% of votes in a founder holding 42% of equity, controlled-company
 *   exemptions waiving independent-committee requirements, a charter
 *   renouncing corporate-opportunity protections for the controller, and a
 *   ~$1.75T market price set under those terms. Assessed by this reading's
 *   own lights, the arrangement is substantially extractive: minority holders
 *   pay full price for claims whose governance content has been stripped,
 *   while the controller capitalizes private benefits of control into the
 *   headline valuation. Sibling readings (dcf_fundamentalist,
 *   real_options_technologist, musk_cult_believer) are separate constraints
 *   in separate files; nothing here averages across them. The claimed type
 *   and the metrics are authored independently: the claim is tangled_rope
 *   because a genuine insulation/coordination function persists alongside
 *   asymmetric extraction; the metrics describe heavily extractive, actively
 *   enforced operation.
 *
 * KEY AGENTS:
 *   - - elon_musk_control_block: agenda-setter and primary beneficiary (powerful/identity_locked) — holds 82.4% of votes on 42% of equity; sets board, compensation, and cross-company allocation
 *   - - early_class_b_holders: secondary beneficiaries (organized/arbitrage) — supervoting insiders whose votes are redundant but whose shares carry a saleable control premium
 *   - - class_a_public_shareholders: primary targets (powerless/mobile) — full-price capital providers with foreclosed voice; exit-by-sale is their only channel
 *   - - index_fund_stewardship_teams: trapped fiduciary targets (organized/trapped) — must hold under index mandates, litigate and vote against, lose every arithmetic contest
 *   - - retail_musk_aligned_holders: endorsing targets (powerless/identity_locked) — same economic terms as other Class A holders, opposite structural relationship
 *   - - tesla_board_of_directors: formal agenda-setter, dependent beneficiary (institutional/constrained) — approves under controlled-company waivers what the controller selects
 *   - - independent_board_candidates: excluded voices (moderate/constrained) — qualified directors who never reach a ballot
 *   - - proxy_advisory_firms: analytical observers (organized/analytical) — document the gap each cycle without power to close it
 *   - - delaware_chancery_court: institutional observer (institutional/analytical) — policed controller transactions until redomestication moved the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.74).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.58).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.74).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Control Wedge Priced into Mega-Cap Valuation (Governance-Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "economic/corporate governance/technology").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, '27383c04-4a2d-4a19-90ec-2c54822be982').
narrative_ontology:cs_kernel_codification('27383c04-4a2d-4a19-90ec-2c54822be982', distributed).
narrative_ontology:cs_authority_grounding('27383c04-4a2d-4a19-90ec-2c54822be982', expertise).
narrative_ontology:cs_interpretation_layer_present('27383c04-4a2d-4a19-90ec-2c54822be982').
narrative_ontology:cs_reading_relation('27383c04-4a2d-4a19-90ec-2c54822be982', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('27383c04-4a2d-4a19-90ec-2c54822be982', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('27383c04-4a2d-4a19-90ec-2c54822be982', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_axiom('27383c04-4a2d-4a19-90ec-2c54822be982', foundational, governance_rights_prerequisite_to_legitimate_price).
narrative_ontology:cs_axiom_status(governance_rights_prerequisite_to_legitimate_price, holdable).
narrative_ontology:cs_axiom_grounding('27383c04-4a2d-4a19-90ec-2c54822be982', governance_rights_prerequisite_to_legitimate_price, deontological).
narrative_ontology:cs_axiom('27383c04-4a2d-4a19-90ec-2c54822be982', secondary, control_premium_shared_pro_rata).
narrative_ontology:cs_axiom_status(control_premium_shared_pro_rata, holdable).
narrative_ontology:cs_axiom_grounding('27383c04-4a2d-4a19-90ec-2c54822be982', control_premium_shared_pro_rata, conventional).
narrative_ontology:cs_reference_frame('27383c04-4a2d-4a19-90ec-2c54822be982', proportionate_control_fiduciary_baseline).
narrative_ontology:cs_drift_state('27383c04-4a2d-4a19-90ec-2c54822be982', concentrated_control_megacap_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('27383c04-4a2d-4a19-90ec-2c54822be982', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, elon_musk_control_block).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, tesla_board_of_directors).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, index_fund_stewardship_teams).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, retail_musk_aligned_holders).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, agency_cost_prediction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds roughly 42% of the equity carrying 82.4% of the votes through 10:1 supervoting shares. Sets board composition, approves his own compensation through a board he nominates, and allocates opportunities — factory capacity, AI talent, capital priorities — across the five-plus companies he controls, under a charter that renounces corporate-opportunity claims against him. His public identity and stated life project are fused with these companies; stepping back would unravel the legend the holdings rest on.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, elon_musk_control_block, agenda_setter,
    powerful, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, elon_musk_control_block, beneficiary).

% Founders, early investors, and affiliated funds holding supervoting shares. Their votes are redundant behind the controller's bloc, but the shares carry a saleable premium in any control event, and they enjoy the same insulation of the long-term agenda without carrying its decisions.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    organized, biographical, arbitrage, global).

% Buy common shares at full market price carrying one vote apiece against the controller's ten. No director they prefer can reach a ballot, pay packages reach them as ratified facts, and their sole lever is selling — which surrenders the position and realizes any governance discount personally.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    powerless, biographical, mobile, global).

% Hold Class A blocks on behalf of retirement savers under mandates that track indices, so divesting would mean leaving the benchmark. They run proxy campaigns against directors and pay packages, pursue derivative litigation, and publish voting rationales — and lose every contest by fixed arithmetic before it is called.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, index_fund_stewardship_teams, payer,
    organized, generational, trapped, global).

% Atomized individual holders who vote their small stakes with the controller, attend annual meetings as supporters, and treat concentrated control as the point of the investment. They carry identical economic terms to other common holders but would oppose the reforms other holders seek.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, retail_musk_aligned_holders, payer,
    powerless, immediate, identity_locked, global).

% Formally approves executive compensation, nominates successor directors, and signs off on dealings with affiliates. Operates under controlled-company exemptions that waive independence requirements its listing would otherwise demand; members are nominated by the controller and their fees and tenure depend on his continued support.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, tesla_board_of_directors, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, tesla_board_of_directors, beneficiary).

% Experienced directors and governance professionals willing to serve on a genuinely independent nominating or compensation committee. None appears on a ballot, because nominations run through the controller's gate; their availability is never tested.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, independent_board_candidates, excluded,
    moderate, biographical, constrained, national).

% Issue recommendations each cycle against the pay package and parts of the director slate, publish the reasoning, and see the recommendations overridden by the vote count. They shape disclosure norms and record the recurring gap between recommendation and outcome.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, proxy_advisory_firms, observer,
    organized, biographical, analytical, global).

% Adjudicated the derivative challenge that voided the 2018 compensation grant and applied entire-fairness review to controller transactions. The company's redomestication to Texas in 2024 removed future controller disputes from its docket, relocating the enforcement boundary the structure operates behind.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, delaware_chancery_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, elon_musk_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real collective problem: raising tens of billions from dispersed public capital for decade-long industrial bets — gigascale manufacturing, autonomy, launch-adjacent supply chains — while insulating the allocation agenda from quarterly-market discipline and hostile accumulation. The dual-class wedge is the instrument: public money flows in while the multi-year decision right stays in one place.
% TRANSFER_FUNCTION: Moves voting control and agenda-setting power from all capital providers to the founder block (82.4% of votes on 42% of equity); moves capital from public shareholders into the company; and moves private benefits of control — compensation-setting outcomes, cross-company opportunity allocation under a waived corporate-opportunity doctrine, and the control premium capitalized into the share price — toward the controller and early Class B holders.
% ABSENT_VOICES: Independent director candidates never reach a ballot because nomination runs through the controller's gate; would-be activist accumulators are deterred by the arithmetic of 10:1 shares; future shareholders were unrepresented when the charter terms were set; employees compensated in Class A equity hold economic exposure with zero voice. They sit outside the room the charter built.
% DISAPPEARANCE_RATIONALE: Overnight conversion to one-share-one-vote would rearrange the governance economy immediately: board composition would reset at the next election, compensation would require genuinely independent committee approval, related-party dealings across the controller's companies would face full fiduciary scrutiny, and the control premium currently capitalized into the price would redistribute to all holders pro rata. The operating business continues; the rent distribution does not survive the night.
% FOUNDING_PROBLEM: The 2010-era problem: a capital-hungry industrial founder needed public markets' scale but feared the fate of founders diluted into irrelevance or ousted by boards after early stumbles. The dual-class structure was built to admit public capital while guaranteeing the founder's multi-decade agenda could not be outvoted.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: institutional investor policy bodies (ISS, Glass Lewis, CII) attest the insulation rationale is legitimate for immature, capital-starved firms but exhausted for a profitable mega-cap, citing the dual-class sunset literature; the Delaware chancery's Tornetta framing treated the controller's dominance as the problem to be policed rather than the solution. The controller and aligned holders attest the problem is live; no source outside the benefiting parties attests continued liveness at current scale — the attestation split is itself the signal.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.74, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is high (0.74 at interval end) because the vote/equity wedge (82.4% vs 42%), compensation set without an independent committee, cross-company opportunity allocation under a waived corporate-opportunity doctrine, and the control premium embedded in the $1.75T price all flow one way. Suppression (0.58) operates on voice, not entry: buying in is voluntary, but inside the arrangement every voice channel is foreclosed — advisory-only votes, gatekept nominations, forum relocation to escape adverse precedent — and index funds cannot even exit. Theater (0.42) grows as annual meetings become rallies and say-on-pay becomes ritual with predetermined outcomes. Accessibility_collapse is low-moderate (0.35): alternatives persist outside the asset (other equities, other exposures), but within the asset none exist — there is no way to hold this company with governance rights. Resistance is substantial (0.62): the Tornetta trial win, recurring ISS/Glass Lewis opposition, and institutional campaigns are real and repeated, though arithmetically futile. The temporal series run on one shared grid (2010/2013/2016/2019/2022/2025) with every tracked metric authored at every point; trajectories are monotonic rising, reflecting enforcement hardening (forum selection, Texas redomestication) layered onto maturing extraction rather than cyclical oscillation.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the same structure reads as the coordination device that made the capital program possible; from the trapped index-fund seat it reads as an arithmetic impossibility — every contest lost before filed; from the retail-aligned seat it reads as rightful order; from the mobile Class A holder it reads as a discount to be sold into. Same asset, same meeting, four different lived arrangements. The engine computes per-seat classifications from the power/exit/role data; the divergence between the powerful identity-locked beneficiary and the trapped organized fiduciary is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the controller block and early Class B holders toward the subsidy end (d near 0): the structure pays them in votes, insulation, and private benefits. Class A public shareholders derive high d from the payer declaration plus mobile-but-voiceless exit — exit exists but purchased voice does not, which keeps them near the target end rather than the arbitrage end. Index-fund stewardship teams are pushed further toward the full-target end than their organized power alone would suggest, because trapped exit removes the damping that mobile exit provides. Retail-aligned holders derive high d from their payer role, but their endorsement of the arrangement lowers effective grievance below the derived value; the schema keys overrides by power atom, which cannot separate them from other powerless seats in this story, so the divergence is documented in the retail_acquiescence_mechanism omega rather than forced through a mis-specified override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — insulate a capital-starved long-horizon builder from short-term markets — has decayed as the firm matured into a profit-generating mega-cap, while the structure's extraction has grown: the classic mandatrophy signature, carried in the R5 fields with status 'contested'. Resolving the type as tangled_rope rather than snare preserves the genuine residual coordination (public capital at scale still flows through the structure, and insulation may remain load-bearing for current autonomy and robotics bets); resolving it as anything purer would mislabel either the coordination or the extraction. The corroboration split along beneficiary lines is itself the capture signal the mismatch consumer reads: dead-or-dying mandate plus world_rearranges persistence flags the zombie structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    valuation_legitimacy_kernel_reading,
    'This constraint is the governance_skeptic reading of the valuation_legitimacy kernel; which structural elements would the sibling readings relocate or dissolve?',
    'Author the three sibling files and compare victim sets, epsilon referents, and computed classifications; the disagreement locates in whether governance structure enters the legitimacy function at all.',
    'Adopting dcf_fundamentalist relocates epsilon to cash-flow proof status and shrinks the harmed set to overpayers for unproven assets; adopting musk_cult_believer dissolves the harmed set entirely (disenfranchisement becomes legitimacy-neutral); adopting real_options_technologist recasts the control wedge as option-preserving infrastructure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(valuation_legitimacy_kernel_reading, conceptual, 'Kernel membership: one of four readings; the structural delta is the location of valuation legitimacy.').

omega_variable(
    private_benefit_share_of_price,
    'What fraction of the $1.75T price is private benefits of control versus standalone enterprise value?',
    'Event studies around governance shocks (pay-package rulings, redomestication), related-party contract disclosures, and sum-of-the-parts comparison against equivalently scaled firms with dispersed ownership.',
    'A large private-benefit share confirms the extraction reading and supports drift toward snare; a negligible share would push this reading toward the technologist sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_benefit_share_of_price, empirical, 'Decomposition of the headline valuation into control rents and enterprise value.').

omega_variable(
    insulation_function_liveness,
    'Is the founder-insulation function still load-bearing for current capital allocation, or has it decayed into pure entrenchment?',
    'Counterfactual comparison against dual-class peers that adopted sunset clauses post-maturity: did one-share-one-vote conversion alter their long-horizon investment programs?',
    'If insulation remains load-bearing, tangled_rope holds; if decayed, the coordination story is cover and the constraint drifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insulation_function_liveness, conceptual, 'Liveness of the founding coordination function at current company maturity.').

omega_variable(
    cross_company_allocation_conflict,
    'Does the controller allocate opportunities — Terafab-class capacity, AI talent, capital priorities — across his five-plus companies to maximize his aggregate portfolio rather than any single shareholder body?',
    'Intercompany contract disclosure, funding-source tracing, and comparison of terms offered to affiliates versus third parties.',
    'Confirmed portfolio-maximizing allocation raises effective extraction on Class A holders above the authored epsilon; arm''s-length evidence lowers it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_company_allocation_conflict, empirical, 'Structural conflict from multi-company control combined with a waived corporate-opportunity doctrine.').

omega_variable(
    redomestication_enforcement_effect,
    'Did the 2024 redomestication to Texas materially raise the cost of fiduciary enforcement against controller transactions?',
    'Compare pre- and post-reincorporation litigation outcomes, applicable statutory standards, and filing patterns for controller-related disputes.',
    'If enforcement costs rose materially, the suppression series understates the end-state and its trajectory steepens; if substitutable forums exist, suppression flattens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redomestication_enforcement_effect, empirical, 'Enforcement-boundary relocation as suppression intensification.').

omega_variable(
    retail_acquiescence_mechanism,
    'Is Class A retail alignment with the controller consent, identity fusion, or learned helplessness under arithmetic futility?',
    'Voting-behavior analysis across proposal types, post-disappointment holding behavior, and survey instruments on holder motivation.',
    'If identity fusion dominates, part of the measured quiescence is internalized rather than structural, and feasible reform coalitions are smaller than ownership arithmetic suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retail_acquiescence_mechanism, conceptual, 'Mechanism behind victim-seat acquiescence despite mobile exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t2010, valuation_legitimacy__governance_skeptic, theater_ratio, 2010, 0.2).
narrative_ontology:measurement_basis(valu_tr_t2010, observed).
narrative_ontology:measurement(valu_tr_t2013, valuation_legitimacy__governance_skeptic, theater_ratio, 2013, 0.24).
narrative_ontology:measurement_basis(valu_tr_t2013, observed).
narrative_ontology:measurement(valu_tr_t2016, valuation_legitimacy__governance_skeptic, theater_ratio, 2016, 0.28).
narrative_ontology:measurement_basis(valu_tr_t2016, observed).
narrative_ontology:measurement(valu_tr_t2019, valuation_legitimacy__governance_skeptic, theater_ratio, 2019, 0.33).
narrative_ontology:measurement_basis(valu_tr_t2019, observed).
narrative_ontology:measurement(valu_tr_t2022, valuation_legitimacy__governance_skeptic, theater_ratio, 2022, 0.38).
narrative_ontology:measurement_basis(valu_tr_t2022, observed).
narrative_ontology:measurement(valu_tr_t2025, valuation_legitimacy__governance_skeptic, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(valu_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(valu_be_t2010, valuation_legitimacy__governance_skeptic, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement_basis(valu_be_t2010, observed).
narrative_ontology:measurement(valu_be_t2013, valuation_legitimacy__governance_skeptic, base_extractiveness, 2013, 0.5).
narrative_ontology:measurement_basis(valu_be_t2013, observed).
narrative_ontology:measurement(valu_be_t2016, valuation_legitimacy__governance_skeptic, base_extractiveness, 2016, 0.56).
narrative_ontology:measurement_basis(valu_be_t2016, observed).
narrative_ontology:measurement(valu_be_t2019, valuation_legitimacy__governance_skeptic, base_extractiveness, 2019, 0.62).
narrative_ontology:measurement_basis(valu_be_t2019, observed).
narrative_ontology:measurement(valu_be_t2022, valuation_legitimacy__governance_skeptic, base_extractiveness, 2022, 0.68).
narrative_ontology:measurement_basis(valu_be_t2022, observed).
narrative_ontology:measurement(valu_be_t2025, valuation_legitimacy__governance_skeptic, base_extractiveness, 2025, 0.74).
narrative_ontology:measurement_basis(valu_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t2010, valuation_legitimacy__governance_skeptic, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement_basis(valu_su_t2010, observed).
narrative_ontology:measurement(valu_su_t2013, valuation_legitimacy__governance_skeptic, suppression_requirement, 2013, 0.34).
narrative_ontology:measurement_basis(valu_su_t2013, observed).
narrative_ontology:measurement(valu_su_t2016, valuation_legitimacy__governance_skeptic, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement_basis(valu_su_t2016, observed).
narrative_ontology:measurement(valu_su_t2019, valuation_legitimacy__governance_skeptic, suppression_requirement, 2019, 0.46).
narrative_ontology:measurement_basis(valu_su_t2019, observed).
narrative_ontology:measurement(valu_su_t2022, valuation_legitimacy__governance_skeptic, suppression_requirement, 2022, 0.52).
narrative_ontology:measurement_basis(valu_su_t2022, observed).
narrative_ontology:measurement(valu_su_t2025, valuation_legitimacy__governance_skeptic, suppression_requirement, 2025, 0.58).
narrative_ontology:measurement_basis(valu_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, musk_cult_believer).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'what makes the $1.75T legitimate?' decomposes, per the epsilon-invariance principle, into four structurally distinct constraints sharing the valuation_legitimacy kernel — one per reading. Each member authors its own epsilon over the same standing arrangement (the dual-class-controlled mega-cap) assessed by its own lights: dcf_fundamentalist measures cash-flow proof status, real_options_technologist measures option-space preservation, musk_cult_believer measures founder-track-record credibility, and this file measures governance-structure protection of minority holders. The governance reading sits upstream of the dcf reading in practice (governance discounts feed discount-rate and cash-flow-attribution debates) and stands in logical contradiction to the cult-believer reading. All four files link one another via affects_constraints; orphaning any member would break contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
