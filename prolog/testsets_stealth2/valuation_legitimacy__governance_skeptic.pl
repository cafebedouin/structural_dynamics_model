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
 *   human_readable: Dual-Class Control Structure as Valuation-Legitimacy Defect (Governance-Skeptic Reading)
 *   domain: economic/corporate_governance/technology
 *
 * SUMMARY:
 *   A founder-controlled public company carries a $1.75T valuation built on a
 *   dual-class structure: the controller holds 82.4% of votes on 42% of
 *   equity via 10:1 supervoting shares, governed under controlled-company
 *   exemptions with no independent compensation or nominating committee, a
 *   charter that routes corporate opportunities among the controller's
 *   five-plus ventures at his discretion, and board processes that have
 *   approved successive record compensation packages. This file instantiates
 *   the governance_skeptic reading of the valuation_legitimacy kernel: the
 *   claim is that valuation legitimacy requires structures protecting
 *   minority shareholders, and that the vote/equity gap prices controller
 *   private benefits, not public shareholder value. Per the epsilon referent
 *   rule, extractiveness (0.76) is authored for the STANDING arrangement as
 *   this reading assesses it - not for the reformed, proportionate-governance
 *   arrangement this reading would endorse (which would score near zero by
 *   construction). The colloquial question 'is the valuation legitimate?'
 *   decomposes into four structurally distinct constraints (this reading plus
 *   dcf_fundamentalist, real_options_technologist, musk_cult_believer); each
 *   gets its own file, its own epsilon, and its own classification, linked
 *   through network.affects_constraints. Claim and metrics are independent
 *   authored facts: the tangled_rope claim states the structure's dual nature
 *   as this reading sees it; the metrics describe its actual operation; the
 *   engine computes per-seat types and any divergence is the measurement the
 *   corpus exists to take.
 *
 * KEY AGENTS:
 *   - musk_control_block: agenda-setting controller (institutional/arbitrage) - holds 82.4% of votes on 42% of equity; sets board composition, approves own compensation via appointed directors, routes opportunities across ventures; collects the arrangement's gains directly
 *   - class_a_public_shareholders: primary target (powerless/mobile) - buy one-vote shares at full price; each dollar buys roughly one-fifth the controller's voting weight per dollar; exit by sale is open but the price already embeds the governance terms
 *   - institutional_index_funds: trapped target (organized/trapped) - hold by index mandate, cannot sell without breaking tracking; wield voice (recommendations, votes against) that passes nothing against the bloc
 *   - early_class_b_holders: secondary beneficiary (powerful/arbitrage) - insiders and early backers riding supervoting economics; can convert and monetize gradually at controller-set governance prices
 *   - captive_board_directors: enforcing beneficiary (moderate/constrained) - seated through a nominating process the controller owns; approve related-party terms; departure forfeits compensation and standing
 *   - rival_bidders_activists: excluded challenger (powerful/trapped) - would pay a control premium or run contesting slates; the supervoting arithmetic makes every path hopeless, removing the market-for-control check
 *   - passive_fund_beneficial_owners: excluded cost-bearer (powerless/trapped) - savers behind index products; bear diluted governance and controller-favorable transfers with no direct channel to the company
 *   - proxy_advisors_iss_glass_lewis: analytical observer (institutional/analytical) - publish recommend-against campaigns and governance ratings; influence stops at recommendation
 *   - delaware_chancery_court: adjudicating observer (institutional/analytical) - voided one compensation award for process failure, watched it re-ratified under new process; rulings reshape terms without touching the vote ratio
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(valuation_legitimacy__governance_skeptic, 0.76).
domain_priors:suppression_score(valuation_legitimacy__governance_skeptic, 0.62).
domain_priors:theater_ratio(valuation_legitimacy__governance_skeptic, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, extractiveness, 0.76).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(valuation_legitimacy__governance_skeptic, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(valuation_legitimacy__governance_skeptic, tangled_rope).
narrative_ontology:human_readable(valuation_legitimacy__governance_skeptic, "Dual-Class Control Structure as Valuation-Legitimacy Defect (Governance-Skeptic Reading)").
narrative_ontology:topic_domain(valuation_legitimacy__governance_skeptic, "economic/corporate_governance/technology").

domain_priors:requires_active_enforcement(valuation_legitimacy__governance_skeptic).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(valuation_legitimacy__governance_skeptic, '0ef68d7f-1528-4e1d-9f8c-5e59274e02b8').
narrative_ontology:cs_kernel_codification('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', formalized).
narrative_ontology:cs_authority_grounding('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', expertise).
narrative_ontology:cs_interpretation_layer_present('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8').
narrative_ontology:cs_reading_relation('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', valuation_legitimacy__dcf_fundamentalist, coexists_with).
narrative_ontology:cs_reading_relation('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', valuation_legitimacy__real_options_technologist, coexists_with).
narrative_ontology:cs_reading_relation('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', valuation_legitimacy__musk_cult_believer, forecloses).
narrative_ontology:cs_axiom('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', foundational, control_rights_must_track_capital_at_risk).
narrative_ontology:cs_axiom_status(control_rights_must_track_capital_at_risk, holdable).
narrative_ontology:cs_axiom_grounding('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', control_rights_must_track_capital_at_risk, deontological).
narrative_ontology:cs_axiom('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', secondary, controller_transactions_require_disinterested_approval).
narrative_ontology:cs_axiom_status(controller_transactions_require_disinterested_approval, holdable).
narrative_ontology:cs_axiom_grounding('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', controller_transactions_require_disinterested_approval, conventional).
narrative_ontology:cs_reference_frame('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', capital_proportional_governance).
narrative_ontology:cs_drift_state('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', post_tornetta_chancery_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0ef68d7f-1528-4e1d-9f8c-5e59274e02b8', '').
narrative_ontology:cs_kernel_id(valuation_legitimacy__governance_skeptic, valuation_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, musk_control_block).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, early_class_b_holders).
narrative_ontology:constraint_beneficiary(valuation_legitimacy__governance_skeptic, captive_board_directors).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, class_a_public_shareholders).
narrative_ontology:constraint_victim(valuation_legitimacy__governance_skeptic, institutional_index_funds).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, controlled_company_exemption_doctrine).
narrative_ontology:constraint_vindicates(valuation_legitimacy__governance_skeptic, founder_vision_premium_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founder-controller holding supervoting shares that carry 82.4% of votes on 42% of equity. Sets board composition through a nominating process he controls, sees compensation packages approved by directors he seated, and routes corporate opportunities among his ventures under charter provisions written for that purpose. Operating under controlled-company exemptions, he faces no independent committee checks. Exit for him means unwinding the control block itself; instead he can reorganize, reincorporate, or take entities private on his own timing.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, musk_control_block, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, musk_control_block, beneficiary).

% Insiders and early backers holding pre-public supervoting stock whose votes ride alongside the founder's bloc. They receive the same per-share economics as public buyers while contributing none of the public float's liquidity provision, and their votes decay only on transfer. Converting or selling gradually lets them monetize at prices set under founder-controlled governance terms they helped write.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, early_class_b_holders, beneficiary,
    powerful, biographical, arbitrage, global).

% Directors seated through a nominating pipeline the controller effectively owns, compensated chiefly in equity awards and fees contingent on continued service. They approve compensation packages and related-party transactions; several carry personal or professional ties to the controller. Several also bear personal fiduciary exposure when process failures reach litigation. Resigning means forfeiting compensation, standing, and the professional identity of the seat.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, captive_board_directors, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(valuation_legitimacy__governance_skeptic, captive_board_directors, agenda_setter).

% Public buyers of one-vote shares at full market price. Each dollar invested buys roughly one-fifth the voting weight per dollar that the controller's shares carry. They may attend meetings and cast votes, but no proposal, director slate, or charter amendment passes without the controller's bloc. Selling is always available; the price received already reflects the governance terms, so exit realizes the discount rather than escaping it.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, class_a_public_shareholders, payer,
    powerless, biographical, mobile, global).

% Asset managers holding the stock because index construction mandates it. Selling would break tracking commitments owed to their own clients, so their leverage is voice: engagement letters, proxy recommendations, votes against directors and packages. The historical record shows votes against passing nothing while the supervoting bloc holds; they absorb governance costs on behalf of millions of underlying savers.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, institutional_index_funds, payer,
    organized, generational, trapped, global).

% Savers whose retirement money sits inside index products holding the stock. They bear the diluted governance weight and any controller-favorable transfers embedded in the price, but have no direct channel: they cannot instruct votes, their names never reach the company, and their only lever is slow fee pressure on the fund managers above them.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, passive_fund_beneficial_owners, excluded,
    powerless, generational, trapped, global).

% Acquirers and activist funds who would pay a control premium, run contesting director slates, or force strategic restructurings at comparable public companies. The supervoting arithmetic makes tender offers, proxy fights, and mergers arithmetically hopeless regardless of price offered, so they stand outside the transaction entirely; their exclusion removes the market-for-control discipline that governs peer firms.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, rival_bidders_activists, excluded,
    powerful, biographical, trapped, global).

% Advisory firms publishing vote recommendations and governance-quality ratings used by institutional holders. They recommend against the compensation packages and flag the gap between voting weight and economic exposure; issuers respond with engagement campaigns rather than structural change. Their influence terminates at the recommendation - they hold no votes of their own.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, proxy_advisors_iss_glass_lewis, observer,
    institutional, biographical, analytical, global).

% The court adjudicating fiduciary disputes inside the incorporation jurisdiction. It voided one historic compensation award for flawed process, then watched a substantially similar package re-ratified under freshly constituted process, and now operates alongside statutes shielding controller transactions from certain challenges. Its rulings reshape the terms of individual deals without altering the underlying vote ratio.
narrative_ontology:constraint_stakeholder(valuation_legitimacy__governance_skeptic, delaware_chancery_court, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(valuation_legitimacy__governance_skeptic, musk_control_block).
narrative_ontology:fixing_cost_class(valuation_legitimacy__governance_skeptic, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrates irreversible, decade-scale capital-allocation decisions - launch systems, fabrication capacity, compute buildouts - in a single agent insulated from quarterly earnings discipline and hostile takeover, solving the problem of impatient public capital forcing premature abandonment of long-horizon programs that diversified holders might rationally veto term-by-term.
% TRANSFER_FUNCTION: Moves voting control out of proportion to capital: public money buys one vote per share while controller money buys ten, so 58% of the economic interest commands 17.6% of the votes. Separately, board processes the bloc appoints move compensation and inter-company opportunity allocations toward the control block and its affiliates.
% ABSENT_VOICES: Two seats would object and have no place in the conversation: passive fund beneficial owners, who bear the transfers inside products they cannot individually exit, and rival bidders and activists, whom the supervoting arithmetic locks out of offering any control premium at all. Proxy advisors speak adjacent to the room but hold no votes; their objections arrive as letters, not outcomes.
% DISAPPEARANCE_RATIONALE: If the supervoting structure vanished overnight, control would reprice to economic ownership: boards would reconstitute through independent nominating processes, compensation would face arm's-length negotiation, related-party terms would require disinterested approval, and rival bids would become arithmetically possible again. The mission programs would continue, but the valuation would redistribute from controller private benefits toward pro-rata shareholder value, and every governance decision would become contestable.
% FOUNDING_PROBLEM: Built to keep a capital-starved founder's long-horizon program alive against short-termist markets: early near-insolvency, credible hostile-takeover risk, and quarterly pressure to abandon decade-scale technology bets before they matured.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: Delaware Chancery opinions in the fiduciary-litigation line treat the original defensive need as historically genuine while judging current processes as falling below fiduciary baselines; proxy-advisor policy papers and institutional-investor coalitions (governance-rating publications, pension-system letters, unified-stewardship statements) attest that the defensive rationale no longer covers the present structure at maturity; the law-and-economics dual-class literature documents that the founding hazard declines with firm age and argues for sunset mechanisms. No source outside the benefiting parties attests that the founding hazard persists at its original intensity.
narrative_ontology:disappearance_verdict(valuation_legitimacy__governance_skeptic, world_rearranges).
narrative_ontology:founding_problem_status(valuation_legitimacy__governance_skeptic, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(valuation_legitimacy__governance_skeptic, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(valuation_legitimacy__governance_skeptic, 'none', 1).
narrative_ontology:epsilon_provenance(valuation_legitimacy__governance_skeptic, 0.76, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.76: the referent is the standing dual-class arrangement assessed by this reading's lights - votes decoupled from capital at a 10:1 ratio, self-dealing-prone compensation processes, and discretionary cross-venture opportunity routing are exactly the transfers agency theory predicts from ungoverned control. Suppression 0.62: the arrangement suppresses VOICE, not EXIT - no alternative board, bidder, or proposal can succeed while the bloc holds, and controlled-company exemptions remove the independence checks that would otherwise bind; but selling remains open, which keeps suppression below snare-grade. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled (by directionality and scope) in the engine's computation. Theater_ratio 0.46 with an oscillating series: ratification rituals (re-approved packages, say-on-pay passes, independence certifications) are performative, while strategic decisions are real; the dip at t=16 documents the litigation cycle - adverse ruling, re-ratification under fresh process, partial theater reset, rebuild. Accessibility_collapse 0.45: alternatives do not fully collapse once the structure is understood - holders can sell, litigate (and did win), and engage; the collapse is confined to governance alternatives specifically. Resistance 0.60: sustained and occasionally successful - a landmark fiduciary victory, persistent advisor recommend-against campaigns, accumulating say-on-pay dissent, and legislative counter-pressure. All three tracked metrics run on ONE shared time grid ({0,4,8,12,16,20,24}); later points are marked projected because the terminal valuation state is scenario-forward. Identity-lock note: captive directors exhibit professional/institutional identity fusion - board-seat career dependence makes resignation unthinkable independent of fee calculus; retail holders' quiescence has a possible internalized-charisma component routed to an omega rather than asserted.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the structure as mission-protective infrastructure it built and maintains: insulation from quarterly discipline is, from that seat, the entire point. The payer seats experience the same structure as voicelessness: full-price capital with one-fifth proportional vote and no winnable contest. Same-side divergence: retail Class A holders are mobile (sell and exit) while index funds are trapped (mandate-bound), so identical economic exposure computes different directionalities and different per-seat types - the engine derives this from exit options, not from labels. Inter-institutional divergence: proxy advisors and the chancery court sit outside the arrangement with analytical exit; their findings reshape terms at the margin while leaving the vote ratio untouched. The captive-director seat straddles: collecting fees contingent on the arrangement while bearing personal fiduciary exposure when process fails. No authored claim adjudicates these gaps; the structural data lets the engine compute them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the control block (agenda-setter and collector, arbitrage exit) sits nearest the beneficiary end; early Class B holders similarly. Captive directors derive mid-low d - they collect but bear liability and constrained exit. Victims drive high d: retail Class A holders near the target end, with mobile exit DAMPING effective extraction somewhat (they can leave, at a price that already discounts the governance terms); index funds near the target end with trapped exit AMPLIFYING it (mandate-bound holders cannot arbitrage away the terms). Excluded seats (rival bidders, passive beneficial owners) feed the suppression picture rather than directionality - their exclusion is what the enforcement machinery maintains. Global spatial scope raises verification difficulty, modestly amplifying effective extraction engine-side. No directionality_overrides are authored: the power atoms are distinct across seats and the derivation from beneficiary/victim declarations plus exit options already tracks the true relationships; an override would second-guess data the structural layer encodes correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - keeping a capital-starved founder's decade-scale program alive against short-termist markets and hostile takeover - is CONTESTED, not dead: the defensive rationale was real at founding and is attested as historically genuine by sources outside the benefiting parties, while governance-skeptic evidence shows the present structure exceeds any surviving defensive need. The R5 mismatch consumer therefore sees status=contested paired with verdict=world_rearranges - no dead-mandate zombie flag fires yet. But the drift indicators point one way: extraction accumulates monotonically while the defensive rationale matures out, and theater oscillates around ratification rituals that normalize each prior violation. If the mission programs reach steady state and the coordination function atrophies while the ritual layer persists, the structure trends toward inertial maintenance administered by a seat that could change it but bears less cost than fixing would return. The tangled_rope claim prevents mislabeling in both directions: a pure-snare reading would erase the genuine patient-capital coordination that funded programs diversified quarterly-disciplined capital would have killed; a pure-rope reading would erase the measured, actively enforced transfer from capital providers to the control block. The engine's per-seat computation keeps both truths load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (governance_skeptic) of the valuation_legitimacy kernel - which sibling reading''s structural premises, if adopted instead, would dissolve this constraint into a different one?',
    'Compile the sibling stories (dcf_fundamentalist, real_options_technologist, musk_cult_believer) and compare per-seat classifications; the disagreement is located in the premise of what confers valuation legitimacy - structural minority protection (this reading) versus discounted cash flows, technological option space, or founder track record.',
    'Under dcf_fundamentalist or real_options_technologist, the vote/equity gap becomes a priced risk factor rather than a legitimacy defect; under musk_cult_believer it becomes evidence of deserved authority. Victim and beneficiary sets stay fixed across readings; epsilon and classification move.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: this file instantiates one reading of a four-reading kernel; sibling readings are separate constraints, not folded into this one.').

omega_variable(
    sibling_epsilon_divergence,
    'How much of the measured extraction survives when the same standing arrangement (dual-class control, controlled-company exemptions, cross-venture opportunity routing) is assessed under each sibling reading''s own lights?',
    'Author the three sibling stories over the identical referent and stakeholder surface, then compare engine-computed chi per seat across the family.',
    'If sibling epsilons diverge widely, the corpus gains a clean measurement of reading-indexed valuation disagreement; if they converge, the governance defect is reading-robust and the kernel contest is narrower than the discourse suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_epsilon_divergence, conceptual, 'Epsilon is a property of a reading over a fixed referent; this omega tracks the expected spread across the kernel family.').

omega_variable(
    coordination_vs_rent_balance,
    'What share of the structure''s persistence serves genuine patient-capital coordination (insulation of decade-scale programs from quarterly discipline and hostile takeover) versus enforced preservation of controller rents?',
    'Natural experiments at dual-class sunset conversions and controlled-company exemption lapses: compare program continuity, capital-allocation quality, and minority-holder returns before and after voice equalization.',
    'A high coordination share supports the tangled_rope claim and cautions against abolition remedies; a low share shifts the computed classification toward snare for the payer seats and supports mandatory sunset or conversion regimes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_rent_balance, empirical, 'Balance between the arrangement''s real coordination function and its rent-preservation function.').

omega_variable(
    private_benefits_magnitude,
    'What is the quantified magnitude of controller private benefits - above-arm''s-length compensation, inter-company opportunity capture, valuation premium attributable to control - relative to the cost borne by Class A holders?',
    'Fiduciary-litigation discovery of compensation deliberations, event studies around related-party and cross-venture allocation announcements, and comparison of controller pay against matched independent-committee benchmarks.',
    'Large quantified private benefits raise effective extraction for every trapped seat and strengthen the extraction-not-value-creation premise; small benefits would support a founder-compensation framing and lower epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_benefits_magnitude, empirical, 'Size of the transfer the vote/equity gap enables.').

omega_variable(
    attention_allocation_conflict,
    'Does the controller''s division of time across five or more ventures impose a net uncompensated cost on Class A holders, or is attention allocation efficiently priced - and who decides allocation when ventures compete (e.g., fab capacity benefiting one venture over another)?',
    'Audit of cross-venture capital, talent, and opportunity flows against arm''s-length counterfactuals; track allocation decisions where ventures'' interests diverge.',
    'If allocation systematically favors controller-affiliated ventures without disinterested approval, the conflict component of extraction rises and the no-independent-committee structure becomes the binding mechanism; if allocation is efficient, part of the measured extraction is misattributed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attention_allocation_conflict, empirical, 'Structural conflict from divided controller attention across ventures.').

omega_variable(
    internalized_charisma_quiescence,
    'Is Class A holder quiescence purely structural (arithmetic vote impotence: no proposal passes without the controller) or partly internalized (charismatic deference that suppresses objection even where voice exists, e.g., say-on-pay and director elections)?',
    'Post-reform trajectory test: if governance structures were equalized and retail dissent remained suppressed relative to comparable issuers, the internalized component is real; track dissent-vote deltas when charismatic framing breaks.',
    'If internalized, effective suppression exceeds the structural measure - holders carry the deference into reformed arrangements - and remediation requires more than charter change; if purely structural, equalizing votes dissolves the quiescence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_charisma_quiescence, empirical, 'Structural versus internalized mechanism behind minority-holder inaction.').

omega_variable(
    stewardship_coalition_potential,
    'Can a coalition of index-fund stewards, proxy advisors, and organized retail holders convert voice into structural change (sunset clauses, independent committees) despite the controller''s 82.4% bloc?',
    'Track universal-owner initiatives, say-on-pay dissent accumulation, litigation co-funding, and listing-standard or statutory reform campaigns; measure whether any channel forces charter renegotiation.',
    'If coalition channels are exhausted, the payer seats'' powerlessness is confirmed and resistance stays reactive; if a channel works, the constraint''s enforcement cost rises and drift toward sunset-based resolution becomes plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stewardship_coalition_potential, preference, 'Whether dispersed victims can assemble actionable coalition power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(valuation_legitimacy__governance_skeptic, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(valu_tr_t0, valuation_legitimacy__governance_skeptic, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(valu_tr_t0, observed).
narrative_ontology:measurement(valu_tr_t4, valuation_legitimacy__governance_skeptic, theater_ratio, 4, 0.26).
narrative_ontology:measurement_basis(valu_tr_t4, observed).
narrative_ontology:measurement(valu_tr_t8, valuation_legitimacy__governance_skeptic, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(valu_tr_t8, observed).
narrative_ontology:measurement(valu_tr_t12, valuation_legitimacy__governance_skeptic, theater_ratio, 12, 0.4).
narrative_ontology:measurement_basis(valu_tr_t12, observed).
narrative_ontology:measurement(valu_tr_t16, valuation_legitimacy__governance_skeptic, theater_ratio, 16, 0.34).
narrative_ontology:measurement_basis(valu_tr_t16, projected).
narrative_ontology:measurement(valu_tr_t20, valuation_legitimacy__governance_skeptic, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(valu_tr_t20, projected).
narrative_ontology:measurement(valu_tr_t24, valuation_legitimacy__governance_skeptic, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(valu_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(valu_be_t0, valuation_legitimacy__governance_skeptic, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(valu_be_t0, observed).
narrative_ontology:measurement(valu_be_t4, valuation_legitimacy__governance_skeptic, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(valu_be_t4, observed).
narrative_ontology:measurement(valu_be_t8, valuation_legitimacy__governance_skeptic, base_extractiveness, 8, 0.61).
narrative_ontology:measurement_basis(valu_be_t8, observed).
narrative_ontology:measurement(valu_be_t12, valuation_legitimacy__governance_skeptic, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(valu_be_t12, observed).
narrative_ontology:measurement(valu_be_t16, valuation_legitimacy__governance_skeptic, base_extractiveness, 16, 0.7).
narrative_ontology:measurement_basis(valu_be_t16, projected).
narrative_ontology:measurement(valu_be_t20, valuation_legitimacy__governance_skeptic, base_extractiveness, 20, 0.73).
narrative_ontology:measurement_basis(valu_be_t20, projected).
narrative_ontology:measurement(valu_be_t24, valuation_legitimacy__governance_skeptic, base_extractiveness, 24, 0.76).
narrative_ontology:measurement_basis(valu_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(valu_su_t0, valuation_legitimacy__governance_skeptic, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(valu_su_t0, observed).
narrative_ontology:measurement(valu_su_t4, valuation_legitimacy__governance_skeptic, suppression_requirement, 4, 0.53).
narrative_ontology:measurement_basis(valu_su_t4, observed).
narrative_ontology:measurement(valu_su_t8, valuation_legitimacy__governance_skeptic, suppression_requirement, 8, 0.56).
narrative_ontology:measurement_basis(valu_su_t8, observed).
narrative_ontology:measurement(valu_su_t12, valuation_legitimacy__governance_skeptic, suppression_requirement, 12, 0.58).
narrative_ontology:measurement_basis(valu_su_t12, observed).
narrative_ontology:measurement(valu_su_t16, valuation_legitimacy__governance_skeptic, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(valu_su_t16, projected).
narrative_ontology:measurement(valu_su_t20, valuation_legitimacy__governance_skeptic, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(valu_su_t20, projected).
narrative_ontology:measurement(valu_su_t24, valuation_legitimacy__governance_skeptic, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(valu_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(valuation_legitimacy__governance_skeptic, resource_allocation).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__dcf_fundamentalist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__real_options_technologist).
narrative_ontology:affects_constraint(valuation_legitimacy__governance_skeptic, valuation_legitimacy__musk_cult_believer).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'is the $1.75T valuation legitimate?' conflates four structurally distinct claims and is decomposed per the epsilon-invariance principle. This file authors the governance_skeptic member: epsilon 0.76 measures the standing dual-class arrangement as the governance-skeptic reading assesses it. Sibling files author their own epsilon over the SAME referent (the standing arrangement) with the same stakeholder surface but reading-indexed values: dcf_fundamentalist prices unproven programs as options rather than assets; real_options_technologist treats vertical integration as compounding optionality; musk_cult_believer grounds legitimacy in founder track record. Upstream/downstream: dcf_fundamentalist is the most established member and is cited as evidence by the others; a governance shock (adverse ruling, revealed related-party loss) propagates contamination fastest into musk_cult_believer's evidentiary base and slowest into dcf_fundamentalist's cash-flow anchor. Edges here carry propagation analysis only; no member folds another's premises into its own classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
