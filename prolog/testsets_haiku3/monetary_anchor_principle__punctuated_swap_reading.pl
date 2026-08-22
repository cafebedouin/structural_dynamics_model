% ============================================================================
% CONSTRAINT STORY: monetary_anchor_principle__punctuated_swap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monetary_anchor_principle__punctuated_swap_reading, []).

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
 *   constraint_id: monetary_anchor_principle__punctuated_swap_reading
 *   human_readable: Bretton Woods Gold-Standard Anchor (Punctuated Swap Reading)
 *   domain: economic/political
 *
 * SUMMARY:
 *   On August 15, 1971, President Nixon announced the closure of the gold
 *   window and the suspension of dollar-gold convertibility—the Bretton Woods
 *   monetary anchor. This reading treats the announcement as a discrete
 *   institutional choice: a single decision made at a specific moment by a
 *   specific actor (the U.S. Executive) that could have been made differently
 *   or delayed. The reading emphasizes agency, discretion, and reversibility:
 *   the Shock was not inevitable, but chosen. Under this reading, the
 *   gold-standard peg was a coordination mechanism that solved a real postwar
 *   problem (exchange-rate stability) but evolved into a constraint
 *   extracting seigniorage from foreign dollar holders and fiscal autonomy
 *   for the U.S. The claim is 'rope' (coordination problem solved, genuine
 *   beneficiary, enforcement required), while the metrics describe moderate
 *   extractiveness—the peg did coordinate, but the U.S. extracted fiscal
 *   rents and eventually expropriated foreign reserves unilaterally.
 *
 * KEY AGENTS:
 *   - United States fiscal authority: chose the moment and modality of regime change; extracted seigniorage and fiscal autonomy; enforced dollar dominance before and after the Shock
 *   - Foreign dollar-reserve holders: benefited from the stability of the peg but expropriated when the U.S. broke it; had constrained exit options (dollar or gold, not alternatives)
 *   - International central banks: administered the peg on behalf of their governments; faced cascading instability when the anchor broke
 *   - Bretton Woods signatories (excluded): the original compact was violated without consent or renegotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, 0.62).
domain_priors:suppression_score(monetary_anchor_principle__punctuated_swap_reading, 0.45).
domain_priors:theater_ratio(monetary_anchor_principle__punctuated_swap_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(monetary_anchor_principle__punctuated_swap_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monetary_anchor_principle__punctuated_swap_reading, rope).
narrative_ontology:human_readable(monetary_anchor_principle__punctuated_swap_reading, "Bretton Woods Gold-Standard Anchor (Punctuated Swap Reading)").
narrative_ontology:topic_domain(monetary_anchor_principle__punctuated_swap_reading, "economic/political").

domain_priors:requires_active_enforcement(monetary_anchor_principle__punctuated_swap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monetary_anchor_principle__punctuated_swap_reading, '0323d47a-717f-4b6e-93b3-b095b2e24148').
narrative_ontology:cs_kernel_codification('0323d47a-717f-4b6e-93b3-b095b2e24148', formalized).
narrative_ontology:cs_authority_grounding('0323d47a-717f-4b6e-93b3-b095b2e24148', extraction).
narrative_ontology:cs_reading_relation('0323d47a-717f-4b6e-93b3-b095b2e24148', monetary_anchor_principle__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_reading_relation('0323d47a-717f-4b6e-93b3-b095b2e24148', monetary_anchor_principle__triffin_inevitability_reading, coexists_with).
narrative_ontology:cs_axiom('0323d47a-717f-4b6e-93b3-b095b2e24148', foundational, monetary_regime_reversible_by_choice).
narrative_ontology:cs_axiom_status(monetary_regime_reversible_by_choice, holdable).
narrative_ontology:cs_axiom_grounding('0323d47a-717f-4b6e-93b3-b095b2e24148', monetary_regime_reversible_by_choice, empirically_contingent).
narrative_ontology:cs_axiom('0323d47a-717f-4b6e-93b3-b095b2e24148', secondary, unilateral_defection_reveals_extraction).
narrative_ontology:cs_axiom_status(unilateral_defection_reveals_extraction, holdable).
narrative_ontology:cs_axiom_grounding('0323d47a-717f-4b6e-93b3-b095b2e24148', unilateral_defection_reveals_extraction, conventional).
narrative_ontology:cs_reference_frame('0323d47a-717f-4b6e-93b3-b095b2e24148', bretton_woods_gold_standard_commitment).
narrative_ontology:cs_drift_state('0323d47a-717f-4b6e-93b3-b095b2e24148', august_1971_shock, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0323d47a-717f-4b6e-93b3-b095b2e24148', '').
narrative_ontology:cs_kernel_id(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, united_states_fiscal_authority).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_reserve_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, multinational_corporations).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, gold_miners_and_speculators).
narrative_ontology:constraint_beneficiary(monetary_anchor_principle__punctuated_swap_reading, domestic_labor_and_welfare_constituencies).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, multinational_corporations).
narrative_ontology:constraint_victim(monetary_anchor_principle__punctuated_swap_reading, international_central_banks).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_exogenous_choice).
narrative_ontology:constraint_vindicates(monetary_anchor_principle__punctuated_swap_reading, monetary_regime_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the gold-standard peg: initially commits to redeeming dollars for gold at $35/oz and enforces this through the Treasury. After August 15, 1971, unilaterally suspends convertibility, eliminating the constraint. Benefits from fiscal autonomy: ability to run deficits and expand monetary supply without gold redemption pressure. The Shock is their chosen moment to seize that autonomy.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, united_states_fiscal_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Hold dollar reserves as the world's safe asset, believing dollars are 'as good as gold' under the gold-standard peg. When the U.S. breaks the peg unilaterally, the dollar loses value relative to gold and their reserves are devalued without compensation. They cannot convert dollars to gold (U.S. closed the window) and face massive losses. Their choice was to hold reserves or demand conversion before the break; once the break occurs, they bear expropriation.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, foreign_dollar_reserve_holders, payer,
    organized, biographical, constrained, global).

% U.S. manufacturers benefit from the dollar peg because it keeps U.S. goods expensive relative to foreign competitors, protecting domestic market share (under the fixed peg). But they also benefit from dollar devaluation after the Shock because it makes U.S. exports cheaper and more competitive globally. The constraint shift improves their position post-1971 (currency devaluation benefits exporters), though it cost them pre-1971 (overvaluation hurt competitiveness).
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, multinational_corporations, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, multinational_corporations, payer).

% Hold dollar reserves as official assets and back their domestic currencies on the dollar peg. When the U.S. breaks the peg unilaterally, they face a cascade: reserve valuations collapse, their domestic peg anchors destabilize, and they must quickly negotiate new exchange-rate arrangements. They did not consent to the regime change and had limited ability to stop it (the U.S. closed its gold window unilaterally). Post-Shock, they transitioned to floating or coordinated pegs, losing the monetary autonomy the dollar standard had provided them.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, international_central_banks, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(monetary_anchor_principle__punctuated_swap_reading, international_central_banks, observer).

% When the U.S. suspended gold convertibility at $35/oz, the official price was capped but the market price of gold on the private market rose sharply (reaching $183/oz by 1980). Miners and gold speculators captured the spread between the official and market prices once the price ceiling was lifted. This group benefits from the regime change as gold becomes a tradeable commodity again rather than a fixed monetary peg.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, gold_miners_and_speculators, beneficiary,
    moderate, biographical, mobile, global).

% Domestic U.S. constituencies benefit indirectly from dollar devaluation: it improves export competitiveness, boosts manufacturing employment, and allows the government to expand social spending without gold-standard deflationary pressure. But they also experience inflation as import prices rise (devalued dollar makes foreign goods more expensive). The constraint shift was opaque to them and its costs and benefits are diffuse.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, domestic_labor_and_welfare_constituencies, beneficiary,
    powerless, biographical, trapped, national).

% The 44 nations that negotiated and signed the Bretton Woods agreement in 1944 committed to the gold-standard anchor in exchange for U.S. monetary leadership and postwar stability. The U.S. unilateral abrogation of the peg in 1971 violated the original compact. They would have objected if consulted but were not: the decision was made in secret and announced as fait accompli. Their exclusion from the regime-change decision is the core violation this reading emphasizes.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, bretton_woods_signatories_excluded, excluded,
    organized, generational, trapped, global).

% Dispute whether the Shock was a discrete institutional choice (punctuated equilibrium, this reading) or an inevitable collapse of an overdetermined system (the other readings). They analyze archival evidence, interview policymakers, and produce competing historical narratives. No material stake; the reading they endorse affects historical understanding and policy lessons for future monetary design.
narrative_ontology:constraint_stakeholder(monetary_anchor_principle__punctuated_swap_reading, academic_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(monetary_anchor_principle__punctuated_swap_reading, united_states_fiscal_authority).
narrative_ontology:fixing_cost_class(monetary_anchor_principle__punctuated_swap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The Bretton Woods gold-standard anchor solved a post-WWII coordination problem: how to restore international trade and investment with stable exchange rates, without repeating the 1930s beggar-thy-neighbor currency wars and without recreating the pre-1914 gold standard's deflationary rigidity. The compromise was a fixed peg to the dollar, with the dollar redeemable for gold at $35/oz, giving both stability and modest flexibility for domestic policy.
% TRANSFER_FUNCTION: The constraint transfers the benefits of monetary leadership (seigniorage, fiscal autonomy, geopolitical leverage) from the international system to the U.S. fiscal authority, and transfers the costs (reserve-asset volatility, devaluation risk, loss of autonomous monetary policy) to foreign central banks and dollar-reserve holders. The U.S. gains the ability to run deficits; foreign holders absorb the risk of those deficits eroding dollar value.
% ABSENT_VOICES: The nations that signed Bretton Woods but were excluded from the 1971 decision: they would have objected to unilateral abrogation, demanded renegotiation or compensation, and sought a more symmetric monetary order. They were kept out of the closed-door decision on August 15 specifically so they could not block or modify it. The U.S. also excluded its own Congress from the initial decision (presented after the fact).
% DISAPPEARANCE_RATIONALE: If the gold-standard anchor had persisted after August 1971, the U.S. would have faced mounting pressure to either restore gold convertibility (depleting reserves further and constraining fiscal policy) or defend the peg militarily/diplomatically (as markets tested it). Most likely, the system would have collapsed from a different shock within years, with chaotic devaluation and currency crises. Alternatively, if the Shock had not happened, the U.S. would have faced a hard constraint on deficit spending and monetary expansion, forcing different fiscal and social policy post-1971. The world of floating rates, dollar hegemony, and petrodollars that emerged after 1971 would not have taken the form it did.
% FOUNDING_PROBLEM: After World War II, the international monetary system needed a stable numeraire for trade and investment, one that would prevent the deflationary spirals and exchange-rate chaos of the 1930s. The gold standard had failed because it locked nations into deflationary discipline. The Bretton Woods compromise was to use the U.S. dollar as the reserve asset (backed by gold at a fixed peg) while allowing modest domestic monetary flexibility.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of the punctuated-swap reading (this reading) argue the founding problem—stable exchange rates—was largely solved by 1971 and could have been maintained if the U.S. had simply accepted the discipline of the peg or negotiated a collective reset (like the 1968 two-tier gold market). They cite policymaker testimony (Volcker, Connally, Nixon's own memoirs) stating the Shock was chosen to escape fiscal constraints and extract seigniorage, not because the original system was technically unsustainable. Critics of this reading (the overdetermined and Triffin-inevitability readings) argue that by the late 1960s, structural pressures—Vietnam War deficits, Keynesian policy orthodoxy pushing all nations to run deficits simultaneously, capital mobility—made the founding problem unsolvable under the original framework and collapse was inevitable. Independent analyses (Steil, Eichengreen, Bordo) support both readings with different emphases: the technical Triffin contradiction was real but manageable, while U.S. policy choices (refusing to deflate, running deficits, taxing capital outflows) were the proximate cause of reserve depletion.
narrative_ontology:disappearance_verdict(monetary_anchor_principle__punctuated_swap_reading, world_rearranges).
narrative_ontology:founding_problem_status(monetary_anchor_principle__punctuated_swap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monetary_anchor_principle__punctuated_swap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monetary_anchor_principle__punctuated_swap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monetary_anchor_principle__punctuated_swap_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(monetary_anchor_principle__punctuated_swap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(monetary_anchor_principle__punctuated_swap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.62 at interval end) because the peg did coordinate global trade and finance—it solved a real problem—but the U.S. extracted seigniorage and eventually unilaterally expropriated foreign reserves by breaking the peg. Suppression is relatively low (0.45) because the peg was enforced primarily through the credibility of the U.S. commitment and gold backing, not through coercion of foreign parties (they chose to hold dollars). Theater is low-moderate (0.28) because the peg's coordination function was real, though performance/maintenance activity (U.S. jawboning about the 'strength of the dollar,' defense of the peg against speculators) increased over time. Accessibility of alternatives was moderate (0.48 collapse): foreign countries could theoretically demand gold conversion or negotiate a reset, but the costs of collective action were high and the U.S. had structural advantage. Resistance is high (0.71) because foreign governments and speculators mounted consistent pressure on the peg by accumulating dollars, demanding conversion, and testing the U.S. commitment—this resistance ultimately forced the Shock. The measurements trace the pre-Shock period (0–15) where pressure mounted and extractiveness rose, then plateau (15–30) post-Shock where the new floating-rate regime stabilized. The shared time grid anchors all measurements to the same points.
 *
 * PERSPECTIVAL GAP:
 *   The U.S. agenda-setter seat perceives the peg as coordination it provided and maintained; from that seat, the Shock was a necessary adaptation to preserve dollar hegemony against structural pressures. Foreign-holder seats perceive the peg as a trap: they held dollars expecting gold convertibility and were expropriated by unilateral rule change. Central bank seats faced a cascade: their own currencies' anchors destabilized, forcing rapid renegotiation. The punctuated-swap reading emphasizes the agency gap: from the U.S. executive seat, the Shock was a choice; from foreign seats, it was a violation. This divergence is structural, not opinion: the two parties had fundamentally different positions relative to the constraint's change.
 *
 * DIRECTIONALITY LOGIC:
 *   The U.S. fiscal authority is the beneficiary: it extracted seigniorage (the ability to run deficits and expand the money supply without gold-redemption pressure) and eventually expropriated the reserve premium when it broke the peg unilaterally. Directionality d ≈ 0.15 (full beneficiary). Foreign dollar holders are victims: they absorbed devaluation risk implicitly and expropriation explicitly when convertibility was closed. Directionality d ≈ 0.85 (full target). The peg coordinated global commerce, so there is genuine coordination benefit (that's why the 'rope' claim has credibility), but the benefit flowed asymmetrically and the U.S. extracted rents by controlling the anchor and later by breaking it unilaterally.
 *
 * MANDATROPHY ANALYSIS:
 *   The punctuated-swap reading deflates the Triffin-inevitability narrative (that the peg was doomed by structural forces) and the overdetermined-composite narrative (that collapse was overdetermined by multiple pressures). This reading argues: the founding coordination problem (exchange-rate stability) remained solvable; the peg could have been reformed (wider bands, adjusted parity, multilateral rate-setting); the U.S. *chose* to break rather than renegotiate, because breaking extracted more rent (unilateral devaluation + seigniorage) than renegotiation would have allowed. The constraint's mandate (provide stable exchange rates) was not obsolete—foreign governments still wanted it—but the U.S. defected because the enforcement cost (accepting fiscal discipline) exceeded the perceived benefit to its interests. This is mandatrophy in the weak form: not the founding problem becoming impossible, but the constraint's function being subordinated to the beneficiary's extraction interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_versus_choice_inevitability,
    'Was the August 1971 Shock an inevitable collapse of an overdetermined system (Triffin dilemma + Vietnam deficits + Keynesian consensus + capital mobility), or a discrete institutional choice the U.S. could have deferred or negotiated differently?',
    'Archival analysis of decisionmaking in 1971 (now declassified): did policymakers perceive the peg as technically unsustainable, or as politically unsustainable (i.e., the costs of maintaining it exceeded the perceived benefits)? Counterfactual modeling: could the peg have been reformed (wider bands, adjusted parities, collective rate-setting) if the U.S. had chosen negotiation over unilateral action?',
    'If structural: the constraint type approaches mountain (inevitable); if choice-based: type remains rope (coordination with discretionary exit). The reading''s core premise depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_versus_choice_inevitability, conceptual, 'Whether the Shock was inevitable structural collapse or institutional choice.').

omega_variable(
    unilateral_expropriation_vs_implicit_risk,
    'Did foreign dollar holders accept devaluation risk implicitly (as the price of holding the safe asset), or were they expropriated by a rule change they did not consent to and could not have anticipated?',
    'Historical analysis of what foreign central bankers understood about U.S. commitment to the peg, and whether they had exit opportunities (demanding gold conversion pre-Shock) they failed to exercise. Did the U.S. signal in advance that the peg might break, or did the Shock come as a complete surprise?',
    'If implicit risk: the constraint''s suppression is lower and extraction more symmetric (victims accepted the terms ex ante); if expropriation: suppression and extraction are both higher (the rule change violated implicit contract). Type remains rope, but the distribution of costs shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_expropriation_vs_implicit_risk, empirical, 'Whether foreign holders explicitly or implicitly accepted devaluation risk.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the punctuated-swap reading''s emphasis on institutional choice and reversibility logically foreclose the overdetermined_composite_reading and triffin_inevitability_reading, or do all three readings remain coherent when applied to different layers of the same historical event?',
    'Logical analysis: can a system be both structurally constrained (Triffin dilemma exists) and discretionally navigable (the U.S. could choose how and when to adapt)? Or does the mathematical certainty of reserve depletion (Triffin) foreclose any reading that treats the timing or modality as a choice?',
    'If foreclosed: the readings are mutually exclusive and the engine computes which is true; if coexisting: they apply to different analytical frames (structural vs. institutional) and reflect genuine ambiguity about what ''inevitable'' means in complex systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between punctuated-choice and structural-inevitability readings of the same kernel.').

omega_variable(
    theater_of_gold_commitment,
    'Was the U.S. commitment to redeem dollars for gold at $35/oz performing genuine coordination (reassuring foreign holders, stabilizing expectations), or was it primarily theater masking a known-to-be-unsustainable situation?',
    'Decisionmaking analysis: did U.S. policymakers in the 1960s understand that gold reserves were depleting and the peg could not be maintained indefinitely? If yes, the commitment was partly theater; if no, the coordination was genuine up to the moment the constraints tightened.',
    'If genuine coordination: theater_ratio should be lower; if mostly theater: theater_ratio should be higher, shifting the piton-detection calculus. Type remains rope, but the proportion of function to performance changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theater_of_gold_commitment, empirical, 'Proportion of genuine coordination to performative commitment in the gold-standard peg.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monetary_anchor_principle__punctuated_swap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mone_tr_t0, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mone_tr_t0, observed).
narrative_ontology:measurement(mone_tr_t5, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(mone_tr_t5, observed).
narrative_ontology:measurement(mone_tr_t10, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(mone_tr_t10, observed).
narrative_ontology:measurement(mone_tr_t15, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 15, 0.24).
narrative_ontology:measurement_basis(mone_tr_t15, observed).
narrative_ontology:measurement(mone_tr_t20, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(mone_tr_t20, observed).
narrative_ontology:measurement(mone_tr_t30, monetary_anchor_principle__punctuated_swap_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(mone_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(mone_be_t0, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(mone_be_t0, observed).
narrative_ontology:measurement(mone_be_t5, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(mone_be_t5, observed).
narrative_ontology:measurement(mone_be_t10, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(mone_be_t10, observed).
narrative_ontology:measurement(mone_be_t15, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(mone_be_t15, observed).
narrative_ontology:measurement(mone_be_t20, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement_basis(mone_be_t20, observed).
narrative_ontology:measurement(mone_be_t30, monetary_anchor_principle__punctuated_swap_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(mone_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(mone_su_t0, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(mone_su_t0, observed).
narrative_ontology:measurement(mone_su_t5, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 5, 0.38).
narrative_ontology:measurement_basis(mone_su_t5, observed).
narrative_ontology:measurement(mone_su_t10, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement_basis(mone_su_t10, observed).
narrative_ontology:measurement(mone_su_t15, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 15, 0.43).
narrative_ontology:measurement_basis(mone_su_t15, observed).
narrative_ontology:measurement(mone_su_t20, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 20, 0.44).
narrative_ontology:measurement_basis(mone_su_t20, observed).
narrative_ontology:measurement(mone_su_t30, monetary_anchor_principle__punctuated_swap_reading, suppression_requirement, 30, 0.45).
narrative_ontology:measurement_basis(mone_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monetary_anchor_principle__punctuated_swap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(monetary_anchor_principle__punctuated_swap_reading, 0.12).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__overdetermined_composite_reading).
narrative_ontology:affects_constraint(monetary_anchor_principle__punctuated_swap_reading, monetary_anchor_principle__triffin_inevitability_reading).

% DUAL FORMULATION NOTE:
% The monetary_anchor_principle kernel admits three structurally distinct constraint stories, each representing a competing causal narrative of the August 1971 regime change. The punctuated-swap reading (this story) emphasizes institutional choice and agency; the overdetermined-composite reading emphasizes multiple structural pressures converging; the Triffin-inevitability reading emphasizes mathematical necessity. All three share the same historical referent (the Bretton Woods system and its collapse) but differ in epsilon (moderate vs. high vs. high), in the allocation of agency/inevitability, and in the identity of victims (foreign reserves holders vs. all participants equally constrained vs. reserve-currency issuer ultimately trapped). They are linked via network.affects_constraints to enable comparative analysis of how different causal framings of the same constraint produce different classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
