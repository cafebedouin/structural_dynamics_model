% ============================================================================
% CONSTRAINT STORY: bretton_woods_treaty_substrate__sovereignty_defense
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bretton_woods_treaty_substrate__sovereignty_defense, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: bretton_woods_treaty_substrate__sovereignty_defense
 *   human_readable: Bretton Woods External-Discipline Shield (Sovereignty-Defense Reading)
 *   domain: international_political_economy/monetary_history
 *
 * SUMMARY:
 *   This story instantiates the sovereignty_defense reading of the
 *   bretton_woods_treaty_substrate kernel: the claim that the Bretton Woods
 *   arrangement constrains external monetary discipline — the
 *   gold-standard-style automatic adjustment that forces domestic deflation
 *   on deficit countries — in order to preserve national monetary
 *   sovereignty. Per the ε referent rule, ε is authored for the standing
 *   arrangement under contest: the Articles of Agreement machinery as it
 *   actually operated from the 1944 conference to the August 1971 suspension
 *   — the dollar-gold anchor at $35/oz, declared par values, Fund
 *   surveillance and lending conditions — assessed by this reading's own
 *   lights, never for the symmetric arrangement this reading would prefer. On
 *   that referent the reading finds the promised sovereignty defense was
 *   asymmetric by design: the reserve issuer wrote the rules at a conference
 *   its delegation dominated, issued the asset every other member was
 *   obligated to hold, and remained exempt from the adjustment burdens it
 *   administered on others; the external discipline the design abolished
 *   re-entered through the Fund's lending window as conditionality. The gold
 *   anchor operates as snare rather than stabilizer in this reading: its
 *   bindingness fell entirely on non-issuer members while the issuer's own
 *   compliance was optional and was finally withdrawn unilaterally. The
 *   claimed_type (snare) is this reading's independent structural assessment;
 *   the metrics are authored independently as descriptive of the
 *   arrangement's operation — where the engine's per-seat computations
 *   diverge from the claim, that divergence is the datum the corpus exists to
 *   take. Sibling readings (keynesian_embedded_liberalism,
 *   neoliberal_convertibility) are other constraints in other files, linked
 *   through network.affects_constraints; they are not folded into this story.
 *
 * KEY AGENTS:
 *   - us_reserve_currency_issuer: agenda-setter and primary beneficiary (institutional/arbitrage) — issues the reserve asset, dominates quota-weighted governance, remains exempt from the adjustment it enforces on others, and exits its own constraint unilaterally in 1971
 *   - imf_fund_secretariat: administering agenda-setter (institutional/identity_locked) — polices parities and lends under conditionality; its institutional existence is fused with the system it administers
 *   - non_reserve_currency_states: primary payers (moderate/constrained) — peg to the dollar, accumulate claims on the issuer as peg collateral, import issuer monetary policy, devalue under supervision
 *   - imf_conditionality_recipients: sharpest payer seat (moderate/trapped) — accept austerity and standing policy review as the price of balance-of-payments support
 *   - gaullist_france_gold_converters: resisting payer (powerful/constrained) — converts dollar claims to gold and publicly attacks the issuer's privilege; can press the anchor but not leave the system
 *   - adjusting_states_domestic_populations: excluded (powerless/trapped) — bear austerity and wage restraint with no seat at founding or in conditionality
 *   - keynes_bancor_clearing_union: excluded non-agent — the symmetric alternative design defeated at founding; its absence is the origin of the asymmetry
 *   - triffin_monetary_economists: analytical observer — diagnoses the contradiction (liquidity requires issuer deficits; issuer deficits undermine reserve confidence) the arrangement cannot resolve
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, 0.74).
domain_priors:suppression_score(bretton_woods_treaty_substrate__sovereignty_defense, 0.65).
domain_priors:theater_ratio(bretton_woods_treaty_substrate__sovereignty_defense, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, extractiveness, 0.74).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(bretton_woods_treaty_substrate__sovereignty_defense, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bretton_woods_treaty_substrate__sovereignty_defense, snare).
narrative_ontology:human_readable(bretton_woods_treaty_substrate__sovereignty_defense, "Bretton Woods External-Discipline Shield (Sovereignty-Defense Reading)").
narrative_ontology:topic_domain(bretton_woods_treaty_substrate__sovereignty_defense, "international_political_economy/monetary_history").

domain_priors:requires_active_enforcement(bretton_woods_treaty_substrate__sovereignty_defense).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bretton_woods_treaty_substrate__sovereignty_defense, '75a2a894-6299-41a5-b08c-d032e2fb0047').
narrative_ontology:cs_kernel_codification('75a2a894-6299-41a5-b08c-d032e2fb0047', formalized).
narrative_ontology:cs_authority_grounding('75a2a894-6299-41a5-b08c-d032e2fb0047', extraction).
narrative_ontology:cs_interpretation_layer_present('75a2a894-6299-41a5-b08c-d032e2fb0047').
narrative_ontology:cs_reading_relation('75a2a894-6299-41a5-b08c-d032e2fb0047', bretton_woods_treaty_substrate__keynesian_embedded_liberalism, coexists_with).
narrative_ontology:cs_reading_relation('75a2a894-6299-41a5-b08c-d032e2fb0047', bretton_woods_treaty_substrate__neoliberal_convertibility, forecloses).
narrative_ontology:cs_axiom('75a2a894-6299-41a5-b08c-d032e2fb0047', foundational, external_discipline_primary_sovereignty_threat).
narrative_ontology:cs_axiom_status(external_discipline_primary_sovereignty_threat, holdable).
narrative_ontology:cs_axiom_grounding('75a2a894-6299-41a5-b08c-d032e2fb0047', external_discipline_primary_sovereignty_threat, empirically_contingent).
narrative_ontology:cs_axiom('75a2a894-6299-41a5-b08c-d032e2fb0047', foundational, reserve_issuer_adjustment_exemption).
narrative_ontology:cs_axiom_status(reserve_issuer_adjustment_exemption, holdable).
narrative_ontology:cs_axiom_grounding('75a2a894-6299-41a5-b08c-d032e2fb0047', reserve_issuer_adjustment_exemption, empirically_contingent).
narrative_ontology:cs_reference_frame('75a2a894-6299-41a5-b08c-d032e2fb0047', sovereignty_preserving_parity_design).
narrative_ontology:cs_drift_state('75a2a894-6299-41a5-b08c-d032e2fb0047', late_gold_pool_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('75a2a894-6299-41a5-b08c-d032e2fb0047', '').
narrative_ontology:cs_kernel_id(bretton_woods_treaty_substrate__sovereignty_defense, bretton_woods_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_currency_issuer).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(bretton_woods_treaty_substrate__sovereignty_defense, gaullist_france_gold_converters).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the dollar, pegged to gold at $35 an ounce and redeemable in gold to official holders. Because every other member pegs to the dollar, it settles external deficits by issuing dollar liabilities that other central banks must accumulate, and finances overseas military spending and domestic programs on that basis. The convertibility promise is the one obligation the arrangement places on it; it holds the power to suspend that promise, as it does in August 1971, without prior consent from the members whose reserves depend on it.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_currency_issuer, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_currency_issuer, beneficiary).

% Administers the par-value system: registers declared parities, runs consultations on members' exchange and trade restrictions, and lends to members in balance-of-payments difficulty under negotiated policy conditions. Its staff, quota shares, and voting structure were fixed at the founding conference, with the largest share held by the issuing country. Its mandate, funding, and institutional existence are constituted by the parity system it polices; if the system ends, it must reconstitute itself around whatever replaces it.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_fund_secretariat, agenda_setter,
    institutional, generational, identity_locked, global).

% Peg their currencies to the dollar and defend those pegs by intervening in exchange markets. Defending a peg requires holding dollar balances as reserves, so they ship real exports and hold claims on the issuing country in return, earning less than the balances would fetch elsewhere. Because their currencies move with the dollar, their domestic monetary conditions track the issuing country's policy stance. When their own deficits strike, they devalue under Fund supervision rather than by unilateral choice.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, non_reserve_currency_states, payer,
    moderate, biographical, constrained, national).

% Members that draw on the Fund in crisis — the United Kingdom in 1947 and again in 1967, and others — accept credit squeezes, budget cuts, and standing policy review as the price of support. The conditions are negotiated under a quota-weighted board in which the issuing country holds the largest vote. Once a crisis is on them, the Fund is effectively the only lender available on acceptable terms.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, imf_conditionality_recipients, payer,
    moderate, biographical, trapped, national).

% From 1965 the French government publicly attacks the issuing country's position and converts dollar reserves into gold at the official price, pressing the gold pool that defends $35. The conversions hedge its reserves against devaluation of its dollar claims, but remaining in the system stays necessary to its trade finance; it can press the anchor, not leave it.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, gaullist_france_gold_converters, payer,
    powerful, biographical, constrained, continental).

% Workers, firms, and taxpayers in countries undergoing Fund-supervised adjustment bear the credit squeezes, public-spending cuts, and wage restraint the programs require. No delegation of theirs sat at the founding conference, and conditionality is negotiated between the Fund board and national finance ministries; they appear in the process only as the object of the measures.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, adjusting_states_domestic_populations, excluded,
    powerless, biographical, trapped, national).

% The British delegation's founding proposal: a global clearing union using a neutral international reserve unit (bancor), with symmetric adjustment pressure on surplus and deficit countries alike. Defeated at the 1944 conference by the American dollar-gold design; preserved only in archives and echoed later in special drawing rights.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, keynes_bancor_clearing_union, excluded,
    moderate, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(bretton_woods_treaty_substrate__sovereignty_defense, keynes_bancor_clearing_union).

% Diagnose the arrangement's central contradiction from 1960: world liquidity depends on the issuing country running external deficits, but running those deficits erodes confidence in its promise to convert at $35. They publish, testify before Congress, and propose reforms; the governing coalition adopts none before the 1971 suspension.
narrative_ontology:constraint_stakeholder(bretton_woods_treaty_substrate__sovereignty_defense, triffin_monetary_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(bretton_woods_treaty_substrate__sovereignty_defense, us_reserve_currency_issuer).
narrative_ontology:fixing_cost_class(bretton_woods_treaty_substrate__sovereignty_defense, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed-parity framework for international trade and payments, a mechanism for supplying world liquidity (dollar reserves issued against the gold anchor), and a crisis lender for members in balance-of-payments difficulty — solving, once and centrally, the interwar problems of competitive devaluation, unstable exchange rates, and gold-scarcity deflation.
% TRANSFER_FUNCTION: Moves seigniorage and real resources from non-reserve-issuing member states — which must earn and hold dollar reserve balances and absorb adjustment — to the reserve-issuing country, which finances external deficits and domestic programs with liabilities others are obligated to hold; and moves policy discretion from deficit countries to the Fund's quota-weighted board via lending conditions.
% ABSENT_VOICES: Domestic populations of adjusting countries had no seat at the founding conference and none in conditionality negotiations. The bancor clearing-union design — the one proposal with symmetric adjustment — was excluded at founding. Smaller members held negligible quota votes; creditor Europe could press (France did) but could not set the agenda.
% DISAPPEARANCE_RATIONALE: The arrangement's actual disappearance in 1971–73 rearranged the world: parities gave way to generalized floating, the issuer shed the convertibility promise it had been defending, non-reserve states began accumulating reserves defensively rather than as peg collateral, and the Fund reconstituted itself from parity administrator into a surveillance-and-lending body. Trade finance, reserve management, and domestic monetary policy in every member state reorganized around the new regime within a decade.
% FOUNDING_PROBLEM: The interwar monetary disorder: competitive devaluations and beggar-thy-neighbor trade policy in the 1930s, the gold standard's deflationary discipline that forced domestic contraction regardless of employment, and the absence of a lender able to stop balance-of-payments crises from cascading.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: Triffin's 1960 congressional testimony and monograph treat the stability function as achieved while documenting the asymmetry; the French Treasury's public position (1965–68) attests the founding problem was solved and the remaining function was asymmetric financing; post-mortem monetary histories (Eichengreen; Bordo and Eichengreen) corroborate that exchange-rate stability was delivered by the mid-1950s while the discipline asymmetry persisted to 1971. No source inside the issuing country's official position attests the sovereignty-preservation function as operating symmetrically.
narrative_ontology:disappearance_verdict(bretton_woods_treaty_substrate__sovereignty_defense, world_rearranges).
narrative_ontology:founding_problem_status(bretton_woods_treaty_substrate__sovereignty_defense, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(bretton_woods_treaty_substrate__sovereignty_defense, 'none', 1).
narrative_ontology:epsilon_provenance(bretton_woods_treaty_substrate__sovereignty_defense, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(bretton_woods_treaty_substrate__sovereignty_defense, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bretton_woods_treaty_substrate__sovereignty_defense_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.74 at interval end) because the arrangement's core transfer — seigniorage and real resources from non-issuing members to the issuer — was decoupled from any marginal service the issuer provided and was enforced by peg mechanics: defending a parity against the dollar requires accumulating the issuer's liabilities. Suppression (0.65) is authored as a raw structural property, unscaled by power or scope: capital controls, Fund lending conditions, and peg-defense obligations coerced compliance from members while binding the issuer only in form. Theater_ratio rises to 0.55 because the anchor's substance decayed while its forms persisted — the London Gold Pool defended a price the two-tier market had already repudiated, par-value adjustability survived only as crisis ritual, and surveillance continued over a system the issuer was preparing to leave. Accessibility_collapse is moderate (0.48): floating rates, sterling-area clearing, and autarkic alternatives existed but carried prohibitive trade-finance costs for members, and the issuer actively discouraged the nearest alternatives (pressing Germany not to revalue, opposing marginal gold conversions). Resistance is moderate (0.55): French conversion policy, the Triffin critique, and the 1967 sterling devaluation were real but never coalesced into a coalition — collective action failed because each member's exit threatened its own trade finance while the issuer could ration swap lines and Fund support to defectors, so the powerless potential coalition (non-reserve states plus adjusting populations) never formed. All tracked metrics share one six-point grid (1944–1971). suppression_requirement is authored because enforcement capacity is this story's tracked dynamic: it eases as European convertibility is restored (1958) then ratchets up — Interest Equalization Tax 1963, the Fed swap network, Gold Pool operations, mandatory US capital controls 1968, the Nixon surcharge and freeze 1971 — as the arrangement's contradictions deepen. Enforcement intensifying while the system decays is the ratchet pattern this reading predicts.
 *
 * PERSPECTIVAL GAP:
 *   The issuer seat and the payer seats compute different types from the same structure. From the issuer's seat the arrangement is a stability framework it built and guarantees — the constraint appears as its own convertibility promise, and the seigniorage appears as the wage of supplying the world's reserve asset. From the non-issuer payer seats the same machinery operates as enforced transfer: they hold the issuer's depreciating claims as the price of their own parities, import the issuer's monetary stance, and face the discipline — through Fund conditionality — that the issuer exempted itself from. The Fund's seat is a third position: it administers a discipline it does not set, and its institutional survival is fused with the system's persistence, so it defends the kernel as written even as practice departs from it. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The issuer is declared beneficiary and holds the agenda: d sits near the beneficiary end, and its arbitrage-grade exit — it can suspend its own convertibility promise, and does — damps its effective extraction toward subsidy. Non-reserve states are declared victims with constrained exit: d near the full-target end, amplified by the global scope that makes coordinated verification and exit hardest. Conditionality recipients are the trapped subset of the victim set — the highest-d seats. France is a payer with powerful power but constrained exit: gold conversion is hedging within the system, not exit from it, so its d stays high despite its power atom. Adjusting populations are excluded and powerless — they bear the transfer's real-resource end with no seat and no exit. The bancor proposal is a non-agent (agent: false) and feeds no directionality computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar chaos: competitive devaluation, gold-standard deflationary discipline, collapsed multilateral trade — was substantially solved by the mid-1950s: parities held, trade expanded, the deflationary bias was broken. The arrangement's persistence after roughly 1958 was not problem-solving but rent collection on the design asymmetry: the stability function was already being delivered by prosperity and habit while the transfer function accelerated (dollar overhang, Gold Pool defense, conditionality). Authoring founding_problem_status dead against disappearance_verdict world_rearranges is the honest genealogy: the world depended on the arrangement's stability shell while its founding problem was dead — the mismatch is the capture/zombie pattern this reading exists to surface. The analysis also guards the reverse mislabel: the genuine coordination delivered 1944–1958 is not denied by the snare claim; the claim is that the coordination was the bait and the asymmetry was the hook, which is why the classification turns on who paid after the founding problem closed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (sovereignty_defense) of the bretton_woods_treaty_substrate kernel; which of its structural features are reading-indexed rather than kernel-invariant?',
    'Generate the sibling readings as separate stories and diff victim/beneficiary sets, epsilon, and claimed type across the family; features invariant across readings are kernel properties, deltas are reading-indexed.',
    'Under keynesian_embedded_liberalism the victim set shifts toward capital-exporting financial interests and the beneficiary set broadens to holders of domestic policy autonomy; under neoliberal_convertibility beneficiaries shift to financial intermediaries and victims to intervention-bound states. The snare claim here is reading-indexed; the kernel itself may compute as a rope under another reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of one kernel; classification may be reading-relative.').

omega_variable(
    exorbitant_privilege_vs_duty,
    'Was the issuer''s position net privilege (seigniorage, deficit financing without adjustment) or net burden (the convertibility commitment, reserve-loss exposure, episodic adjustment pressure)?',
    'Counterfactual policy analysis: compare US fiscal-monetary freedom against a comparable non-reserve-issuer counterfactual; audit whether the convertibility promise ever forced US domestic adjustment (it largely did not — the Gold Pool absorbed the strain instead).',
    'If net burden, the issuer''s d rises toward symmetric, effective extraction falls, and the snare claim weakens toward tangled_rope; if net privilege, the snare claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exorbitant_privilege_vs_duty, empirical, 'Whether the reserve issuer was extractor or ultimate guarantor.').

omega_variable(
    conditionality_discipline_reintroduction,
    'Did Fund conditionality functionally reintroduce the external discipline the Articles abolished — making the sovereignty promise cover — or was it a genuine lender-of-last-resort function whose conditions were incidental safeguards?',
    'Compare adjustment outcomes and policy autonomy under Fund programs against matched no-program crises; test whether conditions tracked creditor interests or crisis-resolution needs.',
    'If genuine lender-of-last-resort, a real coordination component strengthens and classification moves toward tangled_rope; if discipline-by-lending, the snare reading is confirmed at the conditionality margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conditionality_discipline_reintroduction, empirical, 'Whether conditionality is reintroduced external discipline or crisis lending.').

omega_variable(
    gold_anchor_bindingness,
    'How tightly did the $35 gold anchor actually bind issuer policy between 1958 and 1971 — did it ever force US domestic adjustment, or was it already theater sustained by the pool and by holders'' acquiescence?',
    'Historical audit of US policy decisions against the anchor: if no major fiscal-monetary decision was reversed to defend the parity, the anchor''s bindingness on the issuer was near zero and the measured theater is structural.',
    'If the anchor never bound the issuer, the arrangement was asymmetric from the start rather than a stabilizer that decayed — founding_problem_status dead is confirmed and theater_ratio is understated; if it bound episodically, part of the measured transfer is the price of the anchor''s credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(gold_anchor_bindingness, empirical, 'Whether the gold anchor constrained the issuer in fact or only in form.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bretton_woods_treaty_substrate__sovereignty_defense, 1944, 1971).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bw_sov_defense_tr_t1944, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1944, 0.15).
narrative_ontology:measurement_basis(bw_sov_defense_tr_t1944, observed).
narrative_ontology:measurement(bw_sov_defense_tr_t1950, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1950, 0.18).
narrative_ontology:measurement_basis(bw_sov_defense_tr_t1950, observed).
narrative_ontology:measurement(bw_sov_defense_tr_t1958, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1958, 0.3).
narrative_ontology:measurement_basis(bw_sov_defense_tr_t1958, observed).
narrative_ontology:measurement(bw_sov_defense_tr_t1965, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1965, 0.42).
narrative_ontology:measurement_basis(bw_sov_defense_tr_t1965, observed).
narrative_ontology:measurement(bw_sov_defense_tr_t1968, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1968, 0.5).
narrative_ontology:measurement_basis(bw_sov_defense_tr_t1968, observed).
narrative_ontology:measurement(bw_sov_defense_tr_t1971, bretton_woods_treaty_substrate__sovereignty_defense, theater_ratio, 1971, 0.55).
narrative_ontology:measurement_basis(bw_sov_defense_tr_t1971, observed).

% Extraction over time
narrative_ontology:measurement(bw_sov_defense_be_t1944, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1944, 0.4).
narrative_ontology:measurement_basis(bw_sov_defense_be_t1944, observed).
narrative_ontology:measurement(bw_sov_defense_be_t1950, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1950, 0.48).
narrative_ontology:measurement_basis(bw_sov_defense_be_t1950, observed).
narrative_ontology:measurement(bw_sov_defense_be_t1958, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1958, 0.58).
narrative_ontology:measurement_basis(bw_sov_defense_be_t1958, observed).
narrative_ontology:measurement(bw_sov_defense_be_t1965, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1965, 0.66).
narrative_ontology:measurement_basis(bw_sov_defense_be_t1965, observed).
narrative_ontology:measurement(bw_sov_defense_be_t1968, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1968, 0.7).
narrative_ontology:measurement_basis(bw_sov_defense_be_t1968, observed).
narrative_ontology:measurement(bw_sov_defense_be_t1971, bretton_woods_treaty_substrate__sovereignty_defense, base_extractiveness, 1971, 0.74).
narrative_ontology:measurement_basis(bw_sov_defense_be_t1971, observed).

% Suppression requirement over time
narrative_ontology:measurement(bw_sov_defense_su_t1944, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1944, 0.55).
narrative_ontology:measurement_basis(bw_sov_defense_su_t1944, observed).
narrative_ontology:measurement(bw_sov_defense_su_t1950, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1950, 0.52).
narrative_ontology:measurement_basis(bw_sov_defense_su_t1950, observed).
narrative_ontology:measurement(bw_sov_defense_su_t1958, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1958, 0.46).
narrative_ontology:measurement_basis(bw_sov_defense_su_t1958, observed).
narrative_ontology:measurement(bw_sov_defense_su_t1965, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1965, 0.58).
narrative_ontology:measurement_basis(bw_sov_defense_su_t1965, observed).
narrative_ontology:measurement(bw_sov_defense_su_t1968, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1968, 0.62).
narrative_ontology:measurement_basis(bw_sov_defense_su_t1968, observed).
narrative_ontology:measurement(bw_sov_defense_su_t1971, bretton_woods_treaty_substrate__sovereignty_defense, suppression_requirement, 1971, 0.65).
narrative_ontology:measurement_basis(bw_sov_defense_su_t1971, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bretton_woods_treaty_substrate__sovereignty_defense, global_infrastructure).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, keynesian_embedded_liberalism).
narrative_ontology:affects_constraint(bretton_woods_treaty_substrate__sovereignty_defense, neoliberal_convertibility).

% DUAL FORMULATION NOTE:
% The colloquial label 'Bretton Woods system' covers three structurally distinct constraints — three readings of one kernel (bretton_woods_treaty_substrate). This story is the sovereignty_defense reading: epsilon is authored for the standing arrangement (the Articles machinery as it operated 1944–1971) as this reading sees it — asymmetric sovereignty defense with the issuer exempt — hence high epsilon and a snare claim. The keynesian_embedded_liberalism reading instantiates a different constraint: the same arrangement read as capital-flow containment protecting domestic policy space, with a different victim set and lower epsilon. The neoliberal_convertibility reading instantiates a third: the arrangement read as intervention constraints enabling capital markets, with beneficiaries and victims inverted relative to this file. Each reading is a separate file with its own epsilon, its own stakeholders, and its own classification; they are linked here because the upstream readings cite the same treaty text and the family's divergence is the analytical point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
