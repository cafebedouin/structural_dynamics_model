% ============================================================================
% CONSTRAINT STORY: ecb_mandate_article_127__expansive_secondary_objectives
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ecb_mandate_article_127__expansive_secondary_objectives, []).

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
 *   constraint_id: ecb_mandate_article_127__expansive_secondary_objectives
 *   human_readable: ECB Mandate — Expansive Secondary-Objectives Reading ('Without Prejudice' Operational Balancing)
 *   domain: monetary policy/economic/constitutional
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel — Article 127
 *   TFEU's monetary mandate. The expansive_secondary_objectives reading
 *   treats the primary price-stability objective and the 'without prejudice'
 *   support duty as jointly operative: when inflation pressure is absent, the
 *   Governing Council may give real operational weight to employment and
 *   growth, balancing judgmentally. The standing arrangement under contest is
 *   the ECB's actual interpretive practice as entrenched through crisis
 *   programs (SMP, OMT, APP, TLTROs, PEPP) and ratified by CJEU case law. KEY
 *   AGENTS (by structural relationship): ecb_governing_council
 *   (agenda_setter, institutional/arbitrage) — interprets the clause, sets
 *   the stance, collects expanded discretion; eurozone_workers (beneficiary,
 *   organized/constrained); indebted_households_and_smes (dual-positioned
 *   beneficiary/payer, moderate/constrained);
 *   high_debt_member_state_treasuries (principal beneficiary and capturer,
 *   institutional/constrained); eurozone_retirement_savers (primary payer,
 *   moderate/trapped); institutional_fixed_income_holders (payer with hedged
 *   exposure, organized/arbitrage); bundesbank_tradition_actors (excluded
 *   orthodox seat, institutional/constrained); cjeu (analytical observer,
 *   institutional/analytical). The claim/metrics split is deliberate:
 *   claimed_type is what I judge structurally true (a hybrid with genuine
 *   coordination AND asymmetric transfer under active enforcement); the
 *   metrics describe the arrangement's actual operation as the historical
 *   record shows it. Family note: the colloquial label 'the ECB mandate'
 *   decomposes into three structurally distinct constraints — this reading,
 *   the orthodox reading, and climate incorporation — each with its own
 *   epsilon, beneficiary set, and classification; they are linked, not
 *   merged.
 *
 * KEY AGENTS:
 *   - - ecb_governing_council: Agenda setter ([institutional]/[arbitrage]) — interprets the clause, controls stance and programs, collects expanded discretionary authority
 *   - - eurozone_workers: Beneficiary ([organized]/[constrained]) — employment and bargaining conditions respond to growth-weighted stance
 *   - - indebted_households_and_smes: Dual-positioned beneficiary/payer ([moderate]/[constrained]) — gains via debt service, pays via consumption basket
 *   - - high_debt_member_state_treasuries: Principal beneficiary and capturer ([institutional]/[constrained]) — refinancing costs indexed to the stance
 *   - - eurozone_retirement_savers: Primary payer ([moderate]/[trapped]) — sub-inflation returns drain purchasing power, no hedging capacity, no seat
 *   - - institutional_fixed_income_holders: Payer with hedged exposure ([organized]/[arbitrage]) — absorbs yield compression, adapts via portfolio tools
 *   - - bundesbank_tradition_actors: Excluded orthodox seat ([institutional]/[constrained]) — argues the clause is non-operative, loses votes and cases, cannot exit
 *   - - cjeu: Analytical observer ([institutional]/[analytical]) — ratifies the reading's legality while declining to police the internal balance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, 0.56).
domain_priors:suppression_score(ecb_mandate_article_127__expansive_secondary_objectives, 0.6).
domain_priors:theater_ratio(ecb_mandate_article_127__expansive_secondary_objectives, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, extractiveness, 0.56).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(ecb_mandate_article_127__expansive_secondary_objectives, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ecb_mandate_article_127__expansive_secondary_objectives, tangled_rope).
narrative_ontology:human_readable(ecb_mandate_article_127__expansive_secondary_objectives, "ECB Mandate — Expansive Secondary-Objectives Reading ('Without Prejudice' Operational Balancing)").
narrative_ontology:topic_domain(ecb_mandate_article_127__expansive_secondary_objectives, "monetary policy/economic/constitutional").

domain_priors:requires_active_enforcement(ecb_mandate_article_127__expansive_secondary_objectives).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ecb_mandate_article_127__expansive_secondary_objectives, '47b651c2-b4c0-417b-ae90-89f92dde7ad6').
narrative_ontology:cs_kernel_codification('47b651c2-b4c0-417b-ae90-89f92dde7ad6', fixed_text).
narrative_ontology:cs_authority_grounding('47b651c2-b4c0-417b-ae90-89f92dde7ad6', expertise).
narrative_ontology:cs_interpretation_layer_present('47b651c2-b4c0-417b-ae90-89f92dde7ad6').
narrative_ontology:cs_reading_relation('47b651c2-b4c0-417b-ae90-89f92dde7ad6', ecb_mandate_article_127__orthodox_price_stability, coexists_with).
narrative_ontology:cs_reading_relation('47b651c2-b4c0-417b-ae90-89f92dde7ad6', ecb_mandate_article_127__climate_incorporation, influences).
narrative_ontology:cs_axiom('47b651c2-b4c0-417b-ae90-89f92dde7ad6', foundational, conditional_secondary_operativity).
narrative_ontology:cs_axiom_status(conditional_secondary_operativity, holdable).
narrative_ontology:cs_axiom_grounding('47b651c2-b4c0-417b-ae90-89f92dde7ad6', conditional_secondary_operativity, conventional).
narrative_ontology:cs_axiom('47b651c2-b4c0-417b-ae90-89f92dde7ad6', foundational, discretionary_balancing_within_mandate_bounds).
narrative_ontology:cs_axiom_status(discretionary_balancing_within_mandate_bounds, holdable).
narrative_ontology:cs_axiom_grounding('47b651c2-b4c0-417b-ae90-89f92dde7ad6', discretionary_balancing_within_mandate_bounds, instrumental).
narrative_ontology:cs_reference_frame('47b651c2-b4c0-417b-ae90-89f92dde7ad6', primacy_with_operative_support_duties).
narrative_ontology:cs_drift_state('47b651c2-b4c0-417b-ae90-89f92dde7ad6', post_2021_strategy_review, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('47b651c2-b4c0-417b-ae90-89f92dde7ad6', '').
narrative_ontology:cs_kernel_id(ecb_mandate_article_127__expansive_secondary_objectives, ecb_mandate_article_127).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households_and_smes).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, high_debt_member_state_treasuries).
narrative_ontology:constraint_beneficiary(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_retirement_savers).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, institutional_fixed_income_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households_and_smes).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, without_prejudice_operativity_doctrine).
narrative_ontology:constraint_vindicates(ecb_mandate_article_127__expansive_secondary_objectives, monetary_discretion_deference_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the euro area's policy rates, decides asset purchase and targeted lending programs, and issues the strategy statements that define what the treaty's 'without prejudice' clause permits in practice. Its legal service drafts the justifications later tested before the courts; its votes decide how much weight employment and growth receive when inflation pressure is quiet. Every permitted secondary-objective weighting widens the space of defensible policy choices, so the reading itself is an expansion of its decision authority. Members arrive through national appointments but operate under a common institutional identity; an individual member can leave, the institution cannot.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, ecb_governing_council, beneficiary).

% Employment and wage outcomes across the euro area respond to the policy stance. When the stance leans toward growth and employment, hiring and bargaining conditions improve first in regions with slack. Individual workers cannot exit the currency area's labor market except by emigrating, and their voice reaches the council only indirectly through unions, works councils, and national governments.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_workers, beneficiary,
    organized, biographical, constrained, continental).

% Carry mortgages, consumer credit, and business loans priced off euro rates. Accommodative phases cut their debt service and inflation erodes what they owe; the same phases raise their grocery and energy bills, so the gain arrives through the loan book while the cost arrives through the consumption basket. Refinancing or relocating is possible but slow and expensive.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households_and_smes, beneficiary,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(ecb_mandate_article_127__expansive_secondary_objectives, indebted_households_and_smes, payer).

% Continuously refinance large sovereign debt stocks. A persistent accommodative bias compresses the yields they pay and keeps market access routine; a decisive turn to price-stability-first operation widens their spreads immediately. Fiscal plans, pension commitments, and governing-coalition survival are indexed to the stance. Exit would mean leaving the euro or imposing austerity — neither is a live option.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, high_debt_member_state_treasuries, beneficiary,
    institutional, generational, constrained, continental).

% Hold deposits, annuities, and insured products denominated in euros. Through accommodative phases these pay below inflation, so purchasing power drains year over year. Hedging requires financial sophistication, cross-border accounts, or equity risk that many older households decline. Their interests have no seat in the council's composition and surface only indirectly through consumer-price politics.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, eurozone_retirement_savers, payer,
    moderate, generational, trapped, continental).

% Insurers, pension funds, and asset managers running bond portfolios. Yield compression and duration losses hit their liabilities and fee income, but they hold the tools to adapt: inflation-linked paper, foreign-currency allocation, equity overlays, derivatives. Adaptation costs money and tracking error, not existence. They litigate, lobby, and reposition far faster than any household.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, institutional_fixed_income_holders, payer,
    organized, biographical, arbitrage, global).

% Central-bank conservatives in the Bundesbank lineage, ordoliberal economists, and parts of the German constitutional and political establishment. They hold formal seats in council debates and command a public platform, arguing that the clause gives support duties no operational force. Their framework loses the votes and the case law; they cannot leave the euro system, so their leverage runs through appointment politics, court filings, and public argument.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, bundesbank_tradition_actors, excluded,
    institutional, generational, constrained, continental).

% Adjudicates the legality of the programs that give this interpretive practice effect. Its judgments on OMT and PSPP treated monetary policy choices as the council's own expert province, reviewing purpose and proportionality but not the balance struck inside the mandate. It sits outside the benefiting coalitions and outside the council's hierarchy; its relationship to the arrangement is analytical.
narrative_ontology:constraint_stakeholder(ecb_mandate_article_127__expansive_secondary_objectives, cjeu, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ecb_mandate_article_127__expansive_secondary_objectives, high_debt_member_state_treasuries).
narrative_ontology:fixing_cost_class(ecb_mandate_article_127__expansive_secondary_objectives, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides one monetary stance for a heterogeneous currency union in which rigid single-target operation would impose asymmetric unemployment costs on weaker regions. The 'without prejudice' clause lets the stance lean toward employment and growth when inflation pressure is absent, aligning monetary conditions with member-state fiscal and social aims without renegotiating the treaty each cycle.
% TRANSFER_FUNCTION: Moves purchasing power from deposit- and bond-holders to borrowers and indebted states through the level and shape of rates and the composition of asset purchases; separately, it moves decision authority over macroeconomic trade-offs to the Governing Council, insulated from electoral channels. During accommodative phases the purchasing-power flow runs steadily from savers toward debtors and treasuries.
% ABSENT_VOICES: Retirement savers and future retirees have no seat in the council's composition and no direct venue; their loss is diffuse and registers only as aggregate price politics. The orthodox Bundesbank-lineage framework holds formal seats but is operationally marginalized — present in the room, absent from the outcome. Citizens bearing the distributional consequences have no elected channel into monetary goal-setting; the European Parliament's monetary dialogue is consultative only.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight and the clause reverted to strictly non-operative status, the policy stance would tighten toward exclusive price targeting: sovereign spreads in high-debt states would widen within weeks, employment-supporting program design (TLTRO-style lending, purchase-program flexibility) would lapse, and the currency union's political economy would reorganize around austerity-and-spread dynamics that the current arrangement suppresses.
% FOUNDING_PROBLEM: Reconciling a single currency with economically heterogeneous members: a monetary authority tuned solely to price stability imposes its adjustment costs asymmetrically, falling hardest on high-unemployment, high-debt regions. The Maastricht drafters wrote the 'without prejudice' clause so the new institution could support general economic policies once price stability was secured, rather than reproducing Bundesbank-style single-target operation at continental scale.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting set: the CJEU's Gauweiler and Weiss judgments engage the clause's legal operability directly; the German Federal Constitutional Court's judgments acknowledge the clause's existence while disputing its weight; the monetary-economics literature on regional asymmetry inside currency unions documents the founding problem empirically; and European Parliament monetary-dialogue records show the tension recurring across decades. Even the orthodox opposition corroborates the drafting history — it disputes the clause's force, not its presence.
narrative_ontology:disappearance_verdict(ecb_mandate_article_127__expansive_secondary_objectives, world_rearranges).
narrative_ontology:founding_problem_status(ecb_mandate_article_127__expansive_secondary_objectives, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ecb_mandate_article_127__expansive_secondary_objectives, 'none', 1).
narrative_ontology:epsilon_provenance(ecb_mandate_article_127__expansive_secondary_objectives, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ecb_mandate_article_127__expansive_secondary_objectives, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ecb_mandate_article_127__expansive_secondary_objectives_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.56 at interval end reflects a real but conditioned transfer: the reading tolerates inflation above the orthodox frontier and holds rates below it precisely when doing so serves employment and sovereign solvency, and the recipients are identifiable (debtors, treasuries) as are the payers (depositors, bondholders). It is capped, not open-ended — the reading's own frame switches the weighting off when price stability is threatened, which the 2022-24 tightening demonstrated (visible as the 2020->2025 decline in the series). Suppression 0.60 is structural: the reading survives only because its holders actively defend it — council vote management against hawkish dissent, legal-service preparation for constitutional-court confrontation, strategic communication reframing each program as mandate-consistent. Accessibility_collapse is low (0.40): understanding this reading does not eliminate the alternatives — the orthodox reading stays alive in German constitutional politics and hawkish council minorities, and climate incorporation builds on the same gateway. Resistance 0.65 is correspondingly high and documented: Gauweiler and Weiss referrals, the PSPP judgment, persistent Bundesbank-lineage dissent. Theater_ratio 0.32: the substance is real (balance-sheet decisions track the stated weighting), but strategy-review language and secondary-objective rhetoric have grown a performative layer that exceeds tool-level change. The temporal series run on ONE shared grid (1999, 2003, 2008, 2012, 2015, 2020, 2025) with every tracked metric authored at every point; the suppression_requirement series is authored deliberately because the story's enforcement picture is NOT static — the machinery defending this reading (litigation defense, dissent management) was built up from near-nothing in 1999 to a peak around the PSPP confrontation in 2020, then partially settled after Weiss. The extractiveness dip after 2020 is frame-consistent behavior, not decay: the reading's conditionality switched off the weighting when the inflation surge arrived.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute very different arrangements from identical treaty text. From the council chair, this is prudently managed flexibility it built, defended, and got ratified — a functioning instrument. From the trapped saver's seat, the same arrangement is a steady confiscation with no exit and no representation — a diffuse class paying for concentrated beneficiaries. Two same-level institutional actors diverge sharply on exit alone: high-debt treasuries (constrained, existential stake) versus institutional fixed-income holders (arbitrage-grade portfolio mobility), so the same rate path lands as lifeline for one and fee compression for the other. Inter-institutionally, the CJEU experienced the arrangement as a proportionality question it resolved by deference, while the German Federal Constitutional Court experienced it as an identity question of democratic legitimacy — same constraint, incompatible institutional frames. Coalition dynamics matter for persistence: the payer side aggregates enormous wealth but is dispersed, organizationally heterogeneous, and faces classic collective-action barriers, while the beneficiary side includes actors (treasuries) whose entire fiscal architecture depends on continuation.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The council sits near the beneficiary pole (d low): it collects the authority rent the reading creates and faces no external enforcement superior; its arbitrage-grade control over the interpretive frame itself dampens any residual cost. Workers and borrower households derive low d (subsidized by the stance) with the household seat partially offset toward symmetry by its consumption-basket exposure — hence the dual role. High-debt treasuries sit nearest the full-beneficiary end among the large actors: maximal gain, no exit, generational dependence. Retirement savers derive near-full-target d: they bear the transfer with trapped exit, which amplifies effective extraction. Institutional fixed-income holders also derive target-side d, but their arbitrage-grade exit (inflation-linked paper, foreign allocation, derivatives) damps the effective burden well below the retail saver's. The excluded orthodox seat experiences the arrangement as imposed — its preferred reading is outvoted and outruled, and it cannot exit the system. Scope is continental, which scales verification difficulty and thus mildly amplifies effective extraction on the target side; the globally-scoped institutional holder partially escapes that amplification through portfolio geography.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling one currency with heterogeneous members — remains live, so no mandatrophy declaration is made and none is implied by any metric. The classification discipline matters in both directions here: labeling the arrangement pure coordination (rope) would erase the identifiable saver-side transfer and the enforcement machinery needed to hold the reading against domestic constitutional challenge; labeling it pure extraction (snare) would erase the documented stabilization function — the employment and spread-calming effects that the CJEU weighed and found proportionate. The tangled_rope claim forces both halves into the record: who is coordinated (a heterogeneous union getting countercyclical flexibility without treaty amendment) and who pays through the same structure (trapped nominal-asset holders), with active enforcement as the load-bearing wall.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contestation_kernel_ecb_mandate_127,
    'This story instantiates the expansive_secondary_objectives reading of kernel ecb_mandate_article_127. If the orthodox_price_stability sibling prevailed instead, the constraint restructures entirely — savers become the protected beneficiaries, workers and debtors the cost-bearers, and the discretionary-balancing apparatus becomes the violation rather than the rule. Where exactly does the dispute bind?',
    'Consolidated CJEU jurisprudence explicitly characterizing the clause''s operativity, or treaty revision specifying operational criteria for secondary-objective weighting; failing that, sustained Governing Council voting patterns under successive presidents.',
    'Orthodox prevalence inverts this constraint''s beneficiary/victim sets and redirects chi; the classification computed here would be replaced by a different constraint with different seats rather than re-scored in place.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_kernel_ecb_mandate_127, conceptual, 'Committer structure: one of three live readings of Article 127; sibling prevalence changes the constraint''s identity, not just its score.').

omega_variable(
    discretion_coordination_or_rent,
    'Is the Governing Council''s discretionary balancing primarily a genuine coordination function (countercyclical flexibility for a heterogeneous union that rigid single-target operation would hurt asymmetrically), or primarily authority rent — discretion valuable to its holder for its own sake?',
    'Attribution analysis of policy deviations from a rule-based benchmark to secondary-objective motives, combined with comparative outcomes across mandate designs (Fed dual mandate, BoE flexible targeting, pre-EMU Bundesbank); if discretionary deviations systematically track council convenience rather than measurable regional slack, rent dominates.',
    'Rent-dominant resolution pushes the arrangement toward snare-flavored classification with the council as capturer; coordination-dominant resolution supports the tangled_rope claim with the transfer read as the price of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discretion_coordination_or_rent, empirical, 'Whether the discretion the reading confers is load-bearing coordination or self-serving latitude.').

omega_variable(
    transfer_deliberateness,
    'Does the measured saver-side transfer reflect deliberate distributional choice by the council, or an incidental byproduct of stabilization tools aimed at employment and solvency?',
    'Governing Council deliberation records and published accounts, diverging-vote analysis, and term-structure decomposition distinguishing inflation-compensation effects from policy-rate effects on real deposit returns.',
    'Deliberate resolution authenticates the extraction component of epsilon as chosen policy (raising its structural weight); incidental resolution reclassifies part of the measured epsilon as coordination cost inherent to the tools.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_deliberateness, empirical, 'Whether the distributional flow is intended instrument or spillover.').

omega_variable(
    post_surge_frame_revision,
    'Is the post-2022 return to price-stability-first operation (rate normalization, balance-sheet rundown) a durable revision of this reading''s frame, or the cyclical departure the frame''s own conditionality predicts — with secondary-objective weighting resuming once inflation is again contained?',
    'Observe whether secondary-objective weighting re-enters tool design as inflation normalizes: new TLTRO-style facilities, purchase-flexibility language, strategy-review follow-through; compare stated strategy text against actual program composition over the next cycle.',
    'Durable revision would decay this constraint toward piton territory (rhetoric outliving function, theater_ratio climbing); cyclical resolution confirms the oscillation documented in the measurement series as frame-consistent behavior rather than drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_surge_frame_revision, empirical, 'Whether the 2022-25 tightening marks permanent frame shrinkage or scheduled conditionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ecb_mandate_article_127__expansive_secondary_objectives, 1999, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecb_mandate_expansive_tr_t1999, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 1999, 0.2).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t1999, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2003, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2003, 0.22).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2003, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2008, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2008, 0.25).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2008, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2012, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2012, 0.3).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2012, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2015, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2015, 0.28).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2015, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2020, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2020, 0.24).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2020, observed).
narrative_ontology:measurement(ecb_mandate_expansive_tr_t2025, ecb_mandate_article_127__expansive_secondary_objectives, theater_ratio, 2025, 0.32).
narrative_ontology:measurement_basis(ecb_mandate_expansive_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ecb_mandate_expansive_be_t1999, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 1999, 0.28).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t1999, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2003, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2003, 0.31).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2003, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2008, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2008, 0.36).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2008, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2012, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2012, 0.47).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2012, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2015, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2015, 0.55).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2015, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2020, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2020, observed).
narrative_ontology:measurement(ecb_mandate_expansive_be_t2025, ecb_mandate_article_127__expansive_secondary_objectives, base_extractiveness, 2025, 0.56).
narrative_ontology:measurement_basis(ecb_mandate_expansive_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecb_mandate_expansive_su_t1999, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 1999, 0.25).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t1999, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2003, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2003, 0.28).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2003, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2008, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2008, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2012, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2012, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2015, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2015, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2020, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2020, observed).
narrative_ontology:measurement(ecb_mandate_expansive_su_t2025, ecb_mandate_article_127__expansive_secondary_objectives, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(ecb_mandate_expansive_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ecb_mandate_article_127__expansive_secondary_objectives, resource_allocation).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, orthodox_price_stability).
narrative_ontology:affects_constraint(ecb_mandate_article_127__expansive_secondary_objectives, climate_incorporation).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the ECB mandate' conflates three structurally distinct claims with different epsilon values, different beneficiary sets, and different failure modes. This story authors the expansive reading (epsilon ~0.56, beneficiaries: workers/debtors/treasuries/council; payers: savers/bondholders). The orthodox sibling inverts the seat structure (savers protected, workers bear adjustment costs). Climate incorporation EXTENDS this reading's gateway — the expansive precedent that general-EU-policy objectives can take operational weight is exactly the doctrinal bridge climate integration crosses, which is why the influence edge runs from this story to that one. Upstream/downstream: the treaty text and CJEU ratification anchor all three; contest flows between them through council votes, court dockets, and strategy reviews. Linked via affects_constraints; never merged into one observable-dependent story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
