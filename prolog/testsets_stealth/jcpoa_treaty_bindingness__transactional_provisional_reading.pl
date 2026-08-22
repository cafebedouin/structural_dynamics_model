% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   Under the transactional_provisional_reading, the JCPOA is a revocable
 *   exchange among sovereigns: performance is elected, not owed, and any
 *   party may exit upon its own determination that another has acted in bad
 *   faith. The standing arrangement under contest — the framework as it
 *   actually operated from signature (2015) through withdrawal (2018),
 *   snapback attempt (2020), and progressive Iranian breaches — solved a real
 *   coordination problem (verified sanctions-for-limits exchange) while
 *   channeling asymmetric losses through the same structure: Iran's
 *   concessions were physically irreversible, its counterparties' revocable,
 *   and the determination-right holder converted that gap into option value.
 *   KEY AGENTS (by structural relationship): us_executive_branch —
 *   determination-right holder and agenda setter (institutional/arbitrage);
 *   us_congressional_opposition_coalition — beneficiary (powerful/mobile);
 *   iranian_state_and_economy — primary bearer of irreversible concessions
 *   (organized/constrained); european_union_signatories — dual-positioned
 *   payer (institutional/constrained); israeli_government and
 *   regional_sunni_monarchies — non-party beneficiaries (mobile);
 *   chinese_russian_energy_importers — insulated beneficiaries
 *   (powerful/mobile); iaea_inspectorate — verification operator collecting
 *   mandate and funding (institutional/constrained); iranian_civil_society —
 *   excluded bearer (powerless/trapped); nonproliferation_policy_community —
 *   analytical observer. Claim/metric independence is deliberate: the claimed
 *   type (tangled_rope) states what I believe structurally true of the
 *   arrangement's operation; the metrics state what I believe descriptively
 *   true; the engine computes per-seat classifications from the structural
 *   data and owns any divergence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.54).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.52).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.54).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework Voidable Upon Unilateral Bad-Faith Determination").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '9da2bf7d-a2dc-436a-88bc-66fbb8175c91').
narrative_ontology:cs_kernel_codification('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', formalized).
narrative_ontology:cs_authority_grounding('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', distributed).
narrative_ontology:cs_reading_relation('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', jcpoa_treaty_bindingness__binding_multilateral_reading, influences).
narrative_ontology:cs_reading_relation('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', foundational, national_determination_of_compliance).
narrative_ontology:cs_axiom_status(national_determination_of_compliance, holdable).
narrative_ontology:cs_axiom_grounding('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', national_determination_of_compliance, conventional).
narrative_ontology:cs_axiom('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', foundational, performance_conditioned_on_mutual_satisfaction).
narrative_ontology:cs_axiom_status(performance_conditioned_on_mutual_satisfaction, holdable).
narrative_ontology:cs_axiom_grounding('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', performance_conditioned_on_mutual_satisfaction, instrumental).
narrative_ontology:cs_reference_frame('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', sovereign_transactional_exchange).
narrative_ontology:cs_drift_state('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', post_unilateral_withdrawal_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('9da2bf7d-a2dc-436a-88bc-66fbb8175c91', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, us_congressional_opposition_coalition).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, israeli_government).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_sunni_monarchies).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, chinese_russian_energy_importers).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_inspectorate).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state_and_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, european_union_signatories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, european_union_signatories).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, national_determination_of_bad_faith).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__transactional_provisional_reading, executive_agreement_flexibility_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets whether the arrangement continues: conducts the periodic compliance certifications, determines unilaterally whether counterparties have acted in bad faith, and exercised that determination by withdrawing in 2018 and reimposing sanctions by national decision, including a Security Council snapback attempt most members declined to join. Collects the practical value of counterparties' completed concessions while recovering its own financial leverage; its exit costs are limited chiefly to alliance friction.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch, beneficiary).

% Legislative bloc that opposed the arrangement from signature, used the statutory review calendar and recurring certification requirements to keep it perpetually contestable, and rewarded the executive's exit. Bears almost none of the arrangement's operating costs and collects the policy outcome it sought.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, us_congressional_opposition_coalition, beneficiary,
    powerful, biographical, mobile, national).

% Delivered the physically irreversible half of the exchange by Implementation Day 2016 — thousands of centrifuges removed and destroyed, the enriched-uranium stockpile shipped abroad, the Arak reactor core disabled — in return for sanctions relief that counterparties revoked in 2018. Retains the ability to breach limits incrementally, and has done so, rebuilding enrichment capability at rising economic and diplomatic cost; full exit would forfeit the remaining relief entirely.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_state_and_economy, payer,
    organized, generational, constrained, regional).

% Extended export credits, energy contracts, and banking reintegration on the expectation the exchange would hold. Formally maintained the arrangement after 2018 while their firms withdrew under threat of exclusion from US financial channels; the special-purpose vehicle created to preserve trade processed negligible volume. Gained verified limits on the Iranian program while the exchange operated.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, european_union_signatories, payer,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__transactional_provisional_reading, european_union_signatories, beneficiary).

% Never a party; opposed the arrangement from signature and published intelligence dossiers documenting Iranian activities beyond its scope. Its security position improved when the transactional reading enabled US exit and renewed economic pressure; it bears none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, israeli_government, beneficiary,
    institutional, generational, mobile, regional).

% Gulf monarchies that lobbied against the arrangement, objected to its omission of missile and proxy activity, and benefited from the renewed pressure that followed the US exit; similarly unburdened by its costs.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, regional_sunni_monarchies, beneficiary,
    powerful, generational, mobile, regional).

% Purchased discounted Iranian crude during sanction gaps, expanded refinery and port investments tied to Iranian supply, and rejected the 2020 snapback procedure. Their trade exposure gives them a stake in the arrangement's persistence, while insulation from US financial channels lowers their exposure to its enforcement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, chinese_russian_energy_importers, beneficiary,
    powerful, generational, mobile, continental).

% Runs the continuous verification the exchange rests on — daily access to declared sites, environmental sampling, surveillance-camera continuity — and collected expanded mandate, staffing, and funding from the arrangement's operation. Its access now depends on Iranian cooperation that has narrowed since 2021.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iaea_inspectorate, beneficiary,
    institutional, generational, constrained, global).

% Endured the sanctions-era inflation, medicine shortages, and currency collapse both before the exchange and after its collapse, without a seat in the negotiation or the joint commission; household welfare was the principal lever the exchange moved in both directions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iranian_civil_society, excluded,
    powerless, biographical, trapped, national).

% Analytical community tracking enrichment levels, inspection access, and precedent effects; assesses whether the arrangement's collapse raised proliferation incentives elsewhere and documents the gap between declared rationales and verified compliance records.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, nonproliferation_policy_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, us_executive_branch).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a covert-enrichment arms race into an inspectable exchange: Iran accepts intrusive continuous monitoring and physical limits on centrifuges, stockpiles, and reactor design; the P5+1 and EU deliver calibrated sanctions relief; a joint commission and a Security Council-endorsed resolution arbitrate disputes and timeline questions.
% TRANSFER_FUNCTION: Moves purchasing power and trade access from the P5+1 economies to Iran (frozen assets released, oil exports reopened, banking channels restored) and moves verified nuclear restraint — breakout time, inspected inventories, dismantled capacity — onto the verifying parties' security ledgers.
% ABSENT_VOICES: Iranian civil society, which absorbs the sanctions lever in both directions, had no seat; third-country firms coerced by secondary sanctions had no seat; future administrations bound by predecessors' determinations had no seat. All sat outside the joint commission, which seated signatory governments only.
% DISAPPEARANCE_RATIONALE: After 2018 the world visibly rearranged: Iranian enrichment climbed from 3.67% to 60%, inspection access narrowed, Gulf states began discussing indigenous enrichment, European firms rewrote supply chains around US financial-channel risk, and successive negotiation rounds reorganized around the abandoned framework's terms. Regional procurement decisions, sanctions architectures, and inspectorate budgeting all depend on whether such an exchange exists.
% FOUNDING_PROBLEM: Verifiably foreclose an Iranian nuclear weapons breakout — covert enrichment or plutonium routes — through negotiated, inspected limits exchanged for economic reintegration, avoiding both a preventive war and an uninspected dash.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: inspectorate Board of Governors reports documenting enrichment levels and narrowing access after 2018; NPT Review Conference working papers treating the erosion as a regime-level problem; European foreign-ministry statements defending the exchange's continuation on security grounds; and the persistence of the Iranian program's advanced capability, attested by inspectors rather than by any party to the original bargain.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.54, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.54: the exchange delivered real value in both directions while it held, capping epsilon below snare territory, but the reversibility asymmetry let the determination-holder reclaim its own consideration while the counterparty's sunk concessions remained extracted — extraction accumulating to a 2020 peak (0.64) as the framework decayed into pure pressure, then easing (0.54) as the arrangement hollowed into residual architecture. Suppression 0.52: the enforcement machinery (secondary sanctions, financial-channel exclusion, escort and seizure operations) is genuinely coercive, but its incidence falls largely on third-country traders rather than on signatory membership — suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope in the engine's computation. Theater ratio 0.44: verification was functional, but the certification ritual decoupled from assessment by 2017 (compliance certified while the arrangement was declared unacceptable) and the 2020 snapback was procedural performance most Council members declined to join. Accessibility collapse 0.32: understanding the arrangement as voidable leaves alternatives fully accessible — revival rounds, the surviving Council resolution, graduated proposals — which is the reading's own selling point. Resistance 0.62: calibrated Iranian breaches, the European special-purpose vehicle, Russian and Chinese rejection of snapback, and sustained US domestic defense of the exchange. Temporal series run on ONE shared grid, t0=2015 (signature) through t8=2023, annual points, every tracked metric authored at every point; suppression_requirement is tracked because the story's central dynamic is enforcement-capacity change (the post-2018 secondary-sanctions ratchet and its subsequent attrition), not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the determination-holder seat compute differently from identical structural facts. From the us_executive_branch position the arrangement is a voluntary transaction it remains free to decline — low constraint, preserved liberty, exit as legitimate election. From the iranian_state_and_economy seat the same structure operated as enforced exposure: irreversible concessions delivered against revocable consideration, with exit priced beyond reach. The european_union_signatories seat splits internally — governments formally maintained the framework while their firms were coerced out of it, so organizational compliance and class-level exit diverge. The analytical observer seat sees the reversibility asymmetry that no partisan seat foregrounds. The engine computes this divergence per seat from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries cluster near the subsidy pole: the us_executive_branch combines agenda control with arbitrage-grade exit (it exits while retaining others' completed concessions), driving its derived d toward the beneficiary end despite bearing alliance-friction costs; the congressional coalition, Israeli and Gulf governments, and energy importers collect outcomes without bearing operating costs, with mobile exit damping their effective extraction further. The iaea_inspectorate holds a mild beneficiary position — real collections, constrained autonomy. Targets cluster near the extraction pole: iranian_state_and_economy sits nearest the full-target end (constrained exit, generational horizon, sunk irreversible costs), and european_union_signatories derive mid-high d as payers partially offset by verified-limit gains. iranian_civil_society carries near-target directionality with no seat at all — the absent-voices entry records that its extraction registers nowhere in the commission's accounting. Global enforcement scope amplifies effective extraction modestly in the engine's computation; no directionality overrides are authored because the beneficiary/victim declarations plus exit atoms already separate the seats correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifiably foreclosing an Iranian breakout — remains live, corroborated by inspectorate reporting from outside the beneficiary set, so no zombie declaration is authored: the arrangement's post-collapse persistence is pressure architecture, not inertial theater, and the elevated theater ratio traces to certification and snapback performance rather than vestigial maintenance. The tangled_rope claim prevents two symmetrical mislabelings: reading the arrangement as pure coordination (which the reversibility asymmetry and enforcement ratchet contradict) or as pure extraction (which the genuine two-way delivery and open exit rights contradict). The R5 mismatch consumer finds status=live crossed with verdict=world_rearranges — consistent, no capture flag. Fixing cost is prohibitive: revival rounds failed against collapsed trust, advanced enrichment, and hardened domestic coalitions on both sides, while the gains from fixing accrue diffusely except at the determination-holder seat, which is recorded as the receipt surface because the story establishes that recovered leverage and counterparties' sunk concessions demonstrably accrued there.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_location,
    'This constraint is one reading of kernel jcpoa_treaty_bindingness — the transactional_provisional_reading. Which structural element do the three readings disagree on, and what would each sibling change?',
    'Comparative structural audit across the three sibling stories (binding_multilateral_reading, graduated_compliance_reading, this file): victim sets, exit structures, and epsilon referents authored per reading.',
    'Adopting the binding_multilateral sibling converts the 2018 exit from a legitimate election into a violation, moving the defecting seat sharply toward the target pole; the graduated sibling inserts proportional-assessment machinery between the poles and redistributes extraction across compliance tiers. This file''s epsilon is indexed to the transactional reading''s lights only.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_location, conceptual, 'Committer structure: one reading of a three-reading kernel; disagreement located in the bindingness premise.').

omega_variable(
    bad_faith_determination_standard,
    'What evidence threshold governs a unilateral bad-faith determination sufficient to void the arrangement — sunset clauses, missile activity, regional conduct, inspector findings, or some narrower standard?',
    'Cross-case comparison of the grounds actually invoked at each determination point against the inspectorate-verified compliance record at the same dates.',
    'A broad standard keeps the framework permanently provisional and concentrates option value in the determination-holder; a narrow standard converges this reading toward the graduated sibling and lowers measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bad_faith_determination_standard, conceptual, 'Voidability trigger standard is undefined in the arrangement''s text and set by practice.').

omega_variable(
    concession_reversibility_asymmetry,
    'How much of each side''s delivered consideration was physically or institutionally irreversible at the moment of exit?',
    'Technical audit: centrifuge rebuild timelines, enriched-uranium re-acquisition paths, reactor-core reconstruction estimates, versus sanctions-relief restoration lag and export market-share recovery rates.',
    'High asymmetry confirms that losses were channeled through the exchange structure toward the irreversible conceder; low asymmetry would support a plain-coordination reading with extraction near the coordination-cost floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concession_reversibility_asymmetry, empirical, 'Magnitude of the reversibility gap between the two halves of the exchange.').

omega_variable(
    secondary_sanctions_incidence,
    'On whom does the enforcement machinery''s coercive burden actually fall — signatory governments or third-country traders and financiers?',
    'Trade-flow and firm-exit data 2018-2021, special-purpose vehicle throughput records, and financial-channel exclusion case files.',
    'If incidence falls mainly on non-signatory traders, the measured suppression describes coercion applied outside the consent structure, changing whose exit options the numbers characterize and raising the effective extraction seen by bystander seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_sanctions_incidence, empirical, 'Incidence of enforcement coercion across signatory and non-signatory populations.').

omega_variable(
    provisionality_design_vs_loophole,
    'Was the arrangement''s provisionality a designed transitional feature of the original bargain, or an exploitable gap that the transactional reading opened after signature?',
    'Drafting history of the review, dispute-resolution, and sunset provisions; stated intent of the negotiating principals; subsequent usage patterns of the determination machinery.',
    'Designed-transition resolution pushes the structure toward a transitional-support profile with a natural expiry; loophole resolution amplifies the extraction asymmetry and strengthens the tangled-rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provisionality_design_vs_loophole, conceptual, 'Whether provisionality was authored intent or post-hoc construction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(jcpo_tr_t0, observed).
narrative_ontology:measurement(jcpo_tr_t1, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 1, 0.15).
narrative_ontology:measurement_basis(jcpo_tr_t1, observed).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2, 0.28).
narrative_ontology:measurement_basis(jcpo_tr_t2, observed).
narrative_ontology:measurement(jcpo_tr_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 3, 0.34).
narrative_ontology:measurement_basis(jcpo_tr_t3, observed).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 4, 0.38).
narrative_ontology:measurement_basis(jcpo_tr_t4, observed).
narrative_ontology:measurement(jcpo_tr_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement_basis(jcpo_tr_t5, observed).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 6, 0.44).
narrative_ontology:measurement_basis(jcpo_tr_t6, observed).
narrative_ontology:measurement(jcpo_tr_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 7, 0.43).
narrative_ontology:measurement_basis(jcpo_tr_t7, observed).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 8, 0.44).
narrative_ontology:measurement_basis(jcpo_tr_t8, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(jcpo_be_t0, observed).
narrative_ontology:measurement(jcpo_be_t1, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 1, 0.38).
narrative_ontology:measurement_basis(jcpo_be_t1, observed).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2, 0.46).
narrative_ontology:measurement_basis(jcpo_be_t2, observed).
narrative_ontology:measurement(jcpo_be_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(jcpo_be_t3, observed).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 4, 0.61).
narrative_ontology:measurement_basis(jcpo_be_t4, observed).
narrative_ontology:measurement(jcpo_be_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 5, 0.64).
narrative_ontology:measurement_basis(jcpo_be_t5, observed).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement_basis(jcpo_be_t6, observed).
narrative_ontology:measurement(jcpo_be_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 7, 0.57).
narrative_ontology:measurement_basis(jcpo_be_t7, observed).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(jcpo_be_t8, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(jcpo_su_t0, observed).
narrative_ontology:measurement(jcpo_su_t1, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 1, 0.3).
narrative_ontology:measurement_basis(jcpo_su_t1, observed).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2, 0.36).
narrative_ontology:measurement_basis(jcpo_su_t2, observed).
narrative_ontology:measurement(jcpo_su_t3, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement_basis(jcpo_su_t3, observed).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement_basis(jcpo_su_t4, observed).
narrative_ontology:measurement(jcpo_su_t5, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement_basis(jcpo_su_t5, observed).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(jcpo_su_t6, observed).
narrative_ontology:measurement(jcpo_su_t7, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 7, 0.56).
narrative_ontology:measurement_basis(jcpo_su_t7, observed).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(jcpo_su_t8, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, resource_allocation).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, npt_article_x_exit_precedent).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, us_secondary_sanctions_architecture).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'JCPOA bindingness' conflates three structurally distinct claims — binding multilateral obligation (consensus dissolution), graduated reciprocal commitment (proportional assessment), and provisional transactional exchange (unilateral bad-faith voidability). Each claim carries its own epsilon, victim set, and classification; this file instantiates the third. The upstream multilateral claim (Security Council endorsement) was cited as evidence AGAINST the transactional reading, so the influence edge runs from this reading's exercise (exit, snapback) back onto the siblings' operating environments. All family members link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
