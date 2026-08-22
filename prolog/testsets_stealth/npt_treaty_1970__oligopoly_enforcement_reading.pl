% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Oligopoly Enforcement Reading: Horizontal Prevention as Binding Law, Vertical Disarmament as Aspiration
 *   domain: political/international_law/nuclear_security
 *
 * SUMMARY:
 *   This story instantiates one reading of the NPT kernel: the
 *   oligopoly-enforcement reading, under which Articles I and II constitute
 *   the primary binding obligation set — horizontal proliferation prevention,
 *   actively enforced through IAEA safeguards, supplier-group controls, and
 *   Security Council referral — while Article VI's disarmament commitment is
 *   contingent and aspirational, carrying no justiciable timetable and no
 *   accountability mechanism. The epsilon referent is the standing
 *   arrangement under contest: the regime as actually administered, with its
 *   asymmetric burden allocation, assessed by this reading's own lights. The
 *   claim and the metrics are independent authored facts: claimed_type
 *   records what I take to be structurally true of this arrangement (a
 *   genuine coordination core wrapped in asymmetric extraction), while the
 *   metric values record what I take to be descriptively true of its
 *   operation; the engine computes per-seat classifications from the
 *   structural data and any divergence between claim and computation is the
 *   datum, not an error to reconcile.
 *
 * KEY AGENTS:
 *   - p5_nuclear_weapon_states: Primary beneficiary and agenda-setter (institutional/arbitrage) — administers enforcement, exempt from comprehensive safeguards, anchors the status hierarchy
 *   - nnws_parties: Primary target/payer (organized/constrained) — bears the inspection burden and the permanently foregone deterrent
 *   - treaty_threshold_parties: Dual-positioned payer/beneficiary (powerful/identity_locked) — denied an independent deterrent, subsidized by extended deterrence
 *   - contested_compliance_parties: Secondary target (moderate/trapped) — intensified scrutiny, denied both weapons and the outsider-integration path
 *   - regime_outside_nuclear_powers: Excluded non-parties (powerful/mobile) — acquired outside the rules they were barred from writing, later selectively integrated
 *   - iaea_secretariat: Administering collector (institutional/constrained) — mandate and budget grow with enforcement scope, politically bounded by its funders
 *   - civil_society_disarmament_advocates: Excluded voice (organized/mobile) — presses the deferred disarmament half from outside the voting structure
 *   - analytical_regime_theorists: Analytical observer (analytical/analytical) — maps the full structure without material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.62).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement Reading: Horizontal Prevention as Binding Law, Vertical Disarmament as Aspiration").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "political/international_law/nuclear_security").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'c427c5fa-57e3-4531-aed8-28c1001db059').
narrative_ontology:cs_kernel_codification('c427c5fa-57e3-4531-aed8-28c1001db059', fixed_text).
narrative_ontology:cs_authority_grounding('c427c5fa-57e3-4531-aed8-28c1001db059', extraction).
narrative_ontology:cs_interpretation_layer_present('c427c5fa-57e3-4531-aed8-28c1001db059').
narrative_ontology:cs_reading_relation('c427c5fa-57e3-4531-aed8-28c1001db059', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('c427c5fa-57e3-4531-aed8-28c1001db059', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('c427c5fa-57e3-4531-aed8-28c1001db059', foundational, horizontal_nonproliferation_primary_obligation).
narrative_ontology:cs_axiom_status(horizontal_nonproliferation_primary_obligation, holdable).
narrative_ontology:cs_axiom_grounding('c427c5fa-57e3-4531-aed8-28c1001db059', horizontal_nonproliferation_primary_obligation, conventional).
narrative_ontology:cs_axiom('c427c5fa-57e3-4531-aed8-28c1001db059', foundational, p5_retention_stability_necessity).
narrative_ontology:cs_axiom_status(p5_retention_stability_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c427c5fa-57e3-4531-aed8-28c1001db059', p5_retention_stability_necessity, instrumental).
narrative_ontology:cs_reference_frame('c427c5fa-57e3-4531-aed8-28c1001db059', asymmetric_nonproliferation_compact).
narrative_ontology:cs_drift_state('c427c5fa-57e3-4531-aed8-28c1001db059', contemporary_post_new_start_lapse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c427c5fa-57e3-4531-aed8-28c1001db059', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nnws_parties).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, treaty_threshold_parties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, iaea_secretariat).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, treaty_threshold_parties).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, contested_compliance_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five states recognized as nuclear-weapon states under the treaty's 1967 cutoff definition. They retain arsenals indefinitely, hold veto power in the Security Council organ that answers treaty violations, are exempt from the comprehensive inspections applied to every other party category, and supply most of the security assurances and diplomatic leadership through which the regime operates. They shape what counts as compliance, which crises reach the Council, and which interpretive moves gain traction; leaving the arrangement would mean dismantling the hierarchy they anchor, so they do not leave — they administer it.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% The Vienna-based secretariat that designs and runs the inspection system, reports findings to the Board of Governors and the Security Council, and has seen its verification mandate, staff, and budget expand with each post-1991 crisis. Its funding and political room depend on member-state contributions and Board majorities in which the recognized nuclear states weigh heavily; it can detect and report violations but cannot itself sanction, and its findings enter a political process it does not control.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, iaea_secretariat, beneficiary).

% The large body of non-nuclear-weapon states parties. They accept comprehensive safeguards, additional protocols, and supplier-discipline, and permanently forgo the weapons option; in exchange they receive civilian nuclear cooperation, security assurances of varying formality, and a standing procedural voice at review conferences. Acting as blocs they press the disarmament half of the bargain every five years and see it deferred every five years. Leaving would price them as pariah suppliers and invite the treatment meted out to the one state that withdrew.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nnws_parties, payer,
    organized, generational, constrained, global).

% Technologically capable allies of the leading nuclear power — Japan, Germany, South Korea — that could plausibly field weapons within a few years of a decision but are folded into extended deterrence. They pay the same inspection and foregone-option costs as other non-nuclear parties while receiving alliance protection that substitutes for an independent deterrent. Their non-nuclear posture is woven into postwar constitutional settlements and alliance bargains such that revisiting it would rupture relationships far beyond the treaty; the option is less forbidden than unthinkable inside their current identities.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, treaty_threshold_parties, payer,
    powerful, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, treaty_threshold_parties, beneficiary).

% Parties under standing suspicion or sanction — the recurring profile of Iran — that remain inside the treaty, submit to inspection intensities well beyond what most parties face, and argue the arrangement denies them both the weapons their regional rivals obtained and the commercial integration later granted to a state that tested outside the treaty. Their exit is priced by sanctions precedent; their continued membership is priced by escalating scrutiny and periodic referral to the Security Council.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, contested_compliance_parties, payer,
    moderate, biographical, trapped, regional).

% The three states that acquired and retained arsenals without ever joining — India, Pakistan, Israel. They were absent from the 1968 bargaining that fixed the cutoff date above their tests, remain outside the treaty's governance structures, and live under its supplier-denial consequences; one of them was later granted an exception and integrated into nuclear commerce after the fact. Were they in the room, they would reorder the regime around demonstrated capability rather than 1967 vintage.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, regime_outside_nuclear_powers, excluded,
    powerful, generational, mobile, regional).

% NGO coalitions, affected-community representatives, and academic campaigns that attend review conferences without a vote and built the parallel humanitarian-initiative track that produced a ban treaty the recognized nuclear states refuse to join. They press the disarmament half of the bargain, document its non-performance, and hold no seat where the regime's rules are made.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, civil_society_disarmament_advocates, excluded,
    organized, generational, mobile, global).

% Scholars of international institutions and security who map the regime's bargain structure, compliance record, and exit dynamics across the full interval. They hold no material stake in the arrangement's continuation and publish the comparisons the participants litigate.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, analytical_regime_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the cascade-proliferation collective action problem: IAEA verification makes each state's restraint observable and credible to its neighbors, supplier-group coordination prevents sensitive-transfer races, and the treaty converts every state's security-dilemma incentive to arm into a legible, inspectable commitment — lowering the number of arsenals the system must stabilize.
% TRANSFER_FUNCTION: Moves inspection access, technological-choice restriction, and the permanently foregone deterrent option from non-nuclear parties to the P5-administered regime; moves status permanence and strategic-stability rents to the five recognized nuclear states; moves verification funding and reporting deference to the IAEA; moves security-assurance leverage to the alliance leader among the P5.
% ABSENT_VOICES: The regime-outside nuclear powers were absent from the 1968 bargaining that fixed the cutoff above their tests and remain outside the regime's governance while living under its export-control consequences; their standing objection — that the treaty freezes a discriminatory hierarchy — enters review conferences only as non-party statements. Inside the regime, the non-nuclear bloc's demand for binding disarmament timetables has been voiced continuously since the first review cycle and structurally deferred; civil-society advocates hold no vote anywhere in the rule-making chain.
% DISAPPEARANCE_RATIONALE: Verification infrastructure would vanish overnight; hedging incentives would surface in East Asia, the Middle East, and possibly Europe within months; supplier coordination would fragment into national licensing regimes; and the P5 status hierarchy would lose its legal anchor while its material basis remained intact — a combination historically associated with rapid destabilizing acquisition decisions by states currently deterred from even debating the option.
% FOUNDING_PROBLEM: The early-1960s projection of dozens of nuclear states within two decades — the Nth-country problem — sharpened by the Cuban missile crisis: a cascade in which each state's security response to its neighbor's acquisition produced systemic instability that no one, including the superpowers, wanted.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the non-nuclear bloc attests the cascade danger is live while disputing the allocation of burdens; IAEA technical reporting and the North Korean and Iranian files document continuing proliferation pressure; SIPRI and independent proliferation scholarship corroborate; and the ban-treaty coalition, which rejects the regime's structure outright, attests the same underlying danger. No seat involved claims the cascade problem is solved.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68: under this reading the arrangement transfers inspection burdens, technological choice, and the deterrent option from the many to the few while the reciprocal half of the bargain accumulates fifty-six years of unperformed promises — but the transfer rides on a coordination good (cascade prevention) that the payers themselves affirmatively want, which caps epsilon below snare territory. Suppression is authored at 0.62 as a raw structural property, unscaled by power or scope: the enforcement machinery (comprehensive safeguards, Additional Protocol, supplier denials, sanctioned-exit precedent) is real and coercive, and only the engine scales extractiveness. Theater at 0.45 reflects the ritualized review-conference layer — reaffirmation cycles that produce consensus documents and no consequence — sitting alongside a verification apparatus that does genuine work. Accessibility_collapse at 0.5: alternatives remain visible and have been exercised exactly once (withdrawal) plus persistently via non-membership, but each is heavily priced. Resistance at 0.55: bloc pressure every review cycle, the ban-treaty parallel track, threshold refusal to join, and one completed exit. The measurement series run on one shared grid (t = 0, 8, 16, 24, 32, 40, 48, 56) so every tracked metric is authored at every examined point; the extractiveness step-change across t24-t32 corresponds to the 1995 indefinite-extension decision, which converted a treaty designed around a 25-year sunset review into permanent structure without extracting new disarmament leverage in exchange — the single largest hardening event in the interval. The suppression_requirement series is included because enforcement capacity is precisely what this story traces: it ratchets from thin item-specific inspections at t0 to the post-Iraq Additional Protocol architecture, and plateaus as enforcement matures.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the P5 seat the arrangement is a stability architecture they built, fund diplomatically, and police — coordination with their privileges as its operating cost, and no accountability surface anywhere in view. From the non-nuclear payer seat the same structure is a bargain whose binding half is inspected daily and whose reciprocal half is reviewed ritually every five years. From the threshold seat it is a denied option softened by an alliance subsidy and fused with postwar identity. From the outside-powers seat it is a cartel that froze the rules above their heads and then sold an exception to one of them. The engine computes these divergent per-seat classifications from the power, exit, and role data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 combine the agenda-setter role with sole formal beneficiary status and arbitrage-grade exit (they define compliance and sit outside the inspection burden), placing them near the full-beneficiary end of directionality. Non-nuclear parties are declared victims with constrained exit — withdrawal is priced by the sanctioned precedent — placing them near the full-target end. Threshold parties are dual-declared (payer with beneficiary secondary): the alliance subsidy damps their derived directionality below the pure-payer seats, and the counterfactual omega tests whether the damping is real or nominal. Contested-compliance parties, though not in the formal victim array, sit effectively trapped at high extraction intensity. The excluded outside powers derive near-symmetric directionality: free-riding on restraint they do not pay for offsets the supplier-denial costs imposed on them. The secretariat derives low-to-moderate directionality as administering collector. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct ordering, and the override mechanism keys on power atoms that several distinct seats share, making overrides coarser than the derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cascade proliferation — is live, so no mandatrophy resolution is declared and none should be inferred from the theater plateau. The tangled_rope classification does the protective work in both directions: reading the arrangement as pure extraction (snare) would erase the reason roughly 190 states stay — the cascade fear is genuine, mutually held, and affirmed by every seat including the regime's sharpest critics — while reading it as pure coordination (rope) would erase the 1995 indefinite-extension lock-in, the accountability vacuum on the vertical half, and the concentrated status rents. The sunset-clause flag is deliberately false: the treaty originally carried a 25-year review provision — a transitional design element — and the 1995 decision extinguished it, converting a reviewable arrangement into permanent structure; tracking that conversion is exactly what the temporal series exists to catch, and a scaffold misclassification today would miss that the transition was cancelled rather than completed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_modality_indexicality,
    'Is Article VI''s legal character — binding obligation versus aspirational commitment — determinable from the treaty text and drafting history, or irreducibly indexical to the reading a party adopts?',
    'Authoritative interpretation via ICJ advisory proceedings or a binding review-conference interpretive declaration; comparative classification against the sibling reciprocal-disarmament file.',
    'If Article VI is held binding with temporal urgency, the P5 enter the debtor side of the ledger, effective extraction on non-nuclear seats falls, and the arrangement migrates toward defaulted-bargain territory; if aspirational, this file''s structure stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_modality_indexicality, conceptual, 'Whether the reading-indexed legal modality of Article VI is resolvable or constitutively contested.').

omega_variable(
    sibling_delta_reciprocal_reading,
    'What structurally changes if the reciprocal_disarmament_reading is adopted instead of this oligopoly_enforcement_reading of kernel npt_treaty_1970?',
    'Compile and classify the sibling file; diff victim sets, directionality assignments, and computed types across the two readings of the same text.',
    'Under the sibling reading the same conduct reads as breach of a reciprocal bargain rather than burden allocation: non-nuclear parties become creditors, the P5 become debtors, and the enforcement asymmetry becomes evidence of default rather than design — a different constraint with a different epsilon, not a different measurement of this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_delta_reciprocal_reading, conceptual, 'Committer delta against the reciprocal-disarmament sibling reading.').

omega_variable(
    withdrawal_exit_pricing_interaction,
    'How does this reading''s enforcement practice interact with the withdrawal_sovereignty_reading''s exit option — does sanctioning the one state that withdrew raise the price of exit for every other party?',
    'Track hedging-state compliance investment and internal withdrawal-cost assessments following the North Korean precedent; compare against pre-1993 exit expectations documented in the scholarly record.',
    'If exit pricing is the load-bearing enforcement mechanism, part of this constraint''s suppression is manufactured by the sibling reading''s exercise; if exit was always implausible for allied threshold states regardless, the pricing effect is marginal and suppression rests on the inspection machinery alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_exit_pricing_interaction, empirical, 'Cross-reading interaction between enforcement severity and withdrawal credibility.').

omega_variable(
    coordination_extraction_separability,
    'Is the cascade-prevention coordination function separable from the status-hierarchy asymmetry — could horizontal restraint persist without the P5 privilege structure?',
    'Compare restraint outcomes in regions covered by nuclear-weapon-free-zone treaties achieved without P5 hierarchy (Latin America, Africa, Central Asia, Southeast Asia) against regions relying on the global regime alone; test whether zone-style verification sustains restraint absent status asymmetry.',
    'If separable, the asymmetry is rent riding on a real coordination good and remediable without losing the good; if inseparable, part of the measured asymmetry is the operating cost of the coordination itself and remediation trades away restraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the regime''s coordination good and its hierarchy are structurally separable.').

omega_variable(
    threshold_deterrent_counterfactual,
    'Would the technologically capable allied states actually arm absent the treaty''s prohibition, or does extended deterrence already substitute for the denied option — making part of the denied-deterrent cost nominal?',
    'Alliance-credibility stress tests: measure threshold-state hedging behavior and domestic weapons debates during episodes of perceived abandonment or alliance strain.',
    'If alliance substitution holds, the victim claim for threshold seats weakens and their effective extraction falls toward the beneficiary side; if abandonment fear drives genuine hedging, the denial is a binding cost and the victim claim stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_deterrent_counterfactual, empirical, 'Whether the denied deterrent is a real cost or already substituted by alliance coverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_oligopoly_tr_t0, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(npt_oligopoly_tr_t8, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(npt_oligopoly_tr_t16, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(npt_oligopoly_tr_t24, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(npt_oligopoly_tr_t32, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(npt_oligopoly_tr_t40, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(npt_oligopoly_tr_t48, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 48, 0.44).
narrative_ontology:measurement(npt_oligopoly_tr_t56, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 56, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_oligopoly_be_t0, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(npt_oligopoly_be_t8, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(npt_oligopoly_be_t16, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(npt_oligopoly_be_t24, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(npt_oligopoly_be_t32, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(npt_oligopoly_be_t40, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(npt_oligopoly_be_t48, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 48, 0.66).
narrative_ontology:measurement(npt_oligopoly_be_t56, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 56, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt_oligopoly_su_t0, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(npt_oligopoly_su_t8, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(npt_oligopoly_su_t16, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(npt_oligopoly_su_t24, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(npt_oligopoly_su_t32, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 32, 0.58).
narrative_ontology:measurement(npt_oligopoly_su_t40, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(npt_oligopoly_su_t48, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 48, 0.61).
narrative_ontology:measurement(npt_oligopoly_su_t56, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 56, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the NPT' conflates three structurally distinct constraints instantiated from one kernel text. This file instantiates the oligopoly-enforcement reading (Articles I-II hard, Article VI soft) and authors epsilon for the standing asymmetric arrangement as this reading sees it. The reciprocal-disarmament sibling authors epsilon for the same arrangement read as a breached reciprocal bargain (creditor NNWS, debtor P5); the withdrawal-sovereignty sibling authors epsilon for the exit-contingency structure. Per the epsilon-invariance principle each reading carries its own stable epsilon, its own victim set, and its own classification; the files cross-link here. Upstream/downstream: this reading's enforcement practice shapes the withdrawal sibling's operating environment (the sanctioned-exit precedent), hence the influences edge; the reciprocal sibling coexists as the opposing coalition's live position.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
