% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Welfare Settlement: Sentience-Constrained Instrumental Use (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the modern welfare settlement:
 *   across jurisdictions, animals are recognized as sentient, subjecting
 *   human use to enforceable duties — anti-cruelty statutes, farm animal
 *   codes, project-license review, transport and slaughter rules — while
 *   permitting extensive instrumental use of them. The settlement's defining
 *   structural feature is its exemption architecture: standard industry
 *   practices grandfathered as lawful, religious-slaughter carve-outs,
 *   pest-and-vermin classifications outside code reach, and
 *   scientific-necessity clauses judged by the using institutions themselves.
 *   Protection therefore binds unevenly: dense where enforcement and codes
 *   reach, absent exactly where use is most routine. The claim and the
 *   metrics are authored independently: claimed_type records the structure I
 *   believe true of this arrangement (a genuine coordination function
 *   carrying asymmetric extraction through its exemptions), while the metric
 *   values record its observed operation. Epsilon's referent is fixed
 *   throughout to this standing arrangement, assessed by this reading's own
 *   lights — never to any alternative arrangement the reading might endorse.
 *
 * KEY AGENTS:
 *   - - farmed_animals: Primary target (powerless/trapped) — bear the arrangement's costs in confinement, handling, transport, and slaughter
 *   - - laboratory_animals: Secondary target (powerless/trapped) — bear invasive procedures under institutionally reviewed necessity
 *   - - unprotected_wildlife: Target via exemption (powerless/trapped) — sit wholly outside code reach as pests, game, or vermin
 *   - - industrial_livestock_producers: Primary beneficiary (organized/arbitrage) — operate under the legitimacy shield and capture exemption savings
 *   - - biomedical_research_institutions: Beneficiary (institutional/constrained) — receive licensed access and procedural legitimacy
 *   - - animal_product_consumers: Dual-positioned beneficiary-payer (moderate/mobile) — absorb compliance costs in prices, receive standards and abundance
 *   - - animal_welfare_regulators: Agenda setter (institutional/constrained) — draft, inspect, prosecute, certify
 *   - - animal_advocacy_organizations: Observer (organized/analytical) — litigate, investigate, document gaps
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.62).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare Settlement: Sentience-Constrained Instrumental Use (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '3bce895b-bd85-443a-83c8-288abca5ab19').
narrative_ontology:cs_kernel_codification('3bce895b-bd85-443a-83c8-288abca5ab19', formalized).
narrative_ontology:cs_authority_grounding('3bce895b-bd85-443a-83c8-288abca5ab19', lineage).
narrative_ontology:cs_interpretation_layer_present('3bce895b-bd85-443a-83c8-288abca5ab19').
narrative_ontology:cs_reading_relation('3bce895b-bd85-443a-83c8-288abca5ab19', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3bce895b-bd85-443a-83c8-288abca5ab19', animal_status__property_reading, influences).
narrative_ontology:cs_axiom('3bce895b-bd85-443a-83c8-288abca5ab19', foundational, sentience_grounds_moral_standing).
narrative_ontology:cs_axiom_status(sentience_grounds_moral_standing, holdable).
narrative_ontology:cs_axiom_grounding('3bce895b-bd85-443a-83c8-288abca5ab19', sentience_grounds_moral_standing, deontological).
narrative_ontology:cs_axiom('3bce895b-bd85-443a-83c8-288abca5ab19', foundational, constrained_use_permissible).
narrative_ontology:cs_axiom_status(constrained_use_permissible, holdable).
narrative_ontology:cs_axiom_grounding('3bce895b-bd85-443a-83c8-288abca5ab19', constrained_use_permissible, instrumental).
narrative_ontology:cs_reference_frame('3bce895b-bd85-443a-83c8-288abca5ab19', welfare_constrained_use_settlement).
narrative_ontology:cs_drift_state('3bce895b-bd85-443a-83c8-288abca5ab19', contemporary_post_sentience_recognition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3bce895b-bd85-443a-83c8-288abca5ab19', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, industrial_livestock_producers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_product_consumers).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, unprotected_wildlife).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__welfare_reading, industrial_livestock_producers).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animal_product_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Raised in high-density housing, transported, and slaughtered on schedules set by production economics. Receive whatever space, handling, anesthesia, and stunning the applicable codes require, and nothing where standard-practice or economic carve-outs remove the duty. Cannot leave, refuse, or relocate; every feature of their environment is chosen by others.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals, payer,
    powerless, biographical, trapped, global).

% Bred for and subjected to invasive procedures under project-license review. Protected by replace-reduce-refine requirements whose necessity judgments are made largely by committees staffed from the institutions seeking approval. Cannot exit; their continued availability is the premise the review process administers.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, laboratory_animals, payer,
    powerless, biographical, trapped, continental).

% Classed as pests, vermin, or game and killed under poisoning, trapping, culling, and hunting regimes governed by separate legal frames the protective codes do not reach. Entry into the protected class happens only through reclassification campaigns, one taxon at a time.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, unprotected_wildlife, payer,
    powerless, biographical, trapped, regional).

% Operate the facilities where most protected-class animals live and die. Carry code-compliance costs for housing, enrichment, stunning, and record-keeping, and retain the savings wherever exemptions or thin enforcement spare them those costs. Can shift production across jurisdictions with weaker codes, integrate vertically, or pass residual costs into retail prices.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, industrial_livestock_producers, beneficiary,
    organized, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, industrial_livestock_producers, payer).

% Run licensed facilities and sit on the review committees that approve animal protocols. Depend on continued access to animal models for grant pipelines and publication programs; bear compliance and documentation costs; receive the legitimacy the review framework confers on continued use. Non-animal methods reduce but do not yet replace demand.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_institutions, beneficiary,
    institutional, generational, constrained, global).

% Buy the output: pay compliance costs passed into retail prices, receive assured minimum standards, and enjoy the low prices that exemption-heavy production makes possible. Individual substitution toward plant-based diets is available at moderate cost in money, habit, and social friction; collective exit is not organized.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_product_consumers, beneficiary,
    moderate, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_product_consumers, payer).

% Draft codes, inspect facilities, prosecute violations, and certify exports. Budgets and staffing follow legislative attention; industry consults intensively on code drafts. The portfolio is written to protect animal interests while preserving production continuity, and both aims sit in the same mandate.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Campaign, litigate, and run undercover investigations; win incremental reforms such as stunning mandates, cage bans, and sentience recognitions, and document enforcement gaps behind certifications. Hold no formal seat in code-setting beyond consultation; leverage arrives through publicity and legal challenge rather than administration.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_advocacy_organizations, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(animal_status__welfare_reading, industrial_livestock_producers).
narrative_ontology:fixing_cost_class(animal_status__welfare_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets enforceable minimum standards for human treatment of animals under use: without shared welfare floors, competitive pressure drives each producer toward the cheapest handling, and buyers have no way to distinguish compliant from non-compliant supply. Codes, inspections, and review committees solve that collective-action problem once, centrally.
% TRANSFER_FUNCTION: Moves compliance costs (space, enrichment, anesthesia, humane slaughter equipment, documentation) onto users of animals and onward into consumer prices; moves decision authority over animal interests to human institutions, which weigh those interests against production and research goals; transfers legitimacy for continued use to operators who meet the codes.
% ABSENT_VOICES: The animals whose interests the framework allocates have no seat of their own; they enter only through proxy advocates, veterinary assessment, and scientific inference, all filtered through institutions run by the using parties. Smallholder producers outside formal markets and future persons bearing the ecological and pandemic externalities of intensive systems are likewise unrepresented at code-negotiation tables.
% DISAPPEARANCE_RATIONALE: If the welfare-constrained-use arrangement vanished overnight, the food and research systems built on it would reorganize immediately: either toward wholly unconstrained treatment under bare ownership logic, or toward prohibition of use, with mass restructuring of agriculture, laboratories, and trade. Either branch is a rearrangement, which is itself evidence that substantial human and institutional arrangement depends on the framework's exact shape.
% FOUNDING_PROBLEM: Stop wanton, gratuitous cruelty to domestic animals — working-horse beatings, cattle-drove abuse, unanesthetized vivisection controversies — while preserving the uses society then considered necessary: transport, food production, and later scientific research.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the 1822 Martin's Act, the RSPCA archive, and successive statute revisions corroborates the anti-cruelty origin from outside any benefiting party. Contemporary veterinary-science bodies and published enforcement records attest both directions of the current-status dispute: continuing documented cases of gratuitous harm inside exempted practices (problem still live) and measurable welfare gains where codes bind (problem partly solved). No attestation relies solely on the user industries.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.45, not higher, because the reading itself endorses constrained use: from this seat, welfare-compliant instrumental use is not extraction, and the victim set is confined to gratuitously harmed animals — those inside exempted practices, thin-enforcement zones, and unprotected classifications. The 0.45 measures what the exemption architecture drains from the constraint's own promise, not use as such. Suppression (0.62) is authored as a raw structural property, unscaled by power or scope: it reflects compelled compliance (prosecution, license conditioning) plus the total practical lock on the animals themselves, with no multiplier applied. Theater_ratio (0.50) is high because a large share of visible welfare activity — certification labels, corporate pledges, ethics-committee throughput, audit paperwork — verifies documents more than it changes handling, though real inspection and prosecution continue alongside. Accessibility_collapse (0.48): understanding the arrangement does not collapse alternatives — plant-based substitution, non-animal methods, and abolitionist politics all remain live — but the welfare frame does crowd the discursive middle, presenting itself as the only respectable position. Resistance (0.55): sustained litigation, ballot initiatives, undercover investigation, and industry counter-lobbying against every tightening. The temporal series runs on one shared eight-point grid (1822–2026) with all three tracked metrics authored at every point; the trajectories are monotonic accumulation, not cyclical oscillation — extraction and theater rise together as the settlement expanded from a narrow anti-cruelty statute to the legitimacy infrastructure of industrial-scale use, and suppression_requirement rises with the enforcement machinery built to administer it. Fixing_cost is authored prohibitive: closing the exemption structures requires renegotiating food-system settlements against organized producer opposition, at cost to the fixer far exceeding what the fixer bears. Gain_flow names the producer seat because the exemption savings — the avoided costs of full interest-protection — demonstrably accrue there as margin.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the trapped, powerless payer seats the arrangement is near-total subordination punctuated by occasional relief — every life-feature chosen by others, with duties attaching only where exemptions fail to attach. From the producer seat the same statutes are a manageable cost of doing business and, where exemptions hold, a competitive windfall. From the research seat they are a legitimacy-conferring procedure. From the regulator seat they are a functioning protection system administered in good faith under a dual mandate. Nothing in the authored claim adjudicates among these; the engine derives each seat's classification from power, exit, and directional position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. Farmed, laboratory, and unprotected animals are declared victims with trapped exit — they derive d near the full-target end, and their trap amplifies effective extraction. Industrial producers are declared beneficiaries with arbitrage-grade exit — d near the beneficiary end, with scope-scale operations damping their effective burden further. Researchers derive low d as beneficiaries; consumers, dual-positioned, sit slightly off the beneficiary pole. No directionality_overrides are authored: the available override granularity is the power atom, and the story contains two institutional seats (research institutions, regulators) whose true directionalities diverge — an institutional-level override would correct one seat only by corrupting the other. The mild capture tendency of the regulator seat (fee-funded budgets, intensive industry consultation on code drafts) is recorded qualitatively here rather than forced through an override that cannot target it alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding anti-cruelty mandate is not dead — gratuitous harm continues and the statutes still reach it — so no mandatrophy_resolved flag is authored, and the R5 mismatch check (contested status x world_rearranges verdict) raises no zombie flag. The classification discipline cuts both ways. Reading the arrangement as pure extraction (snare) would erase the real, enforced welfare gains the codes deliver where they bind — measurable reductions in suffering that did not exist before the settlement. Reading it as pure coordination (rope) would erase the exemption architecture through which identifiable payer classes — the animals outside code reach — finance the legitimacy the settlement sells to its users. Tangled_rope holds both facts: a genuine coordination function (standards solving a race-to-the-bottom problem) and asymmetric extraction flowing through the same structure's exemptions, held in place by active enforcement. The rising theater series is watched for Goodhart drift toward inertial maintenance, but the enforcement machinery is still growing, not decaying, so the arrangement is not currently a candidate for the degraded category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This story instantiates only the welfare reading of the animal_status kernel; what structural deltas would the sibling readings produce if instantiated over the same referent arrangement?',
    'Generate and compare the sibling stories (animal_status__abolitionist_reading, animal_status__property_reading); each fixes the same referent — the standing instrumental-use arrangement — and reads it through its own lights.',
    'The abolitionist reading places every instrumentally used animal in the victim set and drives epsilon sharply upward; the property reading empties the victim set entirely and drops epsilon toward the statutory floor. This file''s classification must not blend the three; divergence across the family is the measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: one reading of a contested kernel, with sibling deltas declared.').

omega_variable(
    exemption_boundary_location,
    'Where exactly does the exemption structure end and protected-interest territory begin — which practices count as gratuitous harm rather than welfare-constrained use?',
    'Systematic cross-jurisdictional audit of statutory exemptions, standard-practice carve-outs, enforcement records, and undercover-investigation findings, mapped against the code-covered population.',
    'A wider effective exemption set raises measured extraction toward a snare-like profile; a narrower set supports the coordination-dominant reading. The boundary, not the headline rate, is the load-bearing quantity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exemption_boundary_location, empirical, 'Location of the exemption boundary that drives extraction through the settlement.').

omega_variable(
    sentience_attribution_scope,
    'Which taxa fall inside the protected class as sentience evidence accumulates — cephalopods, decapod crustaceans, insects, and beyond?',
    'Comparative cognition and nociception research programs; statutory review precedents such as decapod and cephalopod recognition decisions.',
    'Each expansion enlarges the victim set and raises effective extraction with no change in human conduct — scope-driven amplification of the same underlying arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_attribution_scope, empirical, 'Taxonomic scope of the protected class under the welfare reading.').

omega_variable(
    theater_drift_diagnosis,
    'Is the rising theater ratio genuine Goodhart drift — certification and documentation substituting for protection — or an artifact of expanded reporting requirements?',
    'Outcome-linked audit correlating certification and audit volume with on-farm and in-facility welfare outcome measures over time.',
    'If drift is real, the welfare layer trends toward inertial maintenance even while use continues; if artifactual, the theater metric overstates decay and the enforcement picture is healthier than the series suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_drift_diagnosis, empirical, 'Whether the rising performative share reflects functional decay or measurement expansion.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel best framed as the formalized statutory text adjudicated through lineage institutions (the framing authored here), or as a diffuse cultural settlement that animal use is normal, with no single adjudicating authority?',
    'Author both framings and compare: under the settlement framing, kernel_codification shifts toward distributed, authority_grounding toward practice, and the drift vector reads as revival_pressure from rights-based quarters rather than practice_drift away from the statutory reference frame.',
    'The alternative framing changes the commitment-system classification and the computed terminal state. Signals guiding the authored choice: codified statutes with named interpretive bodies dominate day-to-day adjudication, making the formalized/lineage framing the better fit for the arrangement as it actually operates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent framings of the same kernel yield different commitment-system classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 1822, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aswr_tr_t1822, animal_status__welfare_reading, theater_ratio, 1822, 0.08).
narrative_ontology:measurement(aswr_tr_t1900, animal_status__welfare_reading, theater_ratio, 1900, 0.14).
narrative_ontology:measurement(aswr_tr_t1959, animal_status__welfare_reading, theater_ratio, 1959, 0.24).
narrative_ontology:measurement(aswr_tr_t1976, animal_status__welfare_reading, theater_ratio, 1976, 0.31).
narrative_ontology:measurement(aswr_tr_t1998, animal_status__welfare_reading, theater_ratio, 1998, 0.39).
narrative_ontology:measurement(aswr_tr_t2009, animal_status__welfare_reading, theater_ratio, 2009, 0.43).
narrative_ontology:measurement(aswr_tr_t2020, animal_status__welfare_reading, theater_ratio, 2020, 0.47).
narrative_ontology:measurement(aswr_tr_t2026, animal_status__welfare_reading, theater_ratio, 2026, 0.5).

% Extraction over time
narrative_ontology:measurement(aswr_be_t1822, animal_status__welfare_reading, base_extractiveness, 1822, 0.14).
narrative_ontology:measurement(aswr_be_t1900, animal_status__welfare_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(aswr_be_t1959, animal_status__welfare_reading, base_extractiveness, 1959, 0.29).
narrative_ontology:measurement(aswr_be_t1976, animal_status__welfare_reading, base_extractiveness, 1976, 0.34).
narrative_ontology:measurement(aswr_be_t1998, animal_status__welfare_reading, base_extractiveness, 1998, 0.4).
narrative_ontology:measurement(aswr_be_t2009, animal_status__welfare_reading, base_extractiveness, 2009, 0.43).
narrative_ontology:measurement(aswr_be_t2020, animal_status__welfare_reading, base_extractiveness, 2020, 0.44).
narrative_ontology:measurement(aswr_be_t2026, animal_status__welfare_reading, base_extractiveness, 2026, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(aswr_su_t1822, animal_status__welfare_reading, suppression_requirement, 1822, 0.1).
narrative_ontology:measurement(aswr_su_t1900, animal_status__welfare_reading, suppression_requirement, 1900, 0.17).
narrative_ontology:measurement(aswr_su_t1959, animal_status__welfare_reading, suppression_requirement, 1959, 0.28).
narrative_ontology:measurement(aswr_su_t1976, animal_status__welfare_reading, suppression_requirement, 1976, 0.38).
narrative_ontology:measurement(aswr_su_t1998, animal_status__welfare_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(aswr_su_t2009, animal_status__welfare_reading, suppression_requirement, 2009, 0.56).
narrative_ontology:measurement(aswr_su_t2020, animal_status__welfare_reading, suppression_requirement, 2020, 0.6).
narrative_ontology:measurement(aswr_su_t2026, animal_status__welfare_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'animal status / animal welfare' decomposes into three structurally distinct constraints — one per reading of the animal_status kernel. Each reading fixes a different victim set over the same referent arrangement (the standing instrumental-use settlement), so each carries its own epsilon and classification: the welfare reading (this file) confines victims to gratuitously harmed animals via the exemption structures; the abolitionist reading universalizes the victim set; the property reading empties it. The family links preserve comparability without averaging. Upstream/downstream structure: the welfare reading's institutionalization modifies the operating environment of the property reading (statutes layered on ownership) without displacing it, while remaining a live rival to the abolitionist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
