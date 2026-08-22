% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__popular_sovereignty_reading, []).

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
 *   constraint_id: secession_legitimacy_boundary__popular_sovereignty_reading
 *   human_readable: Secession Legitimacy Boundary — Popular Sovereignty Reading
 *   domain: political economy/federalism/resource politics
 *
 * SUMMARY:
 *   This story instantiates the popular_sovereignty_reading of the
 *   secession_legitimacy_boundary kernel: the claim that ultimate sovereignty
 *   resides in the democratic majority within provincial boundaries and that
 *   a referendum result is self-legitimating. Per the kernel-reading
 *   epsilon-referent rule, the constraint under measurement is the STANDING
 *   arrangement — the federal constitutional order under which unilateral
 *   provincial exit is impermissible and secession is conditioned on
 *   federally influenced negotiation plus constitutional amendment — assessed
 *   by this reading's own lights. The reading's endorsed alternative
 *   (majority-referendum exit) is NOT the referent and does not enter
 *   epsilon. From this seat the standing arrangement is a snare: a genuine
 *   coordination story (continental fiscal union, common debt, defense,
 *   internal market) covers a structure whose persistence depends on actively
 *   foreclosing the provincial majority's exit while resource revenues and
 *   fiscal contributions flow outward to seats that did not earn them. The
 *   claim and the metrics are independent authored facts: claimed_type states
 *   what this reading holds structurally true of the standing arrangement;
 *   the metrics describe that arrangement as this reading descriptively sees
 *   it; the engine computes per-seat classifications from the structural
 *   data, and any divergence between the claim and computed types is the
 *   measurement the corpus exists to take. Sibling readings of the same
 *   kernel (constitutional_impossibility_reading,
 *   grievance_threshold_reading, treaty_primacy_reading) are separate
 *   constraints with their own epsilon, victim sets, and types; this file
 *   links to them via network.affects_constraints as one constraint family.
 *
 * KEY AGENTS:
 *   - federal_government: agenda_setter (institutional/arbitrage) — administers the boundary through the amendment process, reference litigation, and fiscal leverage; receives the fiscal and resource flows the boundary secures
 *   - supreme_constitutional_court: agenda_setter (institutional/identity_locked) — co-authors the boundary's legal content; its authority is constituted by the constitutional order it enforces
 *   - secessionist_provincial_majority: payer (organized/trapped) — the majority whose referendum result the standing order does not treat as sufficient; bears denial of collective self-determination
 *   - provincial_resource_revenue_payers: payer (moderate/constrained) — provincial taxpayers and resource rights holders whose revenues are pooled federation-wide
 *   - provincial_federalist_minority: beneficiary (moderate/mobile) — the provincial minority the boundary keeps in the union; individually the most mobile party inside the province
 *   - transfer_receiving_provinces: beneficiary (organized/constrained) — collect transfers sustained by the pooling arrangement the boundary secures
 *   - indigenous_nations: excluded (organized/trapped) — treaty holders whose consent neither the referendum franchise nor the amendment table requires
 *   - international_recognition_bodies: observer (institutional/analytical) — arbiters of whether any exit route would be recognized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, 0.78).
domain_priors:suppression_score(secession_legitimacy_boundary__popular_sovereignty_reading, 0.85).
domain_priors:theater_ratio(secession_legitimacy_boundary__popular_sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__popular_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__popular_sovereignty_reading, snare).
narrative_ontology:human_readable(secession_legitimacy_boundary__popular_sovereignty_reading, "Secession Legitimacy Boundary — Popular Sovereignty Reading").
narrative_ontology:topic_domain(secession_legitimacy_boundary__popular_sovereignty_reading, "political economy/federalism/resource politics").

domain_priors:requires_active_enforcement(secession_legitimacy_boundary__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__popular_sovereignty_reading, '2ac417c8-f9b4-4ea6-a076-b82138e6f309').
narrative_ontology:cs_kernel_codification('2ac417c8-f9b4-4ea6-a076-b82138e6f309', fixed_text).
narrative_ontology:cs_authority_grounding('2ac417c8-f9b4-4ea6-a076-b82138e6f309', lineage).
narrative_ontology:cs_interpretation_layer_present('2ac417c8-f9b4-4ea6-a076-b82138e6f309').
narrative_ontology:cs_reading_relation('2ac417c8-f9b4-4ea6-a076-b82138e6f309', secession_legitimacy_boundary__constitutional_impossibility_reading, forecloses).
narrative_ontology:cs_reading_relation('2ac417c8-f9b4-4ea6-a076-b82138e6f309', secession_legitimacy_boundary__grievance_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ac417c8-f9b4-4ea6-a076-b82138e6f309', secession_legitimacy_boundary__treaty_primacy_reading, forecloses).
narrative_ontology:cs_axiom('2ac417c8-f9b4-4ea6-a076-b82138e6f309', foundational, provincial_majority_sovereignty_is_ultimate).
narrative_ontology:cs_axiom_status(provincial_majority_sovereignty_is_ultimate, holdable).
narrative_ontology:cs_axiom_grounding('2ac417c8-f9b4-4ea6-a076-b82138e6f309', provincial_majority_sovereignty_is_ultimate, deontological).
narrative_ontology:cs_axiom('2ac417c8-f9b4-4ea6-a076-b82138e6f309', foundational, referendum_result_self_legitimating).
narrative_ontology:cs_axiom_status(referendum_result_self_legitimating, holdable).
narrative_ontology:cs_axiom_grounding('2ac417c8-f9b4-4ea6-a076-b82138e6f309', referendum_result_self_legitimating, conventional).
narrative_ontology:cs_reference_frame('2ac417c8-f9b4-4ea6-a076-b82138e6f309', provincial_demos_supremacy).
narrative_ontology:cs_drift_state('2ac417c8-f9b4-4ea6-a076-b82138e6f309', contemporary_constitutional_order, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('2ac417c8-f9b4-4ea6-a076-b82138e6f309', '').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, transfer_receiving_provinces).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_federalist_minority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_majority).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_resource_revenue_payers).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__popular_sovereignty_reading, negotiated_exit_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the terms under which any provincial unit could leave: it controls the constitutional amendment process, refers secession questions to the courts, and holds spending and transfer leverage over provincial budgets. It receives the provincial resource revenue shares and tax contributions that flow through the federation and justifies the arrangement as the price of a common economic and security space. Its position at the center means the rules bind others far more than they bind it.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicates what the constitution permits regarding provincial exit and authored the doctrine that unilateral exit is impermissible while a clear referendum result triggers a negotiation duty. Its authority rests on the constitutional order it interprets; repudiating that order would dissolve the source of its own standing. It can evolve doctrine only through slow, case-by-case movement.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, supreme_constitutional_court, agenda_setter,
    institutional, generational, identity_locked, national).

% A recurring electoral majority within the province that has voted, in whole or in part, for a mandate to leave the federation. The standing order does not treat its referendum result as sufficient: exit requires negotiations the federal side controls and constitutional amendments the federal side can block. Its members cannot individually exit the province's collective situation, and the question recurs on a biographical timescale — each generation votes again on the same blocked door.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, secessionist_provincial_majority, payer,
    organized, biographical, trapped, regional).

% Provincial taxpayers and holders of resource rights whose royalties and tax base are pooled federation-wide through equalization and federal taxation. Individual members with portable capital or skills can relocate to other provinces, but the resource base itself cannot move, and the pooling terms are set outside the province.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_resource_revenue_payers, payer,
    moderate, biographical, constrained, regional).

% The minority within the province that opposes leaving and whose preferred outcome — continued membership — is what the standing order delivers. If the province ever did exit, its members could relocate internally within the federation more easily than the majority can exit the federation; individually they are the most mobile party inside the province.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, provincial_federalist_minority, beneficiary,
    moderate, biographical, mobile, regional).

% Provinces that receive net fiscal transfers sustained by the pooling arrangement the boundary secures. They lobby to preserve the current terms, have no seat in the secession question beyond their votes in federal institutions, and their budget planning depends on the flows continuing.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, transfer_receiving_provinces, beneficiary,
    organized, biographical, constrained, regional).

% Nations holding treaties that predate both federal and provincial authority, with territories inside the province. Neither the referendum franchise nor the constitutional amendment table requires their consent, yet any change in the province's status would pass over their territories and treaty relationship. They organize politically but cannot remove their territories from the question.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, indigenous_nations, excluded,
    organized, generational, trapped, regional).

% Foreign states and international organizations whose recognition any new state needs in order to function. They take positions after any referendum or negotiation, weigh analogues within their own territories, and their practice on effective control, negotiated settlements, and minority protections shapes the expected payoff of every exit route.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__popular_sovereignty_reading, international_recognition_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__popular_sovereignty_reading, federal_government).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__popular_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The standing boundary solves a real collective problem: it stabilizes a continental fiscal union — single currency, common debt, common defense, internal market, inter-regional transfers — by making provincial membership non-unilaterally revocable, preventing cascading territorial fragmentation and holding one bargaining table for resource revenue pooling.
% TRANSFER_FUNCTION: Moves resource revenue shares, tax capacity, and borrowing terms from the resource-rich province to the federal center and the transfer-receiving provinces; moves decision authority over the province's continued membership from the provincial majority to the federal constitutional order and its courts.
% ABSENT_VOICES: Indigenous nations holding treaties that predate both orders would object that neither the referendum franchise nor the constitutional amendment table includes their consent; they sit outside both. The provincial minority opposing exit would object to being transferred by a majority vote it cast against; it is present in the province but outvoted rather than absent. International recognition bodies are consulted after the fact rather than before.
% DISAPPEARANCE_RATIONALE: If the boundary vanished overnight — a bare provincial majority referendum sufficing for legitimate exit — the federation would face an immediate secession claim, emergency renegotiation of currency, debt assumption, trade corridors, and border regimes, contested recognition abroad, and a repricing of every inter-regional transfer; the fiscal union the boundary secures would not survive in its current form.
% FOUNDING_PROBLEM: The boundary was built to solve the founding problem of a geographically vast, regionally divergent polity: how to secure a common economic and security space and enable inter-regional redistribution that no province could achieve alone, without permitting any single region's exit to unravel the whole.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: secessionist provincial government policy papers and referendum campaign literature concede the federation delivered scale economies — currency, defense, market access — their province would have to rebuild at cost, and comparative studies of post-secession states re-applying for economic union corroborate that the coordination problem the boundary was built for is real. No corroborating source attests that the problem is dead.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__popular_sovereignty_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(secession_legitimacy_boundary__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.78 at interval end) because, on this reading's accounting, the fiscal and resource flows continue without the consent that would make them contributions, and the flows have deepened as resource revenues grew. Suppression is higher still (0.85) because the arrangement's persistence depends on legal foreclosure of exit — constitutional supremacy, reference jurisprudence, clarity requirements, fiscal leverage — rather than on participant preference; the suppression series is authored because the story specifically tracks enforcement hardening (clarity legislation and reference doctrine built up over the interval), not because extraction alone moved. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled by the engine, via directionality and spatial scope. Theater is moderate (0.45) and rising: the negotiation-obligation doctrine and partnership rhetoric perform reciprocity while the exit door stays shut, though the constitutional machinery itself is functionally real. Accessibility_collapse is 0.65 — negotiated exit, leverage politics, and recognition routes partly survive once the arrangement is understood, which is why this is not a mountain profile. Resistance is 0.7 — an organized, recurrent secessionist movement with real electoral coalition power; notably, that coalition power is constitutionally insufficient, which is precisely what the boundary channels into blocked negotiation. The three tracked metric series share one time grid: every metric is authored at every examined time point (0, 8, 16, 24, 32, 40), with end-state values matching the base_properties scalars. Coordination type is resource_allocation (floor default 0.15, no override): the dominant function whose failure would most directly cause the coordination problem is the pooling and redistribution of fiscal capacity and resource revenues across the federation; the enforcement machinery serves that allocation rather than constituting it.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very different types from identical structural data. From the federal seat the arrangement is the legitimate constitutional order it administers — rope-like from inside, with arbitrage-grade flexibility about rules that bind others. From the payer seats the same structure operates as enforced extraction with a shut exit door. The court seat is identity-fused: its institutional identity is constituted by the doctrine it enforces, so it cannot repudiate the boundary without dissolving its own authority — a cognitive-capture profile distinct from the federal seat's ordinary self-interest. The federalist minority experiences the same boundary as protection, and is the one seat whose exit options improve if the province leaves. Same nominal polity, same legal texts, divergent computed classifications — the engine derives this from power, exit, and role data, not from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (federal_government with arbitrage exit, transfer_receiving_provinces, provincial_federalist_minority with mobile exit) derive low directionality — damped or inverted effective extraction, with the arbitrage-grade federal seat nearest the beneficiary end. Victims derive high directionality: the secessionist_provincial_majority is trapped (the constraint IS the closure of its exit), placing it near the full-target end; provincial_resource_revenue_payers are constrained (individual mobility, immovable resource base), somewhat damped but still target-side, and the larger national scope amplifies verification difficulty and hence effective extraction for both. Two seats fall outside the beneficiary/victim lists and are handled structurally rather than by override: the supreme_constitutional_court collects authority from administering the boundary and pays nothing (modestly beneficiary-side, roughly d 0.3), and indigenous_nations bear the boundary's costs — their consent is bypassed by the same franchise logic — despite the reading's own accounting not counting them among victims. Per R3, the excluded seat is commentary-grade and does not drive classification; the treaty_primacy sibling reading is where that grievance becomes a constraint of its own.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — holding a divergent continental polity together — is live and corroborated from outside the benefiting parties: secessionist provincial policy papers concede the scale economies they would forfeit, and post-secession comparative experience corroborates the coordination problem. Status=live crossed with disappearance_verdict=world_rearranges produces no mismatch flag; this is not a zombie arrangement. The snare claim does the mandatrophy work here in the other direction: it prevents the genuinely live coordination function from laundering the extraction. The test is not whether coordination exists but whether identifiable parties are held in and pay through the same structure that coordinates others — and on this reading's accounting they are, with enforcement doing the holding. The classification keeps the two questions separate: the fiscal union may be worth preserving AND the boundary may still be extracting from a majority that never consented to its terms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Which reading of the secession_legitimacy_boundary kernel governs? This file instantiates only the popular_sovereignty_reading; what would each sibling reading change structurally?',
    'Constitutional adjudication, political settlement, or adoption of a negotiated exit framework would settle which reading binds; comparative international recognition practice would reveal which reading the outside world rewards.',
    'Under the constitutional_impossibility_reading the standing arrangement''s extraction is fully legitimated and this story''s snare claim collapses toward rope; under the treaty_primacy_reading the victim set expands to treaty holders and both federal and provincial authority become subordinate; under the grievance_threshold_reading legitimacy tracks federal conduct, changing the extraction referent itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'This constraint is one reading of a four-way contested kernel; sibling readings instantiate different constraints with different epsilon and victim sets.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the force holding the provincial majority in the union structural (constitutional supremacy, reference jurisprudence, fiscal leverage) or partly internalized (shared-national identity attachment that makes exit unthinkable for part of the majority)?',
    'Post-referendum and post-legal-change trajectory analysis: if exit support and secession politics persist undiminished when legal barriers are relaxed, the structural measure stands; if support decays with the barriers, a substantial share was internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure — the boundary would persist even under legal liberalization, and the majority''s exit position is more trapped than the legal record alone shows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the secession boundary.').

omega_variable(
    majority_threshold_sufficiency,
    'Does this reading''s self-legitimating referendum mean a bare majority of votes cast on any question, or does the reading itself require a clear question and supermajority — and is any such threshold a coordination necessity or a suppression device?',
    'Constitutional design analysis across comparable polities and the reading''s own doctrinal elaboration; clarity-doctrine analogues show where the reading''s adherents actually draw the line.',
    'A supermajority or clear-question requirement narrows the reading''s claim, shrinks the victim set to actual supermajorities, and lowers the extraction this reading attributes to the standing arrangement; a bare-majority reading maximizes it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_threshold_sufficiency, conceptual, 'Threshold ambiguity internal to the popular sovereignty reading''s self-legitimation claim.').

omega_variable(
    fiscal_flow_attribution,
    'How much of the provincial net fiscal contribution reflects genuine cost-sharing for common services (defense, debt service, common market administration) versus transfer exceeding any attributable service cost?',
    'Fiscal federalism accounting with per-capita expenditure attribution and counterfactual unit-cost comparison against comparable independent jurisdictions.',
    'A wide service-cost-to-transfer gap supports the high epsilon authored here; a narrow gap reclassifies much of the measured extraction as coordination cost and pulls this reading''s assessment of the standing arrangement toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_flow_attribution, empirical, 'Whether the fiscal flows the boundary secures are extraction or coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__popular_sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(sece_tr_t8, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(sece_tr_t16, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(sece_tr_t24, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(sece_tr_t32, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__popular_sovereignty_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(sece_be_t8, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(sece_be_t16, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(sece_be_t24, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(sece_be_t32, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 32, 0.74).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__popular_sovereignty_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(sece_su_t8, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(sece_su_t16, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement(sece_su_t24, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(sece_su_t32, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 32, 0.81).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__popular_sovereignty_reading, suppression_requirement, 40, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__popular_sovereignty_reading, resource_allocation).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__constitutional_impossibility_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__popular_sovereignty_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'when is secession legitimate?' decomposes into four structurally distinct readings of one kernel, each with its own epsilon, victim set, and type — per the epsilon-invariance principle these are separate constraint stories, not one story with a measurement parameter. This file is the popular_sovereignty_reading. The currently operative legal order corresponds to the constitutional_impossibility_reading, which is cited against this reading and therefore sits upstream of it in institutional influence; the treaty_primacy_reading contests the franchise from outside it; the grievance_threshold_reading shares this reading's anti-textual orientation but conditions legitimacy on federal conduct rather than majority will.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
