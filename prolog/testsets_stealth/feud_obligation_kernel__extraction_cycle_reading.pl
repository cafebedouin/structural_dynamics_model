% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Obligation Cycle (Extraction-Cycle Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This story instantiates the extraction_cycle_reading of the feud
 *   obligation kernel: the standing arrangement under assessment is the kin
 *   obligation to retaliate and compound, and epsilon is authored for that
 *   arrangement as this reading sees it — not for the pacified order this
 *   reading would endorse. On this reading the feud obligation system
 *   operates as a destructive cycle: kin groups surrender fighters,
 *   livestock, and pledge-land to retaliation campaigns; dependent
 *   cultivators absorb the raided production; and the resulting disorder is
 *   precisely what legitimates royal pacification, whose courts then monetize
 *   every settlement they host. The claim/metric independence rule applies:
 *   the claimed type (tangled_rope) is what this seat believes structurally
 *   true — the feud carries a real deterrence-and-settlement function AND
 *   asymmetric extraction through the same rules — while the metrics describe
 *   the arrangement's actual operation as the historiographic record shows
 *   it. Sibling readings (stateless_coordination_reading,
 *   christianized_pacification_reading) are separate constraints in separate
 *   files; they enter this story only through network edges, reading
 *   relations, and omega variables. KEY AGENTS (by structural relationship):
 *   - feud_bound_kin_groups: Primary target (organized/identity_locked) —
 *   bears mortality, depleted herds, and wergild levies -
 *   peasant_producer_households: Secondary target (powerless/trapped) — bears
 *   the raided production that funds the cycle - royal_fiscal_authority:
 *   Primary beneficiary (institutional/arbitrage) — collects pacification
 *   revenue and legitimation - seigneurial_court_holders: Secondary
 *   beneficiary (powerful/mobile) — monetizes feud adjudication locally -
 *   kin_council_elders: Agenda-setting intermediary
 *   (moderate/identity_locked) — administers the obligation and dies in it -
 *   comparative_legal_anthropologists: Analytical observer — sees the full
 *   cross-societal structure
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.81).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.75).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Obligation Cycle (Extraction-Cycle Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, '215adf8b-3e54-4ce8-8d37-3a89ec44d17f').
narrative_ontology:cs_kernel_codification('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', distributed).
narrative_ontology:cs_authority_grounding('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', practice).
narrative_ontology:cs_interpretation_layer_present('215adf8b-3e54-4ce8-8d37-3a89ec44d17f').
narrative_ontology:cs_reading_relation('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', foundational, net_productive_destruction_exceeds_deterrence_value).
narrative_ontology:cs_axiom_status(net_productive_destruction_exceeds_deterrence_value, holdable).
narrative_ontology:cs_axiom_grounding('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', net_productive_destruction_exceeds_deterrence_value, empirically_contingent).
narrative_ontology:cs_axiom('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', secondary, territorial_consolidation_requires_suppression_of_kin_violence).
narrative_ontology:cs_axiom_status(territorial_consolidation_requires_suppression_of_kin_violence, holdable).
narrative_ontology:cs_axiom_grounding('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', territorial_consolidation_requires_suppression_of_kin_violence, instrumental).
narrative_ontology:cs_reference_frame('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', kin_obligation_extraction_regime).
narrative_ontology:cs_drift_state('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', high_medieval_judicialization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('215adf8b-3e54-4ce8-8d37-3a89ec44d17f', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_fiscal_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, seigneurial_court_holders).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_bound_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, peasant_producer_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, feud_bound_kin_groups).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__extraction_cycle_reading, violence_monopoly_legitimation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Extended kin corporations bound by oath and custom to answer injuries to members with retaliatory force until satisfaction or composition. When their members are slain they receive wergild; when their members kill they owe it. Between collections they supply fighters, livestock, and pledge-land, and they cannot decline a call to vengeance without forfeiting standing, marriage alliances, and oath-helper support.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_bound_kin_groups, payer,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, feud_bound_kin_groups, beneficiary).

% Work the land inside raiding range. Their harvests, livestock, and buildings are the standing targets of reprisal, and their lords levy them to fund wergild pools and retinues. They have no standing in the settlement negotiations that dispose of their production.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peasant_producer_households, payer,
    powerless, biographical, trapped, regional).

% Claims and sells the peace: courts, fines, amercements, reliefs on compositions, and taxes justified as the price of containing retaliatory violence. Its jurisdiction expands wherever feud persists, and its statutes and chronicles portray kin vengeance as disorder requiring royal remedy. It can withdraw from any locality whose costs exceed its take.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_fiscal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, royal_fiscal_authority, agenda_setter).

% Hold local courts where feud disputes are heard and compounded; take fees, fines, and shares of wergild for hosting settlement. Their income tracks the volume of feud business reaching their benches, and they shift allegiance between princely patrons as jurisdictional profits move.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, seigneurial_court_holders, beneficiary,
    powerful, generational, mobile, regional).

% Lead the kin council: declare when vengeance is owed, muster contributions to wergild and raiding parties, and negotiate compositions with opposing kin. They enforce participation through honor sanctions and collective liability, and they bury their own dead in the same cycles they administer.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kin_council_elders, agenda_setter,
    moderate, generational, identity_locked, regional).

% Compare feud systems across stateless and state-forming societies (Nuer mediation, Icelandic saga settlement, Albanian highland codes, Somali diya groups) and reconstruct the balance of deterrence benefit against productive loss from chronicles, court rolls, and law codes.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, comparative_legal_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_fiscal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In regions without capable public enforcement, the obligation structure made injury costly and predictable: a known schedule of who owed retaliation, against whom, for how long, and on what terms it could be compounded. Deterrence of predation and a path from killing back to settlement were both carried by the same rules.
% TRANSFER_FUNCTION: Moves lives, labor, livestock, and pledge-land from feud-bound kin groups and their dependents into retaliatory campaigns and wergild payments; moves fees, fines, amercements, and relief shares from every settled composition to the courts that host them; and moves taxes and jurisdictional deference to royal authority on the strength of the disorder it undertakes to contain.
% ABSENT_VOICES: Dependent cultivators whose production funds and feeds the cycle sit outside every negotiation; women transferred in marriage to seal settlements had no voice in the terms they embodied; younger kinsmen conscripted into vengeance could not refuse without dishonor; and would-be settlers whose composition offers were refused as cowardice left no record except the resumed raids.
% DISAPPEARANCE_RATIONALE: If the obligation structure vanished overnight, kin groups would lose their security guarantee and their liability web simultaneously: predation would spike until new protective associations formed, court fee income would collapse until jurisdictions reinvented themselves around other disputes, and royal pacification claims would lose their chief exhibit. Marriage-alliance politics, honor hierarchies, and the settlement economy would all reorganize within a generation.
% FOUNDING_PROBLEM: How to make violence accountable where no state exists: ensuring that injuries have consequences, that killers face predictable response, and that a path from vengeance back to peaceful exchange remains open.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: the comparative ethnographic record (leopard-skin chief mediation among the Nuer, saga-era Icelandic settlement, Kanun besa institutions) corroborates that the accountability problem was real and was addressed by these rules where states were absent; royal pipe rolls and seigneurial court archives corroborate the fiscalization phase; conciliar acta of the peace assemblies corroborate that contemporaries outside the kin system experienced the cycle as ruinous. No source settles the status question outright, because the founding problem died unevenly — dead where royal courts matured, live where they never arrived.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.81, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.81 at interval end) because the cycle's costs are paid in the least substitutable currencies — lives and productive capacity — while the settlement layer skims every composition it hosts. Suppression (0.75) is structural first: collective liability, oath-helper networks, and the economic impossibility of leaving a kin corporation bind participants; royal criminalization of private war later added external suppression, which is why the suppression_requirement series is authored on the shared grid — the story specifically tracks enforcement-machinery buildup (peace assemblies, then royal courts hardening against kin enforcement across the interval). Theater rises from 0.16 to 0.38 as feud practice ritualizes: formal defiance, staged challenges, and truce ceremonies retain legal-notice function but increasingly stage honor for audiences while substantive settlement migrates into courtrooms. Accessibility_collapse sits at 0.58 because exits existed (composition, arbitration, sanctuary, emigration) but honor framing collapsed their unilateral usability — declining a call to vengeance meant social death even where the physical exit was open. Resistance at 0.55 reflects chronic settlement-seeking, petitioning for peace edicts, and flight. All three series run on one shared nine-point grid (900–1300 at fifty-year steps) so every metric is authored at every examined time point. Individual feuds oscillate (raid, reprisal, truce, breach) below this grid's resolution; the aggregate series resolves the secular trend, and the truce-breaching pattern functions as intermittent reinforcement — the pause resets the ledger and re-arms the obligation rather than ending it.
 *
 * PERSPECTIVAL GAP:
 *   From the royal and seigneurial seats the arrangement presents as a revenue-bearing disorder to be managed — each extension of jurisdiction is experienced locally as reform. From inside the kin groups the same rules present as inescapable duty: the ledger of deaths owed and owed-to-you is the group's memory, and declining payment is social death. The peasant seat experiences neither duty nor revenue, only the raiding calendar and the levies that follow it. Same structure, three incompatible phenomenologies; the engine computes the divergence from the power, exit, and directional data rather than from this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The declarations map directly onto the seats: feud_bound_kin_groups and peasant_producer_households are declared victims and derive high directionality toward the full-target end, amplified by identity_locked and trapped exits respectively. royal_fiscal_authority and seigneurial_court_holders are declared beneficiaries with arbitrage and mobile exits, damping their effective burden toward the subsidized end. kin_council_elders receive an explicit directionality override (d = 0.45 at the moderate power atom): the derivation chain has no beneficiary/victim entry for them, yet they are simultaneously the obligation's administrators and among its casualties — near-symmetric is the honest structural reading, and no other stakeholder occupies the moderate atom, so the override collides with nothing. Scope amplification applies modestly: the cycle operated at regional scale under kingdom-wide legitimation, so verification of settlements was hard enough to let extraction accumulate.
 *
 * MANDATROPHY ANALYSIS:
 *   Two mislabelings are live risks and the classification guards both. Reading the feud as pure coordination (the pull of the stateless sibling reading) hides the asymmetric ledger: the same rules that deliver deterrence drain specific households and enrich the jurisdictions that host settlement. Reading it as pure predation (the snare temptation) hides the delivered service: in genuinely stateless zones the deterrent and settlement functions were real and valued by the very groups that paid for them. The tangled_rope claim keeps both faces visible. On obsolescence: the founding problem's status is contested, not dead — the mandate expired where royal courts matured and stayed live where state capacity never arrived, so the arrangement is neither uniformly vestigial nor uniformly necessary. The rising theater_ratio alongside persistently high extraction marks the fiscalization phase: the form persists while its content converts from blood to fees, which is drift data, not yet a piton verdict — the cost asymmetry test fails for piton because the royal seat demonstrably profits from maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_feud_kernel,
    'This story instantiates only the extraction_cycle_reading of feud_obligation_kernel; which structural facts change if a sibling reading is adopted instead?',
    'Compile the sibling stories (stateless_coordination_reading, christianized_pacification_reading) and compare victim/beneficiary sets, epsilon, and computed types side by side against this file.',
    'Under the stateless reading, kin groups leave the victim set (they become coordinated parties) and epsilon drops toward coordination cost; under the christianized reading, violation relocates to divine law and the beneficiary structure shifts toward ecclesiastical authority. The classification computed here is indexical to this reading, not to the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_feud_kernel, conceptual, 'Reading-indexicality of the feud kernel: this file''s verdict holds for the extraction-cycle reading only.').

omega_variable(
    counterfactual_order_baseline,
    'The destructiveness verdict presupposes a feasible counterfactual of consolidated public order; where no such counterfactual was geographically or demographically available, is the measured depletion the price of any order rather than excess?',
    'Cross-regional comparison of areas with identical obligation norms but differing feasibility of state consolidation (terrain, demography, distance from fiscal cores).',
    'Where no alternative order was feasible, part of the measured extraction is irreducible coordination cost, pulling the classification toward the rope side for those regions and lowering attributable epsilon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_order_baseline, conceptual, 'Whether the reading''s counterfactual baseline (consolidated territory under public order) was actually available to the populations it judges.').

omega_variable(
    fiscal_mutation_vs_net_reduction,
    'Did royal pacification reduce total extraction from the kin economy, or convert blood-cost into fiscal-cost at roughly constant magnitude?',
    'Household-level burden reconstruction from manorial and court records comparing raid-plus-wergild losses before and after court expansion in matched regions.',
    'If conversion rather than reduction, the pacification layer continues the extraction under new administration, supporting the high terminal epsilon and a snare-leaning drift in the late interval; if genuine reduction, the royal seat''s beneficiary position is transitional rather than entrenched.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_mutation_vs_net_reduction, empirical, 'Whether the fiscalization phase mutated or reduced the cycle''s total take.').

omega_variable(
    suppression_internalization_split,
    'Is exit-suppression in the kin obligation structural (collective liability, oath networks, economic dependence) or internalized (honor identity making non-retaliation unthinkable)?',
    'Post-exit trajectories of individuals and splinter groups removed from kin-obligation contexts through exile, resettlement, or urban migration: if vengeance obligation persists after structural removal, the internalized share is substantial.',
    'An internalized share raises effective suppression beyond the structural measure and predicts slower decay of the arrangement than enforcement data alone suggests; a purely structural reading predicts rapid collapse once liability webs break.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural versus internalized mechanism carrying the obligation''s coercive force.').

omega_variable(
    demographic_pressure_confound,
    'Is the cycle''s destructiveness intrinsic to the obligation structure, or driven by land scarcity and elite surplus competition that would have produced comparable violence under any dispute regime?',
    'Natural experiments: regions sharing the obligation norms under sharply different land-labor ratios and elite population densities, holding enforcement environment constant.',
    'If confounded, the epsilon attributable to the obligation itself falls substantially and the reading''s policy implication (suppress the obligation) loses force against the alternative (manage the pressures).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_pressure_confound, empirical, 'Demographic and economic confounds behind the measured depletion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 900, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_extraction_cycle_tr_t900, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 900, 0.16).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t900, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t950, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 950, 0.19).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t950, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t1000, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1000, 0.22).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t1000, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t1050, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1050, 0.25).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t1050, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t1100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1100, 0.28).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t1100, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t1150, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1150, 0.31).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t1150, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t1200, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1200, 0.33).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t1200, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t1250, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1250, 0.36).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t1250, observed).
narrative_ontology:measurement(feud_extraction_cycle_tr_t1300, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 1300, 0.38).
narrative_ontology:measurement_basis(feud_extraction_cycle_tr_t1300, observed).

% Extraction over time
narrative_ontology:measurement(feud_extraction_cycle_be_t900, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 900, 0.62).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t900, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t950, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 950, 0.66).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t950, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t1000, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1000, 0.7).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t1000, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t1050, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1050, 0.73).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t1050, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t1100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1100, 0.75).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t1100, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t1150, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1150, 0.77).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t1150, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t1200, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1200, 0.79).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t1200, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t1250, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1250, 0.8).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t1250, observed).
narrative_ontology:measurement(feud_extraction_cycle_be_t1300, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 1300, 0.81).
narrative_ontology:measurement_basis(feud_extraction_cycle_be_t1300, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_extraction_cycle_su_t900, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 900, 0.3).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t900, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t950, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 950, 0.34).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t950, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t1000, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1000, 0.4).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t1000, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t1050, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1050, 0.47).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t1050, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t1100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1100, 0.54).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t1100, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t1150, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1150, 0.6).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t1150, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t1200, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t1200, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t1250, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1250, 0.7).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t1250, observed).
narrative_ontology:measurement(feud_extraction_cycle_su_t1300, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 1300, 0.75).
narrative_ontology:measurement_basis(feud_extraction_cycle_su_t1300, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'blood feud' per the epsilon-invariance principle. The label covers at least three structurally distinct claims, authored as separate stories: this file (extraction_cycle_reading) authors high epsilon over the feud obligation arrangement with kin groups as victims and royal/seigneurial seats as beneficiaries; the stateless_coordination_reading authors near-floor epsilon over the same practices read as self-enforcing justice; the christianized_pacification_reading relocates evaluation to divine law with ecclesiastical authority. Dependency structure: the stateless reading supplies the mechanism account this reading presupposes for its coordination half (upstream influences downstream); this reading's fiscal diagnosis supplied the material interest that amplified the christianized campaign's princely uptake. Every family member links the others via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__extraction_cycle_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
