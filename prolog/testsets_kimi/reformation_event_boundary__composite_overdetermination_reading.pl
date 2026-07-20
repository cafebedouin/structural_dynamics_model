% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Reformation as Composite Overdetermined Event
 *   domain: historical/epistemological/religious
 *
 * SUMMARY:
 *   The Reformation event boundary is a contested kernel in historical
 *   epistemology. This constraint story instantiates the
 *   composite_overdetermination_reading: the claim that the Reformation was
 *   irreducibly overdetermined by theological, political, institutional, and
 *   denominational factors occurring simultaneously, such that no single
 *   causal narrative or periodization scheme is adequate. As a
 *   historiographical constraint, this reading operates on academic practice
 *   by mandating methodological pluralism and resisting monocausal
 *   explanation. It is one reading of the reformation_event_boundary kernel;
 *   sibling readings (theological_climb_reading, political_swap_reading) are
 *   modeled as separate constraints. The constraint coordinates multiple
 *   historiographical subfields while asymmetrically marginalizing
 *   confessional, materialist-reductionist, and public-communication
 *   approaches.
 *
 * KEY AGENTS:
 *   - secular_academy: Agenda-setter (institutional/arbitrage/global) â enforces peer review norms that treat monocausal Reformation historiography as naive
 *   - confessional_church_historians: Primary payer (moderate/identity_locked/national) â lose theological primacy and mainstream academic standing
 *   - political_reductionist_historians: Secondary payer (moderate/constrained/national) â forced to dilute materialist explanations
 *   - multidisciplinary_research_institutes: Beneficiary (institutional/arbitrage/continental) â justified by irreducible complexity claims
 *   - public_history_educators: Tertiary payer (moderate/constrained/national) â bear communicative labor costs
 *   - historiography_observers: Analytical observer (analytical/analytical/global) â tracks disciplinary framing shifts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.52).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.66).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Reformation as Composite Overdetermined Event").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical/epistemological/religious").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '2acb92c8-5b56-4c52-b348-f4ad64478803').
narrative_ontology:cs_kernel_codification('2acb92c8-5b56-4c52-b348-f4ad64478803', distributed).
narrative_ontology:cs_authority_grounding('2acb92c8-5b56-4c52-b348-f4ad64478803', expertise).
narrative_ontology:cs_interpretation_layer_present('2acb92c8-5b56-4c52-b348-f4ad64478803').
narrative_ontology:cs_reading_relation('2acb92c8-5b56-4c52-b348-f4ad64478803', reformation_event_boundary__theological_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('2acb92c8-5b56-4c52-b348-f4ad64478803', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_axiom('2acb92c8-5b56-4c52-b348-f4ad64478803', foundational, historical_phenomena_resist_monocausal_reduction).
narrative_ontology:cs_axiom_status(historical_phenomena_resist_monocausal_reduction, holdable).
narrative_ontology:cs_axiom_grounding('2acb92c8-5b56-4c52-b348-f4ad64478803', historical_phenomena_resist_monocausal_reduction, empirically_contingent).
narrative_ontology:cs_reference_frame('2acb92c8-5b56-4c52-b348-f4ad64478803', multicausal_historiographical_synthesis).
narrative_ontology:cs_drift_state('2acb92c8-5b56-4c52-b348-f4ad64478803', contemporary_academic_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('2acb92c8-5b56-4c52-b348-f4ad64478803', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, secular_academy).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, multidisciplinary_research_institutes).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, confessional_church_historians).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, political_reductionist_historians).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, public_history_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets peer review standards, tenure criteria, and curriculum guidelines for early modern history. Treats monocausal Reformation narratives as methodologically obsolete. Benefits from sustained research complexity because it justifies professional differentiation, specialized journals, and conference circuits.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, secular_academy, agenda_setter,
    institutional, generational, arbitrage, global).

% Scholars in seminaries and confessional colleges for whom the Reformation is primarily a work of divine providence through doctrinal recovery. The composite frame makes theological causation one variable among many, stripping it of primacy. Their institutional home insulates them partially, but mainstream academic prestige and publication access are reduced.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_church_historians, payer,
    moderate, generational, identity_locked, national).

% Historians emphasizing princely politics, estates, and material interests. The composite frame forces them to integrate theological and social history they view as secondary, diluting their explanatory models and making their work vulnerable to peer review critique for reductionism.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, political_reductionist_historians, payer,
    moderate, biographical, constrained, national).

% Research centers explicitly funding Reformation Studies as an interdisciplinary nexus. The irreducible complexity claim directly justifies their budgets, staffing, and grant applications by making single-disciplinary approaches appear insufficient.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, multidisciplinary_research_institutes, beneficiary,
    institutional, generational, arbitrage, continental).

% Museum professionals, textbook authors, and secondary educators who must communicate the Reformation to non-specialist audiences. The composite frame raises preparation costs, prevents clean narrative arcs, and exposes them to criticism from both academic reviewers for oversimplifying and audiences for obscurity.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, public_history_educators, payer,
    moderate, biographical, constrained, national).

% Scholars of historiography and historical epistemology who track how the discipline's framing of the Reformation shifts across decades. They observe the enforcement of complexity norms without being bound to any single causal narrative.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historiography_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological, political, social, and institutional historians by providing a framework in which all causal registers are treated as equally primordial, preventing the field from collapsing into confessional or materialist warfare.
% TRANSFER_FUNCTION: Moves epistemic authority and research funding from monocausal explanatory traditions to multidisciplinary institutions and complexity-embracing scholars; moves pedagogical and communicative labor costs onto public-facing educators.
% ABSENT_VOICES: Devotional communities for whom the Reformation remains a spiritual event, and popular audiences seeking usable pasts, are structurally absent from peer review and curriculum design; their preference for narrative clarity is recorded as methodological naivety rather than legitimate interest.
% DISAPPEARANCE_RATIONALE: If the composite frame vanished, Reformation historiography would reorganize around competing single-cause schools, curricula would simplify, public history costs would drop, and the current interdisciplinary research infrastructure would lose its primary justification. The discipline would revert to earlier confessional-materialist contestation.
% FOUNDING_PROBLEM: Nineteenth- and early-twentieth-century Reformation historiography was mired in confessional polemic and later in Marxist-vs-idealist warfare, preventing scholarly cooperation across religious and methodological lines.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary historiography textbooks and methodological treatises from within the secular academy attest the founding problem is solved. However, confessional historians and political materialists outside the benefiting parties attest that the composite frame simply replaced old polemics with a new orthodoxy that suppresses their causal claims. The corroboration is split: beneficiary parties say the problem is solved, while excluded parties say it has mutated.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52) is moderate: the composite frame imposes genuine labor costs on educators and suppresses clean explanatory models, but also produces scholarship that is more capacious than its predecessors. Suppression (0.66) is higher because the constraint's persistence depends on active peer review enforcement against reductionism; alternatives (monocausal textbooks, confessional histories) are systematically delegitimized. Theater_ratio (0.50) reflects significant performative complexity â much scholarly output signals methodological sophistication through excessive causal proliferation without proportional explanatory gain. Accessibility_collapse (0.70) is high because once the composite frame is accepted, simple periodization and causal hierarchy become nearly unintelligible within the discipline. Resistance (0.45) is moderate: confessional historians maintain parallel institutions, political materialists persist in some national historiographies, and public educators quietly simplify.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the composite frame is a hard-won methodological peace that ended confessional warfare. From the confessional historian seat, it is a secular capture that demotes divine action to one variable among many. From the public educator seat, it is a professional burden that privileges academic prestige over public understanding. The engine computes these divergences from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular academy and multidisciplinary institutes sit near the beneficiary end (low d): they set the norms, capture research funding, and professionalize complexity. Confessional and political historians sit near the target end (high d): their preferred explanatory models are the ones suppressed by the composite frame. Public educators sit mid-high (d ~0.65): they are not the primary ideological targets but bear structural costs with limited exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â confessional and materialist polemic preventing scholarly cooperation â is contested in status. If dead, the constraint persists as a zombie framework: it still coordinates, but the intensity of its enforcement exceeds what the original problem warrants. The R5 mismatch (contested founding problem + world_rearranges disappearance) prevents automatic piton classification because real coordination continues: theological, political, and social historians do publish in shared venues under composite auspices. The classification as tangled_rope captures this genuine coordination function alongside asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_complexity_vs_theater,
    'Does the composite overdetermination frame represent genuine methodological progress, or has it become a theater of sophistication that raises barrier to entry without improving explanatory power?',
    'Comparison of predictive or retrodictive success between composite-framed and focused-framed Reformation scholarship; citation analysis of whether composite works are engaged for substance or for positional affiliation.',
    'If primarily theater, the constraint''s theater_ratio should be revised upward and it may compute as piton; if genuine methodological necessity, the current extraction metrics are justified coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_complexity_vs_theater, conceptual, 'Whether composite framing is substantive or performative.').

omega_variable(
    disciplinary_extraction_boundary,
    'Is the labor cost imposed on public historians and educators by the composite frame a necessary price of accuracy, or an extracted rent that sustains professional differentiation?',
    'Pedagogical outcome studies comparing student comprehension and retention under composite versus streamlined narratives; employment and salary data for public historians relative to academic specialists.',
    'If educators bear costs without commensurate accuracy gains, the constraint is more extractive than coordinated; if accuracy demonstrably improves, the extraction is necessary coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disciplinary_extraction_boundary, empirical, 'Whether educator labor costs are necessary or extractive.').

omega_variable(
    kernel_reading_incommensurability,
    'This constraint instantiates the composite_overdetermination_reading of kernel reformation_event_boundary. How would classification change if the political_swap_reading or theological_climb_reading were adopted instead?',
    'Generate parallel constraint stories for sibling readings and compare their beneficiary/victim structures, epsilon values, and coordination types.',
    'Adopting a sibling reading would shift the beneficiary set to secular rulers (political) or confessional churches (theological), reverse the directionality for currently excluded groups, and likely lower theater_ratio while raising suppression for the marginalized reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Structural delta between this reading and sibling readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(refo_tr_t10, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(refo_tr_t20, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(refo_tr_t30, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(refo_tr_t40, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(refo_tr_t50, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(refo_be_t10, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(refo_be_t20, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(refo_be_t30, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(refo_be_t40, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(refo_be_t50, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(refo_su_t10, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(refo_su_t20, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(refo_su_t30, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(refo_su_t40, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(refo_su_t50, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 50, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, political_swap_reading).

% DUAL FORMULATION NOTE:
% The kernel reformation_event_boundary decomposes into at least three structurally distinct constraints: composite_overdetermination_reading (this file), theological_climb_reading, and political_swap_reading. Each reading carries a different epsilon, beneficiary/victim structure, and causal priority claim. They are linked as a constraint family because they share the same historical referent but instantiate incompatible explanatory frameworks. The composite reading treats causal irreducibility as constitutive; the siblings treat single-cause primacy as constitutive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
