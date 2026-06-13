% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__graded_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__graded_sovereignty, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: westphalia_sovereignty__graded_sovereignty
 *   human_readable: Graded Sovereignty: Intervention Legitimacy as Capacity Function
 *   domain: international_law/political_theory
 *
 * SUMMARY:
 *   The graded-sovereignty reading construes Westphalian territorial
 *   authority not as a categorical binary (sovereign or not) but as a scalar
 *   quantity indexed to measurable state capacity: fiscal revenue,
 *   institutional quality, monopoly on violence, rule-of-law strength. This
 *   reading emerged in the 1990s post-Cold War security discourse and
 *   crystallized in the Responsibility to Protect doctrine and World Bank
 *   governance indices. It creates a hierarchical state system where
 *   intervention legitimacy becomes a function of capacity deficits: weaker
 *   states are subject to international assessment, conditionality,
 *   trusteeship, or military intervention justified as capacity-building. The
 *   structural delta from sibling readings is stark: absolute
 *   non-intervention treats sovereignty as categorical and inviolable
 *   regardless of internal conduct; conditional responsibility ties
 *   sovereignty status to mass-atrocity thresholds; graded sovereignty ties
 *   it to continuously measured institutional metrics, creating permanent
 *   asymmetry. This reading is one instantiation of the Westphalia kernel — a
 *   contested commitment to how territorial authority is legitimate — and the
 *   engine will compute divergent seat-level classifications because the
 *   constraint simultaneously coordinates (provides a legitimacy framework
 *   for peace operations) and extracts (creates institutional hierarchies
 *   that benefit capacity-evaluators and intervention-capable states).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, 0.68).
domain_priors:suppression_score(westphalia_sovereignty__graded_sovereignty, 0.72).
domain_priors:theater_ratio(westphalia_sovereignty__graded_sovereignty, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, extractiveness, 0.68).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__graded_sovereignty, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__graded_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__graded_sovereignty, "Graded Sovereignty: Intervention Legitimacy as Capacity Function").
narrative_ontology:topic_domain(westphalia_sovereignty__graded_sovereignty, "international_law/political_theory").

domain_priors:requires_active_enforcement(westphalia_sovereignty__graded_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__graded_sovereignty, 'bb84341b-ac8b-458b-9bcb-049aea274506').
narrative_ontology:cs_kernel_codification('bb84341b-ac8b-458b-9bcb-049aea274506', fixed_text).
narrative_ontology:cs_authority_grounding('bb84341b-ac8b-458b-9bcb-049aea274506', lineage).
narrative_ontology:cs_interpretation_layer_present('bb84341b-ac8b-458b-9bcb-049aea274506').
narrative_ontology:cs_reading_relation('bb84341b-ac8b-458b-9bcb-049aea274506', westphalia_sovereignty__absolute_non_intervention, coexists_with).
narrative_ontology:cs_reading_relation('bb84341b-ac8b-458b-9bcb-049aea274506', westphalia_sovereignty__conditional_responsibility, influences).
narrative_ontology:cs_axiom('bb84341b-ac8b-458b-9bcb-049aea274506', foundational, capacity_scalar_legitimacy).
narrative_ontology:cs_axiom_status(capacity_scalar_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bb84341b-ac8b-458b-9bcb-049aea274506', capacity_scalar_legitimacy, instrumental).
narrative_ontology:cs_axiom('bb84341b-ac8b-458b-9bcb-049aea274506', foundational, paternalistic_development_justified).
narrative_ontology:cs_axiom_status(paternalistic_development_justified, holdable).
narrative_ontology:cs_axiom_grounding('bb84341b-ac8b-458b-9bcb-049aea274506', paternalistic_development_justified, deontological).
narrative_ontology:cs_reference_frame('bb84341b-ac8b-458b-9bcb-049aea274506', westphalia_equal_sovereignty).
narrative_ontology:cs_drift_state('bb84341b-ac8b-458b-9bcb-049aea274506', contemporary_capacity_governance_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('bb84341b-ac8b-458b-9bcb-049aea274506', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, capacity_evaluating_authorities).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__graded_sovereignty, intervention_capable_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalia_sovereignty__graded_sovereignty, marginalized_populations_in_weak_states).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__graded_sovereignty, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__graded_sovereignty, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__graded_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__graded_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__graded_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 endpoint) because the constraint asymmetrically allocates sovereignty authority: capacity-evaluation is performed by strong states and multilateral bodies dominated by Western powers; weak states are the subjects of evaluation and intervention, not the evaluators. The measurement series shows steady extraction increase from t=0 (0.38) to t=60 (0.68), then plateau—this reflects the institutionalization of capacity frameworks (World Bank governance metrics, IMF conditionality, UN intervention standards) reaching maturity by the 2000s and stabilizing thereafter. Suppression is also high (0.72) because the constraint's persistence requires continuous assertion that capacity metrics are objective, that interventions serve the intervened-upon states' interests, and that alternative sovereignty frameworks (non-aligned sovereignty, African Union principles) are illegitimate or nostalgic. Theater is substantial (0.58) because significant performance maintenance occurs: development narratives frame extraction as capacity-building, trusteeship is rebranded as partnership, conditionality is presented as technical assistance. Accessibility of alternatives is limited (0.62) because weak states have few exit options—refusing international assessment and aid conditionality means losing access to capital, security guarantees, and multilateral legitimacy. Resistance is high (0.71) because the constraint faces sustained pushback: weak states and regional powers contest capacity metrics as biased, non-aligned movements oppose intervention legitimacy, and scholarship increasingly frames graded sovereignty as post-colonial extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (weak states, competing regional powers) and the beneficiary seats (capacity-evaluating authorities, intervention-capable states) compute different constraint types from the same structural facts. From the weak-state position—trapped, identity-locked (state institutions built around external conditionality), receiving conditional aid—the constraint reads as snare: the coordination story (orderly international response to fragility) is cover for extraction (sovereignty transfer). From the capacity-evaluator position—institutional power, arbitrage exit, control over metrics—it reads as tangled rope: genuine coordination problem (how to respond to failing states without pure imperialism) plus asymmetric extraction (leveraging capacity assessment to extract institutional authority and resource access). The measurement trajectory supports snare inference: theater_ratio rises from 0.35 to 0.58 (increasing proportion of enforcement activity devoted to legitimacy maintenance rather than actual capacity-building), and suppression tracks extractiveness upward. The engine will compute per-seat classifications; the authored claim of tangled_rope reflects the constraint's position before full seat divergence analysis—it is genuinely coordinating (provides a framework for managing state fragility) while genuinely extracting (allocates authority asymmetrically). The measurement data show the theater ratio crossing 0.5 at t=30, a diagnostic boundary where performative activity begins exceeding functional activity.
 *
 * DIRECTIONALITY LOGIC:
 *   Capacity-evaluating authorities (beneficiary + agenda-setter roles, institutional power, arbitrage exit) are the structural center of the constraint: they design capacity metrics, control their application, and authorize interventions. Their directionality is near the beneficiary pole (d ~ 0.1–0.25) because the constraint subsidizes their authority—it legitimizes interventions they could not justify under absolute-non-intervention norms and transfers sovereignty authority to multilateral bodies they dominate. Weak states (payer role, powerless, trapped exit) are the targets: they must accept external assessment, submit to conditionality, and endure intervention. Their directionality is near the target pole (d ~ 0.85–0.95) because the constraint extracts sovereignty authority, imposes conditions on aid access, and permits military or administrative intervention without their consent. Intervention-capable states (beneficiary + agenda-setter, institutional power, arbitrage) sit slightly below the capacity-evaluators (d ~ 0.15–0.35) because they benefit from the legitimacy framework (it permits their interventions) and from resource and geopolitical access in weak states, but they share design authority with capacity-evaluation bodies rather than monopolizing it. Competing regional powers (payer role, institutional power, constrained exit) occupy an asymmetric position (d ~ 0.55–0.75): they are constrained from using the same capacity-evaluation framework for their own regional interventions (double standard), they compete with Western powers for influence, but they cannot fully exit the system. Marginalized populations in weak states (beneficiary + payer, powerless, identity-locked) sit at d ~ 0.70–0.80: they receive some services (security, health, education) from international provision but lose political voice and self-determination, and identity-locking through displacement or economic dependence on international programs prevents exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not show classical mandatrophy (founding problem dead, arrangement persists) because the founding problem remains contested—some parties (Western humanitarian advocates, development agencies) attest it is very much live, while others (weak states, non-aligned scholars) attest it was solved by decolonization and the persistence reflects renewed extraction. The constraint shows instead a framing transition: the founding problem (need a legitimacy standard for interventions post-WWII) was real and solved in the 1950s–1970s (decolonization established sovereignty norms, UN created mechanisms for dispute resolution without intervention). But the constraint itself evolved from a coordination solution (provide a framework for emergency intervention during state collapse) into an extraction mechanism (use capacity metrics to subordinate weak states to external authority on a permanent basis). The measurement data support this reading: theater_ratio rises faster than base_extractiveness in the second half of the interval (t=30 to t=75), suggesting the constraint is increasingly maintained through performance (discourse about development, partnership, capacity-building) rather than through genuine coordination function. The constraint is tangled_rope at the aggregate level but tilts toward snare from the weak-state seat and toward rope from the capacity-evaluator seat. The remedy is not constraint removal (the coordination problem of managing state fragility is real) but functional separation: detach legitimacy for peace operations and emergency relief from the ongoing capacity-evaluation framework that justifies permanent sovereignty subordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_metrics_objectivity,
    'Are state capacity metrics (fiscal revenue, rule-of-law indices, institutional quality scores) objective measurements of state function, or are they culturally specific constructs that embed Western institutional preferences?',
    'Comparative analysis of states that score low on Western metrics but maintain social stability and population trust (Singapore, Rwanda, Vietnam pre-liberalization); examination of which institutional features actually predict state stability vs. which ones are valued for cultural reasons; audit of metric design for Western bias.',
    'If metrics are objective, graded sovereignty is defensible as a rational framework for international response. If metrics are culturally specific, the constraint becomes a mechanism for imposing Western institutional forms on other societies—reclassification would shift from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_metrics_objectivity, conceptual, 'Whether capacity metrics are objective or culturally constructed measures of state function.').

omega_variable(
    suppression_internalization_trajectory,
    'Is weak-state acceptance of external assessment primarily structural (lack of exit options) or internalized (weak-state elites have adopted the capacity-deficit framing as legitimate)?',
    'Post-intervention data from states that exit external conditionality (Malaysia, South Korea, Chile post-Pinochet): if suppression persists (continued deference to capacity metrics, voluntary policy alignment with external preferences) after structural exit, internalization is substantial; if suppression collapses once conditionality is lifted, structural mechanisms dominated.',
    'High internalization increases the effective suppression of the constraint because targets maintain it even without external enforcement. Would shift classification toward snare (self-reinforcing extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Structural vs. internalized mechanisms maintaining weak-state deference to capacity evaluation.').

omega_variable(
    kernel_reading_contest_decomposition,
    'Are the three readings of the Westphalia kernel (absolute_non_intervention, conditional_responsibility, graded_sovereignty) genuinely three distinct constraints with different ε values, or are they interpretations of a single constraint that differ only in application?',
    'Structural analysis of each reading: absolute_non_intervention has zero extraction (categorical ban on interference) but high accessibility_collapse (no alternatives recognized); conditional_responsibility has moderate extraction (intervention only when threshold is met) and lower accessibility_collapse (non-triggering states retain sovereignty); graded_sovereignty has high extraction (permanent assessment, ongoing conditionality) and lowest accessibility_collapse (continuous evaluation). Each reading has a distinct ε and distinct victim set. They should be decomposed as three separate constraints and linked via network.affects_constraints.',
    'Decomposition clarifies that this story (graded_sovereignty) competes with and influences but does not foreclose the sibling readings. Each reading instantiates a different intervention legitimacy standard with different structural consequences for weak states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_decomposition, conceptual, 'Whether the three Westphalia kernel readings are one constraint with interpretive variation or three structurally distinct constraints.').

omega_variable(
    development_efficacy_divergence,
    'Do capacity-building interventions actually increase state capacity (improve fiscal revenue, strengthen institutions, establish violence monopoly), or do they primarily redistribute institutional authority to external controllers without improving underlying state function?',
    'Longitudinal analysis of states post-intervention: measure actual fiscal capacity, institutional independence, and violence-monopoly strength before and after intervention programs. Compare to counterfactual trajectory absent intervention. Distinguish between metrics that improve (institutions look more Western) and outcomes that improve (state actually functions better for its population).',
    'If development efficacy is high, the coordination story is vindicated and extraction is justified by positive externality. If low, the constraint is pure extraction with a development cover story—snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(development_efficacy_divergence, empirical, 'Whether capacity-building interventions improve state function or merely redistribute institutional control.').

omega_variable(
    committer_frame_reading_relations,
    'Does graded_sovereignty truly coexist with absolute_non_intervention as two live positions held by different parties, or does graded sovereignty''s institutionalization in UN and World Bank frameworks functionally foreclose absolute_non_intervention by establishing intervention-legitimacy norms in the only venues that matter?',
    'Track UN General Assembly voting patterns, Security Council intervention authorizations, and World Bank policy positions over the interval: if Western capacity-governance frameworks become the default setting and non-intervention voices become ceremonial rather than decision-making, foreclosure is occurring despite surface coexistence.',
    'If foreclosure is occurring, the reading_relations should be updated from coexists_with to influences_toward_foreclosure for absolute_non_intervention. The structure would be: graded_sovereignty influences (and gradually displaces) absolute_non_intervention by institutionalizing capacity metrics as the legitimacy standard, while being influenced by (but not foreclosed by) conditional_responsibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_reading_relations, empirical, 'Whether graded_sovereignty functionally displaces absolute_non_intervention through institutional dominance despite nominal coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__graded_sovereignty, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t0, westphalia_sovereignty__graded_sovereignty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(west_tr_t10, westphalia_sovereignty__graded_sovereignty, theater_ratio, 10, 0.38).
narrative_ontology:measurement(west_tr_t20, westphalia_sovereignty__graded_sovereignty, theater_ratio, 20, 0.45).
narrative_ontology:measurement(west_tr_t30, westphalia_sovereignty__graded_sovereignty, theater_ratio, 30, 0.52).
narrative_ontology:measurement(west_tr_t45, westphalia_sovereignty__graded_sovereignty, theater_ratio, 45, 0.58).
narrative_ontology:measurement(west_tr_t60, westphalia_sovereignty__graded_sovereignty, theater_ratio, 60, 0.58).
narrative_ontology:measurement(west_tr_t75, westphalia_sovereignty__graded_sovereignty, theater_ratio, 75, 0.58).

% Extraction over time
narrative_ontology:measurement(west_be_t0, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(west_be_t10, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(west_be_t20, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(west_be_t30, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(west_be_t45, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(west_be_t60, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(west_be_t75, westphalia_sovereignty__graded_sovereignty, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t0, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(west_su_t10, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(west_su_t20, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(west_su_t30, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(west_su_t45, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(west_su_t60, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(west_su_t75, westphalia_sovereignty__graded_sovereignty, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__graded_sovereignty, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(westphalia_sovereignty__graded_sovereignty, 0.18).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, westphalia_sovereignty__conditional_responsibility).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, international_development_conditionality).
narrative_ontology:affects_constraint(westphalia_sovereignty__graded_sovereignty, humanitarian_intervention_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the westphalia_sovereignty kernel. Sibling readings are authored as separate constraint stories: absolute_non_intervention (categorical sovereignty protection), conditional_responsibility (sovereignty loss via atrocity threshold). The three readings are linked as a constraint family via network.affects_constraints. Each reading instantiates different intervention legitimacy standards and victim sets. Decomposition follows ε-invariance principle: each reading has distinct ε (0.68 for graded_sovereignty vs. ~0.0 for absolute_non_intervention vs. ~0.35 for conditional_responsibility) and distinct structural consequences. The family structure models the kernel contest: all three readings remain live in international discourse, but graded_sovereignty's institutionalization in UN and World Bank frameworks creates pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(westphalia_sovereignty__graded_sovereignty, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
