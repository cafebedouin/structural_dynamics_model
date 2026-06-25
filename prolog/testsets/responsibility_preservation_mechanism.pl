% ============================================================================
% CONSTRAINT STORY: responsibility_preservation_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_responsibility_preservation_mechanism, []).

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
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: responsibility_preservation_mechanism
 *   human_readable: Responsibility Preservation Mechanism Under Situational Acknowledgment
 *   domain: moral_psychology/philosophy_of_action/legal_theory
 *
 * SUMMARY:
 *   Legal and moral systems face a structural tension: empirical psychology
 *   increasingly demonstrates that situational factors are primary behavioral
 *   determinants, yet criminal justice systems require individual
 *   responsibility attribution to function. The responsibility preservation
 *   mechanism resolves this tension by acknowledging situational influence in
 *   principle while systematically excluding or minimizing it in practice.
 *   Courts allow limited mitigation for extreme circumstances while
 *   maintaining that individuals remain the proper locus of punishment.
 *   Compatibilist philosophy provides intellectual cover by arguing that
 *   responsibility is compatible with causal determinism or situational
 *   influence. The constraint is claimed as tangled_rope: it genuinely
 *   coordinates social order through stable blame attribution while
 *   extracting punishment from individuals for factors beyond their control.
 *
 * KEY AGENTS:
 *   - judicial_authorities: Institutional agenda-setters (institutional/constrained) who control doctrinal boundaries for situational mitigation
 *   - defendants_with_situational_defenses: Primary targets (powerless/trapped) who bear punishment despite situational causation evidence
 *   - situationist_researchers: Secondary targets (moderate/constrained) whose findings are filtered when they threaten responsibility attribution
 *   - compatibilist_philosophers: Agenda-setters and beneficiaries (organized/mobile) who provide intellectual infrastructure for responsibility preservation
 *   - legal_system_legitimacy: Primary beneficiary (non-agent institutional abstraction) whose stability depends on individual culpability
 *   - analytical_observer: Analytical seat (analytical/analytical) who sees the coordination-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(responsibility_preservation_mechanism, 0.68).
domain_priors:suppression_score(responsibility_preservation_mechanism, 0.72).
domain_priors:theater_ratio(responsibility_preservation_mechanism, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(responsibility_preservation_mechanism, extractiveness, 0.68).
narrative_ontology:constraint_metric(responsibility_preservation_mechanism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(responsibility_preservation_mechanism, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(responsibility_preservation_mechanism, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(responsibility_preservation_mechanism, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(responsibility_preservation_mechanism, tangled_rope).
narrative_ontology:human_readable(responsibility_preservation_mechanism, "Responsibility Preservation Mechanism Under Situational Acknowledgment").
narrative_ontology:topic_domain(responsibility_preservation_mechanism, "moral_psychology/philosophy_of_action/legal_theory").

domain_priors:requires_active_enforcement(responsibility_preservation_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(responsibility_preservation_mechanism, '720e9d73-9e59-4653-901d-1cb9a68ec3c2').
narrative_ontology:cs_kernel_codification('720e9d73-9e59-4653-901d-1cb9a68ec3c2', distributed).
narrative_ontology:cs_authority_grounding('720e9d73-9e59-4653-901d-1cb9a68ec3c2', expertise).
narrative_ontology:cs_interpretation_layer_present('720e9d73-9e59-4653-901d-1cb9a68ec3c2').
narrative_ontology:cs_reading_relation('720e9d73-9e59-4653-901d-1cb9a68ec3c2', responsibility_preservation_mechanism__dispositional_reading, coexists_with).
narrative_ontology:cs_reading_relation('720e9d73-9e59-4653-901d-1cb9a68ec3c2', responsibility_preservation_mechanism__interactionist_reading, coexists_with).
narrative_ontology:cs_axiom('720e9d73-9e59-4653-901d-1cb9a68ec3c2', foundational, situations_override_character).
narrative_ontology:cs_axiom_status(situations_override_character, holdable).
narrative_ontology:cs_axiom_grounding('720e9d73-9e59-4653-901d-1cb9a68ec3c2', situations_override_character, empirically_contingent).
narrative_ontology:cs_axiom('720e9d73-9e59-4653-901d-1cb9a68ec3c2', secondary, responsibility_requires_character_stability).
narrative_ontology:cs_axiom_status(responsibility_requires_character_stability, holdable).
narrative_ontology:cs_axiom_grounding('720e9d73-9e59-4653-901d-1cb9a68ec3c2', responsibility_requires_character_stability, deontological).
narrative_ontology:cs_reference_frame('720e9d73-9e59-4653-901d-1cb9a68ec3c2', folk_dispositional_psychology).
narrative_ontology:cs_drift_state('720e9d73-9e59-4653-901d-1cb9a68ec3c2', post_situationist_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('720e9d73-9e59-4653-901d-1cb9a68ec3c2', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(responsibility_preservation_mechanism, legal_system_legitimacy).
narrative_ontology:constraint_beneficiary(responsibility_preservation_mechanism, retributive_justice_framework).
narrative_ontology:constraint_beneficiary(responsibility_preservation_mechanism, dispositional_moral_psychology).
narrative_ontology:constraint_victim(responsibility_preservation_mechanism, defendants_with_situational_defenses).
narrative_ontology:constraint_victim(responsibility_preservation_mechanism, situationist_researchers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(responsibility_preservation_mechanism, compatibilist_philosophers).
narrative_ontology:constraint_beneficiary(responsibility_preservation_mechanism, general_public).
narrative_ontology:constraint_victim(responsibility_preservation_mechanism, defense_attorneys).
narrative_ontology:constraint_victim(responsibility_preservation_mechanism, general_public).
narrative_ontology:constraint_vindicates(responsibility_preservation_mechanism, character_based_moral_agency).
narrative_ontology:constraint_vindicates(responsibility_preservation_mechanism, individual_culpability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The abstract legitimacy of legal systems that depend on individual responsibility attribution. If situational factors are acknowledged as primary determinants of behavior, the entire edifice of criminal law as currently structured faces a legitimacy crisis. The responsibility preservation mechanism allows courts to acknowledge situational influence while maintaining that individuals remain the proper locus of punishment.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, legal_system_legitimacy, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(responsibility_preservation_mechanism, legal_system_legitimacy).

% Set the doctrinal boundaries for when situational factors mitigate responsibility versus when they are deemed irrelevant. They must balance empirical evidence about situational influence against the practical necessity of maintaining a functioning criminal justice system. They control jury instructions, admissibility of expert testimony, and appellate doctrine on mitigating circumstances.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, judicial_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Bear criminal liability despite presenting evidence that situational factors were primary behavioral determinants. They attempt to introduce expert testimony about situational psychology, present evidence of coercive circumstances, or argue diminished capacity based on environmental factors. The constraint operates to exclude or minimize this evidence, maintaining individual culpability even when situational causation is demonstrated. Their punishment is the extraction the system requires to preserve its legitimacy.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, defendants_with_situational_defenses, payer,
    powerless, biographical, trapped, local).

% Produce empirical evidence that situational factors are primary behavioral determinants, but find their research systematically reinterpreted or excluded when it threatens responsibility attribution. When they serve as expert witnesses, their testimony is often ruled inadmissible or given minimal weight. Their professional findings are acknowledged in academic contexts but filtered out when they would undermine individual culpability in legal proceedings.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, situationist_researchers, payer,
    moderate, biographical, constrained, global).

% The philosophical and institutional framework that grounds punishment in individual desert. Requires that individuals be the proper targets of moral blame and legal sanction. If situational factors are primary, retributive punishment loses its justification and must be replaced with preventive or rehabilitative approaches that don't require individual culpability.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, retributive_justice_framework, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(responsibility_preservation_mechanism, retributive_justice_framework).

% The research tradition and theoretical framework that treats character traits as stable, cross-situationally consistent, and causally primary. Benefits from the legal system's continued reliance on character-based responsibility attribution, which provides institutional validation and funding for dispositional research programs while marginalizing situationist alternatives.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, dispositional_moral_psychology, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(responsibility_preservation_mechanism, dispositional_moral_psychology).

% Attempt to introduce situational evidence and expert testimony on behalf of clients, but face systematic evidentiary barriers and doctrinal limits on mitigation. They bear the professional cost of pursuing defenses that courts are structurally disposed to reject, and their clients bear the sentencing consequences when situational factors are excluded or minimized.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, defense_attorneys, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(responsibility_preservation_mechanism, defense_attorneys, excluded).

% Develop and refine philosophical arguments that preserve moral responsibility despite acknowledging causal determinism or situational influence. They provide the intellectual infrastructure that allows legal systems to acknowledge situational factors while maintaining individual culpability. Their work is cited in judicial opinions and legal scholarship to justify responsibility preservation.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, compatibilist_philosophers, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(responsibility_preservation_mechanism, compatibilist_philosophers, beneficiary).

% Benefit from the social coordination function of a stable legal system that can assign blame and impose punishment. Also bear diffuse costs: they are subject to the same responsibility attribution when they become defendants, and they fund a criminal justice system that may be punishing people for situational factors beyond individual control. Their folk psychology strongly supports dispositional attribution, making them natural allies of the constraint.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, general_public, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(responsibility_preservation_mechanism, general_public, payer).

% Argue for alternative frameworks that acknowledge situational causation and focus on repair rather than retribution. They would restructure criminal justice around harm reduction and community healing rather than individual culpability. Their voices are systematically marginalized in mainstream legal discourse because their framework would require abandoning responsibility preservation.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, restorative_justice_advocates, excluded,
    moderate, generational, constrained, national).

% Observes that the constraint operates as a tangled rope: it genuinely coordinates social order by maintaining a stable framework for blame attribution, while simultaneously extracting punishment from individuals for situational factors they did not control. The coordination function is real but could potentially be served by alternative frameworks that don't require individual culpability.
narrative_ontology:constraint_stakeholder(responsibility_preservation_mechanism, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a stable legal and moral framework for assigning responsibility and imposing sanctions. Without some mechanism for preserving individual accountability, criminal law as currently structured would collapse, creating genuine coordination problems around social order, deterrence, and victim recognition.
% TRANSFER_FUNCTION: Moves punishment and social stigma from situational factors (which cannot be sanctioned) onto individuals (who can be imprisoned, fined, and morally condemned), even when empirical evidence suggests situations were primary behavioral determinants. Transfers legitimacy from situationist research findings to dispositional frameworks that support continued responsibility attribution.
% ABSENT_VOICES: Restorative justice advocates, prison abolitionists, and radical situationists who would restructure justice systems around harm reduction rather than culpability are systematically excluded from mainstream legal discourse. Defendants who could present compelling situational defenses often lack resources for expert witnesses and face evidentiary barriers that prevent their full stories from being heard.
% DISAPPEARANCE_RATIONALE: If the responsibility preservation mechanism vanished overnight and courts fully acknowledged situational primacy, the criminal justice system would face immediate legitimacy crisis. Sentencing would become incoherent, appeals would flood the system, and legal doctrine would require fundamental restructuring. Alternative frameworks (restorative justice, preventive detention, pure consequentialism) would compete to fill the void. The social meaning of punishment would shift from deserved retribution to pragmatic social control.
% FOUNDING_PROBLEM: Early legal systems needed a stable framework for assigning blame and imposing sanctions to maintain social order. Individual responsibility attribution solved the coordination problem of determining who should be punished and provided a legitimating narrative for state violence. The dispositional assumption was largely unquestioned in the founding era.
% FOUNDING_PROBLEM_CORROBORATION: Legal system defenders attest the founding problem remains live: we still need stable blame attribution for social order. Situationist researchers and restorative justice advocates attest the founding problem is substantially solved by alternative frameworks that don't require individual culpability: preventive approaches, harm reduction, community accountability. Zimbardo's expert testimony in the Abu Ghraib trials and subsequent scholarly analysis document how situational evidence is systematically excluded even when it would explain behavior, suggesting the mechanism persists beyond its founding justification.
narrative_ontology:disappearance_verdict(responsibility_preservation_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(responsibility_preservation_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(responsibility_preservation_mechanism, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(responsibility_preservation_mechanism, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(responsibility_preservation_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(responsibility_preservation_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(responsibility_preservation_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.68) because the constraint imposes punishment on individuals for situational factors they did not control, while presenting this as deserved retribution. The extraction has accumulated over time as situationist evidence has mounted but been systematically excluded. Suppression is high (0.72) because the constraint requires active enforcement: evidentiary rules excluding situational testimony, jury instructions limiting mitigation, appellate doctrine constraining situational defenses. Theater ratio is moderate (0.41): the coordination function is real—legal systems do need stable blame attribution—but a growing share of enforcement activity defends responsibility preservation against empirical challenge rather than serving genuine coordination needs. Accessibility collapse is moderate (0.48): alternative frameworks exist (restorative justice, preventive detention, pure consequentialism) but face institutional barriers. Resistance is substantial (0.62): defense attorneys push situational evidence, situationist researchers challenge the framework, and some defendants successfully introduce mitigation despite barriers.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial seat, the constraint is necessary coordination: legal systems require stable responsibility attribution to function, and compatibilist philosophy shows this is philosophically defensible. From the defendant seat, the same structure operates as extraction: punishment is imposed for situational factors beyond individual control, with situational evidence systematically excluded. From the situationist researcher seat, it is suppression of empirical findings: their evidence is acknowledged in academic contexts but filtered when it would undermine culpability. The analytical observer sees all three: genuine coordination need, extractive punishment, and active suppression of alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Judicial authorities are agenda-setters with constrained exit: they benefit from system stability but are also bound by precedent and institutional role. Defendants with situational defenses are full targets (d ≈ 0.95): powerless, trapped, bearing direct punishment. Situationist researchers are secondary targets (d ≈ 0.70): moderate power, constrained exit, their work is filtered but they retain academic standing. Compatibilist philosophers are beneficiaries (d ≈ 0.20): organized, mobile, their framework is institutionally validated. Legal system legitimacy and retributive justice framework are abstract beneficiaries (d ≈ 0.10): the constraint preserves their stability. General public is mixed (d ≈ 0.50): benefits from coordination, bears diffuse costs as potential defendants.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy characteristics: its founding problem (need for stable blame attribution in early legal systems) has been substantially addressed by alternative frameworks, but the mechanism persists because legal system legitimacy depends on it. The rising theater ratio reflects this: an increasing share of enforcement activity defends responsibility preservation against empirical challenge rather than serving genuine coordination needs. However, the coordination function is not entirely defunct—legal systems do require some framework for assigning consequences—so this is tangled rope with mandatrophy drift rather than pure piton. The six-questions analysis reveals the gap: founding problem status is contested, with situationists and restorative justice advocates attesting that alternative frameworks could serve the coordination function without requiring individual culpability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity,
    'Is individual responsibility attribution structurally necessary for legal coordination, or could alternative frameworks (restorative justice, preventive detention, pure consequentialism) serve the same coordination function without requiring individual culpability?',
    'Natural experiments from jurisdictions that have substantially adopted restorative or preventive frameworks. If social order and victim recognition are maintained without individual culpability, the coordination function is separable from responsibility attribution.',
    'If separable, the responsibility preservation mechanism is pure extraction riding on a coordination function that could be served otherwise. If inseparable, part of the measured extraction is the necessary price of legal coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity, empirical, 'Whether legal coordination requires individual culpability or could function with alternative frameworks.').

omega_variable(
    situational_primacy_threshold,
    'At what threshold of situational influence does individual responsibility become incoherent? Is there a principled line between mitigating circumstances and complete exculpation, or is the current boundary arbitrary?',
    'Philosophical analysis of responsibility conditions combined with empirical data on situational effect sizes. If no principled threshold exists, current doctrine is arbitrary line-drawing to preserve system legitimacy.',
    'If the boundary is arbitrary, the constraint''s extraction is higher than measured—it punishes individuals on the wrong side of an unprincipled line. If a principled threshold exists, current doctrine might track genuine responsibility conditions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(situational_primacy_threshold, conceptual, 'Whether there is a principled threshold for responsibility attribution under situational influence.').

omega_variable(
    compatibilist_adequacy,
    'Do compatibilist arguments genuinely preserve moral responsibility under situational primacy, or do they merely provide philosophical cover for institutional necessity?',
    'Philosophical analysis of whether compatibilist conditions (reasons-responsiveness, mesh theories, etc.) are satisfied when situations are primary behavioral determinants. If compatibilist conditions fail under situational primacy, the arguments are post-hoc rationalization.',
    'If compatibilist arguments fail, the intellectual infrastructure supporting responsibility preservation collapses and the constraint is exposed as pure institutional necessity. If they succeed, the constraint has genuine philosophical grounding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(compatibilist_adequacy, conceptual, 'Whether compatibilist philosophy genuinely justifies responsibility under situational influence.').

omega_variable(
    evidentiary_filtering_mechanism,
    'Is situational evidence excluded because it is genuinely irrelevant to culpability, or because admitting it would undermine the system''s ability to impose punishment?',
    'Analysis of evidentiary rulings and jury instruction patterns. If exclusion correlates with threat to conviction rather than legal relevance, the filtering is extractive. Zimbardo''s Abu Ghraib testimony provides a documented case.',
    'If filtering is threat-based rather than relevance-based, the suppression component is higher than measured and the constraint operates primarily to preserve system legitimacy rather than serve justice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evidentiary_filtering_mechanism, empirical, 'Whether situational evidence is excluded for legal relevance or system preservation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(responsibility_preservation_mechanism, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(resp_tr_t0, responsibility_preservation_mechanism, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(resp_tr_t0, observed).
narrative_ontology:measurement(resp_tr_t10, responsibility_preservation_mechanism, theater_ratio, 10, 0.29).
narrative_ontology:measurement_basis(resp_tr_t10, observed).
narrative_ontology:measurement(resp_tr_t20, responsibility_preservation_mechanism, theater_ratio, 20, 0.33).
narrative_ontology:measurement_basis(resp_tr_t20, observed).
narrative_ontology:measurement(resp_tr_t30, responsibility_preservation_mechanism, theater_ratio, 30, 0.36).
narrative_ontology:measurement_basis(resp_tr_t30, observed).
narrative_ontology:measurement(resp_tr_t40, responsibility_preservation_mechanism, theater_ratio, 40, 0.39).
narrative_ontology:measurement_basis(resp_tr_t40, observed).
narrative_ontology:measurement(resp_tr_t50, responsibility_preservation_mechanism, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(resp_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(resp_be_t0, responsibility_preservation_mechanism, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(resp_be_t0, observed).
narrative_ontology:measurement(resp_be_t10, responsibility_preservation_mechanism, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(resp_be_t10, observed).
narrative_ontology:measurement(resp_be_t20, responsibility_preservation_mechanism, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(resp_be_t20, observed).
narrative_ontology:measurement(resp_be_t30, responsibility_preservation_mechanism, base_extractiveness, 30, 0.65).
narrative_ontology:measurement_basis(resp_be_t30, observed).
narrative_ontology:measurement(resp_be_t40, responsibility_preservation_mechanism, base_extractiveness, 40, 0.67).
narrative_ontology:measurement_basis(resp_be_t40, observed).
narrative_ontology:measurement(resp_be_t50, responsibility_preservation_mechanism, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(resp_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(resp_su_t0, responsibility_preservation_mechanism, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(resp_su_t0, observed).
narrative_ontology:measurement(resp_su_t10, responsibility_preservation_mechanism, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(resp_su_t10, observed).
narrative_ontology:measurement(resp_su_t20, responsibility_preservation_mechanism, suppression_requirement, 20, 0.65).
narrative_ontology:measurement_basis(resp_su_t20, observed).
narrative_ontology:measurement(resp_su_t30, responsibility_preservation_mechanism, suppression_requirement, 30, 0.68).
narrative_ontology:measurement_basis(resp_su_t30, observed).
narrative_ontology:measurement(resp_su_t40, responsibility_preservation_mechanism, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(resp_su_t40, observed).
narrative_ontology:measurement(resp_su_t50, responsibility_preservation_mechanism, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(resp_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(responsibility_preservation_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(responsibility_preservation_mechanism, dispositional_moral_psychology_framework).
narrative_ontology:affects_constraint(responsibility_preservation_mechanism, retributive_justice_doctrine).
narrative_ontology:affects_constraint(responsibility_preservation_mechanism, mitigating_circumstances_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the moral_causation_locus kernel. The dispositional reading would produce a different constraint with lower extraction (no responsibility crisis to manage). The interactionist reading would produce a constraint with dual intervention targets and more complex coordination-extraction structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
