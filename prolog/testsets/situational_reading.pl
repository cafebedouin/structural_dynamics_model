% ============================================================================
% CONSTRAINT STORY: situational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_situational_reading, []).

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
 *   constraint_id: situational_reading
 *   human_readable: Situational Determinism in Moral Action
 *   domain: moral_psychology/philosophy_of_action/social_psychology
 *
 * SUMMARY:
 *   The situational reading of moral causation holds that external
 *   circumstances are the primary drivers of moral action, that most people
 *   lack robust character traits, and that ethical behavior is highly
 *   context-dependent. This reading emerged from mid-20th-century social
 *   psychology experiments (Milgram, Zimbardo) and has become dominant in
 *   applied fields like organizational behavior and criminal justice reform.
 *   It is one of three major readings of the moral causation kernel; the
 *   dispositional reading emphasizes stable character traits, and the
 *   interactionist reading treats situations and dispositions as
 *   co-determinants. The situational reading coordinates research and policy
 *   around environmental design but extracts authority and resources from
 *   character-based approaches while maintaining individual accountability in
 *   practice, creating a structural tension.
 *
 * KEY AGENTS:
 *   - institutional_designers: Primary agenda-setters (institutional/mobile) — design systems based on situational primacy and collect professional authority from the framework's dominance
 *   - social_engineers: Beneficiaries (powerful/mobile) — implement situational interventions and profit from the framework's application
 *   - individuals_held_accountable: Primary targets (powerless/trapped) — bear moral and legal costs despite the framework's exculpatory logic
 *   - character_education_advocates: Secondary targets (moderate/constrained) — lose institutional support and resources as situational approaches dominate
 *   - moral_philosophers: Analytical observers (analytical/analytical) — examine the reading's coherence and empirical support
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(situational_reading, 0.68).
domain_priors:suppression_score(situational_reading, 0.72).
domain_priors:theater_ratio(situational_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(situational_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(situational_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(situational_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(situational_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(situational_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(situational_reading, tangled_rope).
narrative_ontology:human_readable(situational_reading, "Situational Determinism in Moral Action").
narrative_ontology:topic_domain(situational_reading, "moral_psychology/philosophy_of_action/social_psychology").

domain_priors:requires_active_enforcement(situational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(situational_reading, 'ec0d8812-6e64-4203-aac8-f7e93b6912ee').
narrative_ontology:cs_kernel_codification('ec0d8812-6e64-4203-aac8-f7e93b6912ee', distributed).
narrative_ontology:cs_authority_grounding('ec0d8812-6e64-4203-aac8-f7e93b6912ee', expertise).
narrative_ontology:cs_interpretation_layer_present('ec0d8812-6e64-4203-aac8-f7e93b6912ee').
narrative_ontology:cs_reading_relation('ec0d8812-6e64-4203-aac8-f7e93b6912ee', moral_causation_locus__dispositional_reading, coexists_with).
narrative_ontology:cs_reading_relation('ec0d8812-6e64-4203-aac8-f7e93b6912ee', moral_causation_locus__interactionist_reading, influences).
narrative_ontology:cs_axiom('ec0d8812-6e64-4203-aac8-f7e93b6912ee', foundational, situational_primacy_over_disposition).
narrative_ontology:cs_axiom_status(situational_primacy_over_disposition, holdable).
narrative_ontology:cs_axiom_grounding('ec0d8812-6e64-4203-aac8-f7e93b6912ee', situational_primacy_over_disposition, empirically_contingent).
narrative_ontology:cs_axiom('ec0d8812-6e64-4203-aac8-f7e93b6912ee', foundational, character_instability_across_contexts).
narrative_ontology:cs_axiom_status(character_instability_across_contexts, holdable).
narrative_ontology:cs_axiom_grounding('ec0d8812-6e64-4203-aac8-f7e93b6912ee', character_instability_across_contexts, empirically_contingent).
narrative_ontology:cs_axiom('ec0d8812-6e64-4203-aac8-f7e93b6912ee', secondary, institutional_design_as_primary_intervention).
narrative_ontology:cs_axiom_status(institutional_design_as_primary_intervention, holdable).
narrative_ontology:cs_axiom_grounding('ec0d8812-6e64-4203-aac8-f7e93b6912ee', institutional_design_as_primary_intervention, instrumental).
narrative_ontology:cs_reference_frame('ec0d8812-6e64-4203-aac8-f7e93b6912ee', mid_century_social_psychology_paradigm).
narrative_ontology:cs_drift_state('ec0d8812-6e64-4203-aac8-f7e93b6912ee', post_replication_crisis_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ec0d8812-6e64-4203-aac8-f7e93b6912ee', '').
narrative_ontology:cs_kernel_id(situational_reading, moral_causation_locus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(situational_reading, institutional_designers).
narrative_ontology:constraint_beneficiary(situational_reading, social_engineers).
narrative_ontology:constraint_beneficiary(situational_reading, situational_interventionists).
narrative_ontology:constraint_victim(situational_reading, individuals_held_accountable).
narrative_ontology:constraint_victim(situational_reading, character_education_advocates).
narrative_ontology:constraint_vindicates(situational_reading, environmental_determinism).
narrative_ontology:constraint_vindicates(situational_reading, institutional_design_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design policies, organizational structures, and social systems based on the premise that situations drive behavior. They justify interventions that reshape environments rather than cultivate character, and their professional authority depends on the situational framework being accepted as primary. They collect research funding, institutional positions, and policy influence from this reading.
narrative_ontology:constraint_stakeholder(situational_reading, institutional_designers, agenda_setter,
    institutional, generational, mobile, national).

% Implement situational interventions in organizations, governments, and communities. They benefit from a framework that positions external design as the solution to moral failure, which expands their scope of work and authority. They do not set the theoretical agenda but profit from its application.
narrative_ontology:constraint_stakeholder(situational_reading, social_engineers, beneficiary,
    powerful, biographical, mobile, national).

% Practitioners who apply situational principles in education, criminal justice reform, and organizational behavior. They gain professional legitimacy and employment from the framework's dominance in applied settings. Their training and credentials are built around situational analysis.
narrative_ontology:constraint_stakeholder(situational_reading, situational_interventionists, beneficiary,
    organized, biographical, constrained, regional).

% Face moral judgment and legal accountability for actions the framework attributes primarily to circumstances. The situational reading reduces their culpability in theory but does not eliminate punishment in practice, creating a gap where they bear costs without the framework's exculpatory logic being applied consistently. They cannot exit the accountability structures that ignore the reading's implications.
narrative_ontology:constraint_stakeholder(situational_reading, individuals_held_accountable, payer,
    powerless, biographical, trapped, local).

% Promote virtue cultivation, moral education, and individual responsibility as primary interventions. The situational reading marginalizes their approach in academic and policy circles, reducing funding and institutional support for character-based programs. They bear reputational and resource costs as the situational framework dominates.
narrative_ontology:constraint_stakeholder(situational_reading, character_education_advocates, payer,
    moderate, generational, constrained, national).

% Individuals who committed moral atrocities under corrupting circumstances. The situational reading explains their actions as products of environment, but they remain excluded from the conversation about moral causation and are not granted the reduced accountability the framework implies. They would argue for exculpation based on situational pressure but are not heard.
narrative_ontology:constraint_stakeholder(situational_reading, atrocity_perpetrators, excluded,
    powerless, immediate, trapped, local).

% Analyze the situational reading as one position in the moral causation debate. They examine its empirical support, its normative implications, and its coherence with other philosophical commitments. They see the reading as contested and note the gap between its theoretical claims and its practical application in accountability systems.
narrative_ontology:constraint_stakeholder(situational_reading, moral_philosophers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified explanatory framework for moral failure that directs intervention toward institutional and environmental design rather than individual cultivation, coordinating research programs, policy initiatives, and professional training around situational variables.
% TRANSFER_FUNCTION: Moves authority and resources from character-based interventions to situational design experts; shifts moral responsibility from individuals to system designers in theory while maintaining individual accountability in practice.
% ABSENT_VOICES: Atrocity perpetrators whose actions the framework explains but who are excluded from reduced accountability; individuals in non-Western moral traditions where character and virtue remain central; victims of institutional failures who see designers evade responsibility by blaming 'the situation.'
% DISAPPEARANCE_RATIONALE: If the situational reading vanished, moral psychology would revert to dispositional or interactionist frameworks, character education would regain institutional support, policy interventions would shift from environmental redesign to virtue cultivation, and accountability structures would more explicitly embrace individual responsibility without the theoretical tension the situational reading creates.
% FOUNDING_PROBLEM: Early 20th-century moral psychology needed to explain why ordinary people commit atrocities and why moral behavior varies dramatically across contexts, challenging the assumption that character is stable and predictive.
% FOUNDING_PROBLEM_CORROBORATION: Institutional designers and social psychologists attest the problem is live, citing replication of situational effects in laboratory and field studies. Moral philosophers and character education advocates attest the founding experiments have failed to replicate robustly, that situational effects are smaller and more context-dependent than claimed, and that the framework overstates environmental determinism while understating dispositional stability. Independent meta-analyses and replication failures support the contested status.
narrative_ontology:disappearance_verdict(situational_reading, world_rearranges).
narrative_ontology:founding_problem_status(situational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(situational_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-06-25',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(situational_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(situational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(situational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(situational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is substantial (0.68) because the framework transfers authority from character cultivation to situational design while not reducing individual accountability in practice, creating asymmetric costs. Suppression is high (0.72) because the reading's dominance in academic and policy institutions actively marginalizes dispositional approaches and suppresses alternative frameworks. Theater ratio is moderate (0.42) because genuine coordination exists (the framework does direct research and policy coherently) but a growing share of activity defends the paradigm against replication failures and dispositional evidence rather than solving the founding problem. Accessibility collapse is moderate (0.48) because dispositional and interactionist alternatives remain conceptually available and empirically supported. Resistance is high (0.71) because character education advocates, moral philosophers, and replication-crisis researchers actively contest the framework's claims. The measurements show accumulating extraction and theater as the framework matured from explanatory hypothesis to institutional orthodoxy.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional designer seat, the constraint is genuine coordination solving the problem of moral failure through better system design. From the powerless individual seat, the same structure operates as extraction: the framework explains their actions as situationally determined but does not reduce their punishment, while designers evade responsibility for the situations they create. The engine computes this divergence from the structural data; the claimed type (tangled_rope) reflects the coordination function without adjudicating whether it justifies the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional designers are structural beneficiaries (set the agenda, collect authority, mobile exit — d near 0.2). Social engineers and interventionists are secondary beneficiaries (profit from application, constrained to mobile exit — d near 0.3). Individuals held accountable are primary targets (bear costs without exculpatory benefit, trapped — d near 0.95). Character education advocates are secondary targets (lose resources and legitimacy, constrained exit — d near 0.75). The asymmetry is that designers gain authority by attributing moral failure to situations while individuals still face accountability as if they had dispositional control.
 *
 * MANDATROPHY ANALYSIS:
 *   The situational reading risks mandatrophy if its founding problem (explaining context-dependent moral failure) has been solved or overstated. Replication failures in foundational experiments, meta-analyses showing smaller effect sizes, and evidence of dispositional stability challenge the framework's empirical basis. If moral behavior is more dispositionally stable than the reading claims, the framework persists as extraction from character-based approaches rather than as coordination solving a live problem. The omega variables address this directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replication_crisis_impact,
    'Have the foundational situational experiments (Milgram, Zimbardo, bystander effect) replicated robustly, or do replication failures undermine the framework''s empirical basis?',
    'Systematic meta-analysis of replication attempts, effect size estimation across studies, and independent assessment of experimental validity. The replication crisis in social psychology provides ongoing data.',
    'If foundational effects are smaller or less robust than claimed, the situational reading''s dominance represents extraction from dispositional approaches rather than coordination solving a live empirical problem. The framework would persist as institutional inertia rather than as validated science.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_crisis_impact, empirical, 'Whether the situational reading''s empirical foundation survives replication scrutiny.').

omega_variable(
    accountability_application_gap,
    'Why does the situational reading reduce individual culpability in theory but not in practice? Is this gap a feature (preserving necessary accountability) or a bug (extractive inconsistency)?',
    'Philosophical analysis of the framework''s normative implications combined with empirical study of how situational explanations are applied in legal and organizational accountability systems. Does the gap serve a coordination function or protect designers from responsibility?',
    'If the gap is extractive, the framework allows designers to evade responsibility for creating corrupting situations while individuals still bear full costs. If the gap is functional, it preserves accountability while improving system design. The classification hinges on whether the asymmetry is justified or exploitative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_application_gap, conceptual, 'Whether the theory-practice gap in accountability is coordination or extraction.').

omega_variable(
    dispositional_stability_evidence,
    'Is there robust evidence for dispositional stability and cross-situational consistency in moral behavior that the situational reading marginalizes?',
    'Longitudinal studies of moral behavior across contexts, meta-analyses of personality-behavior correlations, and cross-cultural studies of virtue and character. Independent assessment by researchers not committed to the situational paradigm.',
    'If dispositional stability is empirically supported, the situational reading''s dominance represents suppression of a valid alternative framework. The constraint would be reclassified as more extractive, with higher suppression, as it maintains institutional control by marginalizing contradictory evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispositional_stability_evidence, empirical, 'Whether dispositional approaches have empirical support the situational reading suppresses.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the moral causation locus a genuine empirical question with a discoverable answer, or is it a conceptual framing choice that different traditions make for different purposes?',
    'Philosophical analysis of whether ''where moral action originates'' is an empirical claim or a normative/conceptual commitment. Cross-cultural comparison of moral frameworks that privilege different loci without empirical contradiction.',
    'If the kernel is a framing choice rather than an empirical question, the situational reading''s dominance is a power move rather than a scientific victory. The constraint would be reclassified as more extractive, with the coordination function reinterpreted as paradigm enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Whether the kernel itself is empirical or conceptual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(situational_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(situ_tr_t0, situational_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(situ_tr_t16, situational_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(situ_tr_t32, situational_reading, theater_ratio, 32, 0.31).
narrative_ontology:measurement(situ_tr_t48, situational_reading, theater_ratio, 48, 0.36).
narrative_ontology:measurement(situ_tr_t64, situational_reading, theater_ratio, 64, 0.39).
narrative_ontology:measurement(situ_tr_t80, situational_reading, theater_ratio, 80, 0.42).

% Extraction over time
narrative_ontology:measurement(situ_be_t0, situational_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(situ_be_t16, situational_reading, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(situ_be_t32, situational_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(situ_be_t48, situational_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement(situ_be_t64, situational_reading, base_extractiveness, 64, 0.66).
narrative_ontology:measurement(situ_be_t80, situational_reading, base_extractiveness, 80, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(situ_su_t0, situational_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(situ_su_t16, situational_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(situ_su_t32, situational_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(situ_su_t48, situational_reading, suppression_requirement, 48, 0.65).
narrative_ontology:measurement(situ_su_t64, situational_reading, suppression_requirement, 64, 0.69).
narrative_ontology:measurement(situ_su_t80, situational_reading, suppression_requirement, 80, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(situational_reading, identity_coordination).
narrative_ontology:affects_constraint(situational_reading, dispositional_reading).
narrative_ontology:affects_constraint(situational_reading, interactionist_reading).

% DUAL FORMULATION NOTE:
% The moral_causation_locus kernel decomposes into three readings with different ε values. The situational reading (this constraint) is substantially extractive (0.68) because it marginalizes dispositional approaches and creates an accountability gap. The dispositional reading emphasizes stable character and is likely less extractive but faces its own empirical challenges. The interactionist reading attempts synthesis and may have lower extraction if it genuinely integrates both. The readings are linked because they compete for institutional dominance in moral psychology, education, and policy, and each reading's success affects the others' resource availability and legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
