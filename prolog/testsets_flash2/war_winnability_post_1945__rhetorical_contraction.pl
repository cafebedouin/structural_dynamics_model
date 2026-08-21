% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: War Winnability (Post-1945): Rhetorical Contraction Reading
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint describes the post-1945 dual-layer reality of nuclear war
 *   'winnability': publicly, it became a rhetorical taboo, unsayable in
 *   mainstream discourse; privately, it remained an object of operational
 *   planning and strategic thought. This reading focuses on the contraction
 *   of the discursive space while the strategic space persisted, creating a
 *   significant gap between public perception and classified reality. The
 *   constraint is claimed as a Tangled Rope because it genuinely coordinates
 *   the public narrative (deterrence stability) while extracting democratic
 *   oversight through suppression of open debate.
 *
 * KEY AGENTS:
 *   - strategic_planners: Primary agenda-setter (institutional/identity_locked) — benefits from operational flexibility without public accountability.
 *   - political_leadership: Beneficiary (powerful/constrained) — benefits from deterrence stability and reduced public scrutiny.
 *   - democratic_oversight: Primary payer (organized/constrained) — bears the cost of reduced transparency and accountability.
 *   - public_discourse: Payer (moderate/identity_locked) — constrained by rhetorical taboo, leading to simplified understanding.
 *   - arms_control_advocates: Excluded (organized/constrained) — marginalized from mainstream debate.
 *   - academic_strategists: Observer (analytical/analytical) — analyze the gap between rhetoric and planning.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.65).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.78).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "War Winnability (Post-1945): Rhetorical Contraction Reading").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, '745a8bf9-652c-4dde-ae69-90105f52ce32').
narrative_ontology:cs_kernel_codification('745a8bf9-652c-4dde-ae69-90105f52ce32', distributed).
narrative_ontology:cs_authority_grounding('745a8bf9-652c-4dde-ae69-90105f52ce32', extraction).
narrative_ontology:cs_interpretation_layer_present('745a8bf9-652c-4dde-ae69-90105f52ce32').
narrative_ontology:cs_reading_relation('745a8bf9-652c-4dde-ae69-90105f52ce32', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('745a8bf9-652c-4dde-ae69-90105f52ce32', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_axiom('745a8bf9-652c-4dde-ae69-90105f52ce32', foundational, nuclear_war_is_discursively_unwinnable).
narrative_ontology:cs_axiom_status(nuclear_war_is_discursively_unwinnable, holdable).
narrative_ontology:cs_axiom_grounding('745a8bf9-652c-4dde-ae69-90105f52ce32', nuclear_war_is_discursively_unwinnable, conventional).
narrative_ontology:cs_axiom('745a8bf9-652c-4dde-ae69-90105f52ce32', foundational, operational_winnability_is_strategically_necessary).
narrative_ontology:cs_axiom_status(operational_winnability_is_strategically_necessary, holdable).
narrative_ontology:cs_axiom_grounding('745a8bf9-652c-4dde-ae69-90105f52ce32', operational_winnability_is_strategically_necessary, instrumental).
narrative_ontology:cs_reference_frame('745a8bf9-652c-4dde-ae69-90105f52ce32', post_hiroshima_strategic_dilemma).
narrative_ontology:cs_drift_state('745a8bf9-652c-4dde-ae69-90105f52ce32', contemporary_strategic_environment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('745a8bf9-652c-4dde-ae69-90105f52ce32', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, political_leadership).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, public_discourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain operational plans for various nuclear war scenarios, including those involving 'winnability' or 'victory' in a constrained sense. They benefit from the rhetorical taboo as it reduces public scrutiny of these plans, allowing for greater flexibility and less political cost in maintaining capabilities and doctrines.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefits from the public perception that nuclear war is unwinnable, which reinforces deterrence and reduces pressure for costly or controversial public debates about nuclear strategy. Simultaneously, they rely on strategic planners to maintain operational options, even if those options are not publicly discussed.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, political_leadership, beneficiary,
    powerful, biographical, constrained, national).

% Bears the cost of reduced transparency and accountability regarding nuclear war planning. The rhetorical taboo makes it difficult to scrutinize or challenge operational doctrines that contradict public pronouncements, leading to a gap between declared policy and actual capabilities/plans.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight, payer,
    organized, biographical, constrained, national).

% Is constrained by the rhetorical taboo, which limits the range of acceptable discussion about nuclear war. This leads to a simplified, often alarmist, public understanding of nuclear conflict, hindering nuanced debate and informed democratic participation in strategic policy.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, public_discourse, payer,
    moderate, generational, identity_locked, global).

% Would argue that the persistence of winnability planning undermines arms control efforts and increases the risk of escalation. Their arguments are often marginalized by the rhetorical taboo, which frames any discussion of 'winning' as irresponsible or dangerous.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, arms_control_advocates, excluded,
    organized, generational, constrained, global).

% Analyze the gap between public rhetoric and classified planning, often attempting to bridge the discursive divide or expose its implications for stability. They are aware of the operational persistence of winnability concepts despite their public unsayability.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, academic_strategists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the public narrative around nuclear war (unwinnable) with the operational reality (winnability is a constrained planning objective), allowing political leaders to maintain deterrence credibility without public panic or detailed scrutiny of war plans.
% TRANSFER_FUNCTION: Transfers public accountability and discursive flexibility from strategic planners and political leadership to democratic oversight and public discourse, which bear the cost of limited information and constrained debate.
% ABSENT_VOICES: Arms control advocates and some critical academic strategists are effectively marginalized from mainstream public discourse on nuclear war, as their attempts to discuss operational winnability are often dismissed as dangerous or naive, reinforcing the rhetorical taboo.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo around winnability vanished overnight, public debate on nuclear strategy would intensify dramatically. Strategic planners would face immense pressure to declassify or justify their plans, potentially leading to a re-evaluation of doctrines, increased public resistance to nuclear modernization, and a fundamental shift in the relationship between military planning and democratic accountability.
% FOUNDING_PROBLEM: After the advent of nuclear weapons, the problem was how to maintain deterrence and strategic options without provoking public panic or undermining the perception of nuclear war as catastrophic.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners and political leadership attest the problem is still live, citing the need for both deterrence stability and credible response options. Academic strategists and some former officials corroborate the historical emergence of this dual-layer approach as a pragmatic response to the nuclear age, though they may dispute its current utility or ethical implications.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__rhetorical_contraction, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the rhetorical contraction effectively transfers control over a critical strategic domain from public debate to a closed planning elite. Suppression is very high, as the taboo is actively enforced through social and political mechanisms, marginalizing dissenting voices. Theater ratio is also very high, reflecting the performative maintenance of the 'unwinnable' narrative, which masks the underlying operational planning. Accessibility collapse is moderate-high because while the public can theoretically access information, the discursive environment makes it difficult to engage with the topic meaningfully. Resistance is low because the taboo is largely internalized or seen as a necessary evil for stability.
 *
 * PERSPECTIVAL GAP:
 *   Strategic planners and political leadership experience this as a necessary coordination mechanism for national security, allowing them to manage complex threats without public interference. Democratic oversight and public discourse experience it as a form of epistemic capture, where critical information and debate are suppressed, leading to a less informed and less accountable strategic policy. The engine's classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and political leadership are beneficiaries, as the constraint grants them operational flexibility and reduces political costs. Democratic oversight and public discourse are victims, as they are deprived of transparency and the ability to engage in informed debate. The 'identity_locked' exit for planners reflects their professional commitment to maintaining strategic options, while for public discourse, it reflects the deep internalization of the taboo.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function of maintaining deterrence stability and avoiding public panic. However, it also highlights the asymmetric extraction of democratic oversight, which is a core feature of a Tangled Rope. The high theater ratio indicates that the performative aspect of the 'unwinnable' narrative has become dominant, masking the underlying operational reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discursive_vs_operational_gap_magnitude,
    'What is the precise magnitude of the gap between public rhetoric and classified operational planning regarding nuclear war winnability?',
    'Declassification of historical strategic documents, expert testimony from former planners, and comparative analysis of public statements versus actual military exercises and procurement decisions.',
    'A larger gap would increase the measured extractiveness and suppression, potentially shifting the classification towards Snare, as the coordination function would appear more as a cover for hidden operational agendas. A smaller gap would support a more balanced Tangled Rope or even Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discursive_vs_operational_gap_magnitude, empirical, 'Quantifying the divergence between public discourse and strategic reality.').

omega_variable(
    necessity_of_taboo_for_deterrence,
    'Is the rhetorical taboo on ''winnability'' genuinely necessary for maintaining nuclear deterrence stability, or does it primarily serve to protect strategic planners from public scrutiny?',
    'Comparative analysis of deterrence outcomes in states with more transparent nuclear doctrines versus those with a strong rhetorical taboo, and theoretical modeling of public discourse''s impact on crisis stability.',
    'If the taboo is found to be largely unnecessary for deterrence, the constraint''s coordination function would be significantly diminished, increasing its effective extraction and potentially reclassifying it as a Snare. If it is found to be essential, the coordination aspect would be strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_of_taboo_for_deterrence, conceptual, 'Assessing the functional necessity of the rhetorical taboo for deterrence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of ''winnability'' discourse structural (e.g., classification, media gatekeeping) or internalized (e.g., self-censorship by academics, public aversion to the topic)?',
    'Content analysis of media coverage and academic publications over time, surveys of public attitudes and expert opinions, and analysis of institutional incentives for self-censorship. If suppression persists after formal barriers are removed, it suggests internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the public carries the suppression with them, making open debate harder even if formal barriers are lowered. This would reinforce the Tangled Rope classification''s extractive aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for rhetorical taboo.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1960, 0.5).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.8).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2000, 0.88).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1960, 0.7).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.85).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, identity_coordination).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, countervailing_thinkable).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'war_winnability_post_1945' kernel. It describes the rhetorical contraction of winnability discourse while operational planning persists. It coexists with and influences other readings like 'deterrence_unthinkable' and 'countervailing_thinkable', which represent different interpretations of nuclear war's strategic implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
