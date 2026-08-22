% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Self-Defense — Narrow Armed-Attack Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   The narrow armed-attack reading is the textually orthodox interpretation
 *   of UN Charter Article 51, anchored in the ICJ's repeated holdings that
 *   self-defense requires an armed attack attributable to a state under
 *   established attribution rules (effective control, or at minimum
 *   substantial involvement), and that mere harboring, tolerance, or
 *   inability to suppress non-state armed groups does not itself trigger a
 *   lawful self-defense response against the territorial state. This reading
 *   functions as the doctrinal floor beneath the UN Charter's prohibition on
 *   the use of force: it treats Article 51 as a narrow exception, not a
 *   general license, and channels contested force decisions back toward the
 *   Security Council. It is under sustained pressure from state practice
 *   (Israel, the US, and others invoking force against non-state actors in
 *   third states) that increasingly resembles the unable/unwilling or
 *   preventive doctrines this story's siblings describe.
 *
 * KEY AGENTS:
 *   - weaker_un_member_states: Primary beneficiary (moderate/constrained) — shielded from expansive force doctrines
 *   - un_security_council: Agenda-setter/beneficiary (institutional/analytical) — gatekeeping authority preserved
 *   - international_court_of_justice: Agenda-setter (institutional/analytical) — doctrinal author and defender
 *   - great_power_militaries: Primary target (institutional/constrained) — strategic freedom constrained
 *   - states_facing_non_state_armed_groups: Secondary target (powerful/constrained) — denied self-defense trigger absent attribution
 *   - attacked_civilian_populations: Excluded — bear costs of both the threat and the doctrinal gap
 *   - international_law_scholarship: Analytical observer — tracks crystallization of customary law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.42).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.55).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Self-Defense — Narrow Armed-Attack Reading").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, 'e3fd96ee-021c-4d20-8e8a-890b9102373f').
narrative_ontology:cs_kernel_codification('e3fd96ee-021c-4d20-8e8a-890b9102373f', fixed_text).
narrative_ontology:cs_authority_grounding('e3fd96ee-021c-4d20-8e8a-890b9102373f', lineage).
narrative_ontology:cs_interpretation_layer_present('e3fd96ee-021c-4d20-8e8a-890b9102373f').
narrative_ontology:cs_reading_relation('e3fd96ee-021c-4d20-8e8a-890b9102373f', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_reading_relation('e3fd96ee-021c-4d20-8e8a-890b9102373f', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('e3fd96ee-021c-4d20-8e8a-890b9102373f', foundational, self_defense_requires_state_attributable_armed_attack).
narrative_ontology:cs_axiom_status(self_defense_requires_state_attributable_armed_attack, holdable).
narrative_ontology:cs_axiom_grounding('e3fd96ee-021c-4d20-8e8a-890b9102373f', self_defense_requires_state_attributable_armed_attack, conventional).
narrative_ontology:cs_axiom('e3fd96ee-021c-4d20-8e8a-890b9102373f', foundational, unilateral_force_determination_must_not_be_self_judging).
narrative_ontology:cs_axiom_status(unilateral_force_determination_must_not_be_self_judging, holdable).
narrative_ontology:cs_axiom_grounding('e3fd96ee-021c-4d20-8e8a-890b9102373f', unilateral_force_determination_must_not_be_self_judging, deontological).
narrative_ontology:cs_reference_frame('e3fd96ee-021c-4d20-8e8a-890b9102373f', un_charter_1945_narrow_exception_framework).
narrative_ontology:cs_drift_state('e3fd96ee-021c-4d20-8e8a-890b9102373f', post_9_11_non_state_threat_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e3fd96ee-021c-4d20-8e8a-890b9102373f', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_un_member_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, un_security_council).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, non_aligned_states).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, great_power_militaries).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_armed_groups).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, un_charter_article_2_4_prohibition_primacy).
narrative_ontology:constraint_vindicates(article_51_self_defense__narrow_armed_attack_reading, state_sovereignty_non_intervention_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lack the military capacity to project force preemptively and depend on the prohibition of unilateral force to deter stronger neighbors from invoking self-defense against them on thin pretexts. The narrow reading is their primary shield: it forces any attacker to point to an actual or imminent armed attack traceable to a state, not a vague threat narrative.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, weaker_un_member_states, beneficiary,
    moderate, generational, constrained, global).

% Holds the Chapter VII authorization monopoly that the narrow reading preserves by keeping Article 51 a narrow, self-executing exception rather than a broad license. Every time a state claims self-defense against non-attributable non-state threats without going to the Council, the Council's centrality erodes; the narrow reading is the doctrinal wall that channels force decisions back through it.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, un_security_council, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, un_security_council, beneficiary).

% Has repeatedly articulated and defended this reading (Nicaragua, Armed Activities, Wall Advisory Opinion), tying Article 51 to attributable state action under the ILC attribution rules. Its institutional authority as the interpretive body for the Charter is directly vindicated by states accepting this reading rather than treating self-defense as self-judging.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__narrow_armed_attack_reading, international_court_of_justice, observer).

% Face transnational terrorist networks, proxy militias, and cyber-enabled threats that frequently cannot be attributed to a state under the ILC's effective-control or overall-control tests in time to matter operationally. Under this reading, cross-border strikes against such groups on the territory of a non-consenting, non-complicit state are unlawful regardless of operational necessity, forcing a choice between restraint, legal exposure, or open breach and reputational cost among allies and courts.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, great_power_militaries, payer,
    institutional, immediate, constrained, global).

% Experience sustained cross-border attacks from militias or insurgents based in a neighboring state that is weak or complicit but not clearly 'directing or controlling' the group under legal attribution standards. The narrow reading denies them a self-defense trigger unless attribution is established, leaving them to argue necessity outside Article 51's textual anchor or seek Security Council action that may be blocked by veto.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, states_facing_non_state_armed_groups, payer,
    powerful, biographical, constrained, regional).

% Bear the direct physical cost of non-state armed group attacks and of any resulting cross-border military response, but have no voice in the doctrinal debate over attribution standards; their safety is treated as an input to a legal argument conducted by states and institutions, not a party to it.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, attacked_civilian_populations, excluded,
    powerless, immediate, trapped, local).

% Debates and documents the doctrinal contest, tracking state practice and opinio juris to determine whether the narrow reading, the unable/unwilling doctrine, or an expansive preventive standard is crystallizing as customary law. Has no enforcement power but shapes which reading future tribunals and diplomats treat as authoritative.
narrative_ontology:constraint_stakeholder(article_51_self_defense__narrow_armed_attack_reading, international_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__narrow_armed_attack_reading, diffuse).
narrative_ontology:fixing_cost_class(article_51_self_defense__narrow_armed_attack_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, textually anchored, bright-line trigger for lawful unilateral force — an actual or imminent armed attack attributable to a state — so that the general prohibition on the use of force in Article 2(4) has a narrow, predictable, and mutually verifiable exception rather than a self-judging one any state could invoke.
% TRANSFER_FUNCTION: Moves strategic freedom of action away from militarily powerful states capable of striking non-state threats abroad, and toward weaker states and the Security Council, by requiring attribution to a state before force is lawful — effectively taxing operational flexibility in exchange for protection against being targeted on thin pretexts.
% ABSENT_VOICES: Civilian populations living under continuous non-state armed group violence have no forum in the interstate legal debate; military planners inside great-power states argue the reading is disconnected from operational reality but their objections surface mainly in domestic policy debate, not in the doctrinal record the ICJ and General Assembly treat as authoritative.
% DISAPPEARANCE_RATIONALE: If the narrow reading were abandoned overnight in favor of an unconstrained self-judging standard, powerful states would face far fewer legal barriers to cross-border strikes against non-state actors and even preventive strikes against emerging threats; weaker states would lose their principal doctrinal shield against being targeted under expansive threat narratives, and the Security Council's gatekeeping role over force authorization would be substantially bypassed.
% FOUNDING_PROBLEM: The UN Charter drafters sought to abolish the pre-1945 customary law tradition in which any state could unilaterally judge its own necessity and proportionality for using force, which had provided legal cover for wars of aggression; Article 51 was drafted as a narrow, textually bounded exception to the Article 2(4) prohibition, preserving self-defense only for the clearest case — an actual or imminent armed attack.
% FOUNDING_PROBLEM_CORROBORATION: The ICJ (Nicaragua, 1986; Armed Activities on the Territory of the Congo, 2005) and the majority of UN General Assembly member states attest the founding problem remains live — self-judging force claims by powerful states are still the primary threat the narrow reading guards against. Western military establishments and a substantial body of international law scholarship (writing independently of any state's direct benefit) attest the founding problem has been overtaken by transnational non-state violence the drafters did not anticipate, making rigid attribution requirements a mismatch to the current threat landscape rather than a live safeguard against the original 1945 problem.
narrative_ontology:disappearance_verdict(article_51_self_defense__narrow_armed_attack_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__narrow_armed_attack_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__narrow_armed_attack_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__narrow_armed_attack_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__narrow_armed_attack_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).
:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects moderate but real constraint-driven cost imposed on states that face genuine non-state armed threats but cannot satisfy the attribution requirement — they either forgo action, act unlawfully, or route through a Security Council that is frequently deadlocked by veto. Suppression (0.55) is substantial because the reading depends on active doctrinal defense — ICJ opinions, General Assembly resolutions, diplomatic protest, and scholarly consensus-building — against a competing practice trend; without that active maintenance the reading would likely erode faster than it has. Resistance (0.72) is high: this is among the most contested doctrines in international law, actively resisted by powerful states in both rhetoric and practice. Theater ratio (0.28) is moderate-low: the doctrinal machinery (ICJ opinions, Article 51 notification letters to the Security Council) still does real interpretive work, though an increasing share of state Article 51 notifications now stretch the narrow reading's language while nominally paying it lip service — a mild but real theatrical component.
 *
 * DIRECTIONALITY LOGIC:
 *   Weaker states, the Security Council, and the ICJ are structural beneficiaries: the narrow reading directly protects the former from being targeted under expansive threat narratives and directly vindicates the institutional authority of the latter two. Great-power militaries and states bordering weak or complicit host states are the structural targets: the reading imposes a real cost on their operational flexibility precisely because they possess the capability to act preemptively or cross-border and are the ones most often prevented from doing so lawfully. Civilian populations under non-state armed group violence are neither beneficiaries nor targets in the doctrinal sense — they are excluded from the framing entirely, bearing costs regardless of which reading prevails.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing self-judging unilateral force after two world wars) remains partially live — self-judging invocations by powerful states remain a real risk the narrow reading guards against — which is why founding_problem_status is authored as contested rather than dead. This prevents the classification from either (a) treating the narrow reading as pure obsolete formalism that should simply yield to operational necessity claims, or (b) treating it as a settled Mountain immune from the genuine tension that transnational non-state violence creates. The tangled_rope classification captures both halves: real coordination function (channeling force decisions through collective institutions, preventing pretextual wars) and real asymmetric extraction (imposing disproportionate operational cost on states facing genuine, time-sensitive non-state threats).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    narrow_reading_kernel_committer_structure,
    'This story instantiates the narrow_armed_attack_reading of the article_51_self_defense kernel. The sibling readings — expansive_preventive_reading and unable_unwilling_doctrine_reading — would each restructure who bears the constraint''s cost. Which reading actually governs state practice, and is that convergence point moving?',
    'Track state practice and opinio juris over time: count Article 51 notification letters to the Security Council that explicitly or implicitly invoke unable/unwilling or preventive standards versus those adhering strictly to attributable-armed-attack; a rising share of the former indicates customary law crystallizing away from this reading regardless of ICJ doctrine.',
    'If state practice has already crystallized around the unable/unwilling standard, this reading''s classification should be understood as describing an aspirational or minority-doctrinal position rather than the operative legal rule — its coordination function would then be largely rhetorical (raising its effective theater_ratio) even though its textual and judicial pedigree remains intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(narrow_reading_kernel_committer_structure, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; the choice of reading determines who is beneficiary and who is victim.').

omega_variable(
    attribution_standard_as_natural_or_constructed_barrier,
    'Is the ILC attribution standard (effective/overall control) an objectively correct legal threshold derived from state sovereignty principles, or is it a constructed choke point that happens to preserve Security Council and weaker-state authority by making self-defense claims against non-state threats structurally difficult to satisfy?',
    'Comparative analysis of whether attribution thresholds in other areas of international law (state responsibility, countermeasures) are set at comparably strict levels, and whether the strictness specifically in the self-defense context correlates with an institutional interest in Security Council centrality.',
    'If the attribution standard is shown to be calibrated specifically for the self-defense context in a way inconsistent with attribution doctrine elsewhere, this would support classifying the reading''s coordination story as partly a cover for preserving institutional and weaker-state prerogatives against operationally capable states — reinforcing the tangled_rope reading over a pure rope reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attribution_standard_as_natural_or_constructed_barrier, conceptual, 'Whether the doctrinal attribution threshold is principled or strategically calibrated.').

omega_variable(
    civilian_cost_externality,
    'Does the narrow reading, by denying a lawful self-defense trigger to states facing non-attributable non-state threats, indirectly increase harm to civilians in the affected border regions by pushing states toward either inaction (leaving populations exposed) or unlawful action (escalatory strikes conducted outside legal constraint and therefore less restrained)?',
    'Empirical comparison of civilian harm outcomes in cases where states acted under Security Council authorization versus unilateral unlawful action versus restraint, controlling for threat severity.',
    'If restraint correlates with worse civilian outcomes relative to authorized or even unlawful-but-constrained responses, the narrow reading''s coordination benefit (preventing pretextual wars) would need to be weighed against a real victim class (excluded civilian populations) currently outside the beneficiary/victim structural declarations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_cost_externality, empirical, 'Whether the reading''s restraint function externalizes cost onto civilians outside the interstate legal frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(arti_tr_t1970, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1970, 0.14).
narrative_ontology:measurement(arti_tr_t1990, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(arti_tr_t2001, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2001, 0.24).
narrative_ontology:measurement(arti_tr_t2014, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(arti_tr_t2025, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1945, 0.2).
narrative_ontology:measurement(arti_be_t1970, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(arti_be_t1990, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(arti_be_t2001, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2001, 0.38).
narrative_ontology:measurement(arti_be_t2014, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2014, 0.4).
narrative_ontology:measurement(arti_be_t2025, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(arti_su_t1970, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(arti_su_t1990, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(arti_su_t2001, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2001, 0.5).
narrative_ontology:measurement(arti_su_t2014, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2014, 0.53).
narrative_ontology:measurement(arti_su_t2025, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% Part of the article_51_self_defense kernel family (3 readings). narrow_armed_attack_reading is the textually orthodox, judicially dominant reading (ICJ jurisprudence) with lower ε (0.42) reflecting its coordination-heavy function of channeling force through collective institutions. expansive_preventive_reading and unable_unwilling_doctrine_reading are separate constraint files with their own ε values, beneficiary/victim structures, and classifications — they are NOT alternative measurements of this same constraint but structurally distinct claims about when Article 51 is triggered, per the ε-invariance principle. This reading tends to constrain the sibling readings' legitimacy conditions (an influences relationship) rather than logically foreclosing them, since all three remain live positions in actual diplomatic and judicial practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
