% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope – Hybrid Proportionality Reading
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   The Geneva Conventions establish humanitarian protections for armed
 *   conflict, but the scope and stringency of those protections depends on
 *   whether a conflict is classified as international (Protocol I) or
 *   non-international (Protocol II/Common Article 3). Under the hybrid
 *   proportionality reading instantiated here, the Conventions' protective
 *   scope is determined by conflict type classification and constrained by
 *   proportionality analysis, which allows stronger parties to justify
 *   attacks on protected persons or objects when military advantage is deemed
 *   sufficient. This reading vindicates the proportionality principle as
 *   determinative and conflict-type classification as the protective gateway
 *   — both of which create interpretive ambiguity that favors institutional
 *   actors with legal capacity to navigate it. The reading differs from the
 *   state-centric reading (which treats Geneva protections as applying only
 *   to recognized uniformed combatants under responsible command) and the
 *   universal rights reading (which extends protections to all persons
 *   affected by conflict regardless of combatant status or conflict
 *   classification).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.76).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope – Hybrid Proportionality Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/humanitarian").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'bfda4971-8d77-4307-8b3d-01df6ae1fd8e').
narrative_ontology:cs_kernel_codification('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', fixed_text).
narrative_ontology:cs_authority_grounding('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', lineage).
narrative_ontology:cs_interpretation_layer_present('bfda4971-8d77-4307-8b3d-01df6ae1fd8e').
narrative_ontology:cs_reading_relation('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', foundational, protective_scope_scales_by_conflict_type).
narrative_ontology:cs_axiom_status(protective_scope_scales_by_conflict_type, holdable).
narrative_ontology:cs_axiom_grounding('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', protective_scope_scales_by_conflict_type, conventional).
narrative_ontology:cs_axiom('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', foundational, proportionality_principle_overrides_categorical_protections).
narrative_ontology:cs_axiom_status(proportionality_principle_overrides_categorical_protections, holdable).
narrative_ontology:cs_axiom_grounding('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', proportionality_principle_overrides_categorical_protections, deontological).
narrative_ontology:cs_reference_frame('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', protocol_one_universal_humanitarian_restraint).
narrative_ontology:cs_drift_state('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', contemporary_strategic_interpretation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bfda4971-8d77-4307-8b3d-01df6ae1fd8e', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_capacity).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_infrastructure_interpreters).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, unclassified_combatants).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_near_conflict).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, insurgent_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_near_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the legal infrastructure, trained judge advocate corps, and institutional capacity to navigate and invoke the hybrid proportionality framework. Can choose to classify conflicts under AP I or AP II depending on strategic and diplomatic interests. Benefit from ambiguity: when a conflict is framed as 'non-international' under this reading, fewer restraints apply to state conduct; when international, they can claim AP I compliance while interpreting proportionality generously. Control enforcement interpretation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_capacity, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_capacity, beneficiary).

% Lack the institutional apparatus to reliably determine their own protective status under this reading. May be combatants in a conflict classified as non-international under AP II, receiving fewer humanitarian protections than they would under AP I, with no voice in that classification. Subject to summary treatment if classification is ambiguous or contested. Their protective status depends on institutional decisions made by parties with asymmetric power.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, unclassified_combatants, payer,
    powerless, immediate, trapped, local).

% Receive protection under Common Article 3 (non-international) or broader AP I protections (international) depending on how the conflict is classified. The hybrid proportionality reading makes their protection contingent on a classification decision over which they have no control. Proportionality analysis can justify attacks on civilian targets if military advantage is deemed sufficient, creating ambiguity about their protected status. Constrained by geography; cannot exit the conflict zone easily.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_near_conflict, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilian_populations_near_conflict, beneficiary).

% International courts, legal scholars, military lawyers, and humanitarian organizations interpret and apply the Geneva framework. Benefit from the interpretive work required by the hybrid proportionality reading: they become gatekeepers of protective scope determinations. Their interpretations carry authority without direct accountability to those affected. Can shift between readings depending on institutional context and constituency.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, legal_infrastructure_interpreters, beneficiary,
    organized, generational, mobile, global).

% Rarely have the institutional capacity to invoke Geneva protections effectively, even when the hybrid proportionality reading might theoretically afford them. Classification of their conflict as non-international often leaves them with fewer explicit protections. Cannot exit their movement without abandoning their cause. Depend on external recognition and interpretation by legal infrastructure for any protective coverage. Subject to characterization as terrorists or unlawful combatants, which can trigger exclusion from the protective scope entirely.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, insurgent_movements, payer,
    moderate, biographical, identity_locked, regional).

% Monitor compliance, document violations, and advocate for consistent interpretation of Geneva protections. Can leverage the hybrid proportionality reading's ambiguities to call for more protective interpretation, but lack enforcement authority. Operate from outside the conflict but interpret the framework for public accountability. Neutral in principle but functionally constrained by their access to all parties.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_humanitarian_organizations, observer,
    organized, generational, mobile, global).

% International courts, UN bodies, and expert legal panels determine whether a conflict is international or non-international, triggering different protective standards under the hybrid reading. This determination is often contested and politically freighted. The authority is distributed across multiple institutions without clear hierarchy, creating multiple competing interpretations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, conflict_classification_authority, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, state_militaries_with_capacity).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified normative framework for humanitarian protection in armed conflicts by calibrating protections to conflict type (international vs. non-international) and applying proportionality analysis to determine the scope of protected persons and objects. Solves the problem of how to apply consistent humanitarian restraint across diverse conflict scenarios without a single enforcing authority.
% TRANSFER_FUNCTION: Transfers interpretive authority from individual states to a distributed legal infrastructure (international courts, legal scholars, military justice systems). The stronger interpretation-position (institutional capacity to invoke Geneva reading favorably) flows toward state militaries and legal interpreters; the weaker position (vulnerability to protective scope ambiguity) falls on unclassified combatants, civilian populations, and insurgent movements.
% ABSENT_VOICES: Persons who will be affected by conflict classification decisions but lack any role in making those decisions: civilians in potential conflict zones, insurgent movements with limited institutional voice, detained persons awaiting classification determination. These actors are structurally excluded from the classification process that determines their protective status.
% DISAPPEARANCE_RATIONALE: If the hybrid proportionality framework vanished, states would lose a primary mechanism for calibrating their humanitarian obligations to conflict circumstances, and the protective scope would either revert to simpler categorical protections (all combatants receive uniform treatment) or fragment into purely state-defined standards. Legal ambiguity as a tool for selective application would disappear.
% FOUNDING_PROBLEM: Early Geneva Conventions treated all armed conflicts identically, creating implementation barriers when conflicts ranged from interstate wars to internal insurgencies. The founding problem was how to apply humanitarian restraint proportionally to different conflict types without a single authority to enforce uniform standards.
% FOUNDING_PROBLEM_CORROBORATION: State militaries and legal scholars attest the founding problem remains live: different conflict types require calibrated responses. Humanitarian organizations and courts attest the founding problem is substantially solved by Protocol I and II but the hybrid proportionality reading allows states to reverse-engineer ambiguous classifications for strategic advantage, suggesting the protective function has shifted into a rent-extraction mechanism. Legislative history from the Additional Protocols Diplomatic Conference (1974-1977) and subsequent case law from international courts document this shift.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.42 to 0.68 over the interval because the hybrid proportionality framework's ambiguities about protective scope accumulate over time: each time a court applies proportionality analysis to justify an attack on a protected person or object, the extractive potential of the reading increases (the framework is used to override protective status). Suppression is high (0.76) because the framework's complexity and the interpretive authority it concentrates in institutional actors make it difficult for weaker parties (unclassified combatants, insurgent movements, civilians) to challenge protective-scope determinations. Theater ratio rises from 0.18 to 0.42 because the performative aspect of the framework increases: states increasingly invoke proportionality analysis to justify conduct that would otherwise be prohibited, creating the appearance of humanitarian reasoning while the protective function erodes. The measurement series on a single shared time grid shows the constraint evolving from genuine coordination (early interval, lower extractiveness) to rent extraction via legal ambiguity (late interval, higher extractiveness and theater).
 *
 * PERSPECTIVAL GAP:
 *   From the seat of state militaries and legal interpreters, the hybrid proportionality reading provides essential flexibility: it allows them to calibrate humanitarian obligations to conflict circumstances and invoke proportionality as a principled framework for necessary military action. From the seat of unclassified combatants and civilian populations, the same framework is opaque and extractive: they cannot predict whether they will receive AP I or AP II protections, cannot challenge classification decisions, and face summary treatment if their status is ambiguous. The engine computes this divergence from power (institutional vs. powerless), exit options (arbitrage vs. trapped/identity-locked), and the beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   State militaries with institutional legal capacity are structural beneficiaries (d near 0.0): they control conflict classification, can invoke proportionality favorably, and set the interpretive agenda. Legal infrastructure interpreters are secondary beneficiaries (d near 0.15): they gain professional authority and interpretive power from the framework's ambiguity. Unclassified combatants and civilian populations are targets (d near 0.9): they lack institutional capacity, face protective-scope ambiguity, and bear the consequences of proportionality calculations. Insurgent movements sit higher on the target end (d near 0.85) because their identity-lock to their cause prevents exit and their moderate power is insufficient to leverage the legal infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem — how to apply humanitarian restraint proportionally to different conflict types — remains live, but the framework's protective function has substantially eroded as states and courts have learned to use proportionality analysis to justify constraints on protected persons and objects. This is not mandatrophy in the classical sense (function completely atrophied), but a functional drift from pure coordination (Geneva protections as universal restraint) toward hybrid extraction (Geneva protections as a framework for limiting humanitarian obligations). The theater ratio rise reflects this: performative humanitarian reasoning increasingly substitutes for protective substance. The tangled-rope classification (not snare) is warranted because genuine coordination remains: the framework does establish humanitarian norms that constrain some conduct, and the proportionality principle is not purely fabricated — but it is increasingly used as a tool to override protective status rather than to enhance it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conflict_classification_determinacy,
    'How objectively determinable is the boundary between international and non-international armed conflict under the hybrid proportionality reading?',
    'Systematic analysis of international court decisions and state practice: do courts consistently classify similar conflicts the same way, or does classification vary based on the parties'' institutional relationships and legal capacity?',
    'If classification is indeterminate, the hybrid reading confers discretionary power on institutional interpreters and allows stronger parties to choose classifications strategically. If classification is determinate, the extractiveness is lower and the framework is more genuinely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_determinacy, empirical, 'Whether conflict classification under the hybrid proportionality reading is deterministic or subject to strategic interpretation.').

omega_variable(
    proportionality_asymmetry,
    'Do stronger parties and weaker parties receive symmetrically applied proportionality constraints, or does proportionality analysis systematically justify constraints on weaker parties'' conduct while justifying stronger parties'' violations?',
    'Corpus analysis of prosecutions, court decisions, and documented violations: measure the frequency and severity of proportionality-based justifications for stronger vs. weaker parties'' conduct.',
    'Asymmetric application would establish the proportionality principle as a vehicle for extractive inequality. Symmetric application would support the genuine-coordination reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_asymmetry, empirical, 'Whether proportionality analysis is applied asymmetrically to stronger and weaker parties.').

omega_variable(
    kernel_reading_foreclosure,
    'Does the hybrid proportionality reading''s core premise (that protective scope scales by conflict type and proportionality) logically foreclose either the state-centric reading (protections limited to recognized combatants) or the universal rights reading (universal protections regardless of conflict type)?',
    'Jurisprudential analysis: can a single legal framework simultaneously hold that protections scale with conflict type (hybrid) AND that protections are universal and non-negotiable (universal)? Or is the foreclosure relation one of influence rather than logical contradiction?',
    'If the hybrid reading forecloses the universal reading, the constraint should be classified as excluding an alternative normative position from its framework. If the readings coexist, the constraint is a contestable interpretation rather than a structural requirement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether the hybrid proportionality reading logically forecloses the universal rights reading or merely influences its implementation.').

omega_variable(
    protective_scope_suppression_internalization,
    'Do weaker parties (unclassified combatants, insurgent movements) internalize the uncertainty about their protective status as legitimate or inevitable, or do they experience it as externally imposed suppression that would dissolve if the structural barriers were removed?',
    'Post-removal trajectory analysis: if legal frameworks were changed to provide categorical, non-negotiable protections, would insurgent movements and weaker parties rapidly adopt new behavioral patterns, indicating suppression was structural? Or would behavioral patterns persist, indicating suppression has become internalized?',
    'If suppression is internalized, the constraint''s effective suppression is higher than the structural measure suggests — weaker parties carry the suppression forward even if the framework changes. If suppression is purely structural, removing the framework would substantially reduce vulnerability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(protective_scope_suppression_internalization, empirical, 'Whether protective-status uncertainty is internalized by weaker parties or experienced as external suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(gene_tr_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 40, 0.67).
narrative_ontology:measurement(gene_be_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(gene_su_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 50, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.18).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% The Geneva Conventions' protective scope is a contested kernel. Three constraint stories instantiate three readings: the state-centric reading (protections apply to recognized uniformed combatants only), the universal rights reading (protections extend to all persons affected by conflict), and this hybrid proportionality reading (protections scale by conflict type and proportionality analysis). Each reading has distinct beneficiary/victim structures, beneficiaries, and ε values. The three readings coexist as live positions held by different institutional actors, courts, and states. Shared kernel: the text of the Geneva Conventions and Additional Protocols. Shared referent: the standing protective arrangements in armed conflicts. Different readings → different constraints → different classifications per-seat.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geneva_conventions_protective_scope__hybrid_proportionality_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
