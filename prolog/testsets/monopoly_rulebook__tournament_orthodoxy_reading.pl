% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_rulebook__tournament_orthodoxy_reading, []).

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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Monopoly Rulebook as Tournament Standard (Orthodoxy Reading)
 *   domain: game_theory/social_coordination
 *
 * SUMMARY:
 *   The tournament orthodoxy reading frames the Monopoly rulebook as a
 *   genuine coordination mechanism: a shared standard that enables fair
 *   competitive play and reliable skill comparison. The rulebook imposes no
 *   extraction; participation is voluntary; the constraint's function is
 *   purely to solve the problem of rule ambiguity and local variation. The
 *   reading rejects pedagogical and social-correction interpretations as
 *   category errors—they treat the rulebook as if it were a teaching
 *   instrument or a social bond-preservation mechanism, when its actual
 *   function is competitive standardization. This is ONE reading of a
 *   contested kernel; the extraction_demo and social_scaffold readings
 *   instantiate different structural interpretations of the same rule text.
 *
 * KEY AGENTS:
 *   - competitive_gaming_community: voluntary participants in tournament play; benefit from standardized rules enabling fair ranking
 *   - tournament_organizers: stewards of rule text; maintain authority and enforce consistency; do not extract rents
 *   - casual_social_players: use house rules for social play; not subject to tournament rules; outside the coordination scope
 *   - pedagogical_reformers: excluded from rule-setting; argue rules produce bad learning outcomes; excluded voice
 *   - social_cohesion_advocates: excluded from rule-setting; argue rules damage relationships; excluded voice
 *   - rules_scholars: analytical observers; document variation and competitive consequences; neutral on legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.06).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.06).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook as Tournament Standard (Orthodoxy Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '62d9a859-a263-4aa8-9df5-97af6bc1665c').
narrative_ontology:cs_kernel_codification('62d9a859-a263-4aa8-9df5-97af6bc1665c', fixed_text).
narrative_ontology:cs_authority_grounding('62d9a859-a263-4aa8-9df5-97af6bc1665c', lineage).
narrative_ontology:cs_interpretation_layer_present('62d9a859-a263-4aa8-9df5-97af6bc1665c').
narrative_ontology:cs_reading_relation('62d9a859-a263-4aa8-9df5-97af6bc1665c', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('62d9a859-a263-4aa8-9df5-97af6bc1665c', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('62d9a859-a263-4aa8-9df5-97af6bc1665c', foundational, text_authority_enables_fair_competition).
narrative_ontology:cs_axiom_status(text_authority_enables_fair_competition, holdable).
narrative_ontology:cs_axiom_grounding('62d9a859-a263-4aa8-9df5-97af6bc1665c', text_authority_enables_fair_competition, instrumental).
narrative_ontology:cs_axiom('62d9a859-a263-4aa8-9df5-97af6bc1665c', foundational, rule_clarity_is_primary_function).
narrative_ontology:cs_axiom_status(rule_clarity_is_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('62d9a859-a263-4aa8-9df5-97af6bc1665c', rule_clarity_is_primary_function, deontological).
narrative_ontology:cs_reference_frame('62d9a859-a263-4aa8-9df5-97af6bc1665c', published_immutable_standard).
narrative_ontology:cs_drift_state('62d9a859-a263-4aa8-9df5-97af6bc1665c', contemporary_multiverse_play, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('62d9a859-a263-4aa8-9df5-97af6bc1665c', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_gaming_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).
:- end_tests(monopoly_rulebook__tournament_orthodoxy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is minimal (0.06) because the rulebook does not systematically transfer resources from one group to another. Competitive players consent to the standard voluntarily; they benefit from text authority because it enables fair comparison. Theater is negligible (0.08) because the constraint's function is purely administrative coordination, not performance masking degraded function. Suppression is near-zero (0.02) because the constraint does not require coercive enforcement; tournament organizers manage rule disputes, but players generally accept authority as legitimate and necessary. The trajectory is flat across the interval because the tournament orthodoxy reading treats text authority as stable and functional—no accumulating extraction, no performative drift, no enforcement decay. The constraint operates as intended throughout.
 *
 * PERSPECTIVAL GAP:
 *   Tournament organizers and competitive players converge on a rope classification: shared standard, voluntary participation, mutual benefit. Pedagogical reformers diverge sharply, reading the constraint as extractive (capital demonstration) or scaffolding (temporally justified correction). This divergence is not a measurement error; it is a reading-specific delta. The tournament orthodoxy reading commits to the rope frame and structures the metrics accordingly. The other readings, in their own JSON files, will author different epsilon values and different stakeholder structures.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive gaming community is the beneficiary (d near 0.0): they benefit from standardized rules without bearing extraction costs. Tournament organizers sit near symmetric (d ≈ 0.5): they administer the rules and bear the administrative burden, but they also benefit from the coordination—their authority and the community's trust are symbiotic. Casual social players and excluded reformers are not targets of the constraint (they operate outside it), so directionality is not applicable. The low extractiveness feeds low effective extraction even at institutional power levels because there is no asymmetry between beneficiaries and payers—all competitive participants are beneficiaries of the shared standard.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rule ambiguity and local variation) remains live under the tournament orthodoxy reading. Pedagogical reformers and social advocates dispute this, arguing the problem has been solved and the rulebook persists for reasons unrelated to its founding function. From the tournament orthodoxy seat, this disagreement is a category error: pedagogical and social concerns are orthogonal to competitive standardization. The constraint does not suffer mandatrophy because its founding function (enabling fair competitive ranking) remains primary and functional. A mandatrophy reading would argue that the rulebook now persists mostly as institutional theater (high theater_ratio), with the founding coordination problem either solved or displaced—the social_scaffold reading or extraction_demo reading may author that pattern, but the tournament orthodoxy reading does not.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_kernel_reading_ambiguity,
    'Is the Monopoly rulebook''s primary function competitive standardization (tournament orthodoxy reading) or is it an implicit pedagogical device (extraction demo reading) or a constraint requiring social correction (social scaffold reading)? Can a single text bear all three functions simultaneously, or does adopting one frame necessarily exclude the others?',
    'Genealogical investigation: track the rulebook''s stated purpose (competitive fairness vs. teaching capital dynamics vs. preserving group play) through publisher documentation, designer intent statements, and community evolution. Compare how competitive players, pedagogical users, and social players actually deploy the rule text—does the text function differently in each context, or do different communities impose their reading onto the same functional mechanism?',
    'If the rulebook''s primary function is competitive standardization, the tournament orthodoxy reading is correct and epsilon remains minimal (~0.06). If the function is primarily pedagogical demonstration, the extraction_demo reading is correct and epsilon rises substantially. If the function is primarily social constraint, the social_scaffold reading is correct and beneficiaries shift to include social players. The three readings rest on different claims about what the rule text actually does in the world.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_kernel_reading_ambiguity, conceptual, 'Whether the Monopoly rulebook''s function is competitive standardization, pedagogical demonstration, or social constraint—or whether all three functions coexist in an irreducibly ambiguous way.').

omega_variable(
    omega_text_authority_legitimacy,
    'Is text authority (treating the published rulebook as immutable and authoritative) inherently legitimate, or does legitimacy depend on the rulebook''s content? If the text were changed to include explicitly predatory mechanics (e.g., ''the richest player may seize properties from the poorest''), would text authority persist?',
    'Counterfactual stress test: measure community response to proposed rule changes that would make the game more harsh or more corrupt. Track whether players resist the change because they value text authority intrinsically or because they judge the content unacceptable. If players accept a changed-text standard without loss of tournament legitimacy, text authority is intrinsically valued. If players resist the change to preserve content, legitimacy is content-dependent.',
    'If text authority is intrinsically valued, the tournament orthodoxy reading''s commitment to immutability is correct and the constraint''s function is purely coordination. If legitimacy is content-dependent, the reading overstates text authority and smuggles in substantive claims about what rules are acceptable—which opens space for the other readings to contest the reading''s claim to neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(omega_text_authority_legitimacy, empirical, 'Whether text authority is intrinsically legitimate or legitimacy depends on rule content.').

omega_variable(
    omega_extraction_layering,
    'Does the tournament orthodoxy reading''s claim of zero extraction depend on ignoring the ways text authority can be weaponized? If tournament organizers use rule authority to defend rule changes that benefit certain player types or venues (e.g., speed-play tournaments favor aggressive players), is that a form of structural extraction, or is it merely a side effect of standardization?',
    'Audit tournament rule changes and clarifications over time: measure whether changes systematically favor certain player types, strategies, or venues. Compare the impact on competitive balance, demographic participation, and resource flows. If changes show systematic asymmetry, assess whether organizers are aware of the asymmetry and whether they justify it as a feature or defend themselves against the charge.',
    'If systematic asymmetry is found and organizers do not acknowledge it, the tournament orthodoxy reading may understate extraction by treating rule authority as neutral and purely coordinative. This would support a hybrid classification (weakly tangled rope: real coordination plus incidental asymmetric effects), not a pure rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(omega_extraction_layering, empirical, 'Whether text authority''s implementation introduces systematic asymmetric effects that constitute a form of structural extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(mono_tr_t5, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 5, 0.08).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(mono_tr_t15, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 15, 0.08).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(mono_tr_t25, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.08).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(mono_be_t5, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 5, 0.05).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.06).
narrative_ontology:measurement(mono_be_t15, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 15, 0.06).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(mono_be_t25, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 25, 0.06).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.06).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.06).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(mono_su_t5, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 5, 0.02).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.02).
narrative_ontology:measurement(mono_su_t15, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 15, 0.02).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.02).
narrative_ontology:measurement(mono_su_t25, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.02).
narrative_ontology:measurement(mono_su_t40, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 40, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% The Monopoly rulebook kernel is interpreted differently by three constraint readings. The tournament_orthodoxy_reading treats the text as a genuine coordination mechanism for competitive play, with minimal extraction. The extraction_demo_reading interprets the same text as an implicit pedagogical demonstration of capitalist accumulation, with substantial extraction. The social_scaffold_reading interprets the text as a constraint requiring community correction through house rules to preserve social cohesion. All three readings operate on the same rule text, but their epsilon values and structural interpretations differ substantially. The readings coexist in public discourse and community practice; competitive players adopt orthodoxy, pedagogical users adopt extraction framing, and social players adopt scaffolding. No single reading foreclosed the others in principle, but each reading's commitments constrain how the others can be coherently held within the same framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
