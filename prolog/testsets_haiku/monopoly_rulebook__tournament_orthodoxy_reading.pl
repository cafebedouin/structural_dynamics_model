% ============================================================================
% CONSTRAINT STORY: monopoly_rulebook__tournament_orthodoxy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: monopoly_rulebook__tournament_orthodoxy_reading
 *   human_readable: Monopoly Rulebook as Tournament Standard (Orthodoxy Reading)
 *   domain: game_theory/social_coordination
 *
 * SUMMARY:
 *   The Monopoly rulebook, treated by competitive communities as an immutable
 *   standard for tournament play, instantiates a coordination mechanism:
 *   players rank themselves and compare skill against a common text that
 *   tournament organizers enforce as authoritative. This is the orthodoxy
 *   reading of the contested kernel. The rulebook is a rope constraint from
 *   the competitive community's perspective — it solves the collective-action
 *   problem of rules fragmentation and enables meaningful ranking. This
 *   reading rejects pedagogical and social-correction interpretations that
 *   treat house rules as legitimate improvements; from the orthodoxy frame,
 *   house rules are noise obscuring skill-based competitive depth. The claim
 *   and metrics are aligned here (both claim rope, metrics support low
 *   extraction and real coordination), because the reading's own epistemic
 *   frame treats the rulebook as genuinely solving a coordination problem
 *   without extractive layering.
 *
 * KEY AGENTS:
 *   - competitive_community: beneficiaries of the shared standard; voluntary participants whose coordination depends on text immutability
 *   - tournament_organizers: agenda-setters maintaining text authority; institutions enforcing the rulebook in official play
 *   - casual_players: excluded from the competitive frame; adopt house rules outside the standard
 *   - pedagogical_reformers: excluded from the competitive frame; treat the rulebook as a means to teach social lessons
 *   - game_theorists: analytical observers measuring whether the reading's skill-isolation claim holds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.12).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook as Tournament Standard (Orthodoxy Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "game_theory/social_coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, 'c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356').
narrative_ontology:cs_kernel_codification('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', fixed_text).
narrative_ontology:cs_authority_grounding('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', expertise).
narrative_ontology:cs_interpretation_layer_present('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356').
narrative_ontology:cs_reading_relation('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', foundational, text_authority_enables_fair_ranking).
narrative_ontology:cs_axiom_status(text_authority_enables_fair_ranking, holdable).
narrative_ontology:cs_axiom_grounding('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', text_authority_enables_fair_ranking, instrumental).
narrative_ontology:cs_axiom('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', foundational, skill_isolation_requires_immutable_standard).
narrative_ontology:cs_axiom_status(skill_isolation_requires_immutable_standard, holdable).
narrative_ontology:cs_axiom_grounding('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', skill_isolation_requires_immutable_standard, empirically_contingent).
narrative_ontology:cs_reference_frame('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', unified_competitive_standard).
narrative_ontology:cs_drift_state('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', contemporary_house_rule_adoption, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('c4a0f6c5-d2e7-41a9-8eeb-8f79364d1356', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Players who adopt the rulebook standard as the legitimate framework for skill-based competition. They benefit from a shared, immutable text that enables ranking, comparison across games, and recognition of strategic excellence. The rulebook gives them a common language for adjudicating disputes and comparing their play against others' — the coordination function that grounds the constraint.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community, beneficiary,
    organized, generational, mobile, global).

% Institutions (chess federations, esports leagues, game clubs) that publish and enforce the rulebook as the standard for official play. They maintain text authority by treating the rulebook as immutable for ranking purposes and refusing to recognize modified variants in sanctioned tournaments. Their legitimacy rests on their commitment to the immutability principle.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, generational, mobile, global).

% Players who adopt house rules to reshape the game's social dynamics — to slow elimination, inject luck, or redistribute advantage toward newer players. From the orthodoxy reading's frame, they are not part of the competitive community and their rule modifications are invisible to official ranking; their absence from the constraint's legitimacy structure means they could object but are not counted.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, excluded,
    moderate, biographical, mobile, local).

% Educators and game designers who argue the rulebook should be modified to teach specific lessons (cooperation, resource sharing, resilience against luck). From the orthodoxy reading, they are excluded because they treat the rulebook as a means to an educational end, not as a standard for competitive skill measurement. They would reject text immutability as a principle.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, pedagogical_reformers, excluded,
    moderate, generational, mobile, regional).

% Analysts who study whether the rulebook actually achieves what the orthodoxy reading claims (whether it truly isolates strategic skill, whether eliminations are deserved by the framework's own logic, whether the immutability principle is coherent). They observe the constraint from outside it and can measure whether the reading's core claims about skill and fairness hold.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, game_theorists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, immutable text as the legitimate frame for competitive play, enabling players to rank themselves against others using a shared standard and to recognize skill-based excellence. Solves the collective-action problem of rules fragmentation: without a standard, every game would require separate rules negotiation, and comparative ranking across games would be impossible.
% TRANSFER_FUNCTION: Transfers the authority to define legitimate play from ad hoc group agreement to a published rulebook maintained by an institutional authority. This centralizes rule interpretation and makes it binding for official competitive recognition.
% ABSENT_VOICES: Casual players and pedagogical reformers who adopt house rules are structurally excluded from the competitive-community frame — they are not consulted in setting the standard and their modifications are treated as noise, not innovations.
% DISAPPEARANCE_RATIONALE: If the rulebook standard disappeared and tournament organizers stopped enforcing text authority, competitive ranking would collapse — there would be no basis for comparing players across different games or rule variants, and each community would fragment into house-rule clusters with incomparable skill measures.
% FOUNDING_PROBLEM: How to establish a fair, skill-based competitive framework where outcomes reflect player excellence rather than luck or rule variance, and where one player's rank can be meaningfully compared to another's across different games and regions.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers and competitive players attest the problem is live and the rulebook solution works — ranking systems and world championship structures depend on text authority. Game theorists independently document that shared rule standards are empirically necessary for rank coherence. Pedagogical reformers attest the problem is partly solved but the immutability principle sacrifices social resilience; this attestation is from outside the beneficiary set (they reject the framework's core premise).
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(monopoly_rulebook__tournament_orthodoxy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 0.08, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is very low (0.08) because the rulebook itself collects no rents and no party bears structural costs relative to benefits — participation is voluntary, and the shared standard is the public good all beneficiaries value. Suppression is minimal (0.12) because the constraint's persistence does not depend on coercing participation; competitive communities adopt it because it enables ranking and recognition they want. Theater is very low (0.05): the rulebook functions primarily as a coordination text, not as performative authority. Accessibility collapse is very high (0.92) because once a player understands the competitive frame, alternatives (playing by different rules, adopting house rules) become structurally invisible to official ranking — the collapse is epistemic, not coercive. The measurement series shows stability across the interval: this reading posits the rulebook as a genuine standard that has sustained the competitive community's coordination without need for intensifying enforcement or theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the competitive community's seat, the rulebook is a genuine coordination success — it enables ranking, recognition, and skill-based reputation. From the pedagogical reformer's seat, the same rulebook is an obstacle to teaching and social resilience. From the casual player's seat, it is a constraint on how they can modify the game for their social needs. The orthodoxy reading brackets these other perspectives as outside the competitive frame; they are not absent voices in the decision to maintain text authority, they are structurally excluded by the reading's own epistemic boundaries. The engine computes per-seat classifications from power and exit options; the reading's boundary-drawing is captured in the stakeholder roles (beneficiary vs. excluded) and the commentary's framing.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive community sits at the beneficiary end of directionality (d ≈ 0.1): they benefit from the shared standard, have mobile exit options (they can play by other rules if they choose), and participate voluntarily. Tournament organizers sit near symmetric (d ≈ 0.5): they derive status and authority from enforcing the standard, but they also bear the cost of maintaining institutional commitment to immutability and resolving disputes. Casual players and pedagogical reformers are excluded rather than coordinated — their exclusion is not extracted from them, but it means they do not feed into the beneficiary directionality derivation. The lack of a victim set is structurally significant: the reading posits no party that is harmed by the rulebook — only parties that choose to participate and parties that opt out.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling fair, skill-based competitive ranking across games) remains live under the orthodoxy reading. The rulebook standard continues to solve it — competitive ranking depends on text authority, and no alternative standard has superseded it in the competitive community. There is no mandatrophy here; the founding problem has not died and the constraint has not become zombie machinery. However, the existence of the excluded voices (pedagogical reformers, casual players) and the sibling readings suggests the constraint is subject to contest. Mandatrophy resolution in the network comes through the competing readings: if the extraction_demo or social_scaffold readings gain institutional adoption, the orthodoxy reading's founding problem diagnosis would be called into question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_isolation_empirical_claim,
    'Does the rulebook actually isolate strategic skill from luck and rule-variance effects, or do skill and luck remain entangled in outcome variance?',
    'Game-theoretic analysis and simulation of outcomes under the rulebook vs. alternative rule sets; empirical data from competitive play comparing ranking coherence across variants; statistical decomposition of outcome variance into skill, luck, and rule-interaction terms.',
    'If skill isolation fails empirically, the orthodoxy reading''s core claim (that text authority serves competitive fairness) is undermined, and the rulebook becomes an arbitrary standard whose immutability serves institutional authority rather than competitive depth. This would support the extraction reading''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_isolation_empirical_claim, empirical, 'Whether the rulebook achieves its core epistemic claim that text authority isolates strategic skill.').

omega_variable(
    immutability_vs_reform_boundary,
    'Is the principle of text immutability for ranking purposes separable from the rulebook''s competitive coordination function, or are they structurally entangled?',
    'Comparative analysis of rule-evolution practices in competitive communities (chess, esports, card games): do communities that allow rule amendment while maintaining ranking coherence experience coordination collapse, or do they sustain both? Do amended rules enable broader participation without degrading skill measurement?',
    'If immutability is separable from coordination, then the competitive community could benefit from rule reform (house-rule innovations, accessibility amendments) without losing ranking validity — the orthodoxy reading''s demand for absolute immutability would be revealed as a contingent institutional choice, not a structural necessity. This would undermine the reading''s claim that alternatives are noise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immutability_vs_reform_boundary, empirical, 'Whether text immutability is structurally necessary or contingent to competitive coordination.').

omega_variable(
    contested_kernel_boundary,
    'Is the reading''s boundary between the competitive frame (beneficiary) and the social/pedagogical frame (excluded) a principled distinction or an arbitrary institutional gatekeeping choice?',
    'Ethnographic and institutional analysis: do tournament organizers and competitive players articulate principled reasons for excluding pedagogical and social readings, or do they enforce exclusion for institutional convenience? Are there coherent hybrid communities that treat competitive and social goals as compatible within the rulebook?',
    'If the boundary is arbitrary, the excluded stakeholders (casual players, pedagogical reformers) have a legitimate claim to be present in the constraint''s authority structure, and the reading misrepresents them as outside rather than suppressed. This would shift the reading toward the social_scaffold framing (house rules as legitimate correction, not noise) or the extraction reading (institutional gatekeeping as rent-protection).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_kernel_boundary, conceptual, 'Whether the orthodoxy reading''s boundary between competitive and social frames is principled or institutional gatekeeping.').

omega_variable(
    reading_family_kernel_identity,
    'Do all three readings (orthodoxy, extraction, social-scaffold) address the same constraint, or has the reading divergence produced three structurally distinct constraints that merely share a name?',
    'Apply the ε-invariance principle: if changing the reading changes epsilon by more than can be attributed to directionality scaling, the readings are authoring different constraints. Verify whether stakeholder structures, founding problems, and beneficiary/victim declarations remain coherent across readings when the same observables are used.',
    'If readings produce structurally distinct constraints (different epsilon, different victim sets, different founding problems), then the network should decompose the kernel into separate stories per ε-invariance rule. If readings are coherent variations of one constraint authoring different d values per seat, the readings remain siblings in the kernel family. This distinction affects how the corpus treats reading families: decomposed stories get separate JSON files linked via affects_constraints; contested readings of one constraint share one kernel entry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_family_kernel_identity, conceptual, 'Whether the three readings represent different lenses on one constraint or fundamentally different constraints using a shared name.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(mono_tr_t0, observed).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement_basis(mono_tr_t10, observed).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(mono_tr_t20, observed).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement_basis(mono_tr_t30, observed).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement_basis(mono_tr_t40, observed).
narrative_ontology:measurement(mono_tr_t50, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 50, 0.05).
narrative_ontology:measurement_basis(mono_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.07).
narrative_ontology:measurement_basis(mono_be_t0, observed).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.07).
narrative_ontology:measurement_basis(mono_be_t10, observed).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement_basis(mono_be_t20, observed).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement_basis(mono_be_t30, observed).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement_basis(mono_be_t40, observed).
narrative_ontology:measurement(mono_be_t50, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 50, 0.08).
narrative_ontology:measurement_basis(mono_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(mono_su_t0, observed).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.1).
narrative_ontology:measurement_basis(mono_su_t10, observed).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.11).
narrative_ontology:measurement_basis(mono_su_t20, observed).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.12).
narrative_ontology:measurement_basis(mono_su_t30, observed).
narrative_ontology:measurement(mono_su_t40, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(mono_su_t40, observed).
narrative_ontology:measurement(mono_su_t50, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 50, 0.12).
narrative_ontology:measurement_basis(mono_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.03).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the monopoly_rulebook kernel. The extraction_demo_reading authors much higher epsilon (~0.65) and a snare classification; the social_scaffold_reading authors moderate epsilon (~0.38) and a scaffold-hybrid classification with house rules as essential correction. All three share the kernel (the Monopoly rulebook text) but author irreconcilable epistemic frames. The orthodoxy reading treats the rulebook as a genuine competitive standard; the extraction reading treats it as a demonstration of wealth concentration; the social-scaffold reading treats it as requiring community amendment to be socially playable. Each reading author their own stakeholder structures, founding problems, and epsilon values independently per ε-invariance rule. The readings do not converge or disagree on metrics — they instantiate different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
