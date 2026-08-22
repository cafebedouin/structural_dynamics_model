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
 *   human_readable: Monopoly Rulebook as Tournament Standard (Orthodox Reading)
 *   domain: social/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the tournament-orthodoxy reading of the
 *   Monopoly rulebook — the reading that treats the text as a legitimate,
 *   immutable competitive standard where outcomes reflect player skill under
 *   fixed rules. House rules and rule modifications are read as noise that
 *   obscures skill differentiation; the rulebook's value is precisely its
 *   stability and refusal to adapt to social outcomes. This is a pure
 *   coordination good (rope): the rulebook provides a shared frame enabling
 *   skill comparison and tournament legitimacy across contexts. The reading
 *   rejects both the pedagogical reading (which treats the game as a teaching
 *   tool for capitalism criticism) and the social-scaffold reading (which
 *   treats rule modifications as necessary for social playability). Under
 *   this reading, the rulebook is not extractive, coercive, or theatrical —
 *   it is a voluntary standard the competitive community chooses because it
 *   enables the comparison function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).
domain_priors:suppression_score(monopoly_rulebook__tournament_orthodoxy_reading, 0.05).
domain_priors:theater_ratio(monopoly_rulebook__tournament_orthodoxy_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(monopoly_rulebook__tournament_orthodoxy_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_rulebook__tournament_orthodoxy_reading, rope).
narrative_ontology:human_readable(monopoly_rulebook__tournament_orthodoxy_reading, "Monopoly Rulebook as Tournament Standard (Orthodox Reading)").
narrative_ontology:topic_domain(monopoly_rulebook__tournament_orthodoxy_reading, "social/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_rulebook__tournament_orthodoxy_reading, '546f8c7e-b8b7-48e3-bac6-67af379a02b5').
narrative_ontology:cs_kernel_codification('546f8c7e-b8b7-48e3-bac6-67af379a02b5', fixed_text).
narrative_ontology:cs_authority_grounding('546f8c7e-b8b7-48e3-bac6-67af379a02b5', practice).
narrative_ontology:cs_reading_relation('546f8c7e-b8b7-48e3-bac6-67af379a02b5', monopoly_rulebook__extraction_demo_reading, coexists_with).
narrative_ontology:cs_reading_relation('546f8c7e-b8b7-48e3-bac6-67af379a02b5', monopoly_rulebook__social_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('546f8c7e-b8b7-48e3-bac6-67af379a02b5', foundational, rulebook_immutable_standard).
narrative_ontology:cs_axiom_status(rulebook_immutable_standard, holdable).
narrative_ontology:cs_axiom_grounding('546f8c7e-b8b7-48e3-bac6-67af379a02b5', rulebook_immutable_standard, instrumental).
narrative_ontology:cs_axiom('546f8c7e-b8b7-48e3-bac6-67af379a02b5', foundational, skill_determination_requires_fixed_rules).
narrative_ontology:cs_axiom_status(skill_determination_requires_fixed_rules, holdable).
narrative_ontology:cs_axiom_grounding('546f8c7e-b8b7-48e3-bac6-67af379a02b5', skill_determination_requires_fixed_rules, empirically_contingent).
narrative_ontology:cs_reference_frame('546f8c7e-b8b7-48e3-bac6-67af379a02b5', competitive_standard_authority).
narrative_ontology:cs_drift_state('546f8c7e-b8b7-48e3-bac6-67af379a02b5', contemporary_casual_modification_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('546f8c7e-b8b7-48e3-bac6-67af379a02b5', '').
narrative_ontology:cs_kernel_id(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, casual_players).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, rulebook_publisher).
narrative_ontology:constraint_beneficiary(monopoly_rulebook__tournament_orthodoxy_reading, skill_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Players who engage with the rulebook as the authoritative competitive standard. They gain a shared frame for comparing skill, establishing tournaments, ranking players, and generating reproducible outcomes across contexts. The rulebook's immutability enables this comparison function: if the rules change between games, skill differentiation becomes noise. They choose to use the rulebook; nothing locks them in.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, competitive_community, beneficiary,
    organized, biographical, mobile, global).

% Maintain the rulebook's authority as the standard frame for official play. They set tournament rules by reference to the rulebook, resolve disputes using it as canonical text, and maintain ranking systems that assume rulebook consistency. They benefit from a stable reference point; they could choose alternative rule sets but opt for the standard because it maximizes participation and legitimacy.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, tournament_organizers, agenda_setter,
    institutional, generational, arbitrage, national).

% Play the game primarily for social coordination and fun. They benefit from knowing a common rulebook even if they modify it in house play. The standard exists whether they follow it; their choice to use it locally is voluntary and reversible. They can deviate freely without losing coordination with the rulebook standard.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, casual_players, beneficiary,
    moderate, immediate, mobile, local).

% Believe the rulebook's text produces socially harsh outcomes (prolonged player elimination, wealth concentration) and should be modified in house play. They are structurally excluded from the tournaments that privilege the rulebook standard. Their objection is to the rulebook's competitive purity; they would argue for rule variants but are not seated at tournament governance.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rule_modification_advocates, excluded,
    moderate, biographical, constrained, local).

% Read the rulebook's mechanics as demonstrating capitalism's structural dynamics (wealth concentration, elimination, asymmetric power accumulation). They see the rulebook as a teaching tool and argue it should be modified or replaced to teach alternative social arrangements. They are excluded from tournament governance and from the competitive framing that treats the rulebook as a standard rather than a pedagogical argument.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, pedagogical_critics, excluded,
    analytical, generational, analytical, global).

% Publishes and licenses the official rulebook text. They benefit from the immutability norm: if the rulebook could be revised freely, licensing authority would degrade and variants would proliferate. The tournament standard's legitimacy sustains their publishing business.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, rulebook_publisher, beneficiary,
    institutional, generational, arbitrage, global).

% Competitive players who have invested in rulebook expertise and use the standard to establish reputation and ranking. They benefit from the immutability norm because their accumulated skill stays valid across time and context. Rule changes would devalue their expertise; the fixed standard preserves it.
narrative_ontology:constraint_stakeholder(monopoly_rulebook__tournament_orthodoxy_reading, skill_community, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, shared, immutable competitive standard that enables skill comparison across players, contexts, and time. The rulebook solves the collective-action problem of agreeing on game mechanics: without a standard, every game group would negotiate rules separately, making tournament play and skill ranking impossible.
% TRANSFER_FUNCTION: No transfer: the rulebook is a coordination good (an information standard), not an extractive mechanism. Players gain access to shared competitive frames; nothing flows from losers to winners beyond what the game itself determines. The rulebook does not redistribute value — it defines how value is competed for.
% ABSENT_VOICES: Advocates for rule modification (house-rule proponents, pedagogical critics using the game to teach anti-capitalist dynamics) are structurally excluded from tournament governance. They would argue the rulebook produces socially undesirable outcomes and should be modified to prevent player elimination or wealth concentration. They are not seated at the table where tournament standards are set.
% DISAPPEARANCE_RATIONALE: If the tournament orthodoxy reading vanished and the rulebook lost its immutable standard authority, tournaments would fragment into house-rule variants, skill comparison would become context-dependent, and the competitive skill hierarchy that depends on rulebook consistency would collapse. Players could still play casually with modified rules, but the coordination function the standard provides would be gone.
% FOUNDING_PROBLEM: How can geographically dispersed players compete fairly and establish reproducible skill rankings without a shared rulebook? Early informal play featured rule negotiation at every table; tournaments require a standard reference point to make results comparable.
% FOUNDING_PROBLEM_CORROBORATION: Tournament organizers, competitive communities, and rulebook publishers all attest that the founding problem is live: tournaments depend on rulebook consistency. Independent gaming researchers and competitive gaming communities document that variance in rulebook adherence directly degrades ranking validity. The founding problem is corroborated from outside the publisher's beneficiary seat by the tournament infrastructure that depends on it.
narrative_ontology:disappearance_verdict(monopoly_rulebook__tournament_orthodoxy_reading, world_rearranges).
narrative_ontology:founding_problem_status(monopoly_rulebook__tournament_orthodoxy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(monopoly_rulebook__tournament_orthodoxy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.08 at interval end) because the rulebook functions as an information standard, not a mechanism that moves value from one party to another. No party is structurally harmed by the rulebook's immutability; all parties that adopt it do so voluntarily to gain the coordination benefit. Suppression is minimal (0.05 at interval end) because participation is genuinely voluntary and exit is costless for casual players — only competitive players are exit-constrained, and they choose to be (arbitrage-level choice). Theater is low (0.08) because the rulebook's function is structural coordination, not performance maintenance; the negligible theater reflects how little active defense the standard requires. The measurement series remain flat because the rulebook's function as a coordination good has not materially changed — the standard persists because the coordination benefit is stable. Minor upward drift in suppression reflects increased tournament infrastructure that enforces rulebook adherence in formal contexts, but this is enforcement of a shared standard, not coercion toward an asymmetric goal.
 *
 * PERSPECTIVAL GAP:
 *   The tournament organizer and the competitive-community seats should compute identically under this reading: both endorse the rulebook as legitimate standard and both benefit from its immutability. The excluded seats (rule-modification advocates) would compute a very different type from their alternative reading, but they are not represented in this constraint's stakeholder set because this constraint instantiates only the tournament reading. The pedagogical reading would compute the same rulebook as snare or extraction mechanism; the social-scaffold reading would compute it as tangled rope requiring correction. These are not perspectival gaps within one reading; they are reading-choice divergences between different constraints in the kernel family.
 *
 * DIRECTIONALITY LOGIC:
 *   The competitive community is the beneficiary seat (d near 0.0 beneficiary end): they gain access to a shared standard that enables skill comparison and ranking. They have mobile exit (can stop playing anytime) and choose the rulebook because it serves their interests in reproducible competition. Tournament organizers sit near symmetric (d ≈ 0.5): they administer the standard but gain legitimacy from its authority rather than extracting asymmetric benefit; their power is delegated from the community's endorsement of the rulebook as authoritative. No victims are declared because no party bears costs without reciprocal benefit — the voluntary structure of participation means only those who benefit from the coordination stay. The excluded seats (rule-modification advocates, pedagogical critics) are not victims; they are simply not parties to the competitive reading, and their objections locate them outside the tournament frame rather than inside it bearing extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy signal: the rulebook's founding problem (need for a shared competitive standard) remains live and the founding_problem_status is 'live' — the standard is not a vestigial constraint whose original purpose has dissolved. The constraint does not show the theater-ratio plus diffuse-cost signature of degraded function. The tournament-orthodoxy reading sustains the rulebook's legitimacy as a standard rather than as pedagogy or social correction, which prevents the reading itself from encountering mandatrophy. Mandatrophy appears only if the founding problem dies (tournaments no longer require rulebook consistency) or if the ruling infrastructure collapses, neither of which this reading predicts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rulebook_immutability_legitimacy,
    'Is the rulebook''s immutability a structural requirement for the coordination function, or a contingent choice by tournament governance?',
    'Natural experiment from alternative competitive game systems that modify rules between seasons while maintaining ranking systems (e.g., Magic: The Gathering, competitive gaming patches). If tournaments maintain ranking validity under rule changes, immutability is not structurally required.',
    'If immutability is contingent, the tournament reading becomes contestable — rule modifications might serve coordination without degrading it. If immutability is structural, the reading''s core axiom holds. This directly affects whether the sibling readings coexist or foreclose.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rulebook_immutability_legitimacy, empirical, 'Whether the rulebook''s resistance to modification is necessary for tournament function or a chosen governance structure.').

omega_variable(
    reading_boundary_specification,
    'Where does the tournament-orthodoxy reading end and the pedagogical reading begin? Do casual players who use house rules but acknowledge the rulebook standard count as the tournament reading or the social-scaffold reading?',
    'Ethnographic study of player communities: ask players which reading they hold (is the rulebook legitimate standard or text requiring modification) and compare to their actual play behavior. Measure the correlation between reading endorsement and rulebook adherence.',
    'If the boundary is sharp (players either treat the rulebook as immutable standard or as modifiable text), the readings truly coexist. If the boundary is blurred (players hold mixed positions), the constraint may not cleanly separate into three readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_boundary_specification, conceptual, 'Whether the three readings are structurally distinct or represent a continuum of positions.').

omega_variable(
    extraction_detection_through_tournament_structure,
    'Does the tournament-orthodoxy reading''s insistence on immutability benefit the rulebook publisher or tournament organizers in ways that mask extraction as coordination?',
    'Cost-structure analysis of rulebook publishing and tournament administration. Compare licensing revenue from tournaments that require official rulebooks versus alternative revenue models. Survey alternative competitive game systems that share rules without publisher licensing.',
    'If immutability serves primarily to sustain publisher licensing revenue, the reading may be masking institutional extraction under the guise of competitive purity. If immutability serves primarily to stabilize rankings, the coordination reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_detection_through_tournament_structure, empirical, 'Whether the tournament reading serves coordination or institutional capture by the rulebook publisher.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_rulebook__tournament_orthodoxy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mono_tr_t0, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(mono_tr_t10, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(mono_tr_t20, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 20, 0.07).
narrative_ontology:measurement(mono_tr_t30, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 30, 0.08).
narrative_ontology:measurement(mono_tr_t40, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 40, 0.08).
narrative_ontology:measurement(mono_tr_t50, monopoly_rulebook__tournament_orthodoxy_reading, theater_ratio, 50, 0.08).

% Extraction over time
narrative_ontology:measurement(mono_be_t0, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(mono_be_t10, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(mono_be_t20, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(mono_be_t30, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 30, 0.08).
narrative_ontology:measurement(mono_be_t40, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 40, 0.08).
narrative_ontology:measurement(mono_be_t50, monopoly_rulebook__tournament_orthodoxy_reading, base_extractiveness, 50, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(mono_su_t0, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(mono_su_t10, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 10, 0.04).
narrative_ontology:measurement(mono_su_t20, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 20, 0.05).
narrative_ontology:measurement(mono_su_t30, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 30, 0.05).
narrative_ontology:measurement(mono_su_t40, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 40, 0.05).
narrative_ontology:measurement(mono_su_t50, monopoly_rulebook__tournament_orthodoxy_reading, suppression_requirement, 50, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_rulebook__tournament_orthodoxy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(monopoly_rulebook__tournament_orthodoxy_reading, 0.02).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__extraction_demo_reading).
narrative_ontology:affects_constraint(monopoly_rulebook__tournament_orthodoxy_reading, monopoly_rulebook__social_scaffold_reading).

% DUAL FORMULATION NOTE:
% The Monopoly rulebook is a single kernel with three structurally distinct constraint readings. The tournament_orthodoxy_reading treats the rulebook as a legitimate, immutable competitive standard (low extraction, pure coordination). The extraction_demo_reading treats the same rulebook as a pedagogical mechanism for demonstrating capitalism's dynamics (high extraction, intentional asymmetry). The social_scaffold_reading treats the rulebook as a text requiring community correction through house rules to be socially playable (moderate extraction, tangled rope with modification as correction). All three readings coexist as live positions held by different communities. The ε values differ dramatically (0.08 vs 0.85 vs 0.35) because the readings define the referent differently: tournament orthodoxy measures the rulebook's role as a coordination standard; extraction demo measures the rulebook's role as teaching mechanism; social scaffold measures the rulebook's role as a constraint requiring modification. The decomposition follows the ε-invariance principle: one observable (the rulebook text) yields three different constraint stories because the readings' own epistemic frames define what counts as the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
