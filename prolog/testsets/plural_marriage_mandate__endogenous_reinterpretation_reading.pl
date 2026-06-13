% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__endogenous_reinterpretation_reading, []).

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
 *   constraint_id: plural_marriage_mandate__endogenous_reinterpretation_reading
 *   human_readable: Prophetic Reinterpretation of Plural Marriage Mandate (1890 Endogenous Reading)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   In 1890, the leadership of the Church of Jesus Christ of Latter-day
 *   Saints issued a Manifesto declaring that God had revealed the temporal
 *   suspension of plural marriage practice (doctrine retained, practice
 *   halted) to preserve the church's salvific mission under existential
 *   federal pressure. This constraint story instantiates the ENDOGENOUS
 *   REINTERPRETATION READING: the constraint's legitimacy grounds itself in
 *   authentic prophetic revelation and theological coherence. From this
 *   reading, the coordination function is real (reorganizing the faith
 *   community around the reinterpreted doctrine to preserve institutional
 *   continuity, temple practice, and missionary capacity) and the extraction
 *   is incidental (fundamentalists excommunicated for maintaining the
 *   original reading bear the cost of institutional survival). The constraint
 *   is claimed as ROPE: genuine coordination enabled by legitimate prophetic
 *   directive. The measurement series shows the constraint crystallizing
 *   sharply in 1890 (extractiveness and theater both spike at the decision
 *   point) and slowly normalizing as the reinterpretation is institutionally
 *   internalized over the following decades. Theater_ratio remains elevated
 *   (~0.5+), indicating that the reinterpretation narratively performs the
 *   reconciliation of doctrine-retained-practice-suspended more than the
 *   actual fidelity to the original revelation's apparent binding force—a
 *   sign the coherence is more institutional than logical, though not
 *   dispositive.
 *
 * KEY AGENTS:
 *   - institutional_leadership: the church presidency and apostolic council, identity-locked to the church's doctrine-setting authority; authors the Manifesto as prophetic reinterpretation
 *   - mainstream_latter_day_saints: the larger organized membership accepting the reinterpretation; gain temple access and institutional participation at the cost of reorganizing family practice
 *   - fundamentalist_splinter_communities: organized communities maintaining plural marriage as binding doctrine; excommunicated and excluded from temples and institutional sacraments; bear the cost of institutional coordination
 *   - women_in_plural_marriages: powerless, trapped between legal jeopardy and doctrinal identity; face both immediate dissolution and long-term doctrinal dignity loss
 *   - federal_government: structurally external to this reading (coercion is backgrounded, not foregrounded); represented as absent voice challenging the endogenous reinterpretation framing
 *   - historical_analysts: observer seat assessing whether the Manifesto grounds itself in revelation, capitulation, or pragmatic rebranding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.38).
domain_priors:suppression_score(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.45).
domain_priors:theater_ratio(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__endogenous_reinterpretation_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(plural_marriage_mandate__endogenous_reinterpretation_reading, "Prophetic Reinterpretation of Plural Marriage Mandate (1890 Endogenous Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__endogenous_reinterpretation_reading, "religious institutional history / commitment systems / political theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__endogenous_reinterpretation_reading, '7062a2dc-8e4a-4470-a4d1-c58035b67d3f').
narrative_ontology:cs_kernel_codification('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', fixed_text).
narrative_ontology:cs_authority_grounding('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', lineage).
narrative_ontology:cs_interpretation_layer_present('7062a2dc-8e4a-4470-a4d1-c58035b67d3f').
narrative_ontology:cs_reading_relation('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', plural_marriage_mandate__institutional_pragmatism_reading, coexists_with).
narrative_ontology:cs_axiom('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', foundational, revelation_temporally_suspends_not_negates).
narrative_ontology:cs_axiom_status(revelation_temporally_suspends_not_negates, holdable).
narrative_ontology:cs_axiom_grounding('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', revelation_temporally_suspends_not_negates, deontological).
narrative_ontology:cs_axiom('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', foundational, institutional_authority_interprets_revelation_legitimately).
narrative_ontology:cs_axiom_status(institutional_authority_interprets_revelation_legitimately, holdable).
narrative_ontology:cs_axiom_grounding('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', institutional_authority_interprets_revelation_legitimately, conventional).
narrative_ontology:cs_reference_frame('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', original_plural_marriage_revelation).
narrative_ontology:cs_drift_state('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', post_1890_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7062a2dc-8e4a-4470-a4d1-c58035b67d3f', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, latter_day_saint_institutional_church).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, missionaries_and_temple_workers).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__endogenous_reinterpretation_reading, adherents_pursuing_salvation_through_institutional_channels).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, fundamentalist_splinter_communities).
narrative_ontology:constraint_victim(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_practitioners_excommunicated_post_1890).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__endogenous_reinterpretation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__endogenous_reinterpretation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).
:- end_tests(plural_marriage_mandate__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) because the constraint transfers doctrinal authority from believers to institutional leadership and excludes fundamentalists from temple participation, but the beneficiary set (mainstream adherents, missionaries, temple workers) is large and substantially benefits from coordination, not merely from extraction. Suppression is lower than extraction (0.45 vs 0.38) because the enforcement of the reinterpretation relies primarily on institutional excommunication and doctrinal authority, not on active coercive machinery—the fundamentalists' identity-lock makes their resistance costly without requiring continuous suppression. Theater is elevated (0.52) because the doctrine-retained-practice-suspended framing is narratively sophisticated but structurally unstable: the original revelation commanded practice; the reinterpretation suspends it; the coherence narrative emphasizes 'preservation of the salvific mission' more than 'obedience to the original command.' This theatrical component grows as fundamentalist communities challenge the coherence, forcing increasingly elaborate explanations. Accessibility collapse is moderate-high (0.62) because the reinterpretation's internal logic is available to members (the church teaches the doctrine) but the practical exit is prohibitively costly (identity loss, community rupture, loss of salvific pathway as understood). Resistance is elevated (0.68) because fundamentalist communities and individual practitioners maintained plural marriage despite institutional pressure, attesting that the reinterpretation never achieved unanimous acceptance. The measurement series is authored on one shared time grid spanning 1880–1920, capturing the decision point (1890), institutionalization (1900–1910), and normalization (1920).
 *
 * PERSPECTIVAL GAP:
 *   The institutional leadership and mainstream membership compute the constraint differently from fundamentalist communities and historical observers skeptical of the revelation narrative. From the leadership's seat, the reinterpretation is legitimate prophetic guidance coordinating the faith community. From the fundamentalist seat, the same structure is institutional betrayal under federal coercion, excommunicating those who remained faithful to the original revelation. From the federal government's seat (excluded from this reading's frame), the Manifesto is capitulation to prosecution pressure. From the historian's analytical seat, the constraint's legitimacy cannot be determined from structure alone—the evidence (internal deliberations, timing, doctrinal precedent) remains contested and drives the kernel contest into three irreducible readings. The engine computes per-seat classification: a mainstream member accepting the prophetic framing may compute rope; a fundamentalist maintaining the original doctrine computes snare; an analyst skeptical of the revelation narrative computes snare or tangled_rope with a piton component (theater-elevated preservation of a superseded doctrine). This divergence IS the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership holds directionality near the beneficiary end (d ~ 0.15–0.25): they collect legitimacy, authority, and institutional continuity from the reinterpretation; their exit options are identity-locked (no leadership position exists outside the institutional hierarchy) but their power is institutional so the extraction formula yields low d. Mainstream members hold d ~ 0.4–0.5 (symmetric): they benefit from temple access and doctrinal coherence while bearing the modest cost of practice reorganization and implicit acceptance of institutional authority over personal revelation. Fundamentalists hold d near the target end (d ~ 0.75–0.85): they are excommunicated, excluded from temples, and bear doctrinal ostracism for maintaining what they understand as fidelity to the original binding revelation; their identity-lock makes exit existentially costly; their power is organized but subordinate to the institutional hierarchy. Women in fundamentalist plural marriages hold even higher d (d ~ 0.85–0.95): they combine the fundamentalist victim status with legal jeopardy and powerlessness. These directionality differentials drive the per-seat classification divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   The 1890 Manifesto resolves an acute mandatrophy by reinterpreting the founding mandate (plural marriage as divine requirement) in light of new revelation (practice suspended, doctrine intact). From the endogenous reading, this is legitimate theological work—the mandate persists but its application is redefined by later revelation, which is standard within Latter-day Saint framework (subsequent revelation supersedes prior revelation in practice). From the exogenous reading, the mandatrophy is unresolved: the founding mandate's apparent binding force contradicts the 1890 suspension, creating a permanent theological incoherence papered over by institutional authority and narrated as revelation. The test is whether the reinterpretation holds under member belief and practice: if members successfully internalize 'doctrine intact, practice suspended' as coherent, mandatrophy is resolved within the endogenous frame. If the constraint requires continuous theatrical justification (omegas suggest it does), mandatrophy is partially unresolved, and the constraint carries a piton component (institutional performance of a framework that has lost internal coherence). The measurement series shows theater_ratio rising from 1890 to 1910 (stabilizing the reinterpretation narratively) and plateauing (suggesting the theatrical work is ongoing rather than complete), consistent with partial mandatrophy—the founding problem is addressed (federal pressure relieved, institutional survival secured) but the doctrinal coherence is not fully assured.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_capitulation,
    'Is the 1890 Manifesto an instance of endogenous prophetic reinterpretation (God revealed a new directive), exogenous capitulation under federal coercion (the leadership chose institutional survival over doctrinal fidelity), or pragmatic institutional rebranding (capitulation narrated as revelation to preserve legitimacy)?',
    'Detailed historical analysis of: (1) contemporaneous leadership diaries, deliberations, and correspondence; (2) timing alignment between federal prosecution intensity and institutional decision-making; (3) doctrinal precedent for temporary revelation-directed practice suspension; (4) framing consistency in how the leadership explained the decision internally vs. publicly.',
    'If endogenous (revelation), the constraint is rope (legitimate coordination around new prophetic directive). If exogenous or pragmatic-rebranding, the constraint is snare (coerced institutional extraction with doctrinal cover). This is the reading''s irreducible ambiguity and the kernel contest''s central site.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(revelation_vs_capitulation, conceptual, 'Whether the constraint grounds its legitimacy in authentic prophetic reinterpretation or in institutionally beneficial rebranding of coercion.').

omega_variable(
    doctrine_vs_practice_coherence,
    'Does retaining plural marriage doctrine while suspending practice create genuine theological coherence (as the endogenous reading claims) or is it an unstable compromise that systematically privileges institutional survival over doctrinal fidelity?',
    'Sustained textual and theological analysis of how the church (a) justifies the suspension as temporary and doctrinal truth intact, (b) instructs members on the relationship between eternal doctrine and temporal practice, and (c) handles contradictions as fundamentalist communities challenge the coherence claim by maintaining practice alongside doctrine.',
    'If coherent, the constraint is sustainable as rope (coordination around reinterpreted revelation). If incoherent, the theatrical component (theater_ratio) rises and the constraint approaches piton (doctrine retained performatively while practice has structurally shifted). Persistent fundamentalist objections suggest the coherence is contested even internally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_practice_coherence, conceptual, 'Whether doctrine-retained-practice-suspended is a stable theological framework or an inherently unstable institutional compromise.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the suppression of fundamentalist practice primarily structural (legal jeopardy, institutional excommunication, resource denial) or internalized (fundamentalists have absorbed a shame/illegitimacy narrative that makes exit unthinkable even absent structural barriers)?',
    'Post-institutional observation: how much fundamentalist dissent and practice persistence occurs in jurisdictions where federal prosecution ended (post-1920s) and institutional penalties weakened? Do communities reform practice or continue? Do excommunicated individuals maintain practice if legal risk subsides?',
    'If structural, the constraint''s suppression depends on continued federal/institutional enforcement; if internalized, the suppression persists even after external enforcement weakens (higher effective suppression). Identity-locked exit suggests internalization, but the distinction matters for understanding the constraint''s persistence mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether suppression is structural (external barriers) or internalized (identity/shame narratives that persist after barriers weaken).').

omega_variable(
    kernel_reading_contest,
    'This constraint instantiates ONE reading of the plural_marriage_mandate kernel. What is the relationship between this endogenous_reinterpretation reading and its sibling readings (exogenous_override_reading, institutional_pragmatism_reading)? Are they logically foreclosed by each other, structurally coexisting in different communities, or creating mutual pressure?',
    'Comparative constraint story analysis: the three readings should have different ε values, different beneficiary/victim structures, and different claimed types. If one reading''s foundational axiom is directly refuted by another reading''s grounding evidence, they foreclose. If the readings co-persist as competing interpretive traditions, they coexist. If one reading''s adoption changes the other''s operating conditions, it influences.',
    'The kernel contest is the meta-problem: all three readings are live in the historical record and in contemporary institutional discourse. The engine''s per-seat classification should diverge by reading—an observer adopting the endogenous reading will compute rope; an observer adopting the exogenous reading will compute snare. The readings form a constraint family; their relationships structure how the kernel itself is understood.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Relationship between this reading and its sibling readings of the contested plural_marriage_mandate kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__endogenous_reinterpretation_reading, 1880, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1880, 0.25).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.48).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.54).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.56).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.52).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1880, 0.22).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.38).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.41).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.39).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1880, 0.28).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.45).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.48).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.46).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__endogenous_reinterpretation_reading, 0.12).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__exogenous_override_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__endogenous_reinterpretation_reading, plural_marriage_mandate__institutional_pragmatism_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the plural_marriage_mandate kernel family. All three readings (endogenous_reinterpretation, exogenous_override, institutional_pragmatism) share the same historical event (the 1890 Manifesto) but instantiate different constraints because they ground the Manifesto's legitimacy differently (revelation vs. coercion vs. pragmatic rebranding). Each reading has a distinct ε value, beneficiary/victim structure, and claimed type. The three stories are linked bidirectionally via affects_constraints to enable comparative analysis of how the same historical event produces different constraint classifications under different legitimacy framings. The kernel contest is the meta-problem: determining which reading's framing is structurally defensible requires examining historical evidence external to the readings' own narratives (documented coercion, institutional deliberations, doctrinal precedent). The Boltzmann floor is elevated (0.12 vs. 0.08 canonical for identity_coordination) because this constraint coordinates boundary maintenance of group membership under identity pressure: the reinterpretation is fundamentally a question of who counts as legitimately practicing the faith (doctrinal members who suspend practice) vs. who is excluded (fundamentalists who maintain practice). Identity coordination at this scale (whole faith community, founding doctrine, salvific pathway) requires elevated coherence overhead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
