% ============================================================================
% CONSTRAINT STORY: party_state_duality__description_not_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_party_state_duality__description_not_constraint_reading, []).

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
 *   constraint_id: party_state_duality__description_not_constraint_reading
 *   human_readable: 1936 Soviet Constitution as Anatomical Description (Not Constraint)
 *   domain: legal/political/constitutional
 *
 * SUMMARY:
 *   The 1936 Soviet Constitution presents an inversion of constitutionalism's
 *   defining premise: instead of constraining power, it describes power in
 *   language that legitimates its unconstrained exercise. Adopted following
 *   Stalin's consolidation of party control through the Great Purge, the 1936
 *   text enumerates rights (assembly, speech, organization) while the party
 *   apparatus simultaneously suppresses their exercise — the document
 *   provides no mechanism through which its stated constraints could
 *   function. This is not a failed or incomplete constitution; it is a
 *   constitution that successfully inverts its own form, turning the language
 *   of constitutional constraint into an instrument of party supremacy. The
 *   constraint operates at the intersection of legal doctrine (the text
 *   itself), institutional practice (party discipline and nomenklatura
 *   control), and legitimacy claims (the regime's use of constitutional
 *   language to justify unconstrained power). From the perspective of the
 *   party leadership, the 1936 text is brilliant coordination: it provides
 *   constitutional legitimacy while removing constitutional constraint. From
 *   the perspective of citizens, jurists, and constitutionalism as a
 *   doctrine, it is pure extraction — the document itself becomes the
 *   mechanism of suppression.
 *
 * KEY AGENTS:
 *   - Party Leadership: Primary beneficiary (institutional/arbitrage) — captures both unconstrained power and constitutional legitimacy simultaneously
 *   - Soviet Citizens: Primary victims (powerless/trapped) — bound by a document that describes rights while suppressing their exercise, with no mechanism for redress
 *   - Constitutional Doctrine: Secondary victim (analytical/analytical) — the 1936 text inverts and invalidates the entire tradition of constitutionalism as constraint-on-power
 *   - Jurists and Legal Theorists: Moderate victims (moderate/constrained) — forced to choose between abandoning constitutionalism or arguing that the 1936 text is not a real constitution
 *   - The Constitutional Form Itself: Institutional actor (institutional/arbitrage) — maintains performative appearance of constraint while actual constraint mechanism operates entirely outside it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(party_state_duality__description_not_constraint_reading, 0.88).
domain_priors:suppression_score(party_state_duality__description_not_constraint_reading, 0.92).
domain_priors:theater_ratio(party_state_duality__description_not_constraint_reading, 0.95).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(party_state_duality__description_not_constraint_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(party_state_duality__description_not_constraint_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(party_state_duality__description_not_constraint_reading, theater_ratio, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(party_state_duality__description_not_constraint_reading, snare).
narrative_ontology:human_readable(party_state_duality__description_not_constraint_reading, "1936 Soviet Constitution as Anatomical Description (Not Constraint)").
narrative_ontology:topic_domain(party_state_duality__description_not_constraint_reading, "legal/political/constitutional").

domain_priors:requires_active_enforcement(party_state_duality__description_not_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(party_state_duality__description_not_constraint_reading, '166c1ab8-6d3d-41cf-9ed0-8acbeecea804').
narrative_ontology:cs_kernel_codification('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', fixed_text).
narrative_ontology:cs_authority_grounding('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', extraction).
narrative_ontology:cs_interpretation_layer_present('166c1ab8-6d3d-41cf-9ed0-8acbeecea804').
narrative_ontology:cs_reading_relation('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', party_state_duality__article_126_keyhole_reading, forecloses).
narrative_ontology:cs_reading_relation('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', party_state_duality__dual_hierarchy_mechanics_reading, coexists_with).
narrative_ontology:cs_axiom('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', foundational, constitution_describes_rather_than_constrains).
narrative_ontology:cs_axiom_status(constitution_describes_rather_than_constrains, holdable).
narrative_ontology:cs_axiom_grounding('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', constitution_describes_rather_than_constrains, deontological).
narrative_ontology:cs_axiom('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', foundational, legitimacy_derives_from_power_anatomy_not_power_limit).
narrative_ontology:cs_axiom_status(legitimacy_derives_from_power_anatomy_not_power_limit, holdable).
narrative_ontology:cs_axiom_grounding('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', legitimacy_derives_from_power_anatomy_not_power_limit, instrumental).
narrative_ontology:cs_reference_frame('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', constitutional_constraint_on_executive).
narrative_ontology:cs_drift_state('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', post_great_purge_stabilization, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('166c1ab8-6d3d-41cf-9ed0-8acbeecea804', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(party_state_duality__description_not_constraint_reading, party_state_duality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(party_state_duality__description_not_constraint_reading, party_leadership).
narrative_ontology:constraint_victim(party_state_duality__description_not_constraint_reading, constitutionalism_as_doctrine).
narrative_ontology:constraint_victim(party_state_duality__description_not_constraint_reading, soviet_citizens_assumed_rights_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SOVIET CITIZEN (SNARE) — Trapped within a document that describes power but does not constrain it. The constitution purports to grant rights (assembly, speech, organization) while the party apparatus simultaneously suppresses their exercise. The citizen cannot exit; the document provides no mechanism for redress. The suppression is total and definitional.
constraint_indexing:constraint_classification(party_state_duality__description_not_constraint_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE JURIST SEEKING CONSTITUTIONAL CONSTRAINT (SNARE) — Constrained by the absence of any mechanism to enforce the document's stated limits. A lawyer trained in constitutionalism recognizes the 1936 text as an inversion: it describes without constraining. The document itself is the mechanism of suppression — it creates the false appearance of constraint while definitionally removing constraint. The jurist's only exit is to abandon constitutionalism itself.
constraint_indexing:constraint_classification(party_state_duality__description_not_constraint_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PARTY LEADERSHIP (ROPE) — Experiences the 1936 constitution as pure coordination: it describes the anatomy of power in a way that consolidates party control. The leadership benefits from the document's inversion — it provides the language of constitutionalism (legitimating the regime) while removing the substance (constraining power). No extraction is experienced from above; the leadership is the beneficiary of the architecture itself.
constraint_indexing:constraint_classification(party_state_duality__description_not_constraint_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE CONSTITUTIONAL FORM ITSELF (PITON) — The 1936 text maintains the performative appearance of constitutionalism (elections, article-by-article structure, rights enumeration) while the actual constraint mechanism operates entirely outside it (party discipline, nomenklatura, apparatus control). The constitutional form persists through institutional inertia — it provides the theater of legitimacy that the regime requires. The functional constraint is elsewhere; the document is theatrical.
constraint_indexing:constraint_classification(party_state_duality__description_not_constraint_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (SNARE) — From a universal/civilizational view, the 1936 constitution represents an inversion so complete that it instantiates a new category: a constitutional document that is structurally antagonistic to constitutionalism itself. The document's function is not to constrain power but to describe it in terms that legitimize its unconstrained exercise. The suppression is definitional — built into the very form that purports to protect. This is snare at the largest scale.
constraint_indexing:constraint_classification(party_state_duality__description_not_constraint_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(party_state_duality__description_not_constraint_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(party_state_duality__description_not_constraint_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(party_state_duality__description_not_constraint_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(party_state_duality__description_not_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(party_state_duality__description_not_constraint_reading, TR),
    TR >= 0.70.

:- end_tests(party_state_duality__description_not_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.88): Very high. The party leadership extracts unconstrained power while the document provides the language of legitimation. The extractiveness is not 1.0 because the regime still requires minimal compliance with the performative appearance of constitutionalism (elections must be held, articles must exist, the text must be cited). The measurements show rising extractiveness from 1936 (0.72) through the Great Purge period (0.84) to post-purge stabilization (0.88), reflecting the regime's increasing confidence that the document's inversion is complete and stable. Suppression (0.92): Extremely high. The document itself is a suppression mechanism — it creates the false appearance of constraint while removing all functional constraint. The suppression includes: (1) legal prohibition of independent organization (despite constitutional protection of assembly), (2) nomenklatura control (despite constitutional protection of democratic elections), (3) party discipline enforced through apparatus (despite constitutional protection of speech), (4) absence of any judicial mechanism to enforce the text's stated limits. Theater ratio (0.95): Extremely high. The constitutional form is almost entirely performative. Elections are held but outcomes are predetermined. Councils meet but decisions are taken in party committees. Articles are written but the real constraint is party discipline. The theater increases across the measurements as the regime consolidates confidence that the form requires no substance. At t=10 (post-purge stabilization), the theater ratio is maximum because the party has demonstrated that the form persists regardless of how completely the substance has been removed.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the party's perception (coordination, rope) and the citizen's perception (extraction, snare) is maximal. The gap is not a disagreement about facts but about what the same facts mean. The party sees a document that successfully legitimates power; the citizen sees a document that suppresses rights while claiming to protect them. The gap is not resolvable by additional information — it is structural to the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit options. The party leadership (institutional/arbitrage exit) is the beneficiary — they benefit from unconstrained power while escaping the constraint they impose. Their d-value is low (≈0.05), producing negative effective extractiveness from their perspective — they experience the constraint as coordination, not extraction. Soviet citizens (powerless/trapped exit) are victims — they bear the cost of suppression while receiving no benefit. Their d-value is high (≈0.95), producing maximum effective extractiveness from their perspective. Constitutional doctrine (analytical/analytical) occupies a paradoxical position: it is both victim (the 1936 text inverts the entire tradition) and observer (the tradition can recognize the inversion). The doctrine's d-value reflects the magnitude of the inversion — the 1936 text is maximally antagonistic to the constitutional tradition that generated it.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through explicit recognition that the 1936 text is not a failed constitution but an inverted one. The constraint is not Tangled Rope (mixed coordination and extraction) because there is no genuine coordination function for the victims — the document does not solve a collective action problem for citizens. The constraint is not Scaffold because there is no sunset clause (the inversion is designed to be permanent). The constraint is not Piton because, despite high theater, the constraint's functional mechanism (party supremacy) is active and enforced, not degraded. The constraint is Snare because: (1) base extractiveness is very high (0.88); (2) suppression is very high (0.92); (3) effective extraction χ is maximized across all perspectives except the beneficiary; (4) the beneficiary (party leadership) derives extreme benefit from the suppression of alternatives. The mandatrophy is resolved by recognizing that the inversion of constitutionalism is not a corruption of the document but its perfect execution — the 1936 text succeeds completely at its actual function (legitimating unconstrained party power), not at its claimed function (constraining power). This is snare by definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anatomy_vs_leash_boundary,
    'Is the 1936 text fundamentally different from other one-party constitutions, or does it instantiate a general pattern of party-state inversion?',
    'Comparative constitutional analysis: examination of similar constitutions (Yugoslav, Chinese 1954, East German 1968) to determine whether inversion is unique to 1936 or structural to party-state systems generally.',
    'If unique: the 1936 text is a specific historical artifact of Stalin''s period. If general: inversion is inherent to party-state constitutionalism, and the 1936 text is an unusually explicit case of a broader phenomenon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anatomy_vs_leash_boundary, conceptual, 'Whether the anatomy-vs-leash inversion is specific to 1936 or structural to party-state constitutions').

omega_variable(
    extractiveness_unconstrained_by_definition,
    'Does the document''s inversion mean that extractiveness has no upper bound, or is extractiveness limited by party regime stability?',
    'Historical analysis of extraction patterns: compare periods of high party consensus (Stalin 1930s, later Brezhnev stability) vs periods of party factionalism (1950s succession struggles). If extractiveness remains constant despite regime stability shifts, the constraint is unconstrained; if extractiveness varies with regime stability, external factors limit the apparent extractiveness.',
    'If truly unconstrained: ε=0.88 is understated; classification is definitively Snare with zero resistance. If stability-limited: ε should be adjusted downward; classification remains Snare but with a secondary coordination function (maintaining party regime stability itself).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_unconstrained_by_definition, empirical, 'Whether extractiveness is truly unconstrained or limited by regime stability').

omega_variable(
    reading_contest_foreclosure_logic,
    'Does this reading (description-not-constraint) logically foreclose the keyhole reading (Article 126 as hidden constitution)?',
    'Logical analysis of the two readings'' core premises. The description-not-constraint reading asserts: the 1936 text inverts constitutionalism entirely, describing unconstrained power. The keyhole reading asserts: Article 126 is the real constraint, naming the party as the hidden constitutional anchor. Can both be true simultaneously within a single interpretive framework?',
    'If they foreclose each other: the relation is ''forecloses'' (one reading rules out the other). If they coexist: different parties hold them simultaneously (e.g., Western analysts see inversion; party theorists see Article 126 as the constraint). The answer determines cs_structure.reading_relations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_foreclosure_logic, conceptual, 'Logical relationship between description-not-constraint and keyhole readings').

omega_variable(
    kernel_identity_across_readings,
    'What is the kernel itself — the contested commitment that all three readings are readings of? Is it the 1936 text? The relationship between party and state? The concept of constitutional legitimacy?',
    'Clarification through comparative reading: identify what is stable across all three readings (the thing being read) vs what differs (each reading''s interpretation). The kernel is what remains stable; the readings are the competing interpretations of that stable core.',
    'Affects omega variable documentation and cs_structure.reading_relations logic. If the kernel is ''the 1936 text itself,'' readings differ on what the text means. If the kernel is ''party-state relationship,'' the three readings are competing frames for understanding the same institutional dynamic. This determines how to characterize the kernel_codification (formalized text vs implicit institutional arrangement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_identity_across_readings, conceptual, 'Identity of the contested kernel that all three readings interpret').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(party_state_duality__description_not_constraint_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1936_adoption, party_state_duality__description_not_constraint_reading, theater_ratio, 0, 0.85).
narrative_ontology:measurement(theater_mid_great_purge, party_state_duality__description_not_constraint_reading, theater_ratio, 5, 0.92).
narrative_ontology:measurement(theater_post_purge_stabilization, party_state_duality__description_not_constraint_reading, theater_ratio, 10, 0.95).

% Extraction over time
narrative_ontology:measurement(extract_1936_adoption, party_state_duality__description_not_constraint_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(extract_mid_great_purge, party_state_duality__description_not_constraint_reading, base_extractiveness, 5, 0.84).
narrative_ontology:measurement(extract_post_purge_stabilization, party_state_duality__description_not_constraint_reading, base_extractiveness, 10, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1936_adoption, party_state_duality__description_not_constraint_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(suppress_mid_great_purge, party_state_duality__description_not_constraint_reading, suppression_requirement, 5, 0.88).
narrative_ontology:measurement(suppress_post_purge_stabilization, party_state_duality__description_not_constraint_reading, suppression_requirement, 10, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(party_state_duality__description_not_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(party_state_duality__description_not_constraint_reading, article_126_keyhole_reading).
narrative_ontology:affects_constraint(party_state_duality__description_not_constraint_reading, dual_hierarchy_mechanics_reading).

% DUAL FORMULATION NOTE:
% The three readings of the party_state_duality kernel are structurally distinct constraints with different ε values and classification profiles. The 'description_not_constraint_reading' asserts that the document's function is to describe rather than constrain (ε=0.88, Snare). The 'keyhole_reading' asserts that Article 126 is the hidden constraint (likely ε < 0.88, Tangled Rope or lower). The 'dual_hierarchy_reading' asserts that the real constitution is practice, not text (likely ε variable depending on nomenklatura stability). All three readings interpret the same kernel (the party-state relationship) but produce different classifications. They are linked through network.affects_constraints to form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
