% ============================================================================
% CONSTRAINT STORY: establishment_clause__endorsement_test_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_establishment_clause__endorsement_test_reading, []).

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
 *   constraint_id: establishment_clause__endorsement_test_reading
 *   human_readable: Establishment Clause: Endorsement Test Reading
 *   domain: constitutional_law/religious_liberty
 *
 * SUMMARY:
 *   The Establishment Clause reading instantiated here treats establishment
 *   as a problem of government endorsement and the perception of religious
 *   outsider status. Under this reading, the government violates the
 *   Establishment Clause when a reasonable observer would perceive the
 *   government as sending a message that religion is favored and nonadherents
 *   are outsiders. This is a doctrinal reading of the Establishment Clause
 *   kernel — one of four major interpretive frameworks. Unlike the coercion
 *   test (which focuses on compulsion), the history-tradition reading (which
 *   anchors in founding practice), or the Lemon test (which applies a
 *   three-part legal formula), the endorsement test foregrounds social
 *   meaning and the experience of marginalization. The key structural feature
 *   is that the extraction mechanism is primarily symbolic and perceptual —
 *   the constraint targets the state's messaging about religious status —
 *   rather than mechanical coercion or procedural entanglement. This makes it
 *   a tangled rope: genuine coordination function (preventing religious
 *   warfare in a plural state) coexists with asymmetric extraction (majority
 *   religious symbols are constrained; outsider status is enforced through
 *   state messaging). The constraint's extractiveness has increased over time
 *   (0.35 → 0.58) as courts have broadened the reasonable-observer standard
 *   and applied it to more categories of government expression, while theater
 *   has simultaneously increased as legislatures adopt formal compliance
 *   (removing religious language) while maintaining informal religious
 *   preference.
 *
 * KEY AGENTS:
 *   - Religious Minorities and Nonadherents: Primary beneficiary (powerless/trapped) — nominally protected by the test but face perpetual outsider status enforcement through state messaging
 *   - Majority Religious Community: Secondary victim (powerful/constrained) — experiences constraint on public religious expression and symbolic status in state practice
 *   - Courts: Institutional enforcer (institutional/arbitrage) — applies the test and defines the reasonable observer; experiences coordination function
 *   - Federal and State Legislatures: Implementation actors (institutional/arbitrage) — face constraint on religious references in law and policy; often adopt performative compliance
 *   - Judicial Minimalists: Institutional critic (institutional/arbitrage) — argue the test is incoherent or over-expansive; advocate narrowing or replacement
 *   - Analytical Observer: Detached perspective (analytical/analytical) — risks naturalizing the endorsement test as a fixed feature of how government messaging operates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(establishment_clause__endorsement_test_reading, 0.58).
domain_priors:suppression_score(establishment_clause__endorsement_test_reading, 0.65).
domain_priors:theater_ratio(establishment_clause__endorsement_test_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(establishment_clause__endorsement_test_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(establishment_clause__endorsement_test_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(establishment_clause__endorsement_test_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(establishment_clause__endorsement_test_reading, tangled_rope).
narrative_ontology:human_readable(establishment_clause__endorsement_test_reading, "Establishment Clause: Endorsement Test Reading").
narrative_ontology:topic_domain(establishment_clause__endorsement_test_reading, "constitutional_law/religious_liberty").

domain_priors:requires_active_enforcement(establishment_clause__endorsement_test_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(establishment_clause__endorsement_test_reading, '32cae93a-677f-4223-8d67-0e6b98634fc0').
narrative_ontology:cs_kernel_codification('32cae93a-677f-4223-8d67-0e6b98634fc0', fixed_text).
narrative_ontology:cs_authority_grounding('32cae93a-677f-4223-8d67-0e6b98634fc0', lineage).
narrative_ontology:cs_interpretation_layer_present('32cae93a-677f-4223-8d67-0e6b98634fc0').
narrative_ontology:cs_reading_relation('32cae93a-677f-4223-8d67-0e6b98634fc0', establishment_clause__coercion_test_reading, coexists_with).
narrative_ontology:cs_reading_relation('32cae93a-677f-4223-8d67-0e6b98634fc0', establishment_clause__history_tradition_reading, coexists_with).
narrative_ontology:cs_reading_relation('32cae93a-677f-4223-8d67-0e6b98634fc0', establishment_clause__lemon_test_reading, influences).
narrative_ontology:cs_axiom('32cae93a-677f-4223-8d67-0e6b98634fc0', foundational, endorsement_by_message_violates_clause).
narrative_ontology:cs_axiom_status(endorsement_by_message_violates_clause, holdable).
narrative_ontology:cs_axiom_grounding('32cae93a-677f-4223-8d67-0e6b98634fc0', endorsement_by_message_violates_clause, deontological).
narrative_ontology:cs_axiom('32cae93a-677f-4223-8d67-0e6b98634fc0', foundational, reasonable_observer_social_meaning_metric).
narrative_ontology:cs_axiom_status(reasonable_observer_social_meaning_metric, holdable).
narrative_ontology:cs_axiom_grounding('32cae93a-677f-4223-8d67-0e6b98634fc0', reasonable_observer_social_meaning_metric, conventional).
narrative_ontology:cs_reference_frame('32cae93a-677f-4223-8d67-0e6b98634fc0', government_religious_neutrality_through_messaging).
narrative_ontology:cs_drift_state('32cae93a-677f-4223-8d67-0e6b98634fc0', contemporary_religious_pluralism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('32cae93a-677f-4223-8d67-0e6b98634fc0', '').
narrative_ontology:cs_kernel_id(establishment_clause__endorsement_test_reading, establishment_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(establishment_clause__endorsement_test_reading, religious_minorities).
narrative_ontology:constraint_beneficiary(establishment_clause__endorsement_test_reading, nonadherents_perceiving_outsider_status).
narrative_ontology:constraint_victim(establishment_clause__endorsement_test_reading, public_religious_expression).
narrative_ontology:constraint_victim(establishment_clause__endorsement_test_reading, majority_religious_community).
narrative_ontology:constraint_victim(establishment_clause__endorsement_test_reading, state_ceremonial_practices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PERCEIVED RELIGIOUS OUTSIDER (SNARE) — A nonadherent or member of a religious minority in a community where government messaging conveys religious favoritism experiences maximal extraction with minimal coordination function. The outsider cannot exit the jurisdiction; cannot change the perceived message without private exit; bears full social cost of marginalization encoded in state action. No legitimate coordination benefit — only suppression of their standing as equal citizens.
constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE MINORITY RELIGIOUS COMMUNITY (TANGLED ROPE) — Constrained by social pressure, employment risk, and political underrepresentation. The endorsement test nominally protects minority religions by preventing government endorsement of majority beliefs — a genuine coordination function for a multi-religious society. But enforcement requires litigation, costs resources, creates visibility that can trigger backlash, and addresses only messaging rather than material access. Mixed extraction and protection.
constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COURTS APPLYING THE ENDORSEMENT TEST (ROPE) — Courts perceive the test as a coordination mechanism: it defines what counts as permissible state expression and what counts as unconstitutional favoritism. The test solves a collective-action problem (preventing religious warfare in secular state). Courts have interpretive authority and can refine the test via doctrine. Low experienced extraction — the enforcement mechanism serves their institutional role.
constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE MAJORITY RELIGIOUS COMMUNITY (TANGLED ROPE) — Experiences the endorsement test as a constraint on public religious expression (prayer at graduation, religious monuments, holiday displays). The constraint coordinates religious accommodation in a plural society (genuine function), but enforces asymmetric cost: majority religious symbols must be removed or reframed; minority symbols are tolerated. Powerful agents can absorb constraints, but experience extraction of symbolic status.
constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGISLATIVE-EXECUTIVE IMPLEMENTATION (PITON) — Government officials and legislatures experience the endorsement test as a largely performative constraint: many adopt formal compliance (removing religious language, restructuring displays) while maintaining informal religious preference through patronage, staffing, and budget allocation. The formal implementation theater persists (legislative prayer, invocations before public meetings) with constitutional gloss from narrow precedent, despite substantive tension with the test's logic. Theater ratio reflects the gap between endorse-test rhetoric and actual practice.
constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the endorsement test appears to rest on an immutable principle: any government will inevitably send some message about religion through its symbols, speech, and practices. The 'reasonable observer' standard is presented as a fixed test of how perception works. The engine's false summit detector flags this perspective as a naturalization of the endorsement test's contingent doctrinal commitments as an unchangeable feature of how government messaging operates.
constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(establishment_clause__endorsement_test_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(establishment_clause__endorsement_test_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(establishment_clause__endorsement_test_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(establishment_clause__endorsement_test_reading, TR),
    TR >= 0.70.

:- end_tests(establishment_clause__endorsement_test_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The endorsement test extracts symbolic status and social standing from religious minorities through the mechanism of government messaging. The extraction is moderate rather than maximal because: (1) enforcement is doctrinal (invalidating laws) rather than direct coercion, (2) social meaning is partially measurable rather than purely mechanical, (3) minorities gain some protection (nonadherents cannot be subjected to coercive religious participation). But extraction is substantial because the test addresses perception of outsider status — a core dimension of civic equality — and the test's application has expanded over time. Suppression (0.65): Suppression is moderate-high because the test suppresses majority religious expression in public contexts (prayer at graduation, religious holiday displays, religious monuments) through constitutional invalidation. But suppression is not total because private religious expression remains protected and informal religious preference persists in executive patronage and staffing. Theater ratio (0.52): Moderate. The test has both functional and performative dimensions. Functionally, it prevents government from using its machinery to promote religious belief. Performatively, legislatures often adopt surface compliance (removing explicit religious language) while maintaining informal preferences, and executive officials invoke legislative prayer and religious invocation without endorsement-test challenge due to narrow precedent and judicial self-restraint.
 *
 * PERSPECTIVAL GAP:
 *   The endorsement test produces sharp perspectival gaps across different institutional positions. Religious minorities perceive it as a constraint that nominally protects them but leaves them perpetually marked as outsiders (the 'reasonable observer' standard foregrounds outsider perception rather than eliminating the marginalization). The majority religious community perceives it as an asymmetric constraint on their public expression. Courts perceive it as legitimate doctrine solving a coordination problem. Legislatures perceive it as partly circumventable through narrow precedent and informal practice. The piton perspective captures the observation that legislatures often adopt performative compliance (restructuring religious references) while maintaining informal religious preference, suggesting the constraint's functional force is decaying relative to its theater. The mountain perspective risks naturalizing the reasonable-observer standard as a fixed feature of how perception and messaging work, when in fact the standard is a contingent doctrinal construction that different frameworks (coercion test, history-tradition) dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The endorsement test is applied from multiple institutional and individual positions. A religious outsider (powerless/trapped) experiences directionality d ≈ 0.85: they cannot exit the jurisdiction, cannot change the government's messaging without costly litigation, bear the full social cost of any endorsement message, and have no benefit from the constraint itself — only protection from the worst coercive outcomes. A majority religious community (powerful/constrained) experiences directionality d ≈ 0.55: they can absorb the constraint through institutional resources (alternative religious forums, private expression) but experience extraction of symbolic status and public religious expression. Courts (institutional/arbitrage) experience d ≈ 0.15: they benefit from the coordination function and interpretive authority; the constraint is experienced as legitimate doctrine. Legislatures (institutional/arbitrage, but constrained by the test) experience d ≈ 0.45: they face binding doctrine that limits religious references but retain substantial discretion through narrow precedent and executive implementation. The perspectival gap emerges from these different directionality positions: the powerless agent sees a snare with minimal coordination benefit; the constrained powerful agent sees a mixed extraction-coordination mechanism; the court sees a coordination solution; the legislature sees a partly-circumventable constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The endorsement test reading resolves mandatrophy by showing that the extraction mechanism (symbolic status through government messaging) is genuinely distinct from coercive extraction and from procedural entanglement. The test is not a rope because it does not solve a pure coordination problem — the constraint produces asymmetric social meaning effects that privileged agents (majority religious adherents) do not experience the same way. It is not a snare because courts retain interpretive authority to prevent the worst abuses, and the test does protect some interests of religious minorities (preventing explicit government preference for majority religion). The tangled-rope classification reflects the genuine coordination function (preventing religious warfare in a plural state) coexisting with asymmetric extraction (status subordination of religious outsiders through government messaging). The mandatrophy is resolved by recognizing that the test's legitimacy rests on the coordination claim, but its empirical effect includes substantial extraction via social meaning — the two dimensions cannot be separated without abandoning the test's entire logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reasonable_observer_definition,
    'What is the constitutive definition of the ''reasonable observer'' in the endorsement test, and does it foreclose other readings of the Establishment Clause?',
    'Jurisprudential analysis of how courts define ''reasonable observer'': does it presume familiarity with constitutional doctrine (sophisticated), or social realism (naive), or hypothetical neutrality? Does the definitional choice logically rule out coercion-test or history-tradition readings?',
    'If ''reasonable observer'' is defined as knowledgeable-about-doctrine: coercion test is foreclosed (a reasonable observer would understand many religious supports as noncoercive). If ''reasonable observer'' is defined as naive-social-participant: history-tradition reading is foreclosed (no observer could treat founding-era religious establishment as neutral). If hypothetically neutral: both sibling readings remain defensible within competing frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reasonable_observer_definition, conceptual, 'Whether the reasonable observer standard forecloses sibling readings').

omega_variable(
    symbolic_status_extraction_mechanism,
    'Is the extraction mechanism of the endorsement test measurable social meaning (observer perception), or is it a formal legal mechanism (invalidating laws)?',
    'Empirical study: do nonadherents report reduced sense of political outsider status after endorsement-test invalidation of religious displays? Or does the extraction persist as internalized stigma despite formal legal victory? Does invalidation itself create backlash that increases symbolic subordination?',
    'If social meaning is primary: extractiveness stays at 0.58 (suppression is perceptual/social). If formal mechanism is primary: extractiveness drops to 0.35 (constraint is procedural, not status-based). If backlash is strong: extractiveness rises to 0.72 (constraint generates counter-extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_status_extraction_mechanism, empirical, 'Whether extraction is measurable as observer perception or persists independently of legal outcomes').

omega_variable(
    endorsement_vs_coercion_boundary,
    'Can a government endorse religion (under the endorsement test) without coercing participation, and if so, does this undermine the coercion-test reading''s core premise?',
    'Historical-doctrinal: identify cases where courts find endorsement without coercion. Assess whether coercion-test advocates treat these as failures of the endorsement test or as distinguishable on coercion-unavailability grounds.',
    'If endorsement without coercion is doctrinally coherent: readings coexist. If coercion-test advocates argue endorsement-without-coercion is permissible: readings partially foreclose each other depending on which framework is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endorsement_vs_coercion_boundary, empirical, 'Whether endorsement can occur without coercion, and if so, whether readings conflict').

omega_variable(
    kernel_committer_frame_adequacy,
    'Is the endorsement test properly understood as ONE reading of an Establishment Clause kernel, or is it a competing meta-rule that itself defines what counts as the kernel?',
    'Jurisprudential analysis: does the endorsement test presume a pre-existing Establishment Clause kernel (historical practice, textual meaning, founding intent) that it then interprets? Or does it constitute the kernel through the reasonable-observer standard? If the former: it is a reading. If the latter: it is a framework that generates multiple readings below it.',
    'If interpreting a kernel: the cs_structure framing is appropriate. If constituting the framework: the reading_relations should reflect that endorsement-test advocates use different reading definitions for coercion, history, and lemon-test approaches than those advocates do self-understandingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_committer_frame_adequacy, conceptual, 'Whether the endorsement test is a reading of the kernel or a framework that defines the kernel').

omega_variable(
    temporal_drift_of_reasonable_observer,
    'Does the ''reasonable observer'' standard change over time as social meaning shifts, or is it a fixed doctrinal reference point?',
    'Longitudinal case analysis: do courts apply the same ''reasonable observer'' standard to religious displays across decades, or does the standard itself evolve as social attitudes toward minority religion, secularism, and government neutrality shift?',
    'If fixed: drift_state is stable; the reading is robust. If evolving: drift_state shows practice_drift (actual application diverges from the reference frame of ''reasonable observer''); the reading''s authority structure is weakening. This affects mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_drift_of_reasonable_observer, empirical, 'Whether the reasonable observer standard is fixed or drifts with social meaning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(establishment_clause__endorsement_test_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esta_tr_t0, establishment_clause__endorsement_test_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(esta_tr_t10, establishment_clause__endorsement_test_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(esta_tr_t20, establishment_clause__endorsement_test_reading, theater_ratio, 20, 0.52).

% Extraction over time
narrative_ontology:measurement(esta_be_t0, establishment_clause__endorsement_test_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(esta_be_t10, establishment_clause__endorsement_test_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(esta_be_t20, establishment_clause__endorsement_test_reading, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(establishment_clause__endorsement_test_reading, identity_coordination).
narrative_ontology:affects_constraint(establishment_clause__endorsement_test_reading, establishment_clause__coercion_test_reading).
narrative_ontology:affects_constraint(establishment_clause__endorsement_test_reading, establishment_clause__history_tradition_reading).
narrative_ontology:affects_constraint(establishment_clause__endorsement_test_reading, establishment_clause__lemon_test_reading).

% DUAL FORMULATION NOTE:
% The Establishment Clause kernel admits four distinct readings, each with different extractiveness and suppression values. The endorsement-test reading (ε=0.58) emphasizes symbolic status extraction and social meaning. The coercion-test reading (ε=0.35, projected) emphasizes legal compulsion only. The history-tradition reading (ε=0.42, projected) emphasizes consistency with founding practice. The lemon-test reading (ε=0.48, projected) emphasizes procedural criteria. Each reading is a separate constraint story with its own base_properties, perspectives, and beneficiary/victim declarations. They are linked via network.affects_constraints because adoption of one reading influences the legitimacy and application of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
