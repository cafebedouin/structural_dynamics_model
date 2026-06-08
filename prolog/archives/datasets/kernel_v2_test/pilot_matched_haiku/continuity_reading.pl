% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: continuity_reading
 *   human_readable: Correct Latin as Continuous Living Practice (Continuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel
 *   'correct_latin': the continuity reading asserts that correct Latin is the
 *   form transmitted through continuous living practice, with medieval Latin
 *   as legitimate evolved Classical Latin rather than corruption. The reading
 *   grounds legitimacy in organic language evolution and the unbroken
 *   transmission of Latin through the medieval ecclesiastical tradition. This
 *   reading competes with the discontinuity reading (correct Latin is only
 *   Classical Latin, medieval forms are corruptions) and a hybrid reading
 *   (both Classical and medieval forms are legitimate in different contexts).
 *   The continuity reading is structurally a tangled rope: it solves a
 *   genuine coordination problem (how to maintain Latin as a living language
 *   across centuries) while simultaneously extracting legitimacy from
 *   medieval scribes by condemning purist attacks on their practice. The
 *   constraint exhibits high theater ratio (0.58) because the continuity
 *   reading requires constant performative assertion that medieval forms are
 *   'evolved' rather than 'corrupted' — the distinction is normative, not
 *   empirically observable from the forms themselves.
 *
 * KEY AGENTS:
 *   - Medieval ecclesiastical scribes: Primary victims (powerless/trapped) — their actual practice is condemned as corruption by purists while being defended as legitimate evolution by continuity advocates
 *   - Continuity doctrine defenders: Primary beneficiaries (moderate/constrained) — scholars and ecclesiastical authorities who benefit from the reading by validating medieval practice; constrained by need to maintain coordination with Classical authorities
 *   - Living ecclesiastical tradition: Institutional beneficiary (institutional/arbitrage) — the Church benefits from continuity reading by maintaining Latin as a living liturgical language without impossible Classical purity requirements
 *   - Classical purist tradition: Secondary victim (powerless/trapped) — trapped in a losing position; cannot enforce Classical purity on a dead language; persists through theater rather than function
 *   - Renaissance humanist revival: Institutional actor (institutional/arbitrage) — revived Classical Latin as prestige standard; maintains purist reading through educational theater and institutional inertia (piton perspective)
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing the normative claim that language evolution legitimates medieval forms as if it were a law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.35).
domain_priors:suppression_score(continuity_reading, 0.42).
domain_priors:theater_ratio(continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, tangled_rope).
narrative_ontology:human_readable(continuity_reading, "Correct Latin as Continuous Living Practice (Continuity Reading)").
narrative_ontology:topic_domain(continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_reading, 'db7f06c3-c261-40b4-ac6e-621fb42c6ae7').
narrative_ontology:cs_kernel_codification('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', fixed_text).
narrative_ontology:cs_authority_grounding('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', lineage).
narrative_ontology:cs_interpretation_layer_present('db7f06c3-c261-40b4-ac6e-621fb42c6ae7').
narrative_ontology:cs_reading_relation('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', continuity_reading__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', continuity_reading__hybrid_reading, influences).
narrative_ontology:cs_axiom('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', foundational, language_evolution_legitimates_forms).
narrative_ontology:cs_axiom_status(language_evolution_legitimates_forms, holdable).
narrative_ontology:cs_axiom_grounding('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', language_evolution_legitimates_forms, empirically_contingent).
narrative_ontology:cs_axiom('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', foundational, continuous_transmission_preserves_identity).
narrative_ontology:cs_axiom_status(continuous_transmission_preserves_identity, holdable).
narrative_ontology:cs_axiom_grounding('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', continuous_transmission_preserves_identity, conventional).
narrative_ontology:cs_reference_frame('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', classical_latin_as_living_tradition).
narrative_ontology:cs_drift_state('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', medieval_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('db7f06c3-c261-40b4-ac6e-621fb42c6ae7', '').
narrative_ontology:cs_kernel_id(continuity_reading, correct_latin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, medieval_ecclesiastical_scribes).
narrative_ontology:constraint_beneficiary(continuity_reading, continuity_doctrine_defenders).
narrative_ontology:constraint_victim(continuity_reading, classical_purist_tradition).
narrative_ontology:constraint_victim(continuity_reading, linguistic_precision_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(continuity_reading, continuity_defenders).
narrative_ontology:constraint_beneficiary(continuity_reading, ecclesiastical_tradition).
narrative_ontology:constraint_beneficiary(continuity_reading, humanist_revival).
narrative_ontology:constraint_victim(continuity_reading, medieval_scribes).
narrative_ontology:constraint_victim(continuity_reading, purist_defenders).
narrative_ontology:constraint_vindicates(continuity_reading, organic_language_evolution).
narrative_ontology:constraint_vindicates(continuity_reading, living_tradition_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Medieval ecclesiastical scribes write in the Latin they know — evolved forms, phonetic spellings, new syntax — because Classical Latin is a dead language. Their practice is necessary for communication within the Church but condemned as corruption by purist authorities. They cannot write in Classical Latin (impossible without living speakers) nor in vernacular (ecclesiastical authority requires Latin). They bear the cost of being caught between two incompatible standards.
narrative_ontology:constraint_stakeholder(continuity_reading, medieval_scribes, payer,
    powerless, biographical, trapped, regional).

% Scholars and ecclesiastical authorities who defend medieval Latin as legitimate evolution. They benefit from the continuity reading by validating their intellectual position and the actual practice of the Church. They set the agenda by arguing that medieval forms are evolved, not corrupted. Constrained by the need to maintain coordination with Classical authorities and by the threat of purist counter-claims. Must continuously argue and defend the reading against purist attacks.
narrative_ontology:constraint_stakeholder(continuity_reading, continuity_defenders, agenda_setter,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(continuity_reading, continuity_defenders, beneficiary).

% The living ecclesiastical tradition (the Church as an institution) benefits from the continuity reading because it enables Latin to function as a living liturgical language across centuries without the impossible burden of Classical purity. The tradition can arbitrage between strict Classical forms (in formal documents) and evolved forms (in daily practice). The continuity reading solves a genuine coordination problem: how to maintain Latin as a unified language when the language naturally evolves.
narrative_ontology:constraint_stakeholder(continuity_reading, ecclesiastical_tradition, beneficiary,
    institutional, generational, arbitrage, global).

% Classical purist authorities who assert that correct Latin is only Classical Latin and medieval forms are corruptions. Trapped in a losing position: Classical Latin is a dead language with no living speakers, so purist purity is impossible to enforce without destroying the living tradition entirely. They cannot offer a viable alternative to medieval practice. They persist through theater (performative appeals to Classical authority) rather than through functional superiority. Condemned to condemn medieval scribes while offering no viable path forward.
narrative_ontology:constraint_stakeholder(continuity_reading, purist_defenders, payer,
    powerless, biographical, trapped, global).

% Renaissance humanist scholars and printing press operators who revived Classical Latin as a prestige standard in the 15th-16th centuries. They benefit from the purist reading by establishing Classical purity as a marker of elite education and intellectual status. They set the agenda by promoting Classical Latin through education and printing. Their revival was partly functional (establishing a common learned language) but increasingly performative (theater of Classical purity). They maintain the purist reading through institutional inertia and educational theater rather than through functional necessity.
narrative_ontology:constraint_stakeholder(continuity_reading, humanist_revival, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(continuity_reading, humanist_revival, beneficiary).

% The natural process of language evolution (sound changes, analogical reanalysis, semantic drift) that produces medieval Latin forms from Classical Latin. This is not an agent but a natural process. The continuity reading risks naturalizing the normative claim that this process legitimates medieval forms as if it were a law of nature rather than a contestable reading.
narrative_ontology:constraint_stakeholder(continuity_reading, linguistic_evolution_process, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(continuity_reading, linguistic_evolution_process).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintain Latin as a unified language across centuries of natural linguistic evolution, enabling the Church to preserve Latin as a living liturgical language without the impossible burden of Classical purity.
% TRANSFER_FUNCTION: The continuity reading transfers legitimacy from medieval scribes to continuity defenders and the ecclesiastical tradition. Medieval scribes' actual practice is defended as legitimate evolution, which benefits the tradition and the defenders but extracts from the scribes by positioning them as needing defense rather than as practitioners of a legitimate form.
% ABSENT_VOICES: Classical Latin speakers (extinct) cannot object to the reading because they are dead. Vernacular speakers are excluded from the conversation because ecclesiastical authority requires Latin. Linguistic descriptivists who would argue that medieval forms are simply evolved, not 'legitimate' or 'corrupted,' are absent from the normative debate.
% DISAPPEARANCE_RATIONALE: If the continuity reading disappeared, the ecclesiastical tradition would need to either adopt Classical purity (impossible without living speakers) or adopt vernacular languages (contrary to ecclesiastical authority). The world would rearrange itself by fragmenting Latin into regional variants or replacing it with vernaculars. However, purist defenders would argue that the world would simply return to correct Latin (Classical), which is a contested claim.
% FOUNDING_PROBLEM: How to maintain Latin as a unified language across centuries when the language naturally evolves through regular linguistic processes. The continuity reading solves this by legitimating evolved forms as legitimate evolution rather than corruption.
% FOUNDING_PROBLEM_CORROBORATION: The ecclesiastical tradition attests that the founding problem is live: Latin must function as a liturgical language across diverse regions and centuries, and natural linguistic evolution makes Classical purity impossible. Medieval scribes attest that the problem is live: they must write in Latin but cannot write in Classical Latin. Purist defenders attest that the problem is live but claim the solution is wrong: they argue that correct Latin is Classical Latin, not evolved medieval forms. The founding problem is attested by all parties; they disagree on the solution.
narrative_ontology:disappearance_verdict(continuity_reading, contested).
narrative_ontology:founding_problem_status(continuity_reading, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL SCRIBE (SNARE) — Trapped in a linguistic regime where their actual practice (medieval Latin forms, phonetic spellings, evolved syntax) is simultaneously necessary for communication and condemned as corruption. No exit: they cannot write in Classical Latin (the language is dead) nor can they write in vernacular (ecclesiastical authority requires Latin). Purist judgment extracts legitimacy from their work while denying its validity. Maximum extraction from the powerless position.
constraint_indexing:constraint_classification(continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CONTINUITY DOCTRINE DEFENDER (TANGLED ROPE) — Moderate power (scholars, ecclesiastical authorities defending medieval practice as legitimate evolution). Benefits from the continuity reading: it validates their actual linguistic practice and provides intellectual cover for medieval Latin as a legitimate form. But constrained by the need to maintain coordination with Classical authorities and by the threat of purist counter-claims. Active enforcement required: must continuously argue that medieval forms are evolved, not corrupted.
constraint_indexing:constraint_classification(continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: LIVING ECCLESIASTICAL TRADITION (ROPE) — Institutional beneficiary with arbitrage options. The continuity reading enables the Church to maintain Latin as a living liturgical language across centuries without the impossible burden of Classical purity. The tradition benefits from the reading because it solves a genuine coordination problem: how to preserve Latin as a unified language across generations when the language naturally evolves. The tradition can arbitrage between strict Classical forms (in formal documents) and evolved forms (in daily practice).
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLASSICAL PURIST TRADITION (SNARE) — Trapped in a losing position. The purist reading asserts that correct Latin is Classical Latin, but this reading is structurally indefensible: Classical Latin is a dead language with no living speakers. The purist position extracts legitimacy from medieval scribes (by condemning their practice) while offering no viable alternative. Trapped because the purist cannot actually enforce Classical purity without destroying the living tradition entirely. The purist position persists through theater (performative appeals to Classical authority) rather than function.
constraint_indexing:constraint_classification(continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: RENAISSANCE HUMANIST REVIVAL (PITON) — Institutional actors (humanist scholars, printing press operators) who revived Classical Latin as a prestige standard in the 15th-16th centuries. This revival was partly functional (establishing a common learned language across Europe) but increasingly performative (the theater of Classical purity as a marker of elite education). The humanist position is a piton: it maintains the purist reading through institutional inertia and educational theater, not because Classical purity is actually achievable or necessary for communication.
constraint_indexing:constraint_classification(continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, language evolution is an immutable natural law: all living languages change over time through regular sound changes, analogical reanalysis, and semantic drift. Medieval Latin forms are not corruptions but predictable outputs of the same processes that created Classical Latin from earlier Italic dialects. This perspective risks naturalizing what is actually a contestable reading: the claim that medieval forms are 'legitimate' because they follow natural language evolution is a normative claim, not a descriptive law.
constraint_indexing:constraint_classification(continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(continuity_reading, TR),
    TR >= 0.70.

:- end_tests(continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The continuity reading extracts legitimacy from medieval scribes by positioning their practice as 'evolved' rather than 'corrupted,' but this extraction is not as severe as pure snare because the reading also solves a genuine coordination problem (maintaining Latin as a living language). The extraction increases over time (0.25 → 0.35) as the reading becomes institutionalized and requires more performative assertion. Suppression (0.42): Moderate. Medieval scribes face suppression from purist attacks on their practice, but the suppression is not total because the continuity reading provides intellectual cover. Suppression increases over time (0.30 → 0.42) as the Renaissance humanist revival strengthens purist institutional power, forcing continuity defenders to argue more forcefully. Theater ratio (0.58): Moderate-high. The continuity reading requires constant performative assertion that medieval forms are 'evolved' rather than 'corrupted' — the distinction is normative, not empirically observable. Theater increases over time (0.35 → 0.58) as the reading becomes more institutionalized and requires more rhetorical work to maintain against purist counter-claims.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal in this constraint. Medieval scribes experience snare (trapped, condemned, no exit). Continuity defenders experience tangled rope (mixed coordination and extraction). The ecclesiastical tradition experiences rope (genuine coordination benefit). Purist defenders experience snare (trapped in a losing position). Humanist revivalists experience piton (maintaining a degraded standard through theater). The analytical observer risks mountain (naturalizing a normative claim as linguistic law). The gap reveals that the same linguistic phenomenon (medieval Latin forms) is classified as legitimate evolution by some perspectives and corruption by others — the classification depends entirely on the normative framework (continuity vs discontinuity) rather than on observable linguistic facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the constraint. Medieval scribes are victims (d ≈ 0.95): they bear the cost of being condemned while their practice is defended. Continuity defenders are beneficiaries (d ≈ 0.15): they benefit from the reading by validating their intellectual position. The ecclesiastical tradition is a strong beneficiary (d ≈ 0.05): it solves a genuine coordination problem. Purist defenders are victims (d ≈ 0.85): they are trapped in a losing position. Humanist revivalists are beneficiaries (d ≈ 0.20): they benefit from the prestige of Classical purity. The analytical observer is neutral (d ≈ 0.50): they see the structure from outside. The engine computes effective extraction (χ) from d and the constraint's scope; larger scope (continental/global) amplifies extraction for victims and damps it for beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading resolves mandatrophy by showing that the constraint's mandate (maintain Latin as a living language) has NOT outlived its function — the ecclesiastical tradition still uses Latin liturgically, and the continuity reading still solves the coordination problem of how to do so without impossible Classical purity. However, the reading exhibits mandatrophy symptoms: the performative assertion that medieval forms are 'evolved' rather than 'corrupted' has become increasingly theatrical (theater_ratio rising from 0.35 to 0.58) as the reading becomes institutionalized. The constraint is not a piton (the mandate is still live) but shows piton-like features (increasing theater). The mandatrophy is resolved by recognizing that the constraint solves a real coordination problem, but the solution requires constant performative work to maintain against purist counter-claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is ''correct Latin'' defined by historical continuity (this reading) or by fidelity to a fixed Classical standard (discontinuity reading) or by a hybrid that acknowledges both?',
    'Examination of how medieval and Renaissance authorities actually justified their linguistic choices: did they appeal to continuity with living practice, or to Classical authority, or to both? Analysis of which justification was more persuasive in different contexts.',
    'If continuity is the primary justification: this reading is structurally sound. If Classical authority is primary: the discontinuity reading is more accurate. If both are invoked: the hybrid reading captures the actual practice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether correct Latin is defined by continuity, Classical standard, or hybrid').

omega_variable(
    legitimacy_grounding_shift,
    'Does the continuity reading ground legitimacy in actual practice (descriptive) or in a normative claim that practice-based evolution is legitimate (prescriptive)?',
    'Textual analysis of continuity defenders: do they describe medieval forms as evolved (descriptive) or argue they should be accepted as correct (prescriptive)? The shift from description to prescription marks the reading''s normative content.',
    'If primarily descriptive: the reading is a linguistic observation. If primarily prescriptive: the reading is a legitimacy claim that requires enforcement (tangled rope structure confirmed). If mixed: the reading conflates description and prescription, which is the core of the mandatrophy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_grounding_shift, conceptual, 'Whether continuity reading is descriptive or prescriptive').

omega_variable(
    purist_counter_extraction,
    'Does the purist reading extract legitimacy from medieval scribes by condemning their practice, or does it offer a genuine alternative standard?',
    'Historical analysis: did purist authorities offer medieval scribes a viable path to Classical purity, or did they simply condemn medieval practice without offering alternatives? If no viable alternative, the purist position is extractive (snare). If alternatives were offered, the purist position is a competing coordination mechanism (rope or tangled rope).',
    'If extractive: the purist reading is a snare that persists through institutional power, not through functional superiority. If competitive: both readings are live options with different trade-offs. This determines whether the purist perspective is snare or rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(purist_counter_extraction, empirical, 'Whether purist reading extracts or offers genuine alternative').

omega_variable(
    sibling_reading_foreclosure,
    'Does the continuity reading logically foreclose the discontinuity reading, or do they coexist as different normative frameworks?',
    'Logical analysis: if medieval forms are legitimate because they evolved continuously from Classical Latin, does this rule out the claim that correct Latin is only Classical Latin? Or can both claims be held simultaneously by different parties (continuity defenders vs purist defenders)? The answer determines the reading_relations type.',
    'If foreclosure: the readings are in logical contradiction and cannot coexist in a single framework. If coexistence: the readings are different normative choices that different parties can hold. This affects the cs_structure.reading_relations classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether continuity reading forecloses discontinuity reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_theater_t0, continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cont_theater_t3, continuity_reading, theater_ratio, 3, 0.45).
narrative_ontology:measurement(cont_theater_t6, continuity_reading, theater_ratio, 6, 0.55).
narrative_ontology:measurement(cont_theater_t10, continuity_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(cont_extract_t0, continuity_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(cont_extract_t3, continuity_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(cont_extract_t6, continuity_reading, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(cont_extract_t10, continuity_reading, base_extractiveness, 10, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cont_supp_t0, continuity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cont_supp_t3, continuity_reading, suppression_requirement, 3, 0.38).
narrative_ontology:measurement(cont_supp_t6, continuity_reading, suppression_requirement, 6, 0.42).
narrative_ontology:measurement(cont_supp_t10, continuity_reading, suppression_requirement, 10, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(continuity_reading, discontinuity_reading).
narrative_ontology:affects_constraint(continuity_reading, hybrid_reading).
narrative_ontology:affects_constraint(continuity_reading, classical_purist_authority).
narrative_ontology:affects_constraint(continuity_reading, medieval_ecclesiastical_practice).

% DUAL FORMULATION NOTE:
% The continuity reading is one of three readings of the kernel 'correct_latin'. Each reading has its own constraint story with its own ε value and perspectives. The discontinuity reading (correct Latin is only Classical Latin) has higher suppression and lower coordination function. The hybrid reading (both Classical and medieval are legitimate in context) has lower extractiveness and lower theater. The three readings are linked by network.affects_constraints to show the kernel family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(continuity_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
