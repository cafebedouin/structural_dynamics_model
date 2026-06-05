% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Statehood Criteria (Declaratory Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention on the Rights and Duties of States (1933)
 *   established four objective criteria for statehood: a defined territory, a
 *   permanent population, a government, and the capacity to conduct foreign
 *   relations. The declaratory reading of these criteria holds that meeting
 *   all four criteria establishes statehood as a matter of fact, not as a
 *   matter of external recognition. This reading reverses historical
 *   practice: previously, statehood was a political grant awarded by major
 *   powers through recognition. Under the declaratory reading, recognition
 *   becomes declaratory (announcing an existing fact) rather than
 *   constitutive (creating a new entity). This constraint illustrates the
 *   committer-frame structure: the same legal text — the Montevideo criteria
 *   — can be read as (1) an objective standard that makes statehood
 *   self-executing (declaratory), (2) a descriptive standard of what
 *   statehood looks like but requiring recognition to create it
 *   (constitutive), or (3) a hybrid in which criteria establish a default
 *   that recognition can override. Each reading reorganizes the
 *   beneficiary/victim structure. The declaratory reading benefits de facto
 *   authorities meeting the criteria and extracts from parent states or
 *   occupying powers. It also threatens the great-power discretion
 *   historically exercised through recognition withholding.
 *
 * KEY AGENTS:
 *   - De Facto Authorities Meeting Criteria: Primary beneficiary (powerful/mobile) — entities like Kosovo, Palestinian Authority, Turkish Republic of Northern Cyprus gain legal standing if they satisfy the objective Montevideo standard. The declaratory reading grants them statehood as a legal fact, independent of parent state or major-power consent.
 *   - Parent States or Occupying Powers: Primary victim (powerful/trapped) — states that control territory inhabited by separatist polities lose the structural ability to withhold recognition. The parent state's political leverage is constrained by the declaratory logic: if the criteria are met, statehood follows as a matter of law, not negotiation.
 *   - International Legal System and UN Consensus: Institutional victim/beneficiary (institutional/constrained) — the system gains objectivity and legitimacy through criteria-based entry (coordination function), but loses flexibility by removing the consensus requirement. Major powers lose discretion to condition recognition on political alignment.
 *   - Great Powers (Historical Framework): Institutional actor experiencing degradation (institutional/arbitrage) — the constraint degrades the great-power practice of using recognition as a political tool. Theater persists (0.52) because states continue to invoke subsidiary grounds for withholding recognition despite the declaratory criteria.
 *   - Decolonial and Subaltern State Movement: Organized beneficiary (organized/mobile) — postcolonial coalitions benefit from criteria-based entry that forecloses the historical great-power veto. However, the Montevideo criteria themselves encode European state-form assumptions, creating extraction pressure to conform to imported institutional models.
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — at risk of naturalizing a constructed institutional convention as immutable law. The 'objective criteria' framing can obscure that the Montevideo standard is a specific historical choice benefiting specific actors.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.38).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.45).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Statehood Criteria (Declaratory Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '6c2e662d-06be-44e2-ae44-d39e5f52268b').
narrative_ontology:cs_kernel_codification('6c2e662d-06be-44e2-ae44-d39e5f52268b', formalized).
narrative_ontology:cs_authority_grounding('6c2e662d-06be-44e2-ae44-d39e5f52268b', lineage).
narrative_ontology:cs_interpretation_layer_present('6c2e662d-06be-44e2-ae44-d39e5f52268b').
narrative_ontology:cs_reading_relation('6c2e662d-06be-44e2-ae44-d39e5f52268b', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('6c2e662d-06be-44e2-ae44-d39e5f52268b', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('6c2e662d-06be-44e2-ae44-d39e5f52268b', foundational, objective_criteria_self_constitute_statehood).
narrative_ontology:cs_axiom_status(objective_criteria_self_constitute_statehood, holdable).
narrative_ontology:cs_axiom_grounding('6c2e662d-06be-44e2-ae44-d39e5f52268b', objective_criteria_self_constitute_statehood, deontological).
narrative_ontology:cs_axiom('6c2e662d-06be-44e2-ae44-d39e5f52268b', foundational, recognition_is_declaratory_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_is_declaratory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('6c2e662d-06be-44e2-ae44-d39e5f52268b', recognition_is_declaratory_not_constitutive, deontological).
narrative_ontology:cs_reference_frame('6c2e662d-06be-44e2-ae44-d39e5f52268b', objective_criteria_determination_of_statehood).
narrative_ontology:cs_drift_state('6c2e662d-06be-44e2-ae44-d39e5f52268b', contemporary_post_cold_war_international_system, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('6c2e662d-06be-44e2-ae44-d39e5f52268b', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities_meeting_criteria).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states_or_occupying_powers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, international_law_discretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DE FACTO AUTHORITY MEETING CRITERIA (ROPE) — A polity that controls defined territory, has a functioning government, can conduct foreign relations, and demonstrates population independence possesses statehood as a matter of objective law under the declaratory reading. The constraint is purely coordinative: the Montevideo criteria establish what counts as entry into the international legal system. The de facto authority benefits from this recognition as a natural consequence of meeting objective standards, not as a discretionary grant. Extraction is minimal — the authority gains entry to an existing coordinate system.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: PARENT STATE OR OCCUPYING POWER (SNARE) — Under the declaratory reading, a parent state loses the structural ability to withhold recognition from a de facto authority that meets the objective criteria. This is an extraction mechanism: the parent state's political leverage is constrained by law. The parent cannot maintain suzerainty or prevent international standing through diplomatic pressure alone. The constraint operates as pure extraction from the parent's perspective — it forecloses discretionary withholding of recognition. The parent state is trapped by the declaratory logic: objective fact replaces diplomatic negotiation.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL LEGAL SYSTEM AND UN CONSENSUS (TANGLED ROPE) — The declaratory reading entails that the international legal system both coordinates and extracts. It coordinates by establishing objective criteria for entry into the legal system (shared standard, predictable adjudication). It extracts by removing the consensus requirement from recognition decisions — individual states lose discretion to condition recognition on political preferences. The UN system, which operates on consensus, experiences this constraint as requiring recognition of entities that meet criteria even when major powers object. Extraction runs bidirectionally: the system gains legitimacy through objectivity, but loses flexibility through self-binding.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: HISTORICAL GREAT-POWER DISCRETION FRAMEWORK (PITON) — Historically, great powers used recognition as a political tool, conditioning it on alignment, resources, or geopolitical advantage. The declaratory reading degrades this mechanism by replacing discretion with criteria. The theater is high (0.52): states continue to withhold recognition from entities meeting criteria by invoking subsidiary grounds (disputed territory status, human rights concerns, legitimacy of the governing authority), performatively preserving their discretionary role even as the declaratory reading constrains it. The constraint persists through institutional inertia — the great-power preference for discretion — but its primary function (political leverage through recognition) has atrophied.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the declaratory reading risks naturalizing a specific legal convention as an immutable fact of international order. The analytical observer sees statehood as emerging naturally from objective criteria — defined territory, functioning government, capacity for foreign relations, population — as though these criteria reflect discoverable law rather than a constructed institutional framework. However, the structural data reveals this as a false summit: the Montevideo criteria are a mid-20th-century codification designed by specific actors (the Organization of American States) to solve specific problems (recognition politics in the Americas). The 'objective criteria' framing naturalizes what is actually a contestable legal convention.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DECOLONIAL AND SUBALTERN STATE MOVEMENT (TANGLED ROPE) — Organized coalitions of postcolonial and subaltern states benefit from the declaratory reading because it forecloses the historical great-power veto on recognition — criteria-based entry replaces consensus-based exclusion. However, the constraint also extracts: the Montevideo criteria themselves encode European state-form assumptions (defined borders, bureaucratic government, written laws) that subaltern polities may not meet or may reject as imposed standards. The constraint coordinates entry into the international legal system while simultaneously requiring conformity to a specific institutional model. Exit options are mobile — these states can operate outside the formal system or push to revise the criteria — but the pressure to meet them is substantial.
constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(montevideo_statehood_criteria__declaratory_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, TR),
    TR >= 0.70.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The declaratory reading produces mixed extraction flows. De facto authorities gain statehood as a legal fact (positive extraction — they benefit from a rule that was previously discretionary). Parent states lose the ability to withhold recognition (extraction against them — their political leverage is constrained). The measurement trajectory (0.15 → 0.38 over 40 years) reflects increasing consolidation of the declaratory norm: as more states internalize the criteria as binding, the extraction against parent states increases. Suppression (0.45): Moderate. The barrier to recognizing a criterion-meeting entity under the declaratory reading is not absolute — major powers continue to find grounds to withhold recognition (contesting population, territorial definition, or government legitimacy), invoking subsidiary doctrine. The suppression reflects states' continued use of discretionary withholding despite the declaratory framework. Theater ratio (0.52): Moderate-high, increasing. The theater reflects the performative dimension of recognition politics: states invoke doctrinal objections (disputed territory, illegitimate government, international law violations) to justify withholding recognition from criterion-meeting entities, preserving the appearance of discretion even as the declaratory reading constrains it. Theater increases over time as the declaratory norm solidifies and discretion becomes harder to defend openly.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a dramatic perspectival gap. The de facto authority sees a rope (pure coordination: meeting criteria grants entry as a matter of law). The parent state sees a snare (extraction through loss of discretionary veto). The international legal system sees a tangled rope (coordination through objectivity, but extraction through loss of flexibility). The great-power discretion framework sees a piton (degraded mechanism, maintained through theater). The analytical observer risks seeing a mountain (natural law of political organization), but structural analysis reveals a false summit — the 'objective criteria' naturalize a constructed institutional choice. The decolonial movement sees tangled rope (coordination through criteria-based entry, extraction through conformity requirements to European state-form). These perspectives are not observational variations on a single constraint — they reflect the reading's actual structural logic: the declaratory reading reorganizes who benefits and who bears costs compared to the constitutive or hybrid readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) for the declaratory reading derive from the structural reorganization the reading produces: De facto authorities meeting criteria: d ≈ 0.2 (beneficiary status, mobile exit → low d → negative effective extraction). Parent states or occupying powers: d ≈ 0.85 (victim status, trapped exit → high d → high f(d) → high experienced extraction). International legal system: d ≈ 0.5 (both benefits from objectivity and costs from lost flexibility → symmetric). Great powers: d ≈ 0.75 (victim of discretion loss, though arbitrage exit option means d is not maximum). Decolonial movement: d ≈ 0.55 (organized beneficiary of criteria-based entry, but victim of conformity pressure → moderate d). The analytical observer: d ≈ 0.72 (standard analytical position, observing all structural flows).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not resolve mandatrophy in a single reading — it INSTANTIATES mandatrophy at the kernel level. The Montevideo kernel permits three substantively different readings (declaratory, constitutive, hybrid), each producing different classifications from the same material facts. The declaratory reading produces tangled rope as the primary classification (primary beneficiary perspective sees rope, other perspectives see snare/piton). The constitutive reading would produce different classifications by reversing beneficiary/victim roles. The hybrid reading would place the constraint at a different position on the extraction dimension. The mandatrophy is not resolved by choosing one reading — it is resolved by recognizing that the constraint's classification IS READING-RELATIVE. The engine's task is not to determine 'the' correct classification but to compute the classification for EACH reading and document how they diverge. The omegas flag the key ambiguities that distinguish the readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declaratory_constitutive_boundary,
    'Does meeting the Montevideo criteria logically entail statehood under international law, or does statehood require external recognition as a constitutive act?',
    'Historical case analysis: do de facto authorities that meet all four criteria but lack major-power recognition acquire enforceable legal status? Examples: Turkish Republic of Northern Cyprus, Palestinian Authority, Kosovo post-2008. If such entities can enforce legal claims without formal recognition, the declaratory reading is confirmed; if their claims remain unenforceable absent consensus recognition, the constitutive reading is confirmed.',
    'If declaratory: the constraint is a rope (coordination mechanism); parent states lose structural leverage; international law becomes self-executing. If constitutive: the constraint is negotiable; parent states retain effective veto through recognition withholding; international law remains consensus-dependent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_constitutive_boundary, empirical, 'Whether meeting Montevideo criteria logically entails statehood or requires external recognition').

omega_variable(
    criteria_universality_vs_europeanization,
    'Are the four Montevideo criteria universal standards for political organization, or do they encode specifically European/Western state-form assumptions that exclude or penalize alternative political structures?',
    'Structural analysis of ''defined territory,'' ''permanent population,'' ''government,'' and ''capacity for foreign relations'' against non-Western polities (pastoralist confederations, stateless societies, network-based governance, indigenous commons). Empirical question: can these criteria be met by polities that do not conform to Weberian state-form? Do stateless societies that satisfy functional equivalents of these criteria gain recognition?',
    'If criteria are culturally specific: the declaratory reading exports a particular institutional model as universal law, creating extraction pressure on subaltern polities to conform. If criteria can be satisfied by diverse forms: the reading remains open to diverse political organization. The extraction magnitude and victim set depend on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(criteria_universality_vs_europeanization, empirical, 'Whether Montevideo criteria are universal or encode European state-form assumptions').

omega_variable(
    recognition_gap_enforcement_mechanism,
    'What enforcement mechanism makes recognition binding on actors that contest it? If a major power refuses to recognize an entity meeting all criteria, what compels that power to accept the recognition as valid?',
    'Examination of cases where major powers withheld recognition from criterion-meeting entities (Turkish Republic of Northern Cyprus, Kosovo before 2008, Palestinian Authority). Did the declaratory reading compel recognition? Or did recognition require political negotiation despite objective criteria? Track enforcement outcomes: treaty ratification barriers, security council vetoes, UN seat blockage.',
    'If declaratory reading is self-enforcing: the constraint is rope (coordination with automatic entry). If enforcement requires consensus or major-power acceptance: the constraint is tangled_rope or snare (extraction that extracts through recognition withholding). The difference is structural: does meeting criteria automatically grant rights, or merely establish a legal claim that requires political acceptance?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_gap_enforcement_mechanism, empirical, 'What mechanism enforces recognition binding under the declaratory reading').

omega_variable(
    false_summit_natural_law_frame,
    'Is the declaratory reading genuinely a discovery of natural law (objective criteria that constitute statehood), or is it a constructed institutional convention designed to serve specific interests in the international legal system?',
    'Genealogical analysis: who benefited from the declaratory reading when the Montevideo Convention (1933) was adopted? Did the organizational states (primarily Latin American republics) gain by establishing objective criteria that overrode European great-power discretion? Does the ''objectivity'' of the criteria track the political interests of the actors who established them?',
    'If natural law: mountain classification is appropriate; the criteria reflect immutable features of political organization. If constructed convention: false summit triggers; the mountain perspective naturalizes a contestable institutional choice that benefits identifiable actors (postcolonial states, small powers, subaltern movements).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_frame, conceptual, 'Whether declaratory reading reflects natural law or constructed institutional convention').

omega_variable(
    reading_kernel_divergence_implications,
    'This constraint instantiates the declaratory reading of the Montevideo statehood kernel. What structural consequences follow if the constitutive or hybrid readings are adopted instead?',
    'Comparative omega analysis across sibling constraint stories (montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading). If constitutive reading is correct: de facto authorities meeting criteria remain victims of recognition withholding; extraction continues through discretionary denial; parent states retain leverage. If hybrid reading is correct: statehood emerges from criteria but requires threshold acceptance to become binding; the constraint becomes negotiated tangled_rope rather than structural rope or snare.',
    'The declaratory reading assigns different beneficiary/victim roles than its siblings. Clarifying which reading is correct determines whether the constraint extracts from parent states (declaratory) or from de facto authorities (constitutive) or both (hybrid). The omega documents that this reading''s classification is reading-relative, not objective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_divergence_implications, conceptual, 'How sibling readings change the constraint''s beneficiary/victim structure and extraction direction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mont_tr_t20, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement(mont_tr_t40, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(mont_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mont_be_t20, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(mont_be_t40, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, recognition_discretion_great_power_veto).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, separatist_legitimacy_doctrine).

% DUAL FORMULATION NOTE:
% The Montevideo kernel admits three structurally distinct constraint readings. The declaratory reading (this file) posits that meeting criteria establishes statehood as a legal fact. The constitutive reading posits that criteria are descriptive, requiring recognition to create statehood. The hybrid reading posits that criteria establish a default that recognition can override. Each reading has its own ε, beneficiary/victim structure, and primary classification. All three are linked via network.affects_constraints to show their kinship and mutual influence. Decomposition: The constraint's ε value under each reading differs significantly (declaratory ≈ 0.38, constitutive ≈ 0.55–0.65, hybrid ≈ 0.45–0.50), reflecting different extraction structures. This is NOT an observable-dependent measurement of one constraint — these are three different constraints instantiated by three readings of one kernel. The ε-invariance principle applies: each reading gets its own constraint story with its own stable ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
