% ============================================================================
% CONSTRAINT STORY: first_amendment_1951__speech_grounds_expansion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_1951__speech_grounds_expansion_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: first_amendment_1951__speech_grounds_expansion_reading
 *   human_readable: First Amendment Speech Grounds Expansion (1951) — Suppression Enlargement Reading
 *   domain: constitutional_law/doctrinal
 *
 * SUMMARY:
 *   The First Amendment to the Indian Constitution (1951) amended Article 19
 *   of the Ambedkar Constitution to add 'public order' and 'friendly
 *   relations with foreign states' as grounds for restricting free speech and
 *   expression. This reading instantiates the constraint as experienced by
 *   those who relied on the early, broader interpretation of Article 19 and
 *   now face enlarged grounds for prosecution. The amendment appears as
 *   retroactive narrowing of a foundational liberty — the speakers who were
 *   acquitted under the broad reading now face jeopardy under the new narrow
 *   reading. The constraint is a tangled rope: it coordinates legal standards
 *   (provides clear grounds for regulation) while simultaneously extracting
 *   from the speech community (reduces the scope of protected expression,
 *   enlarges the scope of prosecution). The beneficiary is the public-order
 *   prosecution authority; the victim set includes both the early
 *   broad-speech precedents (invalidated retroactively) and persons whose
 *   speech now falls under the expanded prohibition. This reading does NOT
 *   ask whether the amendment was wise policy or whether public-order
 *   concerns justify the narrowing — it models the structural constraint that
 *   the amendment created. The sibling readings
 *   (founders_amending_founders_reading, ninth_schedule_immunity_reading)
 *   emphasize different aspects of the same amendment and coexist as
 *   legitimate political framings within the constitutional order.
 *
 * KEY AGENTS:
 *   - Early Acquitted Speaker: Primary victim (powerless/trapped) — relied on broad speech precedent; now faces prosecution under amended grounds
 *   - Public-Order Prosecution Authority: Primary beneficiary (institutional/arbitrage) — gains enlarged prosecutorial grounds and discretionary enforcement capacity
 *   - Constitutional Court: Institutional actor (institutional/constrained) — enforces the amendment; experiences tension between honoring earlier precedent and applying new constitutional text
 *   - Speech Advocate Community: Secondary victim (moderate/constrained) — faces increased legal exposure; constrained exit (can withdraw but lose voice, or continue and face jeopardy)
 *   - Ambedkar Founding Vision: Vestigial reference (institutional/arbitrage) — early foundational rhetoric claims broad speech as essential; now contradicted by amendment (piton perspective)
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the amendment as immutable law rather than a political choice with identifiable winners and losers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_1951__speech_grounds_expansion_reading, 0.58).
domain_priors:suppression_score(first_amendment_1951__speech_grounds_expansion_reading, 0.68).
domain_priors:theater_ratio(first_amendment_1951__speech_grounds_expansion_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_1951__speech_grounds_expansion_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_amendment_1951__speech_grounds_expansion_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(first_amendment_1951__speech_grounds_expansion_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_1951__speech_grounds_expansion_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_1951__speech_grounds_expansion_reading, "First Amendment Speech Grounds Expansion (1951) — Suppression Enlargement Reading").
narrative_ontology:topic_domain(first_amendment_1951__speech_grounds_expansion_reading, "constitutional_law/doctrinal").

domain_priors:requires_active_enforcement(first_amendment_1951__speech_grounds_expansion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_1951__speech_grounds_expansion_reading, '3ef262a5-8914-411d-9fb5-2d01eac7f8ac').
narrative_ontology:cs_kernel_codification('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', formalized).
narrative_ontology:cs_authority_grounding('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', extraction).
narrative_ontology:cs_interpretation_layer_present('3ef262a5-8914-411d-9fb5-2d01eac7f8ac').
narrative_ontology:cs_reading_relation('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', first_amendment_1951__founders_amending_founders_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', first_amendment_1951__ninth_schedule_immunity_reading, coexists_with).
narrative_ontology:cs_axiom('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', foundational, public_order_narrows_speech).
narrative_ontology:cs_axiom_status(public_order_narrows_speech, holdable).
narrative_ontology:cs_axiom_grounding('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', public_order_narrows_speech, empirically_contingent).
narrative_ontology:cs_axiom('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', foundational, amendment_benefits_state_authority).
narrative_ontology:cs_axiom_status(amendment_benefits_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', amendment_benefits_state_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', broad_speech_protection_regime).
narrative_ontology:cs_drift_state('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', post_first_amendment_1951, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3ef262a5-8914-411d-9fb5-2d01eac7f8ac', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(first_amendment_1951__speech_grounds_expansion_reading, first_amendment_1951).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_1951__speech_grounds_expansion_reading, public_order_prosecution).
narrative_ontology:constraint_beneficiary(first_amendment_1951__speech_grounds_expansion_reading, state_authority_over_expression).
narrative_ontology:constraint_victim(first_amendment_1951__speech_grounds_expansion_reading, early_broad_speech_precedents).
narrative_ontology:constraint_victim(first_amendment_1951__speech_grounds_expansion_reading, persons_advocating_suppressed_grounds).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY ACQUITTED SPEAKER (SNARE) — Speakers who relied on early broad-speech precedents now face enlarged grounds for prosecution (public order, friendly relations with foreign powers). No exit option: the precedent that shielded them has been retroactively narrowed by constitutional amendment. Maximum extraction experienced — caught by rules they did not know were coming.
constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTRAINED SPEECH ADVOCATE (TANGLED ROPE) — Persons and groups advocating for broad speech rights experience both coordination function (the constraint establishes a shared legal framework for discourse) and asymmetric extraction (the framework is now narrower than before, extraction has increased, career risk and legal exposure rise). Constrained exit: can withdraw from advocacy but loses political voice; can continue but faces legal jeopardy.
constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC-ORDER PROSECUTION (ROPE) — State authorities conducting prosecutions under the expanded speech grounds experience the constraint as pure coordination: the amendment clarified legal grounds previously contested. Arbitrage exit: can choose which speech to prosecute; enforcement is discretionary within the new grounds. Net beneficiary — the expanded grounds increase prosecution capacity.
constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL COURT (TANGLED ROPE) — Courts enforcing the amended Article 19(2) experience both coordination (establishing consistent legal standards for speech review) and extraction (they must retroactively apply narrower standards to cases decided under broader precedent, invalidating earlier acquittals, reversing prior interpretations of constitutional liberty). Constrained by the constitutional text itself — cannot simply ignore the amendment.
constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FOUNDATIONAL LIBERTY NARRATIVE (PITON) — The broader speech reading was treated as foundational to Ambedkar's constitutional vision (Part III, Articles 19-22 on fundamental rights). The First Amendment's narrowing appears as a betrayal of the founding. Yet the 'founding vision' becomes increasingly theatrical — the narrow reading is now the law, the broad reading persists as ideological rhetoric, not binding doctrine. Theater ratio increases as advocates invoke the early precedents knowing they no longer bind courts.
constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, constitutional amendments are the apex of sovereign authority — once adopted through proper process, the amendment state IS the law. No appeal beyond the people's amendment. The narrowed grounds are not extraction but the rectified foundation. This perspective treats the amendment as immutable, natural law from the body politic. However, the structural data (identified beneficiaries, suppression mechanism, retroactive narrowing) reveals this as a false summit: naturalizing a political choice as immutable law.
constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_1951__speech_grounds_expansion_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_amendment_1951__speech_grounds_expansion_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_1951__speech_grounds_expansion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_amendment_1951__speech_grounds_expansion_reading, TR),
    TR >= 0.70.

:- end_tests(first_amendment_1951__speech_grounds_expansion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The amendment does not extract pure rent — it coordinates regulation around identifiable grounds (public order, friendly relations). But the coordination is asymmetric: the grounds were chosen to expand prosecutorial capacity, not to clarify existing doctrine. The measurement trajectory (0.35 → 0.48 → 0.58) shows extractiveness accumulating as prosecutions under the new grounds increase and retroactive enforcement against prior acquittals begins. The final value of 0.58 reflects that the constraint is real coordination (courts apply consistent standards) layered over extraction (the standards are narrower than before, asymmetrically benefiting state authority). Suppression (0.68): High. The expanded grounds themselves are a form of suppression — they reduce the scope of protected expression. But suppression is not total — speech remains protected within the narrowed bounds. The measurement trajectory (0.45 → 0.60 → 0.68) reflects increasing enforcement capacity and willingness to apply the expanded grounds. Theater ratio (0.52): Moderate. Early foundational rhetoric invokes the broad Ambedkar vision of speech, but that rhetoric no longer binds courts — it becomes performance for the speech community. Prosecution authority experiences the amendment as clarification, not theater. The moderate theater ratio reflects that the constraint is partly functional (actual regulation) and partly performative (invocation of the founding text that no longer constrains).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gaps are the analytical core. The prosecution authority sees coordination (Rope) — the amendment clarified legal grounds. The early acquitted speaker sees extraction (Snare) — they lost protection retroactively. The speech advocate community sees mixed extraction and coordination (Tangled Rope) — the constraint regulates speech clearly but the regulation is narrower than before. The constitutional court sees enforcement of both founding commitment and amendment (Tangled Rope) — coordination through consistent standards, extraction through narrowing prior liberty. The foundational liberty narrative sees betrayal of the founding (Piton) — the rhetoric persists but no longer binds, theater increases. The analytical observer at civilizational scale risks seeing the amendment as immutable natural law (Mountain) — the sovereign people spoke through amendment, no appeal possible — but the structural data reveals this as a false summit: the 'immutability' naturalizes a political choice, the 'sovereign people' is a construction legitimizing state authority expansion.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation reflects the structural asymmetry created by the amendment. The public-order prosecution authority (institutional/arbitrage) derives d from beneficiary status (gains enlarged grounds) and arbitrage exit (can choose which speech to prosecute, enforcement is discretionary) — low d, negative f(d), net benefit. The early acquitted speaker (powerless/trapped) derives d from victim status (lost earlier protection) and trapped exit (cannot exit the amended constitutional order) — high d, high f(d), maximum extraction. The speech advocate community (moderate/constrained) derives d from victim status (enlarged jeopardy) and constrained exit (can withdraw but loses voice) — moderate-high d, moderate-high f(d), moderate extraction. The constitutional court (institutional/constrained) derives d from mixed status: enforcer of the amendment (beneficiary logic) but also guardian of the fundamental rights it narrows (victim logic) — constrained by the text itself, cannot arbitrage. The foundational liberty narrative (institutional/arbitrage, piton perspective) derives d from beneficiary logic in the broad-speech regime but victim logic in the new regime — arbitrage exit means the rhetoric can persist without binding courts (piton signature: high theater, low functional extraction).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through the reading frame. From the speech_grounds_expansion_reading, the extraction is real: the grounds were chosen to enlarge suppression, and the amendment created winners (prosecution authority, state control of expression) and losers (early acquitted speakers, speech advocates). The coordination function is secondary — it exists but serves the extraction. The constraint is a tangled rope because both functions exist; the mixture is not illusory. The sibling readings (founders_amending_founders_reading, ninth_schedule_immunity_reading) would classify the same amendment differently: as continuous founding process, or as constitutional vault for statutes. No single classification is 'correct' across all readings — the mandatrophy is resolved by acknowledging that the amendment means different things from different structural positions within the constitutional order.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retroactive_narrowing_legitimacy,
    'Is retroactive narrowing of speech grounds by constitutional amendment legitimate exercise of sovereignty or unjust extraction of rights previously enjoyed?',
    'Jurisprudential analysis: does the amendment''s legitimacy derive from process alone (proper amendment procedure = valid), or does it require substantive continuity with prior fundamental rights framework (prior rights constrain future amendments)?',
    'If process-only: narrowing is coordinate action (Rope). If substantive continuity required: narrowing is extraction (Snare/Tangled Rope). This omega determines whether the amendment is experienced as legitimate law-clarification or rights-violation by the constrained speech advocate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retroactive_narrowing_legitimacy, conceptual, 'Legitimacy of retroactive narrowing via constitutional amendment').

omega_variable(
    founders_amending_founders_identity,
    'Does Nehru''s government amending Ambedkar''s text within months represent the founding as an open process (founders amending founders), or a reversion to executive power over original constitutional design?',
    'Historical analysis of the 1951 amendment debates, statements of intent, and constitutional theory. Did framers view the Constitution as revisable, or did they view the First Amendment as correcting a founding mistake?',
    'If open process: the founding was iterative, amendment is normal (ties to founders_amending_founders_reading coexists). If reversion to executive: the amendment is a departure from founding intent (forecloses founders_amending_founders_reading''s premise of continuous founding process). Classification shifts based on this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founders_amending_founders_identity, conceptual, 'Whether the 1951 amendment represents the founding as open or closed process').

omega_variable(
    early_precedent_binding_force,
    'Did the early broad-speech acquittals (pre-1951) establish binding precedent that only the High Court could overturn, or did the constitutional amendment render them immediately non-binding?',
    'Doctrinal analysis of Marbury v. Madison principles in the Indian constitutional context: does a constitutional amendment supersede prior precedent, or do precedents retain binding authority unless explicitly overruled?',
    'If amendment supersedes: speakers had no binding protection to lose (extraction narrative weakens, classification may shift toward Rope). If precedent persists until overruled: speakers lost established rights retroactively (extraction narrative strengthens, Snare classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(early_precedent_binding_force, empirical, 'Whether early acquittals established binding precedent or amendment nullified them immediately').

omega_variable(
    public_order_ground_breadth,
    'How broadly is ''public order'' (Article 19(2)) construed in practice? Is it a genuine safety constraint (narrow: direct incitement to violence, immediate threat) or an elastic prosecutorial tool?',
    'Empirical analysis of prosecutions under Article 19(2) post-1951: distribution of conviction grounds, rate of acquittals, pattern of charge escalation. Compare to pre-1951 acquittals on same conduct.',
    'If narrow/genuine: suppression is moderate and justified (Rope-leaning). If elastic: suppression is high and extractive (Snare-leaning). This determines whether the expanded ground is actually a safety mechanism or a suppression tool.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_order_ground_breadth, empirical, 'Actual breadth of ''public order'' prosecutions under Article 19(2)').

omega_variable(
    reading_vs_sibling_alternatives,
    'This reading (speech grounds expansion) competes with two sibling readings of the 1951 amendment kernel. Are they logically foreclosed by each other, or can they coexist as different political framings?',
    'Structural analysis: founders_amending_founders_reading emphasizes process-continuity; ninth_schedule_immunity_reading emphasizes the constitutional vault for statutes; speech_grounds_expansion_reading emphasizes suppression enlargement. Can all three be true simultaneously within one constitutional framework?',
    'If coexist: all three readings are live (the amendment appears as multiple things to different constituencies). If one forecloses others: the amendment has a single structural logic, and the other readings are ideological cover (ties directly to reading_relations in cs_structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_alternatives, conceptual, 'Logical relation between the three 1951 amendment readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_1951__speech_grounds_expansion_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fa51_sge_theater_t0, first_amendment_1951__speech_grounds_expansion_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fa51_sge_theater_t3, first_amendment_1951__speech_grounds_expansion_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement(fa51_sge_theater_t6, first_amendment_1951__speech_grounds_expansion_reading, theater_ratio, 6, 0.52).

% Extraction over time
narrative_ontology:measurement(fa51_sge_extract_t0, first_amendment_1951__speech_grounds_expansion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fa51_sge_extract_t3, first_amendment_1951__speech_grounds_expansion_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(fa51_sge_extract_t6, first_amendment_1951__speech_grounds_expansion_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fa51_sge_supp_t0, first_amendment_1951__speech_grounds_expansion_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fa51_sge_supp_t3, first_amendment_1951__speech_grounds_expansion_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(fa51_sge_supp_t6, first_amendment_1951__speech_grounds_expansion_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_1951__speech_grounds_expansion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_1951__speech_grounds_expansion_reading, first_amendment_1951__founders_amending_founders_reading).
narrative_ontology:affects_constraint(first_amendment_1951__speech_grounds_expansion_reading, first_amendment_1951__ninth_schedule_immunity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 1951 First Amendment. The sibling readings (founders_amending_founders_reading, ninth_schedule_immunity_reading) are separate constraint stories modeling the same amendment from different structural perspectives. All three are linked via network.affects_constraints. Each story has its own epsilon, beneficiary/victim set, and perspectival classification. The shared event (the amendment) instantiates three structurally distinct constraints because the readings emphasize different mechanisms and serve different analytical purposes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_amendment_1951__speech_grounds_expansion_reading, institutional, 0.28).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
