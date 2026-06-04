% ============================================================================
% CONSTRAINT STORY: party_state_duality__article_126_keyhole_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_party_state_duality__article_126_keyhole_reading, []).

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
 *   constraint_id: party_state_duality__article_126_keyhole_reading
 *   human_readable: Article 126 Keyhole Reading: Party Sovereignty Hidden in Nine Words
 *   domain: legal/constitutional/political_theory
 *
 * SUMMARY:
 *   Article 126 of the 1936 Soviet Constitution contains exactly nine words
 *   establishing the Communist Party as 'the leading core of all
 *   organizations of the working people.' This reading instantiates a
 *   specific structural interpretation: Article 126 functions as a keyhole
 *   through which the actual state becomes visible — not through what it
 *   says, but through the systematic suppression of what formal
 *   constitutionalism should say. The constraint operates through
 *   near-omission. A true constitutional clause would enumerate the party's
 *   powers, limits, and relationship to state institutions. Instead, Article
 *   126 provides only a naming — the party is identified, not defined or
 *   constrained. This textual minimization enables the party core to exercise
 *   comprehensive authority while maintaining the fiction of a separate,
 *   sovereign state apparatus described in subsequent articles. The formal
 *   state institutions (Supreme Soviet, Council of Ministers, courts) are
 *   described in detail with apparent jurisdictions, but Article 126's
 *   keyhole reveals that actual power flows from the party's implicit
 *   authority rather than the state's textual grant. Constitutional readers
 *   seeking to understand the operative state must read Article 126 not as a
 *   subordinate clause but as the hidden master text — nine words that carry
 *   the entire actual constitution while the 146 articles of formal machinery
 *   provide performative cover.
 *
 * KEY AGENTS:
 *   - Constitutional readers (powerless/trapped): Seek to reconstruct sovereignty from the text; trapped in the constraint because alternative readings do not become available from the text alone. Bear full cost of the suppression — cannot determine actual power distribution from the formal document.
 *   - Formal state institutions (moderate/constrained): Supreme Soviet, Council of Ministers, courts. Constrained by their formal jurisdiction and the requirement to maintain the fiction of constitutional authority. Benefits from deniability (they are not responsible for power concentration) but pays cost of performative ratification without real decision authority.
 *   - Party core (institutional/arbitrage): Centralized party leadership structure (Politburo, Central Committee). Benefits from Article 126's naming and the comprehensive authority it signals without the need for enumerated powers. Arbitrage through textual minimization — gains power through what the text does not say.
 *   - Theoretical juridical order (powerless/trapped): The abstract commitment to constitutional rule of law. Suppressed by the constraint — victims in the sense that the constraint undermines the principle that power should be enumerated and limited by written law.
 *   - Post-Soviet constitutional theorists (analytical/analytical): Analytical observers attempting to reconstruct what Article 126 actually meant and how it functioned. Can see the constraint structure retrospectively but lack access to contemporaneous drafting intentions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(party_state_duality__article_126_keyhole_reading, 0.68).
domain_priors:suppression_score(party_state_duality__article_126_keyhole_reading, 0.72).
domain_priors:theater_ratio(party_state_duality__article_126_keyhole_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(party_state_duality__article_126_keyhole_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(party_state_duality__article_126_keyhole_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(party_state_duality__article_126_keyhole_reading, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(party_state_duality__article_126_keyhole_reading, snare).
narrative_ontology:human_readable(party_state_duality__article_126_keyhole_reading, "Article 126 Keyhole Reading: Party Sovereignty Hidden in Nine Words").
narrative_ontology:topic_domain(party_state_duality__article_126_keyhole_reading, "legal/constitutional/political_theory").

domain_priors:requires_active_enforcement(party_state_duality__article_126_keyhole_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(party_state_duality__article_126_keyhole_reading, 'e4f229b5-b838-4f65-8dff-60b85f6a0c50').
narrative_ontology:cs_kernel_codification('e4f229b5-b838-4f65-8dff-60b85f6a0c50', formalized).
narrative_ontology:cs_authority_grounding('e4f229b5-b838-4f65-8dff-60b85f6a0c50', extraction).
narrative_ontology:cs_interpretation_layer_present('e4f229b5-b838-4f65-8dff-60b85f6a0c50').
narrative_ontology:cs_reading_relation('e4f229b5-b838-4f65-8dff-60b85f6a0c50', party_state_duality__description_not_constraint_reading, coexists_with).
narrative_ontology:cs_reading_relation('e4f229b5-b838-4f65-8dff-60b85f6a0c50', party_state_duality__dual_hierarchy_mechanics_reading, coexists_with).
narrative_ontology:cs_axiom('e4f229b5-b838-4f65-8dff-60b85f6a0c50', foundational, suppression_by_textual_minimization).
narrative_ontology:cs_axiom_status(suppression_by_textual_minimization, holdable).
narrative_ontology:cs_axiom_grounding('e4f229b5-b838-4f65-8dff-60b85f6a0c50', suppression_by_textual_minimization, empirically_contingent).
narrative_ontology:cs_axiom('e4f229b5-b838-4f65-8dff-60b85f6a0c50', foundational, named_authority_requires_no_enumeration).
narrative_ontology:cs_axiom_status(named_authority_requires_no_enumeration, holdable).
narrative_ontology:cs_axiom_grounding('e4f229b5-b838-4f65-8dff-60b85f6a0c50', named_authority_requires_no_enumeration, instrumental).
narrative_ontology:cs_reference_frame('e4f229b5-b838-4f65-8dff-60b85f6a0c50', constitutional_rule_of_law_frame).
narrative_ontology:cs_drift_state('e4f229b5-b838-4f65-8dff-60b85f6a0c50', soviet_collapse_1991, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e4f229b5-b838-4f65-8dff-60b85f6a0c50', '').
narrative_ontology:cs_kernel_id(party_state_duality__article_126_keyhole_reading, party_state_duality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(party_state_duality__article_126_keyhole_reading, party_core).
narrative_ontology:constraint_beneficiary(party_state_duality__article_126_keyhole_reading, textual_deniability).
narrative_ontology:constraint_victim(party_state_duality__article_126_keyhole_reading, constitutional_readers).
narrative_ontology:constraint_victim(party_state_duality__article_126_keyhole_reading, formal_state_institutions).
narrative_ontology:constraint_victim(party_state_duality__article_126_keyhole_reading, theoretical_juridical_order).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUTIONAL READER (SNARE) — The reader searching Article 126 for enumerated state sovereignty finds only nine words: 'The Communist Party of the Soviet Union is the leading core of all organizations.' No exit from this trap: the actual locus of power is textually invisible. The constraint extracts authority from the formal juridical apparatus and deposits it in the named party through near-omission. Maximum suppression — no alternative reading yields the sovereignty distribution.
constraint_indexing:constraint_classification(party_state_duality__article_126_keyhole_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FORMAL STATE INSTITUTION (SNARE) — The constitutional apparatus (Supreme Soviet, Council of Ministers) is constrained to performative ratification of decisions taken elsewhere. High costs to breaking the ritual (loss of legitimacy, institutional dissolution) but structurally no real decision authority. The constraint suppresses alternatives (formal juridical models based on institutional separation of powers) and extracts legitimacy from these organs into the party's actual command structure.
constraint_indexing:constraint_classification(party_state_duality__article_126_keyhole_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARTY CORE (ROPE) — The constraint provides coordination infrastructure. Article 126 names the party as the 'leading core,' establishing a single locus for policy coordination across all state and social organizations. From this perspective, the minimal text is elegant — nine words suffice to establish the coordination mechanism. The party experiences the constraint as coordination, not extraction: the clause solves a collective action problem (which institution holds binding authority?) with high clarity.
constraint_indexing:constraint_classification(party_state_duality__article_126_keyhole_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POST-SOVIET ANALYST (PITON) — Looking backward, Article 126 appears as a vestigial clause — the constitutional facade persists long after the party's actual control mechanisms (nomenklatura, party groups in every soviet, parallel decision structures) have been exposed or dismantled. The theater of reading Article 126 as constitutionalism continues in post-Soviet jurisprudence as an attempt to reconstruct legitimacy from the text, despite knowing the actual power mechanics lay elsewhere. High theater ratio because the clause's function (textual deniability) has atrophied; it persists through doctrinal inertia.
constraint_indexing:constraint_classification(party_state_duality__article_126_keyhole_reading, piton,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the civilizational perspective, some degree of textual concealment is inherent to power: law always describes less than it governs; the gap between written and operative constitutions is a structural feature of politics itself. This reading risks naturalizing what the article_126_keyhole_reading reveals as deliberate suppression through textual minimization. The engine's false summit detector will flag this as precisely the kind of naturalization the constraint engineering.
constraint_indexing:constraint_classification(party_state_duality__article_126_keyhole_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(party_state_duality__article_126_keyhole_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(party_state_duality__article_126_keyhole_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(party_state_duality__article_126_keyhole_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(party_state_duality__article_126_keyhole_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(party_state_duality__article_126_keyhole_reading, TR),
    TR >= 0.70.

:- end_tests(party_state_duality__article_126_keyhole_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The party core extracts comprehensive authority from formal state institutions through Article 126's textual minimization. The extraction flows through a specific mechanism: the article names the party once, which establishes it as the binding reference point for all organizations, but provides no enumeration of powers, limits, or relationship to state authority. This creates an asymmetry between what is named (the party) and what is defined (the state apparatus in subsequent articles). Constitutional readers extract nothing from Article 126 — they must infer the actual state from its silence. Formal state institutions experience high extraction because they are required to perform sovereignty while actual decision authority resides in the party structure. Suppression (0.72): High. The constraint suppresses alternative readings of the formal text. A constitutionalist reader might expect Article 126 to read: 'The party serves as an advisor to state institutions' (subordinate) or 'The state incorporates party representation through formal structures' (integrated) or 'Party and state maintain separate hierarchies with defined coordination mechanisms' (dual). Instead, Article 126 reads: 'The Communist Party of the Soviet Union is the leading core of all organizations.' The word 'leading' establishes priority but contains no mechanism. 'Core' suggests centrality without definition. 'All organizations' includes but does not differentiate state and non-state. This textual ambiguity suppresses the ability to read the actual constitution from the formal document. Theater ratio (0.85): Very high, and rising over time. At 1936 adoption, Article 126 was part of a new formal constitutional framework that many readers took at face value — the theater was lower because the formality was fresh. By 1956 (post-Twentieth Congress, when party authority was questioned), the theater of reading Article 126 as the source of constitutional truth increased — readers had to work harder to sustain the reading despite emerging evidence of party shadow structures. By 1977 (Brezhnev Constitution, new adoption), the theater had risen to maximum — the ritual of constitutional reading had become entirely performative; everyone understood that Article 126 was a fragment that concealed rather than revealed, but the ritual continued. By 1991 (Soviet collapse), the theater completely dissolved — the constraint disappeared because its entire function was maintaining a fiction that had become untenable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a profound perspectival gap between the beneficiary (party core) and the victims (constitutional readers, formal state institutions). The party core experiences Article 126 as elegant coordination infrastructure — one clause that establishes hierarchical authority across all organizations with minimal text, no enumerated powers that could be exceeded, and no formal limits that could constrain party leadership. The constraint enables the party to coordinate all state and social organizations without the burden of constitutional definition. For constitutional readers and formal state institutions, the constraint appears as suppression and extraction. Readers cannot determine actual state structure from the formal document. State institutions must perform sovereignty while their actual authority is captured by the party structure that Article 126 merely names without defining. The piton perspective (post-Soviet) reveals that the entire theater of constitutional reading has atrophied — the ritual persists in post-Soviet jurisprudence but the constraint's actual suppressive function has dissolved. The mountain perspective risks naturalizing this as inherent to all law (written rules always contain gaps, operant power always exceeds formal authority), which would convert what the keyhole reading reveals as suppression into inevitable structural feature.
 *
 * DIRECTIONALITY LOGIC:
 *   The party core's directionality (d) is low despite institutional power — they are the beneficiary with arbitrage options, meaning they can credibly claim the constraint benefits them (coordination infrastructure, textual deniability) or ignore it (their actual authority derives from party structure, not constitutional text). The engine derives d toward 0.15-0.20 range from institutional power + arbitrage exit + beneficiary status. Constitutional readers have d near 1.0 (trapped + powerless + victim status). Formal state institutions have d around 0.65-0.75 (moderate power + constrained exit + victim status, but with some institutional capacity). The perspectival gap between d ≈ 0.15 (party) and d ≈ 0.75 (state institutions) is the mechanism through which Article 126 extracts — the same nine words are experienced as coordination by one institutional actor and as suppression by another. This structural differentiation is the constraint's core function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_minimization_intentionality,
    'Is Article 126''s brevity and obliqueness a deliberate suppression mechanism, or does it reflect genuine doctrinal ambiguity about the party-state relationship?',
    'Historical analysis of constitutional drafting deliberations; comparison with earlier drafts and contemporaneous party theory; interviews with redactors (where available). If deliberate minimization was chosen over clearer formulations, intentionality is established.',
    'If deliberate: confirms snare classification — suppression is engineered. If ambiguous: constraint may be tangled_rope (coordination with embedded asymmetry) rather than snare (pure extraction). If theoretical gap: constraint becomes mountain-adjacent (inherent to translating power into law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_minimization_intentionality, empirical, 'Whether Article 126''s textual minimization was deliberate or doctrinal').

omega_variable(
    nine_word_sufficiency_paradox,
    'How does a single clause of nine words establish and maintain hierarchical control over all state and social organizations without explicit operational mechanisms?',
    'Analysis of nomenklatura appointment records, party group meeting minutes, and decision-making sequences in paired party/state organs. Demonstration of how Article 126 functions as a coordination anchor rather than a constraint mechanism.',
    'If nine words genuinely suffice (network effect of shared reference point): constraint is rope with high coordination efficiency. If mechanisms require supplementary party documents and practices: constraint is snare — Article 126 provides textual deniability while actual control flows through suppressed parallel structures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nine_word_sufficiency_paradox, empirical, 'How nine words achieve comprehensive hierarchical coordination').

omega_variable(
    formal_state_sovereignty_fiction,
    'Does the formal state apparatus (Supreme Soviet, Council of Ministers) actually believe it holds constitutional sovereignty, or is the fiction collectively maintained at all institutional levels?',
    'Content analysis of internal state documents, correspondence between state and party organs, and testimony from state officials. Detection of instances where state institutions assert independent authority or where the fiction breaks.',
    'If genuine belief at lower levels: suppression operates through obscured structure (snare). If collective fiction: suppression operates through shared epistemic closure (tangled_rope with identity-locked agents). If exposed fiction (post-Soviet): constraint degrades to piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_state_sovereignty_fiction, empirical, 'Whether formal state sovereignty is believed or collectively maintained fiction').

omega_variable(
    reading_contest_underdetermination,
    'How should Article 126 be read relative to the sibling readings: as a keyhole revealing hidden state (this reading), as a true description-without-constraint (description_not_constraint reading), or as a textual anchor for mechanical dual hierarchy (dual_hierarchy_mechanics reading)?',
    'Structural comparison of what each reading predicts about institutional behavior, decision sequences, and the role of textual ambiguity. Historical case analysis of moments where the reading becomes contested or breaks down.',
    'Keyhole reading emphasizes suppression and minimization; validates snare classification. Description reading emphasizes legal positivism (law describes operative power structure); would suggest rope or piton classification. Mechanics reading emphasizes parallel structures; would suggest tangled_rope. The contest is not empirically resolvable — it is a difference in what aspects of the constraint structure each reading privileges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_underdetermination, conceptual, 'Which sibling reading best captures the kernel''s structural mechanics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(party_state_duality__article_126_keyhole_reading, 1936, 1991).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1936_adoption, party_state_duality__article_126_keyhole_reading, theater_ratio, 1936, 0.6).
narrative_ontology:measurement(theater_1956_post_twentieth_congress, party_state_duality__article_126_keyhole_reading, theater_ratio, 1956, 0.75).
narrative_ontology:measurement(theater_1977_new_constitution, party_state_duality__article_126_keyhole_reading, theater_ratio, 1977, 0.88).

% Extraction over time
narrative_ontology:measurement(extractiveness_1936_adoption, party_state_duality__article_126_keyhole_reading, base_extractiveness, 1936, 0.55).
narrative_ontology:measurement(extractiveness_1956_post_twentieth_congress, party_state_duality__article_126_keyhole_reading, base_extractiveness, 1956, 0.65).
narrative_ontology:measurement(extractiveness_1977_new_constitution, party_state_duality__article_126_keyhole_reading, base_extractiveness, 1977, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1936_adoption, party_state_duality__article_126_keyhole_reading, suppression_requirement, 1936, 0.5).
narrative_ontology:measurement(suppression_1956_post_twentieth_congress, party_state_duality__article_126_keyhole_reading, suppression_requirement, 1956, 0.7).
narrative_ontology:measurement(suppression_1977_new_constitution, party_state_duality__article_126_keyhole_reading, suppression_requirement, 1977, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(party_state_duality__article_126_keyhole_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(party_state_duality__article_126_keyhole_reading, party_state_duality__description_not_constraint_reading).
narrative_ontology:affects_constraint(party_state_duality__article_126_keyhole_reading, party_state_duality__dual_hierarchy_mechanics_reading).
narrative_ontology:affects_constraint(party_state_duality__article_126_keyhole_reading, nomenklatura_appointment_system).
narrative_ontology:affects_constraint(party_state_duality__article_126_keyhole_reading, parallel_party_soviet_decision_structures).

% DUAL FORMULATION NOTE:
% Article 126 keyhole reading decomposes from the contested kernel 'party_state_duality' alongside two sibling readings that privilege different structural aspects. The keyhole reading emphasizes textual suppression (extractiveness routed through nine words); description reading emphasizes legal-positivist inversion; mechanics reading emphasizes operational parallel structures. Each has distinct epsilon but all three share the same kernel (Article 126 of the 1936 Constitution) and the same constraint field (party-state relationship). Network edges link all three to operational constraint stories (nomenklatura, party group structures) that implement the suppression the keyhole reading reveals.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(party_state_duality__article_126_keyhole_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
