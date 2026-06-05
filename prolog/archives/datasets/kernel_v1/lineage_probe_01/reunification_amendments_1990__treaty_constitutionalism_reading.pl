% ============================================================================
% CONSTRAINT STORY: reunification_amendments_1990__treaty_constitutionalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reunification_amendments_1990__treaty_constitutionalism_reading, []).

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
 *   constraint_id: reunification_amendments_1990__treaty_constitutionalism_reading
 *   human_readable: Treaty Constitutionalism in German Reunification (1990)
 *   domain: constitutional_law/international_law
 *
 * SUMMARY:
 *   The German reunification of 1990 was accomplished through an
 *   international treaty instrument — the Unification Treaty of August 31,
 *   1990 — rather than through Article 146 of the Basic Law (which
 *   contemplates adoption of a new constitution by the whole people) or
 *   through simple Article 23 amendment to accede the GDR to the Federal
 *   Republic. The treaty ran to hundreds of pages and covered constitutional
 *   essentials: the extension of Basic Law to the eastern territory, the
 *   status of the new Länder, property restitution frameworks, transition
 *   schedules for integrating courts and civil service, federalism
 *   restructuring, and the terms of joining the European Union. The choice to
 *   use treaty form (requiring two-thirds ratification by both legislatures
 *   rather than constitutional amendment) suppressed the visibility of these
 *   constitutional changes into ordinary legislative deliberation. This
 *   constraint story instantiates ONE reading of the contested kernel
 *   'reunification_amendments_1990': the TREATY CONSTITUTIONALISM READING,
 *   which holds that the real constitutional work was done by negotiated
 *   international instrument at the treaty table, not by the visible
 *   constitutional amendment procedure or by a constituent assembly.
 *
 * KEY AGENTS:
 *   - Negotiating state governments (Bonn Federal Government, East German successor state, allied governments): Primary beneficiaries (institutional/arbitrage) — captured treaty terms and transition schedules
 *   - Five new Länder legislatures: Secondary actor (moderate/constrained) — faced negotiated terms as fait accompli; constrained by need for federal integration but also benefited from concrete transition guarantees
 *   - Constituent deliberation commons: Primary victim (powerless/trapped) — ordinary amendment visibility and clause-by-clause deliberation mechanism were suppressed; bear cost of hidden constitutional work
 *   - Popular sovereignty claim under Article 146: Symbolic victim (analytical/analytical) — the path to constitution-by-the-people remained technically open but was bypassed in practice; doctrinally displaced
 *   - International law precedent custodian: Institutional beneficiary (institutional/arbitrage) — treaty form became canonical model for state succession; preserved through precedent maintenance
 *   - Constitutional future-framers: Organized observers (organized/constrained) — Article 146 remains available for future use; constraint has sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reunification_amendments_1990__treaty_constitutionalism_reading, 0.52).
domain_priors:suppression_score(reunification_amendments_1990__treaty_constitutionalism_reading, 0.58).
domain_priors:theater_ratio(reunification_amendments_1990__treaty_constitutionalism_reading, 0.63).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reunification_amendments_1990__treaty_constitutionalism_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reunification_amendments_1990__treaty_constitutionalism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reunification_amendments_1990__treaty_constitutionalism_reading, theater_ratio, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reunification_amendments_1990__treaty_constitutionalism_reading, tangled_rope).
narrative_ontology:human_readable(reunification_amendments_1990__treaty_constitutionalism_reading, "Treaty Constitutionalism in German Reunification (1990)").
narrative_ontology:topic_domain(reunification_amendments_1990__treaty_constitutionalism_reading, "constitutional_law/international_law").

domain_priors:requires_active_enforcement(reunification_amendments_1990__treaty_constitutionalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reunification_amendments_1990__treaty_constitutionalism_reading, '512e3815-8201-409e-8499-7e37d914714b').
narrative_ontology:cs_kernel_codification('512e3815-8201-409e-8499-7e37d914714b', fixed_text).
narrative_ontology:cs_authority_grounding('512e3815-8201-409e-8499-7e37d914714b', extraction).
narrative_ontology:cs_interpretation_layer_present('512e3815-8201-409e-8499-7e37d914714b').
narrative_ontology:cs_reading_relation('512e3815-8201-409e-8499-7e37d914714b', reunification_amendments_1990__accession_not_merger_reading, coexists_with).
narrative_ontology:cs_reading_relation('512e3815-8201-409e-8499-7e37d914714b', reunification_amendments_1990__article_146_question_reading, coexists_with).
narrative_ontology:cs_axiom('512e3815-8201-409e-8499-7e37d914714b', foundational, treaty_constitutionalism_core).
narrative_ontology:cs_axiom_status(treaty_constitutionalism_core, holdable).
narrative_ontology:cs_axiom_grounding('512e3815-8201-409e-8499-7e37d914714b', treaty_constitutionalism_core, conventional).
narrative_ontology:cs_axiom('512e3815-8201-409e-8499-7e37d914714b', secondary, visibility_suppression_acceptable).
narrative_ontology:cs_axiom_status(visibility_suppression_acceptable, holdable).
narrative_ontology:cs_axiom_grounding('512e3815-8201-409e-8499-7e37d914714b', visibility_suppression_acceptable, instrumental).
narrative_ontology:cs_reference_frame('512e3815-8201-409e-8499-7e37d914714b', treaty_based_constitutional_amendment_as_valid_form).
narrative_ontology:cs_drift_state('512e3815-8201-409e-8499-7e37d914714b', contemporary_post_treaty_analysis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('512e3815-8201-409e-8499-7e37d914714b', '').
narrative_ontology:cs_kernel_id(reunification_amendments_1990__treaty_constitutionalism_reading, reunification_amendments_1990).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__treaty_constitutionalism_reading, negotiating_state_governments).
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__treaty_constitutionalism_reading, treaty_drafters).
narrative_ontology:constraint_beneficiary(reunification_amendments_1990__treaty_constitutionalism_reading, international_law_precedent).
narrative_ontology:constraint_victim(reunification_amendments_1990__treaty_constitutionalism_reading, constituent_power_deliberation).
narrative_ontology:constraint_victim(reunification_amendments_1990__treaty_constitutionalism_reading, popular_sovereignty_claim).
narrative_ontology:constraint_victim(reunification_amendments_1990__treaty_constitutionalism_reading, ordinary_amendment_procedure_visibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSTITUENT DELIBERATION COMMONS (SNARE) — The ordinary amendment procedure and clause-by-clause legislative deliberation are bypassed entirely. Trapped within the treaty-packaged substitution, with no visibility into or control over the constitutional essentials hidden in bilateral negotiation. Maximum extraction — the deliberative mechanism that claims legitimacy in democratic constitutionalism is suppressed.
constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FIVE NEW LÄNDER LEGISLATURES (TANGLED_ROPE) — Constrained by the treaty terms already negotiated by federal governments and East German successor representatives before Länder accession. Face high costs of exit (exclusion from federal system; loss of negotiated transition guarantees). But also benefit from the treaty's concrete transition schedules, property restitution frameworks, and federal revenue-sharing formulas. Mixed: coordination function (stabilizing transition) plus extraction (terms set without their full participation).
constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NEGOTIATING STATE GOVERNMENTS (ROPE) — Primary beneficiaries (Bonn, East Berlin successor representatives, allied governments) experience the constraint as coordination: the treaty stabilizes transition terms, prevents alternative scenarios (piecemeal accession, confederation, Article 146 constituent assembly), and locks in the international framework that enabled reunification. Arbitrage is available (could have pursued alternative constitutional pathways) but was rejected. Net beneficiary — negotiated detail and scheduling capture value.
constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONSTITUTIONAL FUTURE-FRAMERS (SCAFFOLD) — Article 146 remains: the Basic Law contemplates its own replacement by a constitution the people adopt. The treaty-based amendment is temporary expedient (sunset logic: low effective extraction because the door stays open to full constituent deliberation later). The constraint has an exit path: if future generations exercise Article 146, the treaty structure is superseded. This perspective sees the constraint as coordination with embedded sunset, not permanent extraction.
constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL LAW PRECEDENT CUSTODIAN (PITON) — The treaty form is preserved and cited for its precedential value: reunification via international instrument ratified at two-thirds (not constitutional amendment) becomes the doctrinal template. But the mechanism is largely performed through formal ratification theater; the real constitutional work was bilateral negotiation between states, not the treaty text's public justification. The precedent persists through institutional inertia, maintaining the treaty form as the canonical model for state succession, despite the recognition that it bypasses ordinary constitutional processes.
constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, state succession is inherently an international law phenomenon, not a constitutional one: when two states merge, the legitimacy framework must rest on international instruments because no single constitutional order governs the negotiation. This perspective sees the treaty form as immutable consequence of state sovereignty. However, the structural data contradicts the mountain classification — the choice to use treaty amendment rather than Article 146 constituent process is contingent, not necessary. The false summit reveals that 'state succession requires treaty form' naturalizes what was actually a negotiated choice among available constitutional pathways.
constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reunification_amendments_1990__treaty_constitutionalism_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reunification_amendments_1990__treaty_constitutionalism_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reunification_amendments_1990__treaty_constitutionalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reunification_amendments_1990__treaty_constitutionalism_reading, TR),
    TR >= 0.70.

:- end_tests(reunification_amendments_1990__treaty_constitutionalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The negotiating governments captured significant value by setting constitutional terms at the treaty table without public amendment visibility. The treaty form enabled asymmetric negotiation (bilateral state-to-state rather than all-party legislative deliberation). However, the extraction is not maximal because: (a) outcomes were constrained by Basic Law limits (Änderungsunfähigkeit protects certain core principles), (b) eastern German transition movements had real agency in shaping some terms, and (c) the treaty's substantive terms (fiscal transfers, property frameworks, federalism structure) produced coordination benefits alongside extraction. Suppression (0.58): Moderate-high. The treaty form suppresses the visibility of constitutional essentials into ordinary amendment deliberation, replacing it with bilateral executive negotiation. However, suppression has eroded over time (measurement shows 0.62→0.53 over 24 months) as scholars and politicians increasingly recognize and articulate the constitutional character of what the treaty accomplished. Theater ratio (0.63): Moderate. The two-thirds ratification process produced formal theater of legislative approval, but the substance was negotiated before ratification. Over time (0.45→0.72), the recognition that the ratification was theater has increased, but the treaty form itself persists as the established precedent.
 *
 * PERSPECTIVAL GAP:
 *   The negotiating state governments perceive Rope (coordination benefit; stability for transition). The new Länder perceive Tangled Rope (mixed coordination and constraint). The constituent deliberation commons perceives Snare (complete suppression of their mechanism). The international law custodian perceives Piton (the treaty form is preserved through institutional inertia, no longer defended on merits). The scaffold perspective sees Scaffold (Article 146 remains available for future use). The analytical observer risks seeing Mountain (state succession inherently requires treaty form) but structural data reveals false summit: the treaty form was a chosen path among available alternatives, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary (negotiating governments, international law precedent) has arbitrage options (could have pursued Article 146 or piecemeal accession) but chose the treaty form because it maximized their control over constitutional outcomes. The treaty form gives them d ≈ 0.15 (beneficiary + arbitrage → low d → negative f(d) → negative experienced extraction). The victim (constituent deliberation commons) has trapped exit (no alternative path exists once treaty is signed; no exit until constituent power reasserts via Article 146). This gives victim d ≈ 0.95 (trapped + victim → high d → high f(d) → high experienced extraction). The moderate agent (new Länder) has constrained exit (could resist treaty, but at cost of exclusion; could seek renegotiation, but within narrow window). This gives them d ≈ 0.60 (constrained + mixed victim/beneficiary → moderate d → moderate f(d) → moderate experienced extraction). The analytical observer (d ≈ 0.73, canonical analytical) is at risk of naturalizing the treaty form as inevitable, missing the contingent choice.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates the mandatrophy between multiple readings of the same historical event. The 'true' classification depends on which reading one adopts: is reunification character (merger/accession/replacement) or form (treaty/amendment/constituent assembly)? The treaty reading answers the form question but leaves character open. If the accession reading is correct (GDR simply joined West Germany), then the treaty is merely documenting and implementing accession — classification shifts toward Rope (coordination for implementing a settled decision). If the Article 146 reading is correct (constituent power remains available), then the treaty is temporary Scaffold (sunset logic: the constraint persists only until Article 146 is invoked). The mandatrophy is resolved not by choosing one reading, but by recognizing that each reading asks a different question about the same historical facts. The engine's false summit detector will flag the analytical perspective's mountain classification and trigger domain expert review of whether state succession truly requires treaty form or whether it was a contingent choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_146_availability,
    'Did Article 146 (constitution by the whole people) remain available as a genuine alternative to treaty-based amendment in 1990, or was it foreclosed by geopolitical and temporal constraints?',
    'Counterfactual analysis of constitutional scenarios post-1989; examination of whether Article 146 constituent assembly was ever seriously proposed by significant political forces; assessment of whether allied veto or transition timeline made it infeasible vs. merely undesired by negotiating governments',
    'If genuinely available: treaty form was a chosen path with extractive features; false summit confirmed. If foreclosed: treaty form was necessity; mountain classification more defensible; scaffold perspective weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_146_availability, empirical, 'Whether Article 146 constituent pathway was genuinely available').

omega_variable(
    treaty_versus_amendment_deliberation_visibility,
    'What constitutional essentials were resolved in treaty negotiation rather than public legislative deliberation, and could they have been resolved through Article 23 ordinary amendment with equivalent outcome?',
    'Clause-by-clause mapping of Unification Treaty content to potential Article 23 amendments; identification of ''hidden constitutional work'' in treaty articles; comparison with scenarios where same outcomes were achieved via public legislative amendment in other federations facing integration (e.g., EU enlargement constitutional amendments)',
    'If significant constitutional essentials were hidden in treaty form: suppression is real and extraction is high. If treaty merely expedited procedurally equivalent amendments: constraint is coordination with theater, not genuine extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_versus_amendment_deliberation_visibility, empirical, 'Visibility of constitutional deliberation: treaty-packaged vs. amendment-proceeded').

omega_variable(
    two_thirds_ratification_versus_constitutional_majority,
    'Is two-thirds ratification of an international treaty a lower or higher threshold than the constitutional amendment bar (Article 79, Clause 2: two-thirds of Bundestag and Bundesrat, but with Änderungsunfähigkeit clause protecting core principles)?',
    'Formal analysis of threshold mechanics: two-thirds of which bodies? Substantive review of whether Änderungsunfähigkeit-protected principles were effectively protected or bypassed in the treaty form',
    'If two-thirds treaty < two-thirds amendment + Änderungsunfähigkeit: suppression is genuine (circumventing substantive protection). If equivalent: treaty is formal bypass but not substantive circumvention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(two_thirds_ratification_versus_constitutional_majority, empirical, 'Comparative threshold: treaty ratification vs. constitutional amendment with unamendability protection').

omega_variable(
    negotiation_asymmetry_in_treaty_form,
    'Did the treaty form enable more extractive negotiation outcomes (e.g., property restitution terms, industrial transition schedules, fiscal transfers) than would have resulted from public Article 23 amendment debate?',
    'Comparison of treaty-negotiated terms with documented positions of Länder legislatures, civil society, and democratic movements during the transition period; identification of where treaty terms diverged from expressed preferences in public deliberation',
    'If treaty enabled more extraction: beneficiary claim is high, victim claim is high, extractiveness justified above 0.50. If no material divergence: extraction is performative theater, extractiveness should be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_asymmetry_in_treaty_form, empirical, 'Negotiation asymmetry: did treaty form enable extractive outcomes unavailable via public amendment').

omega_variable(
    foreclosure_versus_coexistence_with_article_146,
    'Does the treaty reading''s core claim — that reunification was constitutionally accomplished via international instrument — logically foreclose or merely defer the Article 146 reading''s core claim that a constituent assembly could still produce a new constitution?',
    'Doctrinal analysis: can both readings coexist in a single constitutional framework? Is Article 146 still available to future generations? Or does treaty-based constitutional amendment establish precedent that foreclosed the constituent route?',
    'If foreclosed: reading_relation to article_146_question_reading is ''forecloses''. If deferred/coexists: reading_relation is ''coexists_with''. Determines whether scaffold perspective''s sunset logic is real or merely aspirational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_versus_coexistence_with_article_146, conceptual, 'Logical foreclosure: does treaty reading preclude Article 146 constitution-by-the-people').

omega_variable(
    committer_frame_kernel_identity,
    'Is the contested kernel the constitutional CHARACTER of reunification (merger vs. accession vs. new constitution), or the FORM through which it was accomplished (treaty vs. constitutional amendment vs. constitutional replacement)?',
    'Examine whether the three sibling readings address the same structural question or different ones. If the accession_not_merger_reading answers ''was reunification a merger?'' while treaty_constitutionalism_reading answers ''was reunification accomplished by treaty or amendment?'', the readings may not be siblings in a single kernel but rather independent axes',
    'If different axes: decomposition required; separate kernel identities for character and form. If same kernel: confirm sibling structure and ensure reading_relations are logically sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_identity, conceptual, 'Kernel identity: character vs. form of reunification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reunification_amendments_1990__treaty_constitutionalism_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reun_treaty_theater_t0, reunification_amendments_1990__treaty_constitutionalism_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement(reun_treaty_theater_t12, reunification_amendments_1990__treaty_constitutionalism_reading, theater_ratio, 12, 0.63).
narrative_ontology:measurement(reun_treaty_theater_t24, reunification_amendments_1990__treaty_constitutionalism_reading, theater_ratio, 24, 0.72).

% Extraction over time
narrative_ontology:measurement(reun_treaty_extract_t0, reunification_amendments_1990__treaty_constitutionalism_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(reun_treaty_extract_t12, reunification_amendments_1990__treaty_constitutionalism_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(reun_treaty_extract_t24, reunification_amendments_1990__treaty_constitutionalism_reading, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(reun_treaty_suppress_t0, reunification_amendments_1990__treaty_constitutionalism_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(reun_treaty_suppress_t12, reunification_amendments_1990__treaty_constitutionalism_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(reun_treaty_suppress_t24, reunification_amendments_1990__treaty_constitutionalism_reading, suppression_requirement, 24, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reunification_amendments_1990__treaty_constitutionalism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(reunification_amendments_1990__treaty_constitutionalism_reading, reunification_amendments_1990__accession_not_merger_reading).
narrative_ontology:affects_constraint(reunification_amendments_1990__treaty_constitutionalism_reading, reunification_amendments_1990__article_146_question_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the contested kernel reunification_amendments_1990. All three stories address the same historical event (German reunification 1990) but from different doctrinal frames: treaty constitutionalism (this file) vs. accession without merger vs. Article 146 constituent pathway still available. Each reading has its own constraint_id, its own ε value, and its own beneficiary/victim structure. They are linked via network.affects_constraints to enable joint analysis of how the same institutional facts receive different doctrinal classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
