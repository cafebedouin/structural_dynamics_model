% ============================================================================
% CONSTRAINT STORY: kesavananda_basic_structure__implied_limits_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kesavananda_basic_structure__implied_limits_reading, []).

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
 *   constraint_id: kesavananda_basic_structure__implied_limits_reading
 *   human_readable: Kesavananda Basic Structure Doctrine (Implied Limits Reading)
 *   domain: constitutional_law/interpretive_doctrine
 *
 * SUMMARY:
 *   The Kesavananda Bharati v. State of Kerala judgment (1973) articulated
 *   the 'basic structure' doctrine: the Indian Constitution can be amended,
 *   but not in ways that destroy its essential identity. The implied limits
 *   reading locates this constraint in the word 'amend' itself — a power to
 *   'amend' logically cannot include the power to 'replace.' This is one
 *   reading of the kernel among three: the democratic safeguard reading
 *   emphasizes the doctrine's historical role in blocking the Emergency
 *   (1975–1977), while the judicial usurpation critique argues the doctrine
 *   illegitimately concentrates power in an unelected judiciary. The implied
 *   limits reading claims the constraint is textually derived rather than
 *   politically motivated. The structural analysis reveals this reading as a
 *   tangled rope: it coordinates interpretation around a stable
 *   constitutional text (rope function) while simultaneously concentrating
 *   interpretive authority in the judiciary (extraction function). The
 *   constraint's extractiveness has risen over 25 years (from 0.15 to 0.35)
 *   as courts have repeatedly invoked the doctrine to block amendment
 *   proposals, accumulating veto power. Theater ratio has also risen (0.38 to
 *   0.52), suggesting the doctrine's enforcement has become increasingly
 *   ritualistic — courts perform textual analysis of word 'amend' to reach
 *   predetermined judgments.
 *
 * KEY AGENTS:
 *   - Textual-Fidelity Adjudication: Primary beneficiary (institutional/arbitrage) — the doctrine preserves judicial authority to enforce constitutional text's integrity; gains interpretive privilege
 *   - Constitutional Identity Preservation: Beneficiary (institutional) — the doctrine protects the document's essential character from unlimited revision; serves coordination function
 *   - Unlimited Amendment Power Advocates: Primary victim (organized/constrained) — political movements seeking to amend or replace Constitution face judicial veto on grounds of 'basic structure' violation; suppressed by doctrine
 *   - Parliamentary Supremacy Doctrine: Secondary victim (institutional/constrained) — traditional understanding that supermajority Parliament can amend or replace Constitution is subordinated to judicial review
 *   - The Judiciary: Institutional enforcer (institutional/constrained) — benefits from veto authority (extraction) while performing coordination function (stable adjudication); requires continuous enforcement
 *   - Analytical Observer: Civilizational view (analytical/analytical) — questions whether the constraint is textually necessary or judicially constructed; risks naturalizing institutional choice as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kesavananda_basic_structure__implied_limits_reading, 0.35).
domain_priors:suppression_score(kesavananda_basic_structure__implied_limits_reading, 0.48).
domain_priors:theater_ratio(kesavananda_basic_structure__implied_limits_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kesavananda_basic_structure__implied_limits_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(kesavananda_basic_structure__implied_limits_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(kesavananda_basic_structure__implied_limits_reading, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kesavananda_basic_structure__implied_limits_reading, tangled_rope).
narrative_ontology:human_readable(kesavananda_basic_structure__implied_limits_reading, "Kesavananda Basic Structure Doctrine (Implied Limits Reading)").
narrative_ontology:topic_domain(kesavananda_basic_structure__implied_limits_reading, "constitutional_law/interpretive_doctrine").

domain_priors:requires_active_enforcement(kesavananda_basic_structure__implied_limits_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kesavananda_basic_structure__implied_limits_reading, '9989c40b-7874-45a7-a724-14da854c6a64').
narrative_ontology:cs_kernel_codification('9989c40b-7874-45a7-a724-14da854c6a64', formalized).
narrative_ontology:cs_authority_grounding('9989c40b-7874-45a7-a724-14da854c6a64', lineage).
narrative_ontology:cs_interpretation_layer_present('9989c40b-7874-45a7-a724-14da854c6a64').
narrative_ontology:cs_reading_relation('9989c40b-7874-45a7-a724-14da854c6a64', kesavananda_basic_structure__democratic_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('9989c40b-7874-45a7-a724-14da854c6a64', kesavananda_basic_structure__judicial_usurpation_critique, coexists_with).
narrative_ontology:cs_axiom('9989c40b-7874-45a7-a724-14da854c6a64', foundational, amendment_power_bounded_by_word_semantics).
narrative_ontology:cs_axiom_status(amendment_power_bounded_by_word_semantics, holdable).
narrative_ontology:cs_axiom_grounding('9989c40b-7874-45a7-a724-14da854c6a64', amendment_power_bounded_by_word_semantics, deontological).
narrative_ontology:cs_axiom('9989c40b-7874-45a7-a724-14da854c6a64', secondary, constitutional_identity_as_amendable_essence).
narrative_ontology:cs_axiom_status(constitutional_identity_as_amendable_essence, holdable).
narrative_ontology:cs_axiom_grounding('9989c40b-7874-45a7-a724-14da854c6a64', constitutional_identity_as_amendable_essence, deontological).
narrative_ontology:cs_reference_frame('9989c40b-7874-45a7-a724-14da854c6a64', amendment_as_modification_not_replacement).
narrative_ontology:cs_drift_state('9989c40b-7874-45a7-a724-14da854c6a64', contemporary_hyper_amendment_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9989c40b-7874-45a7-a724-14da854c6a64', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(kesavananda_basic_structure__implied_limits_reading, kesavananda_basic_structure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kesavananda_basic_structure__implied_limits_reading, textual_fidelity_adjudication).
narrative_ontology:constraint_beneficiary(kesavananda_basic_structure__implied_limits_reading, constitutional_identity_preservation).
narrative_ontology:constraint_victim(kesavananda_basic_structure__implied_limits_reading, unlimited_amendment_power_claimants).
narrative_ontology:constraint_victim(kesavananda_basic_structure__implied_limits_reading, parliamentary_supremacy_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITERALIST AMENDMENT ADVOCATE (SNARE) — A party seeking unlimited amendment authority (theoretically two-thirds Parliament + half the states) finds their textual reading — that 'amend' contains no intrinsic limit — foreclosed by the judicial doctrine. The constraint operates as suppression: the reading is accessible but rendered judicially illegitimate. No exit mechanism exists within the constitutional framework; the advocate is trapped in a subordinate interpretive position with no procedural remedy.
constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PARLIAMENTARY MAJORITARIANS / ORGANIZED FACTION (TANGLED ROPE) — Political movements seeking to amend or replace the Constitution through supermajority processes face dual constraints: judicial review of amendments (extraction: judicial veto power) but also genuine coordination benefits from the constitutional framework itself (rope function: shared text enables stable political contestation). Exit is constrained by political cost of destabilizing the legitimacy order, but not impossible — the faction can organize, contest, and attempt persuasion. Mixed experience: suppression is real (judicial review gate) and extraction is real (veto power), but coordination benefits exist (stability, predictability, shared reference frame).
constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL TEXT / TEXTUAL-FIDELITY DOCTRINE (ROPE) — The implied limits reading benefits the constitutional text itself as an institution: judicial enforcement of the doctrine preserves the text's identity (what it IS cannot be amended away) and enables stable institutional coordination around a fixed reference point. The judiciary, in enforcing textual fidelity, performs a coordination function (enabling all parties to reference a stable document) rather than pure extraction. From this perspective, the constraint is cooperative — the text's preservation enables everyone's political contestation within bounds.
constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / JUDICIAL INERTIA (PITON) — From a civilizational view, the judicial enforcement of implied limits has become substantially ritualistic. The doctrine persists through institutional momentum (courts cite Kesavananda; law schools teach the doctrine; political actors accept the framing) rather than through active verification of the underlying logic. The theater has accumulated: courts perform textual analysis of word 'amend' to produce a predetermined constraint on amendment power, but the analytical observer questions whether the textual reading is genuinely forced by the language or whether it reflects judicial institutional interests in preserving review authority. Theater ratio: high. The doctrine's enforcement is performative — it achieves its function (blocking unlimited amendment) through judicial interpretation rather than through explicit constitutional text.
constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: THE JUDICIARY / INSTITUTIONAL ENFORCER (TANGLED ROPE) — The courts benefit from enforcement of implied limits (preservation of judicial review authority and institutional legitimacy as constitutional guardians) while also performing genuine coordination (applying consistent doctrine, resolving interpretive disputes). Exit is constrained: the judiciary cannot simply abandon the doctrine without risking its own legitimacy or triggering constitutional crisis. The courts experience both extraction (benefit: preserved veto authority) and coordination (function: stable adjudication of boundary questions). The doctrine requires active enforcement — courts must continually re-interpret word 'amend' to block specific amendment proposals. This distinguishes tangled rope from pure rope: coordination exists (stable reference point) but asymmetric extraction also exists (judicial veto authority over amendment process).
constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: NATURAL LAW / AMENDMENT LOGIC (MOUNTAIN) — A civilizational observer might argue that the limit is inherent in the word 'amend' itself: logically, a power to 'change' cannot include a power to 'replace with something new.' The word contains its own boundary. This reading naturalizes the constraint: it appears as a logical law rather than a judicial doctrine. However, this perspective instantiates a false summit — the constraint is not logically forced but doctrinally constructed. The engine will flag this as naturalization of a contingent institutional choice.
constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kesavananda_basic_structure__implied_limits_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kesavananda_basic_structure__implied_limits_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(kesavananda_basic_structure__implied_limits_reading, TR),
    TR >= 0.70.

:- end_tests(kesavananda_basic_structure__implied_limits_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, accumulating. At origin (1973), the constraint was primarily coordinating — establishing a stable reference point for constitutional interpretation (ε ≈ 0.15). Over 25 years, as courts invoked the doctrine to block specific amendment proposals, the extractiveness increased as judicial veto authority became visible and accumulated. The current value (0.35) reflects both coordination (stable text) and extraction (judicial control over amendment process). The trajectory is rising, indicating accumulation of extracted authority. Suppression (0.48): Moderate. The doctrine suppresses literalist readings of 'amend' (unlimited power) but does not suppress amendment entirely — many amendments have been permitted. The suppression is textually derived (the word 'amend' has limits) rather than purely coercive. Theater ratio (0.52): Moderate-high, rising. The doctrine's enforcement increasingly relies on judicial interpretation of abstract word 'amend' to reach predetermined blocking judgments, rather than on explicit textual gates. The rise from 0.38 to 0.52 suggests institutionalization and ritualization — the doctrine persists through momentum more than through active verification of textual necessity.
 *
 * PERSPECTIVAL GAP:
 *   The critical gap runs between the textual-fidelity reading (the constraint is derived from the word 'amend') and the institutional-power reading (the constraint concentrates veto authority in the judiciary). Both are true: the text does limit amendment, AND the judiciary has seized interpretive authority. The implied limits reading emphasizes textual derivation; the judicial usurpation critique emphasizes institutional concentration. These do not logically exclude each other (both can be true), but they lead to different evaluations: textual derivation suggests legitimacy; institutional concentration suggests illegitimacy. The perspectival gap is not logical but evaluative — it tracks whose interests are centered in the reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives d from the agent's relationship to the constraint: beneficiaries of textual-fidelity enforcement have low d (they gain from the constraint, so f(d) is negative or flat); victims of the amendment veto have high d (they bear suppression, so f(d) is positive). The judiciary, as institutional enforcer, experiences mixed directionality — they benefit (arbitrage exit via veto authority) but also perform coordination, yielding moderate d. Analytically, the doctrine's beneficiaries are identifiable (courts, text, fidelity adjudication) and distinct from victims (unlimited-power claimants, parliamentary supremacy doctrine), enabling clear directionality derivation without override. The tangled rope classification rests on the coexistence of genuine coordination (stable constitutional text) and asymmetric extraction (judicial review authority).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the classical sense (confusion between coordination and extraction) because the doctrine is genuinely hybrid: it coordinates interpretation (stable text reference) while extracting authority (judicial veto). The tangled rope classification captures this. However, the implied limits reading *itself* risks mandatrophy by emphasizing textual derivation (disguising the constraint as logical necessity) while the institutional-power reading emphasizes veto authority (disguising the constraint as judicial aggrandizement). The resolution is not to collapse the readings into one type, but to recognize that all three readings are structurally valid — they highlight different aspects of the same doctrine. The judicial usurpation reading does not falsify the implied limits reading; it adds institutional context. The democratic safeguard reading does not contradict either; it adds historical context. The presheaf over the observation site (beneficiary, victim, organized enforcer, analytical critic) IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_necessity_vs_judicial_interpretation,
    'Is the limit on amendment power logically inherent in the word ''amend,'' or is it a judicial interpretation choice that could have been made differently?',
    'Comparative constitutional law: how do other democracies interpret equivalent amendment clauses? Do they derive identical limits from identical language? If interpretation diverges, limit is not textually necessary.',
    'If limit is textually necessary: mountain classification confirmed (natural law), implied limits reading is universally true. If limit is interpretive choice: constraint is tangled rope (judicial doctrine with asymmetric beneficiary), implied limits reading is one reading among others, and the natural law perspective is false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_necessity_vs_judicial_interpretation, empirical, 'Whether amendment limits derive from text necessity or judicial interpretation').

omega_variable(
    reading_sibling_contingency,
    'Do the democratic_safeguard_reading and judicial_usurpation_critique identify the same constraint or different constraints? Can both be true under the same implied limits doctrine?',
    'Textual analysis of Kesavananda judgment: does the opinion ground the doctrine in textual implication (implied limits reading) or in democratic policy judgment (democratic safeguard reading) or in institutional power-seeking (judicial usurpation)? One judgment may support multiple readings, or readings may be incompatible.',
    'If readings are mutually coexistent: all three are live doctrinal positions (coexists_with relations). If democratic safeguard and judicial usurpation are contradictory about the doctrine''s function: one forecloses the other within a consistent institutional framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_contingency, conceptual, 'Whether sibling readings of Kesavananda are logically coexistent or exclusive').

omega_variable(
    amendment_vs_replacement_boundary,
    'Where is the boundary between permissible ''amendment'' and impermissible ''replacement''? Can the boundary be stated with enough precision to be applied consistently, or is it inherently contestable?',
    'Case law analysis: what specific amendment proposals have courts accepted vs. rejected? Do courts apply a consistent principle or do outcomes track political factors? Can one generate a decision procedure from the doctrine or does it remain discretionary?',
    'If boundary is determinate: doctrine operates as constraint on amendment process (suppression is structural, not performative). If boundary is discretionary: doctrine is substantially performative (theater ratio rises), and the true constraint is judicial review authority, not textual limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_vs_replacement_boundary, empirical, 'Determinacy of the amendment/replacement boundary').

omega_variable(
    identity_presupposition_critique,
    'Does the doctrine assume a fixed constitutional identity (this Constitution has an immutable essence), or can identity be redefined through amendment process? Is identity metaphysical or conventional?',
    'Philosophical analysis: if the Constitution''s identity is conventional (what it IS is what the polity agrees it IS), then amendment process itself can redefine identity, and the implied limits doctrine collapses. If identity is metaphysical (discovered or intrinsic), the doctrine stands. Which framework guides the judgment?',
    'If identity is conventional: the constraint is not natural law but a choice to freeze identity at a historical moment (Partition-era India). If identity is metaphysical: constraint is natural law. The reading''s entire logical force depends on this presupposition, which the judgment may not articulate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_presupposition_critique, conceptual, 'Whether constitutional identity is metaphysical or conventional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kesavananda_basic_structure__implied_limits_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kbs_implied_theater_t0, kesavananda_basic_structure__implied_limits_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kbs_implied_theater_t10, kesavananda_basic_structure__implied_limits_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(kbs_implied_theater_t25, kesavananda_basic_structure__implied_limits_reading, theater_ratio, 25, 0.52).

% Extraction over time
narrative_ontology:measurement(kbs_implied_extract_t0, kesavananda_basic_structure__implied_limits_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(kbs_implied_extract_t10, kesavananda_basic_structure__implied_limits_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(kbs_implied_extract_t25, kesavananda_basic_structure__implied_limits_reading, base_extractiveness, 25, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(kbs_implied_supp_t0, kesavananda_basic_structure__implied_limits_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(kbs_implied_supp_t10, kesavananda_basic_structure__implied_limits_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(kbs_implied_supp_t25, kesavananda_basic_structure__implied_limits_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kesavananda_basic_structure__implied_limits_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(kesavananda_basic_structure__implied_limits_reading, kesavananda_basic_structure__democratic_safeguard_reading).
narrative_ontology:affects_constraint(kesavananda_basic_structure__implied_limits_reading, kesavananda_basic_structure__judicial_usurpation_critique).

% DUAL FORMULATION NOTE:
% Kesavananda basic structure doctrine decomposes into three distinct constraint stories representing three live interpretive readings of the same 1973 judgment. Each reading has different extractiveness values and different beneficiary/victim structures: (1) implied_limits_reading (ε=0.35, tangled_rope) — semantic implication of 'amend'; (2) democratic_safeguard_reading (ε=0.25, rope) — doctrine as protection against authoritarianism; (3) judicial_usurpation_critique (ε=0.55, snare) — doctrine as judicial veto authority. Network links show mutual influence: all three interpretations derive from the same judgment text, but each emphasizes different structural aspects (semantic, political, institutional). The ε values differ by ~0.3, confirming these are three structurally distinct constraints, not one constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
