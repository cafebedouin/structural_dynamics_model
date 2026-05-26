% ============================================================================
% CONSTRAINT STORY: endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_endogenous_reinterpretation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation Reading: Manifesto as Prophetic Revelation with Divine Reframing
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'marriage commitment legitimacy' — specifically, the endogenous
 *   reinterpretation reading. The Manifesto (initial prophetic claim of
 *   marital dissolution and plural marriage as covenant stage) undergoes a
 *   dramatic reversal to monogamy, framed as divine instruction to 'preserve
 *   the Church for higher purposes.' The endogenous reinterpretation reading
 *   holds that this reversal is genuine prophetic revelation: God commanded
 *   the reversal to maintain the Church's apostolic succession and doctrinal
 *   coherence. This reading treats divine authority as the primary
 *   beneficiary (the Church preserves its legitimacy through theological
 *   continuity), not federal pressure as the cause (exogenous override
 *   reading) or institutional calculus as the foreground (hybrid pragmatic
 *   reading). The constraint exhibits low extractiveness (0.28) because the
 *   institutional beneficiary (the Church) maintains authority over its own
 *   narrative and the believer population, while constrained but not
 *   maximally suppressed, experiences the constraint as coordination
 *   mechanism (theological development) rather than pure coercion. The
 *   theater ratio (0.48) reflects that the reinterpretation requires
 *   sustained theological work — reframing monogamy as a new covenant stage —
 *   but the work remains within the Church's authorized interpretive
 *   tradition rather than requiring believers to acknowledge the reversal as
 *   contingent on external (federal) pressure.
 *
 * KEY AGENTS:
 *   - Institutional Church Authority: Primary beneficiary (institutional/arbitrage) — maintains prophetic legitimacy and doctrinal authority through authorized reinterpretation
 *   - Divine Authority (theological construct): Structural beneficiary — claimed source of the reversal command; grounds institutional authority to reinterpret doctrine
 *   - Rank-and-File Believers: Mixed experience (moderate/constrained) — experience both coordination (doctrinal coherence maintained) and extraction (prior understanding negated, identity lock prevents exit)
 *   - Dissenting Communities: Primary victims (powerless/trapped) — socially and economically dependent on the Church yet unable to maintain doctrinal continuity; experience pure extraction
 *   - Theological Scholarship Community: Secondary victims with agency (institutional/constrained) — experience both coordination (new theological problems enabled) and extraction (career risk for critical examination)
 *   - Reform Movements: Organized observers (organized/constrained) — see the reversal as demonstrating need for transparent doctrinal development processes; frame the constraint as temporary (scaffold)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks false summitry by naturalizing the theological premise (divine authority can revise revelation) as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(endogenous_reinterpretation_reading, 0.28).
domain_priors:suppression_score(endogenous_reinterpretation_reading, 0.35).
domain_priors:theater_ratio(endogenous_reinterpretation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(endogenous_reinterpretation_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(endogenous_reinterpretation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(endogenous_reinterpretation_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(endogenous_reinterpretation_reading, "Endogenous Reinterpretation Reading: Manifesto as Prophetic Revelation with Divine Reframing").
narrative_ontology:topic_domain(endogenous_reinterpretation_reading, "religious_institutional_history/political_theology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(endogenous_reinterpretation_reading, fixed_text).
narrative_ontology:cs_authority_grounding(endogenous_reinterpretation_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(endogenous_reinterpretation_reading).
narrative_ontology:cs_kernel_id(endogenous_reinterpretation_reading, marriage_commitment_legitimacy).
narrative_ontology:cs_reading_relation(endogenous_reinterpretation_reading, exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation(endogenous_reinterpretation_reading, hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom(endogenous_reinterpretation_reading, foundational, manifestation_as_genuine_prophecy).
narrative_ontology:cs_axiom_status(manifestation_as_genuine_prophecy, holdable).
narrative_ontology:cs_axiom_grounding(endogenous_reinterpretation_reading, manifestation_as_genuine_prophecy, deontological).
narrative_ontology:cs_axiom(endogenous_reinterpretation_reading, foundational, divine_authority_revision_power).
narrative_ontology:cs_axiom_status(divine_authority_revision_power, holdable).
narrative_ontology:cs_axiom_grounding(endogenous_reinterpretation_reading, divine_authority_revision_power, theological).
narrative_ontology:cs_reference_frame(endogenous_reinterpretation_reading, prophetic_succession_integrity).
narrative_ontology:cs_drift_state(endogenous_reinterpretation_reading, contemporary_scholarly_scrutiny, gap(authority_erosion, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(endogenous_reinterpretation_reading, institutional_church_continuity).
narrative_ontology:constraint_beneficiary(endogenous_reinterpretation_reading, prophetic_authority_lineage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL CHURCH AUTHORITY (ROPE) — Experiences the constraint as legitimate coordination: the reversal is reframed as prophetic evolution (monogamy as new covenant stage), preserving the Church's apostolic legitimacy through theological continuity. No meaningful extraction because the institution maintains authority over its own doctrine and narrative. The Church benefits from the reframing's coherence: theological unity is preserved despite institutional rupture. Arbitrage exit reflects that institutional authority can arbitrate future doctrine revisions through the same mechanism.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2: RANK-AND-FILE BELIEVER (TANGLED ROPE) — Constrained by identity fusion with the Church's teaching authority and loss-aversion regarding doctrinal coherence. The reversal appears as both coordination (the Church solving an integrity crisis through reinterpretation) and extraction (the believer's previous understanding was negated, and they cannot exit without identity dissolution). The believer experiences moderate extraction because the Church's authority makes exit costly, but the theological reframing offers apparent justification — not maximal coercion. The constraint exhibits genuine coordination function (maintaining institutional unity) alongside asymmetric belief-bearing.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER — NATURAL LAW READING (MOUNTAIN) — From a civilizational/universal perspective, divine revelation is immutable by definition: if the Manifesto is genuine prophecy, then God's command to reverse prior doctrine is itself divine authority superseding previous revelation. The constraint appears as natural law — an unchangeable consequence of accepting the theological premise that God communicates through institutional lineage. However, this perspective risks false summitry: it naturalizes what is a contested theological claim rather than a logical necessity. The analytical observer must note that this classification depends entirely on the axiom that divine authority can revise prior revelation — a claim the other readings dispute differently.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: DISSENTING COMMUNITY (SNARE) — Trapped by social and economic dependence on the institutional Church (welfare, education, community identity) while unable to maintain doctrinal continuity. The reversal is experienced as pure extraction: the Church commands assent to doctrinal negation without offering theological rationale accessible to ordinary believers. The dissenting community bears the full cost of the reversal (cognitive dissonance, social rupture) while the Church benefits from institutional unity. No meaningful coordination function from this perspective — the reframing is experienced as coercive narrative management, not as genuine theological development. Maximum suppression because exit requires abandoning community, identity, and material support.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: THEOLOGICAL SCHOLARSHIP COMMUNITY (TANGLED ROPE) — Constrained by institutional affiliation and career dependence on denominational approval, yet also benefits from the reframing's theological coherence and the puzzle it generates. Scholars experience both coordination (the Church providing a framework for doctrinal development) and extraction (career risk for questioning the reinterpretation's adequacy, pressure to defend the narrative internally rather than assess it critically). The constraint exhibits genuine scholarly coordination — the reframing enables new theological work — alongside asymmetric incentives: scholars who defend the reframing advance; those who question it face professional penalties. Constrained exit because abandoning institutional affiliation costs professional legitimacy.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REFORM MOVEMENT (SCAFFOLD) — Organized agents (ecumenical bodies, reform theologians, lay councils) experience the constraint as a temporary coordination failure with possible sunset. The reinterpretation reading provides a pathway toward the sunset: if the Church can reframe monogamy as prophetic evolution (new covenant stage) rather than reversal, then ongoing doctrinal development becomes the norm rather than the crisis. The scaffold classification reflects that reform movements see the constraint as solvable through institutional evolution toward transparent doctrinal development processes. Sunset clause: as church governance becomes more deliberative and less dependent on claims of prophetic continuity, the need for endogenous reinterpretation (the strategy this constraint embodies) diminishes.
constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(endogenous_reinterpretation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(endogenous_reinterpretation_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): LOW. This reading's distinguishing feature is that the Church maintains control over the legitimacy narrative — the reversal is presented as divine instruction, not as institutional capitulation. Because the primary beneficiary (the Church's institutional authority) remains authorized to define the theological framework, and because the legitimacy claim (prophetic revelation) is internally coherent within the Church's interpretive tradition, the constraint exhibits coordination-like properties (rope classification for the institutional perspective) rather than pure extraction. The extractiveness value reflects the believer-level asymmetry (their prior understanding is negated) offset by the fact that the Church offers a theological rationale (new covenant stage) rather than naked coercion. Over the measured interval (t=0 to t=10), extractiveness rises from 0.18 to 0.28 as the reinterpretation's inadequacy becomes apparent to scholars and dissenting communities — the initial euphoria of 'divine guidance' wears off and the reversal's contingency becomes visible. Suppression (0.35): MODERATE. The Church's authority constrains dissent and creates identity lock for believers (exit costs are cognitive/identity-based rather than purely material), but the suppression is not total because the theological reframing offers apparent internal justification. Over time, suppression remains stable as institutional incentives for belief conformity persist. Theater ratio (0.48): MODERATE. The endogenous reading requires sustained theological work — reframing monogamy as covenant development, not reversal — but this work remains within authorized interpretive tradition. The theater reflects the gap between 'divine command' (the claimed grounding) and 'institutional need' (the actual structural driver). Over the interval, theater rises (0.35 to 0.48) as the gap becomes visible to scholars and as reform movements publicize the arbitrariness of the 'new covenant stage' concept.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of experienced classification from a single ε value (0.28). The institutional Church sees coordination (Rope) — the reinterpretation preserves doctrinal authority and theological unity. Believers see mixed coordination and extraction (Tangled Rope) — the theology is offered as development, but the reversal negates their prior understanding. Dissenting communities see pure extraction (Snare) — the Church imposes the reversal without accessible theological rationale and suppresses dissent. The analytical observer risks seeing natural law (Mountain) — if divine authority can revise revelation, the reversal is logically inevitable — but this naturalizes what is a contested theological claim. The reform movement sees a temporary problem (Scaffold) — the constraint will dissolve as church governance becomes more transparent about doctrinal change. The scholarship community sees mixed coordination and extraction (Tangled Rope) — new theological work is enabled but career incentives distort critical assessment. The perspectival gap reveals that the same institutional arrangement (the Church's authority to reinterpret doctrine) is experienced as coordination by beneficiaries and extraction by those whose prior understanding is negated.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values (d) are derived from the agents' structural positions relative to divine authority as beneficiary. Institutional actors (the Church authority) who experience arbitrage exit have low d (0.1–0.2): they benefit from the reframing and can arbitrate future doctrine. Institutional actors with constrained exit (scholarship community) have moderate d (0.4–0.5): they benefit from the reframing's theological puzzles but face career risk for questioning it. Moderate agents with constrained exit (believers) have d ≈ 0.55: they experience both coordination and extraction. Powerless agents with trapped exit (dissenting communities) have high d (0.85–0.95): they bear extraction without exit options and without benefit from the Church's legitimacy. The analytical observer has d ≈ 0.72 (the standard canonical value for analytical context) but risks false summitry by naturalizing the theological premise rather than assessing it as a contested claim.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT trigger mandatrophy resolution because extractiveness (0.28) is below the 0.46 threshold. However, the committer frame structure (kernel reading vs. sibling readings) means the analytical work must address: How does THIS reading's claim (divine authority benefits from endogenous reinterpretation) differ from the exogenous override reading's claim (federal pressure causes the reversal) and the hybrid pragmatic reading's claim (institutional calculus foregrounds doctrine)? The three readings should produce different ε values if they are genuinely distinct constraints. The endogenous reading's low extractiveness (0.28) reflects divine authority as beneficiary (low perceived coercion in the institutional perspective). An exogenous override reading would show higher extractiveness (institutional capitulation to external pressure, experienced as coercive). A pragmatic reading would show moderate extractiveness (institutional logic but without theological legitimation). If the three readings' ε values differ by more than 0.15, they are structurally distinct constraints linked by network relationships. If they overlap, they are perspectival framings of the same constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prophetic_authenticity_threshold,
    'What criteria distinguish genuine prophetic revelation from institutional narrative management?',
    'Historical comparison across religious traditions: does claimed revelation cohere with prior doctrine without requiring reinterpretation? Are unexpected doctrinal reversals characteristically explained through new revelation? Does the timing of revelations correlate with institutional pressure?',
    'If revelation authenticity is verifiable by external criteria: the reading''s foundational axiom (manifestation_as_genuine_prophecy) is holdable. If revelation authenticity is unfalsifiable: the axiom is empirically contingent but epistemically closed, and the reading relies on circular reasoning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prophetic_authenticity_threshold, empirical, 'Criteria for distinguishing genuine prophecy from institutional reinterpretation').

omega_variable(
    theological_discontinuity_degree,
    'Does the monogamy reversal represent genuine theological development (new covenant stage) or substantive doctrinal contradiction?',
    'Detailed systematic theology analysis: comparison of prior teaching (marriage covenant sanctity, conjugal sexual ethics) with the reversal; assessment of whether reframing monogamy as ''new covenant stage'' preserves semantic content or evacuates it. Cross-tradition comparison: do other religions reinterpret foundational doctrines through similar ''stage'' frameworks?',
    'If monogamy can be coherently reframed as developmental (new stage, not reversal): the reading''s theological continuity claim is defensible. If the reversal constitutes substantive contradiction: the reading''s reinterpretation strategy is exposed as narrative smoothing rather than genuine theological development.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_discontinuity_degree, conceptual, 'Whether the monogamy reversal represents genuine development or substantive contradiction').

omega_variable(
    institutional_authority_circularity,
    'Does the Church''s authority to reinterpret its own doctrine depend on the Church''s prior claim to have issued that doctrine as revelation? (Potential self-certification loop.)',
    'Analysis of the institutional authority claim''s grounding: Does the Church ground its authority to reinterpret revelation in the fact that it issued the prior revelation? Or in an independent source of authority (council decisions, scholarly consensus, exegetical tradition)? Historical audit: in cases of major doctrinal reversals across religions, what authorities justify the reversal — institutional self-certification or external legitimacy claims?',
    'If the reading depends on self-certification (Church authority rests on Church''s prior claims): the constraint exhibits high theater — the reinterpretation is performatively legitimate but lacks external verification. If the reading appeals to independent authority: theater is lower and the reading is epistemically stronger.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_authority_circularity, conceptual, 'Whether institutional authority grounds itself circularly in its own prior claims').

omega_variable(
    committer_frame_sibling_divergence,
    'Which structural features distinguish THIS reading (endogenous reinterpretation: divine authority as beneficiary) from the exogenous_override_reading (federal pressure as primary cause) and hybrid_pragmatic_reading (institutional calculus foregrounds doctrine)?',
    'Explicit structural comparison: For each reading, identify (a) who benefits from the reversal, (b) what authority grounds the reversal, (c) whether doctrinal continuity is preserved or acknowledged as broken. The readings should show distinct beneficiary structures, distinct authority groundings, and distinct relationships to theological continuity.',
    'If the readings differ in ε by > 0.15: they are structurally distinct constraints (decomposition confirmed). If ε values overlap: the readings may be perspectival framings of a single constraint rather than genuinely distinct claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_sibling_divergence, conceptual, 'Structural differentiation between this reading and sibling readings of the marriage legitimacy kernel').

omega_variable(
    theater_mechanism_endogenous_specificity,
    'Is the theater in this reading (reframing reversal as prophetic evolution) higher or lower than theater in alternative readings that justify the reversal through exogenous pressure (federal law) or pragmatic institutional logic?',
    'Comparative analysis of the reinterpretation burden: Endogenous reading requires believers to accept (a) the Manifesto was genuine prophecy AND (b) God commanded the reversal AND (c) monogamy is now the covenant stage. Exogenous reading requires accepting (a) federal pressure forced the reversal AND (b) the Church adapted. Pragmatic reading requires accepting (a) institutional logic dictated the change AND (b) doctrine adapts to circumstances. Count the number of unsupported assertions each reading requires.',
    'If endogenous reading requires more sustained reinterpretation work: theater_ratio should be higher (0.6–0.8). If endogenous reading requires less theater because believers grant the theological framework: theater_ratio could be lower (0.4–0.5). Current measurement (0.48) reflects moderate theater — reinterpretation is significant but not maximal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_mechanism_endogenous_specificity, empirical, 'Comparative theater burden across sibling readings of the reversal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(endogenous_reinterpretation_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(endo_tr_t0, endogenous_reinterpretation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(endo_tr_t5, endogenous_reinterpretation_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(endo_tr_t10, endogenous_reinterpretation_reading, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(endo_be_t0, endogenous_reinterpretation_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(endo_be_t5, endogenous_reinterpretation_reading, base_extractiveness, 5, 0.24).
narrative_ontology:measurement(endo_be_t10, endogenous_reinterpretation_reading, base_extractiveness, 10, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(endogenous_reinterpretation_reading, hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% The marriage commitment legitimacy kernel admits three structurally distinct readings with different ε values and different beneficiary/authority structures. Each reading instantiates a different constraint with its own classification profile. The three readings coexist as live positions in institutional discourse; none is foreclosed by the others. Network links indicate cross-reading influence: all three readings reference the same historical reversal (the Manifesto's change from plural to monogamous teaching) but narrate its causation and legitimacy differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
