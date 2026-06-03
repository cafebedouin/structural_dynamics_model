% ============================================================================
% CONSTRAINT STORY: vatican_ii_magisterial_authority__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vatican_ii_magisterial_authority__composite_overdetermination_reading, []).

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
 *   constraint_id: vatican_ii_magisterial_authority__composite_overdetermination_reading
 *   human_readable: Vatican II Magisterial Authority: Composite Overdetermination Reading
 *   domain: ecclesiastical_history/institutional_hermeneutics
 *
 * SUMMARY:
 *   Vatican II represents a unique institutional moment: an ecumenical
 *   council whose final documents simultaneously support incompatible
 *   theological readings. This constraint story instantiates the 'composite
 *   overdetermination reading'—the hypothesis that the Council texts were
 *   deliberately crafted as ambiguous compromises to achieve supermajority
 *   votes by encoding both continuity and rupture ecclesiology. The key
 *   structural feature is that neither faction (traditionalists seeking
 *   doctrinal continuity; reformers seeking pastoral renewal and theological
 *   development) could have produced these texts alone, yet both factions
 *   voted for them by reading them differently. The 10-12% rejection votes
 *   signal unresolved theological incompatibility embedded in the final
 *   documents, not isolated dissent. Implementation divergence across
 *   dioceses (progressive vs. conservative interpretations) flows
 *   structurally from the texts' overdetermination, not from pre-existing
 *   factions. The constraint concentrates hermeneutical authority in the
 *   institutional magisterium: only the post-conciliar Church can adjudicate
 *   which reading is 'authentic,' converting textual ambiguity into
 *   institutional power. The theater ratio (0.68) reflects that doctrinal
 *   consensus is performatively asserted while contradictory readings persist
 *   in actual implementation. The suppression (0.62) reflects that
 *   alternatives to the ambiguous compromise formulations—explicit continuity
 *   statements or explicit rupture statements—were systematically excluded
 *   from the final documents to maintain the supermajority coalition.
 *
 * KEY AGENTS:
 *   - Traditionalist Council Fathers (minority ~10-12%): victim/powerless/trapped — bound to magisterial documents they cannot authoritatively interpret; rejection votes ineffective
 *   - Reformist Council Fathers (majority ~88-90%): beneficiary/institutional/arbitrage — control theological innovation through hermeneutical ambiguity; can claim continuity while implementing rupture
 *   - Reformist Theological Elite (especially German, Belgian, French theologians): beneficiary/institutional/arbitrage — consolidate hermeneutical authority over interpretation; texts enable their theological project under magisterial cover
 *   - Progressive Bishops Implementation Coalition: beneficiary/organized/constrained — coordinate pastoral renewal via loose interpretation of ambiguous texts; benefit from textual flexibility
 *   - Conservative Bishops Implementation Coalition: victim/organized/constrained — constrained by texts that simultaneously validate and undermine their position; implementation divergence forces doctrinal concessions
 *   - The Magisterial Authority Structure (papal-episcopal): beneficiary/institutional/arbitrage — gains control over hermeneutical authority; texts enable fiat reinterpretation without theological justification
 *   - Doctrinal Clarity (collective good): victim/powerless/trapped — suppressed as alternative; Church trades doctrinal coherence for institutional flexibility
 *   - The Analytical Observer: observer/analytical/analytical — measures the constraint as a structurally coherent mechanism: coordination of Church adaptation to modernity + extraction of traditional doctrinal authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.58).
domain_priors:suppression_score(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.62).
domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(vatican_ii_magisterial_authority__composite_overdetermination_reading, "Vatican II Magisterial Authority: Composite Overdetermination Reading").
narrative_ontology:topic_domain(vatican_ii_magisterial_authority__composite_overdetermination_reading, "ecclesiastical_history/institutional_hermeneutics").

domain_priors:requires_active_enforcement(vatican_ii_magisterial_authority__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vatican_ii_magisterial_authority__composite_overdetermination_reading, 'd0175b66-8af3-40d1-b4bc-1e90c25a8564').
narrative_ontology:cs_kernel_codification('d0175b66-8af3-40d1-b4bc-1e90c25a8564', formalized).
narrative_ontology:cs_authority_grounding('d0175b66-8af3-40d1-b4bc-1e90c25a8564', extraction).
narrative_ontology:cs_interpretation_layer_present('d0175b66-8af3-40d1-b4bc-1e90c25a8564').
narrative_ontology:cs_reading_relation('d0175b66-8af3-40d1-b4bc-1e90c25a8564', vatican_ii_magisterial_authority__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0175b66-8af3-40d1-b4bc-1e90c25a8564', vatican_ii_magisterial_authority__rupture_reading, coexists_with).
narrative_ontology:cs_axiom('d0175b66-8af3-40d1-b4bc-1e90c25a8564', foundational, doctrinal_ambiguity_as_deliberate_compromise).
narrative_ontology:cs_axiom_status(doctrinal_ambiguity_as_deliberate_compromise, holdable).
narrative_ontology:cs_axiom_grounding('d0175b66-8af3-40d1-b4bc-1e90c25a8564', doctrinal_ambiguity_as_deliberate_compromise, empirically_contingent).
narrative_ontology:cs_axiom('d0175b66-8af3-40d1-b4bc-1e90c25a8564', foundational, hermeneutical_authority_consolidation_via_ambiguity).
narrative_ontology:cs_axiom_status(hermeneutical_authority_consolidation_via_ambiguity, holdable).
narrative_ontology:cs_axiom_grounding('d0175b66-8af3-40d1-b4bc-1e90c25a8564', hermeneutical_authority_consolidation_via_ambiguity, empirically_contingent).
narrative_ontology:cs_reference_frame('d0175b66-8af3-40d1-b4bc-1e90c25a8564', doctrinal_supermajority_coalitional_equilibrium).
narrative_ontology:cs_drift_state('d0175b66-8af3-40d1-b4bc-1e90c25a8564', contemporary_post_conciliar_era_stabilization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d0175b66-8af3-40d1-b4bc-1e90c25a8564', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, ecumenical_reformers).
narrative_ontology:constraint_beneficiary(vatican_ii_magisterial_authority__composite_overdetermination_reading, institutional_flexibility_advocates).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, doctrinal_traditionalists).
narrative_ontology:constraint_victim(vatican_ii_magisterial_authority__composite_overdetermination_reading, magisterial_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONALIST CLERGY (SNARE) — Bound to a magisterial text (the Council's decrees) that simultaneously validates their position (continuity language) and undermines it (rupture language). Cannot exit the constraint: they must teach the documents, but the documents contain contradictions they cannot resolve without appearing unfaithful. Maximum experienced extraction — the ambiguity is weaponized against them through hermeneutical authority shifts.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REFORM-MINDED BISHOPS (TANGLED ROPE) — Constrained by need for doctrinal legitimacy but also coordinating genuine pastoral renewal. The ambiguous texts enable their reform agenda (coordination benefit) while suppressing explicit continuity claims (extraction). They experience both genuine ecclesial coordination (serving the faithful) and asymmetric extraction (being freed from doctrinal constraints their opposition faces).
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMIST THEOLOGICAL ELITE (ROPE) — Primary beneficiary (institutional/arbitrage exit). The compromise formulations preserve their theological innovation under the guise of development; hermeneutical authority over interpretation becomes their domain. They experience the constraint as coordination: managing the tension between continuity and rupture language enables their position to dominate without theological refutation.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COUNCIL'S REFORMIST MAJORITY COALITION (TANGLED ROPE) — Organized agents (progressive bishops, theological advisors) who engineered the compromise texts. They coordinated genuine pastoral renewal (coordination function) while consolidating power through hermeneutical ambiguity (extraction function). The constraint served their strategic goal: supermajority votes on texts that could be read multiple ways.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MAGISTERIAL AUTHORITY STRUCTURE (PITON) — The formal teaching authority (papal magisterium, episcopal college) treats the Council texts as unified coherent statements while managing escalating implementation divergence. Performative unity: documents claim internal consistency while structural reality is fragmented. Theater ratio elevated because the institution must maintain the fiction of doctrinal coherence while allowing contradictory readings. Piton classification: the enforcement mechanism (appeals to Vatican II as unified authority) persists despite functional atrophy (no mechanism to resolve contradictory readings).
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: DOCTRINAL CLARITY (SNARE) — The abstract collective good of clear magisterial teaching cannot exit the constraint. The ambiguous formulations suppress alternatives (doctrinal precision, explicit choice between continuity and rupture) while concentrating hermeneutical power in institutional hands. Maximum extraction: the field loses coherent doctrinal framework; the Church gains flexibility to control interpretation.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Views the constraint as a structurally coherent institutional mechanism: Vatican II coordinates the Church's adaptation to modernity (genuine coordination function) while enabling the reformist theological coalition to consolidate institutional control through ambiguous language (extraction function). The 10-12% rejection votes signal unresolved theological incompatibility embedded in final texts, not isolated dissent. This reading dissolves the false binary: the texts are neither univocal development nor rupture, but rather overdetermined compromise encoding incompatible readings.
constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(vatican_ii_magisterial_authority__composite_overdetermination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(vatican_ii_magisterial_authority__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(vatican_ii_magisterial_authority__composite_overdetermination_reading, TR),
    TR >= 0.70.

:- end_tests(vatican_ii_magisterial_authority__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The constraint extracts from traditionalist agents (who face contradictory doctrinal authority they cannot resolve) and from doctrinal clarity as a collective good (suppressed in favor of institutional flexibility). The extraction increases over time (0.35 → 0.48 → 0.58) as the reformist interpretation becomes institutionally dominant and traditionalist positions lose hermeneutical standing. By year 20, the constraint has stabilized as a mechanism for enforcing reformist readings while claiming continuity. Suppression (0.62): Moderate-high. The alternative to ambiguous compromise formulations—explicit choice between continuity and rupture—is systematically excluded. Traditionalists cannot argue for strict continuity without appearing to reject the Council; reformers need not defend rupture explicitly because ambiguity permits implementation without explicit theological break. Theater ratio (0.68): High and rising. The performative assertion of doctrinal consensus increases over time (0.42 → 0.58 → 0.68) as implementation divergence becomes undeniable and institutional consensus becomes more difficult to maintain. The magisterial authority must work harder (higher theater) to preserve the fiction of unified doctrine. The rising theater indicates that the constraint's functional enforcement (hermeneutical control) is increasingly theatrical as the underlying theological divergence becomes visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full perspectival range. The traditionalist clergy see a snare: they are bound to texts they cannot control, and hermeneutical authority has been taken from them. The reform-minded bishops see tangled rope: they coordinate genuine pastoral renewal while benefiting from doctrinal flexibility. The reformist theological elite see rope: the ambiguity enables their theological innovation under legitimate magisterial cover. The reformist coalition sees tangled rope: they coordinated both genuine renewal and power consolidation. The magisterial authority sees its own process as piton-like: it must performatively assert consistency while managing escalating divergence. Doctrinal clarity sees snare: it is suppressed as an alternative, and the Church extracts flexibility at its expense. The analytical observer sees tangled rope: the structure coherently combines coordination (Church adaptation to modernity) and extraction (consolidation of hermeneutical control). The gap reveals that no single reading of Vatican II as univocal (whether pure continuity or pure rupture) captures the structural reality. The texts are overdetermined—they encode incompatible visions deliberately.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation flows from beneficiary/victim declarations and exit options. Traditionalist clergy are victims with trapped exit (high d → high f(d) → high experienced extraction). Reformist theologians are beneficiaries with arbitrage exit (low d → low f(d) → negative experienced extraction—they gain from the constraint). Reform-minded bishops are both coordinator-agents (moderate benefit from renewal) and constrained victims (moderate cost from doctrinal ambiguity)—their d is intermediate (constrained exit). The magisterial authority as beneficiary with analytical exit experiences its own institutional mechanisms as normal (low experienced extraction—they control the hermeneutics). The analytical observer at civilizational scope (universal scale) experiences neither pure extraction nor pure coordination, but rather the structural mechanism that produces both simultaneously depending on position. No directionality override is needed; the automatic derivation captures the structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that Vatican II is neither pure coordination (rope) nor pure extraction (snare), but rather a tangled hybrid that encodes incompatible visions through ambiguous language. The mandatrophy dissolves when we recognize that the texts were DESIGNED to be read multiple ways: the supermajority vote depended on each faction seeing what it wanted. This is not a failure of drafting—it is the success of institutional compromise. The constraint is a Tangled Rope because: (1) it coordinates genuine pastoral renewal (coordination function), (2) it consolidates hermeneutical authority in the magisterium (extraction mechanism), (3) it requires active enforcement (pastoral implementation guided by post-conciliar reinterpretation), and (4) it produces asymmetric extraction (reformers benefit from theological flexibility; traditionalists bear the cost of doctrinal ambiguity). The mandatrophy is fully resolved by the composite overdetermination reading: the texts ARE internally contradictory, AND this contradiction was an intended feature of the compromise mechanism, AND the Church's post-conciliar institutional management consists largely of controlling which reading dominates hermeneutically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_authority_locus_shift,
    'Is hermeneutical authority (power to determine what the texts mean) located in the texts themselves, in the authorial intent of the Council Fathers, or in the institutional magisterium''s post-conciliar interpretation?',
    'Historical analysis of hermeneutical disputes (Lefebvre schism, progressive implementation, Vatican III speculation). Compare instances where text-based continuity argument was overruled by magisterial reinterpretation. Track whether the institution treats contradictory readings as errors or as legitimate development.',
    'If authority is textual: the contradictory readings violate the constraint and demand resolution (either continuity or rupture wins). If authority is in authorial intent: historical analysis reveals whether compromise was deliberate (supports overdetermination reading). If authority is magisterial: the constraint is self-reinforcing—the institution controls meaning through interpretation, making empirical falsification impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_authority_locus_shift, conceptual, 'Locus of hermeneutical authority for interpreting Vatican II texts').

omega_variable(
    rejection_vote_significance,
    'What do the 10-12% rejection votes on Council documents signify—isolated theological dissent, unresolved doctrinal incompatibility, or structural factions?',
    'Qualitative analysis of voting patterns by document and by bishop faction (conservative/progressive/centrist). Comparison with voting patterns on other councils (Nicaea, Trent, Vatican I). Correlation between rejection votes and subsequent implementation divergence by diocese.',
    'If isolated dissent: the overdetermination reading is exaggerated; texts are coherent with legitimate minority opposition. If structural incompatibility: rejection votes are a diagnostic signal that multiple irreconcilable readings coexist by design. If faction-driven: the constraint is a function of political coalition-building, not theological substance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rejection_vote_significance, empirical, 'Significance of 10-12% rejection votes as signal of embedded incompatibility').

omega_variable(
    implementation_divergence_causation,
    'Does implementation divergence (progressive vs. conservative dioceses following Council differently) flow from ambiguous texts, from divergent theological traditions predating the Council, or from post-conciliar power struggles over interpretation?',
    'Comparative study of 1960 pre-Council diocesan practice vs. 1970 post-Council practice, controlling for pre-existing faction. Analyze whether same bishop implements same text differently over time (support for shifting hermeneutical authority) or whether implementation differences correlate with pre-Council positions (support for pre-existing faction hypothesis).',
    'If textual ambiguity drives divergence: the overdetermination reading is supported. If pre-existing factions: texts merely enabled expression of prior theological divisions (texts are coordinate mechanism, not source of constraint). If hermeneutical authority shifts: the constraint is a function of institutional power dynamics, not textual content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_divergence_causation, empirical, 'Whether implementation divergence causally flows from textual ambiguity').

omega_variable(
    continuity_vs_rupture_text_coverage,
    'In specific disputed documents (Lumen Gentium on papal primacy, Unitatis Redintegratio on separated churches, Gaudium et Spes on natural law), what proportion of the text supports continuity reading vs. rupture reading? Is the split approximately 50-50 (overdetermination) or is one reading dominant with minority language accommodating the other?',
    'Detailed textual analysis by independent readers assigned to code each passage as continuity-supporting, rupture-supporting, or ambiguous. Calculate proportion distribution. Compare across disputed documents.',
    'If approximately 50-50 split: strong support for overdetermination hypothesis. If one reading dominant (70-80%): texts are coherent with minor contradictions (refinement, not overdetermination). If random distribution: texts are incoherent without deliberate structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(continuity_vs_rupture_text_coverage, empirical, 'Textual proportion of continuity vs. rupture support in disputed Council documents').

omega_variable(
    overdetermination_vs_development_boundary,
    'At what point does theological ambiguity cross from legitimate ''organic development'' (continuity reading axiom) into ''encoded incompatibility'' (overdetermination reading axiom)? What is the threshold metric?',
    'Establish criteria: (a) textual evidence of deliberate compromise phrasing (preparatory documents, voting patterns), (b) explicit rejection votes >10%, (c) implementation divergence uncorrelated with pre-existing factions, (d) institutional inability to reconcile readings without fiat reinterpretation. Measure how many criteria Vatican II satisfies.',
    'If Vatican II meets all criteria: overdetermination reading holds. If Vatican II meets <2 criteria: texts are coherent development. If 2-3 criteria: boundary case requiring judgment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(overdetermination_vs_development_boundary, conceptual, 'Threshold metrics distinguishing legitimate development from encoded incompatibility').

omega_variable(
    false_summit_naturallaw_risk,
    'Is the analytical observer''s classification (tangled_rope) naturalizing what is actually an institutional extraction mechanism? Could this constraint be classified as snare from the beneficiary''s perspective (hermeneutical controllers) rather than rope?',
    'Examine whether hermeneutical authority consolidation was an intended feature of the compromise texts or an unintended consequence. Compare institutional statements on textual interpretation authority before vs. after Council implementation. Measure extraction experienced by traditionalist clergy vs. coordination experienced by reformers.',
    'If hermeneutical consolidation was intended feature: beneficiary experiences rope (coordination of theological innovation under legitimate cover), victim experiences snare (trapped in authoritative text they cannot interpret). If unintended consequence: analytical reading of tangled_rope is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturallaw_risk, empirical, 'Risk of naturalizing hermeneutical authority consolidation as institutional feature rather than extractive mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vatican_ii_magisterial_authority__composite_overdetermination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vii_overdetermine_theater_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vii_overdetermine_theater_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(vii_overdetermine_theater_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(vii_overdetermine_extract_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vii_overdetermine_extract_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(vii_overdetermine_extract_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vii_overdetermine_suppress_t0, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(vii_overdetermine_suppress_t10, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(vii_overdetermine_suppress_t20, vatican_ii_magisterial_authority__composite_overdetermination_reading, suppression_requirement, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vatican_ii_magisterial_authority__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__continuity_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, vatican_ii_magisterial_authority__rupture_reading).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, doctrinal_implementation_divergence_diocesan_authority).
narrative_ontology:affects_constraint(vatican_ii_magisterial_authority__composite_overdetermination_reading, hermeneutical_control_post_conciliar_magisterium).

% DUAL FORMULATION NOTE:
% The vatican_ii_magisterial_authority kernel decomposes into three constraint stories, each representing a coherent reading of the Council. The composite overdetermination reading is upstream of the implementation divergence constraint (different dioceses follow the same ambiguous texts differently) and the hermeneutical control constraint (the post-conciliar magisterium consolidates authority over interpretation). The sibling readings (continuity and rupture) are constraints in their own right—each represents a coherent theological position that claims to be the authentic reading of Vatican II. All three readings coexist in the contemporary Church's institutional structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
