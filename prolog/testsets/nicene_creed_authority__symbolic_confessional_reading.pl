% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__symbolic_confessional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__symbolic_confessional_reading, []).

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
 *   constraint_id: nicene_creed_authority__symbolic_confessional_reading
 *   human_readable: Nicene Creed Authority (Symbolic-Confessional Reading)
 *   domain: systematic_theology/ecclesiology/history_doctrine
 *
 * SUMMARY:
 *   The Nicene Creed (325 CE) represents a pivotal effort to coordinate
 *   Christian belief across dispersed communities in the face of Arian
 *   heterodoxy. This constraint story models ONE reading of the creed's
 *   authority structure: the symbolic-confessional reading, which interprets
 *   the creed as historically contingent witness to the early church's
 *   faith-commitment rather than timeless metaphysical truth. In this
 *   reading, the creed's authority derives from community discernment and
 *   personal faith engagement, not from institutional enforcement of
 *   doctrinal correctness. The constraint exhibits rope classification
 *   (genuine coordination without significant extraction) at the
 *   congregational and analytical levels, tangled rope at the hierarchical
 *   institutional level (mixed coordination and institutional extraction),
 *   and piton characteristics at the doctrinal enforcement apparatus level
 *   (degraded function masked by continued theater). The low base
 *   extractiveness (0.18) reflects that the symbolic-confessional reading
 *   minimizes institutional extraction and vests authority in communities of
 *   faith. Suppression is moderate (0.25) because while the hierarchical
 *   church exercises authority over interpretation, the symbolic reading
 *   itself permits plurality and historical contingency, reducing the binding
 *   force of centralized doctrinal control. Theater ratio is moderate (0.35)
 *   at the constraint level because the confessional act (public profession
 *   of faith in the creed) involves genuine theological commitment, though
 *   enforcement apparatus maintains higher theater (monitoring orthodoxy,
 *   gatekeeping communion).
 *
 * KEY AGENTS:
 *   - Local Congregational Communities: Primary beneficiary (moderate/constrained) — experience the creed as enabling shared worship and collective discernment; authority vests in community recognition
 *   - Hierarchical Church Authority (Roman Catholic, Orthodox, High Anglican institutional structures): Secondary actor (powerful/arbitrage) — benefits from creed-based coordination of regional churches AND from institutional control of interpretation; experiences tangled rope structure
 *   - Interfaith Dialogue Practitioners: Beneficiary (moderate/mobile) — use symbolic-confessional reading to coordinate ecumenical theology across faith traditions
 *   - Doctrinal Enforcement Apparatus (theological seminaries, heresy courts, creedal tests): Institutional actor (institutional/arbitrage) — maintains enforcement theater despite degraded functional coordination (piton)
 *   - Analytical Observers (theological historians, comparative religionists): Observers (analytical/analytical) — recognize historical contingency and reinterpretability as structural features enabling the creed's longevity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__symbolic_confessional_reading, 0.18).
domain_priors:suppression_score(nicene_creed_authority__symbolic_confessional_reading, 0.25).
domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__symbolic_confessional_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__symbolic_confessional_reading, "Nicene Creed Authority (Symbolic-Confessional Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__symbolic_confessional_reading, "systematic_theology/ecclesiology/history_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__symbolic_confessional_reading, 'a11bb2a4-2a13-4926-9f06-5a6e8f2c4216').
narrative_ontology:cs_kernel_codification('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', formalized).
narrative_ontology:cs_authority_grounding('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', lineage).
narrative_ontology:cs_interpretation_layer_present('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216').
narrative_ontology:cs_reading_relation('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', nicene_creed_authority__strict_orthodox_reading, coexists_with).
narrative_ontology:cs_reading_relation('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', nicene_creed_authority__liturgical_habituation_reading, coexists_with).
narrative_ontology:cs_axiom('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', foundational, creed_historically_contingent).
narrative_ontology:cs_axiom_status(creed_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', creed_historically_contingent, empirically_contingent).
narrative_ontology:cs_axiom('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', foundational, authority_derives_from_community_discernment).
narrative_ontology:cs_axiom_status(authority_derives_from_community_discernment, holdable).
narrative_ontology:cs_axiom_grounding('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', authority_derives_from_community_discernment, conventional).
narrative_ontology:cs_reference_frame('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', early_christian_congregational_consensus).
narrative_ontology:cs_drift_state('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', contemporary_pluralist_ecumenical_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a11bb2a4-2a13-4926-9f06-5a6e8f2c4216', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, local_congregational_communities).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, interfaith_dialogue_practitioners).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__symbolic_confessional_reading, theological_pluralists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL CONGREGATION (ROPE) — Communities of faith experience the creed as a shared confessional commitment that enables coordination without requiring doctrinal uniformity. The constraint functions as coordination mechanism: the creed enables joint worship, shared liturgy, and collective discernment without imposing vertical enforcement of interpretation. Exit is constrained (leaving the congregation carries social cost) but not trapped — congregations retain agency to interpret the creed's symbols in light of contemporary experience. Extracted value (if any) is minimal because the coordination function is genuine and benefits the community.
constraint_indexing:constraint_classification(nicene_creed_authority__symbolic_confessional_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 2: HIERARCHICAL CHURCH AUTHORITY (TANGLED ROPE) — From the institutional perspective, the creed functions simultaneously as coordination (enabling universal communion and doctrinal coherence across regions) and as extraction mechanism (centralizing interpretive authority, controlling theological innovation, enabling institutional control over local practice). The hierarchical authority experiences the constraint as enabling their power — they coordinate regional churches AND extract institutional loyalty. Exit is available (schism, reformation) but costly (loss of communion, institutional legitimacy, resource access). This mixed structure — genuine coordination function + asymmetric extraction — defines tangled rope.
constraint_indexing:constraint_classification(nicene_creed_authority__symbolic_confessional_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: INTERFAITH DIALOGUE COMMUNITY (ROPE) — For practitioners of interfaith engagement, the symbolic-confessional reading of the creed enables coordination across faith traditions by parsing the creed's affirmations as historically situated witness rather than timeless metaphysical truth. This perspective sees the creed as a coordination mechanism for ecumenical theology: it establishes a shared historical reference point without requiring agreement on metaphysical interpretation. Exit is mobile (practitioners can shift to purely philosophical discourse) — the coordination function is sufficiently valuable to justify engagement, and no significant extraction burden is experienced.
constraint_indexing:constraint_classification(nicene_creed_authority__symbolic_confessional_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: DOCTRINAL ENFORCEMENT APPARATUS (PITON) — The institutional infrastructure for enforcing Nicene orthodoxy (theological education, heresy prosecution, creedal tests for communion) persists through institutional inertia despite degraded function in this reading. Enforcement theater remains high (the creed is still formally required, dissent is still monitored) but the actual coordination work is done by the symbolic-confessional interpretation itself (shared symbols, plural meanings). The enforcement apparatus is theatrical — it maintains the appearance of gatekeeping while the gate has functionally shifted to community discernment. Theater ratio elevated (0.35 at constraint level; 0.65+ at the enforcement apparatus level) because the apparatus performs orthodoxy control that the actual coordination mechanism (shared symbols, community interpretation) has already made functionally unnecessary.
constraint_indexing:constraint_classification(nicene_creed_authority__symbolic_confessional_reading, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (ROPE) — From the standpoint of historical and theological analysis, the Nicene Creed functions as a coordination mechanism for affirming Christ's divinity and enabling sacramental practice across dispersed communities without requiring uniform metaphysical framework. The analytical perspective recognizes that the creed's historical contingency (the specific formulations of Nicaea address 4th-century Arian heterodoxy, not timeless metaphysical truth) is precisely what enables its reinterpretation across centuries. The constraint is rope: it coordinates without forcing. The analytical observer sees low extraction because the genuine coordination function produces benefits for all participants, and authority derives from community recognition rather than institutional coercion.
constraint_indexing:constraint_classification(nicene_creed_authority__symbolic_confessional_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

test(piton_threshold) :-
    domain_priors:theater_ratio(nicene_creed_authority__symbolic_confessional_reading, TR),
    TR >= 0.70.

:- end_tests(nicene_creed_authority__symbolic_confessional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The symbolic-confessional reading decouples the creed's authority from institutional hierarchy and vests it in community discernment and personal faith engagement. While the hierarchical church may extract institutional loyalty through creedal gatekeeping, the symbolic-confessional reading itself generates minimal extraction because it enables plural interpretations, historical recontextualization, and interfaith dialogue. The agent experiencing the constraint (local congregations, interfaith practitioners) gains genuine coordination benefits without bearing disproportionate costs. Suppression (0.25): Moderate. The reading permits substantial theological freedom (congregations can reinterpret symbols; interfaith practitioners can engage across traditions) but maintains formal requirements (the creed is still required for communion in many traditions). Suppression is lower than the strict-orthodox reading (which would enforce uniform metaphysical interpretation) because the symbolic reading treats the creed's specific formulations as historically situated responses to specific controversies, not as fixed metaphysical propositions. Theater ratio (0.35): Moderate. The confessional act of professing the creed involves genuine theological commitment in this reading — believers confess concrete affirmations (Christ's divinity, incarnation, bodily resurrection) rather than merely performing institutional conformity. However, the enforcement apparatus maintains theater above the constraint level because institutional gatekeeping (monitoring orthodoxy, testing orthodoxy for communion) performs a control function that the symbolic reading has already rendered functionally superfluous. The trajectory shows rising theater ratio over time (0.20 → 0.35) as the creed's historical contingency became more explicit (Enlightenment historical consciousness, modern textual criticism, ecumenical dialogue) and enforcement theater intensified to maintain institutional gatekeeping in the face of interpretive diversity.
 *
 * PERSPECTIVAL GAP:
 *   The creed's classification varies dramatically across the indexed contexts. For local congregations with constrained exit, the creed functions as genuine coordination (rope) — it enables shared worship without requiring identical interpretation. For the hierarchical institutional authority with arbitrage options, the creed functions as mixed coordination-extraction (tangled rope) — it coordinates regional churches AND centralizes interpretive power. For the interfaith community with mobile exit, the creed is pure coordination (rope) — the symbolic reading enables interfaith theology without extraction burden. For the enforcement apparatus, the creed is degraded function (piton) — it persists through institutional inertia despite symbolic reinterpretation having already shifted the actual coordination work. The analytical observer sees rope across all contexts because the confessional reading minimizes extraction and maximizes coordination. This perspectival range demonstrates that the creed's classification is not intrinsic but relational — it depends on the observer's structural position relative to institutional authority and interpretive freedom.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to institutional authority and their capacity for theological reinterpretation. Local congregations experience moderate d (0.50–0.55): they are constrained by the formal requirement (cannot exit without cost) but also possess genuine agency (communities can interpret the creed's symbols locally). The hierarchical authority experiences low d (0.15–0.25): as beneficiary of creed-based institutional coordination and interpretive control, the institutional authority's extraction is modest relative to the genuine coordination it enables. Interfaith practitioners experience very low d (0.05–0.10): they are pure beneficiaries of the coordination mechanism with mobile exit (can shift discourse if the creed becomes too constraining). The enforcement apparatus experiences negative d (−0.05–0.05): it maintains theater but extracts little because the symbolic reading has already redistributed authority to communities. The analytical observer experiences moderate d (0.65–0.75): standing outside the institutional authority structure, the analyst experiences the constraint as a descriptive structure rather than a binding commitment, which shifts the perspective toward experiencing the extraction of narrative authority rather than doctrinal authority. The engine's sigmoid f(d) applies to these values to produce effective extractiveness chi; the low d values explain the low chi (≤ 0.35) across perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through low base extractiveness (0.18) and clear coordination function. The rope classification is not aspirational — local congregations and interfaith practitioners genuinely experience the creed as enabling shared meaning-making without disproportionate extraction. The tangled rope classification at the hierarchical level (mixing genuine coordination with institutional extraction) is not mislabeling: the creed simultaneously enables regional communion AND centralizes interpretive authority. The piton classification at the enforcement-apparatus level is appropriate because the apparatus maintains theater (monitoring orthodoxy, gatekeeping) while the symbolic reading has already shifted functional authority to communities. No type is forced or incoherent. The constraint resolves by recognizing that the same creed can function differently depending on the observer's relationship to institutional authority and theological interpretive freedom.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_metaphysical_axis,
    'Is the creed''s authority primarily theological-metaphysical (asserting timeless truth about Christ''s nature) or historical-confessional (bearing witness to the early church''s faith-commitment)?',
    'Comparative historical analysis: does interpretive authority vest in the metaphysical propositions (ousios, homoousios definitions) or in the act of confessing the creed''s core affirmations (Christ''s divinity, incarnation, resurrection)?',
    'If metaphysical: the creed requires hierarchical enforcement of correct interpretation (strict-orthodox reading, ε ≈ 0.42). If confessional: authority derives from community discernment and reinterpretation (symbolic-confessional reading, ε ≈ 0.18). This omega locates the kernel itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symbolic_vs_metaphysical_axis, conceptual, 'Whether creed authority is metaphysical or historical-confessional').

omega_variable(
    community_discernment_sufficiency,
    'Can local congregational communities reliably discern faithful reinterpretation of the creed without hierarchical doctrinal oversight?',
    'Historical case studies: comparison of heretical movements in contexts with strong hierarchical enforcement vs. contexts with distributed community discernment; examination of contemporary mainline Protestant communities where creedal enforcement is minimal',
    'If communities can discern faithfully: symbolic-confessional reading is structurally sound (rope classification holds). If communities diverge into incompatible theologies: reading degrades toward tangled_rope or snare (hierarchical enforcement becomes necessary). This determines whether the rope coordination is sustainable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_discernment_sufficiency, empirical, 'Whether congregational discernment maintains creedal fidelity without institutional enforcement').

omega_variable(
    interfaith_compatibility_boundary,
    'How far can the creed be reinterpreted as ''historically situated witness'' without losing its function as a Christian boundary marker?',
    'Interfaith theology literature analysis: identification of interpretive moves where Christian theologians have engaged Muslim, Jewish, and other faith traditions using creedal language; assessment of whether these engagements weaken Christian doctrinal identity or strengthen it through dialogical deepening',
    'If boundary remains stable: interfaith reading enables coordination with other faiths while maintaining Christian identity (rope at expanded scope). If boundary erodes: interfaith reinterpretation becomes functionally indistinguishable from generic monotheism, losing distinctive Christian content. This determines scope limitations of the coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interfaith_compatibility_boundary, conceptual, 'How far symbolic-confessional reinterpretation can extend before losing Christian specificity').

omega_variable(
    orthodox_sibling_coexistence,
    'Can the symbolic-confessional reading coexist within a single ecclesial community alongside the strict-orthodox reading, or does one necessarily displace the other?',
    'Ecclesiastical history: examination of mainline denominations and Catholic modernist movements where both readings have been held simultaneously; assessment of whether coexistence produces productive tension or institutional paralysis',
    'If coexistence: the two readings influence one another without foreclosing (influences relation). If displacement: one reading eventually forecloses the other within the community (forecloses relation). This determines the nature of the kernel itself — whether it is genuinely contested or whether one reading is in historical ascendance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orthodox_sibling_coexistence, empirical, 'Whether symbolic-confessional and strict-orthodox readings can coexist in one community').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__symbolic_confessional_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ncasc_theater_early_christian_period, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ncasc_theater_medieval_period, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(ncasc_theater_contemporary_period, nicene_creed_authority__symbolic_confessional_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(ncasc_extract_early_christian_period, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(ncasc_extract_medieval_period, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(ncasc_extract_contemporary_period, nicene_creed_authority__symbolic_confessional_reading, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__symbolic_confessional_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__symbolic_confessional_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% The Nicene Creed Authority kernel decomposes into three structurally distinct constraint stories corresponding to three live readings. Each reading instantiates different authority topologies, beneficiary/victim structures, and extractiveness profiles. This story (symbolic-confessional) prioritizes ε=0.18 (low extractiveness, rope coordination); the strict-orthodox reading prioritizes ε≈0.42 (tangled rope with institutional enforcement); the liturgical-habituation reading prioritizes ε≈0.25 (rope through embodied practice). The three stories are linked via network.affects_constraints and represent coexisting interpretive frameworks within contemporary Christianity rather than historical succession. Each story models what the constraint BECOMES under that reading's interpretation; the readings are not competing claims about a single fixed constraint but rather competing interpretations that constitute different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
