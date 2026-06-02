% ============================================================================
% CONSTRAINT STORY: homoousios_nicene__subordinationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_homoousios_nicene__subordinationist_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: homoousios_nicene__subordinationist_reading
 *   human_readable: Homoousios Compatible with Functional or Ontological Subordination (Subordinationist Reading)
 *   domain: historical_theology/ecclesiastical_history/philosophy_of_religion
 *
 * SUMMARY:
 *   The Nicene council's adoption of homoousios (same essence) in 325 CE
 *   created a doctrinal constraint that the subordinationist reading presents
 *   as interpretively open but institutionally closed. This constraint
 *   embodies a historical contest over whether the homoousios formula
 *   logically entails metaphysical equality of the Son to the Father, or
 *   whether it permits a hierarchy of derivation (functional or ontological)
 *   while preserving essential identity. The subordinationist
 *   reading—defended by Arian and Semi-Arian communities, and subtly
 *   maintained in Eastern theological distinctions between ousia and
 *   hypostasis—interprets homoousios as compatible with scriptural evidence
 *   for the Son's derivation from the Father. The Nicene orthodox consensus
 *   (supported by councils of Constantinople, Ephesus, Chalcedon, and later
 *   formalized in the Filioque controversy) treats the formula as forbidding
 *   any subordination whatsoever. The constraint is tangled rope: it
 *   coordinates Christian doctrinal discourse (provides common creedal
 *   language) while asymmetrically extracting interpretive freedom from those
 *   who read scripture as permitting subordination. The extractiveness has
 *   increased over the interval (0.25 → 0.38) as successive councils built
 *   enforcement capacity around the anti-subordinationist interpretation, and
 *   the theater_ratio has risen (0.40 → 0.65) as the institutional
 *   performance of doctrinal purity has outpaced the functional theological
 *   content.
 *
 * KEY AGENTS:
 *   - Subordinationist Theological Communities: Primary victims (powerless/trapped) — Arian, Semi-Arian, and later Ante-Nicene theological remnants forced into heresy status with no exit mechanism
 *   - Eastern Christian Theologians: Secondary victims (moderate/constrained) — maintain subordinationist sympathies through interpretive flexibility (ousia vs hypostasis) but constrained by institutional orthodoxy
 *   - Nicene Orthodox Consensus: Primary beneficiary (institutional/arbitrage) — councils (Nicaea, Constantinople, Ephesus, Chalcedon) enforce doctrinal unity through homoousios closure; extract intellectual authority
 *   - Historical-Critical Scriptural Scholars: Organized agents (organized/constrained) — modern interpreters recognize scriptural subordination but constrained by deference to magisterial tradition
 *   - Conciliar Institutional Maintenance: Institutional actor (institutional/arbitrage) — bishops and metropolitan authority maintain the anti-subordinationist formula through succession of councils and doctrinal enforcement
 *   - Analytical Theology: Civilizational view (analytical/analytical) — risks naturalizing the conciliar choice as logically necessary rather than historically contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(homoousios_nicene__subordinationist_reading, 0.38).
domain_priors:suppression_score(homoousios_nicene__subordinationist_reading, 0.52).
domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(homoousios_nicene__subordinationist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(homoousios_nicene__subordinationist_reading, tangled_rope).
narrative_ontology:human_readable(homoousios_nicene__subordinationist_reading, "Homoousios Compatible with Functional or Ontological Subordination (Subordinationist Reading)").
narrative_ontology:topic_domain(homoousios_nicene__subordinationist_reading, "historical_theology/ecclesiastical_history/philosophy_of_religion").

domain_priors:requires_active_enforcement(homoousios_nicene__subordinationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(homoousios_nicene__subordinationist_reading, 'd072ab6a-e401-4d41-b9a6-4c74c4b79291').
narrative_ontology:cs_kernel_codification('d072ab6a-e401-4d41-b9a6-4c74c4b79291', formalized).
narrative_ontology:cs_authority_grounding('d072ab6a-e401-4d41-b9a6-4c74c4b79291', lineage).
narrative_ontology:cs_interpretation_layer_present('d072ab6a-e401-4d41-b9a6-4c74c4b79291').
narrative_ontology:cs_reading_relation('d072ab6a-e401-4d41-b9a6-4c74c4b79291', homoousios_nicene__metaphysical_equality_reading, forecloses).
narrative_ontology:cs_reading_relation('d072ab6a-e401-4d41-b9a6-4c74c4b79291', homoousios_nicene__honorific_similarity_reading, influences).
narrative_ontology:cs_axiom('d072ab6a-e401-4d41-b9a6-4c74c4b79291', foundational, scriptural_subordination_authority).
narrative_ontology:cs_axiom_status(scriptural_subordination_authority, holdable).
narrative_ontology:cs_axiom_grounding('d072ab6a-e401-4d41-b9a6-4c74c4b79291', scriptural_subordination_authority, empirically_contingent).
narrative_ontology:cs_axiom('d072ab6a-e401-4d41-b9a6-4c74c4b79291', foundational, homoousios_semantic_openness).
narrative_ontology:cs_axiom_status(homoousios_semantic_openness, holdable).
narrative_ontology:cs_axiom_grounding('d072ab6a-e401-4d41-b9a6-4c74c4b79291', homoousios_semantic_openness, conventional).
narrative_ontology:cs_reference_frame('d072ab6a-e401-4d41-b9a6-4c74c4b79291', scriptural_subordinationist_framework).
narrative_ontology:cs_drift_state('d072ab6a-e401-4d41-b9a6-4c74c4b79291', contemporary_academic_theology, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d072ab6a-e401-4d41-b9a6-4c74c4b79291', '').
narrative_ontology:cs_kernel_id(homoousios_nicene__subordinationist_reading, homoousios_nicene).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, subordinationist_theological_communities).
narrative_ontology:constraint_beneficiary(homoousios_nicene__subordinationist_reading, scriptural_literalist_interpreters).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, conciliar_orthodox_consensus).
narrative_ontology:constraint_victim(homoousios_nicene__subordinationist_reading, metaphysical_egalitarian_theology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATIONIST THEOLOGICAL REMNANTS (SNARE) — Communities committed to subordination as scripturally necessary cannot escape the Nicene council's exclusionary definition without abandoning their scriptural hermeneutic. Trapped by the forced choice: accept metaphysical equality (violates scriptural reading) or be declared heretical. No exit mechanism; maximum experienced suppression.
constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: EASTERN THEOLOGIANS WITH RESIDUAL SUBORDINATION (TANGLED ROPE) — Can maintain theological practice through interpretive flexibility (ousia vs hypostasis distinction, economic vs ontological trinities) but face career and doctrinal constraints within institutional orthodoxy. Genuine coordination function exists (the homoousios language does enable shared liturgical and dogmatic framework) alongside asymmetric extraction (interpretive freedom is curtailed by conciliar definition).
constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: NICENE ORTHODOXY / METROPOLITAN AUTHORITY (ROPE) — Institutional beneficiary. The homoousios formula, interpreted as forbidding subordination, enables doctrinal unity and metropolitan control of theological variation. Experiences the constraint as pure coordination: establishing shared creedal language solves the fragmentation problem. Benefits from the interpretive closure that subordinationist readings create as transgressive.
constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: HISTORICAL-CRITICAL SCHOLARSHIP (TANGLED ROPE) — Modern interpreters see the subordinationist reading as textually grounded (scriptural evidence for functional subordination is substantial) and the Nicene ban on it as a doctrinal imposition. Organized but constrained by institutional deference to magisterial authority. Both benefits from the constraint (rigorous textual analysis is now possible because boundaries are clear) and bears extraction (canonical doctrine forecloses certain valid interpretive paths).
constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONCILIAR INSTITUTIONAL MAINTENANCE (PITON) — The councils that built on Nicene authority now maintain the anti-subordinationist formula through institutional inertia and performance of doctrinal purity. The functional content has atrophied — modern Christian practice (Incarnation theology, liturgical subordination language) does not require the strict equality principle. Theater is high: the prohibition persists despite weakened enforcement and internal theological drift.
constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / LOGICAL NECESSITY (MOUNTAIN) — From a civilizational view, metaphysical equality is logically necessary to avoid infinite regress (if Son derives being from Father, infinite chain of dependence) or to preserve divine simplicity (perfection cannot be ranked). This perspective treats the prohibition of subordination as a natural law of trinitarian metaphysics. However, structural data contradicts the mountain: the constraint depends on specific metaphysical frameworks (simplicity, necessity) that are themselves contested and historically contingent.
constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(homoousios_nicene__subordinationist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(homoousios_nicene__subordinationist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(homoousios_nicene__subordinationist_reading, TR),
    TR >= 0.70.

:- end_tests(homoousios_nicene__subordinationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The subordinationist reading identifies real scriptural evidence for the Son's derivation from the Father, but the Nicene closure does not eliminate subordinationism entirely — it makes it heretical. The extraction is moderate rather than severe because interpretive flexibility (Eastern theology's ousia/hypostasis distinction, economic vs ontological trinity frameworks) permits some subordination discourse to persist within Orthodox tradition. Modern theological practice largely accommodates Incarnation and christological subordination without contradiction. The interval trajectory (0.25 → 0.38) reflects increasing enforcement intensity as councils accumulated. Suppression (0.52): Moderate-high. Significant barriers to subordinationist discourse include conciliar anathemas, magisterial prohibition, and the cost of heterodox labeling. However, suppression is not total — academic theology, Eastern Orthodox flexibility, and Protestant textual attention permit subordinationist readings in carefully qualified forms. Suppression has intensified from 0.35 to 0.52 as institutional enforcement accumulated (councils of Constantinople, Ephesus, Chalcedon, later Lateran councils). Theater ratio (0.65): High and rising. Doctrinal purity performance has outpaced functional theological content: modern Christian practice and worship readily accommodate functional subordination (Christ submits to the Father in the Incarnation; Son's will is aligned with Father's) without violating the Nicene formula. The theater has risen from 0.40 to 0.65 as councils multiplied and doctrinal formulations became more elaborate.
 *
 * PERSPECTIVAL GAP:
 *   The subordinationist reading demonstrates deep perspectival gap. Subordinationist communities perceive the constraint as a snare — total extraction, no exit. Eastern theologians perceive tangled rope — genuine doctrinal coordination (creedal unity) alongside interpretive extraction. Nicene orthodoxy perceives pure rope — coordination mechanism solving fragmentation. Scholars perceive tangled rope — both constraints and enablements of rigorous exegesis. The conciliar system perceives piton — institutional maintenance of a formula whose functional content has partially atrophied. The analytical observer risks perceiving mountain (metaphysical necessity) when the constraint is historically contingent (tangled rope). The perspectival gap is the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the constraint. Subordinationist communities have no exit (trapped → high d → high f(d) → high experienced extraction). Sympathetic Eastern theologians face constrained exit (can maintain practice through interpretation but cannot claim orthodoxy openly → moderate d). Metropolitan authority benefits from closure (arbitrage → low d → negative f(d)). Modern scholars are organized but constrained by deference (moderate d, mixed benefits and costs). The conciliar system maintains the constraint through institutional inertia (institutional/arbitrage, low d). The analytical observer risks computing false necessity (high d at analytical level, but the mountain perspective misses the contingency).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: The subordinationist reading resolves the mandatrophy by showing that the homoousios kernel admits at least two coherent readings with genuinely different extraction profiles. The subordinationist reading (this story) interprets homoousios as compatible with scriptural evidence for functional or ontological subordination; it classifies as tangled_rope. The metaphysical_equality_reading interprets homoousios as entailing strict metaphysical equality; it would classify as rope or mountain. The honorific_similarity_reading treats homoousios as a council-defined consensus label without independent metaphysical content; it would classify as scaffold. No single type 'is' correct — the kernel's underdetermined semantics admit all three. The mandatrophy is resolved by recognizing that the presheaf of readings over the kernel IS the constraint structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    functional_vs_ontological_subordination,
    'Does the scriptural evidence support functional subordination (Son''s agency derives from Father in economic order) but not ontological subordination (Son''s being/essence is independent)? Can these be consistently distinguished within a single framework?',
    'Detailed exegetical analysis of subordination language in John, Hebrews, 1 Corinthians; philosophical reconstruction of what functional vs ontological subordination entails in terms of agency, causation, and being; examination of whether the distinction is coherent or collapses under scrutiny.',
    'If coherent distinction: subordinationist reading is internally consistent and textually defensible; Nicene prohibition overreaches into ontology unnecessarily. If distinction collapses: any functional subordination implies ontological dependence; Nicene equality principle is metaphysically sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_vs_ontological_subordination, empirical, 'Whether functional and ontological subordination are distinguishable').

omega_variable(
    homoousios_linguistic_underdetermination,
    'Does homoousios (same essence) logically entail metaphysical equality of power and agency, or is it compatible with a hierarchy of derivation while preserving essential identity?',
    'Semantic analysis of homoousios in Greek metaphysics (Plato, Aristotle, Stoics); examination of whether ''same ousia'' is compatible with functional subordination in early Christian theological applications; reconstruction of what the Nicene bishops explicitly intended vs what the formula semantically permits.',
    'If homoousios is compatible with subordination: subordinationist reading is a valid reading of Nicene language; conciliar tradition has imposed an additional constraint (equality) beyond what the formula requires. If homoousios logically entails equality: subordinationist reading is internally contradictory; prohibition is semantically justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(homoousios_linguistic_underdetermination, conceptual, 'Semantic scope of homoousios (does it entail equality?)').

omega_variable(
    scriptural_authority_vs_conciliar_tradition,
    'When scriptural evidence and conciliar definition diverge, which source carries greater epistemic authority within Christian theology? Is this a resolvable question or a constitutive disagreement rooted in different foundational commitments?',
    'Historical-theological analysis of how different Christian traditions have resolved conflicts between scripture and councils (Catholic magisterium, Orthodox synodal theology, Protestant sola scriptura); examination of whether any single framework coherently prioritizes both sources.',
    'If scripture can override councils: subordinationist reading becomes orthodox in some frameworks. If councils override scripture: subordination remains heretical and reads-as-excluded by definition. If irresolvable: different Christian traditions genuinely hold incommensurable epistemologies — not a factual disagreement but a structural one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scriptural_authority_vs_conciliar_tradition, preference, 'Epistemological priority: scripture vs conciliar tradition').

omega_variable(
    kernel_reading_contest_status,
    'Is the subordinationist reading a live theological position in contemporary Christianity, or has it been functionally foreclosed by centuries of conciliar development and magisterial enforcement?',
    'Survey of contemporary Christian theological discourse (academic, ecclesial, popular); identification of whether subordinationism appears as a defended position or only as a historical artifact; analysis of whether the foreclosure is doctrinal (councils have ruled it out) or practical (no living community defends it).',
    'If live: the subordinationist reading remains a genuine alternative within the kernel; coexists_with classification is correct. If functionally foreclosed: the reading persists as scholarly historical reconstruction but not as claimed orthodox position; may approach forecloses territory if modern theology''s egalitarian axioms are seen as mandatory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_status, empirical, 'Whether subordinationist reading remains a live theological position').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(homoousios_nicene__subordinationist_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_sub_tr_t0, homoousios_nicene__subordinationist_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(homo_sub_tr_t750, homoousios_nicene__subordinationist_reading, theater_ratio, 750, 0.55).
narrative_ontology:measurement(homo_sub_tr_t1500, homoousios_nicene__subordinationist_reading, theater_ratio, 1500, 0.65).

% Extraction over time
narrative_ontology:measurement(homo_sub_be_t0, homoousios_nicene__subordinationist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(homo_sub_be_t750, homoousios_nicene__subordinationist_reading, base_extractiveness, 750, 0.32).
narrative_ontology:measurement(homo_sub_be_t1500, homoousios_nicene__subordinationist_reading, base_extractiveness, 1500, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(homo_sub_su_t0, homoousios_nicene__subordinationist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(homo_sub_su_t750, homoousios_nicene__subordinationist_reading, suppression_requirement, 750, 0.48).
narrative_ontology:measurement(homo_sub_su_t1500, homoousios_nicene__subordinationist_reading, suppression_requirement, 1500, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(homoousios_nicene__subordinationist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(homoousios_nicene__subordinationist_reading, 0.12).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__metaphysical_equality_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, homoousios_nicene__honorific_similarity_reading).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, filioque_controversy).
narrative_ontology:affects_constraint(homoousios_nicene__subordinationist_reading, chalcedon_two_natures_constraint).

% DUAL FORMULATION NOTE:
% The homoousios kernel decomposes into three constraint stories, each with its own ε value and classification type, reflecting the kernel's semantic underdetermination. The subordinationist reading (this story) has ε=0.38 (tangled rope); the metaphysical_equality_reading has lower ε (rope or mountain); the honorific_similarity_reading has different suppression profile (scaffold). All three are linked via affects_constraints and share the same kernel_id but differ by reading_id and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(homoousios_nicene__subordinationist_reading, analytical, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
