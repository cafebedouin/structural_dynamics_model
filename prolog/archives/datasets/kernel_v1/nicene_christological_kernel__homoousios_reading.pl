% ============================================================================
% CONSTRAINT STORY: nicene_christological_kernel__homoousios_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_christological_kernel__homoousios_reading, []).

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
 *   constraint_id: nicene_christological_kernel__homoousios_reading
 *   human_readable: Nicene Homoousios Doctrinal Enforcement (Substance Identity Reading)
 *   domain: historical_theology/christology/ecclesiastical_authority
 *
 * SUMMARY:
 *   The homoousios reading of the Nicene christological kernel represents the
 *   institutional enforcement of doctrinal uniformity through imperial
 *   authority. Following the Council of Nicaea (325 CE), the formula 'Christ
 *   is homoousios (of one substance) with the Father' was imposed as the
 *   binding orthodoxy against subordinationist theologies that maintained the
 *   Son's derivativeness from the Father. This reading instantiates a snare
 *   constraint: it extracts doctrinal conformity through exile, anathema,
 *   property confiscation, and institutional erasure of dissenting
 *   communities (Arian churches, regional theological traditions). The
 *   constraint exhibits high extractiveness (0.68) because its primary
 *   function is to suppress theological diversity and consolidate imperial
 *   ecclesiastical authority, not to resolve a genuine coordination problem.
 *   The suppression requirement (0.72) rises over the measurement interval as
 *   enforcement machinery develops (imperial councils, church property laws,
 *   theological enforcement canons). The theater ratio (0.58) reflects that
 *   the homoousios formula increasingly functions as a symbol of orthodoxy
 *   (recited in creeds, enforced in ritual) rather than as a theological
 *   claim that addresses substantive Christological problems — by the 6th
 *   century, alternative formulations (Cappadocian synthesis, homoiousios
 *   compromises) had largely solved the coordination problems without
 *   requiring the full homoousios language. The constraint's false summit
 *   signature is high: analytical perspectives risk naturalizing this as an
 *   eternal truth of Christian doctrine when it is actually a historically
 *   contingent institutional choice enforced by imperial decree.
 *
 * KEY AGENTS:
 *   - Arian Theologians: Primary victims (powerless/trapped) — face exile and property confiscation for maintaining theological positions incompatible with homoousios; no exit option
 *   - Regional Theological Communities: Secondary victims (moderate/constrained) — Gothic Arians, North African churches with pre-Nicene traditions forced into conformity or marginalization
 *   - Imperial Ecclesiastical Hierarchy: Primary beneficiary (institutional/arbitrage) — Constantine and successors consolidate religious authority through doctrinal uniformity enforced by imperial councils and property laws
 *   - Nicene Orthodox Faction: Secondary beneficiary (organized/arbitrage) — bishops and theologians aligned with homoousios gain institutional resources and exclusivity
 *   - Non-Nicene Orthodox Coalition: Mixed actor (organized/constrained) — Semi-Arian theologians (Basil, Gregory of Nazianzus) benefit from broader anti-Pagan alliance but constrained by homoousios enforcement; later achieve modified synthesis
 *   - Conciliar Institution: Degraded actor (institutional/arbitrage) — Council of Nicaea persists as binding precedent long after functional necessity fades (piton perspective)
 *   - Theological Pluralism: Abstract victim (powerless/trapped) — diversity of Christological approaches is suppressed under uniformity mandate; no organized advocate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, 0.68).
domain_priors:suppression_score(nicene_christological_kernel__homoousios_reading, 0.72).
domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nicene_christological_kernel__homoousios_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_christological_kernel__homoousios_reading, snare).
narrative_ontology:human_readable(nicene_christological_kernel__homoousios_reading, "Nicene Homoousios Doctrinal Enforcement (Substance Identity Reading)").
narrative_ontology:topic_domain(nicene_christological_kernel__homoousios_reading, "historical_theology/christology/ecclesiastical_authority").

domain_priors:requires_active_enforcement(nicene_christological_kernel__homoousios_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_christological_kernel__homoousios_reading, 'e2a90f62-9490-440d-936f-84867b8a0408').
narrative_ontology:cs_kernel_codification('e2a90f62-9490-440d-936f-84867b8a0408', formalized).
narrative_ontology:cs_authority_grounding('e2a90f62-9490-440d-936f-84867b8a0408', extraction).
narrative_ontology:cs_interpretation_layer_present('e2a90f62-9490-440d-936f-84867b8a0408').
narrative_ontology:cs_reading_relation('e2a90f62-9490-440d-936f-84867b8a0408', nicene_christological_kernel__homoiousios_reading, forecloses).
narrative_ontology:cs_axiom('e2a90f62-9490-440d-936f-84867b8a0408', foundational, divine_substance_identity_necessary).
narrative_ontology:cs_axiom_status(divine_substance_identity_necessary, holdable).
narrative_ontology:cs_axiom_grounding('e2a90f62-9490-440d-936f-84867b8a0408', divine_substance_identity_necessary, deontological).
narrative_ontology:cs_axiom('e2a90f62-9490-440d-936f-84867b8a0408', foundational, doctrinal_uniformity_prerequisite_authority).
narrative_ontology:cs_axiom_status(doctrinal_uniformity_prerequisite_authority, holdable).
narrative_ontology:cs_axiom_grounding('e2a90f62-9490-440d-936f-84867b8a0408', doctrinal_uniformity_prerequisite_authority, instrumental).
narrative_ontology:cs_reference_frame('e2a90f62-9490-440d-936f-84867b8a0408', apostolic_substance_parity_framework).
narrative_ontology:cs_drift_state('e2a90f62-9490-440d-936f-84867b8a0408', post_cappadocian_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('e2a90f62-9490-440d-936f-84867b8a0408', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, imperial_ecclesiastical_hierarchy).
narrative_ontology:constraint_beneficiary(nicene_christological_kernel__homoousios_reading, nicene_orthodox_faction).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, theological_pluralism).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, regional_theological_autonomy).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, arian_communities).
narrative_ontology:constraint_victim(nicene_christological_kernel__homoousios_reading, subordinationist_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ARIAN THEOLOGIAN (SNARE) — Faces exile, property confiscation, and institutional erasure for maintaining that the Son's substance is subordinate to the Father (homoiousios). No exit option: recantation requires abandoning core theological conviction. Trapped by imperial decree (Council of Nicaea enforced by Constantine and successors). Suppression is total: theological speech is criminalized, synods are convened to enforce conformity, dissenting texts are burned.
constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: REGIONAL THEOLOGICAL COMMUNITY (SNARE) — Communities with existing subordinationist or semi-Arian traditions (Gothic Arians, North African communities influenced by pre-Nicene theology) face enforcement pressure: adopt homoousios doctrine or lose imperial favor, church resources, and social legitimacy. Constrained rather than trapped — regional churches can technically conform — but conformity requires abandoning generations of inherited theological framework. High cost, minimal coordination benefit.
constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMPERIAL ECCLESIASTICAL HIERARCHY (ROPE) — The Nicene formula (homoousios) provides the institutional benefit of doctrinal uniformity across the empire: a single, enforced doctrine eliminates regional variation and strengthens imperial religious authority. The enforcement machinery (imperial councils, exile, property confiscation) is visible but justified as necessary coordination. Benefits far exceed costs for institutional actors: consolidates power, centralizes authority, eliminates rival theological centers. Experiences the constraint as legitimate coordination — 'we must speak with one voice to preserve the Church.'
constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: NON-NICENE ORTHODOX COALITION (TANGLED ROPE) — Semi-Arian and homoiousios theologians (Basil of Caesarea, Gregory of Nazianzus) who reject full homoousios but accept modified orthodox frameworks. Organized enough to articulate alternatives but constrained by imperial enforcement. Some benefit from the broader Nicene coalition against pagan philosophy and Arian extremes; also extract costs of doctrinal subordination. Partial exit: some regional autonomy negotiable through compromise formulas (later Cappadocian synthesis). Mixed extraction and coordination.
constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: CONCILIAR INSTITUTION (PITON) — The Council of Nicaea as an institution persists through inertia and authority lineage long after its functional justification has degraded. By the 6th century, the homoousios formula has become a symbol of orthodoxy rather than a claim that resolves substantive Christological questions. The council persists because it is cited as authoritative precedent, not because it continues to solve the coordination problem it was created to solve. Theater ratio high: ritual recitation of the formula (the Nicene Creed as liturgical recitation) replaces theological engagement.
constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ETERNAL DOCTRINE VIEW (MOUNTAIN) — From a theological perspective within Christian orthodoxy, the homoousios formula expresses a necessary metaphysical truth about the Trinity that cannot be revised without abandoning Christian faith itself. The doctrine appears as a fixed, unchangeable feature of Christian theology — a natural law of Christological thought. However, this perspective naturalizes what is actually a historically contingent institutional choice made under imperial pressure. The engine's false summit detector will flag this as manufactured naturalization of doctrinal enforcement.
constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_christological_kernel__homoousios_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nicene_christological_kernel__homoousios_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_christological_kernel__homoousios_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nicene_christological_kernel__homoousios_reading, TR),
    TR >= 0.70.

:- end_tests(nicene_christological_kernel__homoousios_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The homoousios reading's primary function is to suppress theological diversity and consolidate imperial ecclesiastical power, not to coordinate genuine theological understanding. The constraint extracts conformity through institutional penalties (exile, anathema, property laws). The beneficiaries (imperial hierarchy, Nicene bishops) capture substantial institutional advantage: exclusive authority over doctrinal interpretation, control of church resources, elimination of theological rivals. The extraction rises over the measurement interval as enforcement infrastructure matures. Suppression (0.72): High and increasing. The enforcement mechanisms escalate from doctrinal pronouncement (325) to imperial legal sanctions (Constantine's successors): exile of Arian bishops, destruction of Arian texts, property confiscation, criminal penalties for theological dissent. Regional theological communities face conformity pressure or marginalization. No meaningful alternatives are permitted — recantation is the only exit, which requires abandoning core theological conviction. Theater ratio (0.58): Moderate-high and rising. At Nicaea, the homoousios formula was presented as a theological necessity — genuine metaphysical argument about divine substance. By 450 CE, the formula functions increasingly as a symbol and test of orthodoxy (recited in creeds, enforced in liturgy) rather than as a claim that continues to resolve the theological problems it was created to address. The Cappadocian synthesis had already established that the coordination problem could be solved with different terminology (hypostasis, energeia), yet homoousios persistence is based on institutional authority lineage rather than ongoing theological function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across the observation site. The Arian theologian experiences pure extraction and suppression (snare) — no coordination function, only institutional erasure. The regional community experiences constrained extraction (snare with some structural negotiation possible). The imperial hierarchy experiences pure coordination and benefit (rope) — they see the formula as legitimate uniformity-building. The non-Nicene orthodox see mixed coordination (broader anti-Pagan alliance) and extraction (subordination to homoousios formula) (tangled rope). The conciliar institution itself shows degraded function (piton) — it persists through authority inertia, not because it solves ongoing problems. The analytical observer risks seeing immutable theological truth (mountain) when confronting the formula's claim to eternal validity. The gap reveals that the constraint's classification depends entirely on the observer's structural position: beneficiaries of institutional consolidation see rope; victims of suppression see snare; observers outside the power structure risk naturalizing the enforcement as inevitable truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the homoousios enforcement regime. The Arian theologian (powerless/trapped) occupies maximum victimhood: no exit option, no benefit, full extraction cost (d ≈ 0.95, f(d) ≈ 1.42). The imperial hierarchy (institutional/arbitrage) occupies maximum beneficiary status: high exit options (can reinterpret or relax the formula), full extraction benefit, resource concentration (d ≈ 0.05, f(d) ≈ -0.12). Regional communities (moderate/constrained) occupy intermediate victimhood: exit cost is high but surmountable through conformity, some organizational capacity (d ≈ 0.70, f(d) ≈ 1.05). The non-Nicene organized coalition (organized/constrained) similarly occupies intermediate extraction: some benefit from Nicene alliance, but constrained by doctrinal subordination (d ≈ 0.55, f(d) ≈ 0.75). The piton institution (institutional/arbitrage) benefits from persistence (d ≈ 0.10, f(d) ≈ -0.08). The analytical observer occupies the false-summit position: structurally mobile (d ≈ 0.72, f(d) ≈ 1.15) but identity-locked into the theological tradition they are analyzing, unable to perceive the constraint's historical contingency from within the framework it naturalized.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION PATH: This reading resolves mandatrophy by explicitly instantiating the snare type through high extractiveness (0.68) and high suppression (0.72) with declared victims (Arian communities, theological pluralism, regional autonomy). The false summit (analytical/mountain perspective) is properly flagged by the engine because: (1) extractiveness is well above the mountain threshold (0.25), (2) suppression is well above mountain threshold (0.05), (3) emerges_naturally is not declared (would be false — the homoousios formula emerges from imperial decree, not natural law), (4) the constraint has declared victims (triggering FSM candidate evaluation). The engine will reclassify the mountain perspective as a false summit instantiation, revealing that the 'eternal doctrine' framing naturalizes what is actually a contingent institutional arrangement. The mandatrophy is resolved through the omega variables, which identify the core uncertainty: whether homoousios is metaphysical necessity (which would justify suppression as doctrinal enforcement) or historical contingency (which would indict suppression as institutional extraction). Historical and philosophical analysis of these omegas determines the true classification within the analytical/civilizational frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_claim_vs_imperial_enforcement,
    'Is homoousios a metaphysical necessity (eternal truth about divine substance) or a historically contingent institutional choice enforced by imperial power?',
    'Historical analysis: trace the theological argument for homoousios independent of Constantine''s imperial authority; compare Nicaea''s theological reasoning with later Cappadocian synthesis that achieved broader acceptance without full homoousios language; examine pre-Nicene Christological diversity to identify whether homoousios was the only coherent solution or one among viable alternatives.',
    'If metaphysical necessity: the constraint legitimately classifies as a theological mountain — unavoidable truth. If contingent choice: the snare classification is correct — imperial enforcement of a particular theological formula that suppresses alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(metaphysical_claim_vs_imperial_enforcement, conceptual, 'Whether homoousios is metaphysical necessity or historical contingency').

omega_variable(
    substance_terminology_adequacy,
    'Does the Greek term ''ousia'' (substance/essence) adequately express the metaphysical claim about the Father-Son relationship, or does the formula obscure genuine theological disagreement under a vocabulary constraint?',
    'Philosophical analysis of ousia terminology: examine the semantic field of substance language in Platonism, Stoicism, and pre-Nicene Christian theology; document post-Nicene theological disputes that arose from ambiguity in homoousios language (Arian reinterpretation, Cappadocian reformulation); identify whether alternative philosophical vocabularies (energeia, hypostasis) captured the intended meaning more precisely.',
    'If terminology adequate: homoousios expresses a stable claim and the enforcement is justified by doctrinal clarity. If terminology obscures: the formula''s enforcement power derives from vocabulary gatekeeping rather than metaphysical necessity — pure extraction masked as coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substance_terminology_adequacy, conceptual, 'Whether ousia terminology adequately expresses the theological claim').

omega_variable(
    regional_arian_persistence,
    'Why did homoousios-dissenting Christologies (Arian and semi-Arian variants) persist and flourish in regions beyond imperial enforcement reach (Gothic kingdoms, Persia), despite 150+ years of imperial suppression?',
    'Historical tracking: document the geographic distribution of Arian vs Nicene orthodoxy; identify regions where enforcement was weak and non-Nicene theology survived; examine whether persistence correlates with theological coherence of alternatives or merely with enforcement capacity gaps; analyze whether Arian communities that persisted longer produced theological refinements that addressed Nicene criticisms.',
    'If persistence = stronger alternative: the homoousios suppression prevented genuine theological progress — extractive suppression of viable alternatives. If persistence = enforcement gap: the formula''s authority would consolidate if enforcement were universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_arian_persistence, empirical, 'Why non-Nicene Christologies persisted despite suppression').

omega_variable(
    post_nicene_synthesis_dependency,
    'Did the Cappadocian synthesis (Basil, Gregory of Nazianzus, Gregory of Nyssa) achieve broader ecclesiastical acceptance by softening homoousios language, indicating that the original formula was over-restrictive?',
    'Doctrinal history: compare the Nicene formula''s reception (resisted by many bishops, required imperial enforcement) with the Cappadocian framework''s reception (voluntary adoption, broad consensus); analyze whether the synthesis resolved genuine theological problems that homoousios had created; identify which elements of homoousios were preserved vs modified in later orthodox tradition.',
    'If synthesis succeeded by relaxing homoousios: the original formula was extractive over-enforcement, and the constraint''s classification as snare is vindicated. If synthesis merely repackaged homoousios: the formula''s authority is robust despite reformulation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_nicene_synthesis_dependency, empirical, 'Whether Cappadocian synthesis indicates homoousios was over-restrictive').

omega_variable(
    reading_kernel_ambiguity,
    'Is the Nicene kernel (the Council of Nicaea''s christological pronouncement) best understood as a fixed formula (homoousios must be affirmed exactly) or as a flexible commitment (substance identity between Father and Son, but terminology negotiable)?',
    'Ecclesiastical history: trace how different church traditions after Nicaea interpreted the Council''s authority; identify which reformulations (Cappadocian, later councils) the church accepted as continuous with Nicaea vs which it rejected as violating Nicaea; examine whether the homoousios word itself or the doctrine it expresses became the binding norm.',
    'If kernel = fixed formula: this reading (homoousios enforcement) is the only legitimate instantiation; sibling readings (homoiousios) foreclose. If kernel = flexible commitment: multiple readings (homoousios, homoiousios, later Cappadocian variants) coexist as legitimate interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Nicene kernel: fixed formula vs flexible commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_christological_kernel__homoousios_reading, 325, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(homo_tr_t0, nicene_christological_kernel__homoousios_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(homo_tr_t25, nicene_christological_kernel__homoousios_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement(homo_tr_t50, nicene_christological_kernel__homoousios_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(homo_be_t0, nicene_christological_kernel__homoousios_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(homo_be_t25, nicene_christological_kernel__homoousios_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement(homo_be_t50, nicene_christological_kernel__homoousios_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(homo_su_t0, nicene_christological_kernel__homoousios_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(homo_su_t25, nicene_christological_kernel__homoousios_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(homo_su_t50, nicene_christological_kernel__homoousios_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_christological_kernel__homoousios_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, nicene_christological_kernel__homoiousios_reading).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, cappadocian_synthesis_hypostasis_framework).
narrative_ontology:affects_constraint(nicene_christological_kernel__homoousios_reading, arian_subordinationist_christology).

% DUAL FORMULATION NOTE:
% The Nicene christological kernel decomposes into at least two structurally distinct constraints: the homoousios reading (this story, high extractiveness, snare) enforces strict substance identity through imperial authority; the homoiousios reading (sibling story) negotiates substance similarity with broader regional acceptance (tangled rope or scaffold). The two readings represent alternative instantiations of the same kernel with different ε values, different suppression mechanisms, different victim sets, and different beneficiary structures. The Cappadocian synthesis downstream constraint shows how modified terminology (hypostasis, energeia) achieved doctrinal consensus without requiring full homoousios enforcement, indicating that the original reading's extraction was over-restrictive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nicene_christological_kernel__homoousios_reading, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
