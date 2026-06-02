% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__strict_orthodox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__strict_orthodox_reading, []).

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
 *   constraint_id: nicene_creed_authority__strict_orthodox_reading
 *   human_readable: Nicene Creed Authority (Strict Orthodox Reading)
 *   domain: systematic_theology/ecclesiology/doctrinal_policing
 *
 * SUMMARY:
 *   The Nicene Creed's authority in the strict orthodox reading binds all
 *   believers to a single metaphysical ontology (homoousios — 'of one
 *   substance') while treating deviation as heresy warranting institutional
 *   sanction. This constraint exemplifies how doctrinal enforcement operates
 *   as an extraction mechanism: the hierarchy (beneficiary) consolidates
 *   interpretive authority by policing metaphysical uniformity, while
 *   heterodox communities and lay interpreters (victims) face suppression and
 *   exile. The constraint exhibits classically snare characteristics — high
 *   extractiveness, high suppression, clear enforcement mechanisms, minimal
 *   coordination benefit to the victim group. The strict orthodox reading
 *   instantiates the creed kernel as a fixed metaphysical commitment binding
 *   all legitimate believers, with excommunication as the active enforcement
 *   gate. Sibling readings (symbolic interpretation, liturgical habituation)
 *   represent alternative ways of relating to the same creed text but with
 *   structurally different extraction profiles: the symbolic reading treats
 *   the creed as edifying metaphor (lower ε, different beneficiary
 *   structure); the liturgical reading treats it as embodied practice
 *   (different suppression mechanism). This story generates the strict
 *   reading's constraint shape — high extraction, clear victims, hierarchical
 *   beneficiary. The kernel context distinguishes this reading's structural
 *   commitments from the siblings' and routes the committer structure to
 *   omega variables documenting the metaphysical necessity question.
 *
 * KEY AGENTS:
 *   - Hierarchical Clergy: Primary beneficiary (institutional/arbitrage) — exclusive interpretive authority, consolidated power through doctrinal gatekeeping, institutional stability through creedal uniformity
 *   - Orthodox Establishment: Secondary beneficiary (institutional/arbitrage) — doctrinal purity as marker of institutional legitimacy, unified liturgical practice, protection of established order
 *   - Heterodox Communities: Primary victim (powerless/trapped) — subject to anathematization, institutional exile, property confiscation, violent suppression; cannot exit Christian community without losing all social standing
 *   - Lay Interpreters: Secondary victim (moderate/constrained) — theological autonomy suppressed; hermeneutical challenge met with heresy charge; career damage and social penalty for unauthorized interpretation
 *   - Philosophical Schools (Arian, Nestorian, Monophysite): Secondary victim (powerful/mobile) — can organize intellectual counter-traditions and maintain institutional structures outside established church, but face active suppression and military campaigns (Council of Nicaea used imperial coercion)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional power arrangement as logical necessity of coherent theology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, 0.62).
domain_priors:suppression_score(nicene_creed_authority__strict_orthodox_reading, 0.68).
domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__strict_orthodox_reading, snare).
narrative_ontology:human_readable(nicene_creed_authority__strict_orthodox_reading, "Nicene Creed Authority (Strict Orthodox Reading)").
narrative_ontology:topic_domain(nicene_creed_authority__strict_orthodox_reading, "systematic_theology/ecclesiology/doctrinal_policing").

domain_priors:requires_active_enforcement(nicene_creed_authority__strict_orthodox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__strict_orthodox_reading, '90825821-1e06-4803-a66f-012d03f689c2').
narrative_ontology:cs_kernel_codification('90825821-1e06-4803-a66f-012d03f689c2', formalized).
narrative_ontology:cs_authority_grounding('90825821-1e06-4803-a66f-012d03f689c2', extraction).
narrative_ontology:cs_interpretation_layer_present('90825821-1e06-4803-a66f-012d03f689c2').
narrative_ontology:cs_reading_relation('90825821-1e06-4803-a66f-012d03f689c2', nicene_creed_authority__symbolic_confessional_reading, forecloses).
narrative_ontology:cs_reading_relation('90825821-1e06-4803-a66f-012d03f689c2', nicene_creed_authority__liturgical_habituation_reading, forecloses).
narrative_ontology:cs_axiom('90825821-1e06-4803-a66f-012d03f689c2', foundational, metaphysical_claim_binding_all_believers).
narrative_ontology:cs_axiom_status(metaphysical_claim_binding_all_believers, holdable).
narrative_ontology:cs_axiom_grounding('90825821-1e06-4803-a66f-012d03f689c2', metaphysical_claim_binding_all_believers, deontological).
narrative_ontology:cs_axiom('90825821-1e06-4803-a66f-012d03f689c2', foundational, deviation_warrants_institutional_sanction).
narrative_ontology:cs_axiom_status(deviation_warrants_institutional_sanction, holdable).
narrative_ontology:cs_axiom_grounding('90825821-1e06-4803-a66f-012d03f689c2', deviation_warrants_institutional_sanction, deontological).
narrative_ontology:cs_reference_frame('90825821-1e06-4803-a66f-012d03f689c2', metaphysical_uniformity_essential).
narrative_ontology:cs_drift_state('90825821-1e06-4803-a66f-012d03f689c2', post_reformation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('90825821-1e06-4803-a66f-012d03f689c2', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, hierarchical_clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__strict_orthodox_reading, orthodox_establishment).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, heterodox_communities).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, lay_interpreters).
narrative_ontology:constraint_victim(nicene_creed_authority__strict_orthodox_reading, philosophical_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HETERODOX DISSIDENT (SNARE) — Structurally trapped within christendom; cannot exit theological community without losing family, property, and legal standing. Forced compliance with creedal statement despite private doubt. Maximum extraction: the constraint forces public affirmation of a metaphysical claim while suppressing the dissident's actual epistemic position.
constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: LAY INTERPRETER (SNARE) — Constrained by institutional authority and threat of excommunication. Cannot propose alternative readings of creedal language without heresy proceedings. High extraction: theological autonomy is suppressed; public rationalization must conform to clergy interpretation. Exit costs (social exile, economic marginalization) are severe but not absolute — some voluntary communities exist outside the established church.
constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIERARCHICAL CLERGY (ROPE) — Benefits from creedal authority as the exclusive licensed interpreter. The constraint solves genuine coordination problems for the institutional church: doctrinal uniformity enables unified liturgical practice, prevents doctrinal splintering, and consolidates organizational power. Extraction is present but experienced as coordination benefit — the clergy sees creedal enforcement as necessary ecclesiastical governance, not as coercion.
constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: PHILOSOPHICAL SCHOOL (TANGLED ROPE) — Mobile enough to challenge creedal monism through competing theological systems (Arianism, Nestorianism, Monophysitism), yet embedded in Christian institutional structures that constrain full exit. Significant extraction through anathematization and institutional suppression, but also genuine coordination benefit from shared metaphysical language and intellectual scaffolding. The school experiences the constraint as both limiting and enabling.
constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: POST-REFORMATION INSTITUTIONAL CHURCH (PITON) — By the early modern period, the creedal authority mechanism persists through institutional inertia despite reduced functional necessity. Doctrinal conformity is maintained through canon law and social convention rather than active suppression — the constraint is still enforced, but the theater has increased as the threat of heresy has receded in educated circles. The institution maintains the creedal gate as a vestigial marker of orthodoxy.
constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, metaphysical uniformity might appear to be an inherent requirement of coherent theology: a faith tradition cannot hold contradictory claims about divine nature without logical dissolution. This perspective risks naturalizing the creedal constraint as a necessary law of theological coherence. However, the structural data (clear beneficiary/victim distinction, active enforcement mechanisms, suppression rates) reveals this as a false summit: doctrinal uniformity is a contingent institutional arrangement, not a logical necessity.
constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__strict_orthodox_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nicene_creed_authority__strict_orthodox_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nicene_creed_authority__strict_orthodox_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nicene_creed_authority__strict_orthodox_reading, TR),
    TR >= 0.70.

:- end_tests(nicene_creed_authority__strict_orthodox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.62): High. The strict reading demands public affirmation of a specific metaphysical claim while suppressing alternative interpretations. The extraction is from victim groups' epistemic autonomy — they must ratify the creedal metaphysics even if their actual philosophical position differs. For lay interpreters, extraction is from interpretive authority; for philosophical schools, extraction is from institutional legitimacy and institutional resources. The extractiveness increased over the interval (0.45→0.62) as the enforcement apparatus (councils, imperial coercion, property confiscation) matured. Suppression (ψ=0.68): High. Multiple suppression mechanisms: anathematization removes social legitimacy; excommunication removes sacramental access; imperial legislation (Theodosius I onward) made heresy illegal; property confiscation removes economic viability; military campaigns (suppression of Arian and other churches) remove institutional substrate. The suppression is not total (heterodox churches persist), but barriers to exit and to open dissent are extremely high. Theater ratio (θ=0.55): Moderate-high. The creedal affirmation is partly functional (doctrinal clarity enables coordinated liturgical practice) and partly performative (the metaphysical claims cannot be empirically verified; confession of them is purely attestational). The ratio increased over the interval as theological discourse became more scholastic and less empirically testable. The strict reading's theater is lower than the piton's (which is mostly performative) because the early enforcement phase relied on genuine doctrinal enforcement, not ritual performance.
 *
 * PERSPECTIVAL GAP:
 *   The strict orthodox reading generates maximum perspectival divergence across the six perspectives. The powerless heterodox dissident sees pure extraction (Snare) with no exit and no benefit. The moderate lay interpreter sees extraction with constrained exit (also Snare, but slightly less severe). The hierarchical clergy see coordination benefit despite using the same enforcement machinery (Rope) — they experience creedal enforcement as legitimate ecclesial governance. The philosophical schools see mixed coordination and extraction (Tangled Rope) — they benefit from shared metaphysical language and organized inquiry but lose institutional legitimacy and resources. The post-Reformation church sees its own degraded ritual (Piton) — the enforcement persists through institutional inertia, but the material necessity has declined. The analytical observer risks seeing natural law (Mountain) — metaphysical uniformity as necessary for logical coherence — but the structural data (clear beneficiary/victim, active enforcement, suppression of alternatives) reveals false summit: the constraint is institutional choice, not logical necessity. The gap between powerless and institutional perspectives is stark: same mechanism, opposite experienced types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is computed from each agent's structural position relative to the extraction flow. Heterodox dissidents: powerless + trapped + victim status → d=0.95 → high f(d) → maximum experienced extractiveness. Lay interpreters: moderate power + constrained exit + victim status → d=0.75 → high f(d) → high extractiveness. Hierarchical clergy: institutional power + arbitrage exit + beneficiary status → d=0.10 → negative f(d) → negative experienced extraction (they experience it as coordination benefit). Philosophical schools: powerful + mobile + victim status → d=0.55 → moderate f(d) → moderate extractiveness. The engine's directionality derivation chain produces these d values automatically from the beneficiary/victim declarations and exit options. No manual override is needed; the structural relationships determine the d values, which determine the experienced extractiveness (χ), which determine the perspectival classification. The gap in d values (0.10 vs 0.95) is the gap in experienced types (Rope vs Snare).
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY MANDATROPHY: This constraint (strict_orthodox_reading) is part of the nicene_creed_authority family with two siblings. The strict reading presents the creed as a binding metaphysical commitment with high extraction and suppression. The symbolic reading interprets the creed as shared metaphorical language with lower extraction (ε ~0.25-0.35) and different beneficiary structure (shared interpretive community rather than hierarchical control). The liturgical reading treats the creed as embodied practice with ε ~0.20-0.30 and coordination-focused structure (rope-like). The mandatrophy is resolved by recognizing that the SAME KERNEL (the Nicene creed) instantiates different constraints depending on how it is read: as metaphysical claim (strict, high extraction), as edifying metaphor (symbolic, low extraction), or as liturgical habituation (liturgical, low extraction, coordination-focused). The strict reading's snare classification is not contradicted by the symbolic reading's rope classification — they are different readings of the same text, producing different structural constraints. The committer structure (which reading is correct?) is routed to omegas, not resolved in the constraint type. The engine's task is to model the constraint landscape accurately: IF strict reading is adopted, THEN snare with high extraction. IF symbolic reading is adopted, THEN rope with low extraction. The coexistence of these readings in the same tradition is the mandatrophy — the same text binds different communities to different structural constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_necessity_vs_institutional_choice,
    'Is the metaphysical uniformity demanded by the creed a logical necessity for coherent Christianity, or a contingent institutional choice for organizational consolidation?',
    'Historical comparative analysis: do non-Nicene Christian communities (Arian churches, Nestorian churches, Oriental Orthodoxy) remain logically coherent and internally stable? If yes, uniformity is institutional choice; if they fragment due to metaphysical contradiction, it suggests necessity.',
    'If institutional choice: mountain classification is false summit (extractive constraint naturalizing as natural law). If necessity: some perspectives correctly perceive immutability; extraction is unavoidable cost of doctrinal stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_necessity_vs_institutional_choice, conceptual, 'Whether doctrinal uniformity is logically necessary or institutionally contingent').

omega_variable(
    heresy_sanction_efficacy,
    'Do heresy sanctions (anathematization, excommunication, institutional exile) actually suppress heterodox belief, or do they merely suppress public expression while internalized dissent persists?',
    'Textual analysis of heterodox communities pre- and post-sanction: do creedal confessions of the sanctioned continue to express original positions covertly? Do heterodox schools reconstitute outside institutional structure? Do secret or semi-public dissenting communities maintain continuity?',
    'If sanctions suppress belief: suppression metric is accurate. If sanctions only suppress expression: true suppression is lower; constraint is less snare-like (more tangled_rope). The constraint might extract performative compliance while failing to achieve actual orthodoxy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heresy_sanction_efficacy, empirical, 'Whether heresy sanctions suppress belief or merely expression').

omega_variable(
    creedal_reading_as_readings,
    'Can the creedal language (e.g., ''of one substance with the Father'') support multiple logically coherent interpretations, making the ''strict orthodoxy'' reading one reading among valid alternatives rather than the uniquely correct reading?',
    'Detailed semantic and logical analysis: map the set of coherent metaphysical interpretations consistent with creedal language. If cardinality > 1, the strict reading is not uniquely determined; if cardinality = 1, strict orthodoxy reflects the only logically coherent reading.',
    'If multiple readings are valid: the creed does not uniquely determine metaphysical content; the strict reading''s claim to exclusive orthodoxy is a power move, not a logical deduction. This transforms the constraint from ''enforcing necessary doctrinal truth'' to ''suppressing valid alternative readings.'' Reclassifies from snare (coerced compliance) to extraction-plus-coordination hybrid (tangled_rope with high asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creedal_reading_as_readings, conceptual, 'Whether the creed uniquely determines metaphysical content or permits multiple coherent readings').

omega_variable(
    lay_vs_clerical_hermeneutics,
    'Is the gap between lay theological interpretation and hierarchical clergy interpretation driven by genuine epistemic asymmetry (clergy are trained, lay are not), or by institutional gatekeeping (clergy maintain authority by restricting lay access to interpretive authority)?',
    'Comparative hermeneutical analysis: do lay theological schools, where they have existed (Waldensians, some Reformation communities), produce coherent alternative readings? Are lay interpretations rejected on logical grounds or on grounds of unauthorized interpretation? Historical evidence of institutional suppression of lay hermeneutics without logical refutation.',
    'If epistemic asymmetry: the hierarchical constraint reflects legitimate expertise division; extraction is lower. If institutional gatekeeping: the constraint suppresses equally valid lay interpretation; extraction is higher, confirming snare classification and strong victimization of lay interpreters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lay_vs_clerical_hermeneutics, empirical, 'Whether hermeneutical hierarchy reflects expertise or institutional gatekeeping').

omega_variable(
    reading_identity_as_kernel_reading,
    'This constraint is ONE READING (strict_orthodox_reading) of the Nicene creed kernel. Do the sibling readings (symbolic_confessional_reading, liturgical_habituation_reading) represent genuinely distinct structural constraints with different ε values, or are they alternative interpretations of the same constraint?',
    'Constraint decomposition check: does the symbolic reading (interpreting creedal language as edifying metaphor rather than metaphysical claim) produce different extractiveness? Different beneficiary/victim structure? If the symbolic reading dissolves the enforcement mechanism entirely, it is a different constraint (ε much lower). If it preserves the enforcement mechanism, it is the same constraint under different hermeneutical cover.',
    'If different constraints: this file (strict reading) is one of three constraint stories, all linked via network.affects_constraints. The symbolic reading and liturgical reading are separate stories with their own ε values. If same constraint: the strict reading is not a kernel reading; remove cs_structure and omegas relating to reading relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_as_kernel_reading, conceptual, 'Whether sibling readings are distinct constraints or interpretations of the same constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__strict_orthodox_reading, 0, 600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nicene_strict_theater_t0, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nicene_strict_theater_t300, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 300, 0.48).
narrative_ontology:measurement(nicene_strict_theater_t600, nicene_creed_authority__strict_orthodox_reading, theater_ratio, 600, 0.55).

% Extraction over time
narrative_ontology:measurement(nicene_strict_extract_t0, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(nicene_strict_extract_t300, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 300, 0.62).
narrative_ontology:measurement(nicene_strict_extract_t600, nicene_creed_authority__strict_orthodox_reading, base_extractiveness, 600, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(nicene_strict_suppress_t0, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(nicene_strict_suppress_t300, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 300, 0.68).
narrative_ontology:measurement(nicene_strict_suppress_t600, nicene_creed_authority__strict_orthodox_reading, suppression_requirement, 600, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__strict_orthodox_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__symbolic_confessional_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__strict_orthodox_reading, nicene_creed_authority__liturgical_habituation_reading).

% DUAL FORMULATION NOTE:
% The Nicene creed kernel admits three structurally distinct readings, each producing its own constraint story. The strict reading (this file, ε=0.62, Snare) treats the creed as metaphysical commitment with enforcement. The symbolic reading (sibling, ε~0.28, Rope) treats it as shared metaphorical language. The liturgical reading (sibling, ε~0.22, Rope) treats it as embodied practice. These are not three views of one constraint; they are three different constraints instantiated by the same text under different hermeneutical frameworks. All three are live readings in the Christian tradition, held by different communities. The extractiveness delta (0.62 vs 0.28 vs 0.22) reflects not measurement ambiguity but genuine structural difference: enforcement-heavy metaphysical constraint vs. interpretive-community coordination vs. practice-based habituation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
