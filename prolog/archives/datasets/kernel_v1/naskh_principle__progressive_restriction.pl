% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh as Progressive Divine Pedagogy (Restrictive Reading)
 *   domain: islamic_jurisprudence/quranic_hermeneutics
 *
 * SUMMARY:
 *   The naskh principle — how later Quranic rulings relate to earlier ones —
 *   is a foundational problem in Islamic jurisprudence, and the solution
 *   adopted shapes the entire structure of Islamic law. The progressive
 *   restriction reading claims that revelation moved systematically from
 *   permissive to restrictive rulings, representing God's pedagogical
 *   approach: earlier verses accommodated social conditions while later
 *   verses represent final divine intent. This is distinct from the classical
 *   abrogation reading (earlier ruling is nullified/deleted) and the
 *   contextual harmonization reading (both verses remain binding in their
 *   respective contexts). This constraint story instantiates ONLY the
 *   progressive restriction reading as a single coherent ε-invariant
 *   framework. The reading exhibits tangled_rope structure: it serves a
 *   genuine coordination function (providing a systematic method to reconcile
 *   apparent contradictions in the Quranic text) while simultaneously
 *   extracting authority through the interpretive power to determine which
 *   verses are 'earlier/final.' The theater_ratio rises over time (from 0.52
 *   to 0.64) as the framework's explanatory burden increases — modernist
 *   scholars increasingly invoke progressive restriction to justify
 *   restricting pre-modern rulings, stretching the original pedagogical claim
 *   beyond its textual foundations. The framework extracts from literalist
 *   communities and permissive-reading practitioners while benefiting
 *   institutional authorities who control the classification of verses as
 *   'transitional.'
 *
 * KEY AGENTS:
 *   - Evolutionary Legal Scholars: Primary beneficiary (institutional/arbitrage) — gains authority to classify verses as transitional; can deploy framework flexibly across contexts
 *   - Interpretive Authority Structures: Secondary beneficiary (institutional/arbitrage) — traditional madhhabs and reform movements both benefit from the framework's provision of systematic reconciliation method
 *   - Literalist Permissive Reading Community: Primary victim (powerless/identity_locked) — texts they cite as permanent law are reclassified as superseded; cannot exit framework without abandoning literalist epistemology
 *   - Practicing Communities: Secondary victim (moderate/constrained) — face high retraining and social costs to adopting restrictive interpretation of formerly permissive rulings; receive some coordination benefit in return
 *   - Modernist Reform Coalition: Organized actor (organized/mobile) — deploys progressive restriction as temporary methodological tool to restrict archaic rulings while maintaining Quranic authority
 *   - Classical Scholarly Establishment: Institutional actor (institutional/arbitrage) — maintains framework through institutional momentum despite declining explanatory power relative to historical-critical alternatives
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent interpretive framework as unchangeable law of revelation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.52).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.58).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.52).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh as Progressive Divine Pedagogy (Restrictive Reading)").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "islamic_jurisprudence/quranic_hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, 'a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6').
narrative_ontology:cs_kernel_codification('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', fixed_text).
narrative_ontology:cs_authority_grounding('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', lineage).
narrative_ontology:cs_interpretation_layer_present('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6').
narrative_ontology:cs_reading_relation('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', foundational, revelation_pedagogical_trajectory).
narrative_ontology:cs_axiom_status(revelation_pedagogical_trajectory, holdable).
narrative_ontology:cs_axiom_grounding('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', revelation_pedagogical_trajectory, theological).
narrative_ontology:cs_axiom('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', foundational, earlier_verses_transitional_not_repealed).
narrative_ontology:cs_axiom_status(earlier_verses_transitional_not_repealed, holdable).
narrative_ontology:cs_axiom_grounding('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', earlier_verses_transitional_not_repealed, theological).
narrative_ontology:cs_reference_frame('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', revelation_as_pedagogical_progression).
narrative_ontology:cs_drift_state('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', contemporary_modernist_application, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a53b6718-f0e9-4bf0-b9aa-b5ad1623b6d6', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, interpretive_authority_structures).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, literalist_permissive_reading_tradition).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, practitioners_citing_earlier_verses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITERALIST PERMISSIVE READING COMMUNITY (SNARE) — Identity-fused with textual literalism; cannot exit without abandoning foundational epistemology. Bears extraction in the form of marginalization: earlier permissive verses become classified as 'transitional' and 'superseded,' stripping their authority. No alternative interpretive framework available that maintains both literalism and community standing.
constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: PRACTICING COMMUNITIES (TANGLED ROPE) — Face high costs to adopting progressive restriction framework (retraining, community rejection, doctrinal uncertainty) but also benefit from the clarity structure provides: knowing which verses are 'final intent' simplifies jurisprudence. Coordination function genuine (establishing hierarchy of verses reduces interpretive ambiguity) but asymmetrically applied — those whose practice aligns with restrictive rulings gain more benefit than those practicing permissive traditions.
constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERPRETIVE AUTHORITY STRUCTURES (ROPE) — Institutional beneficiary with high exit capacity (can shift between interpretive frameworks as context demands). Progressive restriction principle provides authority to these structures: the claim that 'this verse is earlier/transitional therefore superseded' grants interpretive power to those who can make the judgment. Benefits from coordination function (provides systematic method for reconciling apparent contradictions) without bearing suppression burden.
constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERNIST REFORM COALITION (SCAFFOLD) — Organized agents (reformist scholars, secular-influenced jurists) see progressive restriction as a temporary methodological tool: it enables them to restrict archaic rulings while maintaining Quranic authority. The framework has a sunset: as social context modernizes, the need for this pedagogical cover story diminishes — direct appeal to changed social conditions becomes more tenable. Classification reflects that the constraint's extraction mechanism decays as the underlying social tension (pre-modern vs modern law) resolves.
constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL SCHOLARLY ESTABLISHMENT (PITON) — Maintains progressive restriction framework through institutional momentum despite declining functional explanatory power. The original coordination function (resolving internal Quranic contradiction between permissive and restrictive verses) has partially atrophied as textual criticism and historical-critical methods offer alternative explanations. The framework persists because switching costs are high and because it remains embedded in madhhab jurisprudence, not because it uniquely solves the problem it was designed for.
constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABILITY VIEW (MOUNTAIN) — From a universal/civilizational perspective, the progression from permissive to restrictive in a revealed text is seen as an unchangeable fact of revelation history — God's pedagogical method IS progressive restriction; this is not a contingent interpretive framework but an immutable feature of how revelation operates. The engine will flag this as a false summit: the structural data reveals a contested interpretive principle, not a natural law.
constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(naskh_principle__progressive_restriction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(naskh_principle__progressive_restriction, TR),
    TR >= 0.70.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The framework concentrates interpretive authority in those who determine verse classification, independent of framework's hermeneutical soundness. The measuring point is institutional: who gains power to say 'this verse is earlier therefore transitional'? Earlier answer (classical scholars) was distributed across the madhhab system; modernist application concentrates it in reform authorities. The 0.52 reflects genuine coordination function (reconciling apparent contradiction) alongside measurable institutional extraction (authority concentration). Suppression (0.58): Moderate-high. The framework suppresses alternative readings: literalist permissive interpretation becomes impossible to maintain without being classified as hermeneutically naive or contextually blind. Marginalization is real but not total — some traditional schools still maintain permissive readings, though with reduced institutional authority. Theater ratio (0.64): Moderate-high. The framework requires performative work: scholars must narrate the 'pedagogical arc' from permissive to restrictive rulings, which involves assumption-heavy historical reconstruction and theological rationalization. The theater increases over time as the framework is deployed to justify increasingly aggressive restrictions on pre-modern rulings — the original pedagogical claim strains under the weight of modern application.
 *
 * PERSPECTIVAL GAP:
 *   The literalist community and institutional authorities experience this constraint in opposite directions. The literalist sees a snare: their foundational epistemology is under attack and they have no exit that preserves community standing. The institutional authority sees a rope: the framework provides coordination function (resolving textual contradiction) while giving them interpretive power. The practicing communities see tangled rope: real coordination benefit in knowing which verses are 'final,' but at the cost of abandoning practices based on earlier verses. The modernist coalition sees it as temporary scaffolding — a way to restrict pre-modern law while maintaining Quranic authority, with an expectation that as social modernization accelerates, direct appeal to changed circumstances will replace the pedagogical cover story. The traditional establishment sees it as a piton: the framework persists through institutional inertia (embedded in madhhab jurisprudence) despite historical-critical methods now providing alternative explanations for textual variation. The analytical observer risks the mountain misclassification: treating revelation-pedagogical-progression as an immutable law rather than a contested interpretive framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural relationship to interpretive authority and to the rulings the framework produces. Institutional authorities (interpreters, madhhabs) with arbitrage exit capacity experience low directionality (d ≈ 0.15–0.25) — they benefit from the framework and can shift between interpretive positions. Literalist communities with identity_locked exit experience high directionality (d ≈ 0.89) — their entire epistemological framework is under attack. Practicing communities with constrained exit experience moderate directionality (d ≈ 0.55–0.65) — they can technically shift interpretive traditions but at high cost. The powerless literalists experience maximum effective extraction (f(d) ≈ 1.28 for d=0.89) because they cannot arbitrage away from the framework. The institutional beneficiaries experience negative effective extraction (f(d) ≈ -0.01 for d=0.15 with arbitrage exit) because the framework subsidizes their position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY AVOIDANCE: This constraint is properly typed as tangled_rope, not as snare or rope. The snare misclassification would occur if we treated the framework as pure extraction (authority concentration) without acknowledging the genuine coordination function it provides — reconciling apparent Quranic contradictions. The rope misclassification would occur if we focused only on the coordination benefit (reconciliation method) and ignored the asymmetric authority concentration. The tangled_rope classification holds both: the framework solves a real coordination problem (opposing verses) AND it extracts through authority concentration (determining which verses are 'final'). The measurement trajectory (rising extractiveness and theater_ratio) shows the framework's suppressive and performative content accumulating as it is deployed to justify increasingly aggressive restrictions on pre-modern rulings — the original pedagogical claim strains under modernist application. Modernist scholars increasingly invoke 'progressive restriction' to justify abandoning entire categories of pre-modern rulings, stretching the framework beyond its hermeneutical foundations and raising its theater requirement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_contradiction_resolution_necessity,
    'Do earlier permissive verses actually contradict later restrictive verses, or can the texts be read as addressing different situations without invoking abrogation/restriction?',
    'Detailed contextual analysis of each permissive/restrictive verse pair; examination of whether the verses address distinct legal scenarios or the same ruling across time',
    'If no genuine contradiction exists: progressive restriction framework becomes optional methodological choice rather than necessary epistemic framework. Classification shifts toward rope (pure coordination method) rather than tangled_rope (asymmetric extraction hiding behind coordination). If contradiction is real: framework necessity is confirmed and the tangled_rope classification is structurally justified.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_contradiction_resolution_necessity, empirical, 'Whether earlier permissive and later restrictive verses genuinely contradict').

omega_variable(
    historical_context_accessibility,
    'Is the original historical context of revelation sufficiently recoverable to determine whether a verse was addressing a specific time-bound situation or laying down permanent law?',
    'Hadith analysis, early Quranic commentary (tafsir), sira literature establishing occasion-of-revelation (asbab al-nuzul); assessment of reconstruction reliability',
    'If historical context is reliably recoverable: progressive restriction becomes a precise hermeneutical tool grounded in evidence. If context is epistemically opaque: the framework becomes a hermeneutical assumption projected backward onto text. Affects whether this reading represents empirical knowledge (mountain candidate) or interpretive interpretation choice (rope/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_context_accessibility, empirical, 'Epistemic accessibility of original revelation context').

omega_variable(
    pedagogical_coherence_assumption,
    'Does the claim that ''restriction represents divine pedagogy'' require treating God''s legislative method as fundamentally pedagogical, or is this a reading projected onto the text?',
    'Analysis of whether pedagogical framing appears in classical sources as indigenous to the tradition or emerges as modernist reinterpretation; examination of how pre-modern scholars framed naskh',
    'If pedagogical framing is textually grounded: reading is more defensible as scriptural. If it is interpretive projection: reading becomes a modern theodicy (justifying apparent textual inconsistency through pedagogical rationalization). Affects confidence in the progressive_restriction reading''s claim to authenticity versus innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_coherence_assumption, conceptual, 'Whether pedagogical framing is indigenous to the tradition or modern interpretive projection').

omega_variable(
    authority_structure_benefit_concentration,
    'Does the progressive restriction framework systematically concentrate interpretive authority in those who can determine which verses are ''earlier/final'', independent of the framework''s hermeneutical correctness?',
    'Institutional analysis: comparison of interpretive power distribution under progressive restriction framework versus alternative frameworks (classical abrogation, contextual harmonization); examination of who gains authority to classify verses as ''transitional''',
    'If concentrated: framework is extractive even if hermeneutically sound — the epistemic benefit (resolving contradiction) is secondary to the institutional benefit (concentrating authority). Confirms tangled_rope classification. If distributed: authority concentration is not a systemic feature and extraction is lower than measured.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_structure_benefit_concentration, empirical, 'Whether progressive restriction concentrates interpretive authority asymmetrically').

omega_variable(
    kernel_reading_contested,
    'This story instantiates ONE reading (progressive_restriction) of the contested kernel naskh_principle. Does the classical_abrogation reading genuinely foreclose this reading, or do both remain live positions in contemporary Islamic jurisprudence?',
    'Survey of contemporary Islamic scholarly positions; examination of whether major schools and reformist movements maintain both reading traditions or treat them as mutually exclusive',
    'If both are live: reading_relations should use ''coexists_with''. If classical reading has been formally superseded in modernist contexts: use ''influences''. If this reading''s axioms directly contradict classical reading''s axioms: use ''forecloses''. Current empirical assessment: both readings coexist across different scholarly communities, but progressive restriction reading influences (creates pressure toward) classical reading by changing the legitimacy conditions for citing classical authorities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contested, empirical, 'Relationship between progressive_restriction reading and sibling readings of naskh kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(naskh_prog_theater_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(naskh_prog_theater_t5, naskh_principle__progressive_restriction, theater_ratio, 5, 0.58).
narrative_ontology:measurement(naskh_prog_theater_t10, naskh_principle__progressive_restriction, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(naskh_prog_extract_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(naskh_prog_extract_t5, naskh_principle__progressive_restriction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(naskh_prog_extract_t10, naskh_principle__progressive_restriction, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(naskh_prog_suppress_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(naskh_prog_suppress_t5, naskh_principle__progressive_restriction, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(naskh_prog_suppress_t10, naskh_principle__progressive_restriction, suppression_requirement, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, ijma_consensus_framework).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, maslaha_public_interest_doctrine).

% DUAL FORMULATION NOTE:
% The naskh_principle is a kernel with multiple readings, each instantiated as a separate constraint with different ε values and different cs_structure axioms. This story (progressive_restriction, ε=0.52, tangled_rope) is linked to its sibling readings (classical_abrogation, contextual_harmonization) through network.affects_constraints. Each reading has distinct structural properties: classical_abrogation (ε lower, less institutional extraction, more textual clarity); contextual_harmonization (ε higher, more coordination complexity); progressive_restriction (moderate ε, moderate extraction, pedagogical authority concentration). Do not merge the readings into a single story — the divergent ε values indicate structurally distinct constraints operating within a shared textual domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(naskh_principle__progressive_restriction, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
