% ============================================================================
% CONSTRAINT STORY: domain_partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_domain_partition_reading, []).

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
 *   constraint_id: domain_partition_reading
 *   human_readable: Kami-Buddha Domain Partition (Separation Reading)
 *   domain: religious_studies/comparative_religion/japanese_history
 *
 * SUMMARY:
 *   The domain partition reading claims that kami and buddhas are
 *   structurally separate entities serving functionally distinct domains:
 *   kami preside over life, fertility, purity, and immediate intervention in
 *   the world; buddhas preside over death, impurity, karmic consequence, and
 *   transcendental liberation. This is one reading of a contested kernel —
 *   the kami-buddha ontology — that has been central to Japanese religious
 *   thought for over 1200 years. The partition reading treats the separation
 *   as a cosmic and functional necessity rather than a contingent historical
 *   accommodation. This reading instantiates a specific institutional
 *   arrangement in which Buddhist and Shinto authorities maintain separate
 *   jurisdictions, collect separate revenues, and control separate ritual
 *   specialists. The separation both enables institutional coordination
 *   (clear domains prevent jurisdictional conflicts) and enforces extraction
 *   (devotees must support dual observance systems). From the institutional
 *   perspective, the partition is pure coordination. From the devotee
 *   perspective seeking unified meaning, it is a snare.
 *
 * KEY AGENTS:
 *   - Buddhist Institutional Authority: Primary beneficiary (institutional/arbitrage) — controls death, impurity domains; captures karmic causation explanations; presides over temple economy
 *   - Shinto Ritualist Communities: Primary beneficiary (institutional/arbitrage) — controls life, fertility, purification domains; maintains shrine prestige; independent from Buddhist doctrinal control
 *   - Devotees Seeking Unified Meaning: Primary victim (powerless/trapped) — cannot exit dual observance obligations; experience contradictory cosmologies as givens; bear full cost of maintaining both systems
 *   - Village Practice Communities: Secondary victim (moderate/constrained) — inherit both systems; benefit from ritual clarity but face resource/prestige asymmetry between domains
 *   - Meiji Modernizers: Organized challengers (organized/mobile) — reimagine partition as contingent institutional arrangement subject to rationalization; perceive sunset via state modernization
 *   - Institutional Ritual System: Piton maintainer (institutional/arbitrage) — sustains performative separation despite cosmological erosion; operates through inertia rather than living justification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(domain_partition_reading, 0.35).
domain_priors:suppression_score(domain_partition_reading, 0.45).
domain_priors:theater_ratio(domain_partition_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(domain_partition_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(domain_partition_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(domain_partition_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(domain_partition_reading, tangled_rope).
narrative_ontology:human_readable(domain_partition_reading, "Kami-Buddha Domain Partition (Separation Reading)").
narrative_ontology:topic_domain(domain_partition_reading, "religious_studies/comparative_religion/japanese_history").

domain_priors:requires_active_enforcement(domain_partition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(domain_partition_reading, fixed_text).
narrative_ontology:cs_authority_grounding(domain_partition_reading, lineage).
narrative_ontology:cs_interpretation_layer_present(domain_partition_reading).
narrative_ontology:cs_kernel_id(domain_partition_reading, kami_buddha_ontology).
narrative_ontology:cs_reading_relation(domain_partition_reading, unified_manifestation_reading, forecloses).
narrative_ontology:cs_reading_relation(domain_partition_reading, pragmatic_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom(domain_partition_reading, foundational, purity_impurity_ontological_incommensurability).
narrative_ontology:cs_axiom_status(purity_impurity_ontological_incommensurability, holdable).
narrative_ontology:cs_axiom_grounding(domain_partition_reading, purity_impurity_ontological_incommensurability, deontological).
narrative_ontology:cs_axiom(domain_partition_reading, secondary, functional_complementarity_necessity).
narrative_ontology:cs_axiom_status(functional_complementarity_necessity, holdable).
narrative_ontology:cs_axiom_grounding(domain_partition_reading, functional_complementarity_necessity, instrumental).
narrative_ontology:cs_reference_frame(domain_partition_reading, cosmic_domain_separation).
narrative_ontology:cs_drift_state(domain_partition_reading, contemporary_era, gap(axiom_overriding, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(domain_partition_reading, buddhist_institutional_authority).
narrative_ontology:constraint_beneficiary(domain_partition_reading, shinto_ritualist_practitioners).
narrative_ontology:constraint_victim(domain_partition_reading, unified_cosmic_understanding).
narrative_ontology:constraint_victim(domain_partition_reading, devotees_seeking_integrated_meaning).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVOTEE (SNARE) — Trapped between two institutional domains with no coherent bridge. Cannot exit either ritual obligation. Experiences maximum extraction: must perform both Buddhist and Shinto rites despite the theological gap, paying dual observance costs with no unified meaning framework. Suppression is high — the separation is naturalized as cosmic necessity rather than institutional preference.
constraint_indexing:constraint_classification(domain_partition_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDDHIST AUTHORITY (ROPE) — Experiences the partition as pure coordination. Clear domains eliminate jurisdictional conflict. Buddha presides over death, impurity, and karmic causation; kami handle life, purity, and immediate intervention. The separation enables resource concentration and legitimacy consolidation. No victim relationship — institutional beneficiary from functional clarity.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SHINTO RITUALISTS (ROPE) — Also experiences partition as coordination benefit. Clear separation protects Shinto's agricultural and life-cycle functions from Buddhist reinterpretation. Domain partition legitimizes Shinto's independent ceremonial authority. Institutional beneficiary with arbitrage options — can relocate prestige between domains as needed.
constraint_indexing:constraint_classification(domain_partition_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: VILLAGE COMMUNITIES (TANGLED ROPE) — Constrained by inheritance and geographic embeddedness. Experience genuine coordination benefit from the partition (distinct ritual specialists, clear calendars, institutional support). But also experience asymmetric extraction: must maintain both systems' observances while receiving unequal status/prestige between domains. Village ritual masters are moderately paid; Buddhist priests capture larger institutional resources.
constraint_indexing:constraint_classification(domain_partition_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEIJI REFORMERS (SCAFFOLD) — Organized challenge to the partition itself. The separation is reimagined as temporary institutional configuration subject to rational reorganization. From the Meiji perspective, the domain partition is a sunset clause awaiting modernization policy. High suppression of alternative frameworks (syncretism, unified cosmology) during the campaign period. Effective extraction is low because the organized agents have agency and perceive exit via institutional redesign.
constraint_indexing:constraint_classification(domain_partition_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INSTITUTIONAL RITUAL SYSTEM (PITON) — The domain partition persists through performative maintenance despite erosion of the underlying ontological justification. Modern practitioners often hold unified or syncretic beliefs privately while performing dual-system rituals publicly. Theater ratio (0.55) reflects the increasing gap between the metaphysical partition claim and actual Japanese religious practice. The system survives through inertia — changing it would require renegotiation of centuries of institutional turf, even though the cosmological justification has weakened.
constraint_indexing:constraint_classification(domain_partition_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal perspective, some domains must be separated because purity and impurity are ontologically incommensurable. A cosmic principle requires that life-giving forces (kami) cannot coexist in the same ritual space as death-dealing forces (Buddhist karmic consequence). This perspective risks naturalizing what is actually a contingent institutional arrangement — the false summit detector may flag this as constructed separation presented as natural law.
constraint_indexing:constraint_classification(domain_partition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(domain_partition_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(domain_partition_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(domain_partition_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(domain_partition_reading, TR),
    TR >= 0.70.

:- end_tests(domain_partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The partition benefits institutional authorities (Buddhist and Shinto) by consolidating power and preventing doctrinal conflict. Devotees bear costs: dual observance expenses, cognitive dissonance, and inability to develop unified cosmology. But the constraint is not maximally extractive because genuine coordination benefits exist — clear domains enable efficient ritual practice and support for ritual specialists. The extracted value is not pure surplus (snare) but mixed with legitimate coordination costs. Measurement trajectory shows slow increase over the interval (0.25→0.35), reflecting gradual erosion of ontological justification while institutional structures persist. Suppression (0.45): Moderate. Significant barriers to exiting the partition include institutional legitimacy claims, centuries of separate development, independent funding, and trained specialists. But suppression is not absolute — contemporary syncretic practice and Meiji-era reformist challenges both demonstrated that alternatives were conceivable. Theater ratio (0.55): Moderate. The domain partition is increasingly performative: the ontological claim (purity/impurity incommensurability) has weakened in modern consciousness, but the institutional separation persists through ritual maintenance and historical continuity. Modern practitioners often hold syncretic or unified beliefs privately while performing separated rituals publicly.
 *
 * PERSPECTIVAL GAP:
 *   The domain partition reading exhibits a stark perspectival gap between institutional and devotee views. Buddhist and Shinto authorities both experience the partition as beneficial coordination (Rope) — it prevents doctrinal contamination and enables resource concentration. Organized modernizers experience it as a temporary institutional arrangement subject to rational redesign (Scaffold). But devotees seeking unified cosmic meaning experience the partition as an inescapable constraint forcing dual observance (Snare). The analytical observer risks naturalizing this contingent arrangement as an immutable cosmic law (Mountain) — claiming that purity and impurity are ontologically incommensurable. The institutional ritual system has degraded into performative maintenance (Piton) as the underlying cosmological justification has eroded. This gap reveals that the domain partition functions simultaneously as a coordination mechanism for institutions and an extraction mechanism for devotees.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist authority occupies the beneficiary-plus-arbitrage position (d ≈ 0.05): they benefit from clear domains while maintaining options to reframe or expand their authority. Shinto ritualists occupy similar beneficiary-plus-arbitrage position (d ≈ 0.08): independent prestige, resource capture, and exit options via folk practice flexibility. Village communities occupy the mixed-position (d ≈ 0.55): constrained by inheritance but benefiting from coordination, experiencing unequal prestige distribution. Devotees occupy the victim-plus-trapped position (d ≈ 0.92): unable to exit, bearing full cost of dual systems, unable to develop integrated meaning framework. The piton perspective applies an institutional actor with arbitrage options (d ≈ 0.12) — the ritual system itself, which maintains performative separation despite cosmological weakness. The analytical observer position (d ≈ 0.72) is that of an external analyst with awareness of the structure's contingency.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the domain partition reading is legitimate but perspectival. From institutional authorities, it is pure coordination (Rope) — the separation solves jurisdictional conflicts and enables specialization. From devotees, it is extraction (Snare) — costs are imposed without consent, and alternatives are suppressed. From modernizers, it is a temporary problem with a redesign pathway (Scaffold). From the institutional ritual system, it is theatrical maintenance (Piton) — the cosmological justification has eroded but the structure persists. The false summit risk exists when the natural law perspective (cosmic incommensurability of purity/impurity) naturalizes what is actually an institutional arrangement with identifiable beneficiaries. The constraint's mandatrophy is resolved by recognizing that all readings are structurally defensible from their respective positions — the question is not which type is correct, but which reading the evidence best supports.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_ontological_status,
    'Is the kami-buddha separation an ontological claim about distinct cosmic principles or an institutional claim about administrative domains?',
    'Textual analysis of foundational justifications (medieval Buddhist texts vs Shinto mythology); examination of what violations would falsify the principle (can kami be evil? Can buddhas purify?); cross-cultural comparison with other dual-system ontologies',
    'If ontological: supports mountain and domain_partition reading. If institutional: entire constraint reclassifies toward tangled_rope/snare — the separation is extraction mechanism, not natural law. If mixed (ontology legitimates institution): supports false-summit analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_ontological_status, conceptual, 'Whether the separation is an ontological or institutional claim').

omega_variable(
    historical_contingency_evidence,
    'What historical evidence supports that the kami-buddha partition emerged from deliberate institutional separation rather than pre-existing cosmic distinction?',
    'Archaeological evidence of earlier syncretic practice; manuscript dating of separation rhetoric; institutional incentive analysis (when do Buddhist and Shinto authorities begin claiming incompatibility?); comparison with pre-Heian sources',
    'If strong contingency evidence: domain_partition reading appears as deliberate institutional strategy, reclassifying to tangled_rope or snare depending on victim experience. If weak contingency evidence: partition may reflect deep cosmological intuition — supports mountain/rope readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_evidence, empirical, 'Historical evidence for institutional origins of the partition').

omega_variable(
    contemporary_syncretism_suppression,
    'How much contemporary Japanese religious practice is actually syncretic (unified framework) versus how much is performatively separated?',
    'Survey data on Japanese religious identity and practice; analysis of private belief vs public ritual performance; examination of contemporary shrine-temple cooperative arrangements; ethnographic study of self-described syncretic practitioners',
    'High syncretism with suppressed alternatives: partition operates as snare on devotees seeking unified meaning. Low syncretism: partition reflects enduring functional complementarity (rope/scaffold). Mixed pattern: supports tangled_rope and piton observations — performative separation maintained despite eroded cosmological justification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contemporary_syncretism_suppression, empirical, 'Prevalence of syncretic vs separated practice in contemporary Japan').

omega_variable(
    sibling_reading_committer_decomposition,
    'Which foundational commitments distinguish this domain_partition reading from the unified_manifestation reading and the pragmatic_accommodation reading?',
    'Explicit comparison of axiom declarations across all three readings; identification of which claims are holdable vs overridden vs foreclosed in each reading; analysis of whether any sibling reading directly contradicts the core premise of domain separation',
    'If unified_manifestation forecloses partition: the three readings cannot coexist in one framework. If pragmatic_accommodation coexists: multiple readings are live positions. If partition influences but does not foreclose: structural downstream pressure exists. Maps to reading_relations declarations in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_committer_decomposition, conceptual, 'Foundational differences between this and sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(domain_partition_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doma_tr_t0, domain_partition_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(doma_tr_t5, domain_partition_reading, theater_ratio, 5, 0.48).
narrative_ontology:measurement(doma_tr_t10, domain_partition_reading, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(doma_be_t0, domain_partition_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(doma_be_t5, domain_partition_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(doma_be_t10, domain_partition_reading, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(domain_partition_reading, identity_coordination).
narrative_ontology:affects_constraint(domain_partition_reading, kami_buddha_unified_manifestation_reading).
narrative_ontology:affects_constraint(domain_partition_reading, kami_buddha_pragmatic_accommodation_reading).

% DUAL FORMULATION NOTE:
% The kami-buddha ontology is a single contested kernel with three structurally distinct readings. Each reading produces its own constraint story with different ε values and perspectival classifications. This story instantiates the domain_partition reading only. The unified_manifestation_reading and pragmatic_accommodation_reading are separate constraints with their own ε, beneficiaries, victims, and measurement profiles. All three stories are linked via network.affects_constraints to indicate they are readings of the same kernel, not independent constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(domain_partition_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
