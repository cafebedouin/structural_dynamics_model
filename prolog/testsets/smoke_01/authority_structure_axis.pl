% ============================================================================
% CONSTRAINT STORY: authority_structure_axis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_authority_structure_axis, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: authority_structure_axis
 *   human_readable: Authority Structure Axis in Historical Linguistics
 *   domain: historical_linguistics/philology/commitment_systems
 *
 * SUMMARY:
 *   The authority structure axis in historical linguistics refers to the
 *   institutional and epistemological arrangements that determine whose
 *   methodologies, reconstructions, and linguistic classifications are
 *   considered authoritative within the field. The constraint centers on the
 *   Neogrammarian comparative method and its derivatives as the dominant
 *   framework for establishing legitimate linguistic knowledge. This
 *   structure exhibits characteristics of both coordination (establishing
 *   shared standards enabling cross-linguistic research) and extraction
 *   (gatekeeping which languages, language communities, and alternative
 *   methodologies receive epistemic legitimacy). The tension manifests across
 *   multiple dimensions: between established comparative tradition and
 *   undocumented language communities seeking agency in their own linguistic
 *   reconstruction; between junior scholars constrained by methodology
 *   gatekeeping and senior practitioners who benefit from consolidated
 *   authority; between the language documentation movement building
 *   alternative pathways and the traditional academy maintaining credential
 *   requirements. The extractiveness (0.38) reflects that the constraint
 *   provides real coordination benefits (fieldwork systematization,
 *   comparative frameworks) alongside genuine extraction of epistemic
 *   authority. The theater ratio (0.65) and its upward trajectory (0.38→0.65)
 *   indicate that pedagogical and professional gatekeeping functions have
 *   increasingly become performative rather than functional as computational
 *   and community-based alternatives have emerged.
 *
 * KEY AGENTS:
 *   - Undocumented Language Communities: Primary victims (powerless/trapped) — linguistic features evaluated against tradition's criteria; no exit mechanism from authority structure
 *   - Field Linguists and Junior Scholars: Secondary victims (moderate/constrained) — face gatekeeping barriers and publication bias toward tradition-confirming work; constrained but not trapped
 *   - Established Comparative Tradition: Primary beneficiaries (institutional/arbitrage) — consolidated methodological authority enables prestige publication, funding allocation, credential assignment
 *   - Language Documentation Movement: Organized agents (organized/mobile) — building alternative authority pathways (digital archives, community documentation, computational phylogenetics) with sunset logic
 *   - Neogrammarian Framework: Institutional actor (institutional/arbitrage) — maintains authority through teaching, publication norms, and credential requirements despite degraded epistemic function (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as epistemologically necessary scientific standards
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(authority_structure_axis, 0.38).
domain_priors:suppression_score(authority_structure_axis, 0.48).
domain_priors:theater_ratio(authority_structure_axis, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(authority_structure_axis, extractiveness, 0.38).
narrative_ontology:constraint_metric(authority_structure_axis, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(authority_structure_axis, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(authority_structure_axis, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(authority_structure_axis, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(authority_structure_axis, tangled_rope).
narrative_ontology:human_readable(authority_structure_axis, "Authority Structure Axis in Historical Linguistics").
narrative_ontology:topic_domain(authority_structure_axis, "historical_linguistics/philology/commitment_systems").

domain_priors:requires_active_enforcement(authority_structure_axis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(authority_structure_axis, 'b0948582-97a0-42e1-91fe-c02abd8b5290').
narrative_ontology:cs_created_at('b0948582-97a0-42e1-91fe-c02abd8b5290', '').
narrative_ontology:cs_kernel_codification('b0948582-97a0-42e1-91fe-c02abd8b5290', formalized).
narrative_ontology:cs_authority_grounding('b0948582-97a0-42e1-91fe-c02abd8b5290', extraction).
narrative_ontology:cs_interpretation_layer_present('b0948582-97a0-42e1-91fe-c02abd8b5290').
narrative_ontology:cs_reading_relation('b0948582-97a0-42e1-91fe-c02abd8b5290', authority_structure_pluralistic_reading, forecloses).
narrative_ontology:cs_reading_relation('b0948582-97a0-42e1-91fe-c02abd8b5290', authority_structure_community_based_reading, influences).
narrative_ontology:cs_axiom('b0948582-97a0-42e1-91fe-c02abd8b5290', foundational, methodology_universalism_required).
narrative_ontology:cs_axiom_status(methodology_universalism_required, holdable).
narrative_ontology:cs_axiom('b0948582-97a0-42e1-91fe-c02abd8b5290', foundational, expert_credentialing_necessary_for_validity).
narrative_ontology:cs_axiom_status(expert_credentialing_necessary_for_validity, overridden).
narrative_ontology:cs_axiom('b0948582-97a0-42e1-91fe-c02abd8b5290', secondary, comparative_method_empirically_superior).
narrative_ontology:cs_axiom_status(comparative_method_empirically_superior, overridden).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(authority_structure_axis, established_philological_tradition).
narrative_ontology:constraint_beneficiary(authority_structure_axis, senior_comparative_linguists).
narrative_ontology:constraint_victim(authority_structure_axis, language_documentation_completeness).
narrative_ontology:constraint_victim(authority_structure_axis, minority_language_communities).
narrative_ontology:constraint_victim(authority_structure_axis, alternative_reconstruction_methods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDOCUMENTED LANGUAGE COMMUNITY (SNARE) — Trapped within the authority structure that decides which languages merit documentation and which linguistic features are 'worth' reconstructing. No exit mechanism: their language's grammatical properties are evaluated against criteria established by the philological tradition they cannot challenge. Maximum suppression: alternative reconstruction methods from within the community are dismissed as non-scientific. Zero coordination benefit — the tradition extracts epistemic authority over the language while providing no reciprocal documentation capacity.
constraint_indexing:constraint_classification(authority_structure_axis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD LINGUIST / JUNIOR SCHOLAR (TANGLED ROPE) — Constrained by credential gatekeeping, publication bias toward confirming traditional reconstructions, and career risk of proposing alternative methodologies. Yet also coordinated within the constraint: fieldwork methods were systematized within the tradition, comparative frameworks enable meaningful cross-linguistic analysis, and mentorship pathways exist (albeit narrow). Extraction is real but not total — some agency in choosing research questions within established frameworks.
constraint_indexing:constraint_classification(authority_structure_axis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ESTABLISHED COMPARATIVE TRADITION (ROPE) — Benefits from the authority structure that centralizes methodological standards and coordinates linguistic research under a unified epistemology. The tradition experiences the constraint as pure coordination: arbitrage options are abundant — practitioners can move between research programs, publish in prestige venues, build institutional authority. The constraint serves the tradition's interests directly by consolidating epistemic power over what counts as valid reconstruction.
constraint_indexing:constraint_classification(authority_structure_axis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LANGUAGE DOCUMENTATION MOVEMENT (SCAFFOLD) — Organized coalition (Endangered Languages Project, Linguistic Society of America committees, open-access linguistic databases) sees the traditional authority structure as temporary. Digital documentation, automated phoneme recognition, crowdsourced transcription, and computational phylogenetics are building alternative pathways that bypass expert gatekeeping. Theater is declining as communities themselves become documentation agents. Sunset logic: within 20-30 years, machine-assisted documentation will enable minority communities to establish their own linguistic authority without credentialing from the comparative tradition.
constraint_indexing:constraint_classification(authority_structure_axis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: NEOGRAMMARIAN FRAMEWORK (PITON) — The Neogrammarian principle (sound change is regular and unconditioned by lexical/morphological factors) has degraded from a productive research hypothesis into institutional dogma maintained through inertia. Many linguists acknowledge counterexamples and context-dependent phonological processes, yet the framework persists as a certification requirement for academic credentials. High theater: the framework is taught, cited, and defended in professional contexts despite low functional utility. The piton classification reflects that the tradition maintains the framework because alternatives haven't fully replaced it institutionally, not because it produces superior reconstructions.
constraint_indexing:constraint_classification(authority_structure_axis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some authority structure is inherent to epistemology itself: any field of inquiry must establish shared methodological standards to produce reliable knowledge. The comparative tradition's criteria (regularity of sound change, reconstruction by the comparative method) are not arbitrary but reflect the logical requirements of inference from observable data. This perspective naturalizes the tradition as emergent from the structure of linguistic science itself. However, the presence of identifiable beneficiaries (established tradition) and clear institutional enforcement mechanisms indicates this is a false summit — the structural data reveals the authority structure as contingent and contestable, not immutable.
constraint_indexing:constraint_classification(authority_structure_axis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(authority_structure_axis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(authority_structure_axis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(authority_structure_axis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(authority_structure_axis, TR),
    TR >= 0.70.

:- end_tests(authority_structure_axis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The comparative tradition does provide genuine coordination benefits — fieldwork methods were systematized, comparative techniques enabled discovery of language families, and mentorship within the tradition is functional. However, the tradition also extracts epistemic authority by controlling which reconstructions are publishable, which methodologies are legitimate, and which language communities' self-descriptions are credible. The reduced extractiveness relative to a pure snare reflects that coordination benefits are real, not illusory. Suppression (0.48): Moderate. Barriers to exit the tradition's authority include: credentialing requirements (PhD training in established methods), publication bias (journals favor tradition-confirming articles), career risk (challenging foundations invites reputation damage), access barriers (minority language documentation requires institutional affiliation for funding), and epistemic closure (alternative methodologies dismissed as 'unscientific'). But suppression is not total — computational linguists, documentary linguists, and indigenous scholars have begun establishing alternative pathways. Theater ratio (0.65): Moderately high and rising. The Neogrammarian principle (regularity of sound change) has degraded from productive hypothesis to institutionalized dogma. Linguists teach it as foundational truth despite acknowledging frequent counterexamples (lexical conditioning, morphological analogy effects, frequency-based sound change). The framework persists through inertia — it structures textbooks, credential exams, and professional communication — rather than through empirical superiority. The 20-year upward trajectory (0.38→0.65) reflects that as computational alternatives have emerged, the traditional framework's theater has become more visible: the teaching function is increasingly divorced from actual reconstruction practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests a full perspectival gap across all six types, revealing how a single structural arrangement appears as immutable law, pure coordination, mixed extraction, temporary scaffold, degraded ritual, or arbitrary gatekeeping depending on the observer's position. The undocumented language community sees a snare: their linguistic features are evaluated by criteria they cannot contest, extraction occurs with no coordination benefit. The field linguist sees tangled rope: they benefit from the tradition's research infrastructure but are constrained by its gatekeeping. The established tradition sees pure rope: the constraint is the coordination mechanism that enables their research program. The documentation movement sees a scaffold: the authority structure is temporary and being displaced by digital and community-based alternatives. The Neogrammarian framework itself (piton perspective) is recognized by many practitioners as degraded — they acknowledge it's maintained by inertia, not empirical superiority. The civilizational analytical observer risks a mountain classification: from a 'long enough' perspective, some authority structure seems logically necessary to linguistics as an empirical science. But the structural data falsifies this: beneficiaries are identifiable (the established tradition), enforcement is active (credentialing, publication bias), and alternative methods are feasible (computational, community-based). The mountain perspective is a false summit — naturalization of what is actually a contestable institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The authority structure operates asymmetrically: established scholars benefit from the consolidated methodological authority (low d, arbitrage exit enables switching between programs without loss of status); undocumented communities bear costs of gatekeeping without exit (high d, trapped status). The established tradition's directionality derives from beneficiary status plus arbitrage: they control which research programs are fundable, which publications are prestigious, and which scholars are credentialed — all without dependence on the outcomes of specific reconstructions. Their exit options are abundant: a senior comparative linguist can move between universities, funding agencies, and journals while maintaining authority. Conversely, a minority language community seeking to document their own historical forms must navigate the tradition's institutional gateways — they cannot arbitrage to an alternative authority structure because the tradition is the recognized epistemic standard globally. This asymmetry is not accidental: it is the mechanism through which the tradition extracts epistemic authority. The field linguist's perspective reveals the constraint's mixed nature: they are coordinated within fieldwork standards and mentorship networks (genuine rope elements) while being extracted from by publication gatekeeping and credential requirements (genuine snare elements). Their constrained exit means they experience both simultaneously.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing genuine coordination function (fieldwork systematization, comparative frameworks enabling discovery of language families, mentorship structures) from asymmetric extraction (gatekeeping which methodologies are legitimate, controlling epistemic authority over minority language communities, controlling which research programs are fundable). The constraint is not pure extraction because the established tradition's frameworks genuinely enable research that would not occur without them. But it is not pure coordination because the benefits accrue asymmetrically, and enforcement maintains the asymmetry against emerging alternatives. The tangled rope classification holds: both genuine coordination and asymmetric extraction are structural, and the constraint requires active enforcement (through credentialing, publication gatekeeping, and institutional pressure) to maintain the extraction component. The piton perspective reveals that the enforcement theater has increased as the functional necessity has declined: the Neogrammarian framework is taught and defended less because it produces superior reconstructions and more because it maintains the tradition's institutional authority. The scaffold perspective shows a real sunset: as computational phylogenetics, digital documentation, and community-based reconstruction mature, the traditional authority structure's extraction mechanism loses force — alternative pathways bypass the gatekeeping entirely. The false summit (mountain perspective) is correctly diagnosed: the civilizational view risks naturalizing the authority structure as epistemologically necessary, but the structural data (identifiable beneficiaries, active enforcement, feasible alternatives) indicates it is contingent and contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methodological_universalism_vs_pluralism,
    'Is a single authority structure for evaluating linguistic reconstructions necessary for epistemic reliability, or does pluralistic coexistence of multiple methodologies produce superior science?',
    'Comparative analysis of reconstruction accuracy and robustness: single-method programs vs multi-method programs over 50-year horizon; measurement of how often single-method and multi-method reconstructions diverge, and which proves more stable under new data',
    'If universalism required: authority structure is closer to mountain (structural necessity). If pluralism produces superior results: authority structure is extraction mechanism (Snare/Tangled Rope from more perspectives). Determines whether alternatives to Neogrammarian framework can coexist or must replace it entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_universalism_vs_pluralism, empirical, 'Whether single methodology or multiple methodologies produce more reliable linguistic reconstructions').

omega_variable(
    community_epistemic_authority,
    'Can native speaker communities establish reliable linguistic reconstructions of their own historical forms without external credentialing from the comparative tradition?',
    'Projects where indigenous communities reconstruct historical phonology, morphology, or lexicon using their own methodologies and oral traditions; comparison with tradition-based reconstructions; measurement of internal consistency and predictive power',
    'If communities can establish authority: traditional gatekeeper role becomes optional (Scaffold sunset confirmed). If external validation is necessary: authority structure is functional necessity, not extraction (closer to Rope/Mountain). Determines whether documentation movement can achieve full independence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(community_epistemic_authority, empirical, 'Whether community-based reconstruction can match tradition-based reconstruction').

omega_variable(
    computational_replacement_feasibility,
    'Can computational phylogenetics and machine learning models reconstruct historical linguistic states at comparable accuracy and transparency to manual comparative reconstruction?',
    'Benchmark studies: computational models vs manual reconstruction on known language families with well-established comparative standards; measurement of reconstruction error rates, consistency, and interpretability; assessment of whether computational methods require external credentialing or are self-validating',
    'If computational methods equal or exceed manual: authority structure loses epistemic justification (scaffold sunset accelerates). If computational methods remain inferior or inscrutable: authority structure retains functional role (closer to Rope). Determines whether piton degradation can accelerate toward full institutional replacement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(computational_replacement_feasibility, empirical, 'Whether computational methods can replace manual comparative reconstruction').

omega_variable(
    natural_language_vs_constructed_epistemology,
    'Is the authority structure fundamentally grounded in epistemological requirements (how we reliably infer historical states from observable data) or contingently grounded in institutional path-dependency (the historical accident of which scholars controlled the discipline when methodological standards crystallized)?',
    'Genealogical analysis: trace the specific intellectual decisions that established the Neogrammarian principle, comparative method, and reconstruction criteria; identify counterfactual institutional histories where alternative traditions consolidated; assess whether the standards chosen were logically necessary or one choice among several viable alternatives',
    'If epistemologically necessary: authority structure approaches mountain (inherent to linguistic science). If contingent: authority structure is constructed constraint with real beneficiaries (Tangled Rope/Snare confirmed). Determines whether false summit is correctly diagnosed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_language_vs_constructed_epistemology, conceptual, 'Whether authority structure is epistemologically necessary or institutionally contingent').

omega_variable(
    reconstruction_accuracy_ceiling,
    'Is there a fundamental limit to how far back reliable linguistic reconstruction can extend (reconstructing Proto-Human-Language boundary), and does the authority structure''s gatekeeping function approach this limit, or is gatekeeping maintained well below the epistemic boundary?',
    'Measurement of reconstruction depth across language families: compare distance to language family root in linguistic generations vs confidence in specific reconstructed forms; identify whether reconstruction uncertainty grows monotonically or whether authority structure prevents exploration beyond conventional boundaries; analysis of whether unexplored domains exist below the competence ceiling',
    'If gatekeeping is near epistemic boundary: suppression may be justified (Mountain features emerge). If gatekeeping maintains artificial shallowness: suppression is extraction (Snare features confirmed). Determines how much of suppression is structural necessity vs institutional conservatism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reconstruction_accuracy_ceiling, empirical, 'Whether authority structure maintains gatekeeping at or below epistemic reliability boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(authority_structure_axis, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auth_tr_t0, authority_structure_axis, theater_ratio, 0, 0.38).
narrative_ontology:measurement(auth_tr_t10, authority_structure_axis, theater_ratio, 10, 0.52).
narrative_ontology:measurement(auth_tr_t20, authority_structure_axis, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(auth_be_t0, authority_structure_axis, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(auth_be_t10, authority_structure_axis, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(auth_be_t20, authority_structure_axis, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(authority_structure_axis, identity_coordination).
narrative_ontology:boltzmann_floor_override(authority_structure_axis, 0.12).
narrative_ontology:affects_constraint(authority_structure_axis, language_family_classification_stability).
narrative_ontology:affects_constraint(authority_structure_axis, indigenous_linguistic_sovereignty).
narrative_ontology:affects_constraint(authority_structure_axis, computational_phylogenetics_legitimacy).

% DUAL FORMULATION NOTE:
% The authority structure axis is the upstream constraint that controls the terms under which downstream constraints (classification stability, indigenous sovereignty, computational legitimacy) can be negotiated. Alternative authority structures (community-based, computational, pluralistic) would reshape all three downstream constraints. The authority axis is decomposed from specific domain constraints (e.g., the Neogrammarian principle as applied to Proto-Indo-European vs Proto-Austronesian) and represents the meta-level institutional arrangement that governs how domain-specific constraints are established and enforced.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
