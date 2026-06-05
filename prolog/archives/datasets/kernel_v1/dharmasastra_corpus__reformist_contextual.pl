% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra Reformist Contextual Reading: Ethical Core Separable from Caste Prescription
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   The reformist-contextual reading of dharmasastra claims that the ethical
 *   core (dharma as righteous conduct, social duty, cosmic order) is
 *   separable from time-bound caste prescriptions, allowing modern Hindu
 *   communities to preserve textual authority while discarding oppressive
 *   literalism. This is ONE READING of the contested dharmasastra kernel —
 *   the textual corpus itself remains fixed, but different communities read
 *   it through fundamentally different interpretive lenses. The reformist
 *   reading coexists with the orthodox literalist reading (which affirms
 *   caste prescription as eternal) and contrasts with the abolitionist
 *   rejection (which abandons the text's normative force entirely). The
 *   reformist reading creates a constraint because it enables identifiable
 *   beneficiaries (reformist intellectual tradition, modernist Hindu
 *   institutions) to maintain scriptural authority while navigating
 *   contemporary ethical standards that condemn caste hierarchy. The
 *   constraint extracts from historically subordinated castes through a
 *   mechanism of perpetual reinterpretation: each oppressive passage requires
 *   doctrinal softening rather than transparent reading, imposing an
 *   hermeneutic burden on those reading 'against' the text's apparent
 *   meaning. The theater ratio (0.65) reflects that the apparatus of
 *   dharmasastra authority (recitation, commentarial tradition, ritual
 *   validation) continues even as the substantive normative force of caste
 *   prescription has substantially decoupled from lived practice in many
 *   communities. The measurement trajectory shows rising extractiveness and
 *   theater as the reading has become institutionalized: initial reform
 *   efforts lowered enforcement suppression, but as modernist institutions
 *   adopted the reformist frame, the theater of textual authority increased
 *   (the reading required constant hermeneutic work to maintain coherence)
 *   and the experienced extraction by subordinated groups rose (now bearing
 *   the burden of accepting 'reinterpreted' rather than rejected hierarchy).
 *
 * KEY AGENTS:
 *   - Reformist Intellectual Tradition: Primary beneficiary (organized/mobile) — scholars and religious leaders who benefit from maintaining dharmasastra authority while discarding literalism; low experienced extraction, benefits from continued relevance
 *   - Hindu Modernist Movements and Institutions: Co-beneficiary (institutional/constrained) — institutions navigating dual legitimacy through the reformist reading; benefit from sacred-text authority, constrained by hermeneutic burden
 *   - Historically Subordinated Castes: Primary victim (powerless/trapped) — bear the hermeneutic burden of accepting 'reinterpreted' rather than rejected hierarchy; no exit from textual authority framework
 *   - Modern Hindu Communities (Mixed): Secondary actor (moderate/constrained) — benefit from reformist reinterpretation (can claim modernism while preserving tradition), constrained by perpetual reinterpretive labor
 *   - Textual Authority Apparatus: Institutional actor (institutional/arbitrage) — recitation tradition, commentarial lineages, liturgical validation; maintains theater through inertia despite functional degradation (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the strategic 'timeless vs. time-bound' distinction as a law of hermeneutics rather than an institutional move
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.52).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.48).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.52).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra Reformist Contextual Reading: Ethical Core Separable from Caste Prescription").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'd2896fd1-3734-4b8d-ad91-82eeb4d5e25c').
narrative_ontology:cs_kernel_codification('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', fixed_text).
narrative_ontology:cs_authority_grounding('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', lineage).
narrative_ontology:cs_interpretation_layer_present('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c').
narrative_ontology:cs_reading_relation('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', dharmasastra_corpus__abolitionist_rejection, influences).
narrative_ontology:cs_axiom('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', foundational, ethical_core_separable_from_caste).
narrative_ontology:cs_axiom_status(ethical_core_separable_from_caste, holdable).
narrative_ontology:cs_axiom_grounding('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', ethical_core_separable_from_caste, deontological).
narrative_ontology:cs_axiom('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', foundational, textual_authority_preservable_through_reinterpretation).
narrative_ontology:cs_axiom_status(textual_authority_preservable_through_reinterpretation, holdable).
narrative_ontology:cs_axiom_grounding('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', textual_authority_preservable_through_reinterpretation, conventional).
narrative_ontology:cs_reference_frame('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', ethical_dharma_separable_from_varna_hierarchy).
narrative_ontology:cs_drift_state('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', contemporary_post_colonial_human_rights_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d2896fd1-3734-4b8d-ad91-82eeb4d5e25c', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_intellectual_tradition).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, hindu_modernist_movements).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, historically_subordinated_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, textual_hermeneutic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATED CASTES (SNARE) — Trapped by the textual authority claim: the reformist reading claims to liberate by separating ethical core from caste prescription, but the underlying text still encodes caste hierarchy. The reinterpretation offers no material exit — the constraint persists in symbolic form, justified by reformist authority rather than orthodoxy. No exit option; full experienced extraction despite the doctrinal promise of reform.
constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MODERN HINDU COMMUNITIES (TANGLED ROPE) — Constrained by the need to maintain textual authority while navigating contemporary ethical standards. Benefit from the reformist reinterpretation (can claim modernism while preserving tradition); bear costs of the perpetual reinterpretive burden (each caste-related passage requires doctrinal reframing rather than transparent reading). Genuine coordination function (preserving dharmasastra relevance) alongside asymmetric extraction (subordinated groups must continually accept reinterpretations that soften rather than eliminate hierarchy).
constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORMIST INTELLECTUAL TRADITION (ROPE) — Organized scholarly and religious leaders who benefit from the contextual reading framework. Experience the constraint as coordination: communicating ancient wisdom in contemporary terms enables continued dharmasastra authority and cultural transmission. Low effective extraction — the tradition has agency and benefits from maintaining textual authority while discarding oppressive literalism. The reinterpretive burden is a scholarly exercise, not a material constraint.
constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: HINDU MODERNIST MOVEMENTS (TANGLED ROPE) — Institutional actors navigating dual legitimacy: claiming dharmasastra authority (enabling tradition claims) while advocating contemporary ethical standards (enabling modernist credentials). Benefit from the reformist reading (institutional legitimacy through sacred text); constrained by the hermeneutic burden (must continually justify separations between 'timeless ethics' and 'time-bound prescriptions'). Active enforcement required to maintain coherence across heterodox readings.
constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DHARMASASTRA TEXTUAL AUTHORITY (PITON) — The apparatus of scriptural interpretation (commentaries, recitation, liturgical validation) persists through institutional inertia despite the reformist reading having substantially decoupled textual prescription from lived practice. Theater ratio is high: the ritual validation of dharmasastra authority continues even as the actual normative force of caste prescriptions has degraded. The textual authority frame is maintained because alternatives (explicit rights-based ethics, secular law) haven't fully replaced it in traditional communities, not because the literalist reading functions.
constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalized perspective, the reformist reading appears to identify an unchangeable logical distinction: any ancient ethical text contains historically contingent prescriptions layered over timeless principles; the separation is a natural feature of textual hermeneutics. However, this naturalizes what is actually a strategic interpretive move driven by identifiable beneficiaries (reformist intellectual tradition, modernist institutions). The engine will classify this as a false summit.
constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dharmasastra_corpus__reformist_contextual, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, TR),
    TR >= 0.70.

:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reformist reading preserves hierarchy in doctrinal form while removing it from explicit enforcement. Subordinated groups experience extraction through the hermeneutic burden: they must continually accept 'reinterpreted' passages rather than rejected ones, and the reinterpretation itself becomes a new form of authority claim (the intellectual tradition claiming superior understanding of the text's 'true meaning'). The extraction is not as severe as full literalist enforcement (which would be 0.70+) but higher than pure rope coordination (which would be ≤0.45) because the extraction persists through the claim that the hierarchy is ethically justified, just historically contingent. Suppression (0.48): Moderate. The structural barriers to rejecting dharmasastra authority have declined (secular law, alternative modernities exist), but the internalized acceptance of the text's normative force remains high. The reformist reading exploits this internalization: because subordinated groups accept the text as spiritually authoritative, they accept the reinterpretation as a form of liberation even though the hierarchy persists symbolically. Theater ratio (0.65): Moderate-high. The rituals and institutions of dharmasastra authority (recitation, commentarial validation, performance in temples and households) continue despite the reformist reading having substantially decoupled them from actual prescription. The theater has increased over time as modernist institutions have institutionalized the reformist frame, requiring continuous hermeneutic labor to maintain coherence.
 *
 * PERSPECTIVAL GAP:
 *   The reformist reading produces a dramatic perspectival gap between beneficiaries and victims. The reformist intellectual tradition sees coordination (Rope) — they are solving the problem of preserving tradition in contemporary contexts. Modernist institutions see tangled coordination-and-extraction (Tangled Rope) — they benefit from textual authority but are constrained by the need to continually justify the separation of timeless from time-bound. Subordinated castes see pure extraction (Snare) — the reinterpretation offers no material exit and imposes hermeneutic burden. The piton perspective (institutional textual authority apparatus) sees a degraded ritual persisting through inertia. The analytical observer risks seeing natural law (Mountain) — interpreting the 'timeless vs. time-bound' logic as a universal hermeneutic principle rather than a strategic move that happens to benefit identifiable parties.
 *
 * DIRECTIONALITY LOGIC:
 *   The reformist reading's beneficiaries (organized intellectual tradition, institutional modernist movements) occupy positions with high agency and low extraction exposure: they define the reinterpretive frame and benefit from continued textual authority without bearing the hermeneutic burden. Their d values are low (0.10–0.25), producing negative or minimal f(d), resulting in low experienced extraction. The victims (subordinated castes) occupy positions with low agency (unable to reject the text's authority frame) and high extraction exposure (must continually accept reinterpretations). Their d values are high (0.85–0.95), producing high f(d), resulting in high experienced extraction. The institutional actors navigating dual legitimacy occupy intermediate positions: d ≈ 0.50–0.60, producing moderate f(d) and moderate experienced extraction. The reformist reading's directionality structure is the defining feature of its classification as tangled_rope (not pure rope, despite coordination function) — there is genuine coordination (preserving dharmasastra relevance) alongside genuine asymmetric extraction (who bears the hermeneutic burden).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ethical_core_separability,
    'Is the ''ethical core'' of dharmasastra (righteous conduct, social duty) genuinely separable from its time-bound caste prescriptions, or are they structurally entangled such that the ''core'' only gains meaning within the caste hierarchy?',
    'Close hermeneutic analysis of dharmasastra texts: identify passages where the stated ethical principle directly derives legitimacy from caste-based role differentiation. Determine whether decontextualized principles remain coherent. Cross-cultural comparison with other stratified societies'' reformist reinterpretations.',
    'If separable: the reformist reading is epistemically sound; extraction is incidental to a genuine reinterpretation. If entangled: the separation is performative; the reformist reading naturalizes hierarchy by reframing it as ''timeless ethics'' applied to ''outdated castes.'' Reclassification to higher extractiveness and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethical_core_separability, conceptual, 'Whether ethical core is genuinely separable from caste hierarchy').

omega_variable(
    competing_kernel_readings,
    'Does the reformist reading foreclose the orthodox literalist reading, coexist with it, or merely influence its legitimacy conditions within contemporary discourse?',
    'Institutional analysis: can orthodox communities maintain caste-literalist interpretations simultaneously with reformist communities within the same broader dharmasastra authority framework? Do they treat each other as logically contradictory or as different legitimate readings?',
    'If foreclose: the reformist reading is committed to rejecting literalism as incoherent — requires stronger epistemic claim. If coexist: both readings remain live; the constraint is distributing legitimacy across factions rather than resolving doctrinal dispute. If influence: reformist reading shapes what literalist reading must defend against.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_kernel_readings, empirical, 'Relationship between reformist and literalist readings of dharmasastra').

omega_variable(
    reformist_institutional_beneficiary,
    'Is the reformist reading''s institutional beneficiary status (organized intellectual tradition, modernist movements) the primary driver of the reading''s adoption, or does the reading''s epistemic coherence adequately explain its spread?',
    'Historical analysis of reformist adoption: trace adoption timing relative to institutional modernization pressures (education system changes, state integration, international influence). Identify cases where reformist reading gained traction despite institutional resistance vs. cases of pure institutional advance.',
    'If primarily institutional: the reading is sustained extraction (beneficiaries maintain authority while discarding oppressive literalism); higher suppression and theater ratio. If primarily epistemic: the reading is genuine coordinate reframing; lower extraction and suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_institutional_beneficiary, empirical, 'Whether reformist reading adoption is driven by institutional benefit or epistemic coherence').

omega_variable(
    false_summit_natural_law,
    'Is the reformist reading''s logic of textual hermeneutics (separating timeless principles from time-bound prescriptions) a natural law of interpretation, or a strategic move that happens to benefit identifiable parties?',
    'Comparative hermeneutics: examine whether identical textual forms (ancient authority + contemporary practice) receive the reformist treatment across cultures and historical periods, or whether the treatment is selective (applied to hierarchical prescriptions, not to other contentious elements). Assess whether comparable texts with different beneficiary structures receive different hermeneutic treatment.',
    'If natural law: mountain classification holds from analytical perspective. If strategic: false summit — reclassify to tangled_rope or snare from analytical context, revealing how the ''timeless vs. time-bound'' frame naturalizes extractive institutional arrangements.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether textual hermeneutic logic is a natural law or a strategic beneficiary move').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of subordinated castes under the reformist reading structural (external barriers to rejecting the text''s authority) or internalized (acceptance of the reinterpretation as legitimate liberation)?',
    'Post-exit analysis: identify communities that have abandoned dharmasastra authority entirely (converted religions, secular legal systems). Trace whether the suppression mechanism (belief in textual authority) persists after structural exit or only within the commitment framework.',
    'If structural: suppression persists regardless of reinterpretation; the reading changes nothing materially. If internalized: the reading deepens suppression by making the hierarchy appear ethically justified rather than merely imposed. Affects interpretation of theater_ratio and extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized under reformist reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_t0_preindependence, dharmasastra_corpus__reformist_contextual, theater_ratio, 0, 0.55).
narrative_ontology:measurement(theater_t1_early_reform, dharmasastra_corpus__reformist_contextual, theater_ratio, 1, 0.6).
narrative_ontology:measurement(theater_t2_contemporary, dharmasastra_corpus__reformist_contextual, theater_ratio, 2, 0.65).

% Extraction over time
narrative_ontology:measurement(extractiveness_t0_preindependence, dharmasastra_corpus__reformist_contextual, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(extractiveness_t1_early_reform, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(extractiveness_t2_modernist_institutionalization, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(suppression_t0_literalist_enforcement, dharmasastra_corpus__reformist_contextual, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(suppression_t1_reinterpretive_softening, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1, 0.52).
narrative_ontology:measurement(suppression_t2_internalized_acceptance, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, caste_hierarchy_legitimacy_claims).

% DUAL FORMULATION NOTE:
% The dharmasastra corpus decomposes into three constraint families, each representing one reading: orthodox_literalist (ε≈0.65, Snare from victim perspective), reformist_contextual (ε≈0.52, Tangled Rope), abolitionist_rejection (ε≈0.25, Rope or Scaffold). Each reading has the same underlying textual kernel but produces different structural constraints with different extractiveness values and different beneficiary/victim configurations. The three stories are linked bidirectionally via network.affects_constraints: each reading's adoption affects the others' institutional viability and legitimacy conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__reformist_contextual, organized, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
