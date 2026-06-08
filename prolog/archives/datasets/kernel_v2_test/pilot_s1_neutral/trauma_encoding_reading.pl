% ============================================================================
% CONSTRAINT STORY: trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trauma_encoding_reading, []).

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
 *   constraint_id: trauma_encoding_reading
 *   human_readable: Ritual as Intergenerational Trauma Encoding (Threat-Detection Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel:
 *   catastrophe_memory_kernel. The kernel is the stabilized community
 *   commitment to 'remembering and transmitting the memory of ancestral
 *   catastrophe through successive generations.' Different readings of this
 *   kernel emphasize different mechanisms: this reading
 *   (trauma_encoding_reading) emphasizes the trauma-encoding function —
 *   rituals encode ancestral trauma as intergenerational warning systems,
 *   imposing psychological burden on descendants to maintain collective
 *   threat-vigilance. Sibling readings emphasize symbol-continuity (the
 *   ritual preserves core cultural meaning independent of trauma content),
 *   survival-competence (the ritual transmits practical knowledge for
 *   surviving recurrent threat types), and boundary-maintenance (the ritual
 *   marks community identity against external threat or assimilation). The
 *   same structured ritual can be read through any of these frames. This
 *   constraint documents the trauma-encoding reading as the mechanism
 *   privileged in contemporary trauma studies and psychodynamic religious
 *   studies — the reading treats the ritual as a system for transmitting
 *   unresolved ancestral wounds to descendants, who thereby become carriers
 *   of collective trauma and maintain heightened vigilance against the threat
 *   category that generated the original catastrophe. The reading carries
 *   extractiveness (descendants bear psychological burden) coupled with
 *   genuine coordination (the ritual does create intergenerational collective
 *   identity and threat awareness). Beneficiary is the collective
 *   threat-vigilance capacity; victim is the descendant psychological burden.
 *   The constraint exhibits mixed classification across perspectives: snare
 *   from the descendant's viewpoint (identity-locked, no exit), tangled rope
 *   from leadership and therapeutic perspectives (genuine coordination
 *   coupled with asymmetric extraction), rope from institutional perspectives
 *   (memory preservation is real coordination), piton from academic tradition
 *   (theatrical scholarly interpretation), mountain from the
 *   evolutionary-inevitability framing (naturalized as law of human cultural
 *   cognition). The temporal measurements show increasing extractiveness and
 *   suppression over the interval (0.48→0.68 and 0.42→0.65) and rising
 *   theater ratio (0.35→0.51), suggesting that as institutional scholarly
 *   interest in trauma-encoding mechanisms has intensified, the original
 *   coordination function has become more theatrical (academic frameworks
 *   dominate interpretation) and the suppressive force (the obligation to
 *   carry ancestral trauma) has strengthened. This pattern is consistent with
 *   institutional capture: the constraint's persistence is increasingly
 *   driven by scholarly/institutional benefit rather than by the
 *   intergenerational transmission function it originally served.
 *
 * KEY AGENTS:
 *   - Descendant Inheritors: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with the transmission imperative; bear the full psychological weight of ancestral trauma encoded in ritual
 *   - Community Ritual Leadership: Primary beneficiary (organized/constrained) — maintains interpretive authority over the ritual; benefits from cohesion and vigilance capacity while controlling the transmission mechanism
 *   - Institutional Memory Keepers: Secondary beneficiary (institutional/arbitrage) — archival, scholarly, or religious institutions that preserve and legitimize the trauma narratives; derive authority and research domain from the constraint's operation
 *   - Therapeutic Practitioners: Mixed (moderate/constrained) — see both coordination (ritual processes collective trauma) and extraction (ritual perpetuates unprocessed wounds); constrained by need to respect community autonomy
 *   - Academic Interpretive Tradition: Institutional beneficiary (organized/constrained) — builds career and disciplinary legitimacy from studying trauma-encoding mechanisms; theater is high because scholarly apparatus describes function without enabling modification
 *   - Evolutionary Inevitability Perspective: Naturalizing view (analytical/analytical) — risks treating specific institutional arrangements as immutable properties of human cognition and culture
 *   - Analytical Observer: Committer position (analytical/identity_locked) — recognizes the reading as ONE reading rather than THE fact, yet is identity-locked into frameworks that privilege this reading; instantiates the oracle gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trauma_encoding_reading, 0.62).
domain_priors:suppression_score(trauma_encoding_reading, 0.58).
domain_priors:theater_ratio(trauma_encoding_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trauma_encoding_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(trauma_encoding_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(trauma_encoding_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(trauma_encoding_reading, "Ritual as Intergenerational Trauma Encoding (Threat-Detection Reading)").
narrative_ontology:topic_domain(trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trauma_encoding_reading, '58e1cc0f-29c1-476d-ac85-5fbe7181b253').
narrative_ontology:cs_kernel_codification('58e1cc0f-29c1-476d-ac85-5fbe7181b253', distributed).
narrative_ontology:cs_authority_grounding('58e1cc0f-29c1-476d-ac85-5fbe7181b253', lineage).
narrative_ontology:cs_interpretation_layer_present('58e1cc0f-29c1-476d-ac85-5fbe7181b253').
narrative_ontology:cs_reading_relation('58e1cc0f-29c1-476d-ac85-5fbe7181b253', trauma_encoding_reading__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('58e1cc0f-29c1-476d-ac85-5fbe7181b253', trauma_encoding_reading__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('58e1cc0f-29c1-476d-ac85-5fbe7181b253', trauma_encoding_reading__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('58e1cc0f-29c1-476d-ac85-5fbe7181b253', foundational, trauma_is_constitutive_intergenerational_gift).
narrative_ontology:cs_axiom_status(trauma_is_constitutive_intergenerational_gift, holdable).
narrative_ontology:cs_axiom_grounding('58e1cc0f-29c1-476d-ac85-5fbe7181b253', trauma_is_constitutive_intergenerational_gift, deontological).
narrative_ontology:cs_axiom('58e1cc0f-29c1-476d-ac85-5fbe7181b253', foundational, ritual_encodes_truth_of_vulnerability).
narrative_ontology:cs_axiom_status(ritual_encodes_truth_of_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('58e1cc0f-29c1-476d-ac85-5fbe7181b253', ritual_encodes_truth_of_vulnerability, empirically_contingent).
narrative_ontology:cs_reference_frame('58e1cc0f-29c1-476d-ac85-5fbe7181b253', trauma_transmission_for_threat_vigilance).
narrative_ontology:cs_drift_state('58e1cc0f-29c1-476d-ac85-5fbe7181b253', contemporary_institutional_study_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('58e1cc0f-29c1-476d-ac85-5fbe7181b253', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trauma_encoding_reading, collective_threat_vigilance).
narrative_ontology:constraint_victim(trauma_encoding_reading, descendant_psychological_burden).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESCENDANT INHERITOR (SNARE) — Identity fused with ancestral lineage and the imperative to 'carry forward' the community's memory through embodied ritual performance. Exit would require abandoning not just the ritual but the identity 'member of this traumatized community.' Structurally mobile (could leave the geographic/social community) but identity-locked into the transmission role. Bears the full psychological weight of the encoded trauma without originating it — experiences the ritual as non-negotiable burden.
constraint_indexing:constraint_classification(trauma_encoding_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: COMMUNITY RITUAL LEADERSHIP (TANGLED ROPE) — Benefits from the structural cohesion and vigilance capacity the ritual creates (genuine coordination function: binding trauma-informed collective identity and threat awareness), while also maintaining the ritual through authoritative role and legitimacy. Constrained by the need to preserve the ritual's integrity — deviation is experienced as betrayal. Real coordination (grief, shared narrative, threat awareness) coupled with asymmetric extraction (community leaders control the interpretation and performance of ancestral trauma).
constraint_indexing:constraint_classification(trauma_encoding_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL MEMORY KEEPER (ROPE) — Scholarly, archival, or religious institutional role that preserves and interprets the community's trauma narratives. Benefits from the ritual's continued operation (provides research/preservation domain, legitimates institutional expertise) and also coordinates genuine preservation function. Experiences the constraint as beneficial coordination — the ritual is the mechanism by which institutional knowledge is transmitted and validated. Can exit through institutional mobility (transfer, publication, new role) if needed.
constraint_indexing:constraint_classification(trauma_encoding_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: THERAPEUTIC PRACTITIONER (TANGLED ROPE) — Psychologist, social worker, or healer working within the community sees both genuine coordination (ritual contains and processes collective trauma, creates meaning and solidarity) and extraction (the ritual may perpetuate unprocessed trauma, prevent individualized healing paths, obligate descendants to carry unresolved ancestral wounds). Constrained by the need to respect community autonomy while recognizing the psychological costs. Moderate extraction because some benefit (collective processing) exists alongside substantial cost (transgenerational trauma burden).
constraint_indexing:constraint_classification(trauma_encoding_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ACADEMIC INTERPRETIVE TRADITION (PITON) — Scholarly disciplines (religious studies, trauma studies, anthropology) have built extensive frameworks for understanding ritual encoding of collective trauma. The frameworks are largely performative — they describe and legitimize the ritual's function as 'adaptive threat-detection mechanism' without interrogating whether the ritual still serves that function or has become theater maintained by institutional scholarly interest in 'indigenous knowledge systems.' The analytical apparatus performs interpretation without necessarily enabling change. Theater ratio high because the scholarly apparatus generates publication and career value from studying the ritual's function while rarely producing interventions that modify the ritual itself.
constraint_indexing:constraint_classification(trauma_encoding_reading, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW / EVOLUTIONARY INEVITABILITY (MOUNTAIN) — Some evolutionary psychology and neurobiology frameworks treat trauma encoding in ritual as an inevitable adaptation: human memory systems encode threat through repetition and affect; communities naturally ritualize collective trauma to maintain intergenerational vigilance; this is how meaning-making works at civilizational scale. The constraint appears as an immutable feature of how human societies process catastrophe. However, this perspective risks naturalizing a contestable institutional arrangement (specific forms of ritual trauma transmission) as an unchangeable law of human cognition and culture.
constraint_indexing:constraint_classification(trauma_encoding_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COMMITTER POSITION (TANGLED ROPE with identity_lock) — The analyst who recognizes the trauma-encoding reading as ONE reading of the catastrophe-memory kernel instantiates an oracle-gap paradox (Theorem 4 U4): the analyst can see that the trauma-encoding reading is a reading rather than a fact, yet the analyst's own academic identity and training is locked into interpretive frameworks that privilege this reading. Meta-cognitive awareness ('this is one reading among others') does not equal freedom from the frame ('I analyze through this reading because my professional identity is constituted through it'). The analytical position itself is captured by the tradition it claims to analyze objectively.
constraint_indexing:constraint_classification(trauma_encoding_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trauma_encoding_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trauma_encoding_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trauma_encoding_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trauma_encoding_reading, TR),
    TR >= 0.70.

:- end_tests(trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-to-high. Descendants inherit unresolved ancestral trauma encoded in ritual obligations and symbolic representations. The extraction occurs through psychological burden (carrying the collective wound), identity fusion with the trauma-bearing role (cannot separate self from transmission duty), and intergenerational transmission of threat-vigilance that may exceed present-day threat reality. The value 0.62 reflects that genuine coordination exists (collective identity formation, meaningful connection to ancestral memory) alongside substantial cost (trauma bearer status, obligatory grief, hypervigilance requirements). Suppression (0.58): Moderate-to-high. Suppression operates primarily through identity-lock rather than material barriers — descendants are structurally mobile (could leave community, refuse ritual) but psychologically bound by identity fusion ('I am a member of this traumatized community' is constitutive of self-concept). Secondary suppression mechanisms: social penalty for non-participation (loss of community status, kinship recognition), religious/spiritual authority (ritual legitimized as sacred duty), institutional reinforcement (scholars, priests, therapists reinforce the framework). Theater ratio (0.48): Moderate. The ritual contains both genuine coordination (it does create collective identity and meaning, does transmit shared narrative and threat awareness) and performative elements (contemporary scholarly framing as 'adaptive mechanism' may exceed evidence; academic tradition derives publication and career value from studying it; interpretation increasingly theatrical as empirical threat recedes). The rising trajectory (0.35→0.51) reflects increasing institutional scholarly interest in the trauma-encoding framework, which generates theoretical elaboration and interpretive apparatus that is often performative relative to actual modification of community practices.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces radically different classifications from different positions. The descendant sees snare (burden without exit, though exit is identity-theoretically unavailable rather than materially impossible). The ritual leadership sees tangled rope (genuine coordination of collective identity and threat awareness coupled with asymmetric control of interpretation). The institutional memory keeper sees rope (beneficial preservation and transmission function). The academic tradition sees piton (theatricalized interpretation maintaining scholarly domain). The evolutionary view sees mountain (naturalized as inherent to human cultural cognition). The analytical committer sees tangled rope with their own identity-lock, instantiating the oracle gap where the analyst can recognize the reading as a reading but cannot step outside the interpretive frames that constitute their analytical position. This perspectival range reveals that the constraint's classification is fundamentally observer-dependent: the same ritual structure produces extraction for some agents and coordination for others, depending on their structural position relative to the trauma-transmission mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position. Descendants are full targets (d→1.0) relative to the trauma-burden extraction, modulated upward by identity-lock (the exit option does not reduce experienced d because the target cannot exercise exit psychologically even if materially available). Community leaders are moderate targets (d~0.4-0.5) — they initiate and maintain the constraint, so they are beneficiaries of the coordination function, but they also inherit the cultural weight of the trauma narrative they transmit. Institutional actors are low-extraction (d~0.1-0.2) — they benefit from the constraint's persistence through research/preservation domain, and can exit through institutional mobility if needed. The academic tradition is similarly low-extraction (d~0.15) — derives career value but can exit through field change. The evolutionary perspective, if it claims to be objective natural law, has d~0 (no agent benefits or suffers from physics; but this risks naturalizing what is actually a contested institutional arrangement). The analytical committer at identity-locked position experiences d~0.35 (can see the extraction without being able to escape interpretive frameworks that obscure their own captured position).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does NOT show resolved mandatrophy; instead, it exhibits mandatrophy drift. The original mandate (transmit intergenerational trauma-informed collective identity and threat awareness to maintain community resilience through vigilance) remains formally stated but empirically questionable. The founding problem was catastrophic recurrent threat (persecution, genocide, displacement) that required descendants to maintain threat-awareness and in-group cohesion. Contemporary descendants inherit the trauma-encoding ritual and threat-vigilance requirement even when the original threat category has attenuated (e.g., geographic safety achieved, legal protections established, assimilation pressure reduced). The constraint persists through institutional capture: scholars study it, religious authorities maintain it, therapists interpret it, memory institutions archive it. Descendants perform the ritual partly from identity-lock (cannot separate self from the role of trauma-bearer) and partly from the residual genuine function (collective identity, meaning-making, connection to ancestors). The mandatrophy is not fully resolved because both the functioning-coordination reading and the degraded-extraction reading remain empirically viable depending on how you measure: if you measure threat-vigilance capacity, some communities show genuine coordination benefit; if you measure psychological well-being of descendants, extraction cost is evident. The temporal measurements showing increasing extractiveness and theater ratio (0.35→0.51) suggest drift toward institutional capture — the constraint is becoming theater maintained by scholarly/institutional interest rather than by genuine intergenerational transmission function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_detection_validity,
    'Does the ritual''s encoding of ancestral trauma actually maintain genuine threat-detection capacity, or has the original adaptive function atrophied while the psychological burden persists?',
    'Comparative historical analysis: do communities whose ritual transmission practices are active show measurably higher situational awareness of threat-class recurrence (e.g., historical patterns of persecution, collective vulnerability) compared to communities where such rituals have attenuated or disappeared? Longitudinal psychological assessment of descendant well-being correlated with ritual engagement intensity.',
    'If threat-detection remains functional: the tangled rope classification holds — genuine coordination value justifies some extraction cost. If function has atrophied: reclassify toward snare — the extraction persists as pure burden, maintained by institutional inertia rather than adaptive necessity. This is the core ambiguity the trauma-encoding reading cannot resolve from within itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threat_detection_validity, empirical, 'Whether the ritual maintains genuine adaptive threat-detection or perpetuates obsolete burden').

omega_variable(
    trauma_transmission_mechanism,
    'Is intergenerational trauma transmission through ritual a distinct mechanism from other modes of cultural knowledge transmission, or is it the same coordination structure with affect-laden content?',
    'Comparative study of ritual-encoded knowledge vs. non-traumatic institutional knowledge transmission (e.g., trade practices, spiritual cosmology, historical narratives without trauma content). Measure whether the extractiveness and suppression profiles differ by content valence, or whether the differences are structural (how rituals work, independent of emotional content).',
    'If mechanism is distinct: the trauma-encoding reading is capturing a real structural category requiring separate analysis. If mechanism is generic coordination with variable emotional loading: the reading conflates ritual form with trauma content, and the extractiveness may be attributable to ritual enforcement generally, not to trauma encoding specifically. This affects whether we should decompose the constraint into separate trauma-content and ritual-coordination stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trauma_transmission_mechanism, conceptual, 'Whether trauma encoding is a distinct transmission mechanism or trauma-laden generic coordination').

omega_variable(
    descendant_agency_paradox,
    'Can descendants meaningfully choose their relationship to encoded ancestral trauma while remaining members of the community that encoded it?',
    'Ethnographic study of exit options available to descendants: can they opt out of ritual performance without losing kinship, community status, or identity recognition? Can they reinterpret the ritual privately while conforming publicly? Can they engage selectively (ritual performance without internalization)? Document the social, economic, and psychological costs of each option.',
    'If exit is genuinely unavailable without community expulsion: classification shifts toward snare (trapped) across all descendant perspectives. If exit is available at cost (can leave or redefine privately, but bearing social penalty): classification holds as tangled rope / identity-locked snare. If descendant agency is genuinely preserved: classification may shift toward rope (descendants are coordinating intergenerationally, not just bearing extraction). This omega maps directly to the identity_locked vs. trapped distinction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(descendant_agency_paradox, empirical, 'Whether descendants have genuine exit options from the encoded trauma transmission').

omega_variable(
    reading_foreclosure_boundary,
    'Does the trauma-encoding reading logically foreclose the symbol-continuity reading (sibling 1), or can both readings coexist as different interpretations of the same ritual?',
    'Formal analysis of the axioms declared in each reading: does one reading''s core premise directly contradict the other''s, making both simultaneously true impossible in any single coherent framework? Or do they make compatible (if competing) claims about what the ritual does? If compatible, they coexist; if contradictory, one forecloses the other.',
    'Determines the reading_relations value in cs_structure.reading_relations. If foreclosed: relation = ''forecloses''. If compatible: relation = ''coexists_with''. If one creates pressure on the other without eliminating it: relation = ''influences''. This is a structural-logical question, not an empirical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Logical relationship between trauma-encoding reading and symbol-continuity reading').

omega_variable(
    institutional_benefit_ambiguity,
    'How much of the ritual''s persistence is driven by genuine intergenerational transmission of threat-informed collective identity, and how much by institutional benefit to memory-keepers, scholars, religious authorities, and therapists who derive authority and domain from interpreting the trauma?',
    'Analyze institutional incentive structures: what happens to authority, funding, publication opportunity, and legitimacy of memory institutions if the ritual attenuates or transforms? Compare communities where ritual intensity has increased over time vs. decreased — what institutional changes preceded or followed? Document the scholarship, grants, career advancement, and institutional prestige generated by studying this particular constraint.',
    'If institutional capture is dominant: the beneficiary (collective threat-vigilance) may be shadowed or replaced by institutional actors (scholars, priests, therapists) who benefit from the ritual''s persistence more than the descendant community does. Reclassify from tangled_rope toward snare. If genuine coordination dominates: the tangled rope classification holds. This omega addresses whether the constraint is truly what it claims to be (intergenerational trauma encoding for threat detection) or whether institutional interests have co-opted the framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_benefit_ambiguity, empirical, 'Proportion of ritual persistence driven by genuine transmission vs. institutional beneficiary capture').

omega_variable(
    alternative_trauma_processing,
    'What trauma-processing approaches not involving ritual encoding of ancestral burden are available to communities, and what prevents their uptake?',
    'Document evidence of alternative approaches: individual therapy, community healing circles, narrative reframing practices, institutional accountability processes, secular memorialization. For communities that have adopted alternatives, assess outcomes (psychological well-being, threat awareness, community cohesion) compared to ritual-encoding communities. Identify barriers to adoption: cost, stigma of ''Western'' approaches, incompatibility with existing authority structures, identity threat from decoupling trauma from collective ritual practice.',
    'If effective alternatives exist and are accessible: the ritual-encoding constraint is a choice among options, not a necessity. If barriers are primarily psychological/identity-based (not material): reclassify descendants as identity_locked constrained rather than trapped. If barriers are material/institutional: classification may be correct as-is. If no effective alternatives exist: the ritual maintains unique value and the extraction cost is less easily characterized as arbitrary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_trauma_processing, empirical, 'Availability and effectiveness of non-ritual trauma-processing alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trauma_encoding_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trauma_tr_t0, trauma_encoding_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(trauma_tr_t2, trauma_encoding_reading, theater_ratio, 2, 0.4).
narrative_ontology:measurement(trauma_tr_t4, trauma_encoding_reading, theater_ratio, 4, 0.48).
narrative_ontology:measurement(trauma_tr_t6, trauma_encoding_reading, theater_ratio, 6, 0.51).

% Extraction over time
narrative_ontology:measurement(trauma_be_t0, trauma_encoding_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(trauma_be_t2, trauma_encoding_reading, base_extractiveness, 2, 0.55).
narrative_ontology:measurement(trauma_be_t4, trauma_encoding_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(trauma_be_t6, trauma_encoding_reading, base_extractiveness, 6, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trauma_su_t0, trauma_encoding_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(trauma_su_t2, trauma_encoding_reading, suppression_requirement, 2, 0.48).
narrative_ontology:measurement(trauma_su_t4, trauma_encoding_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(trauma_su_t6, trauma_encoding_reading, suppression_requirement, 6, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trauma_encoding_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(trauma_encoding_reading, 0.12).
narrative_ontology:affects_constraint(trauma_encoding_reading, symbol_continuity_reading).
narrative_ontology:affects_constraint(trauma_encoding_reading, survival_competence_reading).
narrative_ontology:affects_constraint(trauma_encoding_reading, boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of a constraint family decomposed from the catastrophe_memory_kernel. Each sibling reading has a different epsilon value and different beneficiary/victim structure. The trauma_encoding_reading emphasizes psychological burden (high extraction for descendants) coupled with genuine threat-vigilance coordination. Sibling readings decompose the same ritual structure into different functional claims. All four stories are linked via network.affects_constraints to show how scholarly/institutional emphasis on the trauma-encoding frame influences interpretation of the other mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trauma_encoding_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
