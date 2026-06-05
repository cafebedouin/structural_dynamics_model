% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__unified_manifestation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__unified_manifestation_reading, []).

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
 *   constraint_id: kami_buddha_ontology__unified_manifestation_reading
 *   human_readable: Unified Manifestation Reading: Kami as Buddha Traces (Honji Suijaku Doctrine)
 *   domain: religious_studies/comparative_religion/japanese_buddhism
 *
 * SUMMARY:
 *   The unified manifestation reading instantiates one specific
 *   interpretation of the kami-buddha relationship that was constructed and
 *   enforced within medieval and early modern Japanese Buddhism. Under this
 *   reading, kami are not independent spiritual entities but manifestations
 *   or traces (suijaku) of more fundamental buddha-natures (honji). This
 *   framework asserts ontological hierarchy with Buddhism at the foundation
 *   and Shinto worship as dependent practice. The constraint operates through
 *   philosophical authority: Buddhist scholars interpret authoritative texts
 *   (Kūkai's esoteric writings, Tendai doctrine) to establish the
 *   metaphysical dependence of kami on buddha-ground, making it
 *   intellectually difficult (and institutionally costly) to maintain kami
 *   independence. The extractiveness value (0.52) reflects that the
 *   constraint extracts genuine philosophical and institutional authority
 *   from kami-centric traditions while offering real coordination benefits
 *   (shared sacred sites, integrated pilgrimage systems, unified cosmology).
 *   It is tangled_rope rather than snare because both beneficiaries (Buddhist
 *   institutions) and victims (autonomous Shinto practice) experienced
 *   functional benefits alongside costs — the doctrine did genuinely solve
 *   the institutional problem of two religious systems competing for the same
 *   ritual space and devotional populations. The theater ratio rises from
 *   0.32 (early medieval: honji suijaku was a live theological commitment) to
 *   0.68 (contemporary: the doctrine survives primarily as a
 *   historical-descriptive category in academic discourse). The suppression
 *   rises from 0.42 to 0.58 as alternative readings (domain partition,
 *   pragmatic accommodation, nativist independence frameworks) are
 *   increasingly suppressed through institutional control and intellectual
 *   authority.
 *
 * KEY AGENTS:
 *   - Buddhist Institutional Authority (institutional/arbitrage): Tendai, Shingon, and other syncretic schools that construct and deploy honji suijaku doctrine; primary beneficiaries through interpretive authority and institutional expansion
 *   - Kami Devotees (powerless/identity_locked): Practitioners whose religious identity is constituted through kami worship; experience maximum extraction because identity-fusion prevents exit despite subordination
 *   - Shinto Institutional Priests (moderate/constrained): Shrine priests managing the operational coordination of shared sites and integrated rituals; constrained by career dependence on Buddhist interpretive frameworks
 *   - Edo Nativist Coalition (organized/mobile): Moto'ori Norinaga, Hirata Atsutane, and intellectual successors who construct alternative frameworks rejecting honji suijaku; organized agents with political arbitrage (imperial authority)
 *   - Medieval Textual Authorities (institutional/analytical): Kūkai, Tendai patriarchs, and other foundational figures whose works are interpreted to validate unified manifestation reading
 *   - Comparative Religion Scholarship (analytical/analytical): Modern academic analysts who treat honji suijaku as exemplar of universal syncretism, risking naturalization of power effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__unified_manifestation_reading, 0.52).
domain_priors:suppression_score(kami_buddha_ontology__unified_manifestation_reading, 0.58).
domain_priors:theater_ratio(kami_buddha_ontology__unified_manifestation_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__unified_manifestation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(kami_buddha_ontology__unified_manifestation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(kami_buddha_ontology__unified_manifestation_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__unified_manifestation_reading, tangled_rope).
narrative_ontology:human_readable(kami_buddha_ontology__unified_manifestation_reading, "Unified Manifestation Reading: Kami as Buddha Traces (Honji Suijaku Doctrine)").
narrative_ontology:topic_domain(kami_buddha_ontology__unified_manifestation_reading, "religious_studies/comparative_religion/japanese_buddhism").

domain_priors:requires_active_enforcement(kami_buddha_ontology__unified_manifestation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__unified_manifestation_reading, 'd2f65730-33cd-48de-ac3d-f3c82c98bb8a').
narrative_ontology:cs_kernel_codification('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', formalized).
narrative_ontology:cs_authority_grounding('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', lineage).
narrative_ontology:cs_interpretation_layer_present('d2f65730-33cd-48de-ac3d-f3c82c98bb8a').
narrative_ontology:cs_reading_relation('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', kami_buddha_ontology__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', kami_buddha_ontology__pragmatic_accommodation_reading, coexists_with).
narrative_ontology:cs_axiom('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', foundational, buddha_ground_ontological_priority).
narrative_ontology:cs_axiom_status(buddha_ground_ontological_priority, holdable).
narrative_ontology:cs_axiom_grounding('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', buddha_ground_ontological_priority, deontological).
narrative_ontology:cs_axiom('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', foundational, kami_manifestation_dependence).
narrative_ontology:cs_axiom_status(kami_manifestation_dependence, holdable).
narrative_ontology:cs_axiom_grounding('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', kami_manifestation_dependence, deontological).
narrative_ontology:cs_reference_frame('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', syncretic_buddhist_metaphysical_hierarchy).
narrative_ontology:cs_drift_state('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', contemporary_academic_discourse, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('d2f65730-33cd-48de-ac3d-f3c82c98bb8a', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__unified_manifestation_reading, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__unified_manifestation_reading, buddhist_institutional_authority).
narrative_ontology:constraint_victim(kami_buddha_ontology__unified_manifestation_reading, indigenous_kami_worship_autonomy).
narrative_ontology:constraint_victim(kami_buddha_ontology__unified_manifestation_reading, shinto_epistemic_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KAMI DEVOTEE (SNARE) — A practitioner whose identity is constituted through kami worship experiences the unified manifestation reading as a cage. They cannot exit the constraint without abandoning their religious identity — kami worship is rendered philosophically dependent and subordinate, yet their devotional practice is culturally embedded and identity-fused. The binding is cognitive (identity fusion with kami practice) even though their structural options are constrained (limited institutional platforms, marginalized epistemic status). Maximum extraction experienced because they bear the cost of ontological subordination while unable to break the identity frame that makes them a devotee.
constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: SHINTO INSTITUTIONAL PRIEST (TANGLED ROPE) — Priests managing Shinto shrines in the medieval and early modern period experienced genuine coordination: honji suijaku allowed kami and buddhist protector figures to coexist in the same ritual space, enabling shared pilgrimage sites and collaborative festivals. But the reading also extracts: the philosophical framework declares kami fundamentally dependent on buddha-ground, reducing Shinto institutional independence and creating asymmetric authority (Buddhist temples set the interpretive standards). Exit is constrained by institutional commitments and career path dependence — a Shinto priest cannot easily abandon the doctrinal framework without losing institutional position.
constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUDDHIST INSTITUTIONAL AUTHORITY (ROPE) — Buddhist temples and their scholarly interpreters experience the unified manifestation reading as pure coordination: it solves the institutional problem of competing religious authority in Japan by establishing a coherent metaphysical hierarchy that places Buddhism at the foundational level. The reading enables Buddhist institutional expansion into formerly kami-centric sites and gives Buddhist scholars interpretive authority over the meaning of kami practice. Extraction toward the beneficiary is substantial but appears as coordination from their perspective — they are solving a real institutional integration problem.
constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: EDO INTELLECTUAL COALITION (SCAFFOLD) — By the 17th-18th centuries, organized scholars (particularly Shinto nativists like Moto'ori Norinaga and Hirata Atsutane) constructed an alternative interpretive framework rejecting honji suijaku entirely, arguing for kami ontological independence and priority. This coalition is organized (has institutional platforms through nativist schools, written texts, patronage networks) and has exit options (arbitrage: can appeal to imperial authority and nationalist ideology). Their reading of the constraint is as temporary and reversible — they see honji suijaku as an intellectual imposition that can be refuted through correct interpretation of texts and restoration of authentic Shinto. The sunset clause emerges through 19th-century modernization: Meiji ideology privileges kami worship and State Shinto, making nativist frameworks suddenly politically viable. The constraint's coercive force declines as alternative interpretations gain institutional legitimacy.
constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC THEOLOGICAL SYSTEM (PITON) — In contemporary scholarship, honji suijaku survives primarily as a descriptive historical category (scholars describe what medieval thinkers believed) rather than a live theological commitment. The doctrine persists in academic discourse through performative citation — it is taught as the 'standard' medieval Japanese Buddhist position, maintained through textbook conventions and curriculum inertia rather than through genuine theological deployment. The theater ratio is high (0.68) because the doctrine's contemporary function is largely historical-narrative (showing how medieval thinkers solved institutional problems) rather than metaphysically operative (no one is building theology on it now). It is a piton: a former snare/tangled_rope that has degraded into institutional theater.
constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMPARATIVE RELIGION ANALYST (MOUNTAIN) — From a civilizational analytical perspective, the unified manifestation reading can appear as a natural consequence of how syncretic religious systems evolve: when two traditions encounter each other, some form of ontological integration is inevitable and necessary. The analyst can treat honji suijaku as an exemplar of universal syncretism patterns (Vedantic-Hindu integration with local deities, Christian assimilation of pagan saints, Sufi-Islamic accommodation of folk veneration). This perspective risks naturalizing what is actually a contingent institutional arrangement — the framework assumes ontological hierarchy and buddhist priority are 'natural' solutions to syncretism rather than specific power effects. False summit detector will flag this, revealing that the mountain rests on beneficiary framing.
constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__unified_manifestation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kami_buddha_ontology__unified_manifestation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kami_buddha_ontology__unified_manifestation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kami_buddha_ontology__unified_manifestation_reading, TR),
    TR >= 0.70.

:- end_tests(kami_buddha_ontology__unified_manifestation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The unified manifestation reading extracts philosophical authority from kami-centric traditions by declaring them dependent on buddha-ground. Buddhist scholars gain interpretive power to determine what kami 'really are,' and Buddhist institutions gain legitimacy to manage formerly autonomous kami sites. However, the constraint also provides genuine coordination value: honji suijaku allows a single coherent cosmology that permits shared ritual space, integrated pilgrimage systems, and collaborative festivals. The benefit is real but asymmetrically distributed — Buddhist institutions capture most of the coordination surplus. Suppression (0.58): Moderate-high. The framework suppresses alternative interpretations (kami independence, mutual authority, pragmatic pluralism) through philosophical authority and institutional pressure. Buddhist scholars set the terms of legitimate discourse, making it costly to maintain that kami are ontologically autonomous. However, suppression is not complete — nativist scholars articulate alternatives throughout the period, and by the Meiji era, kami-centered frameworks become politically viable and institutional suppression weakens. Theater ratio (0.68): High in contemporary context, lower historically. In medieval Japan (T=0), honji suijaku was a live theological commitment with genuine metaphysical force — scholars believed they were discovering the true nature of kami. By the contemporary period (T=6), the doctrine survives primarily as a historical-descriptive category: it is taught as 'what medieval Buddhists believed' rather than as a true metaphysical claim. The framework persists through academic and institutional inertia rather than through genuine theological deployment — this is the signature of piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates striking perspectival divergence. The Buddhist institutional authority experiences the unified manifestation reading as rope (pure coordination) — they are solving a real institutional problem and genuinely benefit from the interpretive authority it grants them. The kami devotee experiences it as snare (maximum extraction) — their identity-fused practice is philosophically subordinated while they cannot exit the identity frame that makes them a devotee. The Shinto priest experiences tangled_rope (mixed coordination and extraction) — they genuinely coordinate shared rituals but lose institutional independence to Buddhist interpretive standards. The nativist coalition experiences it as a temporary scaffold with a sunset clause — alternative frameworks are live intellectual options that only need political conditions (Meiji restoration) to become institutionally viable. The academic analyst risks experiencing it as mountain (natural law) — treating honji suijaku as a necessary or universal solution to religious syncretism rather than a specific power arrangement. The perspectival gaps reveal that the constraint's classification depends entirely on the observer's structural relationship to the doctrine: who benefits from its ontological hierarchy, who bears the cost of subordination, who has exit options to alternative frameworks, and who is identity-locked into the existing structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural relationships. Buddhist institutional authority are beneficiaries with arbitrage options (can shift interpretive frameworks or appeal to different textual traditions) — they experience low effective extraction (negative chi from their perspective). Kami devotees are victims with identity_locked exit (cannot abandon the devotional practice without becoming a different person) — they experience maximum effective extraction. Shinto priests are victims with constrained exit (can leave priesthood but at career cost) — they experience high but not maximum extraction. Nativist intellectuals are victims with mobile exit (have intellectual platforms and political allies, can construct alternative frameworks) — they experience moderate extraction. The piano applies sigmoid f(d) based on each agent's exit modulation: trapped and identity_locked agents produce higher d values (0.89-0.95) than constrained agents (0.65-0.75) or arbitrage agents (0.15-0.25).
 *
 * MANDATROPHY ANALYSIS:
 *   The unified manifestation reading resolves mandatrophy by showing that tangled_rope classification is appropriate precisely because the doctrine provides genuine coordination (shared sacred sites, unified cosmology) while extracting asymmetric authority (Buddhist scholars determine what kami 'really are'). The constraint is NOT purely extractive (which would require snare classification) because the coordination function is real and valued by many participants. It is NOT purely coordinative (which would require rope) because the asymmetric authority and philosophical subordination cannot be explained as cost-sharing in a neutral mechanism. The measured extractiveness (0.52) reflects that the coordination surplus is genuine but unequally distributed — some agents (Buddhist institutions, temples) capture most benefits while others (kami devotees, Shinto practitioners) bear most of the subordination costs. The piton perspective correctly identifies that the doctrine has degraded into performative theater in contemporary academic discourse, where it survives through institutional inertia rather than live theological commitment. The false summit perspective (mountain from the analytical view) is correctly flagged as naturalization — the doctrine is treated as a necessary universal principle of syncretism rather than a contingent institutional arrangement that was actively constructed and enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_dependence_vs_epistemological_interpretation,
    'Does honji suijaku assert metaphysical dependence (kami cannot exist independently in reality) or merely epistemological interpretation (Buddhist doctrine provides superior framework for understanding kami, but kami remain ontologically autonomous)?',
    'Careful textual analysis of primary sources (Kūkai''s writings, Tendai syncretic texts, Shinto nativist critiques) distinguishing metaphysical claims from hermeneutic claims; examination of whether medieval practitioners treated kami as genuinely dependent beings or as entities whose worship practice could be reinterpreted',
    'If metaphysical: Buddhism structurally dominant and kami cannot exist without buddha-ground — extraction mechanism is stronger, constraint is closer to snare. If epistemological: interpretation is contested and revisable — constraint is closer to rope, and nativist counter-readings have more logical force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_dependence_vs_epistemological_interpretation, empirical, 'Whether honji suijaku makes metaphysical or hermeneutic claims about kami').

omega_variable(
    institutional_coercion_vs_voluntary_adoption,
    'To what extent was honji suijaku adoption coerced through institutional power (Buddhist temple authority, imperial patronage) versus adopted as a genuinely preferred theological solution?',
    'Historical examination of resistance patterns (how many Shinto institutions rejected honji suijaku?), evidence of pressure from Buddhist authorities, correlation between Buddhist institutional proximity and honji suijaku adoption rates, analysis of Shinto nativist critiques as articulations of suppressed alternatives',
    'If primarily coerced: constraint qualifies as snare with high suppression. If substantially voluntary: constraint shifts toward rope, with genuine coordination function reducing extraction coefficient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_coercion_vs_voluntary_adoption, empirical, 'Extent of institutional coercion in honji suijaku adoption').

omega_variable(
    kami_worship_continuity_across_the_reading,
    'Did honji suijaku doctrine actually preserve kami worship practice in functional terms, or did the reinterpretation subtly degrade kami devotional autonomy and epistemic status?',
    'Comparative analysis of kami worship intensity, pilgrimage patterns, and ritual elaboration before and after honji suijaku establishment; examination of whether reinterpreted kami practices attracted the same devotional energy; evidence from nativist revival movements about what aspects of practice needed ''restoration''',
    'If honji suijaku preserved practice: constraint approaches rope (genuine coordination benefit). If honji suijaku degraded practice despite institutional accommodation: constraint strengthens toward snare (extraction masked by apparent continuity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_worship_continuity_across_the_reading, empirical, 'Impact of honji suijaku on continuity and autonomy of kami worship').

omega_variable(
    alternative_ontologies_foreclosure,
    'Was honji suijaku the only possible coherent response to kami-buddha coexistence, or were alternative ontologies (kami independence, mutual but distinct authority, pragmatic pluralism without hierarchy) structurally available but intellectually suppressed?',
    'Reconstruction of intellectual possibilities available in medieval Japanese thought; analysis of whether alternative frameworks were explicitly argued against (suggesting they were live options) or assumed incoherent; examination of whether nativist counter-readings (17th-18th century) recovered suppressed alternatives or invented new ones',
    'If alternatives were available: honji suijaku was contingent framework imposed by power, not necessary philosophical resolution — constraint strengthens toward snare. If honji suijaku was genuinely only coherent option: constraint weakens toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_ontologies_foreclosure, conceptual, 'Whether honji suijaku was the only available coherent ontological framework').

omega_variable(
    reading_kernel_contest_empirical_status,
    'Which reading (unified manifestation, domain partition, pragmatic accommodation) best describes what medieval Japanese Buddhism actually maintained — what were the doctrinal commitments of Tendai and other syncretic schools?',
    'Primary source textual analysis (Kūkai''s Essentials of the Eight Teachings and Exoteric-Esoteric Distinction, Tendai interpretive lineages, Shingon syncretic theology) determining whether unified manifestation was a live doctrinal commitment or post-hoc scholarly interpretation; examination of whether medieval scholars explicitly rejected domain partition and pragmatic accommodation readings or treated all three as compatible',
    'If unified manifestation was dominant medieval doctrine: this reading instantiates an actual historical constraint with full force. If unified manifestation is modern scholarly reconstruction: the constraint is partially a retrospective imposition (higher theater ratio, piton characteristics). If domain partition was preferred: constraint of interest is the pragmatic accommodation reading, not this one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest_empirical_status, empirical, 'Doctrinal status of unified manifestation reading in medieval Buddhist scholarship').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__unified_manifestation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_unified_tr_t0, kami_buddha_ontology__unified_manifestation_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(kami_unified_tr_t3, kami_buddha_ontology__unified_manifestation_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(kami_unified_tr_t6, kami_buddha_ontology__unified_manifestation_reading, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(kami_unified_be_t0, kami_buddha_ontology__unified_manifestation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(kami_unified_be_t3, kami_buddha_ontology__unified_manifestation_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(kami_unified_be_t6, kami_buddha_ontology__unified_manifestation_reading, base_extractiveness, 6, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(kami_unified_su_t0, kami_buddha_ontology__unified_manifestation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(kami_unified_su_t3, kami_buddha_ontology__unified_manifestation_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement(kami_unified_su_t6, kami_buddha_ontology__unified_manifestation_reading, suppression_requirement, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__unified_manifestation_reading, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__unified_manifestation_reading, kami_buddha_ontology__domain_partition_reading).
narrative_ontology:affects_constraint(kami_buddha_ontology__unified_manifestation_reading, kami_buddha_ontology__pragmatic_accommodation_reading).

% DUAL FORMULATION NOTE:
% The kami-buddha ontology kernel contains three structurally distinct constraints corresponding to three live historical readings: unified_manifestation_reading (this file, ε≈0.52, tangled_rope), domain_partition_reading (ε≈0.28, rope or scaffold), and pragmatic_accommodation_reading (ε≈0.45, piton or snare depending on framing). Each reading was defended by medieval scholars and each had different institutional consequences. The epsilon values differ because they model different structural relationships between the constraint and what it constrains: unified manifestation extracts philosophical authority; domain partition preserves institutional separation; pragmatic accommodation masks institutional conflict. All three readings contend for interpretive authority within a single historical kernel (the Japanese kami-buddha relationship). Separate constraint files model each reading as ε-invariant and link via network.affects_constraints to show family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
