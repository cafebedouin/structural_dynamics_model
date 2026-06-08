% ============================================================================
% CONSTRAINT STORY: meiji_separation_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_meiji_separation_enforcement, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: meiji_separation_enforcement
 *   human_readable: Meiji Separation Enforcement: Shinbutsu-Shugo Dissolution and Shinto Primacy
 *   domain: religious_studies/japanese_history/state_ontology
 *
 * SUMMARY:
 *   The Meiji separation (shinbutsu-bunri) enforced institutional and
 *   theological division between Buddhism and Shinto following the 1868 Meiji
 *   Restoration. For approximately 800 years (Heian through Edo periods),
 *   Japanese religious practice had operated under shinbutsu-shugo
 *   (Shinto-Buddhist syncretism): kami were understood through Buddhist
 *   metaphysical frameworks, Buddhist temples managed Shinto shrines, and
 *   practitioners moved fluidly between Buddhist and Shinto ritual
 *   specialists for life-cycle rituals (births, marriages → Shinto; deaths,
 *   memorials → Buddhism). The separation constraint reversed this
 *   arrangement by state decree, establishing Shinto as the primary religion
 *   of imperial ideology and restricting Buddhism to funerary rites and
 *   ancestor veneration. The constraint exhibits high extractiveness (0.65)
 *   and suppression (0.72) because it benefits the Meiji state's imperial
 *   ideology and Shinto institutional primacy while extracting the
 *   cosmological integrative function that Buddhism had provided for nearly a
 *   millennium. The theater_ratio (0.68) reflects that much of the separation
 *   was performatively maintained — shrines and temples that had operated as
 *   unified ritual complexes continued de facto coordination while performing
 *   compliance with the separation mandate for state inspectors. The
 *   constraint is fundamentally contested at the kernel level: the syncretic
 *   reading interprets pre-Meiji practice as a unified cosmology, making
 *   separation pure dissolution and extraction; the partition reading
 *   interprets pre-Meiji practice as always-separate jurisdictions (life vs
 *   death), making separation merely explicit acknowledgment of existing
 *   boundaries and therefore closer to Rope.
 *
 * KEY AGENTS:
 *   - Meiji State Authority: Primary beneficiary (institutional/arbitrage) — leverages separation to establish Shinto-centered imperial ideology and consolidate power through religious monopoly
 *   - Shinto Establishment: Secondary beneficiary (organized/constrained) — gains state resources and ritual monopoly over life events but becomes constrained by state definitions of authentic Shinto practice
 *   - Buddhist Institution (High Temple Authority): Primary victim (powerful/constrained) — retains organizational structure and death-rite monopoly but loses cosmological role and state patronage; faces high-cost exit from state-defined institutional sphere
 *   - Village Priest (Syncretic Practitioner): Secondary victim (powerless/identity_locked) — identity constituted through dual competence; exit from either tradition requires abandoning professional and spiritual identity
 *   - Local Shrine-Temple Complex: Tertiary victim (moderate/constrained) — forced physical or administrative separation while economic and spiritual interdependence persists; maintains dual practice covertly
 *   - Pre-Meiji Ontological Coherence: Analytical victim — the syncretic cosmology is rendered incoherent through retroactive state reading; practitioners' historical self-understanding is labeled 'confused'
 *   - Analytical Observer: Civilizational perspective — risks naturalizing state-constructed incoherence as logical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(meiji_separation_enforcement, 0.65).
domain_priors:suppression_score(meiji_separation_enforcement, 0.72).
domain_priors:theater_ratio(meiji_separation_enforcement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(meiji_separation_enforcement, extractiveness, 0.65).
narrative_ontology:constraint_metric(meiji_separation_enforcement, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(meiji_separation_enforcement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(meiji_separation_enforcement, tangled_rope).
narrative_ontology:human_readable(meiji_separation_enforcement, "Meiji Separation Enforcement: Shinbutsu-Shugo Dissolution and Shinto Primacy").
narrative_ontology:topic_domain(meiji_separation_enforcement, "religious_studies/japanese_history/state_ontology").

domain_priors:requires_active_enforcement(meiji_separation_enforcement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(meiji_separation_enforcement, 'ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e').
narrative_ontology:cs_kernel_codification('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', formalized).
narrative_ontology:cs_authority_grounding('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', extraction).
narrative_ontology:cs_interpretation_layer_present('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e').
narrative_ontology:cs_reading_relation('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', meiji_separation_enforcement__syncretic_partition_reading, forecloses).
narrative_ontology:cs_axiom('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', foundational, kami_buddha_integrated_metaphysically).
narrative_ontology:cs_axiom_status(kami_buddha_integrated_metaphysically, overridden).
narrative_ontology:cs_axiom_grounding('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', kami_buddha_integrated_metaphysically, deontological).
narrative_ontology:cs_axiom('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', foundational, simultaneous_specialist_practice_coherent).
narrative_ontology:cs_axiom_status(simultaneous_specialist_practice_coherent, overridden).
narrative_ontology:cs_axiom_grounding('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', simultaneous_specialist_practice_coherent, conventional).
narrative_ontology:cs_axiom('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', secondary, emperor_as_one_sacred_figure_among_many).
narrative_ontology:cs_axiom_status(emperor_as_one_sacred_figure_among_many, overridden).
narrative_ontology:cs_axiom_grounding('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', emperor_as_one_sacred_figure_among_many, deontological).
narrative_ontology:cs_reference_frame('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', syncretic_integrated_cosmology).
narrative_ontology:cs_drift_state('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', meiji_separation_enforcement, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('ef9b8e8a-e71f-487d-9b20-5a59d4cf2c6e', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, meiji_state_authority).
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, shinto_institutional_identity).
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, imperial_legitimacy_narrative).
narrative_ontology:constraint_victim(meiji_separation_enforcement, buddhist_institutional_survival).
narrative_ontology:constraint_victim(meiji_separation_enforcement, syncretic_practitioners).
narrative_ontology:constraint_victim(meiji_separation_enforcement, pre_meiji_ontological_coherence).
narrative_ontology:constraint_victim(meiji_separation_enforcement, local_shrine_temple_economies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(meiji_separation_enforcement, shinto_establishment).
narrative_ontology:constraint_victim(meiji_separation_enforcement, buddhist_institution).
narrative_ontology:constraint_victim(meiji_separation_enforcement, village_priest_syncretic_specialist).
narrative_ontology:constraint_victim(meiji_separation_enforcement, local_shrine_temple_complex).
narrative_ontology:constraint_victim(meiji_separation_enforcement, syncretic_practitioner_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Meiji state establishes and enforces the separation of Buddhism and Shinto as a core modernization and imperial legitimacy project. The state controls shrine registration, temple licensing, priest certification, and religious education curricula. The state sets what counts as authentic Shinto practice and restricts Buddhism to funerary rites. The state collects no direct revenue from religions but extracts legitimacy through imperial-Shinto fusion and ideological unification. If the state abandoned separation enforcement, it would lose the religious monopoly over imperial ideology but would gain administrative simplification.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, meiji_state_authority, agenda_setter,
    institutional, immediate, arbitrage, national).

% The Shinto establishment (shrine networks, Shinto sectarian organizations, state-sponsored Shinto theology) gains institutional primacy, state resources, and monopoly over life-cycle rituals (births, coming-of-age, marriages, kami worship). Shinto benefits from the separation's state enforcement. However, Shinto is also constrained by state definition of authentic practice — local shrine practices that deviate from approved theology face suppression. Shinto's exit is constrained: breaking with the state's separation policy would forfeit resource access and primacy, but accepting state oversight limits independent institutional development.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, shinto_establishment, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(meiji_separation_enforcement, shinto_establishment, agenda_setter).

% The Buddhist institution (temple networks, sectarian hierarchies, monastic orders) bears the primary institutional cost of separation. Buddhism loses state patronage (which had supported major temples), loses authority over life-cycle rituals and kami interpretation, and faces institutional consolidation (ca. 200,000 temples reduced to 70,000 by 1900 through closures and mergers). Buddhism retains monopoly over funerary rites, which becomes its primary revenue stream and organizational anchor. Buddhism cannot exit without abandoning its institutional structure entirely. Buddhist leadership navigates pressure to declare 'Shinto-compatible theology' (accommodation to the separation) vs defending pre-Meiji cosmology (which brings state suppression).
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, buddhist_institution, payer,
    powerful, generational, constrained, national).

% The village priest trained in pre-Meiji syncretic practice faces forced identity choice: identify as Buddhist or Shinto, abandon dual competence, retrain in one tradition. The priest's professional identity, spiritual authority in the community, and decades of ritual training are constituted through syncretic practice — simultaneous service as both Buddhist priest and Shinto specialist. Exit options are structurally available (could theoretically become pure Buddhist or pure Shinto practitioner) but are blocked by identity fusion: choosing either tradition means becoming a different person, abandoning the integrated identity that made them a respected authority. Many priests face economic collapse (temples close or consolidate) alongside identity erasure. Some priests navigate by maintaining covert dual practice while performing separation compliance.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, village_priest_syncretic_specialist, payer,
    powerless, biographical, identity_locked, local).

% The local shrine-temple compound (common in Edo-period Japan, often physically integrated or closely proximate) faces forced administrative and sometimes physical separation. Many compounds had operated for centuries as unified ritual spaces under shared economic arrangements and overlapping priesthoods. Separation requires physical division, separate staffing, separate treasuries, and public declaration of institutional allegiance. However, economic and spiritual interdependence often persists informally — shared seasonal festivals, cross-referral of practitioners, shared maintenance of ritual spaces. The constraint is maintained partly through performance: official separation masks de facto continued coordination. Communities with strong syncretic identity sometimes resist through covert dual practice; others gradually internalize separation as normality.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, local_shrine_temple_complex, payer,
    moderate, generational, constrained, regional).

% The population of Japan's villages and towns practiced simultaneous Buddhism and Shinto for centuries — attending shrine festivals, consulting Shinto kami specialists for life problems, employing Buddhist priests for funerary rites and ancestor veneration, seeking merit through Buddhist practice and kami blessing through Shinto practice. The separation constraint forces these practitioners to declare exclusive allegiance or navigate covertly. Practitioners lose access to the integrated cosmology that made sense of their ritual life. Many become trapped between institutional demands (declare yourself Buddhist OR Shinto) and lived practice (continue dual engagement). Exit options are trapped: practitioners cannot move to a jurisdiction with syncretic practice; cannot recombine traditions without state sanctions.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, syncretic_practitioner_population, payer,
    powerless, biographical, trapped, local).

% The pre-Meiji syncretic cosmology is not an agent but a configuration of meaning-making that 800 years of Japanese practitioners inhabited. The separation constraint retroactively labels this configuration 'incoherent,' rendering it inadmissible as a legitimate tradition. Practitioners lose standing to claim their integrated religious identity as valid. The constraint excludes the syncretic reading from legitimate discourse — practitioners cannot argue that their simultaneous practice was coherent without being labeled 'confused' by state-sponsored ideology.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, pre_meiji_coherence, excluded,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(meiji_separation_enforcement, pre_meiji_coherence).

% The analytical observer from a civilizational perspective risks naturalizing the state-constructed incoherence reading as logical necessity. The observer may see pre-Meiji syncretism as ontologically confused (separate jurisdictions cannot inhabit unified cosmology) and therefore the separation as clarification of inevitable truth. This naturalizing move obscures that (1) practitioners experienced the arrangement as coherent for 800 years, (2) the 'incoherence' framing was authored by Meiji ideologues to justify suppression, and (3) coherence is a constructed property, not a logical law. The observer's identity lock is methodological: treating what is socially constructed as if it were natural law.
narrative_ontology:constraint_stakeholder(meiji_separation_enforcement, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The separation constraint coordinates religious identity and institutional authority at the national scale. It solves the state's coordination problem of unifying religious institutions under a coherent imperial ideology centered on Shinto primacy. Pre-Meiji syncretism left religious authority distributed across Buddhist-Shinto specialists with no clear priority; the separation establishes Shinto as supreme for life events and imperial legitimacy, leaving Buddhism with death and ancestor commemoration. This reorganization enables the state to claim Shinto primacy while retaining Buddhist institutional infrastructure for death rites.
% TRANSFER_FUNCTION: The separation transfers cosmological authority from Buddhism (which had provided integrative metaphysics unifying kami and Buddha) to Shinto (now positioned as pure, authentic, and imperial). It transfers institutional resources from Buddhist temples (ca. 200,000 temples consolidated to 70,000) to Shinto shrines (state-sponsored, ideologically privileged). It transfers ritual monopolies: Shinto gains life-cycle rituals (births, marriages, kami veneration); Buddhism retains death and ancestor rites. It transfers cultural authority: the syncretic cosmology is labeled incoherent; the partition reading is labeled as natural and discovered. It transfers identity constraints: practitioners and priests are forced to choose singular allegiance, losing the integrated identity that had made syncretic practice meaningful.
% ABSENT_VOICES: The syncretic practitioners themselves are partly excluded from the conversation — their lived experience of coherence is dismissed as confusion. Pre-Meiji Buddhist-Shinto theological texts and their authors are excluded — their arguments for cosmological integration are reread as evidence of confusion rather than as legitimate frameworks. Regional shrine-temple complexes maintaining covert dual practice are excluded — their continued coordination is hidden to preserve the performance of separation. Younger generations trained exclusively in separation (post-1880s cohort) are not yet in the room — their identity lock is being constructed as they are trained to see pre-Meiji syncretism as 'how things were confused before.' By the Taisho period, this newer generation is fully inside the conversation, but they have internalized the separation as natural.
% DISAPPEARANCE_RATIONALE: The state and Shinto establishment argue: if separation enforcement disappeared, the world would rearrange itself back to syncretic chaos — practitioners would revert to dual practice, Shinto's imperial primacy would be lost, the modernization project would unravel. From this perspective, the arrangement is load-bearing. Conversely, practitioners and covert dual-practice communities argue: if separation enforcement disappeared, the world would not rearrange much — informal dual practice would continue, shrine-temple economics would reintegrate, practitioners would reclaim their traditional integrated identity. From this perspective, the constraint is performatively maintained against underlying disposition to syncretism. The verdict is contested because the answer depends on which identity frames have been internalized by the second and third generations of separation-era practitioners.
% FOUNDING_PROBLEM: The founding problem: Japan's modernization required ideological unification around imperial authority, and Buddhist cosmology (which treated the emperor as one figure among many in a complex metaphysical hierarchy) did not provide sufficient primacy for imperial legitimation under Western-influenced nation-state frameworks. Shinto provided pure, undisputed, ancient lineage for the emperor (Shinto texts place the emperor as direct descendent of the sun goddess Amaterasu). Syncretic Buddhism, by integrating kami within Buddhist metaphysics, equivocated the emperor's status — the emperor was sacred but not uniquely so in the integrated framework. Separation of Buddhism from Shinto solved the founding problem by eliminating Buddhist authority over the interpretation of the kami and emperor, leaving Shinto as the sole arbiter of imperial sacrality.
% FOUNDING_PROBLEM_CORROBORATION: Multiple sources outside the Shinto establishment testify that the founding problem was real and pressing: (1) Meiji statesmen's own writings (Iwakura Tomomi, Yamagata Aritomo) document the perceived need for ideological unification and imperial-centered modernization. (2) Western diplomatic records note Japanese statesmen's explicit project of creating a unified imperial ideology as a modernization requirement. (3) Contemporary Buddhist critics acknowledged that syncretism had weakened Shinto's exclusivity and imperial primacy. However, by the Showa period (1926+), the founding problem's urgency has degraded — Japan has achieved ideological unification, the emperor's sacrality is firmly established, and the Meiji modernization project is complete. The constraint persists not because the founding problem still drives it but because institutional actors have internalized separation as normal and the constraint is now maintained through performance and identity lock rather than through continued urgency of the original mandate.
narrative_ontology:disappearance_verdict(meiji_separation_enforcement, contested).
narrative_ontology:founding_problem_status(meiji_separation_enforcement, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE PRIEST (SNARE) — Trapped by identity fusion with pre-Meiji syncretic practice. Structural mobility exists (could theoretically switch allegiance to pure Shinto or pure Buddhism) but the priest's professional and spiritual identity is constituted through simultaneous practice. Exit would require becoming a different person — abandoning decades of training in dual ritual competence and the community role built on that synthesis. Suppression is dual: external (state sanctions, career loss, temple closures) and internalized (the priest's identity cannot see the constraint as changeable from within the syncretic frame).
constraint_indexing:constraint_classification(meiji_separation_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: BUDDHIST INSTITUTION (TANGLED ROPE) — Powerful institutional actor facing high-cost exit. Buddhism provided the Meiji state with organizational infrastructure, ritual legitimacy, and literacy/administration during the Edo period. The separation constraint simultaneously benefits Buddhist survival (guarantees a protected institutional sphere for funeral rites, which becomes Buddhism's primary extracted revenue stream) and extracts its primary integrative function (the cosmological role Buddhism played in syncretism). The institution experiences mixed extraction and coordination: constrained by state definition of its permitted domain, but also stabilized within it. Enforced institutionally through registration mandates and shrine-temple boundaries.
constraint_indexing:constraint_classification(meiji_separation_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEIJI STATE AUTHORITY (ROPE) — Primary beneficiary experiencing the constraint as coordination. The separation enforces Shinto primacy, anchors imperial authority in pure Shinto lineage mythology, and eliminates the Buddhist institutional competitor for state patronage. Beneficiaries have arbitrage options: the state can shift patronage to Shinto without fundamental structural change. The constraint solves a genuine coordination problem from the state's perspective: unifying an ideologically fractured population around a Shinto-centered imperial identity. Effective extraction runs toward the state, but state actors perceive this as legitimate coordination rather than extraction.
constraint_indexing:constraint_classification(meiji_separation_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SHINTO ESTABLISHMENT (TANGLED ROPE) — Organized actors newly empowered by the separation constraint. Shinto temples gain monopoly over life-cycle rituals (births, kami veneration, imperial rites) while Buddhist institutions retain only death rites. This coordination is asymmetric: Shinto benefits from state resources and ideological priority, while being constrained by state oversight and the burden of manufacturing a unified 'pure Shinto' theology from historically heterogeneous local practices. Active enforcement maintains the boundary; state suppression targets Buddhist expansion. Shinto experiences this as beneficial but also constrained by the state's capacity to define what counts as 'true' Shinto practice.
constraint_indexing:constraint_classification(meiji_separation_enforcement, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LOCAL SHRINE-TEMPLE COMPLEX (PITON) — Many local sites were forced to physically separate or affiliate exclusively with one tradition, but the actual spiritual and economic interdependence persisted theatrically. Shrine-temple compounds that had operated as unified ritual spaces for centuries maintained informal coordination (shared seasonal festivals, cross-referral of practitioners) while performing rigid institutional separation for state census-takers and inspection officials. Theater_ratio is high: the constraint is maintained through performance of compliance rather than genuine functional dissolution. Many communities maintained dual practice covertly while publicly affirming separation.
constraint_indexing:constraint_classification(meiji_separation_enforcement, piton,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ONTOLOGICAL NATURALIZATION (MOUNTAIN) — From a civilizational perspective, one reading treats the separation as inevitable consequence of a fundamental ontological incoherence: the syncretic reading holds that Buddhist and Shinto practitioners shared one cosmology, but the partition reading shows they always occupied separate jurisdictions (life vs death), so the appearance of 'syncretism' was never unified but merely pragmatic. Under this view, the Meiji separation simply made explicit what was always structurally true — you cannot have a single cosmology that governs contradictory jurisdictions. However, the structural data reveals this as a false summit: the 'incoherence' was a constructed reading retrospectively applied to justify state-enforced dissolution. For 800+ years, Japanese practitioners experienced the arrangement as coherent. The naturalization (treating historical contingency as logical necessity) is the analytical observer's own identity lock.
constraint_indexing:constraint_classification(meiji_separation_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meiji_separation_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(meiji_separation_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meiji_separation_enforcement, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(meiji_separation_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(meiji_separation_enforcement, TR),
    TR >= 0.70.

:- end_tests(meiji_separation_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65): Moderate-high. The separation extracts Buddhism's primary integrative function — the cosmological role it played in syncretism for 800 years. Buddhist temples retain economic function through funerary rites (which become their exclusive domain and primary revenue stream) but lose the broader spiritual authority that syncretism provided. The Meiji state extracts imperial legitimacy and ideological unification by establishing pure Shinto primacy. For village practitioners, the extraction is severe: they must abandon syncretic identity or risk occupation loss and community sanctions. The value reflects that some coordination function persists (Buddhism does provide organized funerary services, Shinto does organize community life rituals) but substantial extraction runs from Buddhism and practitioners toward the state and Shinto establishment. Suppression (0.72): High. Active enforcement includes temple closures (ca. 200,000 temples consolidated to ca. 70,000 by 1900), state prohibition of dual practice, mandatory shrine registration, defrock orders for priests who refused separation, Buddhist anti-government persecution, and state control of what counts as acceptable Shinto practice. However, suppression is not total — significant regions maintained covert dual practice, and by the 1910s-1920s, enforcement relaxed as normalization occurred. Theater ratio (0.68): Moderate-high. The separation persists partly through genuine institutional reorganization but substantially through performative compliance. Shrine-temple compounds maintained economic interdependence and informal ritual coordination while performing rigid separation for census and inspection officials. Many communities did not install physical separation — instead maintained administrative fiction of separation while practitioners continued dual practice. The theater increases over the interval (0.35 → 0.72) as the initial enforcement effort (high active suppression, low theater) transitions to normalized separation (lower suppression requirement, high theater as performance replaces active coercion). By 1930, most actors have internalized the separation's identity frames; continued performance masks that the constraint is maintained more through established practice than through active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap lies between the beneficiary's experience (Rope: 'we solved the imperial unification problem') and the victim's experience (Snare: 'we lost our cosmological framework'). The gap widens when you recognize that both are describing the same constraint from incommensurable positions. The state coordinated religious identity through suppression; Buddhism experienced the coordination as extraction and boundary-setting; village practitioners experienced it as identity amputation. The analytical observer's Mountain perspective is the most dangerous gap: by naturalizing the separation as inevitable consequence of ontological incoherence, the analytical view obscures that coherence was historically achieved and maintained by practitioners for 800 years, and 'incoherence' is a constructed reading retrospectively applied to justify state violence. This is the oracle gap instantiated: the analyst's native instruments (treating the constraint as natural limit) cannot detect the institutional construction that cross-position analysis reveals (the state created the 'incoherence' reading to justify enforcement). The gap is partially resolvable through the kernel reading approach: once you identify the contested kernel (what does it mean to have 'one cosmology' that governs separate jurisdictions?), you can model the Meiji separation as state-enforced reinterpretation of the kernel rather than as enforcement of natural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary by agent's structural position. The Meiji state (beneficiary, institutional power, arbitrage exit) experiences low d → negative χ (subsidy flows toward them). The Shinto establishment (partial beneficiary, organized power, constrained exit) experiences moderate d → moderate χ (mixed extraction and benefit). The Buddhist institution (victim, powerful but constrained exit, high cost to reorganize) experiences high d → high χ (extraction concentrated here). The village priest (victim, powerless, identity_locked exit preventing any real exit option despite theoretical mobility) experiences maximum d → maximum χ (full target, unable to escape despite structural capacity for exit because identity frame makes departure unthinkable). The engine's derivation chain computes these automatically from the beneficiary/victim declarations and exit option data: victims with no exit → high d; beneficiaries with arbitrage → low d; identity-locked agents trapped in their own frame despite external mobility → high d (constrained exit applies because, while identity is the binding force, the psychological lock is strengthened by external costs). No directionality overrides are needed — the structural data captures the relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's original mandate was to unify and modernize Japan by establishing a coherent national religious identity centered on Shinto and the emperor. By the Meiji period's end (ca. 1912), this mandate was functionally achieved — the state had reorganized religious institutions, established Shinto monopoly over imperial legitimacy, and created a functioning institutional sphere for both Shinto and Buddhism. However, the constraint persists long past mandate fulfillment, now maintained by institutional inertia and performance rather than active enforcement. The theater_ratio trajectory (0.35 → 0.72) traces this: initial separation (high suppression, low theater, mandate-driven) transitions to normalized separation (low suppression, high theater, maintained through performative compliance). By the 1930s, the constraint exists not because the modernization mandate still drives it but because institutional actors have internalized separation as 'normal.' The constraint resolves mandatrophy by showing that fulfillment of the original mandate did not dissolve the constraint — the constraint persists as institutional inertia and reified identity categories long after the urgent mandate expires. This is precisely the Piton pattern: a former Tangled Rope (coordination function + extraction) degrades into Piton (extraction mechanism atrophies but theatrical maintenance continues). The village priest's identity lock (originally externally enforced suppression combined with identity fusion during the separation period) becomes the mechanism that naturalizes the boundary as internal truth rather than external imposition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    syncretic_coherence_ontology,
    'Was shinbutsu-shugo a genuinely unified cosmological framework or a pragmatic partition that only appeared unified through centuries of simultaneous practice?',
    'Textual analysis of pre-Meiji theological writings; ethnographic reconstruction of practitioner lived experience; analysis of shrine-temple economic and ritual interdependence; comparison with other religious syncretism cases (Afro-Cuban, Haitian Vodou, Tibetan Buddhism-Bon) to establish cross-cultural syncretism coherence criteria',
    'If unified: the separation is pure extraction (Snare) — dissolution of a coherent system for state benefit. If pragmatic partition: the separation enforces honesty about always-separate domains (Rope) — clarifying rather than destroying. If contested: the reading itself becomes a kernel reading (this story) vs alternative interpretation reading (separate story, network-linked).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(syncretic_coherence_ontology, conceptual, 'Whether shinbutsu-shugo was unified cosmology or pragmatic partition').

omega_variable(
    state_mandate_vs_popular_practice,
    'To what extent did the Meiji separation enforce genuine institutional dissolution vs theatrical compliance masking persistent dual practice?',
    'Comparative analysis of shrine-temple economy data pre/post separation; archival records of covert dual practice; ethnographic interviews with practitioners in regions of high dual-practice continuity; analysis of shrine-temple compound architecture and its modifications during separation enforcement period (1868-1880)',
    'If genuine dissolution: extraction rate is lower (constraint actually reorganizes practice). If theatrical: extraction rate is higher (constraint persists through performative compliance while suppressing actual practice). Theater_ratio reflects this uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_mandate_vs_popular_practice, empirical, 'Extent of genuine institutional dissolution vs theatrical compliance').

omega_variable(
    identity_lock_mechanism_duration,
    'Did the identity lock of village priests shift from enforced (external suppression) to internalized (identification with pure Shinto or pure Buddhism) across the Meiji period?',
    'Generational analysis of priest training curricula; conversion narratives and institutional defection records; comparison of early-Meiji priests (trained under syncretism) vs Taisho-period priests (trained under separation); measurement of covert dual practice persistence in regions with vs without strong syncretic institutional continuity',
    'If lock remained external: the constraint requires continuous enforcement (Tangled Rope stability). If lock became internalized: the constraint may eventually degrade to Piton status as the generation trained in separation normalizes (institutional inertia replaces active suppression). This tracks the theater_ratio trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_duration, empirical, 'Shift from external suppression to internalized identity lock in priests').

omega_variable(
    false_summit_naturalization,
    'Is the reading of pre-Meiji syncretism as ''ontologically incoherent'' a description of actual historical contradiction or a retrospective rationalization constructed by state ideology to justify separation?',
    'Genealogy of the ''incoherence'' framing — who authored it, when, under what institutional authority. Comparison with contemporary Japanese theological writings (1600-1850) to assess whether coherence was explicitly claimed or pragmatically assumed. Analysis of how Meiji intellectuals re-read Edo-period texts to find incoherence in texts that do not claim it.',
    'If naturalization: the mountain perspective is a false summit (Tangled Rope or Snare computed; constraint reclassifies). If genuine incoherence: the separation enforces ontological clarity (Rope or Scaffold, lower extraction). This determines whether the analytical observer''s perspective should reclassify via FSM.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Whether ''ontological incoherence'' is historical fact or retrospective rationalization').

omega_variable(
    kernel_reading_status,
    'Does the separation constraint instantiate one reading of a single contested kernel (syncretism as unified vs partitioned), or does the attempt to force unification create a genuinely new constraint distinct from pre-Meiji practice?',
    'Identification of the kernel: what is the persisting commitment being read differently? Candidates: (1) the cosmology governing life-cycle and death rituals (reading 1: unified; reading 2: always separate); (2) the nature of the imperial authority (reading 1: sacred-imperial synthesis; reading 2: pure-Shinto imperial authority); (3) the ontological status of the kami (reading 1: compatible with Buddhist metaphysics; reading 2: incommensurable with Buddhist views). Once kernel is identified, determine whether the separation enforces a reading choice or dissolves the kernel entirely.',
    'If kernel reading: this story describes state-enforced reinterpretation (commitment system dynamics; cs_structure applies). If new constraint: separation is pure institutional creation with no inherited kernel; cs_structure omitted. This determines whether cs_structure block is populated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'Whether separation is kernel reading or new institutional creation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(meiji_separation_enforcement, 1868, 1930).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meiji_sep_tr_t1868, meiji_separation_enforcement, theater_ratio, 1868, 0.35).
narrative_ontology:measurement(meiji_sep_tr_t1875, meiji_separation_enforcement, theater_ratio, 1875, 0.52).
narrative_ontology:measurement(meiji_sep_tr_t1885, meiji_separation_enforcement, theater_ratio, 1885, 0.68).
narrative_ontology:measurement(meiji_sep_tr_t1910, meiji_separation_enforcement, theater_ratio, 1910, 0.72).
narrative_ontology:measurement(meiji_sep_tr_t1930, meiji_separation_enforcement, theater_ratio, 1930, 0.65).

% Extraction over time
narrative_ontology:measurement(meiji_sep_be_t1868, meiji_separation_enforcement, base_extractiveness, 1868, 0.45).
narrative_ontology:measurement(meiji_sep_be_t1875, meiji_separation_enforcement, base_extractiveness, 1875, 0.58).
narrative_ontology:measurement(meiji_sep_be_t1885, meiji_separation_enforcement, base_extractiveness, 1885, 0.65).
narrative_ontology:measurement(meiji_sep_be_t1910, meiji_separation_enforcement, base_extractiveness, 1910, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(meiji_sep_su_t1868, meiji_separation_enforcement, suppression_requirement, 1868, 0.85).
narrative_ontology:measurement(meiji_sep_su_t1875, meiji_separation_enforcement, suppression_requirement, 1875, 0.78).
narrative_ontology:measurement(meiji_sep_su_t1885, meiji_separation_enforcement, suppression_requirement, 1885, 0.72).
narrative_ontology:measurement(meiji_sep_su_t1910, meiji_separation_enforcement, suppression_requirement, 1910, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(meiji_separation_enforcement, identity_coordination).
narrative_ontology:affects_constraint(meiji_separation_enforcement, meiji_state_legitimacy_shinto_primacy).
narrative_ontology:affects_constraint(meiji_separation_enforcement, buddhist_institutional_survival_funerary_monopoly).
narrative_ontology:affects_constraint(meiji_separation_enforcement, syncretic_practitioner_identity_fusion).

% DUAL FORMULATION NOTE:
% The Meiji separation should be decomposed into (at minimum) two linked constraint stories: (1) the state-enforced institutional reorganization (this story, focusing on the extraction mechanism and enforcement apparatus), and (2) the ontological kernel reading contest (a separate story modeling the syncretic vs partition readings as sibling interpretations of pre-Meiji practice, linked via network.affects_constraints). This decomposition respects ε-invariance: the institutional reorganization has one ε (0.65), while the ontological reading contest has a different ε reflecting the contested naturality of the syncretic vs partition framings. The two stories are structurally related but epistemically distinct.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
