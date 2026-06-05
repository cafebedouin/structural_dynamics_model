% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__gandhian_allegorical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__gandhian_allegorical_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__gandhian_allegorical_reading
 *   human_readable: Kurukshetra as Internal Struggle: Gandhian Allegorical Reading
 *   domain: religious_studies/textual_hermeneutics/ethical_philosophy
 *
 * SUMMARY:
 *   The Gandhian allegorical reading of the Kurukshetra discourse repudiates
 *   the literal interpretation of warfare as a divinely sanctioned duty and
 *   recasts the battlefield as a metaphor for internal moral struggle. In
 *   this reading, Krishna's counsel to Arjuna is not permission for righteous
 *   violence but an invitation to engage the deepest inner conflict—the
 *   struggle between dharma and ahimsa (duty and non-violence), between ego
 *   and truth, between attachment and detachment. This reading elevates
 *   ahimsa (non-violence) as the supreme ethical principle, delegitimizes the
 *   caste hierarchy that the orthodox reading claims the Gita sanctions, and
 *   transfers interpretive authority from Brahminical scholars to the
 *   individual's moral conscience. The reading emerged as a dominant force in
 *   Hindu thought through Gandhi's reinterpretation during the Indian
 *   independence struggle, where the Gita was simultaneously the tradition's
 *   most authoritative text and the text most weaponized to justify caste
 *   hierarchy and acceptance of oppression. The Gandhian reading resolves
 *   this paradox by claiming the Gita always taught non-violence and
 *   individual moral authority—it just required the right interpretation to
 *   recover that meaning. The constraint classified here is not 'the Gita' as
 *   an object but the interpretive framework: the specific hermeneutic lens
 *   that treats the Kurukshetra discourse as allegorical rather than literal,
 *   and that hierarchy/violence as internal rather than external phenomena.
 *   This reading has real structural consequences: it legitimizes anti-caste
 *   movements, provides textual authority for non-violence campaigns, and
 *   reorients Hindu institutional identity toward alignment with modern
 *   ethical frameworks. But it also extracts a cost: it imposes a demanding
 *   standard of inner work on individuals, it can obscure material structures
 *   of oppression by psychologizing them as internal struggles, and it
 *   constrains institutions that must navigate between traditional authority
 *   and modern ethics. The constraint exhibits tangled_rope characteristics:
 *   genuine coordination function (enabling anti-caste and non-violence
 *   movements), asymmetric extraction (some groups benefit more than others),
 *   and active enforcement (institutional adoption, pedagogical mandates).
 *
 * KEY AGENTS:
 *   - Individual Moral Conscience: Primary beneficiary (organized/mobile) — liberated from caste-based dharma prescription; elevated as ultimate arbiter of ethical action
 *   - Anti-Caste and Non-Violence Movements: Primary beneficiary (organized/mobile) — gain textual authority for collective action; enable coalition-building across caste boundaries
 *   - Subjects of Structural Caste Violence: Primary victim (powerless/trapped) — material extraction persists despite interpretive reframing; liberation lags behind reinterpretation
 *   - Brahminical Interpretive Authority: Secondary victim (institutional/constrained) — loses monopoly on textual interpretation; authority is challenged but not eliminated
 *   - Hindu Institutions: Secondary actor (institutional/constrained) — must reinterpret tradition to maintain relevance; extraction of caste-legitimacy function, coordination on coherence
 *   - Academic Textual Structures: Tertiary actor (institutional/arbitrage) — maintain the reading through pedagogy and scholarship; theater persists even as living interpretive function attenuates
 *   - Analytical Observer: Perspectival actor (analytical/analytical) — risks naturalizing a contingent interpretive choice as immutable textual meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.52).
domain_priors:suppression_score(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.65).
domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__gandhian_allegorical_reading, "Kurukshetra as Internal Struggle: Gandhian Allegorical Reading").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__gandhian_allegorical_reading, "religious_studies/textual_hermeneutics/ethical_philosophy").

domain_priors:requires_active_enforcement(gita_kurukshetra_discourse__gandhian_allegorical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__gandhian_allegorical_reading, '46b20042-4163-49cd-a513-35bc65ad6110').
narrative_ontology:cs_kernel_codification('46b20042-4163-49cd-a513-35bc65ad6110', fixed_text).
narrative_ontology:cs_authority_grounding('46b20042-4163-49cd-a513-35bc65ad6110', lineage).
narrative_ontology:cs_interpretation_layer_present('46b20042-4163-49cd-a513-35bc65ad6110').
narrative_ontology:cs_reading_relation('46b20042-4163-49cd-a513-35bc65ad6110', gita_kurukshetra_discourse__orthodox_literal_reading, forecloses).
narrative_ontology:cs_reading_relation('46b20042-4163-49cd-a513-35bc65ad6110', gita_kurukshetra_discourse__universalist_devotional_reading, influences).
narrative_ontology:cs_axiom('46b20042-4163-49cd-a513-35bc65ad6110', foundational, ahimsa_supremacy_principle).
narrative_ontology:cs_axiom_status(ahimsa_supremacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('46b20042-4163-49cd-a513-35bc65ad6110', ahimsa_supremacy_principle, deontological).
narrative_ontology:cs_axiom('46b20042-4163-49cd-a513-35bc65ad6110', foundational, individual_conscience_hermeneutic_authority).
narrative_ontology:cs_axiom_status(individual_conscience_hermeneutic_authority, holdable).
narrative_ontology:cs_axiom_grounding('46b20042-4163-49cd-a513-35bc65ad6110', individual_conscience_hermeneutic_authority, deontological).
narrative_ontology:cs_reference_frame('46b20042-4163-49cd-a513-35bc65ad6110', allegorical_non_violence_framework).
narrative_ontology:cs_drift_state('46b20042-4163-49cd-a513-35bc65ad6110', contemporary_institutional_adoption, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('46b20042-4163-49cd-a513-35bc65ad6110', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, individual_moral_conscience).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, ahimsa_practitioners).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__gandhian_allegorical_reading, anti_caste_movements).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, literal_violence_perpetrators).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, brahminical_interpretive_authority).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__gandhian_allegorical_reading, caste_hierarchy_legitimacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBJECTS OF STRUCTURAL CASTE VIOLENCE (SNARE) — Communities subjected to the literal caste hierarchy and structural oppression cannot escape the system that the orthodox reading legitimates. This reading repudiates that legitimacy, but the material extraction persists. The constraint is the interpretive framework that either sanctions or delegitimizes structural violence. Trapped agents experience full extraction from the orthodox reading; this reading attempts to invert the framework but material liberation lags interpretive reframing.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL MORAL CONSCIENCE (TANGLED ROPE) — The Gandhian reading liberates the individual from institutional dharma interpretation, elevating personal moral conscience as the ultimate arbiter. This provides genuine coordination value: each person can align action with internal truth without caste-based prescription. But the reading also extracts a cost — the individual bears full responsibility for discerning dharma through ahimsa, and is constrained by social sanctions from rejecting the larger institutional framework. Constrained exit reflects real barriers to living ahimsa in a violent society.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ANTI-CASTE AND NON-VIOLENCE MOVEMENTS (ROPE) — Organized movements (Dalit liberation, Gandhian satyagraha networks) experience this reading as pure coordination: the allegorical interpretation provides a unifying framework for collective action. The reading enables coalition-building across caste boundaries by rooting anti-caste ethics in textual authority (the Gita's elevation of ahimsa and individual conscience). Mobile exit reflects that movements can adopt this frame and mobilize around it; low extraction because the reading serves the movement's structural interests.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: HINDU INSTITUTIONS AND BRAHMINICAL SCHOLARS (TANGLED ROPE) — Traditional Brahminical interpretive authority (pundits, temple institutions, caste-hierarchy maintainers) experience the Gandhian reading as an attack on their legitimacy. But many institutional actors also benefit from the reading's coordination function: it keeps Hinduism coherent with modern ethics, prevents wholesale rejection of the tradition, and provides a bridge to liberal democratic values. Constrained exit reflects that institutions cannot simply abandon the Gita's authority, but must reinterpret it. The reading simultaneously extracts from and coordinates with institutional actors — extraction of the caste-legitimacy function, coordination on textual coherence.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: UNIVERSALIST ETHICAL MOVEMENTS (SCAFFOLD) — Global non-violence and human rights movements adopt the Gandhian reading as a temporary interpretive bridge: it elevates universal principles (ahimsa, individual conscience) above tradition-specific dharma. This perspective sees the allegorical reading as scaffolding toward a post-textual, post-traditional ethics. Low extraction because the movement has agency and a clear exit path: once universal ethics are established institutionally, the Gita's authority becomes optional. The reading's sunset clause is implicit — as global human rights frameworks mature, fewer movements will need to root non-violence in Hindu textual authority.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ACADEMIC TEXTUAL AUTHORITY STRUCTURES (PITON) — Scholarly institutions and academic departments maintain the Gandhian reading through syllabi, canonical texts, and historical narratives (Gandhi as modernizer, Hinduism as amenable to non-violence). But the actual interpretive function has largely atrophied: the reading persists more as a historical artifact and marker of institutional respectability than as a living framework for resolving new ethical questions. The theater ratio is high — academic legitimacy is maintained through repeated citation and ceremonial status, not through active reinterpretation of Gita verses for contemporary dilemmas.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a civilizational/universal perspective, textual interpretation is an immutable hermeneutic necessity: any sacred text admits multiple readings because meaning is always interpreter-dependent. This reading sees the Gandhian allegorical interpretation as simply 'what the Gita always meant' — a natural property of textual authority. However, the structural data contradicts this mountain classification: the reading has identifiable beneficiaries (anti-caste movements, individual conscience), identifiable victims (caste-hierarchy legitimacy), and active enforcement mechanisms (institutional adoption, pedagogical mandates). The engine will detect this as a false summit.
constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra_discourse__gandhian_allegorical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gita_kurukshetra_discourse__gandhian_allegorical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gita_kurukshetra_discourse__gandhian_allegorical_reading, TR),
    TR >= 0.70.

:- end_tests(gita_kurukshetra_discourse__gandhian_allegorical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading provides genuine liberation from caste-based dharma prescription and enables anti-caste mobilization, but it also extracts a cost by elevating individual conscience as arbiter—each person bears full responsibility for discerning dharma through ahimsa, with no institutional guidance. The extractiveness is not as high as pure snare (0.72+) because the reading genuinely coordinates anti-caste action and provides textual legitimacy for non-violence. It is not as low as pure rope (0.35) because institutional actors must enforce the reinterpretation, and individual conscience is a demanding standard that can be weaponized to blame victims for failing to achieve inner transformation while structural oppression persists. Suppression (0.65): High. The reading is not suppressed by physical coercion but by institutional gatekeeping (who gets to interpret the Gita?), educational barriers (access to philosophical training required to adopt the reading), social sanctions (communities can ostracize those who live by ahimsa), and the sheer difficulty of actualizing non-violence in a violent social context. Suppression increased over the measurement interval as the reading became institutionalized (academic adoption raised the bar for 'legitimate' interpretation) and as anti-caste movements faced violent backlash. Theater ratio (0.58): Moderate-high. The reading has increased in theater over time as academic institutions adopted it—the Gita's authority became a pedagogical performance (assigning Gandhi's interpretation in university courses) rather than an active hermeneutic process (communities engaged in live reinterpretation for contemporary dilemmas). The initial theater ratio (0.35) reflected moments of genuine interpretive work (Gandhi's own struggles to articulate the reading); the endpoint (0.58) reflects its institutionalization as canonical doctrine. The reading's claimed type is tangled_rope: it coordinates anti-caste action and provides textual authority for non-violence (rope function) while extracting costs through the demand for individual conscience-based dharma discernment and through institutional enforcement of the 'correct' interpretation (tangled function).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival divergence between interpretive authority and those subjected to structural violence. Anti-caste and non-violence movements see a liberatory reading (Rope); Brahminical institutions and academic structures see a threat to authority (constrained Tangled Rope). Powerless subjects of caste hierarchy experience the reading as insufficient—interpretive reframing does not address material oppression (Snare). Individual conscience-seekers experience genuine liberation from institutional prescription (Tangled Rope). Organized movements experience pure coordination (Rope). Academic institutions experience attenuated authority (Piton). The analytical observer risks seeing the reading as a natural property of the Gita's meaning (Mountain) when it is actually a contingent, politically motivated reinterpretation. The perspectival gap reveals that 'the correct reading of the Gita' is not a hermeneutic question with a single answer—it is a structural question about who benefits from which interpretation and what political work the reading does.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by whether the agent is a beneficiary or victim of the interpretive framework. Anti-caste movements benefit from the allegorical reading (d ≈ 0.15, low extraction experienced); Brahminical scholars benefit from the orthodox literal reading but are victimized by this reading (d ≈ 0.70, moderate-high extraction experienced). Trapped subjects of caste hierarchy are nominally beneficiaries of the reading (it repudiates caste legitimacy) but experientially remain victims because material structures persist (d ≈ 0.85, high extraction experienced—the reading fails to liberate them). The engine's derivation from beneficiary/victim + exit options produces these d values; the sigmoid f(d) then scales them into the chi formula. The key insight is that being nominally benefited by an interpretive reframing does not equal being freed from structural extraction if the material conditions remain intact.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the ambiguity between (a) recovering what the Gita 'always meant' via reinterpretation and (b) imposing a modern ethical framework onto an ancient text. If the reading recovers latent meaning (a), it has stronger hermeneutic legitimacy and the extraction is justified as liberation from false interpretation. If the reading is constructed (b), the extraction is more apparent: it disciplines textual meaning toward contemporary political goals, and the 'victimization' of those subjected to the orthodox reading is real but the 'liberation' of those adopting this reading is contingent on accepting the reinterpretive authority. The resolution mechanism is historiographic: trace the actual emergence of the Gandhian reading through Gandhi's own writings, philosophical development, and political context. This will reveal whether the reading was discovered in the text or constructed for the independence struggle. The likelihood is that it is both—Gandhi recovered authentic textual resources (ahimsa teachings do exist in the Gita) while also reshaping them through the lens of modern non-violence philosophy. The ambiguity itself is irreducible and is documented in the omega variables.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_determinism_vs_reader_agency,
    'Does the allegorical reading recover what the Gita ''always meant,'' or does it impose a modern ethical lens retroactively?',
    'Historiography of Gita interpretation: trace actual scholarly readings across time periods; identify which interpretations emerge from close textual analysis vs which emerge from contemporary political movements. Distinguish between ''the text permits this reading'' and ''the text mandates this reading.''',
    'If deterministic (text always meant this): reading is natural-law-like, interpretation is discovery. If reader-dependent (modern lens imposed): reading is a contingent, politically motivated reframing; classification remains tangled_rope/snare depending on who benefits. This is the core ambiguity that gates false summit detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_determinism_vs_reader_agency, conceptual, 'Whether the allegorical reading recovers textual meaning or imposes modern ethics').

omega_variable(
    literal_vs_allegorical_mutual_foreclosure,
    'Can the Kurukshetra scenario be simultaneously literal (a historical or prescriptive warfare narrative) and allegorical (an internal moral struggle) within a single interpretive framework?',
    'Formal logical analysis: map the literal reading''s core claims (caste duty, warrior dharma, righteous violence) and the allegorical reading''s core claims (internal struggle, ahimsa supremacy, no literal violence) onto a shared semantic domain. Identify whether they logically contradict or occupy different interpretive layers.',
    'If mutually foreclosing: the readings cannot coexist; this reading forecloses the orthodox reading within any framework that adopts it. If logically compatible (e.g., ''the text describes a literal battle that also symbolizes inner struggle''): readings coexist; they are not foreclosed, merely privileging different interpretive layers. This determines the reading_relations values in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literal_vs_allegorical_mutual_foreclosure, conceptual, 'Whether literal and allegorical readings logically exclude each other').

omega_variable(
    caste_hierarchy_legitimacy_vs_textual_authority,
    'If the allegorical reading delegitimizes the caste hierarchy, does it retain or undermine the Gita''s textual authority as a source of dharma?',
    'Discourse analysis of Gandhian and Dalit readings: trace whether anti-caste movements invoke the Gita as binding authority or treat it as historically important but superseded; examine how movements navigate the tension between ''the Gita contains anti-caste truth'' and ''the Gita has been weaponized for caste oppression.''',
    'If authority is retained (Gita is reinterpreted but remains binding): the reading sustains institutional power structures, just on different ethical grounds — high theater, piton-risk. If authority is undermined (Gita is advisory, not binding): the reading genuinely liberates from institutional constraint, but loses its force to bind communities around shared values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_hierarchy_legitimacy_vs_textual_authority, empirical, 'Whether allegorical reading preserves Gita''s binding textual authority').

omega_variable(
    individual_conscience_vs_collective_dharma,
    'Does elevation of individual moral conscience as the ultimate arbiter of dharma resolve caste extraction, or does it relocate it to the individual''s internal struggle without changing the external structural constraint?',
    'Longitudinal ethnography: track communities that adopt individual-conscience-based interpretation; measure actual changes in caste practice, economic redistribution, and social mobility. Distinguish between interpretive liberation (the reading produces new mental frameworks) and structural liberation (material conditions improve).',
    'If individual reinterpretation alone changes material structures: the reading is more powerful than a tangled_rope — it actually coordinates anti-caste action. If material structures persist despite reinterpretation: the reading is primarily a cognitive reframing; the snare classification for trapped agents (victims of structural caste violence) remains accurate because material extraction continues despite rhetorical inversion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(individual_conscience_vs_collective_dharma, empirical, 'Whether individual conscience elevation changes material caste structures').

omega_variable(
    authenticity_of_gandhian_recovery,
    'How much of the Gandhian allegorical reading derives from Gandhi''s personal non-violence philosophy vs. from the Gita''s actual textual content?',
    'Comparative textual analysis: identify Gita verses that explicitly support ahimsa as supreme principle vs. verses that describe warrior dharma and righteous violence; assess the degree to which the allegorical reading requires auxiliary interpretive moves (symbolism, metaphor, recontextualization) vs. literal textual support.',
    'If Gandhi imposed his philosophy onto the text: the reading is contingent on Gandhi''s interpretive authority; classification depends on whether we credit his moral insight or view it as projection. If the text genuinely supports ahimsa-supremacy: the reading recovers latent textual meaning; it has stronger claims to authenticity. This gates whether the reading is legitimate reinterpretation or constructed extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_of_gandhian_recovery, empirical, 'Degree to which Gandhian reading recovers vs. imposes textual meaning').

omega_variable(
    ahimsa_universality_vs_context_specificity,
    'Is ahimsa (non-violence) a universal principle applicable across all contexts, or is it context-specific advice for particular agents or historical moments in the Gita?',
    'Hermeneutic analysis of Gita passages on ahimsa, violence, and dharma; examine textual constraints on the principle''s scope. Analyze whether the Gita permits exceptions to ahimsa (warrior dharma, righteous violence) or elevates ahimsa absolutely.',
    'If universal: the allegorical reading has stronger textual grounding; the orthodox literal reading cannot claim equal textual authority. If context-specific: both readings have textual legitimacy; the readings coexist rather than foreclose each other. This determines whether the reading_relations classify as forecloses vs. coexists_with.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ahimsa_universality_vs_context_specificity, empirical, 'Whether ahimsa is universal or context-specific in Gita').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__gandhian_allegorical_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_gandhi_tr_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gita_gandhi_tr_t30, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 30, 0.48).
narrative_ontology:measurement(gita_gandhi_tr_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(gita_gandhi_be_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gita_gandhi_be_t30, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(gita_gandhi_be_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(gita_gandhi_su_t0, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(gita_gandhi_su_t30, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(gita_gandhi_su_t60, gita_kurukshetra_discourse__gandhian_allegorical_reading, suppression_requirement, 60, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gita_kurukshetra_discourse__gandhian_allegorical_reading, identity_coordination).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__gandhian_allegorical_reading, gita_kurukshetra_discourse__universalist_devotional_reading).

% DUAL FORMULATION NOTE:
% The gita_kurukshetra_discourse kernel admits three structurally distinct constraint stories corresponding to the three major readings: orthodox literal (ε ≈ 0.35, legitimizes caste and righteous violence), universalist devotional (ε ≈ 0.28, emphasizes personal devotion over works), and Gandhian allegorical (ε ≈ 0.52, repudiates violence and caste through reinterpretation). Each reading produces different beneficiary/victim structures and different classifications because the constraint—the interpretive framework—changes meaning across readings. They are linked by network.affects_constraints because adoption of one reading creates pressure on the others: the Gandhian reading's success in claiming textual authority for non-violence influences the viability of the orthodox reading (challenges its legitimacy claims) and partially influences the universalist reading (creates expectations that Hindu ethics align with universal non-violence). The readings do not resolve to a single constraint; they remain as distinct but related stories in the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
