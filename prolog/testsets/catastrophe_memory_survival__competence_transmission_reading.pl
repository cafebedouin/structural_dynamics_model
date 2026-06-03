% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Catastrophe Memory via Competence Transmission (Practical Knowledge Encoding in Ritual)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Ritual preserves and transmits practical survival knowledge — timing
 *   calendars, resource management protocols, family decision-making
 *   structures, kinship-based adaptation networks — that enabled communities
 *   to endure catastrophe, displacement, and resource scarcity. This
 *   constraint, instantiated under the competence_transmission_reading,
 *   models ritual as a functional transmission channel for embodied
 *   knowledge: the specifics of when to plant, how to ration scarce goods,
 *   which kinship networks activate under which stress conditions, how to
 *   navigate unfamiliar territories. The reading focuses on practical
 *   knowledge content rather than symbolic meaning or identity continuity
 *   (which are the foci of the symbol_survival_reading and
 *   hybrid_encoding_reading). Under this reading, the constraint becomes a
 *   tangled_rope: ritual simultaneously coordinates adaptive knowledge
 *   transmission (genuine cooperation function) and extracts labor and
 *   authority from knowledge-bearing elders whose expertise is not recognized
 *   by secular institutions. The victim set is communities that maintain
 *   ritual form while losing practical content — younger generations inherit
 *   ceremonies but not the survival strategies they encode. The beneficiary
 *   set is diaspora communities with access to the transmitted knowledge and
 *   preservation institutions that capture and codify it. The constraint's
 *   extractiveness has risen over a 30-year interval (0.28 → 0.52) as
 *   practical context decays and ritual becomes increasingly performative
 *   (theater_ratio: 0.35 → 0.65). Suppression has increased as secular
 *   institutional systems (schools, bureaucracies, markets) have displaced
 *   ritual as the primary knowledge transmission channel, raising barriers
 *   for communities attempting to maintain embodied practical knowledge
 *   transmission.
 *
 * KEY AGENTS:
 *   - Communities Losing Practical Content: Primary victims (powerless/trapped) — inherit ritual form without embedded survival knowledge; experience identity lock through practice that no longer serves adaptive function
 *   - Knowledge-Bearing Elders: Secondary beneficiaries/victims (moderate/constrained) — authority maintained through elder status but labor extracted through unpaid knowledge transmission; constrained by institutional non-recognition of tacit knowledge
 *   - Diaspora Communities with Adaptive Capacity: Primary beneficiaries (institutional/arbitrage) — gain access to practical survival strategies through ritual participation; experience constraint as low-extraction coordination
 *   - Preservation Organizations and Cultural Institutions: Organized extractors (organized/constrained) — provide genuine preservation service while capturing institutional credit and authority over interpretation; gate access to codified knowledge
 *   - Secular Educational Systems: Institutional performers (institutional/arbitrage) — treat ritual as cultural artifact for study rather than living knowledge transmission channel; maintain form while decoupling practical function
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional displacement of embodied knowledge transmission as inevitable modernization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Catastrophe Memory via Competence Transmission (Practical Knowledge Encoding in Ritual)").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, 'a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c').
narrative_ontology:cs_kernel_codification('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', distributed).
narrative_ontology:cs_authority_grounding('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', practice).
narrative_ontology:cs_interpretation_layer_present('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c').
narrative_ontology:cs_reading_relation('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', foundational, ritual_practical_knowledge_separable).
narrative_ontology:cs_axiom_status(ritual_practical_knowledge_separable, holdable).
narrative_ontology:cs_axiom_grounding('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', ritual_practical_knowledge_separable, empirically_contingent).
narrative_ontology:cs_axiom('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', foundational, practical_knowledge_primary_survival_function).
narrative_ontology:cs_axiom_status(practical_knowledge_primary_survival_function, holdable).
narrative_ontology:cs_axiom_grounding('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', practical_knowledge_primary_survival_function, instrumental).
narrative_ontology:cs_reference_frame('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', embodied_practical_knowledge_transmission).
narrative_ontology:cs_drift_state('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', contemporary_institutional_displacement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a5e1d3f3-1f9a-4fc5-99a6-0f798aa4929c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities_adaptive_capacity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, knowledge_bearing_elders).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, communities_losing_practical_content).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, youth_decoupled_from_tacit_knowledge).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COMMUNITIES LOSING PRACTICAL CONTENT (SNARE) — Trapped in a system where ritual form persists but practical knowledge content erodes across generations. Younger community members inherit the performative shell without learning the embedded survival strategies (seasonal calendars, resource-rationing protocols, family decision-making processes, kinship-based adaptation networks). Maximum extraction: the community bears the cost of maintaining ritual without the adaptive benefit it once provided. No exit option — the practice is identity-fused and geographically bound.
constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: KNOWLEDGE-BEARING ELDERS IN HOST COMMUNITIES (TANGLED ROPE) — Constrained by language barriers, institutional non-recognition of tacit knowledge, and intergenerational knowledge gaps in diaspora. The constraint operates bidirectionally: elders benefit from the preservation of ritual practice (it maintains their authority and community cohesion) but also pay extraction costs (time burden of teaching, marginalization of practical knowledge in secular institutions). Mixed extraction and coordination — some genuine coordination of adaptive knowledge, substantial extraction of unpaid knowledge labor.
constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DIASPORA COMMUNITIES GAINING ADAPTIVE CAPACITY (ROPE) — Benefits from the constraint as a coordination mechanism: ritual practice encodes and transmits practical knowledge (seasonal timing, resource management, family protocols, kinship adaptation networks) that enables survival in hostile or resource-scarce environments. The institutional community (organized through religious or cultural organizations) experiences the constraint as low-extraction coordination — ritual simultaneously preserves identity and transmits survival strategy. Net beneficiary position.
constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRESERVATION ORGANIZATIONS AND CULTURAL INSTITUTIONS (TANGLED ROPE) — Organized actors (museums, cultural centers, academic institutions studying ritual) extract value from codifying practical knowledge in written form while simultaneously providing a genuine preservation service. The constraint contains real coordination (preventing loss of knowledge) and real extraction (institutional credit-capture, publishing rights, authority over interpretation). Constrained exit — these organizations depend on institutional funding and cannot easily abandon the knowledge capture mission.
constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: SECULAR EDUCATIONAL SYSTEMS TREATING RITUAL AS ARTIFACT (PITON) — Schools and curricula increasingly present ritual as cultural heritage or historical artifact rather than as a living transmission mechanism for practical knowledge. The performative content (symbolic meaning, identity maintenance) is preserved in museums and curricula, but the practical knowledge embedded in timing, sequence, and collective enactment is decoupled from functional use. Theater ratio high: ritual is studied and performed for its cultural significance, but the survival knowledge it encodes is treated as ethnographic curiosity rather than actionable information. Piton: the original function (practical knowledge transmission) has atrophied while the formal practice persists through institutional inertia.
constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational perspective, the erosion of practical knowledge in ritual transmission appears as an immutable law of cultural evolution: as societies modernize and institutional systems (schools, bureaucracies, markets) replace kinship-based knowledge transmission, ritual inevitably becomes decorative. The constraint appears as an inescapable feature of the transition from oral/embodied knowledge to institutional/codified knowledge. However, this naturalizes what is actually a contestable institutional choice — the engine's false summit detector will identify this as a constructed constraint presented as natural law.
constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_survival__competence_transmission_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Under the competence_transmission_reading, extraction arises from the asymmetry between knowledge-bearing elders and institutional recipients (diaspora communities, preservation organizations, secular scholars). Elders bear the labor cost of transmitting tacit knowledge without proportional institutional recognition or economic compensation. Knowledge is extracted from origin contexts and redeployed in diaspora and institutional settings where it accrues value to organizations rather than to knowledge bearers. The value is real (adaptive knowledge), but the distribution is asymmetric. Suppression (0.58): Moderate-high. Institutional displacement of embodied knowledge transmission creates substantial barriers: secular schools do not recognize tacit knowledge as curriculum; markets require codified documentation; bureaucracies require written protocols. Youth in secular systems face high barriers to accessing embodied practical knowledge; elders in diaspora face barriers transmitting through non-institutional channels. However, suppression is not total — ritual communities can and do maintain transmission, though at increasing cost. Theater ratio (0.65): Moderately high. As functional context erodes (e.g., seasonal calendars become less relevant in urban diaspora; kinship-based economic networks are displaced by wage labor), ritual increasingly becomes performative — the form is maintained for identity and continuity but the practical knowledge embedded in sequence and timing is no longer actively deployed. The trajectory shows rising theater as the practical functionality decays: t0=0.35 (ritual is still functionally oriented), t30=0.65 (ritual is substantially about cultural preservation and identity rather than applied survival strategy). This rise is the key diagnostic signature of the competence_transmission_reading: it shows the progressive uncoupling of form from practical function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits the core contestation of the kernel: different perspectives emphasize different functional registers of ritual. The primary victim (communities losing practical content) experiences the constraint as a snare — they maintain the form without the adaptive benefit. The knowledge-bearing elder experiences it as tangled_rope — real authority and community coordination value alongside real labor extraction. The diaspora community experiences it as rope — coordination of survival knowledge with low extraction cost. Preservation institutions experience it as tangled_rope — genuine preservation alongside credit-capture. Secular systems see it as piton — the ritual is maintained for cultural reasons but its original function (practical knowledge transmission) has atrophied. The analytical observer risks seeing it as mountain — natural law of cultural evolution. The competence_transmission_reading specifically privileges the functional-knowledge register: ritual is first and foremost a channel for transmitting survival knowledge. This reading coexists with the symbol_survival_reading (which privileges identity and continuity) and the hybrid_encoding_reading (which sees both registers as inseparable). The perspectival gap shows why no single reading fully captures the constraint — different observers legitimately perceive different primary functions, and the kernel contest reflects this real structural ambiguity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective derives from their structural position in the knowledge extraction flow. Knowledge-bearing elders (moderate/constrained) occupy a high-d position: they are the source of extraction (high d toward victim status) but also benefit from elder authority and cultural leadership (low d as beneficiary). The engine derives d from the combination of their power level (moderate), exit options (constrained — leaving the knowledge transmission role carries cultural and identity costs), and structural relationship (victim of labor extraction, beneficiary of authority maintenance). Diaspora institutional communities (institutional/arbitrage) occupy a low-d position: they are beneficiaries (low d toward full beneficiary) with high exit options (arbitrage — they can adopt or abandon ritual practices based on institutional benefit calculation). The engine derives their d from institutional power, arbitrage exit capacity, and net-beneficiary status. The analytical observer (analytical/analytical) occupies a medium-d position reflecting observational stance without structural participation in the extraction flow. The secular educational system (institutional/arbitrage) similarly occupies low-d as institutional beneficiary with high exit options (arbitrage — they can adopt or ignore the ritual knowledge source based on curriculum design choices).
 *
 * MANDATROPHY ANALYSIS:
 *   The competence_transmission_reading does not resolve the kernel contest between the three readings, but it does ground the constraint in a specific empirical function (practical knowledge transmission) that can be measured and traced. The extractiveness measurement (0.28 → 0.52) captures the increasing asymmetry between knowledge source (elders) and knowledge beneficiaries (diaspora, institutions) as practical context decays. The theater_ratio measurement (0.35 → 0.65) is the key mandatrophy signal: it empirically demonstrates that ritual form and practical function can decouple, supporting this reading's core claim about separability. The mandatrophy is resolved not by declaring this reading 'correct' (the kernel contest remains live), but by specifying which structural phenomenon this reading is modeling: the functional atrophy of embodied knowledge transmission under institutional displacement. If future ethnographic work shows that practical knowledge and symbolic function remain inseparable (the hybrid_encoding_reading's claim), the ε values would need to shift — practical knowledge would be reframed as inseparable from identity/boundary functions, moving the constraint's victim set from 'communities losing practical content' to 'communities experiencing disrupted identity continuity.' If symbol maintenance is shown to be primary and practical knowledge secondary (symbol_survival_reading), ε would drop and the constraint would shift from tangled_rope to rope. The measurement trajectory (rising theater_ratio over 30 years) is the empirical signature that should shift if the other readings become diagnostically superior.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    practical_knowledge_decay_rate,
    'At what rate does embedded practical knowledge decay when ritual form persists but functional context is removed?',
    'Longitudinal ethnographic study comparing knowledge transmission in ritual-embedded communities vs. secular institutions; documentation of specific survival strategies (seasonal calendars, resource rationing, kinship protocols) retained vs. lost in diaspora across generational cohorts',
    'If decay is rapid (< 2 generations): competence transmission reading is already obsolete in many diaspora contexts; constraint shifts toward symbol_survival_reading. If decay is slow (> 4 generations): practical knowledge persists embedded in ritual form; competence transmission reading remains diagnostically valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practical_knowledge_decay_rate, empirical, 'Rate of practical knowledge decay when ritual form persists without functional context').

omega_variable(
    ritual_form_versus_tacit_content_separability,
    'Can practical survival knowledge be extracted from ritual form and transmitted through non-ritual institutional channels (written manuals, digital archives, classroom instruction) without substantial loss of adaptive capacity?',
    'Comparative analysis of knowledge transmission outcomes: communities retaining ritual practice vs. communities using alternative institutional transmission methods; measurement of adaptive capacity, contextual applicability, and embodied understanding retention',
    'If separable with low loss: institutional preservation (written/digital/academic) is a viable alternative; competence transmission reading suggests the constraint can be resolved through recodification. If high loss: practical knowledge is fundamentally ritual-embedded and embodied; separation forecloses the adaptive benefit; reading must accommodate inseparability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ritual_form_versus_tacit_content_separability, empirical, 'Whether practical knowledge is separable from ritual form or fundamentally embodied-embedded').

omega_variable(
    beneficiary_versus_extractor_identity_under_reading,
    'Are diaspora communities and preservation institutions genuinely beneficiaries of the competence transmission mechanism, or are they extracting cultural capital from knowledge-bearing communities that lose functional access?',
    'Structural analysis of benefit distribution: Who retains decision-making authority over knowledge interpretation? Who captures institutional credit and professional advancement? Where does economic value flow? Comparison of diaspora institutional access vs. origin-community elder professional recognition.',
    'If genuine mutual benefit: reading is accurate; constraint is tangled_rope across all institutional perspectives. If asymmetric extraction: diaspora and preservation institutions are net beneficiaries; origin communities are net victims; reading shifts focus to extraction mechanism rather than coordination benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_versus_extractor_identity_under_reading, empirical, 'Whether institutional beneficiary status is symmetric mutual benefit or asymmetric extraction').

omega_variable(
    reading_contest_under_determination,
    'Which reading (competence_transmission vs. symbol_survival vs. hybrid_encoding) accurately represents the constraint? Does ritual primarily encode survival knowledge, preserve identity/continuity, or operate on both registers simultaneously?',
    'Ethnographic reconstruction of original constraint function in pre-catastrophe context; analysis of which ritual components correlate with survival outcomes vs. identity maintenance; assessment of whether practical knowledge and symbolic meaning can be analytically separated or are functionally inseparable.',
    'If competence transmission is primary: this reading is correct; victim set is communities losing practical content; ε ≈ 0.52. If symbol_survival is primary: hybrid_encoding_reading better describes constraint; ε lower (approx 0.35-0.42) because identity continuity has lower extraction signature. If hybrid is inseparable: all three readings are partial framings of one constraint; the kernel is genuinely contested and no single reading forecloses others.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_under_determination, conceptual, 'Reading contest: which function (competence vs. symbol vs. hybrid) is primary in ritual constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catas_comp_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catas_comp_tr_t15, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(catas_comp_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.65).

% Extraction over time
narrative_ontology:measurement(catas_comp_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(catas_comp_be_t15, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(catas_comp_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(catas_comp_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(catas_comp_su_t15, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(catas_comp_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_survival kernel has three distinct readings, each modeling a different primary function of ritual constraint. competence_transmission_reading focuses on practical survival knowledge encoding and transmission (ε ≈ 0.52, tangled_rope). symbol_survival_reading focuses on identity and continuity maintenance (ε ≈ 0.35-0.42, likely rope). hybrid_encoding_reading claims inseparability of practical and symbolic functions (ε ≈ 0.40-0.48, tangled_rope with different victim/beneficiary structure). These are not alternative measurements of one constraint — they are alternative readings of one contested kernel. The kernel contest reflects real structural ambiguity: ritual may serve multiple functions simultaneously, and different communities may experience different primary functions. Network links enable analysis of how empirical evidence (e.g., successful separation of practical knowledge from symbolic form, or failure to separate without functional loss) would support or refute each reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
