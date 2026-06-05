% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__abolitionist_rejection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__abolitionist_rejection, []).

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
 *   constraint_id: dharmasastra_corpus__abolitionist_rejection
 *   human_readable: Dharmasastra Corpus as Extractive Hierarchy (Abolitionist Reading)
 *   domain: religious_law/textual_authority/normative_hierarchy
 *
 * SUMMARY:
 *   The dharmasastra corpus (Manusmrti, Yajnavalkya Samhita, Narada Smriti,
 *   and related texts) grounds its legitimacy in claims of eternal cosmic
 *   order (rta, brahman manifestation, karma-driven rebirth). The
 *   abolitionist reading rejects this entire framework as fundamentally
 *   oppressive: the texts legitimate a hereditary varna system that assigns
 *   birth-determined occupation, ritual status, and access to knowledge;
 *   creates an untouchable caste beneath the four varnas; subordinates women
 *   across all castes; and consolidates brahminical priestly authority as the
 *   sole interpreter of cosmic law. From the abolitionist perspective, no
 *   reinterpretation can salvage these texts because the core function — to
 *   naturalize hierarchy — is intrinsic to their authority claim. The
 *   abolitionist reading is distinct from the reformist reading (which seeks
 *   to reinterpret dharmasastra as compatible with equality) and the orthodox
 *   literalist reading (which maintains the hierarchy is just cosmic order).
 *   The constraint exhibits all six DR types depending on structural
 *   position. Untouchable communities see a snare (trapped, no exit, maximum
 *   extraction). Lower varnas experience tangled rope (mixed coordination and
 *   extraction). The brahminical beneficiary experiences rope (coordination,
 *   arbitrage, low extraction). Reform movements see a scaffold with visible
 *   sunset (organized, mobile, systemic dismantling possible). Neo-Vedantic
 *   institutions maintain a piton (performative authority, inertial theater).
 *   The analytical observer risks the mountain perspective (treating
 *   contingent hierarchy as immutable law), which the structural data reveals
 *   as a false summit.
 *
 * KEY AGENTS:
 *   - Untouchable (Dalit) Communities: Primary victim (powerless/trapped) — system declares their labor essential but bodies polluting; occupational heredity enforced through violence and ritual exclusion
 *   - Women Across Castes: Primary victim (powerless/trapped) — dharmasastra prescribes subordination to male guardians, restricts Vedic study, assigns inferior ritual status
 *   - Lower Varna Communities (Sudras, Vaisyas): Secondary victim (moderate/constrained) — experience mixed coordination and extraction; occupy defined roles with limited agency
 *   - Brahminical Priesthood and Ritual Specialists: Primary beneficiary (institutional/arbitrage) — monopolize interpretive authority, receive economic support (daksina), maintain social prestige through ritual gatekeeping
 *   - Anti-Caste Reform Movements (Jyotiba Phule, B.R. Ambedkar, contemporary Dalit organizations): Organized agents (organized/mobile) — recognize constraint as dismantleable; build alternative frameworks through education, law, political organizing
 *   - Neo-Vedantic Institutional Authority (Hindu organizations, academic philosophy departments, reformed temples): Secondary beneficiary (institutional/constrained) — maintain textual authority through reinterpretation but with decreasing structural force
 *   - Epistemic Commons Integrity: Abstract victim (powerless/trapped) — cosmological claims treated as natural law obscure actual institutional construction; falsification of brahminical metaphysics prevented by authority claims
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, 0.68).
domain_priors:suppression_score(dharmasastra_corpus__abolitionist_rejection, 0.72).
domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, extractiveness, 0.68).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__abolitionist_rejection, snare).
narrative_ontology:human_readable(dharmasastra_corpus__abolitionist_rejection, "Dharmasastra Corpus as Extractive Hierarchy (Abolitionist Reading)").
narrative_ontology:topic_domain(dharmasastra_corpus__abolitionist_rejection, "religious_law/textual_authority/normative_hierarchy").

domain_priors:requires_active_enforcement(dharmasastra_corpus__abolitionist_rejection).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__abolitionist_rejection, '0ff7d88d-5219-4590-abed-840180a0e874').
narrative_ontology:cs_kernel_codification('0ff7d88d-5219-4590-abed-840180a0e874', formalized).
narrative_ontology:cs_authority_grounding('0ff7d88d-5219-4590-abed-840180a0e874', extraction).
narrative_ontology:cs_interpretation_layer_present('0ff7d88d-5219-4590-abed-840180a0e874').
narrative_ontology:cs_reading_relation('0ff7d88d-5219-4590-abed-840180a0e874', dharmasastra_corpus__orthodox_literalist, forecloses).
narrative_ontology:cs_reading_relation('0ff7d88d-5219-4590-abed-840180a0e874', dharmasastra_corpus__reformist_contextual, coexists_with).
narrative_ontology:cs_axiom('0ff7d88d-5219-4590-abed-840180a0e874', foundational, dharmasastra_authority_corrupted_foundation).
narrative_ontology:cs_axiom_status(dharmasastra_authority_corrupted_foundation, holdable).
narrative_ontology:cs_axiom_grounding('0ff7d88d-5219-4590-abed-840180a0e874', dharmasastra_authority_corrupted_foundation, empirically_contingent).
narrative_ontology:cs_axiom('0ff7d88d-5219-4590-abed-840180a0e874', foundational, hierarchy_extraction_inseparable_from_coordination).
narrative_ontology:cs_axiom_status(hierarchy_extraction_inseparable_from_coordination, holdable).
narrative_ontology:cs_axiom_grounding('0ff7d88d-5219-4590-abed-840180a0e874', hierarchy_extraction_inseparable_from_coordination, empirically_contingent).
narrative_ontology:cs_reference_frame('0ff7d88d-5219-4590-abed-840180a0e874', universal_dignified_personhood).
narrative_ontology:cs_drift_state('0ff7d88d-5219-4590-abed-840180a0e874', contemporary_postcolonial_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0ff7d88d-5219-4590-abed-840180a0e874', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, brahminical_authority).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__abolitionist_rejection, ritual_gatekeepers).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, untouchable_communities).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, women_across_castes).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, lower_varnas).
narrative_ontology:constraint_victim(dharmasastra_corpus__abolitionist_rejection, epistemic_commons_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNTOUCHABLE COMMUNITIES (SNARE) — Trapped within a cosmological hierarchy that declares their labor essential but their bodies polluting, their occupations hereditary and unchangeable. The dharmasastra framework naturalizes occupation-based segregation as cosmic law (varna-jati system). Birth determines lifetime extraction: exclusion from ritual spaces, denial of education access, violent enforcement of status boundaries. No exit options exist within the framework; attempted exit triggers communal enforcement. Maximum experienced extractiveness.
constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOWER VARNA COMMUNITIES (TANGLED ROPE) — Experience genuine coordination function: dharmasastra provides predictable role assignments, ritual status hierarchies that acknowledge their existence, and limited but legible pathways for ritual participation and economic activity. Simultaneously trapped by occupational heredity (varna determines occupation determines social position determines ritual eligibility). Some agency through merchant guilds or ritual specialists, but asymmetric extraction remains. Beneficiary of stability; victim of hierarchy.
constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAHMINICAL PRIESTHOOD (ROPE) — Primary beneficiary with arbitrage options (can reinterpret texts, migrate to new institutional settings, leverage scriptural authority). Experiences dharmasastra as coordination mechanism: the hierarchy produces predictable deference, economic support (daksina/fees), monopoly on ritual authority, and legitimacy for the teaching lineage. The constraint coordinates patronage and ritual dependency. Low experienced extraction because the beneficiary has structural autonomy and can reframe constraint as dharma (duty).
constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANTI-CASTE REFORM MOVEMENTS (SCAFFOLD) — Organized agents (Jyotiba Phule, Ambedkar, contemporary Dalit movements) recognize the constraint as temporary: a institutional arrangement that can be dismantled through education, law, political organizing. The scaffold perspective sees the dharmasastra framework as a contingent historical construction, not cosmic law. Sunset logic applies: modern education access, constitutional equality, and political mobilization are building alternative frameworks that bypass scriptural authority entirely. Effective extraction is low because this perspective has visible exit pathways and organized capacity.
constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: NEO-VEDANTIC INSTITUTIONAL AUTHORITY (PITON) — Contemporary institutions (some Hindu religious organizations, academic departments of Hindu philosophy) maintain the dharmasastra corpus through elaborate reinterpretation: caste is reframed as occupational specialization, hierarchies are reread as inclusive diversity, oppressive passages are contextualized as historically situated. The institutional performance of authority persists (ceremonial reverence of texts, scholarly commentary chains) despite the core legitimacy mechanism being hollowed out. Theater ratio high because the performative maintenance of textual authority has become divorced from its functional extraction mechanism. The institution sees its own preservation as increasingly inertial.
constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COSMOLOGICAL NATURALIZATION (MOUNTAIN) — From a certain analytical distance, the dharmasastra hierarchy appears to be grounded in immutable cosmic order (rta, brahman manifestations, karma-driven rebirth). The text itself claims the varna system is natural law: 'Just as there is a natural gradation in all things, so all living beings have natural distinctions' (Manusmrti). From this view, the hierarchy is not constructed but discovered — an unchangeable feature of being. However, this perspective commits the false summit error: treating a contingent institutional arrangement as natural law, thereby naturalizing what is actually an extractive framework.
constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__abolitionist_rejection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dharmasastra_corpus__abolitionist_rejection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dharmasastra_corpus__abolitionist_rejection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dharmasastra_corpus__abolitionist_rejection, TR),
    TR >= 0.70.

:- end_tests(dharmasastra_corpus__abolitionist_rejection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The abolitionist reading measures extractiveness by the cost borne by lower-caste and untouchable communities: hereditary occupational assignment, denial of knowledge access, ritual exclusion, violent enforcement of status boundaries, and psychological internalization of inferiority. The measurement trajectory (0.48→0.72→0.55) reflects increasing formalization and consolidation of the hierarchy in medieval-period texts (peak extractiveness), partial erosion under colonial and constitutional frameworks, and contemporary persistence through institutional inertia. Suppression (0.72): High. The constraint maintains itself through multiple mechanisms: (1) legal enforcement (caste-based occupational restrictions historically embedded in local law), (2) religious/cosmological legitimacy (hierarchy presented as cosmic order, not human construction), (3) communal surveillance and violent enforcement of status boundaries, (4) denial of literacy and educational access to lower castes, preventing counter-narratives, (5) economic dependency (caste determines occupational options, limiting exit capacity). Theater ratio (0.55): Moderate-high and rising. As legal and social enforcement mechanisms weakened in postcolonial India, the textual and ritual theater increased. Contemporary brahminical institutions maintain authority increasingly through ceremonial reverence of texts, scholarly commentary traditions, and reinterpretation rather than through functional extraction. The rising trajectory (0.25→0.62) models this shift from primarily functional extraction (early periods) to increasingly performative maintenance of authority (contemporary period). The abolitionist reading sees the whole performance as theater masking the extractive core.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence across power atoms. The brahminical priesthood sees dharmasastra as a coordination mechanism ensuring stable role assignments, ritual dependency, and patronage flows — a rope constraint with low experienced extraction because they have autonomy. Untouchable communities see the same texts as a snare: immutable birth-based hierarchy with violent enforcement and no exit options. Lower varnas experience tangled rope: genuine coordination function (role clarity, ritual acknowledgment) mixed with asymmetric extraction (occupational heredity, ritual subordination). The reform movement sees a scaffold: the hierarchy is temporary, dismantleable through education, law, and political power; sunset logic applies because constitutional equality and social mobility are already building alternatives. Neo-Vedantic institutions see a piton: their own authority structure is inertial, maintained through increasing theater as functional extraction decays. The analytical observer risks the mountain perspective: treating the hierarchy as cosmic law, unchangeable. The false summit detector identifies this as naturalization — the cosmological claims are institutional constructions, not discovered truths.
 *
 * DIRECTIONALITY LOGIC:
 *   Untouchable communities: d=0.95 (full target of extraction). They experience maximum asymmetric extraction with no exit options — trapped within the hierarchy. Brahminical priesthood: d=0.10 (full beneficiary with arbitrage exit). They extract benefit while retaining interpretive autonomy and can arbitrage reinterpretation. Lower varnas: d=0.60 (mixed). They experience moderate extraction but also some coordination function and limited agency. Reform movements: d=0.45 (organized victim becoming agent). They bear costs of opposing the hierarchy but have increasingly mobile exit options and organized capacity. Neo-Vedantic institutions: d=0.35 (constrained beneficiary). They maintain institutional benefit but increasingly constrained by law and social mobility; reinterpretation protects some authority while limiting extraction magnitude. The perspectival gap emerges from these divergent d values: the beneficiary experiences rope (coordination, arbitrage); the primary victim experiences snare (trapped, maximum extraction); the organized reformer experiences scaffold (sunset visible, exit possible); the neo-Vedantic institution experiences piton (authority inertial, theater rising, functional extraction declining).
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading resolves mandatrophy by rejecting the premise that dharmasastra provides legitimate coordination: it does not. The hierarchy's extractive function (assigning labor, status, knowledge access by birth) is inseparable from any coordination benefits. Reformism claims mandatrophy is resolvable — the texts can be reread to preserve coordination (stable role assignments, role clarity) while eliminating extraction (hierarchy). Abolitionism rejects this: the coordination benefits (role stability) are purchased entirely through extraction (heredity, status assignment, knowledge denial). Dismantling the hierarchy is the only consistent move. From the snare perspective (untouchable communities), there is no mandatrophy to resolve — the constraint is pure extraction with cosmological theater. From the rope perspective (brahminical priesthood), mandatrophy doesn't apply — they perceive genuine coordination because the constraint benefits them. From the tangled rope perspective (lower varnas), mandatrophy is real but abolitionism may overstate the pure extraction; the constraint does provide some coordination alongside extraction. The abolitionist reading accepts this trade-off judgment (extraction too high to accept any coordination benefit) while the reformist reading rejects it (coordination worth preserving if extraction can be eliminated through reinterpretation). The mandatrophy is not resolved in favor of one reading — both readings make coherent choices about whether coordination benefits justify extraction costs. The abolitionist reading simply answers: no, not at this magnitude.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_authority_contingency,
    'Is the dharmasastra corpus''s claim to cosmological authority itself founded on human convention, or does it represent discovery of immutable social order?',
    'Historical-comparative analysis: tracing how dharmasastra changed across centuries, regional variations, textual reinterpretations, and the role of brahminical institutional power in stabilizing canonical versions. Examination of whether ''eternal law'' claims emerge from the texts themselves or from interpreters'' framings.',
    'If authority is contingent: the entire hierarchical system is dismantleable without cosmological violation. The abolitionist reading''s core premise (frameworks can be wholly abandoned) becomes structurally sound. If authority is discovered law: abolitionist rejection requires denying cosmological reality, which constrains its persuasiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_contingency, empirical, 'Whether dharmasastra''s authority is contingent institutional construction or discovered cosmic order').

omega_variable(
    substitution_framework_sufficiency,
    'Can constitutional equality, secular law, and democratic governance provide sufficient coordination and legitimacy to replace the social role dharmasastra played in providing hierarchical order?',
    'Longitudinal observation: post-1950 India adopting constitutional equality while traditional dharmasastra authority eroded; measurement of social coordination (dispute resolution, role clarity, collective action) under constitutional vs. dharmic frameworks in same populations; assessment of what ''order'' was actually produced by the hierarchy.',
    'If secular frameworks prove adequate: abolitionist rejection''s claim (framework can be wholly abandoned) is empirically supported. If social fragmentation or coordination failure increases: the snare classification stands, but the abandon-entirely thesis becomes contested. The constraint might shift to tangled_rope (some coordination legitimately lost with extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_framework_sufficiency, empirical, 'Whether secular constitutional frameworks can substitute for dharmasastra''s coordination functions').

omega_variable(
    reading_foreclosure_boundary,
    'Does the abolitionist reading (zero textual authority) logically foreclose the reformist reading (reinterpretation retaining some authority), or do both remain live options in different institutional contexts?',
    'Logical analysis of axiom compatibility: can a single dharmic authority holder coherently maintain both ''this text has no legitimate authority'' AND ''this text, properly understood, permits progressive reforms''? Or are these mutually exclusive commitments that force a choice?',
    'If foreclosed: readings are incompatible; adopting abolitionist reading requires rejecting reformist framework wholesale. If coexists_with: different institutional actors can hold different readings without logical contradiction (some abandon, some reinterpret). The relationship between readings becomes a political choice, not a logical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether abolitionist and reformist readings logically foreclose each other or coexist').

omega_variable(
    enforcement_mechanism_identity,
    'What is the actual primary enforcement mechanism that maintains the dharmasastra hierarchy: explicit legal violence, internalized legitimacy acceptance, community surveillance, or economic dependency?',
    'Structural analysis: identification of what breaks the hierarchy in different contexts. Does loss of legal authority (modern India) quickly degrade the system? Does internalized acceptance persist after legal authority erodes? Which communities sustain hierarchy after external enforcement ceases?',
    'If primarily legal: abolitionist rejection (dismantle the framework) is sufficient. If primarily internalized: dismantling requires additional psychological and cultural work beyond textual rejection. Suppression measurement (0.72) may be underestimating the internalized component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_identity, empirical, 'Primary enforcement mechanism sustaining the dharmasastra hierarchy').

omega_variable(
    cosmological_realism_vs_construction,
    'From within the abolitionist reading''s own framework, is the dismissal of dharmasastra''s cosmological claims grounded in epistemological realism (the cosmos doesn''t actually operate this way) or in normative rejection (even if true, such a cosmos would be unjust and should be rejected)?',
    'Textual and argumentative analysis: What grounds Ambedkarite and contemporary Dalit abolitionist rejection? Empirical falsification of brahminical cosmology? Moral argument that cosmic hierarchy is unjust? Both?',
    'If epistemological realism: abolitionist reading rests on challenging brahminical empirical claims about cosmic order. If normative rejection: abolitionist reading accepts some brahminical metaphysical claims but rejects their moral authority. The reading''s foundational axiom shifts (nature of cosmos vs nature of justice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmological_realism_vs_construction, conceptual, 'Whether abolitionist rejection rests on epistemological or normative grounds').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__abolitionist_rejection, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsa_abort_theater_t0_vedic, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dsa_abort_theater_t1_classical, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 1, 0.35).
narrative_ontology:measurement(dsa_abort_theater_t2_medieval, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 2, 0.45).
narrative_ontology:measurement(dsa_abort_theater_t3_colonial, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 3, 0.52).
narrative_ontology:measurement(dsa_abort_theater_t4_postcolonial, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 4, 0.58).
narrative_ontology:measurement(dsa_abort_theater_t5_contemporary, dharmasastra_corpus__abolitionist_rejection, theater_ratio, 5, 0.62).

% Extraction over time
narrative_ontology:measurement(dsa_abort_extractiveness_t0_vedic, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(dsa_abort_extractiveness_t1_classical, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 1, 0.58).
narrative_ontology:measurement(dsa_abort_extractiveness_t2_medieval, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 2, 0.72).
narrative_ontology:measurement(dsa_abort_extractiveness_t3_colonial, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 3, 0.68).
narrative_ontology:measurement(dsa_abort_extractiveness_t4_postcolonial, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(dsa_abort_extractiveness_t5_contemporary, dharmasastra_corpus__abolitionist_rejection, base_extractiveness, 5, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__abolitionist_rejection, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, dharmasastra_corpus__reformist_contextual).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, hindu_institutional_authority).
narrative_ontology:affects_constraint(dharmasastra_corpus__abolitionist_rejection, caste_reproduction_mechanisms).

% DUAL FORMULATION NOTE:
% The dharmasastra_corpus kernel decomposes into three readings: abolitionist_rejection (this story, ε=0.68, snare upstream), reformist_contextual (sibling story, ε=0.42, tangled_rope), and orthodox_literalist (sibling story, ε=0.15, rope upstream — lower because orthodoxy perceives no extraction). Each reading generates different beneficiary/victim structures, different suppression mechanisms, and different classifications from the same underlying texts. The three are not observable-dependent variants of a single constraint (ε-invariance principle) but rather different institutional readings of a contested kernel. The abolitionist reading forecloses the orthodox reading but coexists_with the reformist reading as different political and institutional commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dharmasastra_corpus__abolitionist_rejection, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
