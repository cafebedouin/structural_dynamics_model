% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual as Practical Knowledge Transmission (Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   Under this reading, ritual functions as a practical knowledge storage and
 *   transmission system: the ceremonial form encodes timing, resource
 *   management, family crisis protocols, and adaptation strategies that are
 *   critical to diaspora and displaced communities' survival. The constraint
 *   operates as tangled_rope because genuine coordination (embedding
 *   practical knowledge in memorable form that travels across displacement)
 *   coexists with asymmetric extraction (knowledge-carrier families gain
 *   gatekeeping status and authority; tradition-bearers lose control over
 *   teaching; third-generation practitioners lose competence content while
 *   bearing responsibility for form). The ritual's persistence depends on
 *   active enforcement by institutional religious authorities standardizing
 *   form across diverse communities, which actually accelerates the loss of
 *   context-specific competence content. This reading contests a kernel
 *   (catastrophe_memory_survival) where sibling readings emphasize symbolic
 *   boundary-maintenance (symbol_survival_reading) or dual
 *   symbolic-and-practical functions (hybrid_encoding_reading). This
 *   constraint story instantiates the competence-transmission reading
 *   exclusively.
 *
 * KEY AGENTS:
 *   - diaspora_communities: Benefit from adaptive knowledge encoded in ritual form; gain cultural identity and practical survival strategies
 *   - tradition_bearers_losing_content: Pay the cost of teaching practical content to younger practitioners who learn form without meaning
 *   - knowledge_carrier_families: Benefit from gatekeeping status; control the pace and direction of competence transmission
 *   - institutional_religion_authorities: Enforce standardized ritual performance across diverse communities; erase context-specific practical mappings
 *   - third_generation_practitioners: Perform the form correctly but lose the embedded survival logic; inherit responsibility without competence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.41).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual as Practical Knowledge Transmission (Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '1955566c-a625-453e-893f-10a1a2c8ca8f').
narrative_ontology:cs_kernel_codification('1955566c-a625-453e-893f-10a1a2c8ca8f', fixed_text).
narrative_ontology:cs_authority_grounding('1955566c-a625-453e-893f-10a1a2c8ca8f', lineage).
narrative_ontology:cs_interpretation_layer_present('1955566c-a625-453e-893f-10a1a2c8ca8f').
narrative_ontology:cs_reading_relation('1955566c-a625-453e-893f-10a1a2c8ca8f', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('1955566c-a625-453e-893f-10a1a2c8ca8f', catastrophe_memory_survival__hybrid_encoding_reading, influences).
narrative_ontology:cs_axiom('1955566c-a625-453e-893f-10a1a2c8ca8f', foundational, ritual_encodes_practical_survival_knowledge).
narrative_ontology:cs_axiom_status(ritual_encodes_practical_survival_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('1955566c-a625-453e-893f-10a1a2c8ca8f', ritual_encodes_practical_survival_knowledge, empirically_contingent).
narrative_ontology:cs_axiom('1955566c-a625-453e-893f-10a1a2c8ca8f', foundational, competence_transmission_is_ritual_primary_function).
narrative_ontology:cs_axiom_status(competence_transmission_is_ritual_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('1955566c-a625-453e-893f-10a1a2c8ca8f', competence_transmission_is_ritual_primary_function, empirically_contingent).
narrative_ontology:cs_reference_frame('1955566c-a625-453e-893f-10a1a2c8ca8f', competence_transmission_under_diaspora_pressure).
narrative_ontology:cs_drift_state('1955566c-a625-453e-893f-10a1a2c8ca8f', contemporary_institutional_standardization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1955566c-a625-453e-893f-10a1a2c8ca8f', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, knowledge_carrier_families).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, tradition_bearers_losing_content).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, third_generation_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, third_generation_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Dispersed from homeland or ancestral practice site; receive adaptation strategies encoded in ritual (harvest timing, resource scarcity management, kinship protocols for distributed networks, crisis response patterns). The ritual's apparent formalism (identical words, gestures, sequence) actually bundles practical adjustments invisible to those who do not know the operational context. Gain adaptive capacity and cultural identity simultaneously.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    organized, generational, identity_locked, global).

% Elders, lineage keepers, ritual specialists who carry the competence reading (the knowledge of WHEN to plant, WHO handles what in crisis, how resources flow in hard years). They perform the ritual but increasingly bear the cost of teaching the content layer to younger practitioners who learn the form but not the embedded survival logic. The competence knowledge requires active, continuous explanation outside the ritual frame itself.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, tradition_bearers_losing_content, payer,
    moderate, biographical, constrained, regional).

% Perform the ritual accurately but without understanding the practical content embedded in its structure. They inherit the form (which survives transmission perfectly) but lose the competence mapping (which requires explanation and context-specific application). They pay the cost of appearing/being held responsible for knowledge they do not possess; they also benefit from identity continuity the ritual maintains.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, third_generation_practitioners, payer,
    moderate, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, third_generation_practitioners, beneficiary).

% Families that maintain both the ritual form and the competence mapping (the reading is their property, their lineage's distinctive knowledge). They benefit from being gatekeepers of practical wisdom; they can withhold, negotiate teaching, and position themselves as essential to community survival. They set the pace of knowledge transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, knowledge_carrier_families, beneficiary,
    powerful, generational, mobile, global).

% Religious hierarchies, denominational structures, or canonical authorities that standardize ritual performance, often emphasizing symbolic/devotional content over practical embedded knowledge. They enforce uniform ritual structure across disparate communities, which ensures the form survives but can erase the context-specific competence mappings. They set what counts as 'correct' performance.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, institutional_religion_authorities, agenda_setter,
    institutional, generational, mobile, global).

% Communities that performed rituals tied to specific ecological or social conditions (planting seasons, water sources, kinship structures) and have been displaced, digitized, or integrated into different systems. They would object that the ritual's competence content is now inaccessible because the material conditions it was embedded in no longer exist. They are often not present in diaspora communities' ritual spaces.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, communities_losing_territorial_anchor, excluded,
    powerless, generational, trapped, local).

% Researchers, archivists, and external analysts who document ritual practice and trace embedded practical knowledge. They see the divergence between form preservation and content loss; they can corroborate whether competence transmission is occurring or the ritual is becoming pure symbol.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, anthropological_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, institutional_religion_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual solves the problem of transmitting survival-critical practical knowledge across displacement, generational discontinuity, and institutional standardization. When working, it bundles tacit competence (timing, resource management, family crisis protocols) into repeatable ceremonial form that preserves meaning under conditions of diaspora, migration, or cultural pressure.
% TRANSFER_FUNCTION: Moves practical competence knowledge from elders/tradition-bearers to practitioners; also moves social authority and gatekeeping status from knowledge-carrier families to institutional authorities enforcing standardized performance. The knowledge flows bidirectionally: genuine competence transmission enriches diaspora adaptive capacity; form-without-content transmission transfers authority to institutional standardizers while impoverishing communities losing territorial anchors.
% ABSENT_VOICES: Communities whose original ecological or social conditions no longer exist—whose ritual was meaningfully tied to specific water sources, planting seasons, or kinship structures that displacement or modernization has erased. They would object that the ritual's competence content has become meaningless ghost knowledge; they are excluded from diaspora communities' reconstituted rituals where the old timing/resource logic no longer maps to new material conditions.
% DISAPPEARANCE_RATIONALE: If this constraint vanished—if rituals ceased encoding/transmitting practical survival knowledge and became pure symbolic performance—diaspora communities would lose a major pathway for adaptive learning; knowledge-carrier families would lose gatekeeping status; third-generation practitioners would face an explicit knowledge gap where they now perform without understanding. Communities would reorganize around explicit instruction, written manuals, or other transmission channels, losing the embedded efficiency of ritual-bound competence.
% FOUNDING_PROBLEM: Under catastrophic displacement, diaspora, or institutional pressure, practical survival knowledge accumulated through generations (seasonal timing, resource rationing, kinship obligations during crisis) is vulnerable to loss. Ritual preserves this knowledge by embedding it in ceremonial structure that survives because the form itself is valued, even when the content is forgotten by performers.
% FOUNDING_PROBLEM_CORROBORATION: Diaspora communities attest that embedded ritual knowledge is live and essential: Jewish Passover timing coinciding with spring fertility concerns; Navajo seasonal songs encoding livestock management; Islamic prayer structure encoding regular social gathering and obligation timing. Anthropological documentation (Staal on Agni Cayana, Grimes on ritual competence) confirms the founding problem for communities under displacement pressure. Institutional authorities attest the problem is solved by transmission through ritual form; tradition-bearers and communities losing territorial anchors attest the problem persists because the content is being lost even as the form survives.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 to 0.58 over the interval as institutional standardization (enforced via updated liturgy, centralized teaching, digital documentation) increases the loss of context-specific competence while the ritual's form remains perfectly preserved. The symmetry breaks: diaspora communities initially benefit from knowledge transmission, but as the content layer atrophies, they inherit empty ritual. Theater_ratio rises from 0.28 to 0.52 because an increasing share of ritual activity becomes performative maintenance of form rather than practical knowledge application: practitioners are taught 'how to do it correctly' but not 'why this timing matters' or 'what resource constraint this practice manages.' Suppression_requirement rises as institutional authorities must actively defend standardized performance against communities trying to re-contextualize the ritual to their new material conditions. The measurement trajectory models extraction accumulation under institutional standardization: the coordination benefit (embodied knowledge transmission) is real in the early interval but decays as form enforcement progressively erases content.
 *
 * PERSPECTIVAL GAP:
 *   The institutional authority seat and the tradition-bearer seat compute radically differently from the same constraint. Authorities see rope: coordination of religious identity and practical knowledge transmission, value-neutral standardization. Tradition-bearers see tangled_rope trending toward snare: coordination is real, but its persistence depends on their increasing coercion (teaching without context, performing standardized forms, accepting knowledge loss). Third-generation practitioners compute differently from knowledge-carrier families: the same constraint gives families gatekeeping power and gives practitioners a knowledge gap with identity-lock preventing exit. The engine's per-seat classification exposes these structural asymmetries; the narrative claim (tangled_rope) reflects the tradition-bearer seat's perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Diaspora communities face low directionality (d near 0.2) because they genuinely benefit from the knowledge transmission, especially early in the interval. Knowledge-carrier families face near-zero directionality (d near 0.1) because they control the constraint and profit from gatekeeping—they are not targeted but empowered. Tradition-bearers (moderately powerful, constrained exit) face high directionality (d near 0.75) because they pay the cost of content transmission but are progressively disempowered by institutional standardization. Third-generation practitioners face mixed directionality (d near 0.55) because they inherit both the identity benefit and the competence loss; their role is dual (payer+beneficiary). Institutional authorities face near-zero directionality (d near 0.05) because they are the agenda-setter: they enforce and profit from standardization without bearing its costs. Communities with lost territorial anchors, though excluded, would face extremely high directionality (d near 0.95) if included because they lose all practical content while the ritual's form is imposed on them as meaningless.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (practical knowledge loss under displacement) is live and real. The constraint solves it initially: diaspora communities genuinely gain adaptive knowledge through ritual. But the founding problem's solution is actively undermined by institutional standardization, which preserves the ritual form while erasing the content that makes knowledge transmission possible. At the end of the interval, the ritual persists (form is perfect) but the founding problem is being lost (content is gone)—classic mandatrophy territory. The constraint's mandate (preserve survival knowledge through ritual form) has outlived its function (institutional standardization now prevents the content preservation that justified the form's existence). By interval end, the ritual is maintained by inertia and identity-lock, not because it successfully transmits practical competence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    content_loss_vs_form_preservation_mechanism,
    'Is the loss of practical content in ritual transmission a byproduct of institutional standardization, or is form-preservation actually structurally incompatible with context-specific competence transmission?',
    'Compare institutional-standardized rituals with lineage-based non-standardized rituals in the same diaspora context over generational timescales: if context-specific competence persists in non-standardized lineages, the loss is attributable to standardization; if competence is equally lost regardless of standardization, the incompatibility is structural.',
    'If standardization is the cause, remedying the constraint requires institutional reform (decentralizing teaching authority, recording context-specific mappings). If the incompatibility is structural, no policy change fixes it—diaspora communities need parallel non-ritual transmission channels for practical knowledge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(content_loss_vs_form_preservation_mechanism, empirical, 'Whether form-preservation under institutional authority necessarily erases embedded practical content.').

omega_variable(
    identity_lock_exit_permanence,
    'Does the identity-lock exit status for diaspora practitioners and third-generation carriers reflect genuine identity fusion (the ritual''s performance is constitutive of their self-concept and cultural belonging), or is it imposed by social pressure from knowledge-carrier families and institutional authorities who have structural interest in keeping practitioners bound?',
    'Post-exit ethnographic tracking: do practitioners who stop performing the ritual report identity dissolution or liberation? Do they reconstruct identity through other cultural practices? Do knowledge-carrier families use shame, exclusion, or other social pressure to prevent exit?',
    'If identity fusion is genuine, exit costs are insurmountable and the constraint is deeply binding. If identity-lock is socially imposed, exit is theoretically possible but requires collective action and alternative identity anchors—the constraint''s suppression power is much higher than the structural material analysis alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_exit_permanence, empirical, 'Whether identity-lock reflects genuine identity fusion or socially enforced identity imprisonment.').

omega_variable(
    competence_transmission_alternative_pathways,
    'Can the practical survival knowledge embedded in ritual (resource management, timing, family protocols) be transmitted through non-ritual channels (oral instruction, written manuals, apprenticeship)—or does ritual''s memorability and identity-bundling make it uniquely efficient for diaspora contexts?',
    'Survey diaspora communities that have adopted non-ritual competence transmission (explicit teaching, documentation) and measure retention/application rates compared to ritual-embedded transmission; assess whether practical knowledge survives loss of ritual form.',
    'If alternative pathways are equally effective, the constraint''s coordination benefit is overstated and institutional authorities could decentralize teaching without losing competence transmission. If ritual embedding is uniquely efficient for diaspora contexts, the constraint''s extraction component (gatekeeping by knowledge-carrier families) is harder to remedy without destroying the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_transmission_alternative_pathways, empirical, 'Whether ritual embedding is functionally necessary for practical knowledge transmission in diaspora contexts.').

omega_variable(
    kernel_contest_framing_under_determination,
    'Does this constraint instantiate the competence-transmission reading, or does the three-reading decomposition (competence vs. symbol vs. hybrid) misframe the kernel by treating these as separable when they are actually integrated?',
    'Ethnographic analysis of ritual practitioners'' own framing: do they articulate practical and symbolic registers as separate functions they can weigh, or as integrated meaning-making where separating them destroys both?',
    'If integrated, this reading''s claim that competence transmission is the primary function (with symbolic boundary-maintenance as secondary) is backwards: the ritual''s function is unified and separating them for analysis obscures the kernel. The kernel would need to be reconceptualized, possibly as a single constraint rather than three separate readings. If separable, the three-reading decomposition is valid and this reading''s claim stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_framing_under_determination, conceptual, 'Whether the kernel contest framing (three distinct readings) matches practitioners'' own understanding of ritual function, or imposes an analytical decomposition that practitioners do not recognize.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cata_tr_t8, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(cata_tr_t16, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(cata_tr_t24, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement(cata_tr_t32, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 32, 0.51).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cata_be_t8, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(cata_be_t16, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(cata_be_t24, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 24, 0.54).
narrative_ontology:measurement(cata_be_t32, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(cata_su_t8, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 8, 0.28).
narrative_ontology:measurement(cata_su_t16, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 16, 0.33).
narrative_ontology:measurement(cata_su_t24, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 24, 0.37).
narrative_ontology:measurement(cata_su_t32, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_survival__competence_transmission_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, diaspora_identity_continuity__ritual_performance_constraint).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, institutional_religious_authority__standardization_imposition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_survival kernel. The sibling readings (symbol_survival_reading, hybrid_encoding_reading) decompose the kernel into separable structural claims: this reading asserts ritual's primary function is practical competence transmission; the symbol reading asserts it is identity/boundary preservation; the hybrid reading asserts both are necessary and integrated. Each reading carries different ε, different victim/beneficiary sets, and different claimed types. The three readings should be analyzed as a family with affects relationships documenting how privileging one reading's framing (competence vs. symbol) creates structural pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__competence_transmission_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
