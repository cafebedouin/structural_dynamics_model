% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission: Symbol Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint models one reading of a contested kernel: how should
 *   catastrophe memory be preserved and transmitted across generations? The
 *   symbol-continuity reading holds that ritual form — the specific symbolic
 *   actions, words, and material practices — IS the mechanism through which
 *   collective memory survives. To preserve the memory of a catastrophe
 *   (genocide, cultural destruction, mass violence), one must preserve the
 *   ritual form through which that memory is encoded. This reading
 *   instantiates a constraint where adherence to ancestral symbolic form
 *   becomes non-negotiable: practitioners cannot adapt the ritual to
 *   contemporary contexts without risking loss of the memory itself. The
 *   constraint exhibits tangled-rope structure: it genuinely coordinates
 *   memory transmission (beneficiary: communal identity continuity, religious
 *   authority structures), but simultaneously extracts adaptive capacity from
 *   practitioners (victim: the flexibility to reinterpret, contextualize, or
 *   innovate in response to changed environments). Suppression is high
 *   because the binding mechanism is identity-fusion: practitioners cannot
 *   exercise cognitive exit (reinterpretation) without ceasing to be
 *   legitimate community members. The theater ratio has risen over the
 *   interval as ritualistic performance has increasingly become about
 *   demonstrating fidelity to ancestral form rather than about transmitting
 *   functional understanding of the catastrophe's meaning.
 *
 * KEY AGENTS:
 *   - Practitioner Bound by Form (powerless/identity_locked): Bears extraction of adaptive capacity; identity constituted through ritual fidelity. Primary victim.
 *   - Intergenerational Transmitter (moderate/constrained): Parent or elder charged with teaching fixed ritual form despite environmental change. Experiences mixed coordination and extraction.
 *   - Religious Authority Structure (institutional/arbitrage): Primary beneficiary. Benefits from institutional role continuity and boundary control over legitimate interpretation.
 *   - Adaptive Ritual Innovation Network (organized/mobile): Organized agents developing parallel adaptive practices. See form-preservation as temporary problem with sunset.
 *   - Secular Institutional Memory Systems (institutional/arbitrage): Museums and archives treating ritual as historical object. Piton perspective — performative preservation through institutional inertia.
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangement as immutable law of memory preservation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.68).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission: Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '2c02c432-424c-431e-940e-7a38cad882d1').
narrative_ontology:cs_kernel_codification('2c02c432-424c-431e-940e-7a38cad882d1', distributed).
narrative_ontology:cs_authority_grounding('2c02c432-424c-431e-940e-7a38cad882d1', practice).
narrative_ontology:cs_interpretation_layer_present('2c02c432-424c-431e-940e-7a38cad882d1').
narrative_ontology:cs_reading_relation('2c02c432-424c-431e-940e-7a38cad882d1', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('2c02c432-424c-431e-940e-7a38cad882d1', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('2c02c432-424c-431e-940e-7a38cad882d1', foundational, symbolic_form_inseparable_from_memory_content).
narrative_ontology:cs_axiom_status(symbolic_form_inseparable_from_memory_content, holdable).
narrative_ontology:cs_axiom_grounding('2c02c432-424c-431e-940e-7a38cad882d1', symbolic_form_inseparable_from_memory_content, deontological).
narrative_ontology:cs_axiom('2c02c432-424c-431e-940e-7a38cad882d1', foundational, fidelity_to_ancestral_practice_is_moral_obligation).
narrative_ontology:cs_axiom_status(fidelity_to_ancestral_practice_is_moral_obligation, holdable).
narrative_ontology:cs_axiom_grounding('2c02c432-424c-431e-940e-7a38cad882d1', fidelity_to_ancestral_practice_is_moral_obligation, deontological).
narrative_ontology:cs_reference_frame('2c02c432-424c-431e-940e-7a38cad882d1', ancestral_form_continuity).
narrative_ontology:cs_drift_state('2c02c432-424c-431e-940e-7a38cad882d1', contemporary_pluralistic_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2c02c432-424c-431e-940e-7a38cad882d1', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_identity_continuity).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, religious_authority_structures).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptive_capacity).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, contextual_relevance).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, intergenerational_reinterpretation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTITIONER BOUND BY FORM (SNARE) — Community member whose identity is constituted through ritual practice. Cannot reinterpret or adapt symbolic forms without ceasing to be a legitimate member. Exit would require abandoning communal identity. High suppression through identity fusion; no material exit option and no cognitive frame permitting reinterpretation. Experiences constraint as extraction of adaptive capacity to preserve ancestral form.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: INTERGENERATIONAL TRANSMITTER (TANGLED ROPE) — Parent or elder responsible for teaching ritual to the next generation. Experiences genuine coordination function (preserving communal memory through transmission) alongside extraction of flexibility (must teach fixed form despite environmental change). Constrained: can deviate slightly but risks community sanction; can exit community but loses family/cultural bonds. Moderate extractiveness because some agency exists within the constraint.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RELIGIOUS AUTHORITY STRUCTURE (ROPE) — Institutional beneficiary (clergy, guardians of sacred tradition). Benefits from symbolic form preservation: maintains institutional authority, controls interpretation boundaries, ensures continuity of institutional role. Experiences constraint as pure coordination: the ritual's form IS the mechanism through which institutional knowledge is transmitted and authority is maintained. Arbitrage exit options: can reinterpret symbolism within institutional framework without losing institutional position. Low experienced extraction because extraction runs toward this actor.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADAPTIVE RITUAL INNOVATION NETWORK (SCAFFOLD) — Organized subgroup (progressive clergy, scholars, cultural preservationists) developing parallel ritual practices that maintain symbolic continuity while incorporating contextual reinterpretation. See the form-preservation constraint as a temporary bottleneck being solved through innovation: creating new rituals alongside traditional ones, generating 'living tradition' frameworks that permit both preservation and adaptation. Mobile exit options (can practice new forms or traditional forms); sunset logic applies as adaptive practices accumulate legitimacy.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: SECULAR INSTITUTIONAL MEMORY SYSTEMS (PITON) — Museums, archives, academic departments treating ritual as historical object rather than living practice. See symbol-form preservation as performative: the constraint persists through institutional inertia (museums preserve ritual objects; academics document ritual forms) despite minimal functional role in actual memory transmission. Theater ratio high (0.7+) because the preservation is theatrical documentation, not living transmission. Degraded from original coordinating function (ritual transmission of knowledge) to vestigial archival function.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal vantage, the constraint appears as an immutable law: symbols ARE the forms through which collective memory persists; change the symbol and you have lost the memory; therefore symbol fixity is intrinsic to the function. However, structural data reveals beneficiaries (religious authority) and victims (adaptive capacity), indicating this is a false summit — naturalization of a contingent institutional arrangement, not a law of nature.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__symbol_continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The reading extracts adaptive capacity from practitioners who must teach fixed form despite environmental change. However, extraction is not total because genuine coordination function exists: symbolic form continuity does transmit collective memory across generations, a legitimate communal good. The constraint would score higher (0.70+) if the coordination function were entirely performative, but evidence suggests real memory transmission occurs through ritual adherence. The extractiveness has risen over the interval (0.38 → 0.52) as performance emphasis has increased relative to meaning transmission — a shift toward theater without corresponding increase in coordination function. Suppression (0.68): High. The binding mechanism combines structural barriers (exit costs: family severance, community exclusion) with internalized barriers (identity fusion: practitioners cannot cognitively reframe the ritual without experiencing themselves as inauthentic). This is exactly the condition where suppression stays high even when structural barriers weaken. Theater ratio (0.58, rising to 0.68): Moderate-high. Contemporary ritual practice shows increasing performative character — ritual fidelity is emphasized as demonstration of loyalty and authenticity rather than as functional memory transmission. The rise from 0.35 to 0.58 indicates that what was once primarily functional transmission has accumulated increasing performative overlay.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how different structural positions produce dramatically different classifications of the same phenomenon. The practitioner experiences snare (trapped, identity-locked, high suppression, no exit). The transmitter experiences tangled rope (mixed coordination and extraction, constrained exit). The authority experiences rope (pure coordination from their vantage). The adaptive network experiences scaffold (temporary problem with sunset). The archive experiences piton (degraded institutional function). The civilizational observer risks mountain (naturalizing contingency). The perspectival gap reveals that symbol-continuity is not experienced uniformly — it is experienced as extraction by those constrained to preserve form, as coordination by those who benefit institutionally, and as a solvable problem by those with resources to develop alternatives. No single perspective is 'wrong' — all are accurate descriptions of the constraint from their specific structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position: their power level, exit options, and relationship to extraction flow. The practitioner bound by form (powerless/identity_locked) has high d (~0.89) because they are trapped structurally (community sanctions) AND identity-fused (cognitive exit blocked). The intergenerational transmitter (moderate/constrained) has moderate d (~0.65) — they can exit at material cost but face internal conflict about abandoning cultural responsibility. The religious authority (institutional/arbitrage) has low d (~0.15) — beneficiary with full reinterpretive agency within institutional framework. The adaptive network (organized/mobile) has low d (~0.30) — can exit the constraint by developing parallel practices. The analytical observer has canonical d for analytical (~0.72) but risks the oracle gap: their native instruments cannot detect the identity-lock mechanism that prevents other perspectives from exercising exit options they might have.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that the constraint genuinely coordinates (memory transmission IS a real communal good) while extracting (adaptive capacity IS sacrificed). This is the definitional tangled rope: hybrid coordination-extraction. The potential confusion is whether the symbol-continuity axiom (form-fixity is intrinsic to memory preservation) is a semantic truth (meaning IS form) or an institutional claim (institutions enforce form-fixity for control purposes). If semantic: the constraint is more rope-like (form-fixity is necessary coordination, not extraction). If institutional: the constraint is more snare-like (form-fixity is extractive control). The omega variables are designed to resolve this ambiguity through empirical and conceptual analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbol_semantic_binding_mechanism,
    'Is the constraint''s binding force semantic (meaning is inseparable from form) or institutional (meaning could be preserved in altered form, but authority structure enforces fixity)?',
    'Cross-cultural comparison of rituals with identical functional purpose but divergent symbolic forms; analysis of reinterpretation practices within communities that permit form variation while maintaining functional memory transmission; ethnographic evidence of whether practitioners perceive form-change as meaning-loss or as authorized adaptation.',
    'If semantic: symbol fixity is intrinsic coordination function (Rope from more perspectives). If institutional: symbol fixity is extractive enforcement mechanism (Snare from more perspectives). This resolves the false-summit ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(symbol_semantic_binding_mechanism, conceptual, 'Whether form-fixity is semantically intrinsic or institutionally enforced').

omega_variable(
    intergenerational_transmission_effectiveness,
    'Do rigid symbolic forms actually transmit catastrophe memory more effectively across generations than adaptive forms that maintain functional continuity while permitting contextual reinterpretation?',
    'Longitudinal ethnographic study of memory retention across 3+ generations in communities with form-rigid vs form-adaptive ritual traditions; analysis of what is actually remembered (meaning, emotional content, historical claim) vs what is forgotten (specific symbolic form, ritual details); correlation between form-rigidity and transmission failure rates.',
    'If rigid forms transmit more effectively: suppression is coordination necessity, not extraction. If adaptive forms perform equally or better: suppression is institutional preference, not functional requirement. This determines whether tangled_rope genuinely coordinates or merely disguises snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_effectiveness, empirical, 'Whether rigid symbolic forms improve intergenerational memory transmission').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the symbol-continuity reading''s core premise (form-fixity is intrinsic to memory preservation) logically foreclose the operational-competence reading (adaptive ritual form is compatible with memory preservation), or do both remain live positions held by different communities?',
    'Theoretical analysis: can a single normative framework hold both ''symbols must be fixed to preserve memory'' and ''symbols can adapt while preserving memory''? If both can be true under different interpretive conditions (different understandings of what ''preservation'' means, different contexts), the readings coexist. If one premise directly contradicts the other such that no framework could hold both, foreclosure applies.',
    'If coexist: different communities embody different legitimate readings (sibling = coexists_with). If foreclose: one reading rules out the other (sibling = forecloses). This determines the cs_structure.reading_relations value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether symbol-continuity and operational-competence readings logically foreclose or coexist').

omega_variable(
    committer_axiom_holdability,
    'Is the axiom ''symbolic_form_inseparable_from_memory_content'' genuinely holdable in contemporary scholarly discourse, or has it been substantially overridden by evidence of adaptive ritual transmission across cultures?',
    'Survey of contemporary religious studies, anthropological, and memory studies literature: what do scholars hold as the relationship between symbolic form and memory transmission? Has the axiom been formally abandoned in any major interpretive tradition?',
    'If holdable: axiom status = holdable (live claim in contemporary discourse). If overridden: axiom status = overridden (abandoned within scholarly tradition, but may persist in community belief). This determines cs_structure.axioms[].status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_axiom_holdability, conceptual, 'Whether the form-memory inseparability axiom remains holdable in contemporary scholarly tradition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_sym_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(catmem_sym_tr_t3, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(catmem_sym_tr_t6, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(catmem_sym_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(catmem_sym_be_t3, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(catmem_sym_be_t6, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 6, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(catmem_sym_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(catmem_sym_su_t3, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(catmem_sym_su_t6, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories model the contested kernel 'catastrophe_memory_transmission' with structurally distinct epsilon values and beneficiary/victim structures. Symbol-continuity reading (this file) emphasizes form-fixity as intrinsic function (ε=0.52, tangled rope). Operational-competence reading models adaptive form as compatible with transmission (ε=0.25, rope). Hybrid-embedded reading models irresolvable tension between both requirements (ε=0.68, tangled rope). All three are linked via network.affects_constraints. Each reading produces different classification profiles and beneficiary/victim structures because each reading embodies a different understanding of what ritual transmission requires.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
