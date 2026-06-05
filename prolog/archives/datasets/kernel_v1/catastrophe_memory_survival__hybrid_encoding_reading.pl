% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Ritual Dual-Register Encoding: Symbol and Competence in Catastrophe Survival
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint models the hybrid-register reading of how ritual encodes
 *   and transmits knowledge across catastrophic disruption. The core claim:
 *   ritual operates simultaneously on two distinct registers — symbolic
 *   boundary-maintenance (group identity, normative coherence, sacred
 *   narrative) and embedded practical knowledge (resource protocols, seasonal
 *   timing, kinship structures, adaptation strategies) — and community
 *   survival depends on BOTH registers functioning together in a single
 *   coherent practice. This reading differs from sibling readings that
 *   emphasize one register as primary. The hybrid reading is the most complex
 *   and the least theoretically resolved, creating extraction at the level of
 *   analysis itself: scholars and institutional preservationists are forced
 *   by disciplinary frameworks to choose which register is 'really'
 *   operative, suppressing the hybrid structure that actual communities
 *   maintain.
 *
 * KEY AGENTS:
 *   - Practicing Communities: Primary beneficiary and secondary target (moderate/constrained) — benefit from the coherent dual encoding, constrained by resource and labor costs of maintaining both registers
 *   - Knowledge Bearers (Elders/Specialists): Primary target (powerless/trapped) — face external pressure to disarticulate registers or abandon practice entirely; extracted by incompatible institutional demands
 *   - Theoretical Analysts: Secondary target/victim (powerless/trapped) — forced by disciplinary frameworks to classify ritual into binary categories, suppressing the hybrid reality they observe
 *   - Academic Disciplines: Primary beneficiary (institutional/arbitrage) — maintain disciplinary theater by enforcing binary classification; extract authority and credentialing from the framework itself
 *   - Institutional Preservationists (UNESCO, Heritage Organizations): Mixed (powerful/mobile) — genuinely preserve knowledge but extract control over narrative and methodology; often disarticulate registers in documentation
 *   - State/Modernization Agents: Extractors (institutional/arbitrage) — create pressure for register separation through education, religious conversion, or rationalization policies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.52).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Ritual Dual-Register Encoding: Symbol and Competence in Catastrophe Survival").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__hybrid_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, 'bd7d5599-0e2a-4ab2-b299-89459d5791a0').
narrative_ontology:cs_kernel_codification('bd7d5599-0e2a-4ab2-b299-89459d5791a0', distributed).
narrative_ontology:cs_authority_grounding('bd7d5599-0e2a-4ab2-b299-89459d5791a0', practice).
narrative_ontology:cs_interpretation_layer_present('bd7d5599-0e2a-4ab2-b299-89459d5791a0').
narrative_ontology:cs_reading_relation('bd7d5599-0e2a-4ab2-b299-89459d5791a0', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd7d5599-0e2a-4ab2-b299-89459d5791a0', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('bd7d5599-0e2a-4ab2-b299-89459d5791a0', foundational, symbolic_and_competence_inseparable).
narrative_ontology:cs_axiom_status(symbolic_and_competence_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('bd7d5599-0e2a-4ab2-b299-89459d5791a0', symbolic_and_competence_inseparable, empirically_contingent).
narrative_ontology:cs_axiom('bd7d5599-0e2a-4ab2-b299-89459d5791a0', foundational, institutional_binary_classification_suppresses_hybrid).
narrative_ontology:cs_axiom_status(institutional_binary_classification_suppresses_hybrid, holdable).
narrative_ontology:cs_axiom_grounding('bd7d5599-0e2a-4ab2-b299-89459d5791a0', institutional_binary_classification_suppresses_hybrid, empirically_contingent).
narrative_ontology:cs_reference_frame('bd7d5599-0e2a-4ab2-b299-89459d5791a0', integrated_dual_register_practice).
narrative_ontology:cs_drift_state('bd7d5599-0e2a-4ab2-b299-89459d5791a0', contemporary_disciplinary_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bd7d5599-0e2a-4ab2-b299-89459d5791a0', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, practice_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, theoretical_analysts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTICING COMMUNITY (TANGLED ROPE) — Communities embedded in ritual practice experience genuine coordination: the ritual encodes survival knowledge (water sourcing, seasonal timing, kinship protocols) while simultaneously maintaining group identity and boundary-norms. Both functions are operative and required. Communities benefit from the coherence of dual encoding but are constrained by the labor and specificity required to maintain both registers simultaneously. The constraint is experienced as functional necessity, not extraction.
constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: KNOWLEDGE BEARERS UNDER PRESSURE (SNARE) — Individual knowledge carriers (ritual specialists, elders) face asymmetric extraction when external pressures (state schooling, religious conversion, modernization narratives) demand they disarticulate the two registers or abandon the practice entirely. The extraction mechanism is suppression of the hybrid encoding itself — forced choice between 'keeping the symbols' and 'preserving the competence' as if they were separable. Knowledge bearers become trapped between irreconcilable demands.
constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: INSTITUTIONAL MEMORY STEWARDS (ROPE) — Religious or cultural institutions that formally codify and transmit the ritual experience coordination benefits from the encoding: a single coherent practice yields both symbolic meaning AND practical knowledge without requiring separate transmission pathways. This is an efficient coordination solution. Institutions with arbitrage options (ability to select which communities adopt the ritual, ability to document and adapt transmission) experience the constraint as pure coordination.
constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYSTS FORCED INTO BINARY CHOICE (SNARE) — Theoretical analysts, historians, and anthropologists studying the ritual become trapped by disciplinary pressures to classify the ritual as EITHER 'primarily symbolic' OR 'primarily a knowledge transmission mechanism.' The dual-register reality is suppressed by the framework of analysis itself. Analysts who try to describe the hybrid encoding face accusations of conflating categories or lacking theoretical clarity. The extraction is epistemic: forced binarization prevents accurate description.
constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC DISCIPLINARY FRAMEWORKS (PITON) — The scholarly apparatus (anthropology, religious studies, folklore departments) maintains the binary classification (symbol vs. competence) as a professional standard not because it maps the real structure but because it produces legible publications and grants. The framework is performative: it appears to require serious analytical labor but the labor is mostly in force-fitting data into the binary, not in understanding the hybrid encoding. Theater ratio is high because the disciplinary theater (methodological purity, categorical clarity) actively obscures the structure being studied.
constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: PRESERVATIONIST INSTITUTIONS WITH POWER (TANGLED ROPE) — Large organizations (UNESCO, national heritage agencies, major universities) with resources to document and preserve rituals experience mixed coordination and extraction. They genuinely coordinate the preservation function (otherwise the knowledge would be lost to modernization pressures). But they also extract: they control the narrative of what 'counts' as preserved, they get credentialed authority over the ritual's meaning, and they often disarticulate the two registers in their preservation methodology (digitizing the symbols separately from the embodied competence). The constraint is functional but asymmetric.
constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the dual-register structure appears to be an immutable feature of human ritual cognition itself: symbolic thought and embodied competence are neurologically and evolutionarily inseparable, making the hybrid encoding a natural law of how humans encode survival knowledge through collective practice. However, this perspective risks false-summit naturalization — the real structure is that institutional pressures CREATE the extraction by forcing disarticulation of what communities hold as unified.
constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_survival__hybrid_encoding_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, TR),
    TR >= 0.70.

:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The hybrid-register structure provides genuine coordination benefits (one practice yields two essential functions), reducing the base extraction below what would be measured if symbol and competence were truly separate mechanisms. However, external pressures force disarticulation, creating extraction at the level of knowledge bearers and analysts. The value reflects that extractiveness is not inherent to the dual encoding itself but emerges when institutional frameworks suppress the integration. Suppression (0.52): Moderate-high. The suppression operates at three levels: (1) state/modernization pressures on communities to abandon ritual or choose one register, (2) disciplinary pressures on analysts to classify into binary categories, (3) methodological pressures on preservationists to document symbol and competence separately. All three suppress the hybrid structure that communities maintain. Theater ratio (0.58): Moderate-high. Academic frameworks that enforce binary classification are substantially performative — the theoretical labor is mostly in force-fitting observation into categories, not in understanding the actual structure. Disciplinary prestige derives from categorical clarity rather than descriptive accuracy. Measurements show rising theater (0.42→0.58) as academic frameworks become more institutionalized and less responsive to evidence of hybrid functioning.
 *
 * PERSPECTIVAL GAP:
 *   The practicing community experiences tangled rope (genuine coordination with real constraints). Knowledge bearers experience snare (trapped by incompatible external demands). Analysts experience snare (forced binary choice suppresses what they can accurately describe). Institutional preservationists experience tangled rope (genuine preservation function but asymmetric control). The academic disciplines experience rope (pure coordination of professional standards). The naturalized mountain perspective risks treating cognitive inseparability as justification for the institutional extraction. The perspectival gap reveals that the constraint is not primarily about ritual cognition but about institutional frameworks that suppress hybrid analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural relationship to the constraint. Practice communities are beneficiaries (low d, low effective extraction) but constrained by maintenance costs. Knowledge bearers are victims with no exit (high d, high effective extraction). Analysts are victims forced into binary classification (high d, snare classification from powerless position). Institutional preservationists are complex: they benefit from credentialing authority but also from genuine preservation need — the beneficiary/victim split within a single institutional actor creates the tangled rope classification. The analytical observer risks collapsing to a naturalized mountain that misses the institutional extraction mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLUTION: The mandatrophy is resolved by recognizing that this hybrid reading forecloses neither sibling but provides the integrative framework within which both coexist. Symbol_survival and competence_transmission appear as incompatible only under the false assumption that function and meaning are separable. Under the hybrid reading, they are aspects of a single structure. The beneficiary set (practicing communities, institutional preservationists) gains from the clarity that the constraint's stability depends on maintaining the hybrid encoding. The victim set (knowledge bearers, analysts forced to choose) is identified as those harmed by institutional frameworks that suppress integration. The constraint does not itself generate mandatrophy; instead, it reveals where mandatrophy emerges: in the academic frameworks that force binary choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    register_separation_cost,
    'What is the true cost to community survival when the symbolic and competence registers are forced apart by external pressure?',
    'Longitudinal comparative study of communities that maintained hybrid encoding vs. those forced to specialize in one register; measurement of knowledge retention, transmission success, and cultural continuity metrics across generations',
    'If separation cost is high: the constraint is a snare masquerading as academic analysis. If separation is survivable: the binary classification is a legitimate analytical choice and the victim set is smaller. If separation is selective (some communities survive, others don''t): victim set includes only those below viability threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(register_separation_cost, empirical, 'Cost of forced separation of symbolic and competence registers').

omega_variable(
    reading_kernel_contest,
    'Is this a reading of a single kernel (catastrophe_memory_survival with three competing interpretations) or three distinct constraints masquerading as interpretations of one kernel?',
    'Examine whether all three readings agree on the base structural claim (ritual encodes post-catastrophe knowledge) while disagreeing on what the ritual''s PRIMARY FUNCTION is. If all three accept the base claim and differ only on function emphasis, it is one kernel with three readings. If the readings disagree on whether the base claim is true, they are separate constraints.',
    'If one kernel: the three readings coexist and influence each other; hybrid_encoding is the integrative reading. If three constraints: each has its own ε value; they network together but are not readings of the same claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Whether this is one kernel with three readings or three separate constraints').

omega_variable(
    binary_classification_necessity,
    'Does rigorous academic analysis require binary categorization of ritual function, or is the binary a disciplinary artifact that could be replaced with non-exclusive multi-register models?',
    'Examination of contemporary practice theory, phenomenological anthropology, and embodied cognition frameworks that do not require binary choice; assessment of whether these frameworks produce more accurate predictions about community survival and knowledge retention than binary frameworks',
    'If binary is necessary: analyst-victims are experiencing legitimate theoretical constraints, not extraction. If binary is artifact: the extractive mechanism is specifically the disciplinary insistence on non-overlapping categories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binary_classification_necessity, conceptual, 'Whether binary classification is epistemically required or a disciplinary artifact').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cms_hybrid_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cms_hybrid_tr_t3, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 3, 0.51).
narrative_ontology:measurement(cms_hybrid_tr_t6, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(cms_hybrid_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cms_hybrid_be_t3, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 3, 0.33).
narrative_ontology:measurement(cms_hybrid_be_t6, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 6, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(cms_hybrid_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cms_hybrid_su_t3, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 3, 0.46).
narrative_ontology:measurement(cms_hybrid_su_t6, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel catastrophe_memory_survival. Sibling readings are declared as separate constraints with their own ε values and beneficiary/victim structures. All three readings network together and influence each other through the reading_relations and axioms declared in cs_structure. The hybrid reading is the integrative position; the sibling readings represent the disciplinary forces that create extraction by forcing disarticulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_survival__hybrid_encoding_reading, analytical, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
