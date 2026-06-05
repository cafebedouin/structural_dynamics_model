% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__hybrid_embedded_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__hybrid_embedded_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__hybrid_embedded_reading
 *   human_readable: Catastrophe Memory Transmission via Ritual (Hybrid Embedded Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint models the transmission of survival-critical,
 *   non-propositional knowledge through ritualized form. The core structural
 *   problem: operational competence (how to perform a survival-relevant
 *   action) is encoded not as explicit instructions but as embodied practice
 *   embedded in symbolic form. The ritual's symbolic structure encodes tacit
 *   knowledge — muscle memory, sensory cues, contextual triggers — that
 *   cannot be fully extracted into propositions without loss. This creates a
 *   permanent tension: practitioners must preserve form fidelity to preserve
 *   function, yet form inevitably accumulates rigidity, theatrical
 *   performance, and irrelevant ornament over generations. The constraint is
 *   instantiated in post-catastrophe contexts where transmission chains
 *   break: diaspora communities trying to preserve religious knowledge
 *   without original materials or spaces; traditions interrupted by
 *   repression or colonialism; knowledge holders facing death with no
 *   apprentices. The hybrid-embedded reading holds that form and function are
 *   co-constitutive but sometimes separable at cost. Altering form degrades
 *   function, but this degradation is not always catastrophic — it depends on
 *   which dimensions of form encode which dimensions of function. Some ritual
 *   elements are load-bearing (encode survival-critical tacit knowledge);
 *   others are ornamental (encode tradition identity or theological meaning).
 *   The reading does not claim pure inseparability (mountain) or pure
 *   functional reducibility (scaffold), but rather a tension that different
 *   practitioners and traditions resolve differently.
 *
 * KEY AGENTS:
 *   - Practitioner locked in form (powerless/identity_locked): bears the cost of form fidelity; cannot exit without identity dissolution; trapped in the constraint despite structural mobility
 *   - Knowledge-holding community (organized/constrained): the custodial group that perceives pure coordination; benefits from form stability; experiences suppression as necessary structure
 *   - Practitioner in continuity crisis (moderate/constrained): faces forced choice between form and function when transmission breaks; experiences both extraction and coordination
 *   - Institutionalized tradition (institutional/arbitrage): the formal holder with mobility and agency; benefits from making knowledge transmission auditable and authority concentrated
 *   - Analytical observer (analytical/analytical): risks naturalizing contingent institutional forms as inherent to non-propositional knowledge itself
 *   - Documentation/formalization movement (organized/mobile): scaffold agents building alternative transmission pathways through explicit instruction and recording
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__hybrid_embedded_reading, 0.28).
domain_priors:suppression_score(catastrophe_memory_transmission__hybrid_embedded_reading, 0.42).
domain_priors:theater_ratio(catastrophe_memory_transmission__hybrid_embedded_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__hybrid_embedded_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__hybrid_embedded_reading, "Catastrophe Memory Transmission via Ritual (Hybrid Embedded Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__hybrid_embedded_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__hybrid_embedded_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__hybrid_embedded_reading, '5c7ec385-b53c-4673-bc24-bed7890de069').
narrative_ontology:cs_kernel_codification('5c7ec385-b53c-4673-bc24-bed7890de069', distributed).
narrative_ontology:cs_authority_grounding('5c7ec385-b53c-4673-bc24-bed7890de069', practice).
narrative_ontology:cs_interpretation_layer_present('5c7ec385-b53c-4673-bc24-bed7890de069').
narrative_ontology:cs_reading_relation('5c7ec385-b53c-4673-bc24-bed7890de069', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c7ec385-b53c-4673-bc24-bed7890de069', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_axiom('5c7ec385-b53c-4673-bc24-bed7890de069', foundational, form_and_function_co_constitutive).
narrative_ontology:cs_axiom_status(form_and_function_co_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('5c7ec385-b53c-4673-bc24-bed7890de069', form_and_function_co_constitutive, empirically_contingent).
narrative_ontology:cs_axiom('5c7ec385-b53c-4673-bc24-bed7890de069', foundational, partial_separability_possible_at_cost).
narrative_ontology:cs_axiom_status(partial_separability_possible_at_cost, holdable).
narrative_ontology:cs_axiom_grounding('5c7ec385-b53c-4673-bc24-bed7890de069', partial_separability_possible_at_cost, empirically_contingent).
narrative_ontology:cs_reference_frame('5c7ec385-b53c-4673-bc24-bed7890de069', embodied_knowledge_preservation_through_form).
narrative_ontology:cs_drift_state('5c7ec385-b53c-4673-bc24-bed7890de069', contemporary_documentation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5c7ec385-b53c-4673-bc24-bed7890de069', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, ritual_practitioner_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__hybrid_embedded_reading, intergenerational_knowledge_holders).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, continuity_breakage_risk).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__hybrid_embedded_reading, practitioners_forced_into_formal_transmission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRACTITIONER LOCKED IN FORM (SNARE) — A practitioner whose identity is constituted through embodied ritual participation cannot exit without losing the identity-fused knowledge itself. The form IS the knowledge; abandoning the form means abandoning access to the operational capacity embedded within it. Identity-locked exit reflects that the binding is cognitive (identity fusion with practice) rather than structural (material barriers), yet the agent cannot perceive mutability from within their own frame. High effective extraction because the practitioner's entire epistemic authority depends on form fidelity.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: KNOWLEDGE-HOLDING COMMUNITY (ROPE) — Organized practitioners perceive the constraint as pure coordination: the ritual form is the mechanism by which embodied operational knowledge is encoded, transmitted, and preserved across generations. The constraint solves a genuine problem — how to preserve tacit knowledge that cannot be reduced to explicit propositions. Suppression exists (forms are rigid, innovation is constrained) but the community experiences this as necessary structure, not extraction. The beneficiary and victim roles are nearly aligned — the same agents both coordinate and bear the cost of form rigidity.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRACTITIONER FACING CONTINUITY CRISIS (TANGLED ROPE) — When transmission breaks (lineage dies, ritual context changes, access to materials becomes impossible), practitioners face forced adaptation. They must either rigidly preserve form at the cost of functional degradation, or modify form to preserve function, risking that the modification invalidates the knowledge. Both extraction (forced choice between form and function) and genuine coordination (the constraint still enables intergenerational knowledge transfer) are present. The practitioner bears extraction costs while also benefiting from the transmission mechanism.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: INSTITUTIONALIZED TRADITION (ROPE) — A formal institutional holder (monastery, temple, ceremonial organization) experiences the constraint as pure coordination with high exit mobility. The institution can modify forms, update materials, and adapt practices while maintaining the knowledge-transmission function. The constraint benefits the institution by concentrating authority and making knowledge transmission documentable, auditable, and institutionalizable. Low or negative experienced extraction from the institutional perspective because the institution has agency and benefits from form stability.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / EMBODIED KNOWLEDGE SUBSTRATE (MOUNTAIN) — From a civilizational perspective grounded in cognitive science and the structure of tacit knowledge, the constraint appears as an immutable feature of how non-propositional knowledge is encoded and transmitted. Embodied, procedural knowledge cannot be fully extracted into explicit propositions without losing critical dimensions. The ritual form is not arbitrary — it is the necessary substrate for encoding survival-critical information in a non-semantic channel. This perspective risks false-summing: naturalizing what may be partly constructed institutional arrangements (specific forms, specific authorities) as inherent to knowledge transmission itself.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: DOCUMENTATION MOVEMENT (SCAFFOLD) — Modern attempts to document ritual procedures, create how-to manuals, produce video recordings, and teach through explicit instruction see the constraint as a temporary gap in transmission capacity. The scaffold is the belief that embodied knowledge can be partially extracted into explicit propositional form (recipes, diagrams, video demonstrations) and that this extraction process will eventually reduce dependence on ritual-form fidelity. The sunset logic: as explicit documentation matures, practitioners can learn from manuals rather than purely from embodied practice. Theater ratio is low here because the intention is functional competence, not ritual performance.
constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(catastrophe_memory_transmission__hybrid_embedded_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(catastrophe_memory_transmission__hybrid_embedded_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Low-to-moderate. The constraint does encode genuine coordination (embodied knowledge transmission requires practice and form) but also extraction (practitioners are locked into form fidelity even when function would be preserved through modification; knowledge holders can exploit the inseparability of form and function to enforce compliance). The extractiveness rises slightly over the interval (0.18 → 0.28) because documentation and formalization attempts increase the visibility of what can be extracted vs what cannot, making the boundaries of form-fidelity seem more arbitrary and the suppression more explicit. Suppression (0.42): Moderate. Practitioners face barriers to exit: identity fusion (the embodied knowledge IS their identity), community sanctions (abandoning form is seen as apostasy or incompetence), lack of alternative transmission mechanisms (prior to documentation movement), and the genuine cognitive impossibility of fully extracting tacit knowledge into explicit form. But suppression is not total — some practitioners do modify forms, some traditions do innovate, and the documentation movement is building alternatives. Theater ratio (0.35): Low. The constraint's function is genuine — embodied knowledge transmission requires practice and form — so the ritual is not purely performative. However, over time, ornamental elements accumulate. The theater ratio rises slightly because modernization and diaspora create conditions where practitioners must justify form continuation explicitly, making the gap between essential form and accumulated ornament more visible. Tangled rope classification reflects that both coordination (genuine knowledge transmission function) and extraction (form-fidelity suppression) are present and active.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap lies between the powerless practitioner's experience (snare: identity-locked, maximum extraction) and the institutional holder's experience (rope: mobile, beneficiary, pure coordination). Both agents occupy the same constraint but perceive it entirely differently because their exit options and structural positions differ so dramatically. The organized community perspective (rope) aligns partially with the institution but differs in time horizon and scope — the community is generationally bounded while the institution can persist indefinitely. The scaffold perspective (documentation movement) sees the constraint as temporary, resolvable through technology and explicit instruction — a view not shared by agents with identity-fusion to embodied practice. The mountain analytical perspective risks false-summing: what looks like an immutable law of tacit knowledge may be partly a contingent institutional arrangement (specific forms, specific authorities, specific prohibitions on innovation) that reinforces the appearance of inseparability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent position and exit capacity. The identity-locked practitioner has high d (0.89–0.95) because they are a victim of the constraint (their identity is bound to form preservation) despite structural mobility (they could, in principle, leave the community or stop practicing). The institutional beneficiary has low d (0.15–0.20) because they benefit from the constraint and have arbitrage options (can modify forms, outsource practice, institutionalize procedures). The knowledge-holding community has mid-d (0.45–0.50) because they are both beneficiary (coordinator) and victim (bound by form rigidity); the constraint serves them but also constrains them. The organized scaffold agent has moderate-low d (0.35–0.40) because they are pushing against the constraint (trying to make form separable from function) and have mobile/exit options (can develop alternative transmission mechanisms). The derived directionality values feed into the sigmoid f(d) to produce effective extractiveness chi for each perspective, which then determines classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by showing that the constraint exhibits genuine coordination (embodied knowledge transmission) alongside genuine extraction (form-fidelity suppression and identity-lock). The tangled_rope classification captures both. The mandatrophy would emerge if we tried to collapse this into a single type: calling it pure rope ignores the extraction; calling it pure snare ignores the coordination. The reading's value is to clarify what is being coordinated (preservation of non-propositional knowledge across generational and catastrophic gaps) and what is being extracted (practitioners' freedom to innovate and practitioners' epistemic authority being concentrated in form-custodians).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    form_function_separability,
    'Can operational competence be decoupled from ritual form through documentation, explicit instruction, or digitization?',
    'Comparative study: practitioners trained via explicit documentation vs embodied apprenticeship; measurement of competence retention, error rates, and adaptive capacity across both cohorts; longitudinal tracking of knowledge degradation when form is altered while function is preserved',
    'If separable: the constraint becomes Rope (pure coordination) and the scaffold is real (documentation is viable exit path). If inseparable: the constraint remains Tangled Rope (extraction through forced form fidelity) and mountain-substrate persists (embodied knowledge has physical/cognitive basis that cannot be transcribed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(form_function_separability, empirical, 'Separability of operational function from ritual form').

omega_variable(
    tacit_knowledge_boundedness,
    'Is the non-propositional knowledge embedded in ritual form fundamentally bounded (cannot be fully extracted), or merely difficult to extract with current techniques?',
    'Analysis of failed extraction attempts; identification of knowledge loss signatures when forms are simplified or abstracted; comparison with other embodied-knowledge domains (craft skills, athletic performance, medical diagnostics); measurement of Polanyi-gap persistence across multiple extraction methodologies',
    'If fundamentally bounded: the mountain substrate is real, forms are irreducible, and practitioners are locked into form fidelity (identity_locked justifies itself). If merely difficult: the scaffold is more viable and the identity-lock is contingent on current technology and pedagogy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tacit_knowledge_boundedness, empirical, 'Whether tacit ritual knowledge is fundamentally indissociable from form').

omega_variable(
    reading_kernel_ambiguity,
    'Is this reading (hybrid: form and function co-constitutive but sometimes separable at cost) the correct framing, or do the sibling readings (pure symbol continuity vs pure operational competence) more accurately describe the kernel?',
    'Ethnographic documentation of what practitioners claim about form-function relationship; measurement of how practitioners actually handle form modification in crisis contexts; analysis of whether practitioners invoke form-based or function-based justifications for preserving or altering ritual; comparison across multiple catastrophe-memory traditions (Judaism post-diaspora, Catholic post-Vatican II, Buddhist post-Cultural Revolution, Islamic post-colonialism) to identify consistent patterns in how form-function debates are resolved',
    'If symbol-continuity reading is correct: the constraint is mountain (form itself is the knowledge; modifying form invalidates transmission). If operational-competence reading is correct: the constraint is rope (function is all that matters; form is a vehicle that can be replaced). If this hybrid reading is correct: the constraint is tangled_rope with ongoing tension between form preservation and functional adaptation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Which reading of the catastrophe-memory kernel is structurally accurate').

omega_variable(
    identity_lock_vs_contingent_suppression,
    'Is the powerless practitioner''s binding to ritual form a genuine identity lock (cognitive fusion making exit unthinkable) or a contingent suppression mechanism (external barriers that could be removed)?',
    'Ethnographic study of practitioner narratives about form modification; measurement of how practitioners respond when form-change is presented as acceptable (e.g., institutional permission to innovate); analysis of whether practitioners report identity-dissolution risk or merely cost-of-exit risk when contemplating form abandonment; longitudinal tracking of practitioners who do abandon form—do they report identity loss or merely career disruption',
    'If genuine identity lock: the snare perspective from the powerless agent''s position is accurate (exit would dissolve their epistemic identity). If contingent suppression: the classification downgrades from snare to constrained, and the agent''s agency increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_contingent_suppression, empirical, 'Whether practitioner binding to form is identity-based or structurally-suppressed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__hybrid_embedded_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cmt_hybrid_theater_t0, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cmt_hybrid_theater_t50, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement(cmt_hybrid_theater_t100, catastrophe_memory_transmission__hybrid_embedded_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(cmt_hybrid_extract_t0, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cmt_hybrid_extract_t50, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 50, 0.24).
narrative_ontology:measurement(cmt_hybrid_extract_t100, catastrophe_memory_transmission__hybrid_embedded_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cmt_hybrid_suppress_t0, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(cmt_hybrid_suppress_t50, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(cmt_hybrid_suppress_t100, catastrophe_memory_transmission__hybrid_embedded_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__hybrid_embedded_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__hybrid_embedded_reading, catastrophe_memory_transmission__operational_competence_reading).

% DUAL FORMULATION NOTE:
% The three constraints in the catastrophe_memory_transmission kernel family (symbol_continuity, operational_competence, hybrid_embedded) are distinct readings of a single contested commitment, not independent constraints. Each has its own ε value, beneficiary/victim structure, and perspectival dynamics because each reading produces structurally different claims about what preservation means. The hybrid_embedded reading sits between the two extremes, claiming co-constitutivity with partial separability. All three readings are live positions in actual traditions; they coexist across different communities and within communities during crisis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__hybrid_embedded_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
