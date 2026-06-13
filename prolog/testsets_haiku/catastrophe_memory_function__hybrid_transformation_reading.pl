% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__hybrid_transformation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__hybrid_transformation_reading, []).

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
 *   constraint_id: catastrophe_memory_function__hybrid_transformation_reading
 *   human_readable: Ritual Hybrid: Mourning-Practice + Survival-Competence Encoding
 *   domain: religious/ritual/collective_memory
 *
 * SUMMARY:
 *   The seder and related post-catastrophe commemoration rituals encode a
 *   hybrid function: they preserve the memory of historical catastrophe
 *   through required mourning-practice (bitter herbs, mandatory retelling,
 *   annual re-trauma of loss) while simultaneously transmitting
 *   survival-competence through the ritual's structure (distributed
 *   authority, question-based decision-making, the pattern of 'tell the story
 *   to your descendant' which rehearses how institutional memory persists
 *   without centralization). This story treats the ritual as a single
 *   constraint that fuses mourning and survival-competence — both functions
 *   are structurally entangled and cannot be separated without changing how
 *   each function operates. The hybrid reading asserts that the ritual's
 *   architectural coherence depends on this fusion: the mourning-practice
 *   creates participation obligation that ensures the survival-competence
 *   encoding reaches descendants; the survival-competence structure
 *   legitimizes the mourning-practice as adaptive rather than merely
 *   commemorative. This is ONE reading of a contested kernel; other readings
 *   separate mourning-practice (as primary, treating survival-competence as
 *   secondary or post-hoc interpretation) or survival-competence (as primary,
 *   treating mourning-practice as nostalgic overlay). The three readings
 *   coexist as live positions held by different interpretive communities.
 *
 * KEY AGENTS:
 *   - ritual_practitioners: Participate in the dual-function ritual; bear identity-lock and participation obligation; benefit from memory preservation and from absorbing survival-competence encoded in the ritual structure.
 *   - ritual_authority_transmitters: Administer the ritual, define what it encodes (both mourning and survival-competence), enforce the hybrid reading as canonical. They benefit from authority to interpret the ritual and from the participation commitment it generates.
 *   - community_memory_carriers: Hold and transmit the narrative and material anchors of the catastrophe. They benefit from the mourning-practice's role in preserving memory and from the institutional structures the survival-competence encoding protects.
 *   - post_catastrophe_generations: Inherit both mourning obligation (obligatory commemoration of a catastrophe they did not experience) and survival-competence encoding (absorbing adaptive mechanisms through ritual performance). They are identity-locked and powerless to exit without severance from the group.
 *   - secular_alternative_memory_frameworks: Excluded from defining the ritual's function; would argue that memory-preservation and institutional transmission can be separated from identity-locked ritual performance.
 *   - heterodox_interpreters: Excluded from canonical authority; would argue for single-function readings that permit looser performance norms and optional participation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__hybrid_transformation_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_function__hybrid_transformation_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_function__hybrid_transformation_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(catastrophe_memory_function__hybrid_transformation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__hybrid_transformation_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__hybrid_transformation_reading, "Ritual Hybrid: Mourning-Practice + Survival-Competence Encoding").
narrative_ontology:topic_domain(catastrophe_memory_function__hybrid_transformation_reading, "religious/ritual/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_function__hybrid_transformation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__hybrid_transformation_reading, '03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc').
narrative_ontology:cs_kernel_codification('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', fixed_text).
narrative_ontology:cs_authority_grounding('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', lineage).
narrative_ontology:cs_interpretation_layer_present('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc').
narrative_ontology:cs_reading_relation('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', catastrophe_memory_function__survival_competence_reading, coexists_with).
narrative_ontology:cs_axiom('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', foundational, mourning_survival_structural_entanglement).
narrative_ontology:cs_axiom_status(mourning_survival_structural_entanglement, holdable).
narrative_ontology:cs_axiom_grounding('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', mourning_survival_structural_entanglement, instrumental).
narrative_ontology:cs_axiom('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', foundational, ritual_dual_function_inseparability).
narrative_ontology:cs_axiom_status(ritual_dual_function_inseparability, holdable).
narrative_ontology:cs_axiom_grounding('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', ritual_dual_function_inseparability, conventional).
narrative_ontology:cs_reference_frame('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', original_dual_function_design).
narrative_ontology:cs_drift_state('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', contemporary_secular_alternative_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('03d8bb2c-4cb6-46e5-bbf1-677cfe06f2bc', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, community_continuity_across_catastrophe).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__hybrid_transformation_reading, ritual_authority_transmitters).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__hybrid_transformation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_function__hybrid_transformation_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).
:- end_tests(catastrophe_memory_function__hybrid_transformation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the ritual does generate genuine coordination benefits (memory preservation, transmission of survival-competence) but also extracts identity-lock and participation burden from post-catastrophe generations. The extraction is not pure rent-seeking; it rides on a real coordination function. Suppression is low-moderate (0.28) because the ritual's participation is sustained largely by identity-fusion and social obligation, not by coercive enforcement mechanisms external to the ritual itself. Theater ratio (0.38) indicates that a substantial share of the ritual's contemporary operation involves performative maintenance of the mourning-practice (the re-enactment of suffering, the mandated tears) rather than pure information-transmission about survival-competence. The measurements show slight upward drift in all three metrics over the 25-year interval, suggesting that as direct catastrophe memory fades (generations further from lived experience), the theater component rises and the suppression requirement (to maintain participation among those with weaker identity-lock) inches upward. The accessibility_collapse (0.72) reflects that once the ritual's dual function is understood, practitioners face nearly complete loss of alternatives to participation — exit means severing identity and group membership. Resistance is low (0.35) because the ritual's legitimate functions (memory and adaptation) are widely recognized; resistance comes primarily from secular alternatives and heterodox interpreters, not from practitioners themselves.
 *
 * PERSPECTIVAL GAP:
 *   The ritual-authority seat (agenda_setter) should compute the hybrid reading as a legitimate rope or scaffold — genuine coordination benefit (memory + adaptation) requiring active enforcement (defining the canonical reading, maintaining the performance structure). Post-catastrophe-generation seats (identity-locked, powerless) should compute it as higher extraction — they inherit the obligation without having chosen it, and the ritual's survival-competence function is not transparent to them; they experience it as emotional burden and participation requirement. Ritual-scholar seats (observer/analytical) should compute it as a legitimate hybrid structure with unresolved empirical questions about whether the separation of mourning and survival-competence would degrade both functions or permit each to be optimized independently. The engine computes each seat's perception from the structural data; the divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual authorities (institutional power, arbitrary exit options) are near the beneficiary end of the directionality spectrum (d near 0.2–0.3): they define the ritual, interpret its functions, collect the participation commitment and institutional continuity it generates. Post-catastrophe generations (powerless, identity-locked exit) are near the target end (d near 0.75–0.85): they inherit the obligation, bear the participation burden, and cannot exit without identity dissolution. Ritual practitioners (organized, identity-locked but some choice in how intensely to participate) sit mid-spectrum (d near 0.5–0.6): they benefit from the memory-preservation and survival-competence transmission, but they also bear the participation burden. The directionality derivation from beneficiary/victim declarations maps beneficiaries as low-d (ritual authorities, community continuity, ritual transmitters), with no declared victims because the ritual does generate genuine coordination benefits alongside the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was acute: prevent complete dispersal and memory loss after catastrophe; transmit adaptive capacity for survival without centralization. The hybrid reading asserts that the ritual's dual encoding solves both problems simultaneously and that separating them would degrade both functions. The mandatrophy test is whether the ritual persists because it still solves the founding problem or because the problem has been superseded and the ritual now extracts without justification. The 'contested' founding_problem_status reflects genuine disagreement: ritual authorities argue the problem is live (memory loss and institutional fragility remain threats); secular alternatives argue it is substantially solved (written archives, educational systems, explicit governance frameworks exist and don't require identity-locked participation). The engine's mandatrophy check would look for: (1) whether the ritual's claimed dual function matches its measured extraction profile (yes: moderate extraction with genuine coordination benefits suggests live founding problem); (2) whether theatrical maintenance has risen above functional performance (theater ratio at 0.38, moderate but not dominally theatrical, suggests the ritual still does real work); (3) whether the ritual is maintained by inertia or by continued commitment (strong participation rates, contested rather than resolved founding-problem status, suggests active commitment rather than pure inertia). The constraint is not yet mandatrophic but is at risk if theater ratio continues to rise and if secular alternatives prove effective at preserving both functions without identity-lock.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentional_design_vs_evolved_doubling,
    'Was the seder ritual originally designed with explicit dual encoding of mourning-practice AND survival-competence (hybrid reading), or did the survival-competence function evolve later and become retro-actively attributed to the original design?',
    'Textual archaeology comparing earliest seder descriptions (Mishna, Talmud, early medieval ritual texts) against later interpretive frameworks; comparison with other post-catastrophe rituals to identify whether dual-function encoding is a design pattern or a post-hoc interpretation.',
    'If intentional design: the hybrid reading is the constraint''s foundational form, and the ritual encodes both functions as intrinsic. If evolved/retro-attributed: the hybrid reading is a modern reading imposed on a primarily mourning-focused ritual, which would lower the architectural coherence of the dual function and suggest the survival-competence reading is an interpretive layer rather than an original constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentional_design_vs_evolved_doubling, empirical, 'Whether the ritual''s dual function is original design or evolved interpretation.').

omega_variable(
    mourning_extraction_vs_necessary_cost,
    'To what extent is the emotional burden of annual re-trauma (required mourning-practice) a necessary cost of memory-preservation, and to what extent is it extractive overhead that ritual authority uses to ensure participation?',
    'Comparative analysis of post-catastrophe communities that separate mourning-practice from institutional continuity (e.g., secular historical commemoration + explicit governance training). If memory retention and adaptive transmission remain functional without fused identity-locking, the burden is extractive; if both degrade when separated, the burden is necessary cost.',
    'If separable: the ritual''s effectiveness could be preserved with lower suppression and extraction (post-catastrophe generations could receive survival-competence training without identity-lock mourning obligation). If inseparable: the current extraction metrics reflect necessary architecture, not monopoly overhead, and the hybrid reading is structurally defended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mourning_extraction_vs_necessary_cost, conceptual, 'Whether mourning obligation is necessary to survival-competence transmission or extractive overhead.').

omega_variable(
    identity_lock_mechanism,
    'What specific identity-fusion mechanism binds post-catastrophe generations to the ritual? Is it the explicit teaching (you are part of the community that survived, therefore you participate in its commemorative obligation), relational identity (your role as descendant is defined by your relationship to the catastrophe), or internalized suppression (participants have absorbed the obligation as intrinsic to belonging)?',
    'Study of communities where generations attempt ritual exit: do they experience structural barriers (family rejection, community expulsion, legal or institutional consequences) or primarily internalized barriers (guilt, identity dissolution, sense of betrayal)? Post-exit trajectories of formerly-locked participants reveal which suppression mechanism dominates.',
    'Structural barriers imply suppression requires active enforcement and could be relaxed; internalized identity-lock implies the suppression persists after external mechanisms are removed and is a deeper architectural feature of the constraint. This determines whether alternative mechanisms could preserve memory and transmission without identity-locking.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Mechanism of identity-lock in ritual participation.').

omega_variable(
    hybrid_reading_vs_reading_plurality,
    'Is the hybrid reading (mourning-practice + survival-competence encoding) a coherent reading of the ritual''s original structure, or is it one interpretive layer overlaying a kernel that genuinely supports multiple distinct readings (mourning-only, survival-only, secular alternative)?',
    'Textual analysis of the ritual kernel itself (seder narrative, blessing formulas, performance instructions): does the text contain explicit markers suggesting both functions were intended, or are the dual-function markers imposed by later interpretive frameworks? Examination of how different reading communities (traditional authorities, heterodox interpreters, secular alternatives) disagree about what the text itself specifies versus what they are adding.',
    'If the kernel genuinely supports multiple distinct readings with equal textual warrant: the hybrid reading is one committer reading, not a privileged interpretation, and the enforcement of the hybrid reading as canonical may be extractive. If the kernel text itself encodes both functions clearly: the hybrid reading has stronger structural justification and authority enforcement is defending the actual kernel rather than imposing a reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_reading_vs_reading_plurality, conceptual, 'Whether the hybrid reading is intrinsic to the kernel or one interpretive overlay among equally warranted alternatives.').

omega_variable(
    suppression_mechanism_in_identity_bound_ritual,
    'For identity-locked practitioners, is suppression of ritual exit primarily structural (community expulsion, legal consequences, economic loss) or primarily internalized (guilt, self-concept dissolution, sense of betrayal to the dead)?',
    'Post-exit trajectory study: do practitioners who leave face continued structural barriers (family rejection, community enforcement) or primarily report internalized barriers (guilt persisting after structural exits are made available, identity dissolution, loss of relational coherence)? If internalized suppression persists after structural barriers are removed, the constraint carries suppression internalized into practitioners themselves.',
    'Structural suppression could be reduced by removing enforcement mechanisms without changing the ritual''s core function. Internalized suppression indicates the constraint''s effectiveness is deeply embedded in participants'' self-concept and would persist even if external enforcement declined — a more thorough form of suppression that would require identity-frame collapse to overcome.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_identity_bound_ritual, empirical, 'Structural versus internalized suppression in identity-locked ritual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__hybrid_transformation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_function__hybrid_transformation_reading, theater_ratio, 25, 0.38).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 5, 0.39).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_function__hybrid_transformation_reading, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 0, 0.24).
narrative_ontology:measurement(cata_su_t5, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 5, 0.25).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(cata_su_t15, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 15, 0.27).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_function__hybrid_transformation_reading, suppression_requirement, 25, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__hybrid_transformation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__hybrid_transformation_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__hybrid_transformation_reading, catastrophe_memory_function__survival_competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_function kernel, decomposed into three constraint stories per ε-invariance principle. The hybrid_transformation reading differs from mourning_practice_reading and survival_competence_reading in structural coupling: hybrid asserts D1/D4 and D5 are inseparable (coupling is constitutive), while single-function readings treat them as functionally separable with contingent overlap. Hybrid reading instantiates moderate extraction (0.42) with identity_coordination type; mourning_practice reading would instantiate lower extraction (0.25–0.35) with primary mourning function; survival_competence reading would instantiate lower suppression (0.15–0.20) with primary adaptive transmission. All three share the same kernel text (seder narrative, commemoration ritual) but read different structural functions as primary/foundational. The three readings coexist as live positions held by different interpretive communities (ritual authorities, heterodox interpreters, secular alternatives).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__hybrid_transformation_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
