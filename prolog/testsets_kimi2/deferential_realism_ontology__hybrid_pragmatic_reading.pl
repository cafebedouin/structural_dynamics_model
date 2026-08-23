% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Typology Core-Periphery Arrangement (Hybrid Pragmatic Reading)
 *   domain: epistemological/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_pragmatic_reading of the
 *   deferential_realism_ontology kernel. The kernel is a contested commitment
 *   to a six-category typology for classifying constraints. This reading
 *   holds that the typology possesses a fixed core (mountains, ropes)
 *   grounded in physical and coordination constraints, alongside a contested
 *   periphery (tangled_ropes, snares) where classification depends on
 *   normative judgments about legitimate beneficiaries. The reading thereby
 *   naturalizes the core while opening the periphery to legitimate
 *   contestation, producing asymmetric extraction across the two zones.
 *   Sibling readings include the immutable_diagnostic_reading (all categories
 *   have fixed observational referents) and the rhetorical_scaffold_reading
 *   (the entire typology is a normative vocabulary for policy critique).
 *
 * KEY AGENTS:
 *   - dr_community_guardians (agenda_setter / institutional / constrained): maintain the six-category typology and enforce the core-periphery boundary
 *   - institutional_designers (beneficiary / organized / mobile): use the stable core for uncontroversial institutional analysis
 *   - normative_critics (beneficiary / organized / constrained): deploy the contested periphery to critique extraction and illegitimate beneficiaries
 *   - regulated_entities (payer / powerful / constrained): bear the reputational and defensive costs of peripheral classification disputes
 *   - rival_ontologists (excluded / institutional / analytical): alternative framework theorists marginalized by DR dominance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.6).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Typology Core-Periphery Arrangement (Hybrid Pragmatic Reading)").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemological/institutional").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '51cb1e48-d627-4324-8e58-960900403528').
narrative_ontology:cs_kernel_codification('51cb1e48-d627-4324-8e58-960900403528', formalized).
narrative_ontology:cs_authority_grounding('51cb1e48-d627-4324-8e58-960900403528', expertise).
narrative_ontology:cs_interpretation_layer_present('51cb1e48-d627-4324-8e58-960900403528').
narrative_ontology:cs_reading_relation('51cb1e48-d627-4324-8e58-960900403528', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('51cb1e48-d627-4324-8e58-960900403528', deferential_realism_ontology__rhetorical_scaffold_reading, influences).
narrative_ontology:cs_axiom('51cb1e48-d627-4324-8e58-960900403528', foundational, core_periphery_epistemic_split).
narrative_ontology:cs_axiom_status(core_periphery_epistemic_split, holdable).
narrative_ontology:cs_axiom_grounding('51cb1e48-d627-4324-8e58-960900403528', core_periphery_epistemic_split, empirically_contingent).
narrative_ontology:cs_axiom('51cb1e48-d627-4324-8e58-960900403528', foundational, normative_construction_of_periphery).
narrative_ontology:cs_axiom_status(normative_construction_of_periphery, holdable).
narrative_ontology:cs_axiom_grounding('51cb1e48-d627-4324-8e58-960900403528', normative_construction_of_periphery, conventional).
narrative_ontology:cs_reference_frame('51cb1e48-d627-4324-8e58-960900403528', stable_core_contested_periphery).
narrative_ontology:cs_drift_state('51cb1e48-d627-4324-8e58-960900403528', contemporary_application_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('51cb1e48-d627-4324-8e58-960900403528', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, normative_critics).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, regulated_entities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the six-category typology, peer-review norms, and training curricula that enforce the boundary between the observational core and the contested periphery. Their professional standing and research program depend on the framework's continued uptake.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, dr_community_guardians, agenda_setter,
    institutional, generational, constrained, global).

% Use the stable core categoriesâmountain and ropeâto ground institutional analysis in apparently natural or coordinative facts, benefiting from the framework's epistemic stability and cross-domain portability.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers, beneficiary,
    organized, biographical, mobile, national).

% Deploy the contested periphery categoriesâsnare and tangled ropeâto critique power and illegitimate beneficiaries in policy debates, drawing analytical legitimacy from the framework while contesting specific classifications.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, normative_critics, beneficiary,
    organized, biographical, constrained, national).

% Corporations, states, and platforms whose mechanisms fall into the contested periphery; bear the reputational and defensive costs of resisting 'snare' or 'tangled rope' classifications that depend on unstable normative judgments about legitimate beneficiaries.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, regulated_entities, payer,
    powerful, biographical, constrained, national).

% Critical theorists, structuralists, and alternative-typology scholars whose frameworks are marginalized in mainstream institutional analysis by the DR ontology's dominance in journals, conferences, and policy curricula.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rival_ontologists, excluded,
    institutional, civilizational, analytical, global).

narrative_ontology:fixing_cost_class(deferential_realism_ontology__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, stable vocabulary for distinguishing natural and coordinative constraints (mountains, ropes) from extractive ones (snares, tangled ropes), enabling cross-community institutional analysis and design.
% TRANSFER_FUNCTION: Moves epistemic authority from contested empirical domains to the normative periphery, where classification hinges on judgments about legitimate beneficiaries; transfers reputational and defensive costs to entities classified in the contested periphery.
% ABSENT_VOICES: Rival ontologists from critical theory and structuralist traditions who reject the core-periphery split; empirical researchers whose findings challenge specific core classifications but are excluded from framework governance.
% DISAPPEARANCE_RATIONALE: If the ontology vanished, institutional designers would lose a stable diagnostic vocabulary, normative critics would lose a legitimizing analytical framework, and classification disputes would reorganize around different conceptual axes; the split between 'natural' core and 'contested' periphery would collapse into undifferentiated debate.
% FOUNDING_PROBLEM: The lack of a rigorous, portable framework for distinguishing genuinely coordinative institutional mechanisms from extractive ones across disparate domains.
% FOUNDING_PROBLEM_CORROBORATION: Institutional economists and policy designers outside the immediate DR community attest the need for coordination-extraction diagnostics; critical theorists attest the founding problem has been co-opted by the framework's normative commitments. Corroboration is split between beneficiaries and excluded seats.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 to reflect the hybrid epsilon profile: the core (mountain/rope) carries negligible extraction, while the periphery (snare/tangled_rope) carries substantial extraction through normative contestation. The aggregate score sits at medium-high. Suppression is 0.60 (medium) because alternative typologies are marginalized through institutional gatekeeping but not eliminated. Theater ratio is 0.40, reflecting performative maintenance of the core's 'natural' status. Accessibility collapse is moderate (0.45): the DR framework is widely accessible, but critical alternatives are harder to reach. Resistance is 0.50: regulated entities and rival ontologists mount active but uneven resistance. The measurement series run on a single shared time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   Core practitioners (institutional_designers) experience the ontology as a coordinative ropeâa useful, stable vocabulary. Peripheral targets (regulated_entities) experience it as an extractive snareâa weaponized normative lens. Normative critics experience it as a tool of legitimate critique. The engine computes this divergence from the structural data; the authored claim (tangled_rope) does not adjudicate the seats but identifies the structure that produces the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The dr_community_guardians are agenda-setters with low directionality (they administer the framework). Institutional_designers and normative_critics are beneficiaries with low directionality: the former gain epistemic stability from the core, the latter gain analytical legitimacy from the periphery. Regulated_entities are payers with high directionality: they bear the costs of contested peripheral classifications. Rival_ontologists are excluded and analytically mobile but structurally targeted by the framework's suppression of alternatives, placing them at the high end of directionality despite their global scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The ontology prevents mislabeling by separating the coordinative core from the extractive periphery. Treating the whole framework as a rope would ignore the weaponization of the periphery; treating it as a snare would deny the genuine coordination function of the core. Tangled_rope is the structurally accurate classification because the same arrangement coordinates through the core and extracts through the periphery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is the hybrid_pragmatic_reading of the deferential_realism_ontology kernel. Would the immutable_diagnostic_reading (fixed observational referents for all categories) and the rhetorical_scaffold_reading (purely normative vocabulary) change the structural classification of the core and periphery, and where is the disagreement located?',
    'Comparative analysis of the three readings'' epsilon profiles and directionality derivations for identical classified cases; if the immutable reading assigns observational epsilon to all six categories and the rhetorical reading assigns constructed epsilon to all six, the disagreement is located in the epistemic grounding of the categories, not in the cases themselves.',
    'If the core is not observational, the framework''s mountain and rope classifications are false summits; if the periphery is not normative, the framework''s snare and tangled rope classifications are misattributed observational errors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Epistemic location of the disagreement across the three kernel readings.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative typologies structural (institutional gatekeeping in journals, conferences, and funding bodies) or internalized (analysts believe the DR framework is the only rigorous approach)?',
    'Trace the citation and hiring patterns of rival ontologists; if suppression persists after structural barriers are removed (e.g., open-access publishing), the constraint is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the target carries the suppression with them after structural exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    hybrid_epsilon_integrity,
    'Can a single constraint (the DR ontology) coherently carry a hybrid epsilon profile (observational core, constructed periphery), or does the epsilon-invariance principle require decomposition into two constraints?',
    'Evaluate whether changing the observable (core vs periphery classification) changes the constraint''s referent. If the referent is the ontology itself as an epistemic arrangement, the hybrid profile is a property of the arrangement; if the referent splits, decompose into core and periphery stories.',
    'If decomposition is required, the ontology is two constraints (a core coordinator and a periphery classifier); if not, the single constraint is legitimately tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_epsilon_integrity, conceptual, 'Whether hybrid epsilon violates epsilon-invariance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 40, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 40, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
