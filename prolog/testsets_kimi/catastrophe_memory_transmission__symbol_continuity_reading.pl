% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Catastrophe Memory Transmission â Symbol Continuity Reading
 *   domain: religious/collective_memory/ritual
 *
 * SUMMARY:
 *   This constraint instantiates the symbol_continuity_reading of the
 *   catastrophe_memory_transmission kernel. It treats ritual as preserving
 *   identity and mourning-practice as an intrinsic communal good, where
 *   transmission of symbolic form itself constitutes the survival mechanism.
 *   The reading demands high ritual fidelity and accepts low operational
 *   adaptation, generating a structural tension between identity continuity
 *   and environmental responsiveness. The constraint is claimed as
 *   tangled_rope because it simultaneously coordinates genuine
 *   collective-memory preservation and extracts adaptive capacity from those
 *   who would modify ritual to meet present demands.
 *
 * KEY AGENTS:
 *   - Ritual guardians (agenda_setter): Organized power, identity-locked â adjudicate and enforce symbolic fidelity.
 *   - Communal memory keepers (beneficiary): Organized power, identity-locked â receive identity continuity through ritual transmission.
 *   - Adaptation-seeking members (payer): Moderate power, constrained exit â bear costs of foregone operational adaptation.
 *   - Operational practitioners (excluded): Moderate power, trapped â would advocate competence-based ritual modification but are barred from authority.
 *   - Memory studies observers (observer): Analytical power â document the tension from an external seat.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.55).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Catastrophe Memory Transmission â Symbol Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious/collective_memory/ritual").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce').
narrative_ontology:cs_kernel_codification('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', distributed).
narrative_ontology:cs_authority_grounding('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', practice).
narrative_ontology:cs_interpretation_layer_present('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce').
narrative_ontology:cs_reading_relation('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', foundational, symbolic_form_is_survival_mechanism).
narrative_ontology:cs_axiom_status(symbolic_form_is_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', symbolic_form_is_survival_mechanism, conventional).
narrative_ontology:cs_axiom('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', secondary, ritual_fidelity_over_operational_adaptation).
narrative_ontology:cs_axiom_status(ritual_fidelity_over_operational_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', ritual_fidelity_over_operational_adaptation, instrumental).
narrative_ontology:cs_reference_frame('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', symbolic_continuity_reference).
narrative_ontology:cs_drift_state('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', post_catastrophe_generational_shift, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8b27eb57-32e3-4ee7-81e5-f5c65bcd66ce', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, communal_memory_keepers).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, adaptation_seeking_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive continuity of communal identity and mourning practice through ritual transmission; their collective self-conception across catastrophe depends on symbolic fidelity. Exit would mean identity dissolution, not merely social inconvenience.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, communal_memory_keepers, beneficiary,
    organized, generational, identity_locked, regional).

% Administer and enforce ritual fidelity, adjudicating correct symbolic form and silencing operational or adaptive deviations. Their authority derives from continuity of practice rather than from external empirical validation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, ritual_guardians, agenda_setter,
    organized, generational, identity_locked, regional).

% Bear the cost of foregone operational adaptation; attempts to modify ritual to meet present environmental demands are suppressed in favor of preserving symbolic form. They remain in the community but lose adaptive agency.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, adaptation_seeking_members, payer,
    moderate, biographical, constrained, regional).

% Would advocate for encoding survival competence and environmental pattern recognition into ritual practice, but are structurally excluded from ritual authority because their operational priority contradicts the symbol-continuity mandate.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, operational_practitioners, excluded,
    moderate, biographical, trapped, regional).

% Observe the tension between symbolic preservation and operational adaptation across catastrophe-impacted communities from an analytical distance, documenting both the coordination function of identity continuity and the extraction of adaptive capacity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, memory_studies_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory transmission through symbolic ritual, preserving communal identity and mourning practice across catastrophic ruptures when propositional knowledge and social structure have collapsed.
% TRANSFER_FUNCTION: Moves symbolic fidelity and identity continuity from ritual guardians to the communal memory keepers, while moving adaptive capacity away from adaptation-seeking members and operational practitioners into the preservation of fixed form.
% ABSENT_VOICES: Operational practitioners and adaptation-seeking members who would prioritize environmental responsiveness and survival competence over symbolic preservation; they are present in the community but structurally excluded from ritual authority.
% DISAPPEARANCE_RATIONALE: If the ritual constraint on symbolic continuity disappeared overnight, communal identity would fragment across the catastrophe boundary, mourning practices would adapt to present environmental conditions, and the community's self-conception as a continuous entity would dissolve into adaptive plurality.
% FOUNDING_PROBLEM: Catastrophic rupture threatened communal identity dissolution; the community needed a mechanism to transmit 'who we are' across generational breaks when ordinary social memory had failed.
% FOUNDING_PROBLEM_CORROBORATION: Ritual guardians and communal memory keepers attest the founding problem is still live, citing ongoing identity threat. Anthropologists and adaptation-seeking members attest the problem has shifted to present survival demands, and the ritual now persists as identity performance rather than genuine memory necessity. External trauma-studies scholars provide mixed corroboration depending on whether they weight symbolic or operational survival.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because adaptive capacity is actively sacrificed to preserve symbolic form; the constraint extracts responsiveness from the community and converts it into identity continuity. Suppression (0.55) is moderate because the reading requires active enforcement â guardians must marginalize operational voices and sanction deviation. Theater ratio (0.45) reflects that a growing share of ritual activity may function as identity performance rather than operational survival mechanism. Accessibility collapse (0.70) is high because alternatives (adaptive ritual modification) are understood but normatively collapsed by the identity-continuity mandate. Resistance (0.40) is moderate: adaptation-seeking members and operational practitioners mount real but structurally contained opposition.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (ritual guardians, communal memory keepers) experience the constraint as preserving the community's very existence across catastrophe. The payer and excluded seats (adaptation-seeking members, operational practitioners) experience the same structure as blocking the community's ability to respond to present environmental conditions. The engine computes this divergence from structural data rather than from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Ritual guardians and communal memory keepers are structural beneficiaries of identity continuity; their directionality sits near the subsidy end (low d). Adaptation-seeking members are direct targets because their adaptive agency is the extracted resource (high d). Operational practitioners are excluded targets whose potential contributions are suppressed entirely (high d, approaching full target). Memory studies observers sit at the analytical pole.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the Tangled Rope classification, this constraint would be mislabeled either as a Rope (if one sees only the identity-coordination function) or as a Snare (if one sees only the suppression of adaptation). The Tangled Rope type is warranted because both elements inhere in the same structure: the same ritual that preserves identity simultaneously blocks operational adaptation, and the latter requires active enforcement tied to the former.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_form_as_survival_mechanism,
    'Is the transmission of symbolic form genuinely a survival mechanism for the community, or does it persist as an identity performance that no longer confers operational survival?',
    'Comparative longitudinal studies of catastrophe-impacted communities, tracking viability outcomes for symbol-continuity communities versus operational-competence communities across generational time.',
    'If symbolic continuity does not confer operational survival, the constraint is more extractive than coordinated; if it does, the coordination function is genuine and the Tangled Rope classification tightens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_form_as_survival_mechanism, empirical, 'Whether symbolic continuity confers genuine survival or is performative identity maintenance.').

omega_variable(
    ritual_enforcement_internalized_or_structural,
    'Is the enforcement of ritual fidelity achieved through structural social sanctions or through internalized identity fusion that makes deviation unthinkable?',
    'Ethnographic observation of deviation responses: if suppression persists in the absence of visible sanction, the mechanism is largely internalized.',
    'Internalized suppression raises effective extractiveness because the constraint travels with the agent even in isolation; structural suppression is more locally bounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_enforcement_internalized_or_structural, empirical, 'Structural versus internalized suppression mechanism in ritual fidelity.').

omega_variable(
    sibling_reading_boundary,
    'Does the symbol_continuity reading foreclose the operational_competence reading, or can they coexist within the same ritual tradition?',
    'Analysis of whether any single ritual authority can simultaneously hold that symbolic form is the survival mechanism AND that operational competence is the survival mechanism.',
    'If foreclosed, the kernel generates logical contradiction between readings; if coexistent, the constraint family represents a distributed dispute rather than a strict logical partition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary, conceptual, 'Logical boundary between symbol-continuity and operational-competence readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.44).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.54).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the catastrophe_memory_transmission kernel, instantiating symbol_continuity_reading. It is structurally distinct from operational_competence_reading and hybrid_embedded_reading, which assign different Îµ values, beneficiary/victim structures, and coordination types to the same ritual phenomenon. Decomposition follows the Îµ-invariance principle: the same natural-language label conflates multiple structurally distinct claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
