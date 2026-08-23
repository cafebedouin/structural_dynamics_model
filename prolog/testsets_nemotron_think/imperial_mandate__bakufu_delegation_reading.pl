% ============================================================================
% CONSTRAINT STORY: imperial_mandate__bakufu_delegation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__bakufu_delegation_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: imperial_mandate__bakufu_delegation_reading
 *   human_readable: Bakufu Delegation of Imperial Mandate
 *   domain: political_philosophy/east_asian_history
 *
 * SUMMARY:
 *   The bakufu delegation reading of the imperial mandate holds that the
 *   emperor's ritual function of granting legitimacy is structurally
 *   separable from the exercise of governing authority. This arrangement,
 *   instantiated in the Kamakura, Muromachi, and Tokugawa shogunates, creates
 *   a bifurcated sovereignty: the emperor reigns but does not rule, while the
 *   shogun rules but does not reign. The samurai class becomes the legitimate
 *   governing stratum, and institutional continuity is maintained through
 *   delegation ceremonies across regime changes. The constraint is claimed as
 *   a tangled rope because it solves a genuine coordination problem
 *   (legitimate governance without imperial political involvement) while
 *   simultaneously extracting political authority from the imperial court and
 *   concentrating it in the samurai class, requiring active enforcement to
 *   suppress imperial political resurgence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, 0.65).
domain_priors:suppression_score(imperial_mandate__bakufu_delegation_reading, 0.75).
domain_priors:theater_ratio(imperial_mandate__bakufu_delegation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(imperial_mandate__bakufu_delegation_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__bakufu_delegation_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__bakufu_delegation_reading, "Bakufu Delegation of Imperial Mandate").
narrative_ontology:topic_domain(imperial_mandate__bakufu_delegation_reading, "political_philosophy/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__bakufu_delegation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__bakufu_delegation_reading, 'ea754e8c-74b6-47d3-8cc6-ff298e4bf347').
narrative_ontology:cs_kernel_codification('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', fixed_text).
narrative_ontology:cs_authority_grounding('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', lineage).
narrative_ontology:cs_interpretation_layer_present('ea754e8c-74b6-47d3-8cc6-ff298e4bf347').
narrative_ontology:cs_reading_relation('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', imperial_mandate__loyalist_restoration_reading, forecloses).
narrative_ontology:cs_axiom('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', foundational, imperial_legitimacy_separable_from_governance).
narrative_ontology:cs_axiom_status(imperial_legitimacy_separable_from_governance, holdable).
narrative_ontology:cs_axiom_grounding('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', imperial_legitimacy_separable_from_governance, conventional).
narrative_ontology:cs_axiom('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', secondary, samurai_class_as_legitimate_governing_stratum).
narrative_ontology:cs_axiom_status(samurai_class_as_legitimate_governing_stratum, holdable).
narrative_ontology:cs_axiom_grounding('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', samurai_class_as_legitimate_governing_stratum, conventional).
narrative_ontology:cs_reference_frame('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', bakufu_delegation_framework).
narrative_ontology:cs_drift_state('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', meiji_restoration, gap(codification_collapse, severe, false)).
narrative_ontology:cs_created_at('ea754e8c-74b6-47d3-8cc6-ff298e4bf347', '').
narrative_ontology:cs_kernel_id(imperial_mandate__bakufu_delegation_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, samurai_class).
narrative_ontology:constraint_beneficiary(imperial_mandate__bakufu_delegation_reading, bakufu).
narrative_ontology:constraint_victim(imperial_mandate__bakufu_delegation_reading, imperial_court).
narrative_ontology:constraint_vindicates(imperial_mandate__bakufu_delegation_reading, mandate_of_heaven_delegation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The military government that exercises administrative authority, enforces the delegation, and collects the gains of governance.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, bakufu, agenda_setter,
    institutional, generational, constrained, national).

% The warrior class that receives status, land, and governing authority from the bakufu in exchange for military service; their position depends on the delegation arrangement.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, samurai_class, beneficiary,
    organized, generational, identity_locked, national).

% The emperor and court nobility who retain ritual legitimacy but are excluded from political decision-making; their political agency is extracted by the delegation.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, imperial_court, payer,
    moderate, generational, trapped, national).

% Groups advocating for restoration of direct imperial rule; they are structurally excluded from the delegation arrangement and would object to it.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, loyalist_factions, excluded,
    moderate, biographical, constrained, national).

% Interpretive class that rationalizes the delegation through Confucian political theory; they neither govern nor are governed directly but legitimize the arrangement.
narrative_ontology:constraint_stakeholder(imperial_mandate__bakufu_delegation_reading, confucian_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable mechanism for transferring governing authority from the ritual sovereign to a military administration, ensuring political continuity across regime changes without challenging the cosmic order.
% TRANSFER_FUNCTION: Moves political authority and governing resources from the imperial court to the samurai class (bakufu), while moving ritual legitimacy from the emperor to the bakufu's governance.
% ABSENT_VOICES: Loyalist factions (sonno joi advocates) and imperial loyalists who argue that the mandate requires direct imperial governance; they are suppressed or excluded from the political process.
% DISAPPEARANCE_RATIONALE: Without the delegation, the bakufu loses its cosmic legitimacy, the imperial court must assume administrative burdens it has not exercised for centuries, and the samurai class's governing authority becomes a naked power grab — the political order would fundamentally reorganize.
% FOUNDING_PROBLEM: The imperial court's inability to govern the expanding realm directly while maintaining ritual purity; the need for a military arm that could exercise authority without contaminating the emperor's sacred status.
% FOUNDING_PROBLEM_CORROBORATION: Historical records from the Kamakura and Muromachi periods show the court voluntarily delegated military authority to the shogun; Meiji restoration leaders (outside the bakufu beneficiaries) attested the delegation had become a tool of samurai class entrenchment rather than a solution to imperial incapacity.
narrative_ontology:disappearance_verdict(imperial_mandate__bakufu_delegation_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__bakufu_delegation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__bakufu_delegation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__bakufu_delegation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__bakufu_delegation_reading, 0.65, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__bakufu_delegation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__bakufu_delegation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__bakufu_delegation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.65) reflects the transfer of governing resources and authority from court to samurai class. Suppression (0.75) is high because the bakufu must actively prevent the emperor from reclaiming political power (e.g., through cloistered rule or loyalist movements). Theater ratio (0.4) indicates that rituals of imperial investiture and court ceremonies are real but increasingly performative relative to actual governance. Accessibility collapse (0.8) is high because once the delegation is accepted, direct imperial rule becomes cognitively and institutionally inaccessible. Resistance (0.5) is moderate: loyalist movements exist but are structurally contained until the Meiji period.
 *
 * PERSPECTIVAL GAP:
 *   From the bakufu's seat, the arrangement is a rope: it coordinates legitimacy and governance efficiently. From the imperial court's seat, it is a snare: their political agency is extracted under the cover of ritual honor. From the samurai class's seat, it is a rope that legitimizes their rule. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The bakufu (agenda_setter, institutional power) is the primary beneficiary: it controls enforcement and collects governance gains (d near 0). The samurai class (beneficiary, organized, identity_locked) also benefits but is more constrained by the bakufu's authority (d ~0.2). The imperial court (payer, moderate power, trapped exit) bears the extraction of political authority (d near 1). Loyalist factions (excluded) would be payers if they could participate. Confucian scholars (observer) are analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (imperial inability to govern directly) was live in the 12th century but becomes contested later. The delegation persists long after the court develops administrative capacity, suggesting mandatrophy: the arrangement's original justification atrophies while the extraction continues. The Meiji restoration represents the mandatrophy resolution: the delegation is discarded when the founding problem is recognized as dead by a new coalition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the imperial mandate kernel inherently permit delegation, or does it require unmediated imperial sovereignty?',
    'Comparative analysis of classical texts (e.g., Book of Documents, Confucian commentaries) and historical practice across East Asian dynasties to determine whether delegation is a valid reading or a later construction.',
    'If delegation is textually unsupported, the bakufu_delegation_reading is a constructed constraint (tangled_rope/snare) rather than a legitimate interpretation of a natural-law-like mandate (mountain). This affects the claimed_type and the naturalness assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the mandate of heaven doctrine structurally allows bifurcated sovereignty.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the imperial court''s political exclusion maintained by bakufu coercion (structural) or by internalized acceptance of ritual role (internalized)?',
    'Examine court diaries and edicts during periods of bakufu weakness (e.g., Kenmu Restoration, late Tokugawa): if the court attempts political action when suppression eases, suppression is structural; if the court does not, internalization is significant.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest — the court carries the suppression with it even after bakufu collapse, affecting post-Meiji political culture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of imperial political agency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__bakufu_delegation_reading, 0, 680).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impe_tr_t0, imperial_mandate__bakufu_delegation_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(impe_tr_t100, imperial_mandate__bakufu_delegation_reading, theater_ratio, 100, 0.25).
narrative_ontology:measurement(impe_tr_t200, imperial_mandate__bakufu_delegation_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement(impe_tr_t300, imperial_mandate__bakufu_delegation_reading, theater_ratio, 300, 0.35).
narrative_ontology:measurement(impe_tr_t400, imperial_mandate__bakufu_delegation_reading, theater_ratio, 400, 0.38).
narrative_ontology:measurement(impe_tr_t500, imperial_mandate__bakufu_delegation_reading, theater_ratio, 500, 0.39).
narrative_ontology:measurement(impe_tr_t600, imperial_mandate__bakufu_delegation_reading, theater_ratio, 600, 0.4).
narrative_ontology:measurement(impe_tr_t680, imperial_mandate__bakufu_delegation_reading, theater_ratio, 680, 0.4).

% Extraction over time
narrative_ontology:measurement(impe_be_t0, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(impe_be_t100, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(impe_be_t200, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(impe_be_t300, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 300, 0.6).
narrative_ontology:measurement(impe_be_t400, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 400, 0.62).
narrative_ontology:measurement(impe_be_t500, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 500, 0.63).
narrative_ontology:measurement(impe_be_t600, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 600, 0.64).
narrative_ontology:measurement(impe_be_t680, imperial_mandate__bakufu_delegation_reading, base_extractiveness, 680, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(impe_su_t0, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(impe_su_t100, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(impe_su_t200, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(impe_su_t300, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 300, 0.72).
narrative_ontology:measurement(impe_su_t400, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 400, 0.73).
narrative_ontology:measurement(impe_su_t500, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 500, 0.74).
narrative_ontology:measurement(impe_su_t600, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 600, 0.75).
narrative_ontology:measurement(impe_su_t680, imperial_mandate__bakufu_delegation_reading, suppression_requirement, 680, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__bakufu_delegation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imperial_mandate__bakufu_delegation_reading, imperial_mandate__loyalist_restoration_reading).

% DUAL FORMULATION NOTE:
% This constraint and the loyalist_restoration_reading are two readings of the imperial_mandate kernel. The bakufu_delegation_reading treats the delegation as a stable constitutional arrangement; the loyalist_restoration_reading treats it as a usurpation that must be corrected. They form a constraint family linked by mutual foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
