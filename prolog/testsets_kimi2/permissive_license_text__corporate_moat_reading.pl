% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text â Corporate Moat Reading
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This is the corporate_moat_reading of the permissive_license_text kernel.
 *   It treats widely adopted permissive software licenses (e.g., MIT, Apache,
 *   BSD) not as neutral freedom-maximizing instruments, but as structural
 *   mechanisms that enable enterprise corporations to build proprietary moats
 *   on uncompensated individual maintainer labor. The reading asserts that
 *   the coordination storyâreduced legal frictionâis cover for a
 *   persistent transfer of value from dispersed volunteer producers to
 *   concentrated corporate beneficiaries. Sibling readings include
 *   commons_coordination_reading, which frames the same text as a rope that
 *   maximizes implementation freedom, and copyleft_counterfactual_reading,
 *   which argues that reciprocity requirements are necessary to prevent the
 *   very exploitation this reading identifies.
 *
 * KEY AGENTS:
 *   - Enterprise corporations: primary beneficiaries (institutional/arbitrage) who capture value through proprietary derivatives.
 *   - Individual maintainers: primary payers (powerless/identity_locked) who bear the costs of uncompensated labor transfer.
 *   - Copyleft advocates: excluded voices (moderate/mobile) who argue for reciprocity but are marginalized in norm-setting.
 *   - Open source analysts: observers (analytical/analytical) who document the asymmetric political economy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.62).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.55).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text â Corporate Moat Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'da4528f8-288a-42fd-b0c5-12d2435fcc64').
narrative_ontology:cs_kernel_codification('da4528f8-288a-42fd-b0c5-12d2435fcc64', fixed_text).
narrative_ontology:cs_authority_grounding('da4528f8-288a-42fd-b0c5-12d2435fcc64', lineage).
narrative_ontology:cs_interpretation_layer_present('da4528f8-288a-42fd-b0c5-12d2435fcc64').
narrative_ontology:cs_reading_relation('da4528f8-288a-42fd-b0c5-12d2435fcc64', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('da4528f8-288a-42fd-b0c5-12d2435fcc64', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('da4528f8-288a-42fd-b0c5-12d2435fcc64', foundational, uncompensated_corporate_derivation_is_structurally_extractive).
narrative_ontology:cs_axiom_status(uncompensated_corporate_derivation_is_structurally_extractive, holdable).
narrative_ontology:cs_axiom_grounding('da4528f8-288a-42fd-b0c5-12d2435fcc64', uncompensated_corporate_derivation_is_structurally_extractive, empirically_contingent).
narrative_ontology:cs_axiom('da4528f8-288a-42fd-b0c5-12d2435fcc64', foundational, maximal_permissiveness_asymmetrically_transfers_labor_value).
narrative_ontology:cs_axiom_status(maximal_permissiveness_asymmetrically_transfers_labor_value, holdable).
narrative_ontology:cs_axiom_grounding('da4528f8-288a-42fd-b0c5-12d2435fcc64', maximal_permissiveness_asymmetrically_transfers_labor_value, empirically_contingent).
narrative_ontology:cs_reference_frame('da4528f8-288a-42fd-b0c5-12d2435fcc64', unrestricted_proprietary_derivation_default).
narrative_ontology:cs_drift_state('da4528f8-288a-42fd-b0c5-12d2435fcc64', contemporary_open_source_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('da4528f8-288a-42fd-b0c5-12d2435fcc64', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Incorporate permissively licensed code into proprietary products and services without reciprocity obligations, capturing downstream commercial value while externalizing maintenance and development costs to volunteer or undercompensated upstream labor.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Create and maintain software released under permissive terms that is subsequently built into proprietary derivatives. Receive no payment or code contributions for downstream commercial use, and often experience burnout as their labor subsidizes corporate revenue. Exit is constrained by professional identity fusion with open-source community norms and lack of individual bargaining leverage.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Advocate for reciprocity requirements in software licensing to prevent uncompensated corporate use. Structurally excluded from mainstream industry norm-setting; their arguments are frequently characterized as anti-freedom or anti-commercial rather than as labor protections.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_advocates, excluded,
    moderate, generational, mobile, global).

% Study the political economy of open-source licensing, documenting asymmetric value flows between volunteer maintainers and corporate incorporators without participating in the licensing decisions themselves.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, open_source_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces legal friction for software reuse across organizational boundaries, enabling interoperability and rapid integration without bilateral negotiation of terms.
% TRANSFER_FUNCTION: Moves intellectual labor and commercially deployable code value from individual maintainers to enterprise corporations, who capture it in proprietary derivative products without reciprocity or compensation.
% ABSENT_VOICES: Copyleft advocates and maintainer-labor organizers who would demand reciprocal licensing or direct compensation are excluded from the norm-setting discourse; their positions are treated as ideologically motivated rather than structurally grounded.
% DISAPPEARANCE_RATIONALE: If the permissive license text that enables uncompensated extraction were withdrawn or replaced with reciprocal terms, enterprise business models built on free incorporation would face legal friction, maintainer compensation norms would shift, and the proprietary derivative ecosystem would reorganize around payment or code-sharing obligations.
% FOUNDING_PROBLEM: Legal uncertainty and high bilateral negotiation costs prevented software reuse in the early computing era; early licenses were designed to lower these barriers and enable sharing.
% FOUNDING_PROBLEM_CORROBORATION: Independent software historians and digital labor economists attest that the original legal-friction problem is now solved by the mature licensing ecosystem, and that the permissive text persists primarily as a vehicle for corporate value capture. Corporate legal departments claim the problem remains live, but they speak from the beneficiary seat.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate rather than extreme because not every permissively licensed project is incorporated into a commercial proprietary product; however, where incorporation occurs, the transfer of value is nearly total. Suppression (0.55) reflects the legal and discursive marginalization of copyleft and compensation alternatives within industry norms. Theater_ratio (0.32) captures the growing performative open-source rhetoric that frames the arrangement as charitable or freedom-maximizing rather than extractive. Accessibility_collapse (0.58) indicates that while copyleft alternatives exist, network effects, corporate hiring norms, and package-manager defaults make them structurally costly to adopt. Resistance (0.48) is present but diffuse, consisting primarily of ideological advocacy rather than organized labor power. The measurement series run on a single shared time grid to prevent misaligned drift dating.
 *
 * PERSPECTIVAL GAP:
 *   From the enterprise seat, permissive licensing is a friction-reducing legal convenience that accelerates innovation and reduces contracting overhead. From the maintainer seat, the same legal text operates as a structural arrangement that externalizes costs and captures labor value without return. The engine computes this divergence from the beneficiary/victim declarations and the stark difference in exit options between institutional arbitrage and individual identity lock-in.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are the declared beneficiaries: they incorporate permissively licensed code into proprietary products without payment or reciprocity, yielding low directionality near the beneficiary pole. Individual maintainers are the declared victims: they produce the code that fuels downstream proprietary value capture but receive no transfer, yielding high directionality near the target pole. The asymmetry is structuralâthe license text assigns broad rights to downstream actors while imposing no obligationsâand is amplified by the maintainers' identity_locked exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâlegal uncertainty preventing software reuseâhas been solved by the mature licensing ecosystem, yet the specific permissive text persists as a mechanism for extraction. The R5 genealogy fields (founding_problem_status: dead paired with disappearance_verdict: world_rearranges) flag the mismatch between origin myth and current function, preventing misclassification as a rope or scaffold. The constraint is not a piton because the beneficiary set is concentrated and actively profits from the extraction; theater is moderate but the core function is live rent extraction, not inertial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_vs_coordination_separability,
    'Is the extraction from permissive licensing separable from its coordination function, or is the legal friction reduction inherently coupled to uncompensated corporate capture?',
    'Comparative analysis of ecosystems where default reciprocity or compensation mechanisms are enforced, measuring whether interoperability and reuse rates decline.',
    'If separable, the constraint is a snare with a real but hijacked coordination function; if inseparable, the coordination itself may be structurally extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_separability, conceptual, 'Whether coordination and extraction are structurally separable in permissive licensing').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of reciprocity demands structural (legal default, network effects, corporate hiring norms) or internalized (maintainers believe exposure and reputation are their own reward)?',
    'Post-exit trajectory analysis: if maintainers who leave permissive projects continue to reject reciprocity frameworks, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggestâthe target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in maintainer labor').

omega_variable(
    kernel_reading_indeterminacy,
    'Does the permissive license text kernel inherently underdetermine its readings, or does the corporate moat reading capture the text''s structural tendency under existing power distributions?',
    'Cross-reading comparative analysis: if power distributions shifted and the commons reading became dominant without text change, the kernel is genuinely underdetermined; if the moat reading persists across power shifts, the text is structurally tilted.',
    'If the kernel is underdetermined, classification should vary by reading; if structurally tilted, the corporate moat reading may be the default attractor.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel is neutral among readings or structurally tilted toward extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__corporate_moat_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__corporate_moat_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement(perm_tr_t24, permissive_license_text__corporate_moat_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(perm_tr_t32, permissive_license_text__corporate_moat_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement(perm_tr_t40, permissive_license_text__corporate_moat_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__corporate_moat_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__corporate_moat_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(perm_be_t24, permissive_license_text__corporate_moat_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(perm_be_t32, permissive_license_text__corporate_moat_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(perm_be_t40, permissive_license_text__corporate_moat_reading, base_extractiveness, 40, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__corporate_moat_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'permissive license text' conflates three structurally distinct constraints: a freedom-maximizing coordination mechanism (commons_coordination_reading), a reciprocity-demanding counterfactual (copyleft_counterfactual_reading), and an extraction-enabling corporate moat (this reading). Each reading carries a distinct epsilon, beneficiary structure, and classification. They form a constraint family linked by shared kernel provenance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
