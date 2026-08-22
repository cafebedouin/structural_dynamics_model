% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License — Corporate Moat Reading
 *   domain: technology_governance/intellectual_property/software_licensing
 *
 * SUMMARY:
 *   This constraint story instantiates the 'corporate_moat_reading' of the
 *   permissive_license_text kernel. The permissive license text (MIT, BSD,
 *   Apache-2.0) is a single kernel that admits multiple structurally distinct
 *   readings. This reading holds that copyright relaxation — the removal of
 *   copyleft reciprocity requirements — enables enterprise corporations and
 *   cloud providers to extract commercial value from uncompensated upstream
 *   labor. The constraint is the license term itself, read as an extraction
 *   mechanism: it coordinates universal adoption (genuine coordination
 *   function) while simultaneously enabling asymmetric value capture
 *   (extraction). The claimed type is 'snare' because the coordination story
 *   (universal adoption) operates as cover for the extraction; persistence
 *   depends on suppressing the alternative of reciprocal licensing through
 *   network effects and corporate governance capture. The committer structure
 *   (kernel/reading/sibling relations) is routed to omega variables and
 *   cs_structure per Rules 1-4.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.58).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.32).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.24).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.24).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License — Corporate Moat Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "technology_governance/intellectual_property/software_licensing").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, 'b117dcc3-02df-4b7d-8375-38eb14763cd7').
narrative_ontology:cs_kernel_codification('b117dcc3-02df-4b7d-8375-38eb14763cd7', formalized).
narrative_ontology:cs_authority_grounding('b117dcc3-02df-4b7d-8375-38eb14763cd7', lineage).
narrative_ontology:cs_interpretation_layer_present('b117dcc3-02df-4b7d-8375-38eb14763cd7').
narrative_ontology:cs_reading_relation('b117dcc3-02df-4b7d-8375-38eb14763cd7', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('b117dcc3-02df-4b7d-8375-38eb14763cd7', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('b117dcc3-02df-4b7d-8375-38eb14763cd7', foundational, copyright_relaxation_enables_uncompensated_corporate_extraction).
narrative_ontology:cs_axiom_status(copyright_relaxation_enables_uncompensated_corporate_extraction, holdable).
narrative_ontology:cs_axiom_grounding('b117dcc3-02df-4b7d-8375-38eb14763cd7', copyright_relaxation_enables_uncompensated_corporate_extraction, empirically_contingent).
narrative_ontology:cs_axiom('b117dcc3-02df-4b7d-8375-38eb14763cd7', secondary, permissive_license_adoption_saturation_achieved).
narrative_ontology:cs_axiom_status(permissive_license_adoption_saturation_achieved, holdable).
narrative_ontology:cs_axiom_grounding('b117dcc3-02df-4b7d-8375-38eb14763cd7', permissive_license_adoption_saturation_achieved, empirically_contingent).
narrative_ontology:cs_reference_frame('b117dcc3-02df-4b7d-8375-38eb14763cd7', early_open_source_adoption_era).
narrative_ontology:cs_drift_state('b117dcc3-02df-4b7d-8375-38eb14763cd7', contemporary_cloud_dominance_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b117dcc3-02df-4b7d-8375-38eb14763cd7', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, cloud_providers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, small_independent_developers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, community_contributors).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, permissive_licensing_maximizes_adoption).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Incorporate permissively-licensed open source components into proprietary cloud services and enterprise products without compensation to upstream maintainers. Benefit from community maintenance and security auditing of core infrastructure while capturing commercial value through proprietary differentiation layers. Can relicense or fork at will; no reciprocity obligation. Their exit is arbitrage-grade: they could internalize maintenance if the license became restrictive.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Operate managed services (databases, message queues, search, ML platforms) built on permissively-licensed upstream projects. Capture the majority of commercial value while contributing patches selectively. Influence project governance through employeemaintainer hiring and infrastructure sponsorship. Exit is arbitrage: they have the capital to fork and maintain independently if licensing terms shift unfavorably.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, cloud_providers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, cloud_providers, agenda_setter).

% Perform the bulk of feature development, bug triage, security response, and community management for widely-used open source projects. Receive no direct compensation from corporate downstream users. Professional reputation and identity are fused with the project; exit means abandoning a career-defining body of work and the community that formed around it. Funding exists only through sporadic sponsorships or consulting, not structural revenue share.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    moderate, biographical, identity_locked, global).

% Build products on permissively-licensed foundations but lack the distribution channels, enterprise sales teams, or cloud infrastructure to capture value at scale. Compete against corporate-managed services that use their own upstream contributions against them. Exit is constrained: they can switch foundations but face high switching costs and ecosystem lock-in.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, small_independent_developers, payer,
    powerless, biographical, constrained, global).

% Contribute bug fixes, documentation, translations, and minor features in good faith that the project remains a commons. Discover their labor is commercially exploited without reciprocity. Exit is mobile: they can stop contributing or move to copyleft projects, but the cost is losing the community and tooling they invested in.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, community_contributors, payer,
    powerless, immediate, mobile, global).

% Argue that permissive licensing structurally enables the extraction pattern described. Their proposed alternative (viral reciprocity) is excluded from the governance of permissive projects by license choice. They observe from outside the constraint's operational frame but shape the discourse that could shift license adoption norms.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_advocates, excluded,
    organized, generational, analytical, global).

% Govern the license terms and project direction for major permissively-licensed ecosystems. Comprise corporate representatives and community maintainers. Resist license changes toward reciprocity due to corporate governance weight. Could change the constraint but face collective action barriers and institutional inertia.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, license_steering_committees, agenda_setter,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a low-friction legal framework that enables rapid, universal adoption of shared software infrastructure without negotiation overhead — solves the coordination problem of 'how do we all use this code together without permission-seeking for every use case'.
% TRANSFER_FUNCTION: Moves commercial value from downstream proprietary products (cloud services, enterprise software) to the corporations operating them, while the upstream labor (maintenance, security, feature development) remains uncompensated. The license terms permit this transfer by not requiring reciprocity or revenue sharing.
% ABSENT_VOICES: End users of proprietary downstream products who would benefit from sustainable upstream maintenance but have no visibility into the supply chain. Workers in corporate open source program offices who privately favor reciprocity but cannot advocate against employer interests. Jurisdictions considering public code procurement policies that could mandate reciprocity.
% DISAPPEARANCE_RATIONALE: If permissive licensing vanished overnight (replaced by universal copyleft or proprietary licensing), the corporate moat reading's extraction mechanism would collapse: corporations could no longer freely incorporate upstream work into proprietary products without negotiating terms or releasing modifications. The software supply chain would reorganize around explicit commercial licensing or reciprocal sharing. Maintainers would gain leverage; corporations would face higher internalization costs.
% FOUNDING_PROBLEM: Early open source needed a license that would not scare off corporate adoption — the founding problem was achieving critical mass of usage and contribution by minimizing legal friction for all downstream users, including commercial ones.
% FOUNDING_PROBLEM_CORROBORATION: Corporate open source program offices and cloud providers attest the founding problem is still live: they argue permissive licensing is necessary for adoption velocity. Individual maintainers, copyleft advocates, and independent economic analyses (e.g., European Commission open source studies, academic work on open source sustainability) attest the founding problem is substantially solved — adoption is saturated — and the arrangement now functions as extraction. The OSI and FSF have documented the shift in public statements.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extraction (0.58) is moderate but rising: the license itself extracts nothing directly, but it structurally enables downstream extraction by corporate actors who capture value without reciprocity. The metric reflects the *enabled* extraction, not a direct transfer. Suppression (0.32) is moderate: the constraint does not actively coerce maintainers, but the ecosystem network effects and corporate governance capture suppress the emergence of reciprocal alternatives. Theater (0.24) is low but growing: 'open source sustainability' initiatives and corporate sponsorship programs perform concern while the structural extraction continues. Accessibility collapse (0.28) is low: alternatives (copyleft, dual licensing, proprietary) exist and are legally available, but network effects make them practically inaccessible for new projects seeking adoption. Resistance (0.45) is moderate: maintainers organize funding platforms, advocate for license changes, and some projects shift to source-available or dual-license models.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations and cloud providers are structural beneficiaries (d near 0.0): the license subsidizes their proprietary products by providing zero-cost, high-quality infrastructure components. Individual maintainers are structural targets (d near 1.0): they bear the maintenance burden while the commercial value flows to downstream capturers. Identity-locked exit for maintainers reflects professional identity fusion — the project IS their career. Small developers are constrained: they can exit the ecosystem but face high switching costs. Community contributors are mobile: they can leave with lower personal cost. Copyleft advocates are excluded (analytical seat). License steering committees are agenda_setters with constrained exit: they could change the license but face collective action barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (corporate adoption friction) was real but is now substantially solved — permissive licensing achieved universal adoption. The arrangement persists despite the founding problem's resolution because the corporate beneficiaries captured the governance layer (steering committees, foundation boards) and the network effects create a moat. This is mandatrophy: the mandate (low-friction adoption) outlived its function, but the constraint persists because the beneficiaries control the change mechanism. The classification prevents mislabeling this as pure coordination (rope) because the extraction is asymmetric and the coordination function no longer justifies the asymmetry — the adoption is already universal. It also prevents mislabeling as tangled_rope because the coordination function is not actively maintained by the extraction; the extraction is a structural consequence of the license terms, not an enforced hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_decomposition,
    'Is the permissive_license_text a single constraint with observer-dependent classification, or three distinct constraints (one per reading) with invariant ε?',
    'Test ε-invariance: if measuring the constraint under the commons_coordination_reading yields ε≈0.05 and under the corporate_moat_reading yields ε≈0.58, the label ''permissive license'' covers multiple constraints. Decompose into separate stories per reading (already done). The engine''s classification divergence across readings confirms structural distinctness.',
    'If single constraint: classification ambiguity is a measurement problem. If multiple constraints: each reading gets its own ε, type, and structural data — the corpus correctly models the kernel as a family of constraints linked by network.affects_constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_decomposition, conceptual, 'Whether the kernel admits one ε-invariant constraint per reading (DP-001) or one constraint with reading-dependent metrics.').

omega_variable(
    extraction_measurement_referent,
    'Should ε measure the license''s direct extraction (near zero) or the downstream extraction it enables (moderate)?',
    'Apply the ε-referent rule for kernel-reading stories: ε''s referent is the standing arrangement under contest — the permissive license terms as an extraction enabler — assessed by this reading''s lights. The reading''s endorsed alternative (copyleft) is NOT the referent. This fixes ε at ~0.58 for this reading.',
    'If ε measured direct license extraction only, this reading would claim mountain/rope (ε≈0). The corporate moat reading would be invisible. The referral rule makes the extraction visible by anchoring ε to the standing arrangement''s operation, not the license text in isolation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_measurement_referent, conceptual, 'Referent choice for ε in kernel-reading stories: direct license terms vs. enabled downstream extraction.').

omega_variable(
    coordination_extraction_separability,
    'Is the coordination function (universal adoption) structurally separable from the extraction function (corporate value capture)?',
    'Natural experiment: observe projects that adopted dual licensing (permissive + commercial) or source-available licenses. If adoption velocity drops but corporate extraction drops more, the functions are separable. If both drop together, they are coupled.',
    'If separable: the constraint is a snare — coordination is cover for extraction. If inseparable: the constraint is a tangled_rope — genuine coordination requires the extraction-enabling terms. The current claimed_type (snare) assumes separability; this omega tracks the uncertainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the coordination and extraction components of permissive licensing are structurally separable.').

omega_variable(
    suppression_mechanism_nature,
    'Is the suppression of reciprocal alternatives structural (network effects, governance capture) or internalized (maintainers believe permissive licensing is morally superior)?',
    'Survey maintainers who have considered license changes: if they cite ecosystem adoption as the barrier, suppression is structural. If they cite philosophical commitment to permissive licensing, suppression is partially internalized. Post-exit suppression trajectory: if maintainers who switch to copyleft face sustained community pressure, internalized component is confirmed.',
    'If internalized: effective suppression is higher than the structural measure (0.32) suggests — the target carries the suppression with them. This would increase effective extraction for identity-locked maintainers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_nature, empirical, 'Structural vs. internalized suppression mechanism for reciprocal licensing alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(perm_tr_t4, permissive_license_text__corporate_moat_reading, theater_ratio, 4, 0.15).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__corporate_moat_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__corporate_moat_reading, theater_ratio, 16, 0.23).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.24).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(perm_be_t4, permissive_license_text__corporate_moat_reading, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__corporate_moat_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__corporate_moat_reading, base_extractiveness, 16, 0.54).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.58).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__corporate_moat_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint family (permissive_license_text kernel) decomposes the single label 'permissive license' into three structurally distinct constraints. This reading (corporate_moat_reading) authors ε=0.58, type=snare, with enterprise_corporations and cloud_providers as beneficiaries and individual_maintainers as primary victims. The commons_coordination_reading authors ε≈0.05, type=rope, with universal_users as beneficiaries and no victims. The copyleft_counterfactual_reading authors a counterfactual constraint (viral reciprocity requirement) with different structural data. All three are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
