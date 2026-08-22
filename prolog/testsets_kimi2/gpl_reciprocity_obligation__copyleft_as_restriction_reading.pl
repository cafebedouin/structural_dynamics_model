% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation â Restriction Reading
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the copyleft_as_restriction_reading of
 *   the gpl_reciprocity_obligation kernel. In this reading, the GPL's viral
 *   clause is not a freedom-preservation or commons-protection mechanism but
 *   a structural restriction on business models that disproportionately
 *   benefits proprietary software vendors. By prohibiting proprietary
 *   integration, the clause isolates commons contributions from the dominant
 *   proprietary ecosystem, enabling proprietary vendors to reimplement
 *   commons innovations as closed products without contributing back, while
 *   commons contributors bear the cost of restricted adoption and licensing
 *   complexity. The divergence between the claimed coordination function
 *   (source reciprocity) and the authored metrics (high extraction, active
 *   enforcement, moderate theater) is intentional: the engine measures the
 *   gap between the freedom narrative and the restriction reading's
 *   structural account.
 *
 * KEY AGENTS:
 *   - proprietary_vendors (beneficiary; powerful/mobile) â capture competitive advantage from commons isolation
 *   - commons_contributors (payer; moderate/constrained) â bear integration restrictions and reduced impact
 *   - gpl_enforcement_orgs (agenda_setter; organized/arbitrage) â administer and defend the viral clause
 *   - permissive_license_advocates (excluded; moderate/mobile) â argue for open integration but are outside GPL-core governance
 *   - downstream_users (observer; organized/constrained) â receive source code but do not drive the extraction dynamic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.75).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.72).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation â Restriction Reading").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '86c64fd5-aad6-487e-bcdf-aff2b1eca788').
narrative_ontology:cs_kernel_codification('86c64fd5-aad6-487e-bcdf-aff2b1eca788', formalized).
narrative_ontology:cs_authority_grounding('86c64fd5-aad6-487e-bcdf-aff2b1eca788', lineage).
narrative_ontology:cs_interpretation_layer_present('86c64fd5-aad6-487e-bcdf-aff2b1eca788').
narrative_ontology:cs_reading_relation('86c64fd5-aad6-487e-bcdf-aff2b1eca788', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('86c64fd5-aad6-487e-bcdf-aff2b1eca788', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('86c64fd5-aad6-487e-bcdf-aff2b1eca788', foundational, viral_clause_constitutes_business_model_restriction).
narrative_ontology:cs_axiom_status(viral_clause_constitutes_business_model_restriction, holdable).
narrative_ontology:cs_axiom_grounding('86c64fd5-aad6-487e-bcdf-aff2b1eca788', viral_clause_constitutes_business_model_restriction, conventional).
narrative_ontology:cs_axiom('86c64fd5-aad6-487e-bcdf-aff2b1eca788', foundational, proprietary_vendors_capture_value_from_commons_isolation).
narrative_ontology:cs_axiom_status(proprietary_vendors_capture_value_from_commons_isolation, holdable).
narrative_ontology:cs_axiom_grounding('86c64fd5-aad6-487e-bcdf-aff2b1eca788', proprietary_vendors_capture_value_from_commons_isolation, empirically_contingent).
narrative_ontology:cs_reference_frame('86c64fd5-aad6-487e-bcdf-aff2b1eca788', reciprocal_source_disclosure_mandate).
narrative_ontology:cs_drift_state('86c64fd5-aad6-487e-bcdf-aff2b1eca788', contemporary_permissive_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('86c64fd5-aad6-487e-bcdf-aff2b1eca788', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the isolation of GPL-licensed commons code from proprietary ecosystems. Because GPL code cannot be integrated into proprietary products, these vendors face reduced competition from hybrid open-source offerings and can freely reimplement commons innovations as proprietary products without contributing back. They can avoid GPL code entirely or build parallel proprietary implementations.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Contribute code to GPL-licensed projects under terms that prohibit proprietary integration. Their work is restricted from being combined with proprietary code, limiting adoption in commercial contexts and preventing their contributions from reaching users through proprietary channels. They bear the cost of reduced impact, licensing complexity, and the near-impossibility of relicensing once multiple contributors are involved.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    moderate, biographical, constrained, global).

% Administer and enforce the GPL reciprocity terms through legal action, license interpretation, and advocacy. They set the boundary conditions for what constitutes derivative works and proprietary integration, and their institutional legitimacy depends on maintaining the restrictiveness of the viral clause.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_enforcement_orgs, agenda_setter,
    organized, generational, arbitrage, global).

% Advocate for permissive licensing models that allow proprietary integration. They are largely excluded from GPL-core governance discussions and would argue that the reciprocity obligation harms commons contributors by reducing adoption and enabling proprietary reimplementation of commons innovations.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_license_advocates, excluded,
    moderate, biographical, mobile, global).

% Receive source code under the GPL but do not drive the extraction dynamic. Their user experience is shaped by the constraint yet they are neither the primary beneficiaries of the restriction nor the primary bearers of its costs.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, downstream_users, observer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_vendors).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ostensibly ensures that users of software receive corresponding source code and that modifications are shared back, solving the collective-action problem of private provision of public goods in software development.
% TRANSFER_FUNCTION: Moves the right to integrate code into proprietary products from commons contributors to proprietary vendors by legally prohibiting such integration, thereby transferring competitive advantage and business-model flexibility to the proprietary software sector.
% ABSENT_VOICES: Permissive-license advocates, dual-license proponents, and commons contributors who would prefer wider adoption over strict reciprocity are largely excluded from GPL governance institutions; they would argue the restriction harms the commons by reducing network effects and enabling proprietary reimplementation.
% DISAPPEARANCE_RATIONALE: If the viral clause vanished overnight, proprietary vendors could integrate GPL code directly into proprietary stacks, commons projects would face enclosure pressures but also gain adoption, and the enforcement organizations would lose their primary institutional lever; the open-source licensing landscape would reorganize around permissive or proprietary integration norms.
% FOUNDING_PROBLEM: Preventing the enclosure of freely licensed software commons by proprietary actors who would take code without sharing modifications, thereby depleting the commons.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and Software Freedom Conservancy attest the founding problem remains live from the agenda-setter seat. Independent software industry economists and permissive-license foundation representatives attest the problem is solved or inverted by market evolution; these sources are outside the proprietary_vendor beneficiary set.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.75, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because the constraint systematically transfers business-model options away from commons contributors toward proprietary vendors by prohibiting the primary integration path (proprietary combination). Suppression is substantial (0.72) because persistence depends on active copyright enforcement and the legal suppression of proprietary integration attempts. Theater is moderate-high (0.58) because the constraint is publicly justified by freedom and commons-preservation narratives that, in this reading, serve as cover for a structurally extractive arrangement. Accessibility collapse is high (0.80) because once a project adopts GPL, the cost of relicensing (unanimous contributor consent) makes exit nearly impossible. Resistance is moderate (0.60) reflecting ongoing proprietary-vendor circumvention, permissive-license advocacy, and occasional high-profile litigation. The temporal series show monotonic extraction accumulation and theater growth, consistent with a coordination mechanism whose founding rationale has atrophied while its enforcement apparatus matured.
 *
 * PERSPECTIVAL GAP:
 *   Proprietary vendors experience the constraint as a beneficial barrier that limits commons competition and justifies clean-room reimplementation. Commons contributors experience it as a restriction that traps their work in a licensing regime preventing commercial integration. The enforcement organizations experience it as a mission-critical legal mechanism. These divergent experiences produce different computed seat classifications: the proprietary vendor seat computes toward low directionality (beneficiary), the commons contributor seat toward high directionality (target), and the agenda-setter seat toward moderate directionality reflecting institutional identity-lock.
 *
 * DIRECTIONALITY LOGIC:
 *   The structural beneficiary is proprietary_vendors: they collect reduced competitive pressure and freedom to reimplement commons innovations without reciprocity. The structural target is commons_contributors: they pay through constrained integration options and reduced market reach. gpl_enforcement_orgs are agenda_setters whose directionality is structurally ambiguous â they are not financial beneficiaries but their institutional identity is fused to the constraint's persistence. No override is needed because the beneficiary/victim declarations plus exit options (mobile for proprietary vendors, constrained for commons contributors) correctly derive the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â preventing enclosure of software commons â is authored as dead in this reading. The constraint persists not because enclosure is a live threat but because the enforcement apparatus and institutional identity of the agenda-setters depend on it, and because proprietary vendors benefit from the isolation it creates. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges signals a mandatrophy: the arrangement has outlived its original function and now operates as a zombie constraint extracting from commons contributors. This prevents mislabeling the constraint as a rope or scaffold â the coordination it once provided (if any) has atrophied, and what remains is primarily extraction sustained by enforcement and beneficiary capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the GPL reciprocity obligation better understood as a freedom-preservation mechanism, a commons-protection technology, or a business-model restriction?',
    'Comparative analysis of license choice outcomes across matched project types, measuring commons contributor welfare and proprietary vendor competitive dynamics.',
    'Resolution would determine whether this constraint is a rope, tangled rope, or snare; the current reading treats it as a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between sibling readings of the GPL kernel').

omega_variable(
    proprietary_benefit_empirical_status,
    'Do proprietary vendors actually capture net competitive advantage from GPL isolation, or does the restriction harm them by reducing available commons infrastructure?',
    'Economic analysis of market segments with high vs. low GPL penetration, measuring proprietary vendor market share and reimplementation rates.',
    'If proprietary vendors are net harmed, the directionality and extraction claims of this reading collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_benefit_empirical_status, empirical, 'Whether the claimed beneficiary actually captures value from the constraint').

omega_variable(
    enforcement_target_ambiguity,
    'Does GPL enforcement primarily suppress proprietary integrators or commons contributors seeking wider adoption?',
    'Litigation target analysis: who is sued for violation, and what behavioral changes result.',
    'Determines the primary direction of suppression and refines the victim identification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_target_ambiguity, empirical, 'Who bears the primary burden of GPL enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 25, 0.52).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gpl_reciprocity_obligation kernel. The copyleft_as_restriction_reading decomposes the kernel into a snare that benefits proprietary vendors and extracts from commons contributors, while the sibling readings treat the same legal text as a freedom-preservation mechanism or commons-protection technology. The epsilon values diverge because the referent â the standing arrangement under the GPL viral clause â is assessed by different normative premises: restriction versus freedom versus commons.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
