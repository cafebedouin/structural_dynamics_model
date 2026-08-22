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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Permissive Open-Source License as Corporate Extraction Channel
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This story authors the corporate-moat reading of the
 *   permissive-license-text kernel: the same license clause that commons
 *   advocates read as maximal-freedom coordination is here read as the
 *   structural enabler of uncompensated extraction. Under a permissive
 *   license (MIT/BSD/Apache), a maintainer's labor becomes available to any
 *   downstream party — including well-capitalized firms — with no reciprocity
 *   obligation. Over the last two decades, this has produced a recurring
 *   pattern: individual maintainers and small foundations sustain critical
 *   infrastructure on volunteer or donation-funded labor while cloud
 *   hyperscalers and enterprise vendors build proprietary, revenue-generating
 *   services on top with no obligation to share code, revenue, or even
 *   meaningful funding back. The claimed type (snare) and the metrics
 *   (moderate-high extraction, moderate suppression) are authored
 *   independently: suppression here is not legal coercion but structural
 *   lock-in — a maintainer cannot retroactively impose reciprocity once the
 *   license is chosen and the project has scaled past the point where
 *   relicensing is practical.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.61).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.42).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive Open-Source License as Corporate Extraction Channel").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '176d9f00-59d4-4f0a-8442-efd8e3833104').
narrative_ontology:cs_kernel_codification('176d9f00-59d4-4f0a-8442-efd8e3833104', fixed_text).
narrative_ontology:cs_authority_grounding('176d9f00-59d4-4f0a-8442-efd8e3833104', practice).
narrative_ontology:cs_interpretation_layer_present('176d9f00-59d4-4f0a-8442-efd8e3833104').
narrative_ontology:cs_reading_relation('176d9f00-59d4-4f0a-8442-efd8e3833104', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('176d9f00-59d4-4f0a-8442-efd8e3833104', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('176d9f00-59d4-4f0a-8442-efd8e3833104', foundational, uncompensated_derivative_capture_is_structural_defect).
narrative_ontology:cs_axiom_status(uncompensated_derivative_capture_is_structural_defect, holdable).
narrative_ontology:cs_axiom_grounding('176d9f00-59d4-4f0a-8442-efd8e3833104', uncompensated_derivative_capture_is_structural_defect, empirically_contingent).
narrative_ontology:cs_axiom('176d9f00-59d4-4f0a-8442-efd8e3833104', secondary, license_text_neutrality_is_insufficient_absent_reciprocity).
narrative_ontology:cs_axiom_status(license_text_neutrality_is_insufficient_absent_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('176d9f00-59d4-4f0a-8442-efd8e3833104', license_text_neutrality_is_insufficient_absent_reciprocity, instrumental).
narrative_ontology:cs_reference_frame('176d9f00-59d4-4f0a-8442-efd8e3833104', adoption_maximizing_friction_reduction).
narrative_ontology:cs_drift_state('176d9f00-59d4-4f0a-8442-efd8e3833104', post_hyperscaler_saas_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('176d9f00-59d4-4f0a-8442-efd8e3833104', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, cloud_hyperscalers).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_saas_platforms).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, small_open_source_foundations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, volunteer_contributor_pools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, end_users_of_derivative_products).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and sustains the codebase largely as unpaid or underpaid labor, released under a permissive license (MIT/BSD/Apache) that imposes no reciprocity requirement. Cannot compel any company that builds a commercial product on top of the code to share improvements, pay royalties, or even provide attribution beyond boilerplate. Exit means abandoning the project or relicensing going forward, which does not claw back value already extracted and risks fracturing the user community that depends on the code.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    powerless, biographical, trapped, global).

% Coordinates a permissively licensed project's governance and funding on donations and small grants while much larger commercial derivatives generate substantial revenue from the same code. Cannot renegotiate the license retroactively without contributor consent from potentially hundreds of past authors, making relicensing to a reciprocal scheme practically infeasible once the project has scaled.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, small_open_source_foundations, payer,
    moderate, biographical, constrained, global).

% Submits patches, documentation, and bug fixes for free under the assumption of communal benefit, unaware or unconcerned that the same permissive terms let any company fork the aggregate labor into a closed, monetized product with no obligation to compensate or return improvements. Individually mobile — can walk away from any single project — but the pool as a whole keeps replenishing the resource being extracted.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, volunteer_contributor_pools, payer,
    powerless, immediate, mobile, global).

% Takes permissively licensed database, orchestration, and infrastructure software, wraps it as a managed proprietary service, and captures the majority of the commercial value while contributing a comparatively small fraction of code or funding back to the originating project. Faces no legal obligation to share the service layer, pay license fees, or open-source its modifications, and can walk away from any single project's community entirely without consequence given the abundance of substitutable permissively licensed alternatives.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, cloud_hyperscalers, beneficiary,
    institutional, generational, arbitrage, global).

% Embeds permissively licensed libraries into closed commercial products, avoiding the cost of building equivalent functionality in-house while keeping the resulting product proprietary. Benefits from the absence of any share-alike or royalty obligation and can swap in a different permissively licensed dependency if any single project becomes inconvenient or tries to change terms.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, proprietary_software_vendors, beneficiary,
    institutional, generational, arbitrage, global).

% Builds subscription products on top of permissively licensed frameworks and tooling, marketing the resulting service without disclosing how much of its value derives from unpaid community labor. Structurally insulated from any claim by the underlying project because the license was drafted to impose no reciprocity.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_saas_platforms, beneficiary,
    institutional, generational, arbitrage, global).

% Consumes cheaper, more polished commercial products built atop free open-source labor, benefiting from the price competition permissive licensing enables, largely unaware of the underlying labor asymmetry.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, end_users_of_derivative_products, beneficiary,
    organized, immediate, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__corporate_moat_reading, end_users_of_derivative_products, observer).

% Argues from outside most permissively licensed projects' governance that only reciprocal (copyleft) licensing terms would have prevented this extraction, but has no standing to alter licensing decisions already made by maintainers years earlier, and is often dismissed within permissive-license communities as ideologically motivated rather than structurally correct.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_advocacy_community, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, cloud_hyperscalers).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing does solve a real coordination problem: it minimizes legal friction for anyone who wants to build on the code, maximizing adoption, interoperability, and downstream reuse without requiring lawyers to review compliance obligations.
% TRANSFER_FUNCTION: Moves accumulated, largely unpaid engineering labor from individual maintainers and volunteer contributor pools into commercial value captured by well-resourced firms that face no obligation to share revenue, code, or attribution proportional to what they extracted.
% ABSENT_VOICES: Individual maintainers who wrote the original code under a permissive license years or decades ago are frequently no longer active in the project and have no mechanism to object to how their labor is currently monetized. The copyleft advocacy community raises the structural objection but has no governance standing over projects it does not maintain.
% DISAPPEARANCE_RATIONALE: If permissive licensing terms vanished and all such code reverted to restrictive default copyright, cloud hyperscalers and proprietary vendors would lose free access to a massive existing commons and would need to either license commercially, contribute back, or reimplement from scratch — a major rearrangement for them. Individual maintainers dispute whether their position would actually improve, since without the adoption permissive licensing drove, many of these projects would never have reached the scale where extraction became visible or lucrative at all; the verdict depends on which counterfactual license regime replaces the current one.
% FOUNDING_PROBLEM: Permissive licenses were adopted to solve real friction: proprietary vendors and enterprises were reluctant to touch software under licenses with unclear or reciprocal obligations (GPL contamination fears), and maintainers wanted maximum adoption and interoperability rather than maximum leverage over downstream users.
% FOUNDING_PROBLEM_CORROBORATION: Enterprise legal departments and standards bodies attest the friction-reduction problem is still live and cite permissive licensing's continued dominance in infrastructure software as evidence it functions as intended. Independent researchers studying open-source sustainability (e.g. reports from the Ford Foundation and Tidelift's maintainer surveys) attest from outside the beneficiary set that the original adoption-maximization rationale has curdled into an uncompensated-extraction arrangement for a substantial share of foundational infrastructure projects, without corporate beneficiaries themselves acknowledging the shift.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, contested).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (0.61 at interval end) reflects the growing asymmetry between the value captured by downstream commercial users and the value returned to maintainers, rising over the interval as cloud and SaaS business models matured around free infrastructure software. Suppression (0.42) is moderate rather than high because nothing coerces maintainers into permissive licensing at the outset — the trap closes only after adoption has scaled, at which point relicensing collective action costs become prohibitive. Theater ratio (0.38) reflects the rise of corporate 'open source sponsorship' programs, contributor recognition badges, and foundation-branded goodwill gestures that substitute symbolic acknowledgment for proportional compensation.
 *
 * PERSPECTIVAL GAP:
 *   From the corporate beneficiary seat, the license working exactly as designed: permissive terms invited exactly this kind of unrestricted reuse, and nothing was promised beyond what the license text grants. From the maintainer seat, the same clause increasingly reads as an extraction channel that the original drafters (often the maintainers themselves, decades earlier) did not anticipate would scale to this degree of asymmetry. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate which seat is 'right' — that adjudication is exactly what the sibling readings (commons_coordination_reading, copyleft_counterfactual_reading) exist to carry.
 *
 * DIRECTIONALITY LOGIC:
 *   Cloud hyperscalers, proprietary vendors, and enterprise SaaS platforms are declared beneficiaries: they capture commercial value from permissively licensed code with no reciprocity obligation and can freely substitute among competing projects, giving them near-arbitrage exit. Individual maintainers, small foundations, and volunteer contributor pools are declared victims: their labor is the extracted resource, and once a project has scaled under a permissive license, they are structurally trapped or heavily constrained — they cannot retroactively impose terms on already-permissively-licensed code or on the network of downstream users now depending on the status quo.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reducing legal friction to maximize adoption) was real and, by many measures, remains partially live — permissive licensing genuinely lowered the barrier to enterprise adoption of open infrastructure. But the mandate has drifted: what began as a friction-reduction mechanism now also functions, for a subset of high-value projects, as a mechanism transferring accumulated unpaid labor into corporate balance sheets with no adjustment mechanism. Classifying this as snare (rather than mountain or rope) prevents mislabeling a structurally maintained extraction pattern as either an immutable fact of software economics or as costless, victimless coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_intent_vs_structural_drift,
    'Did corporate beneficiaries anticipate and design toward this extraction pattern when selecting or steering projects toward permissive licenses, or did the extraction emerge as an unintended structural consequence of adoption-maximizing license choices made by maintainers themselves?',
    'Archival analysis of corporate open-source strategy documents, funding patterns for projects immediately before major relicensing pushes, and interviews with maintainers about pressure applied by corporate contributors during original licensing decisions.',
    'If beneficiaries actively steered license selection toward permissive terms in anticipation of future extraction, the snare classification strengthens toward intentional design; if the pattern is emergent from maintainer-driven choices later exploited, the classification still holds but the enforcement/suppression story shifts from designed trap to opportunistic capture of an unguarded commons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_intent_vs_structural_drift, empirical, 'Whether corporate extraction from permissive licensing was designed or opportunistically discovered.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the permissive_license_text kernel genuinely multiply-realizable as coordination, extraction-enabling, or copyleft-necessitating — or does one reading better track the actual empirical distribution of outcomes across the corpus of permissively licensed projects?',
    'Large-sample empirical study comparing value capture, contributor compensation, and project sustainability across permissively licensed vs. copyleft-licensed projects of comparable adoption scale, controlling for project age and domain.',
    'If corporate capture is empirically common only among a narrow subset of highly commercially valuable infrastructure projects, the corporate_moat_reading''s ε should be understood as a conditional/high-variance property rather than a universal feature of permissive licensing — this would not change this story''s own ε (which is about the standing arrangement as this reading sees it) but would inform how much weight the reading carries across the corpus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the corporate-moat pattern is a general property of permissive licensing or a conditional feature of a subset of highly commercialized projects.').

omega_variable(
    relicensing_feasibility_ceiling,
    'At what scale of contributor count and downstream adoption does relicensing to a reciprocal scheme become practically infeasible, and is that ceiling a hard structural fact or a solvable coordination problem given sufficient legal/organizational investment?',
    'Case studies of successful (e.g. MongoDB''s SSPL transition) and failed relicensing attempts, mapping contributor count, legal cost, and community fragmentation outcomes against project scale at the time of the attempt.',
    'If relicensing is genuinely infeasible past a low contributor threshold, the trapped exit_options for maintainers is structurally accurate and durable; if well-resourced legal effort can usually clear the CLA/consent hurdle, maintainer exit options are better described as constrained-but-navigable rather than trapped, softening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relicensing_feasibility_ceiling, empirical, 'Whether maintainer lock-in against relicensing is a hard structural ceiling or a resolvable coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perm_tr_t4, permissive_license_text__corporate_moat_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__corporate_moat_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.32).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__corporate_moat_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.38).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(perm_be_t4, permissive_license_text__corporate_moat_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__corporate_moat_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.54).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__corporate_moat_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.61).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__corporate_moat_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, resource_allocation).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the permissive_license_text kernel. commons_coordination_reading authors the same license clause as a rope (adoption-maximizing coordination with minimal friction). copyleft_counterfactual_reading authors the absence of reciprocity as the structural defect a GPL-style requirement would have prevented. This story (corporate_moat_reading) authors the standing arrangement as actually exploited by well-capitalized downstream firms as a moderate-epsilon snare. Each reading has its own independent ε and stakeholder structure per the ε-invariance principle; they are linked here rather than merged into one story with an observable parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
