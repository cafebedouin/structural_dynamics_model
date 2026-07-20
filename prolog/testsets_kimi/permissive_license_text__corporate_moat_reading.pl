% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   This constraint is the corporate_moat_reading of the contested kernel
 *   permissive_license_text. The kernel comprises standard permissive
 *   software licenses (MIT, Apache, BSD) that relax copyright enforcement to
 *   maximize reuse. The corporate moat reading asserts that this relaxation
 *   does not primarily serve universal coordination; instead, it structurally
 *   enables enterprise corporations to build proprietary moats on
 *   uncompensated maintainer labor. The sibling readings are
 *   commons_coordination_reading (the arrangement is genuine frictionless
 *   coordination) and copyleft_counterfactual_reading (the absence of
 *   reciprocity is the exploitable flaw). This story authors a snare
 *   classification independently of the metrics: the metrics describe an
 *   arrangement whose extraction has grown over time while the coordination
 *   narrative has become increasingly theatrical.
 *
 * KEY AGENTS:
 *   - Enterprise corporations (beneficiary/institutional): capture uncompensated labor in proprietary derivatives and SaaS moats.
 *   - Individual maintainers (payer/moderate/identity_locked): produce the underlying infrastructure, ideologically bound to permissive norms, bearing the extraction.
 *   - Open source foundations (agenda_setter/institutional): promote and normalize permissive texts, constrained by corporate sponsorship.
 *   - Copyleft advocates (excluded/organized): argue for reciprocity, marginalized in corporate-dominated discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.62).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.48).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text â Corporate Moat Reading").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '235b7ffe-2b43-4ea3-bb59-77c552951a2e').
narrative_ontology:cs_kernel_codification('235b7ffe-2b43-4ea3-bb59-77c552951a2e', fixed_text).
narrative_ontology:cs_authority_grounding('235b7ffe-2b43-4ea3-bb59-77c552951a2e', lineage).
narrative_ontology:cs_interpretation_layer_present('235b7ffe-2b43-4ea3-bb59-77c552951a2e').
narrative_ontology:cs_reading_relation('235b7ffe-2b43-4ea3-bb59-77c552951a2e', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('235b7ffe-2b43-4ea3-bb59-77c552951a2e', permissive_license_text__copyleft_counterfactual_reading, influences).
narrative_ontology:cs_axiom('235b7ffe-2b43-4ea3-bb59-77c552951a2e', foundational, proprietary_derivatization_without_reciprocity_legitimate).
narrative_ontology:cs_axiom_status(proprietary_derivatization_without_reciprocity_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('235b7ffe-2b43-4ea3-bb59-77c552951a2e', proprietary_derivatization_without_reciprocity_legitimate, conventional).
narrative_ontology:cs_axiom('235b7ffe-2b43-4ea3-bb59-77c552951a2e', foundational, uncompensated_maintainer_labor_as_efficiency_optimum).
narrative_ontology:cs_axiom_status(uncompensated_maintainer_labor_as_efficiency_optimum, holdable).
narrative_ontology:cs_axiom_grounding('235b7ffe-2b43-4ea3-bb59-77c552951a2e', uncompensated_maintainer_labor_as_efficiency_optimum, instrumental).
narrative_ontology:cs_reference_frame('235b7ffe-2b43-4ea3-bb59-77c552951a2e', neutral_legal_infrastructure).
narrative_ontology:cs_drift_state('235b7ffe-2b43-4ea3-bb59-77c552951a2e', contemporary_cloud_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('235b7ffe-2b43-4ea3-bb59-77c552951a2e', '').
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

% Integrate permissively licensed code into proprietary products and cloud services without reciprocity obligations, capturing the delta between maintainer labor costs and proprietary revenue. They influence licensing norms through foundation sponsorship and can restructure licensing strategies or jurisdictions if regulatory pressure arises.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Produce and sustain core software infrastructure released under permissive licenses, often without direct compensation. Their labor is incorporated into proprietary derivatives and hosted services that generate revenue for corporations. Many are ideologically committed to open-source norms that equate permissive licensing with moral virtue, making demands for reciprocity or payment feel like community betrayal. Exit means re-licensing and facing reputational sanction, or abandoning the project.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    moderate, biographical, identity_locked, global).

% Steward and promote permissive license texts as the preferred legal infrastructure for open collaboration. Their governance and operating budgets often depend on corporate sponsorship, which constrains their ability to advocate for reciprocity requirements or economic compensation mechanisms for individual maintainers.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, open_source_foundations, agenda_setter,
    institutional, generational, constrained, global).

% Argue that copyright relaxation without reciprocity structurally enables exploitation and that viral copyleft licenses are necessary to prevent proprietary capture. They are systematically marginalized in corporate-funded discourse, conference circuits, and governance bodies where permissive licensing is treated as the only legitimate open position.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, copyleft_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates bilateral legal negotiation for software reuse, enabling decentralized development and distribution across organizational boundaries.
% TRANSFER_FUNCTION: Moves uncompensated development labor and intellectual value from individual maintainers to enterprise corporations, which capture it in proprietary derivative products and hosted services.
% ABSENT_VOICES: Individual maintainers who lack resources to enforce copyright claims, and copyleft advocates arguing for reciprocity, are excluded from the corporate-dominated governance and discourse spaces where permissive licensing is entrenched as the default.
% DISAPPEARANCE_RATIONALE: If permissive licenses no longer enabled uncompensated proprietary extraction, enterprise corporations would face immediate cost pressure to negotiate licenses, contribute code reciprocally, or pay maintainers. The current SaaS and proprietary software moat built on free labor would begin to collapse; maintainer labor markets and license distribution would reorganize around reciprocity or direct compensation.
% FOUNDING_PROBLEM: Legal friction and fear of litigation prevented software reuse and collaborative improvement across organizational boundaries in early computing.
% FOUNDING_PROBLEM_CORROBORATION: Enterprise-funded legal scholarship and open-source foundations attest the friction problem remains live. Independent software labor researchers, maintainer collectives, and copyleft advocates attest the friction problem is solved and the arrangement now functions primarily to legitimize uncompensated extraction; they point to the rise of open source as corporate strategy as corroboration that the founding coordination problem is dead.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.62) is moderate-high: the constraint moves substantial value from maintainers to corporations, but some genuine coordination (reduced friction) persists as cover. Suppression (0.48) reflects the collapse of viable alternatives for many maintainers â reciprocity licensing is technically available but socially and economically suppressed by corporate capture of discourse and funding. Theater ratio (0.42) captures the growing gap between open-source community rhetoric and the reality of proprietary capture. Accessibility collapse (0.60) is high because once a project is permissively licensed and integrated into corporate stacks, re-licensing is practically impossible due to multi-party copyright and network effects. Resistance (0.38) is moderate but rising, evidenced by maintainer license changes (Elastic, Redis, Mongo) and growing labor advocacy. Measurements share a single time grid to prevent misaligned drift signals.
 *
 * PERSPECTIVAL GAP:
 *   The enterprise corporation seat computes the constraint as benign infrastructure or even subsidy â it receives code without reciprocity and frames this as efficient market coordination. The individual maintainer seat computes it as extraction â they bear the labor cost and opportunity cost of uncompensated corporate use. The agenda-setter foundation seat sits between, administrating the text while structurally dependent on corporate funders. The engine derives this divergence from the same structural data: low directionality for the beneficiary, high directionality for the identity-locked payer.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are the structural beneficiary: they collect the extraction (low d, near 0.0). Individual maintainers are the structural target: they pay through uncompensated labor and identity-locked exit (high d, near 1.0). Open source foundations are agenda-setters with constrained exit; their directionality is intermediate but biased toward the beneficiary side due to funding capture. Copyleft advocates are excluded from the bargaining table and would experience high directionality if they were seated.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the R5 genealogy check, this constraint could be misclassified as a rope or scaffold: it genuinely solved legal friction in software reuse (founding problem). However, the founding problem status is contested and increasingly read as dead. The arrangement persists not because legal friction still threatens software reuse, but because the permissive text now serves as a legal instrument for proprietary extraction. The rising theater_ratio and base_extractiveness over the measurement interval corroborate the mandatrophy hypothesis. The classification as snare prevents the error of treating a zombie coordination mechanism as live infrastructure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maintainer_consent_as_coercion,
    'Do individual maintainers genuinely consent to uncompensated corporate use, or does structural coercion (reputation dependency, ideological capture, lack of alternative funding) render the consent illusory?',
    'Comparative analysis of maintainer economic outcomes relative to corporate value capture; post-license-change survival rates for projects that attempted reciprocity.',
    'If consent is structurally coerced, suppression is higher than measured and the constraint is more strongly a snare; if genuine, extraction may be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintainer_consent_as_coercion, empirical, 'Whether maintainer participation is freely chosen or structurally compelled.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of maintainer alternatives structural (market network effects, legal cost barriers) or internalized (open-source ideology equating permissive licensing with virtue)?',
    'Post-exit trajectory: if maintainers who switch to reciprocal licensing recover economically and psychologically, suppression was partly internalized.',
    'Internalized suppression increases effective extraction beyond structural measures because the target carries the constraint after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism for maintainers.').

omega_variable(
    kernel_reading_ambiguity,
    'Does the permissive license text inherently enable corporate moats, or is the moat an emergent property of surrounding market structure independent of the text?',
    'Compare extraction levels under permissive licensing in markets with strong vs weak network effects and corporate concentration.',
    'If independent of the text, the corporate_moat reading is better classified as a market-structure constraint; if dependent on the text''s specific legal permissions, the reading is text-bound.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether extraction is text-bound or market-emergent.').

omega_variable(
    cs_reading_position,
    'This constraint is the corporate_moat_reading of kernel permissive_license_text. How would the classification change if the commons_coordination_reading or copyleft_counterfactual_reading were adopted instead?',
    'Compare the epsilon, beneficiary/victim structure, and directionality computed for each sibling reading.',
    'The commons reading would classify as rope or tangled_rope with no victims; the copyleft reading would classify the absence of reciprocity as the snare. The corporate_moat reading identifies the text itself as the enabler.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cs_reading_position, conceptual, 'Position of this reading within the kernel''s contested space.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__corporate_moat_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__corporate_moat_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__corporate_moat_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__corporate_moat_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__corporate_moat_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__corporate_moat_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__corporate_moat_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__corporate_moat_reading, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__corporate_moat_reading, suppression_requirement, 5, 0.33).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__corporate_moat_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__corporate_moat_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__corporate_moat_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__corporate_moat_reading, suppression_requirement, 25, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% The natural-language label 'permissive license text' conflates three structurally distinct constraints. The commons_coordination_reading has negligible extraction and no victims; the corporate_moat_reading has moderate epsilon and identifiable victims; the copyleft_counterfactual_reading frames the absence of reciprocity as the exploitable design flaw. They are linked as a constraint family because they share the same kernel text but instantiate different structural claims with different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
