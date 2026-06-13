% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive License Commons Coordination (Relaxation Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint models one reading of the contested kernel
 *   'permissive_license_text': the commons-coordination reading, which holds
 *   that copyright relaxation through permissive licenses (MIT, Apache 2.0,
 *   BSD) maximizes universal implementation freedom by minimizing legal
 *   friction. Under this reading, the license author voluntarily relinquishes
 *   enforcement leverage to enable unrestricted reuse, modification, and
 *   composition. The constraint coordinates the open-source ecosystem by
 *   providing a low-friction legal framework. No victim set exists under this
 *   reading — implementers benefit, the ecosystem benefits, and the author
 *   chooses dispossession. This reading is structurally distinct from the
 *   corporate_moat_reading (which sees permissive licensing as enabling
 *   proprietary extraction) and the copyleft_counterfactual_reading (which
 *   argues reciprocity-enforced licensing better protects commons interests).
 *   The three readings have different epsilon values, different beneficiary
 *   structures, and different answers to whether derivative products are
 *   extractive or beneficial.
 *
 * KEY AGENTS:
 *   - license_author: Chooses permissive licensing; voluntarily relaxes enforcement
 *   - universal_implementer_pool: Benefits from low legal friction; can compose freely
 *   - open_source_ecosystem: Benefits from rapid, friction-free integration and reuse
 *   - derivative_product_creators: Benefit from ability to build proprietary layers on open-source foundations
 *   - copyleft_advocates: Excluded; dispute the reading and argue for reciprocity-enforced licensing
 *   - copyright_law_system: Provides the background legal enforcement structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.12).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.08).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive License Commons Coordination (Relaxation Reading)").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "software_licensing/intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, '6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e').
narrative_ontology:cs_kernel_codification('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', fixed_text).
narrative_ontology:cs_authority_grounding('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', distributed).
narrative_ontology:cs_reading_relation('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', foundational, universal_implementation_freedom_maximized_by_relaxation).
narrative_ontology:cs_axiom_status(universal_implementation_freedom_maximized_by_relaxation, holdable).
narrative_ontology:cs_axiom_grounding('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', universal_implementation_freedom_maximized_by_relaxation, empirically_contingent).
narrative_ontology:cs_axiom('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', foundational, permissive_licensing_enables_commons_preservation_through_coordination).
narrative_ontology:cs_axiom_status(permissive_licensing_enables_commons_preservation_through_coordination, holdable).
narrative_ontology:cs_axiom_grounding('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', permissive_licensing_enables_commons_preservation_through_coordination, instrumental).
narrative_ontology:cs_reference_frame('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', permissive_licensing_as_freedom_enabling_mechanism).
narrative_ontology:cs_drift_state('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', contemporary_proprietary_integration_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('6d4b0f30-274b-46e0-aa8c-b731e5ab8c3e', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, universal_implementer_pool).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, open_source_ecosystem).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, derivative_product_creators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and publishes permissive license text (MIT, Apache 2.0, BSD family) allowing modification, redistribution, and commercial use with minimal attribution or reciprocity constraints. The author voluntarily relaxes copyright enforcement to maximize implementation freedom. They choose permissive over copyleft, accepting that derivative works may not carry the same license forward.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, license_author, agenda_setter,
    moderate, generational, mobile, global).

% Includes all developers, companies, researchers, and organizations that want to use, modify, and build on the licensed work. They benefit from low legal friction: no viral obligations, no license compatibility negotiation, no risk of license-propagation surprises. They can fork, patch, integrate into proprietary systems, and redistribute with minimal license overhead.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, universal_implementer_pool, beneficiary,
    organized, biographical, mobile, global).

% The distributed coordination structure of permissively-licensed components benefits from rapid integration, remix, and composition. Projects depend on each other's code with minimal license friction; the ecosystem grows through combinatorial reuse. Permissive licensing enables the ecosystem's canonical function: universal composability.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, open_source_ecosystem, beneficiary,
    organized, generational, mobile, global).

% Commercial and non-commercial entities that build products, services, or enhancements on permissively-licensed foundations. They benefit from the ability to integrate without propagating the original license; they can close their own proprietary layers while standing on open-source bases. Their business models (SaaS, proprietary extensions, services) are unencumbered.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, derivative_product_creators, beneficiary,
    powerful, generational, mobile, global).

% The copyright law system that underpins permissive licenses and makes them enforceable. It provides the background legal structure that makes the license a binding commitment, even though the author chooses minimal enforcement. The mechanism is present and operative but configured toward permissiveness rather than restriction.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, license_enforcement_mechanism, observer,
    institutional, generational, analytical, global).

% Communities and developers committed to GPL and other copyleft licenses argue that permissive licensing enables proprietary capture and allows extracted value to flow one-way out of the commons. They dispute the reading that permissive licensing maximizes freedom; they would argue that reciprocity-enforced licensing better protects collective interests. Their position is structurally excluded from this constraint's beneficiary narrative.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, copyleft_advocates, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Minimizes legal friction in software composition and reuse by offering clear, minimal permission grants: implementers can use, modify, and redistribute without navigating license compatibility, viral obligations, or reciprocity constraints. The constraint solves the coordination problem of enabling universal implementation freedom across heterogeneous projects and organizations.
% TRANSFER_FUNCTION: Transfers intellectual property rights (permission to use, modify, distribute) from the original author to an unlimited implementer pool, without reciprocal obligation. The author cedes enforcement: they could legally restrict use but choose not to. No value extraction occurs — the author does not collect from the transfer; they dispossess themselves of leverage and distribute the right freely.
% ABSENT_VOICES: Copyleft advocates, software commons preservation advocates, and collective-benefit-focused communities are not in the decision frame of individual permissive license authors. They would argue for reciprocal licensing to prevent proprietary capture; their position would shift the reading from freedom-maximization to commons-protection. They are structurally excluded because this reading centers the author's unilateral choice to relax, not a collective bargain.
% DISAPPEARANCE_RATIONALE: If permissive licensing disappeared and all code reverted to default copyright (all-rights-reserved), the open-source ecosystem would reorganize: projects would fragment into copyleft (GPL) or closed proprietary distributions. Derivative work composition would require renegotiation of permissions per integration point. The rapid, friction-free reuse that permissive licensing enables would cease; implementers would face legal uncertainty on every integration. Organizations would maintain private forks instead of contributing upstream.
% FOUNDING_PROBLEM: Early software distribution faced high legal friction: copyright law granted default all-rights-reserved status; integrating components from multiple sources required negotiating permission per-source; unclear permission boundaries discouraged reuse and contribution. Permissive licensing was developed to solve this: minimize legal friction by offering clear, minimal-restriction permission grants upfront.
% FOUNDING_PROBLEM_CORROBORATION: The open-source community and implementers attest the founding problem remains live: projects without permissive license clarity face integration friction and contributor hesitation. Independent testimony from software foundations (Apache, Mozilla, Python), corporate adopters (Google, Meta, Microsoft), and academic research on license adoption costs corroborate the problem. Copyleft advocates contest the characterization — they attest the problem is better solved by reciprocal licensing — but they do not dispute the founding problem's historical existence or the current demand for relaxed-friction mechanisms.
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12 at interval end) because the constraint does not move value from one party to another — the author voluntarily dispossesses themselves; the implementer pool gains permission without paying. Suppression is minimal (0.08) because the constraint operates through offered permission, not enforced restriction; implementers face no barrier to using the code or building proprietary extensions. Theater is low (0.15) because the constraint's function is straightforward: permission grants with clear terms. The measurement series is flat to slightly rising because the constraint itself is stable, though adoption and implementation pressure may gradually increase the operational load of the coordination mechanism. The rising trajectory is gentle (0.08→0.12 extractiveness over 40 time units) because friction costs do accumulate at scale — more implementers, more edge cases, more need for clarity and dispute resolution — but the constraint's core function (minimizing legal friction) remains constant. This reading assumes the founding problem (legal friction in software composition) is still live and that permissive licensing solves it effectively.
 *
 * PERSPECTIVAL GAP:
 *   The license author and the implementer pool should compute identical or near-identical types — both see genuine coordination with low extraction. The copyleft_advocates seat would compute very differently under the alternative reading (copyleft_counterfactual_reading): they would see permissive licensing as enabling proprietary capture and would classify the constraint as snare or tangled_rope, with beneficiaries limited to proprietary derivative creators and victims including the open-source commons. The corporate_moat_reading would also diverge: it would name proprietary derivative creators as the primary extractors, not beneficiaries, and would classify the constraint as snare with the open-source ecosystem as victim. The perspectival gap is fundamental to the kernel contest: the same license text yields different epsilon values and different beneficiary structures depending on which reading is applied.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, the license author is a dispossessor (d near 0.0, full beneficiary to the ecosystem, not to themselves). The universal implementer pool and open-source ecosystem are aligned beneficiaries (d near 0.0). Derivative product creators benefit from relaxed friction but do not pay extraction — they are beneficiaries without being targets. Copyleft advocates are excluded from the beneficiary narrative entirely; their interests are not modeled in this reading's structural data. Directionality across all seats is low (all d < 0.3) because no extraction occurs — the author gives freely, implementers receive freely. The constraint coordinates by permission, not by asymmetric transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not face mandatrophy risk in the technical sense: the founding problem (legal friction) remains live, the constraint (permissive licensing) still solves it, and the beneficiary set (universal implementers) still exists. However, the reading is contestable at the empirical level: if empirical analysis showed that permissive licensing systematically produces one-way value extraction to proprietary derivative creators (the corporate_moat_reading's claim), the reading would lose descriptive fit, not prescriptive legitimacy. The copyleft_counterfactual_reading contests the empirical premise that permissive licensing 'maximizes' freedom — it argues that freedom without reciprocity is illusory and that copyleft better serves commons interests. This is a reading-level dispute, not a mandatrophy of the founding problem itself. The reading remains live until empirical evidence establishes that permissive licensing either no longer solves the friction problem or systematically enables extractive capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permissive_vs_reciprocal_empirical,
    'Do permissive licenses empirically produce greater implementation freedom and ecosystem health than copyleft alternatives, or do they systematically enable proprietary capture that copyleft would prevent?',
    'Longitudinal analysis of ecosystem growth, contributor retention, downstream proprietary use patterns, and commons-contribution rates in permissively-licensed vs. copyleft projects matched on initial conditions and domain. Cross-project comparisons (Linux/GPL vs. Node.js/MIT, TensorFlow/Apache vs. OpenAI''s copyleft variants) over 10+ year horizons.',
    'If permissive licenses show greater ecosystem health and lower proprietary capture rates, the commons_coordination_reading is supported. If copyleft projects show better contributor retention and commons contribution despite lower proprietary integration, the copyleft_counterfactual_reading gains empirical ground, and the epsilon of this reading may rise (extraction of commons value by proprietary actors becomes detectable).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(permissive_vs_reciprocal_empirical, empirical, 'Whether permissive licensing produces greater freedom or enables capture relative to copyleft.').

omega_variable(
    author_intentionality_constraint,
    'Is the permissive license text a constraint on implementation freedom, or a voluntary enabler of freedom that removes a constraint (copyright default)? Does the distinction matter for classification?',
    'Conceptual analysis of constraint definition: a constraint typically imposes a structure that would not exist otherwise; permissive licensing removes a default structure (all-rights-reserved copyright) rather than imposing one. This reading treats the result (freedom-maximization) as the constraint; the copyleft_counterfactual_reading would treat the permissive text itself as a constraint on reciprocity enforcement.',
    'If permissive licensing is understood as removal of constraint rather than imposition of one, the reading''s classification as coordination rope is stable. If it is understood as a new constraint that enables proprietary layer-building (corporate_moat reading), the epsilon rises and the type may shift to tangled_rope (coordinating open-source developers while enabling proprietary extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(author_intentionality_constraint, conceptual, 'Whether permissive licensing is a constraint that frees or a removal of constraints, and whether this distinction affects classification.').

omega_variable(
    derivative_product_boundary,
    'Should derivative products (commercial extensions, proprietary layers, closed-source extensions on open-source foundations) be classified as beneficiaries of the constraint or as extractors exploiting it?',
    'Stakeholder interviews with derivative product creators, open-source communities, and commons advocates; econometric analysis of value flows (does the proprietary extension capture more value than the open-source base, and does that constitute asymmetric extraction?); comparison with GPL ecosystems where derivative proprietary products face reciprocal licensing pressure.',
    'If derivative products are beneficiaries (this reading), the constraint coordinates without extraction and epsilon stays low. If they are identified as extractors (corporate_moat reading), they become a victim/target seat, epsilon rises, and the constraint may reclassify to snare or tangled_rope. The boundary between ''building on'' and ''extracting from'' is the empirical crux.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_product_boundary, empirical, 'Whether derivative product creators are beneficiaries or extractors relative to the permissive-license constraint.').

omega_variable(
    reading_committer_ambiguity,
    'This constraint is one reading of a contested kernel; the reading''s persistence depends on the empirical and normative claims it makes (freedom-maximization, no-victim-set, low-extraction). What evidence would shift the reading from live to dead or contested?',
    'Monitor the three empirical omegas above. Additionally track: (1) adoption of reciprocal-licensing alternatives (growth in GPL or newer copyleft variants); (2) legal disputes over proprietary use of permissively-licensed code (suggest extraction/capture narrative gaining traction); (3) author regret and license-change patterns (authors moving from permissive to copyleft); (4) researcher consensus on commons welfare under permissive vs. copyleft licensing.',
    'If empirical omegas resolve against this reading, the reading''s status shifts from ''live'' (founding_problem_status) to ''contested'' or ''dead''. The constraint object would remain authored with this reading''s claim/metrics, but the corpus would record the shift in the founding_problem_status assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, empirical, 'The committer-level ambiguity: whether the commons_coordination_reading remains the live interpretation of permissive licensing, or whether empirical/normative shifts move it to contested or dead status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(perm_tr_t0, observed).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__commons_coordination_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(perm_tr_t5, observed).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__commons_coordination_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(perm_tr_t10, observed).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__commons_coordination_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(perm_tr_t20, observed).
narrative_ontology:measurement(perm_tr_t30, permissive_license_text__commons_coordination_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(perm_tr_t30, observed).
narrative_ontology:measurement(perm_tr_t40, permissive_license_text__commons_coordination_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(perm_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(perm_be_t0, observed).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__commons_coordination_reading, base_extractiveness, 5, 0.09).
narrative_ontology:measurement_basis(perm_be_t5, observed).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__commons_coordination_reading, base_extractiveness, 10, 0.1).
narrative_ontology:measurement_basis(perm_be_t10, observed).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__commons_coordination_reading, base_extractiveness, 20, 0.11).
narrative_ontology:measurement_basis(perm_be_t20, observed).
narrative_ontology:measurement(perm_be_t30, permissive_license_text__commons_coordination_reading, base_extractiveness, 30, 0.12).
narrative_ontology:measurement_basis(perm_be_t30, observed).
narrative_ontology:measurement(perm_be_t40, permissive_license_text__commons_coordination_reading, base_extractiveness, 40, 0.12).
narrative_ontology:measurement_basis(perm_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__commons_coordination_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement_basis(perm_su_t0, observed).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__commons_coordination_reading, suppression_requirement, 5, 0.06).
narrative_ontology:measurement_basis(perm_su_t5, observed).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__commons_coordination_reading, suppression_requirement, 10, 0.07).
narrative_ontology:measurement_basis(perm_su_t10, observed).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__commons_coordination_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(perm_su_t20, observed).
narrative_ontology:measurement(perm_su_t30, permissive_license_text__commons_coordination_reading, suppression_requirement, 30, 0.08).
narrative_ontology:measurement_basis(perm_su_t30, observed).
narrative_ontology:measurement(perm_su_t40, permissive_license_text__commons_coordination_reading, suppression_requirement, 40, 0.08).
narrative_ontology:measurement_basis(perm_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__commons_coordination_reading, 0.08).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'permissive_license_text'. The kernel is the stabilized permission grant text (MIT, Apache 2.0, BSD family). This reading (commons_coordination_reading) interprets permissive licensing as maximizing universal implementation freedom via minimal legal friction. Sibling readings offer structurally distinct interpretations: corporate_moat_reading (permissive licensing enables uncompensated proprietary extraction) and copyleft_counterfactual_reading (reciprocity-enforced licensing better protects commons interests). Each reading has its own epsilon, beneficiary/victim structure, and type classification. They are linked via affects_constraints because changes to one reading (empirical findings, normative consensus shift) create downstream pressure on the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
