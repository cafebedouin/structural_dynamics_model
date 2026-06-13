% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Restriction Reading)
 *   domain: intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint instantiates the 'copyleft as restriction' reading of the
 *   GPL reciprocity obligation. Under this reading, viral licensing is framed
 *   NOT as a freedom-preservation mechanism (the commons or freedom readings)
 *   but as a legal constraint that prohibits proprietary business models from
 *   integrating GPL software without sacrificing proprietary code. The
 *   reading acknowledges GPL's functional role in the commons but emphasizes
 *   that the mechanism operates via restriction: it prevents certain
 *   integrations, forbids certain business models, and forces creators into
 *   binary choices (all-open or all-proprietary). The constraint benefits
 *   proprietary vendors by exempting them from reciprocity while restricting
 *   commons contributors from building proprietary derivatives. This reading
 *   is empirically distinct from siblings—its epsilon measures the extractive
 *   force on creators, not the freedom benefit to users.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors — structurally benefited by exemption and restriction on competitors
 *   - commons_contributors — structurally burdened by prohibition on proprietary integration of their own work
 *   - closed_source_integrators — beneficiaries of legal certainty that GPL cannot be forked into competing proprietary products
 *   - commons_governance_bodies — agenda-setters enforcing reciprocity and adjudicating derivative-work boundaries
 *   - derivative_work_creators — forced to choose between full GPL adoption or proprietary exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.68).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.52).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation (Copyleft as Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'f6c79bf4-302e-4d84-8735-9420e2e4d035').
narrative_ontology:cs_kernel_codification('f6c79bf4-302e-4d84-8735-9420e2e4d035', fixed_text).
narrative_ontology:cs_authority_grounding('f6c79bf4-302e-4d84-8735-9420e2e4d035', lineage).
narrative_ontology:cs_interpretation_layer_present('f6c79bf4-302e-4d84-8735-9420e2e4d035').
narrative_ontology:cs_reading_relation('f6c79bf4-302e-4d84-8735-9420e2e4d035', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6c79bf4-302e-4d84-8735-9420e2e4d035', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_axiom('f6c79bf4-302e-4d84-8735-9420e2e4d035', foundational, reciprocal_obligation_as_business_model_constraint).
narrative_ontology:cs_axiom_status(reciprocal_obligation_as_business_model_constraint, holdable).
narrative_ontology:cs_axiom_grounding('f6c79bf4-302e-4d84-8735-9420e2e4d035', reciprocal_obligation_as_business_model_constraint, deontological).
narrative_ontology:cs_axiom('f6c79bf4-302e-4d84-8735-9420e2e4d035', secondary, proprietary_integration_impermissible_without_source_release).
narrative_ontology:cs_axiom_status(proprietary_integration_impermissible_without_source_release, holdable).
narrative_ontology:cs_axiom_grounding('f6c79bf4-302e-4d84-8735-9420e2e4d035', proprietary_integration_impermissible_without_source_release, conventional).
narrative_ontology:cs_reference_frame('f6c79bf4-302e-4d84-8735-9420e2e4d035', gpl_as_enforceable_legal_restriction).
narrative_ontology:cs_drift_state('f6c79bf4-302e-4d84-8735-9420e2e4d035', post_2010_permissive_dominance, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f6c79bf4-302e-4d84-8735-9420e2e4d035', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, closed_source_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, derivative_work_creators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, intellectual_property_as_enforceable_constraint).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_restriction_reading, property_rights_trump_usage_freedom).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Can license copyleft software's functionality without reciprocating; they extract value by integrating GPL components into proprietary closed-source products while retaining distribution and modification rights, then licensing the combination exclusively. The reciprocity obligation protects them by preventing competitors from legally copying their proprietary integration layered over GPL infrastructure. They benefit from the GPL's functionality while exempting themselves from its transparency demands.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, beneficiary,
    powerful, generational, arbitrage, global).

% Contribute improvements to GPL software expecting those improvements to remain in the commons and benefit the broader community. The reciprocity clause prevents them from ever integrating GPL-licensed work into proprietary systems without either surrendering their proprietary code or ceasing to use GPL components. Their creative work becomes a gift with a contractual ceiling: they cannot benefit from proprietary integration without abandoning their own property rights or the GPL altogether.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_contributors, payer,
    moderate, biographical, identity_locked, global).

% Can safely incorporate GPL dependencies into closed-source products (as long as they control the integration layer and do not distribute modified GPL source). The obligation prevents GPL communities from forking their proprietary work or building competitive closed-source alternatives using the same GPL foundations. The constraint licenses the risk: they know competitors cannot legally replicate their exact closed-source+GPL combination without triggering reciprocity.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, closed_source_integrators, beneficiary,
    institutional, generational, constrained, global).

% Face a hard choice when building on GPL software: either publish all modifications under GPL (surrendering proprietary value) or cease using the GPL dependency. They cannot create hybrid proprietary-open combinations; they are forced to either fully participate in the commons or exit entirely. The constraint is not negotiable per-work—it is baked into the license.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, derivative_work_creators, payer,
    moderate, biographical, constrained, global).

% Maintain and enforce GPL-licensed projects, adjudicating compliance and licensing disputes. They set the agenda for what constitutes a derivative work, what triggers reciprocity, and what enforcement actions to pursue. Their enforcement infrastructure makes the legal obligation into an operational reality.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, commons_governance_bodies, agenda_setter,
    organized, generational, analytical, global).

% End users of closed-source software that incorporates GPL components do not see the licensing structure. They receive the benefit of GPL-funded innovation without knowing or consenting to the reciprocity obligation. The constraint affects their choices indirectly through the software vendors' business model decisions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_users_benefiting_via_integration, observer,
    powerless, immediate, trapped, global).

% Analyze whether GPL's reciprocity obligation functions as a de facto patent licensing mechanism. They debate whether the constraint is justified by copyright (controlling distribution and modification of a copyrightable work) or extends into patent deterrence territory (preventing proprietary firms from using GPL-licensed patents without surrendering proprietary code). This reading treats GPL as a restriction mechanism that may exceed copyright's traditional scope.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, patent_licensing_strategists, observer,
    powerful, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single legal framework for managing shared software infrastructure: all users have visible rights (access to source, modification rights), and contributors understand that their work will be protected against proprietary enclosure. This solves the coordination problem of preventing a commons tragedy of the commons—ensuring contributed software stays in the commons.
% TRANSFER_FUNCTION: Transfers the ability to integrate GPL software into proprietary systems from commons contributors to proprietary vendors and closed-source integrators. Commons contributors surrender the option to build proprietary products on top of their own open-source work; proprietary vendors gain the legal ability to use GPL infrastructure without reciprocating. The constraint also transfers reputational burden: proprietary firms are freed from public obligation to contribute back.
% ABSENT_VOICES: Proprietary software developers who would prefer permissive licensing; users of proprietary systems who benefit from GPL innovation but are unaware the constraint exists; potential derivative-work creators who are discouraged from attempting hybrid models and thus never appear to testify their preferences; patent holders in GPL-licensed projects who benefit from patent indemnification clauses but whose voices are eclipsed by copyright reciprocity debate.
% DISAPPEARANCE_RATIONALE: If the GPL reciprocity obligation vanished and were replaced with permissive licensing (e.g., MIT), proprietary vendors could legally fork GPL infrastructure into closed-source products without any obligation to contribute back. Commons contributors would lose the ability to ensure their work stays in the commons. Proprietary software would immediately incorporate more open-source components. The boundary between proprietary and open-source software would blur as hybrid closed-source+open licensing became legal.
% FOUNDING_PROBLEM: Software commons needed protection against enclosure: early open-source developers contributed work expecting it to remain free, but without legal reciprocity, proprietary vendors could harvest the commons and privatize the benefit, leaving contributors behind.
% FOUNDING_PROBLEM_CORROBORATION: GPL creators (Stallman, Free Software Foundation) and commons advocates attest the founding problem remains live—proprietary enclosure is a persistent threat. Proprietary vendors and libertarian technologists attest the problem is overstated—permissive licensing has thrived alongside GPL, and the constraint is now an obstacle to beneficial integration. Independent technology historians document that permissive open-source licenses (MIT, BSD, Apache) have become dominant post-2010, suggesting the enclosure threat was either solved or was never as severe as the GPL reading asserts.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).

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
 *   Extractiveness is substantial (0.68) because the constraint transfers the integration option exclusively to proprietary vendors while denying it to commons contributors. A contributor who writes GPL software and later wishes to build a proprietary product incorporating their own work faces legal prohibition—the restriction extracts by foreclosing a business model path. Suppression is moderate (0.52) because alternatives exist (permissive licensing, proprietary-only development, forking to a permissive license) but are costly and visible—not coercive, but structured disincentives. Theater is low (0.28) because the constraint's enforcement is largely transparent: license compliance is explicit, compliance disputes are documented, and the mechanism is widely understood. The measurement series shows extractiveness plateauing by t=25, suggesting the constraint has matured—initial adoption friction (1990s–2000s) gave way to normalized operation. Theater remains stable because enforcement infrastructure did not theatricalize over the interval; the obligation was always legible.
 *
 * PERSPECTIVAL GAP:
 *   Proprietary vendors and commons contributors compute radically different constraint types from the same structural data. For vendors, the constraint is rope—it solves the coordination problem of ensuring GPL infrastructure is safe to use without triggering reciprocity obligations that would expose their proprietary code. For contributors, the constraint is snare—it restricts their ability to build proprietary products on work they created, trapping them in a binary choice. The engine computes this divergence from the power atoms, exit options, and beneficiary/victim declarations. The authored claim (tangled_rope) reflects the hybrid nature: genuine coordination function (commons stability) paired with asymmetric restriction (benefiting proprietary integrators, burdening commons creators). The metric/claim independence rule applies: the metrics are honest assessments of extractiveness and suppression; the claim reflects the structural truth that both coordination and extraction coexist in the same legal form.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors and closed-source integrators are structural beneficiaries (d → 0.1–0.2: they gain legal safety and competitive protection without reciprocal obligation). Commons contributors are structural targets (d → 0.8–0.9: they bear the binary-choice cost and the prohibition on proprietary derivatives). Commons governance bodies are agenda-setters (d → 0.5: they maintain the infrastructure and make enforcement decisions, but do not collect extraction in the proprietary-vendor sense). Derivative-work creators face a constrained exit (identity_locked by professional identity or community commitment), pushing their d higher (0.7–0.8). This asymmetry is the tangled_rope signature: a real coordination function (commons stability) married to asymmetric extraction (restriction on proprietary paths).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (enclosure of the commons) is contested in status (live vs. dead). If the problem is dead (permissive licensing has made proprietary enclosure irrelevant, or the threat was overstated), the reciprocity obligation persists as zombie infrastructure—a mandatrophy candidate. However, the constraint is actively maintained and defended (commons governance bodies continue licensing new projects under GPL, enforcement disputes persist, major projects like Linux remain GPL-enforced), so mandatrophy has not yet set in. The constraint would transition to piton/mandatrophy if: (1) the founding problem definitively dies (permissive licensing becomes universally dominant and proprietary enclosure becomes a non-threat), AND (2) enforcement activity becomes primarily theatrical (defending the constraint's legitimacy rather than enforcing material compliance). Currently, enforcement is material and ongoing, so mandatrophy is not present—though the measurement trajectory suggests it as a possible future state if permissive licensing continues to dominate and reciprocity obligations become primarily symbolic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_enclosure_persistence,
    'Is proprietary enclosure of open-source software a live threat, or has permissive licensing made it substantially irrelevant?',
    'Market data on open-source software distribution: (1) rate of permissive-license adoption vs. GPL adoption; (2) frequency of proprietary forks or proprietary wrappers around GPL software; (3) testimony from commons projects about actual enclosure attempts vs. hypothetical fears.',
    'If enclosure is live, the reciprocity obligation remains a necessary coordination mechanism with non-zero commons-preserving value. If enclosure is dead or minimal, the obligation is zombie infrastructure (mandatrophy candidate): it persists in form while its original function has faded. This is the founding_problem_status contested claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_enclosure_persistence, empirical, 'Whether the GPL''s founding problem (proprietary enclosure of commons) persists as a material threat.').

omega_variable(
    restriction_vs_freedom_frame_dependency,
    'Is the GPL''s reciprocity obligation fundamentally a restriction mechanism (this reading) or a freedom-preservation mechanism (sibling readings)? Or does the frame depend on the observer''s relationship to the constraint?',
    'Structural analysis: Does a given observer experience reciprocity as a freedom-enabling rule (freedom reading: I am free to modify and distribute) or a freedom-restricting rule (restriction reading: I am prohibited from proprietary integration)? A single observer''s situation might admit only one frame—the question is whether BOTH frames capture objective structural properties or whether frame selection depends on which stakeholder seat answers the question.',
    'If frame-dependent, the constraint has irreducible perceptual multiplicity: it is simultaneously a restriction and a freedom mechanism, and no single classification can be objectively true for all seats. If not frame-dependent, one reading is structurally correct and the others are misreadings. The engine''s per-seat classification computation assumes frame-independence; this omega documents the alternative possibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(restriction_vs_freedom_frame_dependency, conceptual, 'Whether the copyleft obligation is intrinsically a restriction mechanism or a freedom mechanism, or whether the frame depends on the observer''s structural position.').

omega_variable(
    identity_lock_mechanism_in_commons_contributors,
    'Why are commons contributors described as ''identity_locked'' in exit options? Is their lock structural (they cannot leave the commons without destroying their professional identity and community standing) or internalized (they believe they should not leave)?',
    'Post-exit trajectories: Track contributors who move from GPL-only to permissive-licensed projects or proprietary work. Do they experience structural barriers (community exclusion, loss of access, legal liability) or psychological/identity barriers (guilt, sense of betrayal, loss of meaning)? How many face each type?',
    'If structural, the constraint''s suppression is higher than the 0.52 scalar suggests—the target carries suppression mechanisms with them even after legal exit. If internalized, the constraint has deep psychological anchoring that outlasts legal obligation. Either way, the identity_lock justifies a higher directionality (d → 0.85+) for commons contributors than the legal prohibition alone would explain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_commons_contributors, empirical, 'Whether commons contributors'' identity-lock is structural or internalized, and its role in maintaining the constraint.').

omega_variable(
    patent_scope_ambiguity_in_reciprocity,
    'Does GPL''s reciprocity obligation apply to patents held in GPL-licensed software, or only to copyrightable code? If patents are included, does reciprocity constitute an implicit patent-licensing requirement—and does that exceed copyright law''s traditional scope?',
    'Legal analysis of GPL''s patent-grant language vs. copyright-only interpretation. Empirical cases where proprietary integrators claimed patent infringement or GPL projects claimed patent indemnification triggered reciprocity.',
    'If reciprocity extends to patents, the constraint''s scope is broader than copyright law alone would justify, and the extractiveness might be higher for proprietaries holding patents. If restricted to copyright, the reciprocity obligation is more defensible as a copyright-law consequence. This bears on the ''vindicated propositions'': patent-scope reciprocity would vindicate ''intellectual property as enforceable constraint''; copyright-only reciprocity would vindicate ''copyright control over derivative works.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_scope_ambiguity_in_reciprocity, empirical, 'Whether GPL reciprocity extends to patent licensing or is confined to copyright.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(gpl__tr_t0, observed).
narrative_ontology:measurement(gpl__tr_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 5, 0.21).
narrative_ontology:measurement_basis(gpl__tr_t5, observed).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement_basis(gpl__tr_t10, observed).
narrative_ontology:measurement(gpl__tr_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement_basis(gpl__tr_t15, observed).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement_basis(gpl__tr_t20, observed).
narrative_ontology:measurement(gpl__tr_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t25, observed).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(gpl__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(gpl__be_t0, observed).
narrative_ontology:measurement(gpl__be_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 5, 0.61).
narrative_ontology:measurement_basis(gpl__be_t5, observed).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(gpl__be_t10, observed).
narrative_ontology:measurement(gpl__be_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement_basis(gpl__be_t15, observed).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(gpl__be_t20, observed).
narrative_ontology:measurement(gpl__be_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(gpl__be_t25, observed).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(gpl__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(gpl__su_t0, observed).
narrative_ontology:measurement(gpl__su_t5, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement_basis(gpl__su_t5, observed).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(gpl__su_t10, observed).
narrative_ontology:measurement(gpl__su_t15, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 15, 0.51).
narrative_ontology:measurement_basis(gpl__su_t15, observed).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(gpl__su_t20, observed).
narrative_ontology:measurement(gpl__su_t25, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(gpl__su_t25, observed).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(gpl__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation is a single kernel (fixed text: GPL v2/v3 license) with three structurally distinct readings. Each reading instantiates a different constraint with different epsilon values and beneficiary/victim structures. This constraint (copyleft_as_restriction_reading) emphasizes the business-model prohibition and restriction aspects, making visible the extractive force on proprietary integrators and commons contributors. The sibling readings emphasize commons preservation and user freedom, respectively. All three readings share the same legal text (fixed_text kernel_codification) but interpret it differently, instantiating different constraint types (this one: tangled_rope; commons reading: rope; freedom reading: rope or mountain depending on seat). The readings are not ordered—they coexist simultaneously as live positions held by different factions of the open-source and proprietary-software communities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
