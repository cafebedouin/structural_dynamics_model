% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License without Copyleft Reciprocity
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint models the copyleft counterfactual reading of permissive
 *   license text (MIT/BSD/Apache-2.0 family). The reading asserts that
 *   copyright relaxation without a reciprocity requirement structurally
 *   enables proprietary capture of commons labor: corporations extract value
 *   from permissively-licensed code by incorporating it into closed products
 *   without contributing improvements back. The GPL's viral reciprocity
 *   clause is framed as the necessary alternative that closes this extraction
 *   channel. The constraint is NOT the license text itself (which is a
 *   coordination mechanism for distribution), but the *absence of copyleft
 *   reciprocity* as an structural feature that enables exploitation — a
 *   tangled rope where coordination (easy reuse) coexists with asymmetric
 *   extraction (proprietary capture).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.72).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License without Copyleft Reciprocity").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'e9fb9dee-89b0-47ed-88d0-75e7ac9e9020').
narrative_ontology:cs_kernel_codification('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', fixed_text).
narrative_ontology:cs_authority_grounding('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', lineage).
narrative_ontology:cs_interpretation_layer_present('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020').
narrative_ontology:cs_reading_relation('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', permissive_license_text__corporate_moat_reading, influences).
narrative_ontology:cs_axiom('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', foundational, reciprocity_necessary_for_commons_sustainability).
narrative_ontology:cs_axiom_status(reciprocity_necessary_for_commons_sustainability, holdable).
narrative_ontology:cs_axiom_grounding('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', reciprocity_necessary_for_commons_sustainability, deontological).
narrative_ontology:cs_axiom('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', foundational, copyright_relaxation_without_copyleft_enables_extraction).
narrative_ontology:cs_axiom_status(copyright_relaxation_without_copyleft_enables_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', copyright_relaxation_without_copyleft_enables_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', original_permissive_license_intent).
narrative_ontology:cs_drift_state('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', contemporary_commercial_foss_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e9fb9dee-89b0-47ed-88d0-75e7ac9e9020', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, commons_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, startup_ecosystem_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, corporate_legal_departments).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, commons_maintainers).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, reciprocity_necessary_for_commons_sustainability).
narrative_ontology:constraint_vindicates(permissive_license_text__copyleft_counterfactual_reading, copyright_relaxation_without_copyleft_enables_extraction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and maintain reciprocal licenses (GPL family). They benefit structurally from the permissive license extraction dynamic because it validates their argument that reciprocity is necessary. They can arbitrage by choosing GPL for their own projects and migrating communities toward reciprocal licensing. Their exit is easy — they already operate outside the permissive license regime.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, beneficiary,
    organized, generational, arbitrage, global).

% Develop and maintain the permissively-licensed code that proprietary builders extract. They benefit from wide adoption and occasional corporate sponsorship, but pay through uncompensated labor capture. Their exit is constrained: switching to GPL reduces adoption and invites ecosystem retaliation; staying permissive enables extraction. Many report feeling trapped by the adoption/reciprocity tradeoff.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, commons_maintainers, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, commons_maintainers, payer).

% Incorporate permissively-licensed code into closed commercial products without contributing improvements upstream. They are classified as 'payers' in this reading because the constraint's persistence depends on their extraction — they bear the cost of potential copyleft enforcement, compliance programs, and reputational risk. Their exit is mobile: they could adopt GPL compliance or rewrite dependencies, but the cost is high and the current arrangement is profitable.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_builders, payer,
    powerful, biographical, mobile, global).

% Early-stage companies building on permissive commons. They face asymmetric competition: well-capitalized incumbents free-ride on the same commons without reciprocating, while startups lack resources for clean-room reimplementation. Their exit is constrained by investor pressure to use permissive licenses and by the network effects of the existing ecosystem. They pay through competitive disadvantage and potential future extraction when acquired.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, startup_ecosystem_participants, payer,
    moderate, biographical, constrained, global).

% FSF and OSI steward the license definitions and compliance frameworks. They set the agenda by defining what counts as 'open source' and 'free software', maintaining the license categories, and running compliance education. They do not directly extract but their institutional authority shapes which constraints are viable. Their exit is analytical — they observe the full structure but their institutional role commits them to the current categorization system.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, license_steards_fsf_osi, agenda_setter,
    institutional, generational, analytical, global).

% Draft and enforce corporate open source policies that mandate permissive licensing for outbound code and allow proprietary use of inbound permissive code. They administer the extraction dynamic at the organizational level. They benefit from the current arrangement (low compliance cost, high extraction value) but could arbitrage by switching to GPL compliance if the legal risk calculus changes. Their power is institutional but their exit is arbitrage-grade.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, corporate_legal_departments, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__copyleft_counterfactual_reading, corporate_legal_departments, beneficiary).

% Study the long-term dynamics of software freedom, commons sustainability, and license ecology. They have no stake in the extraction but analyze its structural properties. Their exit is analytical — they can choose any framing. This reading instantiates their analytical frame.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, academic_researchers_software_freedom, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing solves the coordination problem of frictionless software reuse: developers can incorporate code without legal review, companies can build products without compliance overhead, and the commons grows through low-barrier contribution. This is a genuine coordination function — the license text itself is a rope.
% TRANSFER_FUNCTION: The constraint (absence of reciprocity requirement) transfers the value of commons labor from maintainers to proprietary builders. Maintainers produce code under permissive terms; proprietary builders capture the commercial value of that code in closed products without upstream contribution. The transfer is uncompensated and structurally enforced by the license terms.
% ABSENT_VOICES: End-users of proprietary products built on permissive code — they would benefit from upstream improvements if reciprocity were required, but are excluded from the licensing conversation. Developers in jurisdictions with weak IP enforcement — they experience the extraction differently but are not represented in license governance. The global south software communities who bear the maintenance burden without the commercial capture benefits.
% DISAPPEARANCE_RATIONALE: If the permissive license regime vanished overnight (replaced by mandatory reciprocity), the software economy would reorganize: proprietary builders would face GPL compliance or rewrite costs, commons maintainers would gain leverage for funding/support, and the license stewardship institutions would lose their current categorization authority. The world rearranges because the extraction dynamic is structural, not incidental.
% FOUNDING_PROBLEM: In the late 1980s/early 1990s, proprietary software dominated and there was no viable path for collaborative development. The founding problem was: how to enable software sharing and collaborative development without the legal friction of negotiating permissions for every use? Permissive licenses (MIT 1988, BSD 1988, Apache 1.0 1995) solved this by minimizing the legal surface area.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (legal friction blocking collaboration) is attested as substantially solved by: Eric S. Raymond's 'The Cathedral and the Bazaar' (1999) documenting the coordination success; GitHub's 2020 Octoverse report showing 56M+ developers on a permissive-license-friendly platform. It is attested as still live by: FSF's ongoing argument that friction was never the core problem (freedom was); academic work on 'license compliance as friction' (Vendome et al. 2017) showing permissive licenses still reduce coordination cost. No single voice outside the benefiting parties (corporate legal, permissive-license advocates) corroborates the 'solved' status without qualification.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the scale of uncompensated value transfer from commons maintainers to proprietary builders — documented in corporate FOSS audits showing 70-90% of commercial products embed permissive code without upstream contribution. Suppression (0.72) captures the legal and economic pressure that makes copyleft adoption difficult: patent retaliation risk, investor preference for permissive licensing, and ecosystem network effects that penalize reciprocal licenses. Theater ratio (0.22) is low because the coordination function (frictionless reuse) is genuine but the extraction is structural, not performative. The rising trend in all three metrics over the interval tracks the commercialization of open source from hobbyist to infrastructure layer.
 *
 * PERSPECTIVAL GAP:
 *   From the proprietary builder seat, permissive licensing appears as pure coordination (rope) — they experience the benefits of free reuse without feeling the extraction because they ARE the extractors. From the commons maintainer seat, the same license text operates as a snare — their labor is captured without consent or compensation. The engine computes this divergence from the structural data: beneficiaries have arbitrage-grade exit (can switch to GPL), victims are constrained by ecosystem lock-in and investor pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyleft advocates and commons maintainers are beneficiaries (d ~ 0.15) — they gain a structural argument for reciprocal licensing and protection from proprietary capture. Proprietary builders are primary victims (d ~ 0.85) — the constraint's persistence depends on their ability to extract without reciprocity; they bear the cost of compliance when copyleft is enforced and the reputational cost when exposed. Startup ecosystem participants are secondary victims (d ~ 0.75) — they face a tilted playing field where well-capitalized incumbents free-ride on commons labor. The analytical observer (this reading) sees the full structure: a coordination mechanism (permissive licensing) that has been captured by an extraction dynamic it does not internally constrain.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling software sharing without legal friction) is live but the arrangement has drifted: permissive licensing solved the coordination problem of the 1990s but now enables an extraction dynamic that did not exist at scale when the licenses were written. The mandate has not atrophied — it has been hijacked. Mandatrophy is unresolved because the coordination function is still real and valued, but the extraction overlay has grown to dominate the constraint's operation. This is a tangled rope, not a piton, because the coordination function remains active and the extraction requires active legal/economic enforcement to sustain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'Does the copyleft_counterfactual_reading instantiate a genuinely distinct constraint from the corporate_moat_reading, or are they the same extraction dynamic viewed from different normative angles?',
    'Compare beneficiary/victim structures: copyleft_counterfactual centers commons_maintainers as beneficiaries and proposes GPL as remedy; corporate_moat_reading centers proprietary_builders as extractors with no prescribed alternative. If beneficiary sets differ, they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own ε and classification. If same, they collapse to one constraint story with multiple framings — the ε-invariance principle would be violated by maintaining both.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Whether copyleft_counterfactual and corporate_moat are ε-distinct constraints or framings of one constraint').

omega_variable(
    reciprocity_enforcement_feasibility,
    'Is viral reciprocity (GPL enforcement) practically enforceable at scale, or does enforcement cost exceed the extraction it prevents?',
    'Empirical analysis of GPL compliance rates, litigation outcomes, and corporate compliance programs vs. measured extraction from permissive-licensed code in proprietary products.',
    'If enforcement is infeasible, the proposed alternative constraint (GPL) may be a piton or snare itself — the counterfactual remedy fails. If feasible, the tangled rope classification holds with a viable exit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_enforcement_feasibility, empirical, 'Whether the copyleft alternative is practically enforceable or a theoretical remedy').

omega_variable(
    commons_maintainer_agency,
    'Do commons maintainers voluntarily choose permissive licensing knowing the extraction risk, or are they structurally coerced by ecosystem pressures?',
    'Survey data on maintainer licensing choices, funding dependencies, and stated preferences vs. actual license selection under investor/employer pressure.',
    'If voluntary, the extraction has a consent component that reduces its structural severity. If coerced, the constraint is a snare with trapped victims. Current evidence suggests mixed agency — affects χ computation for commons_maintainers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_maintainer_agency, empirical, 'Whether commons maintainers have meaningful exit from the extraction dynamic').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 1989, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t1989, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 1989, 0.08).
narrative_ontology:measurement(perm_tr_t1998, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 1998, 0.12).
narrative_ontology:measurement(perm_tr_t2005, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(perm_tr_t2010, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(perm_tr_t2015, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(perm_tr_t2020, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(perm_tr_t2026, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(perm_be_t1989, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 1989, 0.35).
narrative_ontology:measurement(perm_be_t1998, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 1998, 0.45).
narrative_ontology:measurement(perm_be_t2005, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2005, 0.52).
narrative_ontology:measurement(perm_be_t2010, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(perm_be_t2015, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(perm_be_t2020, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(perm_be_t2026, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t1989, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 1989, 0.4).
narrative_ontology:measurement(perm_su_t1998, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 1998, 0.5).
narrative_ontology:measurement(perm_su_t2005, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(perm_su_t2010, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2010, 0.62).
narrative_ontology:measurement(perm_su_t2015, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2015, 0.66).
narrative_ontology:measurement(perm_su_t2020, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(perm_su_t2026, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 2026, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(permissive_license_text__copyleft_counterfactual_reading, 0.12).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, gpl_viral_reciprocity).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, corporate_foss_audit_requirements).

% DUAL FORMULATION NOTE:
% Decomposed from 'permissive_license_text' kernel per ε-invariance: commons_coordination_reading (ε≈0.15, rope) coordinates frictionless reuse; copyleft_counterfactual_reading (ε≈0.68, tangled_rope) identifies extraction enabled by missing reciprocity; corporate_moat_reading (ε≈0.75, snare) describes the extraction mechanism from the extractor's structural position. Three distinct ε values over the same license text prove they are different constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, organized, 0.15).
constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, powerful, 0.85).
constraint_indexing:directionality_override(permissive_license_text__copyleft_counterfactual_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
