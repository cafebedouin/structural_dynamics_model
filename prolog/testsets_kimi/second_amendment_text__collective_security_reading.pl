% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment Collective Security Reading
 *   domain: constitutional/law/political
 *
 * SUMMARY:
 *   This constraint instantiates the collective security reading of the
 *   Second Amendment text, under which the prefatory militia clause
 *   conditions the operative right on organized civic defense, permitting the
 *   state to regulateâlicense, register, prohibitâindividual arms
 *   possession in service of collective security. It was the dominant
 *   twentieth-century doctrinal framework until challenged by the individual
 *   right reading. The state regulatory apparatus and law enforcement are
 *   structural beneficiaries of the interpretive space it opens; individual
 *   gun owners are the constrained class that bears its compliance and
 *   liberty costs.
 *
 * KEY AGENTS:
 *   - State regulatory apparatus: agenda-setter and beneficiary (institutional/analytical) â derives and wields regulatory authority over arms.
 *   - Individual gun owners: payer (moderate/constrained) â bear licensing burdens and possession restrictions.
 *   - Law enforcement agencies: beneficiary (institutional/constrained) â operate in a legal presumption favoring regulatory control.
 *   - Federal judiciary: observer (institutional/analytical) â adjudicates between competing readings and acknowledges doctrinal drift.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.6).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.72).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment Collective Security Reading").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional/law/political").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'd3573d4c-44ef-4a32-b5c3-f5cd7ad55c82').
narrative_ontology:cs_kernel_codification('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', fixed_text).
narrative_ontology:cs_authority_grounding('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', lineage).
narrative_ontology:cs_interpretation_layer_present('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82').
narrative_ontology:cs_reading_relation('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', second_amendment_text__originalist_civic_virtue_reading, coexists_with).
narrative_ontology:cs_axiom('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', foundational, right_conditioned_on_militia_service).
narrative_ontology:cs_axiom_status(right_conditioned_on_militia_service, holdable).
narrative_ontology:cs_axiom_grounding('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', right_conditioned_on_militia_service, conventional).
narrative_ontology:cs_axiom('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', foundational, state_regulatory_authority_over_arms_permissible).
narrative_ontology:cs_axiom_status(state_regulatory_authority_over_arms_permissible, holdable).
narrative_ontology:cs_axiom_grounding('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', state_regulatory_authority_over_arms_permissible, conventional).
narrative_ontology:cs_reference_frame('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', organized_civic_defense_framework).
narrative_ontology:cs_drift_state('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', post_heller_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('d3573d4c-44ef-4a32-b5c3-f5cd7ad55c82', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, law_enforcement_agencies).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Derives constitutional authority from the militia clause to construct, administer, and enforce licensing, registration, and prohibition regimes over small arms. Sets the regulatory agenda under the banner of organized civic defense and collective security, with authority to condition or deny individual possession.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, state_regulatory_apparatus, beneficiary).

% Bear the compliance burden and liberty costs of state regulatory regimes justified by this reading. Face permitting fees, waiting periods, categorical prohibitions, and geographic restrictions on possession. Their constitutional claim to an unconditioned right is structurally ruled out by the collective security interpretive framework.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% Operate within a legal environment that treats broad citizen disarmament or licensing as constitutionally permissible. Benefit from reduced legal friction when enforcing weapons prohibitions and from the presumption that regulatory possession limits serve collective security.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, national).

% Adjudicates firearms cases under competing constitutional readings. Historically sustained the collective security framework in twentieth-century precedent, but has more recently acknowledged doctrinal drift toward the individual right reading. Retains institutional capacity to restore or further erode this constraint.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a constitutional architecture for organized civic defense by linking arms possession to state-regulated militia service, allowing legislatures to differentiate between civilian and military-complicit possession rather than facing an unqualified individual entitlement.
% TRANSFER_FUNCTION: Moves regulatory authority over arms from the individual possessor to the state legislature and its delegated agencies; moves compliance costs, licensing burdens, and possession restrictions from the state to individuals who wish to keep or bear arms.
% ABSENT_VOICES: Gun rights advocacy organizations and individual self-defense proponents are present in public discourse but structurally excluded from constitutional vindication under this reading; their preferred unconditioned right framework is treated as textually inadmissible.
% DISAPPEARANCE_RATIONALE: If the militia-conditioning and collective security justification vanished overnight, the constitutional footing for extensive federal and state firearms regulation would collapse. Licensing, registration, and prohibition regimes would need to be rebuilt on commerce clause or general police power grounds, rearranging the balance between state authority and individual possession.
% FOUNDING_PROBLEM: The founding generation required state governments to maintain armed, organized militia forces for collective defense without federal interference, and sought to prevent the federal government from disarming the state-organized populace that supplied these forces.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians attest to the founding-era concern with militia-based defense and federalism, but independent constitutional scholars and gun rights historians contest that this was the exclusive purpose, arguing individual self-defense and anti-tyranny purposes were equally central. No consensus from non-beneficiary seats exists.
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60) is substantial because the reading authorizes broad state discretion to burden, condition, and deny individual possession. Suppression (0.72) is high because the constraint persists only through active judicial and legislative enforcement that excludes the competing individual right framework. Theater ratio (0.46) has risen as the reading has lost doctrinal supremacy post-Heller; maintaining it increasingly requires performative historical argumentation that diverges from contemporary constitutional practice. Accessibility collapse (0.42) is moderate: the individual right alternative is now legally accessible after Heller, though it remains partly collapsed in jurisdictions still adhering to the collective framework. Resistance (0.80) is very high due to sustained ideological and legal opposition from the gun rights movement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (state regulatory apparatus) experiences the constraint as a legitimate constitutional grant of police power for collective security. The payer seat (individual gun owners) experiences the same text as an extractive mechanism that conditions a constitutional right on state approval. The engine computes this divergence from the structural data: same constraint, opposite directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (state regulatory apparatus, law enforcement) sit near the full-beneficiary end: the constraint subsidizes their authority and operational discretion. Victims (individual gun owners) sit near the full-target end: the constraint extracts liberty and compliance costs from them. The federal judiciary, as observer, sits at analytical exit with neutral directionality. No override is needed because the structural derivation (beneficiary/victim plus exit options) captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mandatrophy mislabeling by requiring both a genuine coordination function (organized civic defense) and identifiable victims (regulated gun owners). A pure coordination account would miss the asymmetric extraction; a pure snare account would miss the real collective security problem the militia framework was built to address. The tangled rope classification captures the hybrid: the state coordinates security through the same structure that extracts compliance from individuals.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_operative_status,
    'Is the prefatory militia clause a legally operative condition on the right, or merely a non-binding explanatory justification?',
    'Linguistic and legal-historical analysis of eighteenth-century statutory drafting conventions; comparison with other prefatory clauses in the Bill of Rights that have been treated as non-operative.',
    'If the clause is non-operative, the collective security reading collapses into a different constraint (a false summit or snare of policy), and the individual right reading gains textual ground. If operative, the regulatory authority is textually grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_clause_operative_status, conceptual, 'Whether the militia clause is a binding textual condition or a rhetorical preamble.').

omega_variable(
    regulatory_efficacy_for_collective_security,
    'Do firearms licensing and prohibition regimes justified by collective security actually reduce violence or enhance civic defense, or do they extract compliance without measurable security returns?',
    'Empirical criminological studies comparing jurisdictions with strong regulatory regimes against those with permissive regimes, controlling for confounding socioeconomic variables; militia readiness metrics.',
    'If regulatory regimes show no efficacy, the coordination story is cover and the constraint should recompute toward snare. If efficacy is demonstrated, the coordination function is genuine and the tangled rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_efficacy_for_collective_security, empirical, 'Whether the claimed collective security coordination produces empirical benefits or functions as cover for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 0, 85).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__collective_security_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(seco_tr_t17, second_amendment_text__collective_security_reading, theater_ratio, 17, 0.2).
narrative_ontology:measurement(seco_tr_t34, second_amendment_text__collective_security_reading, theater_ratio, 34, 0.25).
narrative_ontology:measurement(seco_tr_t51, second_amendment_text__collective_security_reading, theater_ratio, 51, 0.3).
narrative_ontology:measurement(seco_tr_t68, second_amendment_text__collective_security_reading, theater_ratio, 68, 0.38).
narrative_ontology:measurement(seco_tr_t85, second_amendment_text__collective_security_reading, theater_ratio, 85, 0.46).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__collective_security_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(seco_be_t17, second_amendment_text__collective_security_reading, base_extractiveness, 17, 0.48).
narrative_ontology:measurement(seco_be_t34, second_amendment_text__collective_security_reading, base_extractiveness, 34, 0.55).
narrative_ontology:measurement(seco_be_t51, second_amendment_text__collective_security_reading, base_extractiveness, 51, 0.62).
narrative_ontology:measurement(seco_be_t68, second_amendment_text__collective_security_reading, base_extractiveness, 68, 0.6).
narrative_ontology:measurement(seco_be_t85, second_amendment_text__collective_security_reading, base_extractiveness, 85, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_text__collective_security_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(seco_su_t17, second_amendment_text__collective_security_reading, suppression_requirement, 17, 0.4).
narrative_ontology:measurement(seco_su_t34, second_amendment_text__collective_security_reading, suppression_requirement, 34, 0.5).
narrative_ontology:measurement(seco_su_t51, second_amendment_text__collective_security_reading, suppression_requirement, 51, 0.6).
narrative_ontology:measurement(seco_su_t68, second_amendment_text__collective_security_reading, suppression_requirement, 68, 0.7).
narrative_ontology:measurement(seco_su_t85, second_amendment_text__collective_security_reading, suppression_requirement, 85, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment text decomposes into at least three structurally distinct readings. The collective security reading treats the operative right as conditioned by the prefatory militia clause, permitting broad state regulation. The individual right reading treats the operative clause as guaranteeing an unconditioned individual right. The originalist civic virtue reading treats the militia as universal armed citizenry. These are not observational variants of one constraint; they have different beneficiary/victim structures, different directionality profiles, and different epsilon values. This story instantiates the collective security reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
