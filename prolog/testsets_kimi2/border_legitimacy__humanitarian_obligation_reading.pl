% ============================================================================
% CONSTRAINT STORY: border_legitimacy__humanitarian_obligation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__humanitarian_obligation_reading, []).

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
 *   constraint_id: border_legitimacy__humanitarian_obligation_reading
 *   human_readable: Humanitarian Obligation with Economic Migrant Exclusion
 *   domain: political/international_law
 *
 * SUMMARY:
 *   The post-World War II international refugee regime establishes that
 *   states have obligations to admit and not return persons fleeing
 *   persecution or disaster, while maintaining the categorical legitimacy of
 *   excluding those deemed 'mere' economic migrants. This constraint story
 *   models the humanitarian_obligation_reading of the border_legitimacy
 *   kernel: a bifurcated framework that coordinates protection for a defined
 *   class while extracting exclusion costs from another. The classification
 *   is claimed as tangled_rope because the arrangement simultaneously solves
 *   a genuine coordination problem (who protects the persecuted?) and imposes
 *   asymmetric extraction (categorical exclusion of economic migrants
 *   enforced by border regimes).
 *
 * KEY AGENTS:
 *   - refugee_asylum_seekers: Protected class under the regime (powerless/trapped) â gain admission if categorized correctly
 *   - economic_migrants: Excluded class (powerless/trapped) â bear the costs of categorical closure
 *   - destination_states: Agenda-setters (institutional/arbitrage) â administer the distinction and enforcement
 *   - international_humanitarian_orgs: Analytical observers (institutional/analytical) â monitor and service the regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, 0.55).
domain_priors:suppression_score(border_legitimacy__humanitarian_obligation_reading, 0.72).
domain_priors:theater_ratio(border_legitimacy__humanitarian_obligation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__humanitarian_obligation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__humanitarian_obligation_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__humanitarian_obligation_reading, "Humanitarian Obligation with Economic Migrant Exclusion").
narrative_ontology:topic_domain(border_legitimacy__humanitarian_obligation_reading, "political/international_law").

domain_priors:requires_active_enforcement(border_legitimacy__humanitarian_obligation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__humanitarian_obligation_reading, 'e98fb550-73d5-4f25-9379-55703aa28815').
narrative_ontology:cs_kernel_codification('e98fb550-73d5-4f25-9379-55703aa28815', formalized).
narrative_ontology:cs_authority_grounding('e98fb550-73d5-4f25-9379-55703aa28815', lineage).
narrative_ontology:cs_interpretation_layer_present('e98fb550-73d5-4f25-9379-55703aa28815').
narrative_ontology:cs_reading_relation('e98fb550-73d5-4f25-9379-55703aa28815', border_legitimacy__sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('e98fb550-73d5-4f25-9379-55703aa28815', border_legitimacy__freedom_of_movement_reading, coexists_with).
narrative_ontology:cs_axiom('e98fb550-73d5-4f25-9379-55703aa28815', foundational, state_obligation_to_persecuted_fleeing_persons).
narrative_ontology:cs_axiom_status(state_obligation_to_persecuted_fleeing_persons, holdable).
narrative_ontology:cs_axiom_grounding('e98fb550-73d5-4f25-9379-55703aa28815', state_obligation_to_persecuted_fleeing_persons, conventional).
narrative_ontology:cs_axiom('e98fb550-73d5-4f25-9379-55703aa28815', foundational, categorical_exclusion_of_economic_migrants).
narrative_ontology:cs_axiom_status(categorical_exclusion_of_economic_migrants, holdable).
narrative_ontology:cs_axiom_grounding('e98fb550-73d5-4f25-9379-55703aa28815', categorical_exclusion_of_economic_migrants, conventional).
narrative_ontology:cs_reference_frame('e98fb550-73d5-4f25-9379-55703aa28815', postwar_humanitarian_sovereignty_bargain).
narrative_ontology:cs_drift_state('e98fb550-73d5-4f25-9379-55703aa28815', contemporary_securitized_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e98fb550-73d5-4f25-9379-55703aa28815', '').
narrative_ontology:cs_kernel_id(border_legitimacy__humanitarian_obligation_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__humanitarian_obligation_reading, refugee_asylum_seekers).
narrative_ontology:constraint_victim(border_legitimacy__humanitarian_obligation_reading, economic_migrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals fleeing persecution, armed conflict, or disaster who gain a legal claim to admission and non-refoulement under international refugee law provided they fit the categorical definition; their protection depends on being classified within the privileged group rather than the excluded one.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, refugee_asylum_seekers, beneficiary,
    powerless, immediate, trapped, global).

% Individuals seeking to improve economic conditions or escape poverty who are categorically excluded from admission by the humanitarian obligation framework; they bear the costs of border enforcement, detention, deportation, and legal invisibility because they do not qualify as persecuted or disaster-displaced.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, economic_migrants, payer,
    powerless, biographical, trapped, global).

% Sovereign states that administer the categorical distinction between refugees and economic migrants, granting legal status and protection to the former while enforcing exclusion, detention, and removal against the latter through border regimes and asylum adjudication systems.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, destination_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations such as UNHCR and affiliated NGOs that monitor state compliance with refugee protection norms, provide assistance to asylum seekers, and advocate for the humanitarian framework; they observe and service the regime without being its primary targets or extractors.
narrative_ontology:constraint_stakeholder(border_legitimacy__humanitarian_obligation_reading, international_humanitarian_orgs, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of who will protect and admit persons fleeing persecution, war, or disaster when their home state cannot or will not protect them, by establishing a shared international framework for legal status, burden-sharing, and non-refoulement.
% TRANSFER_FUNCTION: Transfers legal protection and admission obligation from destination states and the international community to recognized refugees; simultaneously transfers exclusion and enforcement costs onto economic migrants who fall outside the protected category.
% ABSENT_VOICES: Economic migrants are structurally absent from refugee-policy design; their interests are represented only indirectly by labor-sending states and migrant-rights advocates, who are routinely excluded from fora where the refugee/economic-migrant distinction is treated as natural and settled.
% DISAPPEARANCE_RATIONALE: If the humanitarian obligation and its accompanying exclusion vanished overnight, refugee protection frameworks would collapse and millions would lose legal status and safe haven; simultaneously, border regimes would lose their humanitarian legitimization and labor migration governance would require entirely different architectures. The world rearranges because current migration governance is organized around this categorical distinction.
% FOUNDING_PROBLEM: Post-World War II displacement crises and the pre-war failure of states to protect refugees, leading to a coordinated international commitment to provide legal protection and non-refoulement for those fleeing persecution.
% FOUNDING_PROBLEM_CORROBORATION: International legal historians and humanitarian organizations attest to the founding problem. Critical migration scholars and post-colonial analysts from outside the primary beneficiary set argue the regime was also designed to manage labor mobility and racialized border control from its inception, making the pure protection genealogy contested.
narrative_ontology:disappearance_verdict(border_legitimacy__humanitarian_obligation_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__humanitarian_obligation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__humanitarian_obligation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_legitimacy__humanitarian_obligation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__humanitarian_obligation_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__humanitarian_obligation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__humanitarian_obligation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__humanitarian_obligation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the categorical exclusion is not total economic extraction but a denial of legal mobility and protection; suppression is higher (0.72) because the distinction requires active border enforcement, detention, and pushback to maintain. Theater ratio rises over the interval (0.55 at endpoint) as securitization discourse increasingly justifies exclusion in humanitarian language (theater of control). Accessibility collapse is moderate (0.62): alternatives like open labor migration or regional free movement are structurally suppressed but not fully eliminated. Resistance is moderate (0.55): migrant movements and some legal advocates actively contest the exclusion. The measurement series run on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The refugee seat experiences the constraint as protective coordination (low d, low effective extraction), while the economic migrant seat experiences it as enforced exclusion (high d, high effective extraction). The destination state seat sits near the beneficiary end institutionally but bears compliance costs; the engine computes this divergence from structural data. The humanitarian organization seat sees the coordination function clearly but may overlook the extraction embedded in the same structure.
 *
 * DIRECTIONALITY LOGIC:
 *   refugee_asylum_seekers are declared beneficiaries (d near 0.0) because the constraint's primary coordination function flows to them: legal protection and admission. economic_migrants are declared victims with role payer (d near 1.0) because the same categorical structure enforces their exclusion. destination_states have arbitrage-grade exit and are not declared victims, so derivation places them near the beneficiary end (low d) despite their enforcement role. No override is needed because the structural derivation matches the intuitive relationship: states are not the primary targets of this constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate was built to solve post-war displacement and prevent refugee crises like the 1930s. The founding problem (protecting persecuted displaced persons) is contested in status: it is partly live (new displacements occur) but partly dead (the regime now serves as legitimizing cover for broader migration control). The mismatch between contested founding status and world_rearranges disappearance verdict flags that the arrangement persists beyond its pure protective function. Mandatrophy is not fully resolved because the coordination function remains partially active, preventing pure piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint instantiates the humanitarian_obligation_reading of the border_legitimacy kernel; how would classification shift if the sovereignty_reading or freedom_of_movement_reading were adopted as the operative framework?',
    'Comparative analysis across the constraint family; each reading reallocates beneficiary and victim sets and redefines the coordination function.',
    'Adopting freedom_of_movement would eliminate the economic_migrant victim set; adopting sovereignty would eliminate the refugee_asylum_seeker beneficiary set and collapse the coordination function into pure state discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Framing ambiguity from kernel sibling readings').

omega_variable(
    persecution_economic_boundary,
    'Is the distinction between persecution or disaster flight and economic migration structurally stable, or does it collapse under empirical inspection of mixed-motive displacement?',
    'Empirical study of migration motives and jurisprudential analysis of asylum adjudication outcomes across jurisdictions.',
    'If the boundary is unstable, the bifurcated victim set is a constructed sorting mechanism rather than a natural classification, raising extractiveness and shifting the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persecution_economic_boundary, empirical, 'Stability of the refugee economic migrant categorical boundary').

omega_variable(
    state_compliance_motivation,
    'Do states comply with refugee admission obligations primarily from normative commitment, or from strategic interest in legitimizing broader exclusionary border control?',
    'Comparative analysis of state compliance with refugee conventions against domestic political economy of labor migration and border externalization practices.',
    'If strategic, the coordination function is cover for extraction, shifting classification toward snare; if normative, the coordination function is genuine and tangled_rope remains accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_compliance_motivation, conceptual, 'Motivation behind state adherence to humanitarian obligation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__humanitarian_obligation_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t0, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t10, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t20, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t30, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t40, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t50, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t60, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 60, 0.52).
narrative_ontology:measurement(border_legitimacy_humanitarian_tr_t70, border_legitimacy__humanitarian_obligation_reading, theater_ratio, 70, 0.55).

% Extraction over time
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t0, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t10, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t20, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t30, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t40, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t50, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t60, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement(border_legitimacy_humanitarian_be_t70, border_legitimacy__humanitarian_obligation_reading, base_extractiveness, 70, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t0, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t10, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t20, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t30, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t40, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t50, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 50, 0.65).
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t60, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement(border_legitimacy_humanitarian_su_t70, border_legitimacy__humanitarian_obligation_reading, suppression_requirement, 70, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
