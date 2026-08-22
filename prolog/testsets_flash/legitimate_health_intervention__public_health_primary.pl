% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary Intervention (Population Morbidity/Mortality Reduction)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of the
 *   'legitimate_health_intervention' kernel. It asserts that the legitimacy
 *   of public health interventions derives from their measurable reduction in
 *   population-level morbidity and mortality. Individual refusal of such
 *   interventions is framed as the imposition of an externality on the
 *   collective. This reading justifies strong enforcement mechanisms, such as
 *   mandatory vaccination, employment termination for non-compliance, and
 *   restrictions on access to public spaces, to protect the vulnerable and
 *   maintain public health infrastructure. The high extractiveness and
 *   suppression reflect the costs imposed on individuals who refuse to comply
 *   and the active enforcement required to maintain the population-level
 *   benefit.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda setter (institutional/global) — enforces compliance, sets policy.
 *   - immunocompromised_individuals: Primary beneficiary (powerless/biographical) — protected by population immunity.
 *   - general_population: Beneficiary (organized/generational) — benefits from reduced disease burden.
 *   - unvaccinated_individuals: Primary payer/victim (powerless/immediate) — bears costs of non-compliance (employment loss, access restrictions).
 *   - individuals_with_religious_exemptions: Payer/victim (powerless/biographical) — bears costs due to refusal based on belief.
 *   - constitutional_lawyers: Observer (analytical/generational) — challenge the legality and ethical basis of interventions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.85).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary Intervention (Population Morbidity/Mortality Reduction)").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '31cea546-07c7-40a5-b7a4-c184bc7d4940').
narrative_ontology:cs_kernel_codification('31cea546-07c7-40a5-b7a4-c184bc7d4940', formalized).
narrative_ontology:cs_authority_grounding('31cea546-07c7-40a5-b7a4-c184bc7d4940', expertise).
narrative_ontology:cs_interpretation_layer_present('31cea546-07c7-40a5-b7a4-c184bc7d4940').
narrative_ontology:cs_reading_relation('31cea546-07c7-40a5-b7a4-c184bc7d4940', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('31cea546-07c7-40a5-b7a4-c184bc7d4940', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('31cea546-07c7-40a5-b7a4-c184bc7d4940', foundational, population_health_supremacy).
narrative_ontology:cs_axiom_status(population_health_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('31cea546-07c7-40a5-b7a4-c184bc7d4940', population_health_supremacy, deontological).
narrative_ontology:cs_axiom('31cea546-07c7-40a5-b7a4-c184bc7d4940', secondary, individual_refusal_as_externality).
narrative_ontology:cs_axiom_status(individual_refusal_as_externality, holdable).
narrative_ontology:cs_axiom_grounding('31cea546-07c7-40a5-b7a4-c184bc7d4940', individual_refusal_as_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('31cea546-07c7-40a5-b7a4-c184bc7d4940', collective_health_imperative).
narrative_ontology:cs_drift_state('31cea546-07c7-40a5-b7a4-c184bc7d4940', contemporary_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('31cea546-07c7-40a5-b7a4-c184bc7d4940', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, general_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, individuals_with_religious_exemptions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for setting and enforcing public health policies, including mandatory interventions. They justify policies based on epidemiological data and population health outcomes. Their mandate is to reduce morbidity and mortality, and they benefit from compliance.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Highly vulnerable to infectious diseases, they rely on population-level immunity (herd immunity) for protection. They are direct beneficiaries of widespread compliance with public health interventions, as it reduces their risk of exposure and severe illness.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Benefits from reduced overall disease burden, stable healthcare systems, and economic continuity. While some individuals may bear indirect costs, the collective benefit from disease control is substantial.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, general_population, beneficiary,
    organized, generational, mobile, national).

% Individuals who refuse public health interventions (e.g., vaccination) for personal, philosophical, or medical reasons. They bear the direct costs of non-compliance, such as employment termination, restrictions on travel, or exclusion from public spaces. They are framed as imposing an externality on the collective.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_individuals, payer,
    powerless, immediate, identity_locked, local).

% Individuals whose religious beliefs preclude participation in certain public health interventions. They face similar costs and restrictions as other unvaccinated individuals, often leading to legal challenges based on religious freedom.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, individuals_with_religious_exemptions, payer,
    powerless, biographical, identity_locked, local).

% Legal professionals who analyze and challenge public health mandates on constitutional grounds, such as bodily autonomy, religious freedom, or due process. They represent individuals and groups affected by the interventions and seek to balance individual rights with state power.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_lawyers, observer,
    powerful, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To achieve population-level immunity and reduce the spread of infectious diseases, thereby protecting vulnerable individuals and preventing healthcare system overload.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from the collective (especially the vulnerable) to individuals who refuse to comply with public health interventions, through social and economic penalties.
% ABSENT_VOICES: Individuals and advocacy groups who prioritize absolute bodily autonomy or specific religious freedoms would object to the coercive nature of these interventions. They are often marginalized in public health discourse, which prioritizes collective good.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, population-level morbidity and mortality would likely increase significantly, especially during outbreaks. Healthcare systems would be overwhelmed, and vulnerable populations would face severe risks. The social contract around collective health would fundamentally shift, leading to a reorganization of public health governance and individual responsibilities.
% FOUNDING_PROBLEM: The problem of managing infectious disease outbreaks and protecting vulnerable populations from widespread illness and death, particularly when individual actions can impact collective health.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (e.g., WHO, CDC) and medical professionals universally corroborate that managing infectious diseases and protecting vulnerable populations remains a live and critical problem. Epidemiological data and historical precedents of disease outbreaks support this assessment from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates public health outcomes (benefiting the immunocompromised and general population) but achieves this through significant, asymmetric extraction from non-compliant individuals. The high extractiveness (0.78) reflects the severe consequences for those who refuse, such as job loss or exclusion from public life. Suppression (0.85) is high due to the active and often coercive enforcement by public health authorities. The theater ratio is low (0.1) as the interventions are largely functional in achieving their stated public health goals, with minimal performative elements. Accessibility collapse is moderate (0.6) as alternatives to compliance (e.g., avoiding public spaces, remote work) exist but are severely constrained. Resistance is high (0.7) due to strong individual rights advocacy and legal challenges.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities perceive this as a necessary coordination mechanism to protect the collective, with individual costs being a justified price for societal well-being. Unvaccinated individuals, however, experience it as pure extraction and suppression, a violation of bodily autonomy. The engine's per-seat classification would reflect this divergence, with authorities seeing a Rope and non-compliant individuals experiencing a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are full beneficiaries (d=0.0) as they achieve their mandate and maintain societal order. Immunocompromised individuals and the general population are also beneficiaries (d=0.1-0.2) as they are directly protected. Unvaccinated individuals and those with religious exemptions are full targets (d=1.0) as they bear the full cost of non-compliance without direct benefit from the specific intervention they refuse. Constitutional lawyers are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not mandatrophic; its mandate (reducing morbidity/mortality) is actively pursued and demonstrably live, especially during public health crises. The classification as Tangled Rope prevents mislabeling it as pure extraction by acknowledging its genuine coordination function, while also highlighting the asymmetric costs imposed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of public health necessity, or an overreach of state power?',
    'Comparative analysis with other readings of the ''legitimate_health_intervention'' kernel, specifically ''bodily_autonomy_primary'' and ''proportionality_reading'', to identify the specific points of structural divergence and their normative implications.',
    'If the ''bodily_autonomy_primary'' reading were adopted, the victim set would shrink, and extractiveness would decrease significantly. If ''proportionality_reading'' were adopted, the constraint''s application would be highly conditional on disease severity and intervention invasiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading (''public_health_primary'') of the ''legitimate_health_intervention'' kernel. It prioritizes population-level health outcomes over individual autonomy, viewing refusal as an externality.').

omega_variable(
    externality_quantification,
    'How precisely can the externality imposed by individual refusal be quantified in terms of population-level morbidity and mortality?',
    'Epidemiological modeling and public health data analysis to establish a direct causal link and magnitude of harm from individual non-compliance.',
    'If the externality is small or difficult to quantify, the justification for high suppression and extractiveness weakens, potentially shifting the constraint towards a ''snare'' or ''piton'' if the coordination function is deemed insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_quantification, empirical, 'The degree to which individual refusal of public health measures directly translates to measurable population harm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__public_health_primary, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__public_health_primary, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__public_health_primary, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__public_health_primary, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__public_health_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__public_health_primary, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legitimate_health_intervention' kernel, each representing a distinct structural claim about the balance between individual rights and public health.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
