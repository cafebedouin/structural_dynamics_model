% ============================================================================
% CONSTRAINT STORY: substance_control_authority__prohibition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__prohibition_reading, []).

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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: State Authority to Criminalize Drug Use (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'prohibition reading' of state authority
 *   over substance control, focusing on criminalizing drug use and possession
 *   to protect third parties from drug-related crime and social disorder. It
 *   is one reading of the 'substance_control_authority' kernel, distinct from
 *   harm reduction or legalization approaches. The metrics reflect a system
 *   with high extraction and suppression, where the claimed public safety
 *   benefits are increasingly overshadowed by the costs of enforcement and
 *   social harms, leading to a significant 'theater ratio' as the stated
 *   mandate diverges from actual outcomes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.9).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Authority to Criminalize Drug Use (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'd53532ec-85b0-41c7-86e1-6b6bba165171').
narrative_ontology:cs_kernel_codification('d53532ec-85b0-41c7-86e1-6b6bba165171', formalized).
narrative_ontology:cs_authority_grounding('d53532ec-85b0-41c7-86e1-6b6bba165171', lineage).
narrative_ontology:cs_interpretation_layer_present('d53532ec-85b0-41c7-86e1-6b6bba165171').
narrative_ontology:cs_reading_relation('d53532ec-85b0-41c7-86e1-6b6bba165171', substance_control_authority__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('d53532ec-85b0-41c7-86e1-6b6bba165171', substance_control_authority__legalization_reading, forecloses).
narrative_ontology:cs_axiom('d53532ec-85b0-41c7-86e1-6b6bba165171', foundational, drug_use_is_inherently_immoral_and_harmful).
narrative_ontology:cs_axiom_status(drug_use_is_inherently_immoral_and_harmful, holdable).
narrative_ontology:cs_axiom_grounding('d53532ec-85b0-41c7-86e1-6b6bba165171', drug_use_is_inherently_immoral_and_harmful, deontological).
narrative_ontology:cs_axiom('d53532ec-85b0-41c7-86e1-6b6bba165171', foundational, criminalization_deters_use_and_reduces_crime).
narrative_ontology:cs_axiom_status(criminalization_deters_use_and_reduces_crime, holdable).
narrative_ontology:cs_axiom_grounding('d53532ec-85b0-41c7-86e1-6b6bba165171', criminalization_deters_use_and_reduces_crime, empirically_contingent).
narrative_ontology:cs_reference_frame('d53532ec-85b0-41c7-86e1-6b6bba165171', public_order_through_deterrence).
narrative_ontology:cs_drift_state('d53532ec-85b0-41c7-86e1-6b6bba165171', contemporary_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d53532ec-85b0-41c7-86e1-6b6bba165171', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, state_legislature).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_parties_seeking_safety).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, affected_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and maintains laws criminalizing drug use and possession, framing them as essential for public safety and order. Benefits from maintaining authority and perceived control over social problems.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Actively enforces drug prohibition laws through arrests, seizures, and incarceration. Benefits from funding, expanded powers, and a clear mandate, even as effectiveness is debated.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement, agenda_setter,
    institutional, biographical, constrained, local).

% Are the primary targets of criminalization, facing arrest, fines, incarceration, and social stigma. Their options are limited to clandestine use or seeking treatment, often under coercive conditions.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, immediate, trapped, local).

% Bear the social costs of prohibition, including disproportionate policing, racial disparities in arrests and sentencing, family disruption, and reduced trust in law enforcement. They organize for reform but face significant institutional inertia.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, affected_communities, payer,
    organized, generational, constrained, local).

% Perceive a benefit from reduced visible drug use and associated crime, leading to a sense of increased public safety and social order. Their support for prohibition is often based on these perceived benefits.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_parties_seeking_safety, beneficiary,
    moderate, biographical, constrained, local).

% Advocate for public health-centered approaches to drug use, emphasizing treatment, prevention, and harm reduction over criminalization. Their voices are often marginalized in policy debates dominated by prohibitionist frameworks.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_advocates, excluded,
    organized, biographical, mobile, national).

% Challenge drug prohibition on grounds of individual rights, due process, and disproportionate impact on marginalized communities. They are often excluded from the core policy-making process but exert pressure through litigation and advocacy.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, civil_liberties_groups, excluded,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(substance_control_authority__prohibition_reading, state_legislature).
narrative_ontology:fixing_cost_class(substance_control_authority__prohibition_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate public behavior by deterring drug use and associated crime, thereby maintaining public order and safety for third parties.
% TRANSFER_FUNCTION: Transfers liberty, resources (fines, incarceration costs), and social capital from drug users and affected communities to the state and law enforcement, in exchange for perceived public safety and order for third parties.
% ABSENT_VOICES: Public health experts, civil liberties groups, and directly affected communities (especially those disproportionately targeted by enforcement) are often marginalized in policy debates, advocating for alternative approaches that are incompatible with prohibition.
% DISAPPEARANCE_RATIONALE: If drug criminalization vanished overnight, public order would initially be disrupted, but new regulatory and public health frameworks would emerge to manage drug use, and the criminal justice system would undergo massive restructuring. The illicit drug market would transform, and resources would shift from enforcement to public health.
% FOUNDING_PROBLEM: Perceived rise in drug use and associated crime, leading to social disorder, public safety concerns, and moral panic.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (state legislature, law enforcement) attest the problem is still live, citing ongoing drug crises and crime statistics. Critics (public health advocates, civil liberties groups, affected communities) attest the founding problem has shifted (e.g., opioid crisis, mental health crisis) and that prohibition exacerbates rather than solves it, citing public health data, sociological studies on racial disparities, and economic analyses of incarceration costs from independent academic and non-governmental organizations.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__prohibition_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(substance_control_authority__prohibition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) stems from the severe penalties imposed on drug users, including incarceration, fines, and loss of civil liberties. Suppression (0.90) is extremely high due to active, coercive enforcement by law enforcement, backed by legal statutes. The theater ratio (0.60) is substantial because while public safety is the stated goal, the actual impact often includes increased social disorder, racial disparities, and the perpetuation of illicit markets, suggesting a significant performative aspect to maintaining the prohibitionist stance despite evidence of its ineffectiveness. Accessibility collapse (0.80) is high as legal alternatives to drug use are suppressed, forcing users into illicit channels. Resistance (0.70) is also high, reflecting ongoing advocacy and social movements challenging the prohibitionist paradigm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and law enforcement, this constraint is a necessary tool for public order and safety (a Rope or Tangled Rope). From the perspective of drug users and affected communities, it is a highly extractive and suppressive Snare. The engine's classification will highlight this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legislature and law enforcement are beneficiaries and agenda-setters, gaining authority, funding, and a clear mandate. Drug users and affected communities are clear targets, bearing the direct costs of criminalization and its social consequences. Third parties seeking safety are beneficiaries of perceived order, though they may also bear indirect costs. Public health and civil liberties advocates are excluded, as their perspectives challenge the foundational premises of prohibition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits signs of mandatrophy, as the founding problem (social disorder from drug use) is increasingly contested in its status. While proponents argue it's still live, critics contend that prohibition itself exacerbates many social problems it purports to solve, leading to a situation where the mandate has outlived its original function or has become counterproductive. The high theater ratio and contested founding problem status are key indicators.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does criminalization effectively deter drug use and reduce drug-related crime, or does it primarily shift markets and exacerbate social harms?',
    'Longitudinal studies comparing crime rates, public health outcomes, and drug use prevalence in jurisdictions with prohibition versus those with harm reduction or legalization policies.',
    'If deterrence is found to be ineffective or counterproductive, the ''criminalization_deters_use_and_reduces_crime'' axiom would be overridden, shifting the constraint''s justification from empirical to purely deontological or performative, likely reclassifying it closer to a Snare or Piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Empirical validity of the deterrence claim underlying prohibition.').

omega_variable(
    racial_disparity_causality,
    'Are racial disparities in drug law enforcement a consequence of underlying social factors, or are they a direct result of the design and implementation of prohibition policies?',
    'Sociological and legal analyses examining policy intent, enforcement patterns, and judicial outcomes across different demographic groups, controlling for other variables.',
    'If disparities are found to be systemic to prohibition, it would strengthen the ''extraction'' component of the constraint, highlighting its disproportionate impact and potentially shifting its classification further towards a Snare for affected communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(racial_disparity_causality, empirical, 'Source of racial disparities in drug law enforcement.').

omega_variable(
    public_safety_definition_ambiguity,
    'Is ''public safety'' primarily defined by the absence of visible drug use and associated crime, or by broader public health and community well-being indicators?',
    'Policy shifts towards public health metrics (e.g., overdose rates, access to treatment, community health surveys) as primary indicators of success, rather than arrest rates or drug seizures.',
    'A redefinition of public safety would challenge the core justification of prohibition, potentially leading to a re-evaluation of its coordination function and a shift towards harm reduction or legalization frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_safety_definition_ambiguity, conceptual, 'Conceptual framing of public safety in drug policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_authority__prohibition_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(subs_tr_t1980, substance_control_authority__prohibition_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(subs_tr_t1990, substance_control_authority__prohibition_reading, theater_ratio, 1990, 0.5).
narrative_ontology:measurement(subs_tr_t2000, substance_control_authority__prohibition_reading, theater_ratio, 2000, 0.55).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__prohibition_reading, theater_ratio, 2010, 0.58).
narrative_ontology:measurement(subs_tr_t2020, substance_control_authority__prohibition_reading, theater_ratio, 2020, 0.6).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_authority__prohibition_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(subs_be_t1980, substance_control_authority__prohibition_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(subs_be_t1990, substance_control_authority__prohibition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(subs_be_t2000, substance_control_authority__prohibition_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__prohibition_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(subs_be_t2020, substance_control_authority__prohibition_reading, base_extractiveness, 2020, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_authority__prohibition_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(subs_su_t1980, substance_control_authority__prohibition_reading, suppression_requirement, 1980, 0.78).
narrative_ontology:measurement(subs_su_t1990, substance_control_authority__prohibition_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_authority__prohibition_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__prohibition_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(subs_su_t2020, substance_control_authority__prohibition_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
