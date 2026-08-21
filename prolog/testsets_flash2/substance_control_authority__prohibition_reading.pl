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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: Prohibitionist Reading of Substance Control Authority
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the prohibitionist reading of state authority
 *   over substance control, focusing on criminalizing drug use and possession
 *   to protect third parties from drug-related crime and social disorder. It
 *   is one reading of the broader 'substance_control_authority' kernel. This
 *   reading emphasizes deterrence through punitive measures, leading to high
 *   enforcement costs and significant social extraction, particularly from
 *   marginalized communities. The claimed type is 'snare' due to its high
 *   extraction, suppression, and identifiable victims, despite its proponents
 *   claiming a 'rope' or 'scaffold' function for public safety.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.85).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.92).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "Prohibitionist Reading of Substance Control Authority").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, 'de244cb6-8643-4fec-8800-296eaf75d9d0').
narrative_ontology:cs_kernel_codification('de244cb6-8643-4fec-8800-296eaf75d9d0', formalized).
narrative_ontology:cs_authority_grounding('de244cb6-8643-4fec-8800-296eaf75d9d0', extraction).
narrative_ontology:cs_interpretation_layer_present('de244cb6-8643-4fec-8800-296eaf75d9d0').
narrative_ontology:cs_reading_relation('de244cb6-8643-4fec-8800-296eaf75d9d0', substance_control_authority__harm_reduction_reading, influences).
narrative_ontology:cs_reading_relation('de244cb6-8643-4fec-8800-296eaf75d9d0', substance_control_authority__legalization_reading, influences).
narrative_ontology:cs_axiom('de244cb6-8643-4fec-8800-296eaf75d9d0', foundational, criminalization_as_primary_deterrent).
narrative_ontology:cs_axiom_status(criminalization_as_primary_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('de244cb6-8643-4fec-8800-296eaf75d9d0', criminalization_as_primary_deterrent, empirically_contingent).
narrative_ontology:cs_axiom('de244cb6-8643-4fec-8800-296eaf75d9d0', foundational, state_duty_to_protect_from_vice).
narrative_ontology:cs_axiom_status(state_duty_to_protect_from_vice, holdable).
narrative_ontology:cs_axiom_grounding('de244cb6-8643-4fec-8800-296eaf75d9d0', state_duty_to_protect_from_vice, deontological).
narrative_ontology:cs_reference_frame('de244cb6-8643-4fec-8800-296eaf75d9d0', war_on_drugs_paradigm).
narrative_ontology:cs_drift_state('de244cb6-8643-4fec-8800-296eaf75d9d0', contemporary_public_opinion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('de244cb6-8643-4fec-8800-296eaf75d9d0', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, private_prison_industry).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_citizens).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, racial_minority_communities).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, public_health_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforce drug laws, leading to arrests and convictions. Their budgets and operational scope are often tied to drug interdiction efforts. They benefit from the mandate to 'protect' society through criminalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, national).

% Profits directly from high incarceration rates, a significant portion of which are due to drug-related offenses. They lobby for policies that maintain or expand criminalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, private_prison_industry, beneficiary,
    organized, generational, arbitrage, national).

% Perceive themselves as protected from drug-related crime and social disorder through criminalization. They support strict drug laws, often without fully understanding the associated social costs or racial disparities.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_citizens, beneficiary,
    moderate, biographical, constrained, local).

% Face criminal charges, incarceration, loss of civil liberties, and social stigma. Their lives are directly targeted by the prohibitionist framework, with limited access to support or alternatives.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by drug enforcement, leading to higher arrest and incarceration rates compared to other groups, despite similar rates of drug use. This perpetuates systemic disadvantage and intergenerational trauma.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racial_minority_communities, payer,
    powerless, generational, identity_locked, national).

% Argue for public health approaches to drug use, emphasizing treatment, prevention, and harm reduction over criminalization. Their policy proposals are often marginalized or actively suppressed by the prohibitionist framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_advocates, excluded,
    organized, generational, constrained, national).

% Analyze the economic costs and benefits of drug prohibition, including enforcement costs, lost tax revenue, and the creation of black markets. They often highlight the inefficiencies and unintended consequences of criminalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, economic_analysts, observer,
    analytical, biographical, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social behavior by deterring drug use and associated criminal activity, thereby maintaining public order and safety as defined by the state.
% TRANSFER_FUNCTION: Transfers resources (taxpayer money) to law enforcement and carceral systems, and transfers freedom/well-being from drug users and disproportionately affected communities to the state and perceived 'protected' third parties.
% ABSENT_VOICES: Drug users themselves, public health experts advocating for alternative models, and communities disproportionately harmed by enforcement are often excluded from policy-making, where they would argue for decriminalization, treatment, and social justice.
% DISAPPEARANCE_RATIONALE: If the authority to criminalize drug use vanished overnight, the criminal justice system would undergo massive restructuring, prison populations would plummet, black markets would shift, and public health systems would face immediate pressure to scale up services. Society would have to rapidly re-evaluate its approach to substance use.
% FOUNDING_PROBLEM: To control substances deemed dangerous and to prevent the social ills (crime, disorder, moral decay) believed to be directly caused by drug use.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement and some citizens attest the problem is live, citing ongoing drug-related crime. Public health advocates and social justice organizations, supported by empirical studies, attest the founding problem is largely misdiagnosed or exacerbated by the prohibitionist approach itself, and that the arrangement persists due to institutional inertia and vested interests rather than effective problem-solving.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is high (0.85) because the criminalization framework imposes severe costs on individuals (incarceration, fines, social stigma) without demonstrably solving the underlying problems of drug use or related crime, often exacerbating them. Suppression (0.92) is extremely high, as the state actively enforces laws, suppresses alternative approaches, and limits exit options for those caught in the system. Theater ratio (0.45) is moderate, reflecting that while some public safety functions are genuinely pursued, a significant portion of enforcement is performative, maintaining a moral stance or institutional power rather than achieving stated public health goals. Accessibility collapse is high (0.70) because legal alternatives for drug use are non-existent, and harm reduction services are often underfunded or stigmatized. Resistance (0.75) is also high, coming from drug users, civil rights groups, and public health advocates who actively challenge the prohibitionist paradigm.
 *
 * PERSPECTIVAL GAP:
 *   The state and its beneficiaries (law enforcement, private prisons, some citizens) perceive this as a necessary 'rope' or 'scaffold' for public safety and order. However, from the perspective of drug users, racial minority communities, and public health advocates, it operates as a 'snare' that extracts freedom, wealth, and well-being, while failing to address the root causes of substance use or crime. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Law enforcement and the private prison industry are clear beneficiaries, gaining resources and mandates from the prohibitionist framework. Third-party citizens are also beneficiaries, as they perceive increased safety and order, even if the actual impact is contested. Drug users and racial minority communities are the primary victims, bearing the brunt of criminalization and disproportionate enforcement. Public health advocates are excluded, as their alternative approaches are not integrated into this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The prohibitionist framework's mandate to protect third parties from drug-related crime is increasingly contested. While the founding problem (social disorder from drug use) is still 'contested', the 'prohibition_reading' approach is argued by many to be 'dead' as a solution, having failed to achieve its goals and created more harm. The persistence of this constraint, despite its high costs and contested efficacy, suggests institutional inertia and vested interests (law enforcement budgets, private prison profits) rather than a live, effective mandate. This prevents mislabeling it as a functional coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does criminalizing drug use effectively deter drug-related crime and protect third parties, or does it merely shift crime patterns and create new harms?',
    'Longitudinal studies comparing crime rates and public safety outcomes in jurisdictions with prohibitionist policies versus those with harm reduction or legalization approaches.',
    'If deterrence is found to be ineffective or counterproductive, the ''protection'' justification for this constraint weakens, reclassifying it further towards a pure ''snare''. If effective, it would lend more credence to its claimed coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Uncertainty regarding the actual effectiveness of criminalization in achieving its stated public safety goals.').

omega_variable(
    racial_disparity_causality,
    'Are the observed racial disparities in drug law enforcement a consequence of the prohibitionist framework itself, or are they due to pre-existing social factors independent of the policy?',
    'Causal inference studies controlling for socio-economic factors and comparing enforcement patterns across different policy regimes and demographic groups.',
    'If the framework is found to be a direct cause of disparities, its ''justice'' claims are undermined, amplifying its extractive nature and solidifying its ''snare'' classification. If disparities are largely independent, the constraint''s structural injustice is less direct, though still present.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_disparity_causality, empirical, 'Ambiguity regarding the causal link between prohibitionist policies and racial disparities in enforcement.').

omega_variable(
    legitimacy_of_paternalism,
    'Is the state''s authority to criminalize drug use for an individual''s own good (paternalism) a legitimate exercise of power, or an overreach that infringes on personal autonomy?',
    'Philosophical and legal debate on the limits of state paternalism, potentially influenced by evolving societal values and human rights frameworks.',
    'If paternalism is deemed illegitimate, the moral grounding for criminalizing drug use (even if framed as ''protection'') collapses, further exposing the constraint''s coercive and extractive elements. If deemed legitimate, it provides a stronger normative basis for the prohibitionist stance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_paternalism, preference, 'Conceptual ambiguity regarding the ethical limits of state paternalism in drug policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_authority__prohibition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(subs_tr_t1980, substance_control_authority__prohibition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(subs_tr_t1990, substance_control_authority__prohibition_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(subs_tr_t2000, substance_control_authority__prohibition_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__prohibition_reading, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(subs_tr_t2024, substance_control_authority__prohibition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_authority__prohibition_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(subs_be_t1980, substance_control_authority__prohibition_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(subs_be_t1990, substance_control_authority__prohibition_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(subs_be_t2000, substance_control_authority__prohibition_reading, base_extractiveness, 2000, 0.9).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__prohibition_reading, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(subs_be_t2024, substance_control_authority__prohibition_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_authority__prohibition_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(subs_su_t1980, substance_control_authority__prohibition_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(subs_su_t1990, substance_control_authority__prohibition_reading, suppression_requirement, 1990, 0.95).
narrative_ontology:measurement(subs_su_t2000, substance_control_authority__prohibition_reading, suppression_requirement, 2000, 0.98).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__prohibition_reading, suppression_requirement, 2010, 0.95).
narrative_ontology:measurement(subs_su_t2024, substance_control_authority__prohibition_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, prison_industrial_complex_funding).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, public_health_funding_allocation).

% DUAL FORMULATION NOTE:
% This constraint is the 'prohibition_reading' of the 'substance_control_authority' kernel. Its high extractiveness and suppression structurally influence the viability and public perception of harm reduction and legalization approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
