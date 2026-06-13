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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__prohibition_reading
 *   human_readable: State Authority to Criminalize Drug Use/Possession (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'prohibition reading' of state authority
 *   over substance control, where criminalization of drug use and possession
 *   is the primary mechanism to protect third parties from drug-related crime
 *   and social disorder. It is characterized by high extraction from drug
 *   users and specific communities, high suppression through active
 *   enforcement, and significant resistance. The claimed type is 'snare' due
 *   to the identifiable victims and the coercive nature of its persistence,
 *   despite being framed by proponents as a 'rope' for public safety.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.78).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.92).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, snare).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Authority to Criminalize Drug Use/Possession (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '572f1111-00b7-4665-87df-8e3c1921f3ef').
narrative_ontology:cs_kernel_codification('572f1111-00b7-4665-87df-8e3c1921f3ef', formalized).
narrative_ontology:cs_authority_grounding('572f1111-00b7-4665-87df-8e3c1921f3ef', lineage).
narrative_ontology:cs_interpretation_layer_present('572f1111-00b7-4665-87df-8e3c1921f3ef').
narrative_ontology:cs_reading_relation('572f1111-00b7-4665-87df-8e3c1921f3ef', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_reading_relation('572f1111-00b7-4665-87df-8e3c1921f3ef', substance_control_authority__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('572f1111-00b7-4665-87df-8e3c1921f3ef', foundational, drug_use_is_inherently_criminal).
narrative_ontology:cs_axiom_status(drug_use_is_inherently_criminal, holdable).
narrative_ontology:cs_axiom_grounding('572f1111-00b7-4665-87df-8e3c1921f3ef', drug_use_is_inherently_criminal, deontological).
narrative_ontology:cs_axiom('572f1111-00b7-4665-87df-8e3c1921f3ef', foundational, criminalization_deters_crime).
narrative_ontology:cs_axiom_status(criminalization_deters_crime, holdable).
narrative_ontology:cs_axiom_grounding('572f1111-00b7-4665-87df-8e3c1921f3ef', criminalization_deters_crime, empirically_contingent).
narrative_ontology:cs_reference_frame('572f1111-00b7-4665-87df-8e3c1921f3ef', war_on_drugs_era).
narrative_ontology:cs_drift_state('572f1111-00b7-4665-87df-8e3c1921f3ef', contemporary_public_opinion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('572f1111-00b7-4665-87df-8e3c1921f3ef', '').
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

% Enact and maintain laws criminalizing drug use and possession, often responding to public pressure regarding crime and disorder. They control funding for enforcement and incarceration, and resist reforms that would undermine the prohibition framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Receive funding and mandates to enforce drug prohibition laws. Their operational scope and budgets are often tied to maintaining high arrest and conviction rates for drug offenses. They benefit from the broad powers granted under these laws.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter).

% Profits directly from high incarceration rates, including those for drug offenses. They lobby legislatures to maintain strict drug laws and oppose reforms that would reduce prison populations, creating a strong financial incentive for the prohibition framework.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, private_prison_industry, beneficiary,
    organized, generational, arbitrage, national).

% Perceive a reduction in drug-related crime and social disorder in their communities due to prohibition, and support policies that maintain public safety. They are often the 'protected' parties in the prohibition narrative, though the actual impact on crime is debated.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_citizens, beneficiary,
    moderate, biographical, mobile, local).

% Face criminal charges, incarceration, loss of employment, and social stigma for drug use or possession. Their lives are directly impacted by the enforcement of prohibition, with limited access to legal or medical alternatives due to criminalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, immediate, trapped, local).

% Disproportionately targeted by drug enforcement, leading to higher arrest and incarceration rates, family separation, and systemic disadvantage. They bear the brunt of the social and economic costs of prohibition, often due to historical biases in policing.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, racial_minority_communities, payer,
    organized, generational, identity_locked, local).

% Argue that drug use is a public health issue requiring medical and social support, not criminalization. Their proposals for harm reduction and treatment are often sidelined or actively opposed by the prohibition framework, limiting their influence on policy.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate social behavior by deterring drug use and related criminal activity, thereby maintaining public order and safety for non-users.
% TRANSFER_FUNCTION: Transfers freedom, economic opportunity, and social capital from drug users and disproportionately affected communities to law enforcement, the carceral system, and citizens who perceive increased safety.
% ABSENT_VOICES: Public health experts, addiction specialists, and civil liberties advocates are often marginalized in policy debates, advocating for evidence-based treatment and decriminalization over punitive measures. Their voices are excluded from the core policy-making process that maintains prohibition.
% DISAPPEARANCE_RATIONALE: If the authority to criminalize drug use vanished, the criminal justice system would undergo massive restructuring, prison populations would plummet, and public health systems would need to rapidly scale up to manage drug use as a medical issue. Social norms around drug use would shift dramatically, and new regulatory frameworks for substances would emerge.
% FOUNDING_PROBLEM: The perception of widespread drug use leading to crime, social decay, and public health crises, necessitating state intervention to protect society.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement and some citizens attest the problem is live, citing ongoing crime and disorder. Public health advocates and civil liberties groups, supported by independent research, attest that the problem has shifted from drug use itself to the harms caused by prohibition, and that the original problem is exacerbated, not solved, by criminalization. The corroboration for the 'live' status comes primarily from within the benefiting parties.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).

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
 *   Extractiveness is high (0.78) due to the severe consequences for drug users (incarceration, loss of rights) and the economic benefits for the carceral system. Suppression is very high (0.92) as the state actively enforces laws, suppresses alternative approaches (e.g., harm reduction), and limits exit options for those caught in the system. Resistance is also high (0.85) from affected communities and advocacy groups. The theater ratio (0.45) reflects that while some public safety goals are genuinely pursued, a significant portion of enforcement activity serves to maintain the carceral system and its associated economic interests, rather than solely addressing drug-related harm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of law enforcement and private prisons, this is a legitimate and necessary framework for public safety, justifying their roles and funding. From the perspective of drug users and racial minority communities, it is a highly extractive and oppressive system that disproportionately targets them, creating more harm than it prevents. Third-party citizens often experience a mixed perspective, valuing perceived safety but sometimes acknowledging the social costs.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures, law enforcement, and the private prison industry are clear beneficiaries (d near 0.0-0.2) as they gain power, resources, and profits from the prohibition framework. Third-party citizens are also beneficiaries (d near 0.3-0.4) due to perceived safety, though they bear indirect costs. Drug users and racial minority communities are clear targets (d near 0.9-1.0), facing direct criminalization and systemic harm. Public health advocates are excluded, their alternative approaches suppressed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (protecting third parties from drug-related crime) is increasingly contested. While the founding problem of drug-related harm was real, the prohibition reading has arguably shifted the problem from drug use to the harms of criminalization itself. The persistence of the constraint, despite evidence of its ineffectiveness and disproportionate impact, suggests a degree of mandatrophy, where the original function is overshadowed by the maintenance of the enforcement apparatus and its beneficiaries. The high suppression and extractiveness prevent a reclassification to Piton, as there are still clear beneficiaries actively maintaining the system, making it a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deterrence_efficacy_ambiguity,
    'Does the criminalization of drug use effectively deter drug-related crime and social disorder, or does it merely displace it and create new forms of harm?',
    'Longitudinal studies comparing crime rates and public health outcomes in jurisdictions with prohibition versus harm reduction or legalization policies.',
    'If deterrence is found to be ineffective or counterproductive, the justification for the prohibition reading weakens, potentially shifting its classification towards a more purely extractive Snare or even a Piton if the original mandate is entirely defunct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deterrence_efficacy_ambiguity, empirical, 'Empirical effectiveness of prohibition in achieving its stated goals.').

omega_variable(
    prohibition_vs_harm_reduction_framing,
    'Is the state''s role primarily punitive (prohibition) or therapeutic (harm reduction) in addressing substance use?',
    'Public policy shifts towards decriminalization and increased funding for public health interventions, or sustained public support for punitive measures.',
    'A shift towards a therapeutic framing would fundamentally alter the constraint''s structure, moving it away from a Snare towards a Rope or Scaffold focused on public health coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prohibition_vs_harm_reduction_framing, preference, 'Conceptual framing of state intervention in substance use.').

omega_variable(
    racial_disparity_causality,
    'Are racial disparities in drug enforcement a consequence of inherent bias in the prohibition framework, or of other socio-economic factors?',
    'Analysis of policing practices, sentencing guidelines, and socio-economic data, controlling for non-racial variables.',
    'If inherent bias is confirmed, it strengthens the ''snare'' classification by highlighting the structural nature of extraction and suppression, particularly for racial minority communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(racial_disparity_causality, empirical, 'Causality of racial disparities in drug enforcement.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''prohibition reading'' of state authority, or is it better understood as a ''harm reduction reading'' with punitive elements, or a ''legalization reading'' with regulatory capture?',
    'Analysis of legislative intent, enforcement patterns, and public discourse to determine the dominant underlying commitment. This specific story instantiates the prohibition reading.',
    'Reclassification to a different reading would entail a different set of beneficiaries, victims, and metrics, leading to a distinct constraint classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the ''prohibition_reading'' of the ''substance_control_authority'' kernel. Sibling readings (harm_reduction_reading, legalization_reading) would shift the victim/beneficiary sets and the primary mechanism of control.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_authority__prohibition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(subs_tr_t1985, substance_control_authority__prohibition_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(subs_tr_t2000, substance_control_authority__prohibition_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__prohibition_reading, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(subs_tr_t2024, substance_control_authority__prohibition_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_authority__prohibition_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(subs_be_t1985, substance_control_authority__prohibition_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(subs_be_t2000, substance_control_authority__prohibition_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__prohibition_reading, base_extractiveness, 2010, 0.82).
narrative_ontology:measurement(subs_be_t2024, substance_control_authority__prohibition_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_authority__prohibition_reading, suppression_requirement, 1970, 0.75).
narrative_ontology:measurement(subs_su_t1985, substance_control_authority__prohibition_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_authority__prohibition_reading, suppression_requirement, 2000, 0.95).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__prohibition_reading, suppression_requirement, 2010, 0.98).
narrative_ontology:measurement(subs_su_t2024, substance_control_authority__prohibition_reading, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__harm_reduction_reading).
narrative_ontology:affects_constraint(substance_control_authority__prohibition_reading, substance_control_authority__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'substance_control_authority' kernel. The prohibition reading emphasizes criminalization and deterrence, contrasting with harm reduction and legalization approaches.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
