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
 *   human_readable: State Authority to Criminalize Drug Use (Prohibition Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'prohibition reading' of state authority
 *   over substance control, where the primary mechanism for protecting third
 *   parties from drug-related crime and social disorder is the
 *   criminalization of drug use and possession. It is one reading of the
 *   broader 'substance_control_authority' kernel, distinct from harm
 *   reduction or legalization approaches. The narrative focuses on the
 *   enforcement, punitive measures, and the claimed public safety benefits,
 *   alongside the significant costs borne by individuals and specific
 *   communities.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__prohibition_reading, 0.8).
domain_priors:suppression_score(substance_control_authority__prohibition_reading, 0.9).
domain_priors:theater_ratio(substance_control_authority__prohibition_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(substance_control_authority__prohibition_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__prohibition_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_authority__prohibition_reading, "State Authority to Criminalize Drug Use (Prohibition Reading)").
narrative_ontology:topic_domain(substance_control_authority__prohibition_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__prohibition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__prohibition_reading, '4b34d0d4-2aaa-4555-b8f0-3683144cb6af').
narrative_ontology:cs_kernel_codification('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', formalized).
narrative_ontology:cs_authority_grounding('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', extraction).
narrative_ontology:cs_interpretation_layer_present('4b34d0d4-2aaa-4555-b8f0-3683144cb6af').
narrative_ontology:cs_reading_relation('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', substance_control_authority__harm_reduction_reading, forecloses).
narrative_ontology:cs_reading_relation('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', substance_control_authority__legalization_reading, forecloses).
narrative_ontology:cs_axiom('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', foundational, drug_use_is_inherently_immoral_and_harmful_to_society).
narrative_ontology:cs_axiom_status(drug_use_is_inherently_immoral_and_harmful_to_society, holdable).
narrative_ontology:cs_axiom_grounding('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', drug_use_is_inherently_immoral_and_harmful_to_society, deontological).
narrative_ontology:cs_axiom('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', foundational, criminalization_effectively_deters_drug_related_crime).
narrative_ontology:cs_axiom_status(criminalization_effectively_deters_drug_related_crime, holdable).
narrative_ontology:cs_axiom_grounding('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', criminalization_effectively_deters_drug_related_crime, empirically_contingent).
narrative_ontology:cs_reference_frame('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', public_order_through_deterrence).
narrative_ontology:cs_drift_state('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', contemporary_evidence_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4b34d0d4-2aaa-4555-b8f0-3683144cb6af', '').
narrative_ontology:cs_kernel_id(substance_control_authority__prohibition_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__prohibition_reading, third_party_citizens).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, drug_users).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, communities_of_color).
narrative_ontology:constraint_victim(substance_control_authority__prohibition_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact and maintain laws criminalizing drug use and possession, often responding to public pressure for 'tough on crime' policies. They benefit from perceived public order and political support.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, state_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Actively enforce drug prohibition laws through arrests, surveillance, and asset forfeiture. They benefit from increased budgets, expanded powers, and a clear mandate, despite the high operational costs.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, law_enforcement_agencies, agenda_setter,
    institutional, biographical, constrained, local).

% Are the intended beneficiaries of reduced drug-related crime and social disorder, experiencing a perceived increase in public safety and quality of life. They bear indirect costs through taxation.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, third_party_citizens, beneficiary,
    organized, biographical, mobile, local).

% Face criminal charges, fines, incarceration, and social stigma for drug use or possession. Their lives are severely disrupted, and access to legal employment, housing, and healthcare is often denied. Exit from the illicit market is difficult and dangerous.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, drug_users, payer,
    powerless, immediate, trapped, local).

% Experience disproportionate targeting and harsher penalties under drug prohibition laws, leading to higher incarceration rates, family separation, and systemic disadvantage. Their identity is often fused with the struggle against racialized enforcement.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, communities_of_color, payer,
    organized, generational, identity_locked, local).

% Bear the substantial financial costs of drug law enforcement, incarceration, and related social services, often without clear evidence of proportional benefits in public safety.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Argue for public health-centered approaches to drug use, emphasizing treatment and harm reduction over criminalization. Their policy recommendations are often marginalized or actively opposed by prohibitionist frameworks.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, public_health_advocates, excluded,
    moderate, generational, constrained, national).

% Analyze the social and economic impacts of drug prohibition, often highlighting its unintended consequences, racial disparities, and economic inefficiencies. They provide evidence that challenges the foundational assumptions of the prohibitionist approach.
narrative_ontology:constraint_stakeholder(substance_control_authority__prohibition_reading, sociologists_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To deter drug use and associated crime, thereby maintaining public order and safety for third parties by removing drugs and users from public spaces.
% TRANSFER_FUNCTION: Transfers freedom, economic resources (fines, asset forfeiture), and social capital from drug users and disproportionately affected communities to the state (via enforcement budgets and carceral systems) and, ostensibly, to third-party citizens (via perceived public safety).
% ABSENT_VOICES: Public health experts, civil liberties advocates, and communities disproportionately affected by enforcement are often excluded from policy-making, and would argue for alternative, less punitive approaches.
% DISAPPEARANCE_RATIONALE: If drug criminalization vanished overnight, the criminal justice system would undergo massive restructuring, illicit drug markets would transform (potentially into regulated ones), and public health and social services would need to dramatically expand to address drug use as a health issue. Social dynamics around drug use would fundamentally shift.
% FOUNDING_PROBLEM: Rising rates of drug use, perceived increases in drug-related crime, and social disorder in the mid-20th century, leading to public demand for state intervention to protect communities.
% FOUNDING_PROBLEM_CORROBORATION: Proponents (e.g., some law enforcement, political figures) argue the problem is still live and requires prohibition. Critics (e.g., public health organizations, civil rights groups, economists) argue the problem has either been exacerbated by prohibition or has evolved to require different solutions, citing independent research and testimony from affected communities.
narrative_ontology:disappearance_verdict(substance_control_authority__prohibition_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__prohibition_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__prohibition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(substance_control_authority__prohibition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_authority__prohibition_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.8) reflects the severe penalties, fines, and incarceration associated with drug offenses, which transfer resources and freedom from individuals to the state. Suppression (0.9) is extremely high due to active, often aggressive, law enforcement, legal barriers, and social stigma that severely limit alternatives. The moderate theater ratio (0.4) acknowledges that while some enforcement genuinely aims at public safety, a significant portion is performative, maintaining a policy that faces increasing evidence of ineffectiveness and disproportionate impact. Accessibility collapse is high (0.85) as legal alternatives are suppressed, forcing users into dangerous illicit markets. Resistance (0.6) is substantial from civil liberties and reform groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state authorities and some third-party citizens, this constraint is a necessary measure for public safety and order. However, from the perspective of drug users, communities of color, and many public health experts, it operates as a highly extractive and suppressive system that exacerbates social problems rather than solving them. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State legislatures and law enforcement agencies are clear beneficiaries, gaining power, budgets, and political capital from their role in enforcing prohibition. Third-party citizens are claimed beneficiaries, experiencing a perceived reduction in drug-related disorder. Drug users and communities of color are the primary victims, bearing the brunt of criminalization, incarceration, and systemic disadvantage. Taxpayers are also victims, funding the high costs of enforcement. Public health advocates are excluded, as their alternative approaches are not integrated into this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of drug-related crime and social disorder is contested in its status. While proponents argue it remains live, critics contend that prohibition has either failed to solve it or actively worsened it by creating illicit markets, driving up violence, and fostering racial disparities. The persistence of high enforcement costs and social harms, despite decades of policy, suggests a potential for mandatrophy, where the original mandate is either dead or the chosen solution is counterproductive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_deterrence,
    'Does the criminalization of drug use and possession effectively deter drug-related crime and reduce social disorder, or does it primarily displace these issues and create new harms?',
    'Comparative analysis of jurisdictions with different drug policies (prohibition vs. harm reduction vs. legalization) on metrics such as crime rates, public health outcomes, and social disorder indicators.',
    'If criminalization is found to be ineffective or counterproductive, the constraint''s claimed coordination function would be undermined, shifting its classification closer to a pure Snare. If effective, it would reinforce the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_deterrence, empirical, 'Whether criminalization achieves its stated public safety goals.').

omega_variable(
    disproportionate_enforcement,
    'Is the application of drug prohibition laws racially and socioeconomically neutral, or does it disproportionately target and harm marginalized communities?',
    'Statistical analysis of arrest, conviction, and sentencing data disaggregated by race, ethnicity, and socioeconomic status, alongside qualitative studies of policing practices in different communities.',
    'If disproportionate enforcement is confirmed, the constraint''s extraction would be seen as unjustly concentrated, amplifying the Snare-like qualities and highlighting systemic injustice within the Tangled Rope structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disproportionate_enforcement, empirical, 'Racial and socioeconomic equity in drug law enforcement.').

omega_variable(
    cost_benefit_balance,
    'Do the societal costs of drug prohibition (enforcement, incarceration, lost productivity, social harms) outweigh the benefits of perceived public safety and order?',
    'Comprehensive economic and social cost-benefit analyses comparing the prohibitionist approach to alternative drug policies, including externalized costs and benefits.',
    'If costs significantly outweigh benefits, the constraint''s legitimacy would be severely challenged, further eroding its coordination claim and pushing it towards a Snare or Piton (if maintained purely by inertia despite high costs).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_benefit_balance, empirical, 'Overall societal cost-effectiveness of drug prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__prohibition_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_authority__prohibition_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(subs_tr_t1980, substance_control_authority__prohibition_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(subs_tr_t1990, substance_control_authority__prohibition_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(subs_tr_t2000, substance_control_authority__prohibition_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__prohibition_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(subs_tr_t2020, substance_control_authority__prohibition_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_authority__prohibition_reading, base_extractiveness, 1970, 0.65).
narrative_ontology:measurement(subs_be_t1980, substance_control_authority__prohibition_reading, base_extractiveness, 1980, 0.72).
narrative_ontology:measurement(subs_be_t1990, substance_control_authority__prohibition_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(subs_be_t2000, substance_control_authority__prohibition_reading, base_extractiveness, 2000, 0.83).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__prohibition_reading, base_extractiveness, 2010, 0.81).
narrative_ontology:measurement(subs_be_t2020, substance_control_authority__prohibition_reading, base_extractiveness, 2020, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_authority__prohibition_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(subs_su_t1980, substance_control_authority__prohibition_reading, suppression_requirement, 1980, 0.8).
narrative_ontology:measurement(subs_su_t1990, substance_control_authority__prohibition_reading, suppression_requirement, 1990, 0.9).
narrative_ontology:measurement(subs_su_t2000, substance_control_authority__prohibition_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__prohibition_reading, suppression_requirement, 2010, 0.91).
narrative_ontology:measurement(subs_su_t2020, substance_control_authority__prohibition_reading, suppression_requirement, 2020, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_authority__prohibition_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'prohibition_reading' of the 'substance_control_authority' kernel, which also includes 'harm_reduction_reading' and 'legalization_reading' as sibling constraints. Each reading instantiates a distinct constraint with different structural properties and outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
