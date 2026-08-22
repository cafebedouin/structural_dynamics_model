% ============================================================================
% CONSTRAINT STORY: substance_control_kernel__harm_reduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_kernel__harm_reduction_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_kernel__harm_reduction_reading
 *   human_readable: Harm Reduction Approach to Substance Use
 *   domain: public_health/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'harm reduction' reading of substance
 *   control policy. It views substance use primarily as a public health issue
 *   requiring pragmatic interventions to reduce negative consequences (e.g.,
 *   overdose, disease transmission), independent of whether use cessation
 *   occurs. While it shifts away from purely punitive approaches, it
 *   maintains a degree of state intervention and paternalism, and the
 *   underlying illicit supply chain often remains criminalized. The claimed
 *   type is 'tangled_rope' because it genuinely coordinates public health
 *   efforts while still extracting costs from people who use drugs and
 *   maintaining a coercive element through ongoing criminalization of supply.
 *
 * KEY AGENTS:
 *   - public_health_agencies: Agenda setter (institutional/constrained) — administer programs, advocate policy.
 *   - people_who_use_drugs: Payer/Beneficiary (powerless/identity_locked) — receive services, bear stigma and criminalized supply costs.
 *   - harm_reduction_advocates: Beneficiary (organized/mobile) — gain influence, push for policy change.
 *   - law_enforcement: Agenda setter (institutional/constrained) — shift focus from punitive arrests to public order.
 *   - illicit_supply_chain_actors: Payer (powerful/constrained) — operate in criminalized market, face enforcement.
 *   - prohibition_advocates: Excluded (organized/constrained) — oppose harm reduction, advocate for strict punishment.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_kernel__harm_reduction_reading, 0.45).
domain_priors:suppression_score(substance_control_kernel__harm_reduction_reading, 0.6).
domain_priors:theater_ratio(substance_control_kernel__harm_reduction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_kernel__harm_reduction_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_kernel__harm_reduction_reading, tangled_rope).
narrative_ontology:human_readable(substance_control_kernel__harm_reduction_reading, "Harm Reduction Approach to Substance Use").
narrative_ontology:topic_domain(substance_control_kernel__harm_reduction_reading, "public_health/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_kernel__harm_reduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_kernel__harm_reduction_reading, '3c5c01ee-06c7-4a9b-a804-7ac7e902379d').
narrative_ontology:cs_kernel_codification('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', formalized).
narrative_ontology:cs_authority_grounding('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', expertise).
narrative_ontology:cs_interpretation_layer_present('3c5c01ee-06c7-4a9b-a804-7ac7e902379d').
narrative_ontology:cs_reading_relation('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', substance_control_kernel__prohibition_reading, influences).
narrative_ontology:cs_reading_relation('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', substance_control_kernel__legalization_reading, coexists_with).
narrative_ontology:cs_axiom('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', foundational, substance_use_is_health_condition).
narrative_ontology:cs_axiom_status(substance_use_is_health_condition, holdable).
narrative_ontology:cs_axiom_grounding('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', substance_use_is_health_condition, empirically_contingent).
narrative_ontology:cs_axiom('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', foundational, harm_reduction_is_ethical_imperative).
narrative_ontology:cs_axiom_status(harm_reduction_is_ethical_imperative, holdable).
narrative_ontology:cs_axiom_grounding('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', harm_reduction_is_ethical_imperative, deontological).
narrative_ontology:cs_reference_frame('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', public_health_first_approach).
narrative_ontology:cs_drift_state('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', contemporary_opioid_crisis_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3c5c01ee-06c7-4a9b-a804-7ac7e902379d', '').
narrative_ontology:cs_kernel_id(substance_control_kernel__harm_reduction_reading, substance_control_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, harm_reduction_advocates).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).
narrative_ontology:constraint_victim(substance_control_kernel__harm_reduction_reading, illicit_supply_chain_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_kernel__harm_reduction_reading, people_who_use_drugs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer harm reduction programs, distribute resources (e.g., naloxone, clean needles), and advocate for policy changes. They benefit from increased funding and legitimacy for their public health mandate, but are constrained by existing legal frameworks.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, public_health_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Receive health services and reduced criminal penalties, but remain subject to paternalistic interventions and social stigma. They bear the costs of a still-criminalized supply chain and the ongoing health risks associated with illicit substances. Their identity is often fused with their use, making 'exit' from the system complex.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, payer,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(substance_control_kernel__harm_reduction_reading, people_who_use_drugs, beneficiary).

% Champion policies that prioritize health over punishment. They gain influence and resources as harm reduction gains traction, but must continuously navigate political opposition and funding limitations.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, harm_reduction_advocates, beneficiary,
    organized, biographical, mobile, national).

% Shifts focus from punitive arrests for possession to addressing violent crime and managing public order. They experience reduced caseloads for minor drug offenses but must adapt to new training and community engagement models, often with internal resistance.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, law_enforcement, agenda_setter,
    institutional, generational, constrained, local).

% Continue to operate in a criminalized market, facing enforcement actions, albeit with some shifts in priorities. They bear the risks of illicit trade, but also benefit from the continued illegality that maintains high profit margins.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, illicit_supply_chain_actors, payer,
    powerful, biographical, constrained, global).

% Believe substance use is a moral failing requiring strict punishment and would object to any policy that normalizes or facilitates drug use. They are increasingly marginalized in policy debates but retain significant political influence in some regions.
narrative_ontology:constraint_stakeholder(substance_control_kernel__harm_reduction_reading, prohibition_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public health interventions, social services, and law enforcement efforts to reduce the negative health and social consequences of substance use, particularly overdose deaths and disease transmission.
% TRANSFER_FUNCTION: Transfers resources (funding, personnel, political capital) from punitive criminal justice approaches to public health and social support services. It also transfers some risk from individual users to public health systems, while maintaining a criminalized supply chain.
% ABSENT_VOICES: People who use drugs, particularly those from marginalized communities, are often not at the table when policies are designed, leading to interventions that may not fully address their needs or preferences. Illicit supply chain actors are also excluded, though their interests are implicitly served by continued prohibition.
% DISAPPEARANCE_RATIONALE: If the harm reduction framework vanished, public health agencies would lose their mandate for these interventions, leading to a resurgence of punitive approaches, increased overdose deaths, and higher rates of infectious disease transmission. The social and health landscape around substance use would revert to a more criminalized, less health-focused state.
% FOUNDING_PROBLEM: The punitive 'War on Drugs' led to escalating overdose deaths, HIV/HCV epidemics among people who inject drugs, mass incarceration, and disproportionate impacts on marginalized communities, without reducing substance use rates.
% FOUNDING_PROBLEM_CORROBORATION: Public health data, medical professionals, and international health organizations consistently corroborate the ongoing public health crisis (overdoses, disease transmission) that harm reduction seeks to address. While some law enforcement and prohibition advocates contest the severity or interpretation of these problems, the core health data is widely accepted by independent scientific bodies.
narrative_ontology:disappearance_verdict(substance_control_kernel__harm_reduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_kernel__harm_reduction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_kernel__harm_reduction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_kernel__harm_reduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(substance_control_kernel__harm_reduction_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_kernel__harm_reduction_reading_tests).
:- end_tests(substance_control_kernel__harm_reduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate, reflecting the reduction in direct criminal penalties for users but the persistence of a criminalized supply chain and paternalistic state oversight. Suppression (0.60) is still substantial, as the state actively enforces regulations, albeit with a health focus, and alternatives to illicit supply remain limited. Theater ratio (0.20) is low, as the public health interventions are genuinely functional, though some enforcement efforts may be performative. The historical trend shows decreasing extractiveness and suppression as harm reduction policies gain traction, moving away from the higher values of a pure prohibitionist approach.
 *
 * PERSPECTIVAL GAP:
 *   Public health agencies and harm reduction advocates perceive this as a 'rope' or 'scaffold' – a necessary, beneficial coordination mechanism. However, people who use drugs, while benefiting from reduced immediate harms, still experience it as a 'tangled_rope' due to ongoing stigma, limited autonomy, and the coercive aspects of a criminalized supply chain. Law enforcement's perspective is mixed, balancing public health goals with traditional enforcement mandates.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and harm reduction advocates are beneficiaries, gaining legitimacy and resources. People who use drugs are both beneficiaries (access to life-saving services) and payers (ongoing stigma, criminalized supply, paternalistic oversight), leading to a complex, identity-locked position. Illicit supply chain actors remain targets of enforcement. Law enforcement's directionality is complex, shifting from pure enforcement to a more coordinated role, but still maintaining coercive power.
 *
 * MANDATROPHY ANALYSIS:
 *   The harm reduction framework emerged as a response to the clear failure of pure prohibition to address public health crises. It prevents mislabeling genuine public health coordination as pure extraction by acknowledging the real benefits (e.g., reduced overdose deaths). However, it avoids mislabeling extraction as pure coordination by recognizing the ongoing coercive elements and the costs borne by people who use drugs due to the continued criminalization of supply. The 'tangled_rope' classification captures this hybridity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    paternalism_vs_autonomy,
    'To what extent does harm reduction, while reducing immediate harms, perpetuate paternalistic state control over individuals'' choices regarding substance use, rather than fully respecting their autonomy?',
    'Analysis of policy design and implementation: do programs prioritize user-driven goals and self-determination, or do they implicitly aim for cessation through ''softer'' means? Longitudinal studies on user empowerment and agency.',
    'If paternalism is high, the effective extractiveness for people who use drugs is higher than measured, as their agency is suppressed. If autonomy is genuinely prioritized, the constraint moves closer to a ''rope'' for users.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paternalism_vs_autonomy, conceptual, 'Ambiguity in the balance between state protection and individual autonomy within harm reduction.').

omega_variable(
    supply_chain_criminalization_impact,
    'What is the true cost (health, social, economic) of maintaining a criminalized illicit supply chain alongside harm reduction services, compared to a regulated legal supply?',
    'Comparative analysis with jurisdictions that have legalized or decriminalized supply chains, examining public health outcomes, crime rates, and economic impacts.',
    'If the costs of criminalized supply are high and avoidable, the ''tangled_rope'' classification is strongly reinforced, highlighting the extraction from people who use drugs and the perpetuation of organized crime. If the costs are deemed necessary or minimal, the ''rope'' aspect of coordination is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_criminalization_impact, empirical, 'The unmeasured costs and benefits of the continued criminalization of the substance supply chain.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, lack of safe supply) or internalized (stigma, self-blame, fear of legal repercussions) for people who use drugs?',
    'Post-exit suppression trajectory: if suppression persists (e.g., difficulty accessing housing/employment) after legal penalties are removed, reclassify as partially internalized. Qualitative research on lived experience.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true ''exit'' from the system harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for people who use drugs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_kernel__harm_reduction_reading, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1980, substance_control_kernel__harm_reduction_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(subs_tr_t1990, substance_control_kernel__harm_reduction_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(subs_tr_t2000, substance_control_kernel__harm_reduction_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(subs_tr_t2010, substance_control_kernel__harm_reduction_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(subs_tr_t2020, substance_control_kernel__harm_reduction_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(subs_tr_t2024, substance_control_kernel__harm_reduction_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(subs_be_t1980, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1980, 0.7).
narrative_ontology:measurement(subs_be_t1990, substance_control_kernel__harm_reduction_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(subs_be_t2000, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(subs_be_t2010, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2010, 0.5).
narrative_ontology:measurement(subs_be_t2020, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2020, 0.47).
narrative_ontology:measurement(subs_be_t2024, substance_control_kernel__harm_reduction_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1980, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1980, 0.9).
narrative_ontology:measurement(subs_su_t1990, substance_control_kernel__harm_reduction_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(subs_su_t2000, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(subs_su_t2010, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(subs_su_t2020, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2020, 0.62).
narrative_ontology:measurement(subs_su_t2024, substance_control_kernel__harm_reduction_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(substance_control_kernel__harm_reduction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__prohibition_reading).
narrative_ontology:affects_constraint(substance_control_kernel__harm_reduction_reading, substance_control_kernel__legalization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'substance_control_kernel'. Its harm reduction approach influences, but does not foreclose, the prohibitionist and legalization readings, as all three represent live policy options or historical trajectories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
