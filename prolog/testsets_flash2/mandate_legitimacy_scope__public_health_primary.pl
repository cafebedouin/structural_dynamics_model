% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Public Health Primary Reading of Mandate Legitimacy
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of state
 *   authority to compel vaccination, where the protection of vulnerable
 *   populations from serious harm is the paramount ethical and legal
 *   justification. It acknowledges the coercive aspect of mandates but frames
 *   it as a necessary, legitimate imposition for the collective good. The
 *   claimed type is Tangled Rope, reflecting a genuine coordination function
 *   (herd immunity) coupled with asymmetric extraction (from the
 *   unvaccinated). The metrics reflect the high social cost of non-compliance
 *   and the active enforcement required to maintain the constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.65).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.7).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Public Health Primary Reading of Mandate Legitimacy").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '5c3b0cea-0cb1-4879-9197-dc4b380cda6b').
narrative_ontology:cs_kernel_codification('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', formalized).
narrative_ontology:cs_authority_grounding('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', lineage).
narrative_ontology:cs_interpretation_layer_present('5c3b0cea-0cb1-4879-9197-dc4b380cda6b').
narrative_ontology:cs_reading_relation('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', foundational, collective_health_priority).
narrative_ontology:cs_axiom_status(collective_health_priority, holdable).
narrative_ontology:cs_axiom_grounding('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', collective_health_priority, deontological).
narrative_ontology:cs_axiom('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', foundational, duty_to_protect_vulnerable).
narrative_ontology:cs_axiom_status(duty_to_protect_vulnerable, holdable).
narrative_ontology:cs_axiom_grounding('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', duty_to_protect_vulnerable, deontological).
narrative_ontology:cs_reference_frame('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', utilitarian_public_health_framework).
narrative_ontology:cs_drift_state('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('5c3b0cea-0cb1-4879-9197-dc4b380cda6b', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_authorities).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, medical_autonomy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, healthcare_systems).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals (e.g., immunocompromised, infants) are at high risk of severe illness or death from vaccine-preventable diseases. They rely on herd immunity for protection and benefit directly from mandates that increase vaccination rates.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Tasked with protecting the collective health of the population, they implement and enforce vaccination mandates. They view mandates as a necessary tool to prevent outbreaks and reduce disease burden, especially for those who cannot be vaccinated.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Are compelled to receive vaccinations against their personal preference or belief, or face restrictions on participation in public life (e.g., school, employment). They bear the direct cost of bodily intrusion and perceived loss of autonomy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, local).

% Represent individuals who prioritize individual bodily autonomy and informed consent above collective health imperatives. They actively resist mandates through legal challenges, protests, and advocacy, viewing them as an overreach of state power.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, medical_autonomy_advocates, payer,
    organized, generational, mobile, national).

% Benefit from reduced caseloads during epidemics, preventing overwhelming surges that strain resources and compromise care for all patients. They support mandates as a means to maintain operational capacity and public trust.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, regional).

% Adjudicate legal challenges to vaccination mandates, balancing state police powers against individual rights. Their rulings shape the legal boundaries of public health authority and individual liberty.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity, protecting both vaccinated and unvaccinated individuals from disease, and preventing healthcare system collapse during epidemics.
% TRANSFER_FUNCTION: Transfers the burden of individual risk and bodily autonomy from vulnerable populations to unvaccinated individuals, in exchange for collective health security.
% ABSENT_VOICES: Future generations who would benefit from robust public health infrastructure and reduced disease burden are not directly represented, but their interests are implicitly championed by public health authorities. Individuals with rare, severe vaccine reactions are often marginalized in the discourse, their voices subsumed by the collective good.
% DISAPPEARANCE_RATIONALE: If state authority to compel vaccination vanished, vaccination rates would likely drop, leading to increased outbreaks, higher morbidity and mortality among vulnerable populations, and potential overwhelming of healthcare systems. The social contract around collective health would fundamentally shift.
% FOUNDING_PROBLEM: The historical problem of widespread infectious disease outbreaks causing significant morbidity, mortality, and societal disruption, particularly impacting vulnerable groups.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (e.g., WHO, CDC), medical associations, and epidemiologists universally corroborate that vaccine-preventable diseases remain a live threat, and that collective immunity is crucial for public health. This is attested by scientific consensus and ongoing disease surveillance data, independent of the specific beneficiaries of mandates.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__public_health_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because individuals are compelled to undergo medical procedures against their will, a significant imposition. Suppression is also high due to legal penalties, social pressure, and restrictions on participation in public life for non-compliance. Resistance is substantial, reflecting ongoing legal and social challenges to mandates. Theater ratio is low, as the public health function is generally genuine, though some enforcement may become performative during periods of low disease threat. The slight dip in extractiveness and suppression at the end of the interval reflects a post-pandemic recalibration of mandate intensity.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable populations, this constraint is a vital Rope, offering protection from existential threats. From the perspective of unvaccinated individuals, it is a Snare, coercively extracting bodily autonomy. Public health authorities view it as a necessary Tangled Rope, balancing individual rights with collective welfare. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable populations and public health authorities are beneficiaries, as mandates directly protect the former and empower the latter to fulfill their mission. Unvaccinated individuals and medical autonomy advocates are victims, bearing the direct costs of bodily intrusion and perceived loss of liberty. Healthcare systems are also beneficiaries, as mandates reduce strain on their resources. Constitutional courts act as observers, mediating the tension.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling legitimate public health interventions as pure extraction by emphasizing the genuine coordination problem (protecting the vulnerable) that mandates address. However, it risks overlooking the potential for mandates to become overly extractive if the 'serious harm' threshold is lowered or if less restrictive alternatives are ignored, which is where the 'proportionality_reading' sibling constraint would offer a different lens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_of_serious_harm,
    'What objective criteria define ''serious harm'' to vulnerable populations, and who determines this threshold?',
    'Consensus among independent epidemiological and medical ethics bodies, codified into public health law with clear, data-driven triggers for mandate implementation.',
    'If the threshold is too low or subjectively defined, mandates could be overused, increasing extraction from the unvaccinated. If too high, vulnerable populations could be left unprotected. Resolution would clarify the legitimate scope of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_of_serious_harm, conceptual, 'Ambiguity in defining the ''serious harm'' that justifies mandates.').

omega_variable(
    efficacy_of_less_restrictive_alternatives,
    'Are there less restrictive public health interventions (e.g., masking, testing, contact tracing) that could achieve comparable protection for vulnerable populations without compelling vaccination?',
    'Empirical studies comparing the effectiveness of various interventions in different epidemiological contexts, with transparent data and peer review.',
    'If effective alternatives exist, the necessity of mandates (and thus their legitimacy under this reading) is weakened, potentially reducing the constraint''s justified extractiveness. This would push the classification closer to a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_of_less_restrictive_alternatives, empirical, 'Uncertainty about the necessity of mandates over less restrictive measures.').

omega_variable(
    reading_bodily_autonomy_primary_delta,
    'How would the structural properties of this constraint change if the ''bodily_autonomy_primary'' reading were adopted?',
    'Analysis of legal frameworks and public health outcomes in jurisdictions that prioritize individual bodily autonomy over collective health mandates.',
    'The ''bodily_autonomy_primary'' reading would likely shift the victim set to include vulnerable populations (due to lack of protection) and reduce the extractiveness from unvaccinated individuals. The constraint itself might cease to exist or be reclassified as a Mountain (natural right to autonomy) or Rope (voluntary coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_bodily_autonomy_primary_delta, conceptual, 'Impact of an alternative reading prioritizing bodily autonomy.').

omega_variable(
    reading_proportionality_reading_delta,
    'How would the structural properties of this constraint change if the ''proportionality_reading'' were adopted?',
    'Analysis of legal frameworks and public health outcomes in jurisdictions that apply a strict proportionality test to mandates.',
    'The ''proportionality_reading'' would introduce dynamic thresholds for extractiveness and suppression, making the constraint''s legitimacy contingent on ongoing assessment of disease severity, vaccine efficacy, and alternative availability. This would likely reduce the average extractiveness and suppression over time, pushing the constraint closer to a Scaffold or a more balanced Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_proportionality_reading_delta, conceptual, 'Impact of an alternative reading prioritizing proportionality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t1900, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(mand_tr_t1950, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(mand_tr_t2000, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2000, 0.08).
narrative_ontology:measurement(mand_tr_t2010, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(mand_tr_t2020, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(mand_tr_t2024, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t1900, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(mand_be_t1950, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(mand_be_t2000, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(mand_be_t2010, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(mand_be_t2020, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(mand_be_t2024, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t1900, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(mand_su_t1950, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(mand_su_t2000, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(mand_su_t2010, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(mand_su_t2020, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement(mand_su_t2024, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('public_health_primary') of the 'mandate_legitimacy_scope' kernel. Its structural properties are distinct from sibling readings ('bodily_autonomy_primary', 'proportionality_reading') which emphasize different ethical priorities and would yield different extractiveness and suppression profiles. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
