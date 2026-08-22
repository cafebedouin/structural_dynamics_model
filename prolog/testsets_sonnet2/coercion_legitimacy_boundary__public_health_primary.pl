% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public-Health-Primacy Reading of State Compulsory Medical Intervention
 *   domain: public_health/constitutional_law/medical_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the public-health-primacy reading of the
 *   coercion legitimacy boundary: the state may compel medical intervention
 *   when its calculus finds collective harm-prevention outweighs individual
 *   bodily autonomy. Under this reading, unvaccinated individuals and
 *   objectors are the coerced subjects (victims of the enforcement
 *   apparatus), while the immunocompromised and immunization-dependent
 *   infants are protected beneficiaries whose safety the compulsion exists to
 *   secure. This is a high-enforcement, high-extraction reading: the
 *   mandate's authority rests on aggregate epidemiological benefit, and its
 *   enforcement apparatus (exclusion, fines, compelled compliance) has
 *   hardened over time as exemption pathways narrowed and compliance
 *   monitoring intensified.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.68).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.79).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public-Health-Primacy Reading of State Compulsory Medical Intervention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health/constitutional_law/medical_ethics").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '99510fb8-849a-4c79-aafc-36b1c8cefd6a').
narrative_ontology:cs_kernel_codification('99510fb8-849a-4c79-aafc-36b1c8cefd6a', distributed).
narrative_ontology:cs_authority_grounding('99510fb8-849a-4c79-aafc-36b1c8cefd6a', distributed).
narrative_ontology:cs_reading_relation('99510fb8-849a-4c79-aafc-36b1c8cefd6a', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('99510fb8-849a-4c79-aafc-36b1c8cefd6a', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('99510fb8-849a-4c79-aafc-36b1c8cefd6a', foundational, collective_harm_prevention_overrides_individual_consent).
narrative_ontology:cs_axiom_status(collective_harm_prevention_overrides_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('99510fb8-849a-4c79-aafc-36b1c8cefd6a', collective_harm_prevention_overrides_individual_consent, instrumental).
narrative_ontology:cs_axiom('99510fb8-849a-4c79-aafc-36b1c8cefd6a', secondary, state_police_power_extends_to_bodily_integrity_when_transmission_risk_exists).
narrative_ontology:cs_axiom_status(state_police_power_extends_to_bodily_integrity_when_transmission_risk_exists, holdable).
narrative_ontology:cs_axiom_grounding('99510fb8-849a-4c79-aafc-36b1c8cefd6a', state_police_power_extends_to_bodily_integrity_when_transmission_risk_exists, conventional).
narrative_ontology:cs_reference_frame('99510fb8-849a-4c79-aafc-36b1c8cefd6a', police_power_collective_harm_doctrine).
narrative_ontology:cs_drift_state('99510fb8-849a-4c79-aafc-36b1c8cefd6a', contemporary_post_pandemic_polarization, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('99510fb8-849a-4c79-aafc-36b1c8cefd6a', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, herd_immunity_dependent_infants).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, vaccine_injured_minority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, employers_and_schools).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, collective_harm_prevention_supremacy_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__public_health_primary, state_police_power_over_bodily_integrity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets mandate policy, defines compliance thresholds (school entry, employment, travel), and enforces via fines, exclusion, or civil penalties. Draws legitimacy from epidemiological modeling and collective-harm framing. Administers exemption categories narrowly and bears no personal cost from the compulsion it authors.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, beneficiary).

% Face fines, exclusion from schools or workplaces, or direct compulsion under this reading's framework. Their refusal — whether from medical contraindication, distrust, religious conviction, or informed risk calculation — is treated as a harm vector to be corrected rather than a legitimate exercise of bodily autonomy. Exit requires relocating to a jurisdiction without the mandate, an option foreclosed for most by economic and social ties.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    powerless, biographical, trapped, national).

% Hold sincerely-grounded objections that this reading treats as insufficient to override collective harm-prevention. Narrow or eliminated exemption pathways mean their objection functions as a cost center (fines, exclusion) rather than a protected liberty interest. Some relocate to accommodate; most absorb the cost of noncompliance or comply under duress.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, constrained, national).

% The small population that experiences genuine adverse reactions bears an individualized harm that this reading's aggregate-benefit calculus does not weight proportionally to their loss. Compensation schemes, where they exist, are administratively slow and cap recovery; the harm is real but statistically invisible at the population level this reading privileges.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, vaccine_injured_minority, payer,
    powerless, biographical, trapped, national).

% Cannot be vaccinated themselves or achieve adequate immune response, and depend entirely on population-level immunity maintained by others' compulsory compliance. Under this reading they exit the victim set entirely — the mandate exists structurally for their protection, and its removal would directly increase their mortality risk. They have no exit from their biological vulnerability and no capacity to generate the protection themselves.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, national).

% Too young for vaccination against certain diseases and protected only by population immunity maintained through compulsory compliance of others. Cannot consent, object, or exit; their protection is a pure downstream effect of the mandate's enforcement against the payer seats.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, herd_immunity_dependent_infants, beneficiary,
    powerless, immediate, trapped, national).

% Adjudicate the boundary between state police power and bodily autonomy, hearing challenges from objectors and defenses from health authorities. Their rulings shift the enforcement apparatus's scope but do not resolve the underlying kernel contest — they operate within whichever reading the presiding doctrine currently favors.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, courts_and_civil_liberties_litigators, observer,
    institutional, generational, analytical, national).

% Serve as the front-line enforcement layer, conditioning attendance or employment on compliance. They absorb administrative and legal costs of enforcement and occasional litigation, while having limited independent power to set the underlying mandate — they implement policy set above them.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, employers_and_schools, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__public_health_primary, employers_and_schools, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__public_health_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__public_health_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents transmission of communicable disease within a population by raising vaccination rates above the epidemiological threshold needed for herd immunity, protecting those who cannot be protected by their own immune response or choice.
% TRANSFER_FUNCTION: Moves bodily-autonomy discretion from the individual to the state, and moves protection from population-level risk from the general population to the specific subpopulations (immunocompromised, infants) who cannot generate that protection themselves — at the cost of compelled intervention, fines, or exclusion imposed on those who would otherwise decline.
% ABSENT_VOICES: Unvaccinated individuals with sincere medical, religious, or philosophical objections are structurally present as payers but not as co-authors of the exemption criteria; their objections are processed as inputs to a harm-minimization calculus they do not participate in designing. Vaccine-injured individuals are statistically acknowledged but rarely individually heard in the aggregate-benefit framing this reading employs.
% DISAPPEARANCE_RATIONALE: Public health authorities and the immunocompromised/infant beneficiary seats would say the world rearranges sharply — transmission rates rise, herd immunity erodes, vulnerable populations face elevated mortality. Objector and unvaccinated seats would say the world simply returns to a baseline of voluntary choice that this reading treats as intolerable but that has historically been the norm outside acute epidemic periods. The two camps dispute which baseline is the relevant one.
% FOUNDING_PROBLEM: Communicable disease outbreaks (smallpox, polio, measles) caused mass mortality and disability in populations where voluntary vaccination rates fell below the threshold needed for population-level protection, and unprotected individuals could not be identified or isolated fast enough to prevent transmission.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and immunocompromised advocacy groups attest the founding problem remains live, citing measles resurgence data and immunocompromised mortality risk from vaccine-preventable disease outbreaks. Independent civil liberties scholars and some public health ethicists — outside the direct beneficiary set — attest that for many currently-mandated interventions the disease burden has shifted enough that the original acute-crisis justification no longer matches the current mandate's scope, without disputing that it once did.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, contested).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the compelled transfer of bodily-autonomy discretion from objecting individuals to the state, discounted somewhat because the reading has a genuine, non-fabricated coordination function (herd immunity is a real epidemiological property, not a pretext). Suppression (0.79) is high and rising because this reading's legitimacy depends on active enforcement — narrowed exemptions, exclusion from institutions, fines — not on voluntary uptake; suppression is authored as the raw enforcement intensity, not scaled by scope. Theater (0.28) stays moderate-low because most enforcement activity targets genuine transmission-reduction rather than performative compliance theater, though it rises modestly as exemption paperwork and compliance bureaucracy accumulate.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health authorities), this looks like coordination: a genuine collective-action problem (herd immunity) solved through necessary compulsion. From the payer seats (unvaccinated individuals, objectors), the same structure operates as enforced extraction of bodily discretion with narrowing avenues for legitimate dissent. The engine should compute a tangled_rope from the aggregate structural data — real coordination function (protecting the immunocompromised, the infants) coexisting with genuine asymmetric extraction (compelled intervention on objecting adults) requiring active enforcement to hold. This divergence between agenda-setter and payer experience is the seat divergence this reading is built to reveal, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and employers/schools sit near the agenda-setter/beneficiary end — they administer the mandate and are largely insulated from its direct costs. Unvaccinated individuals, religious/philosophical objectors, and the vaccine-injured minority are the targets: trapped or constrained exit, bearing fines, exclusion, or uncompensated injury. Immunocompromised individuals and herd-immunity-dependent infants are pure beneficiaries under this reading — they collect the protection without bearing any of the compulsion's direct cost, and their vulnerability is precisely what legitimizes the compulsion in this reading's own terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled epidemic mortality) remains partially live for some pathogens (measles) but is contested for others where disease burden has shifted substantially since mandate design (e.g., certain flu-season workplace mandates). Because founding_problem_status is authored as 'contested' rather than 'dead,' this does not trigger a mandatrophy flag outright, but the corroboration trail (independent ethicists noting mandate scope has outrun the acute-crisis justification for some interventions) is exactly the kind of outside-the-beneficiary-set attestation that would support a future mandatrophy finding if disease burden continues to decline while enforcement infrastructure (suppression_requirement) continues to intensify — the diverging trajectory between the two measured series (rising suppression against contested founding-problem persistence) is the pattern to watch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_benefit_versus_individual_harm_weighting,
    'How should the aggregate epidemiological benefit to the population be weighed against the concentrated, individualized harm borne by the vaccine-injured minority and the liberty harm borne by sincere objectors?',
    'No empirical resolution exists — this is a normative weighting question. Comparative analysis of how different coercion-legitimacy frameworks (bodily_autonomy_primary, proportionality_reading) treat the same underlying case data would clarify the range of defensible weightings but would not resolve which weighting is correct.',
    'If individualized harm is weighted more heavily (as in bodily_autonomy_primary), this reading''s high extractiveness score would be judged illegitimate extraction rather than justified coordination cost. If aggregate benefit is weighted as this reading does, the same extraction is judged a necessary and proportionate cost of population protection.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(aggregate_benefit_versus_individual_harm_weighting, preference, 'Normative disagreement over aggregate-benefit versus individual-harm weighting that the kernel''s readings resolve differently.').

omega_variable(
    founding_problem_currency_by_pathogen,
    'For which specific mandated interventions is the founding problem (epidemic mortality absent compulsion) still live, versus already substantially resolved by the initial rounds of the mandate itself?',
    'Pathogen-specific epidemiological review comparing current transmission and mortality data against pre-mandate baselines, disaggregated by intervention rather than treated as a single undifferentiated mandate.',
    'For pathogens where the founding problem has substantially resolved, continued high-suppression enforcement would indicate mandatrophy (the constraint has outlived its founding function but retains its enforcement apparatus); for pathogens where transmission risk remains acute, the same enforcement would remain justified under this reading''s own terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_currency_by_pathogen, empirical, 'Whether the founding problem''s continued liveness varies by specific pathogen/intervention rather than holding uniformly across the mandate.').

omega_variable(
    sibling_reading_selection_mechanism,
    'What determines which of the three kernel readings (public_health_primary, bodily_autonomy_primary, proportionality_reading) a given court, legislature, or health authority adopts in a specific case?',
    'Comparative jurisprudential analysis across jurisdictions and time periods to identify whether reading-selection correlates with disease severity, political composition of the deciding body, or historical precedent lock-in.',
    'If reading-selection is driven primarily by disease severity in practice (even without an explicit proportionality doctrine), this reading''s practical operation may already resemble the proportionality_reading more than its stated premise admits — narrowing the actual structural delta between the two constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_selection_mechanism, conceptual, 'How courts and authorities actually select among the three kernel readings in practice, and whether stated doctrine matches applied doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 8, 0.13).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 16, 0.17).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 24, 0.21).
narrative_ontology:measurement(coer_tr_t32, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 32, 0.25).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 24, 0.63).
narrative_ontology:measurement(coer_be_t32, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(coer_su_t32, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__bodily_autonomy_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the coercion_legitimacy_boundary kernel. bodily_autonomy_primary holds compelled intervention is categorically impermissible regardless of collective benefit — its core premise directly forecloses this reading's aggregate-benefit override. proportionality_reading holds legitimacy scales with disease severity — it coexists with and structurally influences this reading by supplying the severity-scaling logic this reading's aggregate calculus could adopt as a refinement without abandoning its core premise. All three stories share the underlying compulsory-vaccination arrangement as their referent but author different beneficiary/victim sets and different ε because each reading answers the legitimacy question differently, per the ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
