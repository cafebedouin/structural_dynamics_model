% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary Legitimacy Reading
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the public_health_primary reading of the
 *   legitimate_health_intervention kernel. In this reading, state and
 *   institutional authority to mandate medical intervention derives entirely
 *   from measurable reductions in population-level morbidity and mortality.
 *   Individual refusal is framed as a negative externality imposed on
 *   vulnerable others, justifying coercive enforcement such as employment
 *   termination and access restrictions. The structural delta from sibling
 *   readings is sharp: unvaccinated individuals enter the victim set as
 *   disease vectors rather than rights-holders, while immunocompromised
 *   populations are beneficiaries of coordinated protection. The coordination
 *   functionâherd immunity and vulnerable protectionâis genuine, but the
 *   enforcement mechanisms produce high extractiveness from the constrained
 *   refusal population.
 *
 * KEY AGENTS:
 *   - public_health_authority: Agenda-setter (institutional/constrained) â designs mandates and enforces via legal orders
 *   - immunocompromised_population: Beneficiary (powerless/trapped) â receives protection but does not control the mechanism
 *   - unvaccinated_individuals: Primary target/payer (moderate/constrained) â bear employment and access costs of refusal
 *   - employers: Secondary payer (organized/constrained) â compelled to enforce verification and termination
 *   - civil_liberties_organizations: Observer (organized/analytical) â contest the framework in courts and public discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.65).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.68).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary Legitimacy Reading").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '12503239-778c-498b-b343-ea259dbac0fe').
narrative_ontology:cs_kernel_codification('12503239-778c-498b-b343-ea259dbac0fe', formalized).
narrative_ontology:cs_authority_grounding('12503239-778c-498b-b343-ea259dbac0fe', expertise).
narrative_ontology:cs_interpretation_layer_present('12503239-778c-498b-b343-ea259dbac0fe').
narrative_ontology:cs_reading_relation('12503239-778c-498b-b343-ea259dbac0fe', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('12503239-778c-498b-b343-ea259dbac0fe', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('12503239-778c-498b-b343-ea259dbac0fe', foundational, population_morbidity_reduction_legitimacy_source).
narrative_ontology:cs_axiom_status(population_morbidity_reduction_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('12503239-778c-498b-b343-ea259dbac0fe', population_morbidity_reduction_legitimacy_source, empirically_contingent).
narrative_ontology:cs_axiom('12503239-778c-498b-b343-ea259dbac0fe', foundational, refusal_as_externality_imposition).
narrative_ontology:cs_axiom_status(refusal_as_externality_imposition, holdable).
narrative_ontology:cs_axiom_grounding('12503239-778c-498b-b343-ea259dbac0fe', refusal_as_externality_imposition, instrumental).
narrative_ontology:cs_reference_frame('12503239-778c-498b-b343-ea259dbac0fe', herd_immunity_utilitarian_state).
narrative_ontology:cs_drift_state('12503239-778c-498b-b343-ea259dbac0fe', post_mandate_challenge_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('12503239-778c-498b-b343-ea259dbac0fe', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces vaccination mandates through emergency public health orders, justifies interventions via epidemiological models and population mortality reduction metrics, and faces political and legal pushback from civil society and courts.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authority, agenda_setter,
    institutional, generational, constrained, national).

% Cannot mount effective immune responses to certain pathogens and depend on high community vaccination coverage for protection; they benefit from reduced transmission but bear no direct enforcement costs and cannot exit their biological vulnerability.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_population, beneficiary,
    powerless, biographical, trapped, local).

% Face employment termination, exclusion from public accommodations, and social sanction for refusing vaccination; they are framed by authorities as imposing negative externalities on vulnerable populations and experience the constraint as coerced bodily compliance or material deprivation.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, national).

% Are mandated to verify employee vaccination status, administer termination for non-compliance, and absorb legal and operational costs; they are caught between public health orders and labor relations without recourse to opt out.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, employers, payer,
    organized, biographical, constrained, national).

% Challenge mandates in courts and public discourse on informed consent and bodily integrity grounds; they represent refused individuals but are structurally sidelined in emergency policy formulation rooms where utilitarian epidemiological calculus dominates.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, civil_liberties_organizations, observer,
    organized, generational, analytical, national).

narrative_ontology:fixing_cost_class(legitimate_health_intervention__public_health_primary, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reduces population-level infectious disease morbidity and mortality by achieving high vaccination coverage, thereby protecting immunologically vulnerable individuals who cannot be directly immunized.
% TRANSFER_FUNCTION: Moves compliance burden, medical intervention, documentation costs, and termination risk from unvaccinated individuals and enforcing employers to public health outcomes in the form of reduced transmission and protected vulnerable populations.
% ABSENT_VOICES: Unvaccinated individuals facing employment termination are formally heard in litigation but excluded from policy design; bodily autonomy advocates and consent-based ethicists are sidelined in emergency-policymaking rooms where utilitarian epidemiological calculus dominates.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, vaccination decisions would shift to individual risk assessment and informed consent; immunocompromised populations would face altered risk environments; employer human-resources departments would reorganize away from medical compliance enforcement; and the legal doctrine of police powers would contract.
% FOUNDING_PROBLEM: Contagious disease with significant mortality and morbidity that spreads via human transmission, creating a collective-action problem where individual refusal imposes infection risk on vulnerable others who cannot protect themselves.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health institutions attest the problem is live and justify mandates by case and mortality counts. Civil liberties organizations and some labor economists attest the problem is either resolved through endemic management or does not justify the coercion level employed. Courts provide mixed corroboration, often upholding mandates while explicitly noting the contested proportionality.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.65, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.65) is high because the constraint moves substantial compliance costs onto unvaccinated individuals and enforcing employers through state power. Suppression (0.68) is high because persistence depends on actively excluding unvaccinated individuals from workplaces and public spaces. Theater ratio (0.48) is moderate-to-high: while epidemiological benefit is partially real, a growing share of enforcement activity performs solidarity and compliance theater rather than calibrated risk reduction. Accessibility collapse (0.62) reflects that alternatives such as regular testing, natural immunity accommodation, and remote work were partially collapsed by policy design. Resistance (0.70) captures active legal challenges, political mobilization, and non-compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the public health authority seat, the constraint is legitimate coordination that solves a collective-action problem; from the unvaccinated seat, it is coerced extraction that instrumentalizes their bodies for others' benefit. The immunocompromised seat experiences a subsidized safety that it cannot exit, while the employer seat experiences unfunded regulatory burden. These divergences are structurally derived: the same mandate reads as protection, coercion, or cost depending on position.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (immunocompromised_population) derive low directionality: they receive protection without bearing enforcement costs, and their trapped exit is biological rather than imposed by the constraint. Victims (unvaccinated_individuals) derive high directionality: they are the direct targets of employment and access restrictions, with constrained exit options that penalize refusal. Employers have moderate directionality as secondary payers compelled to administer the extraction. The public health authority sits near the beneficiary pole because it sets the agenda without personal cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not a pure snare because the coordination functionâreducing population morbidity and protecting the immunocompromisedâis structurally genuine and not merely cover. It is not a pure rope because the enforcement mechanisms (termination, exclusion) create identifiable victims who pay asymmetric costs. It is not a piton because the agenda-setter actively profits in authority and budget from maintenance, and the theater ratio, while significant, does not indicate an atrophied function. The tangled_rope classification captures the hybrid: real coordination riding on coercive extraction from a targeted minority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    externality_imputation_validity,
    'Is individual vaccine refusal structurally analogous to negative externalities like pollution, or does the bodily intrusion required for compliance make it a categorically different refusal right that cannot be analogized to environmental harms?',
    'Comparative legal analysis across jurisdictions and empirical study of whether refusers conceptualize their choice as autonomy-protection or free-riding.',
    'If refusal is not a standard externality, the public_health_primary reading loses its foundational analogy and slides toward pure coercion; if it is, the coordination function is partially vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_imputation_validity, conceptual, 'Whether the externality framing of vaccine refusal is structurally valid or a category error.').

omega_variable(
    enforcement_proportionality_uncertainty,
    'Does the marginal population health gain from mandate enforcement justify the extraction imposed on unvaccinated individuals, or does enforcement reach diminishing returns where coercion exceeds epidemiological benefit?',
    'High-quality natural experiments comparing jurisdictions with mandates versus persuasion-only campaigns, controlling for baseline health infrastructure.',
    'If marginal gain is negligible, the constraint is extraction-heavy coordination-light; if substantial, it remains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_proportionality_uncertainty, empirical, 'Whether enforcement generates proportional epidemiological returns.').

omega_variable(
    reading_kernel_contest,
    'This constraint is one reading of the legitimate_health_intervention kernel. The bodily_autonomy_primary reading would reclassify the unvaccinated as rights-holders rather than vectors. Does the public_health_primary reading foreclose bodily autonomy or merely coexist in a different institutional framework?',
    'Constitutional court rulings that either subordinate public health to bodily integrity or subordinate individual refusal to police powers.',
    'If courts adopt the bodily autonomy reading, this constraint''s victim set empties and its classification shifts toward snare or piton; if public health primary is upheld, the tangled rope classification stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Structural relationship between this kernel reading and the bodily autonomy sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t12, legitimate_health_intervention__public_health_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t18, legitimate_health_intervention__public_health_primary, theater_ratio, 18, 0.38).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t24, legitimate_health_intervention__public_health_primary, theater_ratio, 24, 0.45).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t30, legitimate_health_intervention__public_health_primary, theater_ratio, 30, 0.5).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_tr_t36, legitimate_health_intervention__public_health_primary, theater_ratio, 36, 0.48).

% Extraction over time
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t12, legitimate_health_intervention__public_health_primary, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t18, legitimate_health_intervention__public_health_primary, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t24, legitimate_health_intervention__public_health_primary, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t30, legitimate_health_intervention__public_health_primary, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_be_t36, legitimate_health_intervention__public_health_primary, base_extractiveness, 36, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t12, legitimate_health_intervention__public_health_primary, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t18, legitimate_health_intervention__public_health_primary, suppression_requirement, 18, 0.82).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t24, legitimate_health_intervention__public_health_primary, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t30, legitimate_health_intervention__public_health_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(legitimate_health_intervention__public_health_primary_su_t36, legitimate_health_intervention__public_health_primary, suppression_requirement, 36, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, legitimate_health_intervention__proportionality_reading).

% DUAL FORMULATION NOTE:
% The legitimate_health_intervention kernel decomposes into at least three structurally distinct constraints. The public_health_primary reading treats legitimacy as flowing from population-level morbidity reduction and frames refusal as externality; it has high extractiveness and a victim set of unvaccinated individuals. The bodily_autonomy_primary reading would reclassify those same individuals as rights-holders and empty the victim set. The proportionality_reading would redistribute directionality by weighting disease severity. These are not the same constraint viewed differently; they have different epsilon profiles, beneficiary structures, and enforcement requirements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
