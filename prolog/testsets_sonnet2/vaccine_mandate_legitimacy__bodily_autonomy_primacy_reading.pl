% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Vaccine Mandate Regime Under the Bodily-Autonomy-Primacy Reading
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested
 *   vaccine_mandate_legitimacy kernel: the bodily-autonomy-primacy reading,
 *   under which medical self-sovereignty is treated as an absolute right and
 *   state coercion to vaccinate is categorically impermissible regardless of
 *   the outcome of non-vaccination. Under this reading's own lights, the
 *   standing arrangement under contest is the existing (or proposed) mandate
 *   regime itself, evaluated as an impermissible intrusion. The reading's
 *   structural delta from its siblings is specific: it draws the
 *   immunocompromised and other medically vulnerable people into the victim
 *   set (as bearers of elevated exposure risk the categorical rule does not
 *   weigh), and it identifies liberty advocacy movements and vaccine-hesitant
 *   political organizers as the concentrated beneficiaries who gain doctrinal
 *   and political capital from the categorical framing succeeding. This is
 *   not a story about whether mandates are good policy; it is a story about
 *   what follows, structurally, once the autonomy-primacy premise is adopted
 *   as the operative legitimacy standard.
 *
 * KEY AGENTS:
 *   - unvaccinated_individuals_facing_mandate_penalties: primary payer under mandate enforcement, powerless/constrained
 *   - immunocompromised_and_medically_vulnerable_populations: secondary payer, bears elevated exposure risk, powerless/trapped
 *   - essential_workers_under_employer_mandates: dual-positioned payer, moderate/constrained
 *   - liberty_advocacy_movements: primary beneficiary and agenda-setter, organized/mobile
 *   - vaccine_hesitant_political_organizers: beneficiary, organized/mobile
 *   - state_public_health_authorities: agenda-setter whose toolkit is constrained by this reading, institutional/constrained
 *   - courts_and_legislatures: analytical observer adjudicating between kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.68).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.72).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Vaccine Mandate Regime Under the Bodily-Autonomy-Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'f4eb988d-f351-4a28-b081-a9849490d6e4').
narrative_ontology:cs_kernel_codification('f4eb988d-f351-4a28-b081-a9849490d6e4', distributed).
narrative_ontology:cs_authority_grounding('f4eb988d-f351-4a28-b081-a9849490d6e4', distributed).
narrative_ontology:cs_reading_relation('f4eb988d-f351-4a28-b081-a9849490d6e4', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f4eb988d-f351-4a28-b081-a9849490d6e4', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('f4eb988d-f351-4a28-b081-a9849490d6e4', foundational, bodily_integrity_categorically_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_categorically_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('f4eb988d-f351-4a28-b081-a9849490d6e4', bodily_integrity_categorically_inviolable, deontological).
narrative_ontology:cs_axiom('f4eb988d-f351-4a28-b081-a9849490d6e4', secondary, state_health_coercion_never_outcome_justified).
narrative_ontology:cs_axiom_status(state_health_coercion_never_outcome_justified, holdable).
narrative_ontology:cs_axiom_grounding('f4eb988d-f351-4a28-b081-a9849490d6e4', state_health_coercion_never_outcome_justified, deontological).
narrative_ontology:cs_reference_frame('f4eb988d-f351-4a28-b081-a9849490d6e4', informed_consent_doctrine_post_nuremberg).
narrative_ontology:cs_drift_state('f4eb988d-f351-4a28-b081-a9849490d6e4', post_covid19_mandate_litigation_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f4eb988d-f351-4a28-b081-a9849490d6e4', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_political_organizers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_individuals_facing_mandate_penalties).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_and_medically_vulnerable_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, essential_workers_under_employer_mandates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_authorities).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_integrity_as_categorical_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face job loss, exclusion from travel, school, or public accommodations, or direct fines for declining vaccination. Under this reading, the mandate treats a bodily decision as a compliance obligation; their exit options are relocating jurisdictions, accepting economic exclusion, or submitting under duress they experience as coercion rather than consent.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_individuals_facing_mandate_penalties, payer,
    powerless, biographical, constrained, national).

% Cannot themselves be vaccinated or rely on herd-level protection that a bodily-autonomy-primacy regime erodes by permitting widespread non-vaccination as a matter of right. This reading's insistence on categorical non-coercion, while protecting the autonomy of the unvaccinated, structurally raises the ambient exposure risk this population bears; they have no exit from the shared risk pool and no voice that outweighs the autonomy claim within this reading's own framework.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_and_medically_vulnerable_populations, payer,
    powerless, biographical, trapped, national).

% Employed in healthcare, transit, or congregate-care settings where employer or state mandates condition continued employment on vaccination. Under this reading their situation is doubly fraught: they are named as payers of the autonomy violation if mandated, yet many also belong to the vulnerable population bearing exposure risk if mandates are struck down — the reading resolves this tension by treating employment conditioning as coercion regardless of the setting's risk profile.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, essential_workers_under_employer_mandates, payer,
    moderate, biographical, constrained, national).

% Organize litigation, legislative lobbying, and public messaging campaigns around bodily-sovereignty doctrine, using vaccine mandates as the paradigm case to establish categorical precedent against state health coercion generally. They gain political capital, membership growth, and doctrinal wins each time a court or legislature accepts the categorical framing, independent of the mandate's actual public-health merits in any specific case.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, agenda_setter).

% Political actors and media figures who monetize and mobilize around vaccine skepticism find in the bodily-autonomy-primacy reading a durable, principled-sounding vocabulary that outlasts any single mandate controversy; they benefit from the reading's persistence as a rhetorical and legal resource regardless of the underlying epidemiology.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_hesitant_political_organizers, beneficiary,
    organized, biographical, mobile, national).

% Administer and attempt to justify mandates within a legal environment this reading actively works to constrain; each successful autonomy-primacy ruling narrows their available policy instruments and shifts enforcement burden onto softer mechanisms (incentives, information campaigns) whose efficacy against the vulnerable-population exposure problem is lower.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_authorities, payer).

% Adjudicate between the competing kernel readings, weighing categorical bodily-autonomy claims against public-health-primacy and risk-stratification arguments; their rulings determine which reading gains formal legal force at a given moment and jurisdiction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Within this reading, the arrangement coordinates a shared normative commitment among rights-oriented citizens and advocacy networks: that medical decisions remain within individual control regardless of collective epidemiological stakes, providing a stable, generalizable doctrine usable across many future state-health-coercion disputes, not just vaccination.
% TRANSFER_FUNCTION: Moves exposure risk from the vaccine-declining population onto the immunocompromised and medically vulnerable population who cannot vaccinate and cannot rely on herd protection; moves political and reputational capital toward liberty advocacy organizations and vaccine-hesitant political organizers who use each contested mandate as a doctrinal battleground.
% ABSENT_VOICES: Immunocompromised individuals and other medically vulnerable people bearing the elevated exposure risk are rarely named parties in the constitutional and legislative fights fought under this reading's banner; their interests are typically represented, if at all, by public-health authorities arguing the opposing reading, not by anyone inside the autonomy-primacy coalition itself.
% DISAPPEARANCE_RATIONALE: If the bodily-autonomy-primacy reading vanished overnight, liberty advocacy movements would lose their principal doctrinal vehicle and public health authorities would regain a wider mandate toolkit — a significant rearrangement for those organized around it. But large numbers of ordinary people who are simply vaccine-hesitant without ideological commitment would likely experience little immediate change in their day-to-day reasoning, since their resistance often predates and could survive the doctrine's institutional articulation; hence the contested verdict.
% FOUNDING_PROBLEM: Historical experience of coercive and abusive state medical interventions (forced sterilization, non-consensual experimentation, quarantine abuses) that occurred without adequate individual consent or recourse, motivating a categorical rule against future state medical coercion.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists and historians of medicine outside the liberty-advocacy coalition corroborate that the founding problem (historical coercive medical abuse) was real and remains a legitimate cautionary concern; however, public health scholars and epidemiologists, also outside the beneficiary coalition, contest whether a categorical bar on all state health coercion is a proportionate response to that history or whether it has been extended well past the abuses that motivated it, into contexts (contagious disease control) structurally different from the abuses being guarded against.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68, reflecting substantial but not maximal extraction: the reading achieves real coordination (a stable, generalizable doctrine protecting against a documented historical harm class) while also transferring concrete exposure risk onto a population that did not consent to bear it and has no exit. Suppression is authored higher (0.72) because maintaining the categorical rule against contrary public-health evidence requires active litigation, political mobilization, and legislative defense — the doctrine does not sustain itself passively. Theater ratio is moderate-low (0.28) because the doctrinal work is largely genuine advocacy and litigation, not empty performance, though a growing share of activity (rising from 0.15 to 0.28) is symbolic mobilization once the core legal battles in many jurisdictions were substantially won. Accessibility collapse is authored at a middling 0.5: alternatives (targeted mandates, risk-stratified approaches) remain visible and contested rather than fully foreclosed, consistent with tangled_rope rather than mountain. Resistance is high (0.81) because public health authorities, epidemiologists, and vulnerable-population advocates actively contest the categorical framing.
 *
 * PERSPECTIVAL GAP:
 *   From the liberty advocacy seat, this arrangement is a hard-won doctrinal victory protecting against renewed state medical abuse. From the immunocompromised seat, the identical arrangement is an unconsented transfer of exposure risk they cannot avoid. The engine computes these divergent per-seat readings from the declared power/exit/scope data; this story does not adjudicate which seat is correct — it authors the structure that produces the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberty advocacy movements and vaccine-hesitant political organizers sit near the beneficiary end: organized, mobile, gaining durable political and legal capital from the reading's persistence regardless of any single mandate's epidemiological merits. Unvaccinated individuals facing penalties, immunocompromised populations, and essential workers sit near the target end: powerless-to-moderate power, constrained-to-trapped exit, bearing either direct compliance costs or elevated exposure risk with no adequate recourse within this reading's own framework. State public health authorities are agenda-setters but also structurally constrained payers here — the reading actively narrows their available instruments, which is an unusual but real dual position for an institutional actor.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents this reading from being flattened into either pure coordination (ignoring the exposure risk transferred onto the immunocompromised) or pure extraction (ignoring the genuine coordination function the doctrine serves in constraining historically documented coercive medical abuse). Both a coordination function and an asymmetric extraction are structurally present and both are required by the schema gate for this classification; treating the reading as a snare would erase its genuine historical grounding, while treating it as a rope would erase the diffuse cost it imposes on a population with no voice in its construction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_contextual_rights_framing,
    'Is bodily self-sovereignty properly a categorical (context-independent) right, or is its legitimate scope contingent on the presence of externalized harm to third parties (as the public-health-primacy and risk-stratification readings hold)?',
    'This is fundamentally a normative/jurisprudential question not resolvable by empirical data alone; partial resolution mechanisms include tracking how constitutional courts across multiple jurisdictions rule on analogous bodily-autonomy-vs-externality cases outside the vaccine context (e.g., communicable disease quarantine, mandatory reporting) to see whether categorical framing is applied consistently or selectively.',
    'If courts and legal scholarship consistently reject categorical framing outside politically salient vaccine contexts, this reading''s claim to principled consistency weakens considerably, supporting reclassification toward snare (extraction dressed as principle); if categorical framing is applied consistently across analogous cases, the coordination function is more robust and the tangled_rope classification''s coordination component gains support.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_contextual_rights_framing, conceptual, 'Whether bodily autonomy is a genuinely categorical right or a selectively-applied doctrine.').

omega_variable(
    kernel_reading_committer_structure,
    'Which of the three declared readings of the vaccine_mandate_legitimacy kernel (bodily_autonomy_primacy, public_health_primacy, risk_stratification) will courts and legislatures treat as authoritative in a given jurisdiction and period, and does that selection track principled reasoning or political composition of the adjudicating body?',
    'Track court composition, legislative majority, and public health emergency status across jurisdictions and time; compare ruling outcomes to see whether reading selection correlates with these factors independent of case-specific facts.',
    'If reading selection tracks political composition rather than case facts, none of the three readings function as a stable legal kernel and the entire mandate_legitimacy domain is better modeled as unsettled/distributed authority rather than any single reading having doctrinal primacy at a given time; this would not change this story''s own epsilon but would affect how much weight any single reading''s classification should carry in downstream policy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, empirical, 'Whether the sibling reading selected as authoritative tracks principle or political composition.').

omega_variable(
    immunocompromised_representation_gap,
    'Is the absence of immunocompromised and medically vulnerable people as direct, organized parties to bodily-autonomy-primacy litigation a structural exclusion (they have no standing or resources to intervene) or a genuine absence of felt stake (they do not, in fact, experience meaningfully elevated risk from this reading''s dominance)?',
    'Compare epidemiological exposure-risk data for immunocompromised populations in jurisdictions where this reading has prevailed versus jurisdictions where public-health-primacy or risk-stratification readings prevail, controlling for baseline vaccination rates and disease prevalence.',
    'If elevated risk is empirically confirmed and representation is confirmed absent, this strengthens the case that this reading''s victim declaration for immunocompromised populations is not merely rhetorical but empirically grounded, reinforcing the tangled_rope (not rope) classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_representation_gap, empirical, 'Whether immunocompromised exclusion from the debate reflects structural barrier or absence of stake.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.25).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 16, 0.65).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vaccine_mandate_legitimacy kernel (per the epsilon-invariance principle for kernel readings). public_health_primacy_reading treats the unvaccinated as an externality-generating class and authors mandate authority as legitimate coordination; risk_stratification_reading treats blanket mandates as disproportionate but targeted, actuarially-grounded mandates as legitimate. Each reading has its own epsilon, beneficiary/victim structure, and classification, and none should be averaged with the others. All three should be treated as members of one constraint family linked bidirectionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
