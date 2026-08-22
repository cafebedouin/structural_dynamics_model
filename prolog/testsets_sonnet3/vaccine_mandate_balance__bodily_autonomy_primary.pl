% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__bodily_autonomy_primary, []).

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
 *   constraint_id: vaccine_mandate_balance__bodily_autonomy_primary
 *   human_readable: State Vaccine Mandate as Coercion of Bodily Autonomy (Bodily-Autonomy-Primary Reading)
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   vaccine-mandate-balance kernel: a claim that individual consent to
 *   medical intervention is inviolable and that the state may not compel it
 *   regardless of the collective benefit asserted. Under this reading, the
 *   mandate apparatus that emerged during acute outbreak conditions is not a
 *   proportionate coordination device but a coercion mechanism whose victim
 *   set includes every individual compelled by economic or legal penalty to
 *   submit to an intervention they withheld consent from — and explicitly
 *   does NOT include immunocompromised or high-risk individuals exposed to
 *   unvaccinated contacts, because on this reading their exposure is a
 *   feature of a liberty-respecting world, not a harm imposed by the
 *   constraint. The public-health-primary and proportionality readings of the
 *   same underlying mandate episode are separate constraint stories with
 *   their own ε and victim sets; they are not blended here.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter (institutional/analytical) — designs and enforces the mandate
 *   - unvaccinated_coerced_individuals: primary victim (powerless/trapped) — bears direct coercive cost
 *   - religious_and_conscientious_objectors: victim (powerless/constrained) — narrow exemption pathway
 *   - vaccine_manufacturers, employers_seeking_liability_shield: beneficiaries — guaranteed demand, liability shield
 *   - immunocompromised_and_high_risk_individuals: excluded from victim ledger by this reading's own logic
 *   - civil_liberties_and_bodily_autonomy_advocates: observer seat articulating the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.78).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "State Vaccine Mandate as Coercion of Bodily Autonomy (Bodily-Autonomy-Primary Reading)").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, 'e36399fe-8978-4920-8f5f-709ba551ddc4').
narrative_ontology:cs_kernel_codification('e36399fe-8978-4920-8f5f-709ba551ddc4', distributed).
narrative_ontology:cs_authority_grounding('e36399fe-8978-4920-8f5f-709ba551ddc4', distributed).
narrative_ontology:cs_reading_relation('e36399fe-8978-4920-8f5f-709ba551ddc4', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('e36399fe-8978-4920-8f5f-709ba551ddc4', vaccine_mandate_balance__proportionality_reading, influences).
narrative_ontology:cs_axiom('e36399fe-8978-4920-8f5f-709ba551ddc4', foundational, consent_inviolability_absolute).
narrative_ontology:cs_axiom_status(consent_inviolability_absolute, holdable).
narrative_ontology:cs_axiom_grounding('e36399fe-8978-4920-8f5f-709ba551ddc4', consent_inviolability_absolute, deontological).
narrative_ontology:cs_axiom('e36399fe-8978-4920-8f5f-709ba551ddc4', secondary, collective_benefit_never_overrides_bodily_nonconsent).
narrative_ontology:cs_axiom_status(collective_benefit_never_overrides_bodily_nonconsent, holdable).
narrative_ontology:cs_axiom_grounding('e36399fe-8978-4920-8f5f-709ba551ddc4', collective_benefit_never_overrides_bodily_nonconsent, deontological).
narrative_ontology:cs_reference_frame('e36399fe-8978-4920-8f5f-709ba551ddc4', substantive_due_process_bodily_integrity).
narrative_ontology:cs_drift_state('e36399fe-8978-4920-8f5f-709ba551ddc4', post_pandemic_mandate_litigation_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e36399fe-8978-4920-8f5f-709ba551ddc4', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, employers_seeking_liability_shield).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, religious_and_conscientious_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, workers_terminated_for_noncompliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces mandate policy, framing compulsory vaccination as necessary to protect collective health outcomes. Sets exemption criteria, penalty structures, and compliance deadlines. Bears no direct bodily cost of the intervention it compels and collects legitimacy and funding from demonstrating compliance rates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Face employment termination, exclusion from public accommodations, travel restriction, or fines unless they submit to a medical intervention they have declined. Their refusal is treated as noncompliance rather than as an exercise of a right; exit requires either submission, relocation to a jurisdiction without the mandate, or absorbing severe economic and social penalty.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Hold sincerely held objections to the intervention on religious or conscience grounds. Exemption processes are narrow, discretionary, and frequently denied in practice; where exemptions exist on paper, the accessibility_collapse is only partial rather than complete, but the practical burden of securing one is severe.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, religious_and_conscientious_objectors, payer,
    powerless, biographical, constrained, national).

% Lost livelihoods for declining the intervention. From this reading, their termination is a direct transfer of harm from the state's compulsory apparatus onto individuals who withheld consent, regardless of what collective benefit the mandate purportedly produced.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, workers_terminated_for_noncompliance, payer,
    powerless, biographical, trapped, national).

% Receive guaranteed demand and often liability indemnification once a mandate converts a voluntary market into a compelled one. Their exposure to market risk is reduced precisely because consent has been structurally removed as a gating condition for their product's uptake.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    institutional, generational, arbitrage, global).

% Adopt and enforce mandates to shift workplace-transmission liability onto a state-sanctioned compliance standard rather than bearing the cost of individualized risk assessment. They administer terminations for noncompliance and benefit from the legal cover the mandate provides, while workers bear the termination cost directly.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, employers_seeking_liability_shield, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, employers_seeking_liability_shield, agenda_setter).

% Face elevated exposure risk from unvaccinated contacts in shared spaces. On this reading they are not counted among the constraint's victims: their heightened vulnerability is treated as a pre-existing feature of embodied life that liberty does not obligate others to neutralize through compelled intervention on third parties. Their objections are heard in public discourse but do not enter this reading's victim ledger.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_and_high_risk_individuals, excluded,
    powerless, biographical, constrained, national).

% Litigate against mandates, document coercion mechanisms, and articulate the autonomy-primary framework this reading instantiates. They do not bear the direct penalty but shape the doctrinal record from outside the compelled population.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, civil_liberties_and_bodily_autonomy_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate coordinates a uniform compliance standard so employers, institutions, and public agencies can rely on a single verifiable signal (vaccination status) instead of individualized risk assessment — a genuine administrative simplification, though this reading holds the simplification does not license compelling intervention on a nonconsenting body.
% TRANSFER_FUNCTION: Moves bodily decision-making authority from the individual to the state and its delegated enforcers (employers, venues, agencies); moves economic security (employment, access to public life) from noncompliant individuals to the compliance apparatus; moves market and liability risk from manufacturers and employers onto the state's coercive authority.
% ABSENT_VOICES: Unvaccinated individuals who complied under duress rather than genuine consent are rarely surveyed for their post-compliance attitudes; their acquiescence is recorded as agreement rather than coercion. Immunocompromised individuals who feel unprotected without mandates are heard in the public debate but, under this reading's own logic, are treated as bearing an inherent risk of social life rather than as constraint victims — this exclusion is a modeling choice specific to this reading, not an empirical claim that their fear is unfounded.
% DISAPPEARANCE_RATIONALE: If compulsory mechanisms vanished overnight, employment and access decisions would revert to voluntary uptake and individualized negotiation; terminated workers would be reinstatable claimants, exemption litigation would collapse for lack of a mandate to litigate against, and manufacturers would lose guaranteed-demand assurance — the coercive infrastructure is doing real structural work, not merely reflecting an underlying natural consensus.
% FOUNDING_PROBLEM: Voluntary uptake alone was judged insufficient to reach levels of immunity public health authorities believed necessary to suppress transmission and protect populations at elevated risk during acute outbreak conditions.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and employers attest the founding problem remains live wherever transmission risk is elevated. Civil liberties litigators, dissenting physicians, and several judicial opinions attest — from outside the beneficiary set — that the problem, even where once live, does not license the specific compulsory mechanism chosen, and that voluntary and incentive-based alternatives were available but not preferred by agenda-setters.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_balance__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.78 at peak) because the mandate, on this reading, extracts compliance through the direct threat of job loss, exclusion, and fine rather than through consent — the coercion mechanism IS the extraction. Suppression tracks the enforcement apparatus's intensity (hardening through mid-interval as mandates broadened, then relaxing as political and judicial resistance forced retreat) and is authored as its own raw structural quantity, not scaled by scope. Theater ratio is moderate and rising through the peak-enforcement window: some exemption processes and 'religious accommodation' review functioned more as procedural cover than genuine adjudication, then receded as mandates were rolled back. All three tracked metrics share one time grid at t=0,4,8,12,16,20,24 — no metric is asserted at a point another metric lacks.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health agencies) and the payer seats (unvaccinated coerced individuals, objectors, terminated workers) will compute to different types from the same structural data: the agenda-setter's analytical, arbitrage-adjacent exit and institutional power push its seat toward experiencing coordinated public benefit, while the payer seats' trapped/constrained exit and powerless standing push their seats toward experiencing pure coercive extraction. This divergence is exactly what the per-seat computation is supposed to surface, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies, vaccine manufacturers, and liability-shielded employers are declared beneficiaries: they collect compliance, guaranteed demand, or reduced liability exposure without bearing the compelled intervention themselves — low d. Unvaccinated-coerced individuals, objectors, and terminated workers are declared victims: they bear the intervention, the penalty, or the termination directly, with trapped or narrowly constrained exit — high d. Immunocompromised individuals are deliberately NOT declared victims under this reading, consistent with the reading's own axiom that liberty does not generate an enforceable claim against others' bodily refusal — their omission from the victims array is a structural commitment of this specific reading, not an oversight.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pairing is authored as contested rather than resolved: agencies attest the outbreak-suppression problem was live and, in places, remains live; outside corroborators (courts, civil liberties litigators, dissenting physicians) attest that even a once-live problem does not license the specific compulsory mechanism chosen. This keeps the story from either romanticizing the mandate as pure coordination or dismissing it as pure theater — the classification records a snare with a genuine (if contested) coordination predicate underneath it, which is why requires_active_enforcement and a documented beneficiary/victim split are both present rather than treating this as simple theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_inviolability_vs_third_party_harm,
    'Does bodily autonomy remain absolute when non-intervention imposes elevated risk on third parties who cannot themselves reduce that risk (e.g., immunocompromised individuals in shared public spaces), or does the inviolability claim hold only within a purely self-regarding harm model?',
    'Doctrinal analysis of harm-principle case law across jurisdictions that have adjudicated compelled-intervention challenges, cross-referenced with epidemiological transmission-risk data attributing third-party harm specifically to non-vaccination versus baseline risk.',
    'If courts and the framework converge on a third-party-harm exception, immunocompromised-exposed individuals would migrate into a partial-victim status even under this reading, converging the bodily-autonomy-primary and proportionality readings at the margin. If the self-regarding model holds firm, the exclusion of immunocompromised individuals from this reading''s victim set is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_inviolability_vs_third_party_harm, conceptual, 'Whether bodily autonomy is absolute or bounded by demonstrable third-party transmission risk.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the choice of bodily-autonomy-primary as the operative reading itself contestable within a single legal or ethical framework, or do different frameworks (constitutional liberty jurisprudence vs. public-health police-power jurisprudence) simply hold genuinely incompatible premises that cannot be reconciled by more data?',
    'Track outcomes across jurisdictions with differing constitutional traditions (strong substantive due process vs. broad police-power deference) to determine whether reading selection correlates with resolvable doctrinal variables or with irreducible value commitments.',
    'If reading selection is doctrinally resolvable, this reading''s ε and victim set could converge toward the proportionality reading in jurisdictions with strict scrutiny traditions. If irreducible, the three sibling readings remain permanently coexisting rather than converging.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the three kernel readings are converging factual disputes or permanently coexisting value commitments.').

omega_variable(
    coercion_mechanism_severity_variance,
    'How much of the measured extractiveness (0.78 peak) is attributable to termination-based penalties specifically, versus softer mechanisms (access restriction, social pressure) that this reading treats as part of the same coercion apparatus but which carry materially different severity?',
    'Disaggregate enforcement-mechanism data by penalty type (termination vs. access restriction vs. fine) across the measured interval to determine whether the extractiveness trajectory is driven by one dominant mechanism or a blend.',
    'If termination-based penalties dominate, this reading''s high ε is concentrated in employment contexts and the story''s victim set (workers_terminated_for_noncompliance) carries most of the extractive weight; if diffuse, the coercion is more evenly distributed across the payer stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_mechanism_severity_variance, empirical, 'Which specific enforcement mechanism drives the bulk of measured extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 4, 0.15).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 8, 0.2).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.28).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 16, 0.34).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 20, 0.3).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.26).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.78).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.83).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the vaccine_mandate_balance kernel. public_health_primary authors the same underlying mandate episode with collective-protection as the controlling premise, includes immunocompromised-exposed individuals in its victim/affected set, and authors a lower ε keyed to the coordination benefit of averted transmission. proportionality_reading authors a conditional, threshold-gated claim with its own narrower ε keyed to threshold-failure cases and robust-exemption design. All three share the same historical mandate episodes as subject matter but are structurally distinct constraints with non-commensurable ε values, per the ε-invariance principle — they are linked here via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
