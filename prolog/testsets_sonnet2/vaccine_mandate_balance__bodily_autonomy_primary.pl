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
 *   human_readable: Vaccine Mandate Enforcement Under a Bodily-Autonomy-Primary Reading
 *   domain: public_health_ethics/constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of a
 *   contested kernel about vaccine mandate legitimacy: the claim that
 *   individual consent to medical intervention is inviolable and that no
 *   aggregate collective benefit calculus can license state compulsion. Under
 *   this reading, mandate enforcement — job conditioning, exemption
 *   narrowing, exclusion from public accommodation — is coercion applied to a
 *   rights violation, full stop; the epidemiological case for the mandate is
 *   treated as causally irrelevant to whether the compulsion is permissible.
 *   This is NOT the same constraint as the public_health_primary reading
 *   (which treats the identical enforcement machinery as legitimate
 *   collective protection) or the proportionality_reading (which conditions
 *   legitimacy on strict severity/risk/safety thresholds). Each reading has
 *   its own beneficiary/victim structure and its own epsilon; they are linked
 *   as siblings under kernel vaccine_mandate_balance, not merged here.
 *
 * KEY AGENTS:
 *   - public_health_agencies: sets and enforces mandate policy (institutional/analytical) — under this reading, the coercing party
 *   - unvaccinated_coerced_individuals: primary target (powerless/trapped) — bears direct coercion
 *   - religious_and_philosophical_objectors: secondary target (powerless/constrained) — bears coercion mediated through exemption process
 *   - workers_facing_termination_for_noncompliance: bears livelihood-conditioned coercion (powerless/trapped)
 *   - vaccinated_majority_population: incidental beneficiary (organized/mobile)
 *   - employers_offloading_liability: delegated enforcer and beneficiary (organized/mobile)
 *   - immunocompromised_and_medically_vulnerable: excluded from this reading's victim set — their exposure risk is not treated as a cost the mandate legitimately trades against autonomy
 *   - courts_and_constitutional_adjudicators: analytical observer adjudicating the claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__bodily_autonomy_primary, 0.78).
domain_priors:suppression_score(vaccine_mandate_balance__bodily_autonomy_primary, 0.82).
domain_priors:theater_ratio(vaccine_mandate_balance__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_balance__bodily_autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(vaccine_mandate_balance__bodily_autonomy_primary, "Vaccine Mandate Enforcement Under a Bodily-Autonomy-Primary Reading").
narrative_ontology:topic_domain(vaccine_mandate_balance__bodily_autonomy_primary, "public_health_ethics/constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__bodily_autonomy_primary, '64b83d34-2c92-4852-9401-e7860332e9e0').
narrative_ontology:cs_kernel_codification('64b83d34-2c92-4852-9401-e7860332e9e0', distributed).
narrative_ontology:cs_authority_grounding('64b83d34-2c92-4852-9401-e7860332e9e0', distributed).
narrative_ontology:cs_reading_relation('64b83d34-2c92-4852-9401-e7860332e9e0', vaccine_mandate_balance__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('64b83d34-2c92-4852-9401-e7860332e9e0', vaccine_mandate_balance__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('64b83d34-2c92-4852-9401-e7860332e9e0', foundational, consent_inviolable_regardless_of_aggregate_benefit).
narrative_ontology:cs_axiom_status(consent_inviolable_regardless_of_aggregate_benefit, holdable).
narrative_ontology:cs_axiom_grounding('64b83d34-2c92-4852-9401-e7860332e9e0', consent_inviolable_regardless_of_aggregate_benefit, deontological).
narrative_ontology:cs_axiom('64b83d34-2c92-4852-9401-e7860332e9e0', secondary, third_party_exposure_risk_is_liberty_cost_not_harm_licensing_compulsion).
narrative_ontology:cs_axiom_status(third_party_exposure_risk_is_liberty_cost_not_harm_licensing_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('64b83d34-2c92-4852-9401-e7860332e9e0', third_party_exposure_risk_is_liberty_cost_not_harm_licensing_compulsion, deontological).
narrative_ontology:cs_reference_frame('64b83d34-2c92-4852-9401-e7860332e9e0', informed_consent_medical_ethics_tradition).
narrative_ontology:cs_drift_state('64b83d34-2c92-4852-9401-e7860332e9e0', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('64b83d34-2c92-4852-9401-e7860332e9e0', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, vaccinated_majority_population).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__bodily_autonomy_primary, employers_offloading_liability).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__bodily_autonomy_primary, workers_facing_termination_for_noncompliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces mandate policy, sets penalties for noncompliance (loss of employment, school access, public accommodation), and justifies the mandate by aggregate epidemiological benefit. From this reading's premise, the agency's authority to compel is itself the violation under scrutiny, regardless of the epidemiological case it musters.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies, beneficiary).

% Face termination, exclusion from schooling, travel restriction, or social exclusion for declining an unwanted medical intervention. Under this reading, their refusal is the exercise of an inviolable right, and every consequence attached to that refusal is a coercive penalty for exercising it, not a legitimate cost of noncompliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, biographical, trapped, national).

% Hold sincere objections rooted in belief systems or bodily-integrity commitments. Exemption processes are frequently narrowed or eliminated by the agenda-setter, converting a formerly available exit into a trap; where exemptions remain, they are often burdensome enough to function as a soft denial.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, constrained, national).

% Employers, acting under agency directives or their own liability calculus, terminate or suspend employees who decline. For workers without alternative employment mobility, the mandate operates as an ultimatum: submit to the intervention or lose livelihood — under this reading, an ultimatum against bodily autonomy is coercion regardless of the employer's intervening role.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, workers_facing_termination_for_noncompliance, payer,
    powerless, biographical, trapped, national).

% Complied voluntarily or under mild pressure, and benefits from reduced transmission and from social/professional environments where mandates exclude the unvaccinated. Not exposed to the coercion mechanism because they already comply; this reading treats their benefit as incidental and non-dispositive of the mandate's legitimacy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, vaccinated_majority_population, beneficiary,
    organized, biographical, mobile, national).

% Implement mandates to limit workplace-transmission liability and comply with government contracting requirements, functioning as a delegated enforcement layer. They benefit from liability transfer while bearing none of the bodily cost themselves.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, employers_offloading_liability, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__bodily_autonomy_primary, employers_offloading_liability, agenda_setter).

% Face elevated risk from community transmission and would prefer higher population vaccination rates, but under this reading their vulnerability does not license compelling others' bodies — their exposure risk is treated as an outcome of living in a society that protects individual bodily sovereignty, not as a harm generated BY the unvaccinated that licenses coercion. They are named here as excluded rather than as victims of this constraint, since the reading does not recognize their exposure as a cost the mandate exists to remedy at the expense of autonomy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, immunocompromised_and_medically_vulnerable, excluded,
    powerless, biographical, constrained, national).

% Hear challenges to mandates, weigh compelled-intervention claims against police-power precedent, and can enjoin or uphold enforcement. Their rulings determine whether the coercion mechanism this reading identifies is judicially sanctioned or struck down.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__bodily_autonomy_primary, courts_and_constitutional_adjudicators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate purports to solve a collective-action problem: individually rational non-vaccination decisions can produce a population-level failure to reach herd immunity, leaving transmission chains open. The mandate substitutes compulsion for voluntary uptake to close that gap.
% TRANSFER_FUNCTION: Moves bodily decision-making authority from the individual to the state (directly) and to employers/institutions acting as enforcement delegates (indirectly), and moves the material costs of noncompliance — job loss, exclusion, exemption-application burden — onto those who decline, while the health and associational benefits of higher compliance accrue to the vaccinated majority and to institutions avoiding liability.
% ABSENT_VOICES: The reading itself is voiced by an inviolable-consent tradition (constitutional liberty doctrine, informed-consent medical ethics, some religious-liberty advocacy); it is largely absent from public-health agency deliberation, which treats the individual claim as already outweighed by aggregate benefit before the autonomy question is asked.
% DISAPPEARANCE_RATIONALE: If mandate enforcement disappeared overnight, terminated employees would be reinstated or eligible for reinstatement, exemption litigation would collapse for lack of controversy, and the enforcement apparatus (compliance tracking, exemption review boards, employer liability shields) would need to be dismantled or repurposed — a substantial existing infrastructure organized specifically around compelling and verifying compliance would lose its object.
% FOUNDING_PROBLEM: Voluntary vaccination uptake was insufficient to reach herd-immunity thresholds during acute outbreak conditions, and policymakers sought a mechanism to raise compliance rates faster than persuasion or incentive alone could achieve.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and epidemiologists (the benefiting/administering parties) attest the coverage gap was real and remains a live risk in future outbreaks. Constitutional scholars, civil liberties organizations, and dissenting judges outside the administering apparatus attest that, whatever the epidemiological merits, the specific mechanism of compulsion — as opposed to voluntary incentive, education, or accommodation — was never demonstrated necessary rather than merely convenient, and note that several jurisdictions achieved comparable coverage through incentive-based approaches without compulsion.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.78) because, under this reading, the mandate takes something the state has no legitimate claim to at all — the individual's bodily decision — and the 'transfer' is total rather than partial: there is no proportionality discount, since the reading denies collective benefit as a valid offset. Suppression is authored slightly higher still (0.82) because persistence depends entirely on active enforcement mechanisms (termination, exclusion, exemption denial) with no natural expiration; remove the enforcement machinery and compliance among objectors would likely fall sharply, which is itself evidence the mandate is compulsion rather than a self-sustaining coordination norm from this reading's vantage. Theater ratio is comparatively low-moderate (0.28) because the enforcement is mostly functional (real job loss, real exclusion) rather than symbolic, though public messaging about 'community protection' is documented here as a partly performative gloss on a compulsion mechanism. Accessibility collapse is moderate (0.45), not extreme, because alternative arrangements (voluntary compliance regimes, incentive-based programs) are documented to exist and to have worked in some jurisdictions — the collapse is authored to reflect the reading's own claim that this was a choice, not a necessity.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public_health_agencies), the arrangement looks like legitimate exercise of police power in service of a real coordination problem. From the payer seats, the identical enforcement structure looks like a rights violation dressed in coordination language. The engine computes these as structurally different seat-experiences from the declared power/exit/beneficiary data; this reading's authored claim (snare) reflects the payer-seat reading as structurally dominant, while acknowledging the agenda-setter seat would compute closer to tangled_rope or rope on the same data — that divergence is the point, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Under this reading, unvaccinated_coerced_individuals, religious_and_philosophical_objectors, and workers_facing_termination_for_noncompliance are declared victims: the constraint extracts bodily compliance or imposes severe costs for withholding it, and their exit options range from trapped to constrained. public_health_agencies and employers are beneficiaries/agenda-setters: the agency sets and enforces the compulsion; employers offload liability while imposing the mandate's teeth. vaccinated_majority_population benefits incidentally without bearing any coercion themselves — their directionality sits near the beneficiary end because the constraint subsidizes their environment without extracting from their bodies. Critically, immunocompromised_and_medically_vulnerable are excluded rather than victimized here — this is the reading's structural signature: their exposure risk is treated as an outcome of living under a liberty-respecting regime, not a harm the mandate is entitled to remedy at another's bodily expense. A public_health_primary reading of the identical facts would place this group among the beneficiaries the mandate protects and would treat unvaccinated compliance costs as proportionate — that is a different constraint, filed separately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem interview surfaces a genuine live tension: agencies attest the coverage-gap problem persists as an outbreak risk, while outside constitutional and civil-liberties observers attest that compulsion specifically (versus incentive or voluntary approaches) was never shown necessary. Under this reading, that gap between 'problem still live' and 'this specific coercive mechanism still necessary' is exactly what prevents the mandate from being read as pure coordination — the coordination problem's persistence does not, by this reading's axioms, license persistence of the compulsion mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compulsion_necessity_vs_convenience,
    'Was compulsion (as opposed to incentive-based or purely voluntary approaches) ever demonstrated to be necessary to reach the coverage the founding problem required, or was it adopted because it was administratively faster?',
    'Comparative policy analysis across jurisdictions that used incentive-only versus compulsion-based approaches, controlling for baseline hesitancy and outbreak severity.',
    'If compulsion was never necessary, the extraction measured here is fully avoidable rent on liberty rather than a forced tradeoff; if it was necessary in some acute-severity conditions, the bodily_autonomy_primary reading''s foreclosure of any collective-benefit offset becomes harder to sustain in those specific windows, and the proportionality_reading''s conditional structure gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsion_necessity_vs_convenience, empirical, 'Whether compulsion mechanisms were demonstrated necessary or merely convenient relative to voluntary alternatives.').

omega_variable(
    vulnerable_population_exclusion_from_victim_set,
    'Is it structurally correct, under a bodily-autonomy-primary framework, to exclude immunocompromised and medically vulnerable people from the victim set even though they bear real, uncompensated exposure risk from others'' choices?',
    'Philosophical analysis of whether liberty-based frameworks that permit harm-to-third-parties externalities (e.g. via a Millian harm principle) would in fact include exposed vulnerable populations as victims, complicating the clean exclusion asserted here.',
    'If a harm-principle-consistent version of bodily autonomy would recognize third-party exposure harm, this reading''s claim to exclude the immunocompromised from the victim set weakens, and the constraint''s beneficiary/victim structure would need revision — potentially converging partway toward the proportionality_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_exclusion_from_victim_set, conceptual, 'Whether this reading''s exclusion of exposed vulnerable populations from its victim set is internally consistent with the liberty tradition it claims.').

omega_variable(
    reading_selection_under_epistemic_uncertainty,
    'Given genuine scientific uncertainty at the time of mandate adoption about transmission dynamics and vaccine efficacy against transmission (versus severe disease), which reading''s factual premises were best supported at each decision point?',
    'Retrospective epidemiological reconstruction of what was knowable at each policy decision point, cross-referenced against the confidence levels agencies actually expressed contemporaneously.',
    'If transmission-blocking efficacy was well-established at mandate adoption, the public_health_primary reading''s collective-benefit case strengthens for that period; if efficacy was contested or declining (e.g., against later variants), this reading''s rejection of the collective-benefit offset gains empirical support for that period — the correct reading may not be constant across the interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_under_epistemic_uncertainty, empirical, 'Whether the empirical premises underlying the collective-benefit case were well-supported at each policy decision point, which bears on which reading best fits which period.').


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
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 12, 0.25).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 16, 0.27).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 20, 0.28).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__bodily_autonomy_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 12, 0.75).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 16, 0.78).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__bodily_autonomy_primary, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 8, 0.72).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 16, 0.82).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 20, 0.79).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__bodily_autonomy_primary, suppression_requirement, 24, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__bodily_autonomy_primary, vaccine_mandate_balance__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vaccine_mandate_balance kernel. public_health_primary reads the identical enforcement machinery as legitimate coordination with the immunocompromised/vulnerable as beneficiaries rather than excluded parties, and computes low or moderate extraction against a strong coordination function. proportionality_reading occupies a middle position, treating mandate legitimacy as conditional on severity/transmission/safety thresholds and robust exemptions, and would show extraction that varies by disease-severity regime rather than the flat high extraction authored here. Each reading has an independently authored epsilon; none is derived from another by averaging or observable-switching, per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
