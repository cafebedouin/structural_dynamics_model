% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Vaccine/Medical Mandate Authority Read as Categorical Bodily-Autonomy Violation
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primary reading of the
 *   contested public health mandate authority kernel: it treats any
 *   non-consensual medical intervention imposed via employment, travel, or
 *   institutional access conditions as a categorical violation, regardless of
 *   the epidemiological case offered for it. Enforcement intensified sharply
 *   during acute outbreak phases (T4-T12, tracking emergency mandate
 *   expansion) and relaxed somewhat as mandates were rolled back or struck
 *   down in several jurisdictions (T16-T20), then partially re-hardened in
 *   some sectors. This reading recognizes unvaccinated individuals,
 *   religious/philosophical objectors, and workers facing termination as the
 *   victim class — because the coercion mechanism itself is the harm,
 *   independent of medical outcome. It explicitly excludes immunocompromised
 *   and other vulnerable populations from the victim set: their elevated risk
 *   is real but does not, on this reading's foundational premise, generate a
 *   countervailing claim that licenses invading someone else's body.
 *   Public-health-primary advocates bear zero measured extraction here, since
 *   no intervention is imposed on parties who already consent to it.
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda_setter (institutional/analytical) - sets and enforces mandate policy
 *   - employers_requiring_compliance: agenda_setter/beneficiary (organized/constrained) - administers workplace compliance
 *   - vaccine_manufacturers: beneficiary (institutional/arbitrage) - collects mandate-driven demand
 *   - unvaccinated_individuals: payer (powerless/trapped) - bears direct coercion
 *   - religious_and_philosophical_objectors: payer (powerless/trapped) - conscientious claim treated as exception
 *   - workers_facing_termination_for_noncompliance: payer (powerless/trapped) - economic coercion converts choice to non-choice
 *   - immunocompromised_and_vulnerable_populations: excluded (powerless/trapped) - real risk, structurally excluded from this reading's victim set
 *   - public_health_primary_advocates: excluded (organized/analytical) - bears zero coercion under this reading
 *   - courts_and_constitutional_review_bodies: observer (institutional/analytical) - adjudicates the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.81).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.72).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Vaccine/Medical Mandate Authority Read as Categorical Bodily-Autonomy Violation").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '4393e77d-c77b-44fc-bce0-a1e9b44478ce').
narrative_ontology:cs_kernel_codification('4393e77d-c77b-44fc-bce0-a1e9b44478ce', distributed).
narrative_ontology:cs_authority_grounding('4393e77d-c77b-44fc-bce0-a1e9b44478ce', distributed).
narrative_ontology:cs_reading_relation('4393e77d-c77b-44fc-bce0-a1e9b44478ce', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('4393e77d-c77b-44fc-bce0-a1e9b44478ce', public_health_mandate_authority__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('4393e77d-c77b-44fc-bce0-a1e9b44478ce', foundational, bodily_sovereignty_is_categorical_not_balanceable).
narrative_ontology:cs_axiom_status(bodily_sovereignty_is_categorical_not_balanceable, holdable).
narrative_ontology:cs_axiom_grounding('4393e77d-c77b-44fc-bce0-a1e9b44478ce', bodily_sovereignty_is_categorical_not_balanceable, deontological).
narrative_ontology:cs_axiom('4393e77d-c77b-44fc-bce0-a1e9b44478ce', foundational, collective_benefit_cannot_generate_consent).
narrative_ontology:cs_axiom_status(collective_benefit_cannot_generate_consent, holdable).
narrative_ontology:cs_axiom_grounding('4393e77d-c77b-44fc-bce0-a1e9b44478ce', collective_benefit_cannot_generate_consent, deontological).
narrative_ontology:cs_reference_frame('4393e77d-c77b-44fc-bce0-a1e9b44478ce', individual_consent_as_precondition_for_medical_intervention).
narrative_ontology:cs_drift_state('4393e77d-c77b-44fc-bce0-a1e9b44478ce', post_pandemic_mandate_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4393e77d-c77b-44fc-bce0-a1e9b44478ce', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, employers_requiring_compliance).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, vaccine_manufacturers).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, religious_and_philosophical_objectors).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, workers_facing_termination_for_noncompliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces mandate policy, framing non-consensual medical intervention as justified by aggregate epidemiological benefit. From this reading's premise, no aggregate benefit can license the invasion of an individual body — the agency's authority claim is itself the categorical violation, regardless of the epidemiology it cites.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Implements mandates as a condition of employment, transferring the coercive burden onto workers while shielding itself from liability under public-health authority. Benefits from continued operations and legal cover; imposes the choice between bodily submission and livelihood loss.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, employers_requiring_compliance, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__bodily_autonomy_primary, employers_requiring_compliance, beneficiary).

% Collects expanded demand and liability-shielded distribution created by mandate policy. Under this reading, the manufacturer's commercial gain is structurally downstream of coerced consumption, not voluntary market uptake.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, vaccine_manufacturers, beneficiary,
    institutional, biographical, arbitrage, global).

% Bears the direct coercion: compelled medical intervention or loss of employment, travel, education, or public participation as the enforcement lever. Under this reading, the coercion itself is the harm, independent of the medical outcome — bodily sovereignty is violated the moment consent is structurally overridden.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, biographical, trapped, national).

% Holds a conscientious objection to intervention that the mandate structure treats as an exception to be adjudicated rather than a sovereignty claim to be respected outright. Exemption processes are discretionary and can be narrowed or revoked, leaving the objector's exit contingent on administrative goodwill.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, religious_and_philosophical_objectors, payer,
    powerless, biographical, trapped, national).

% Faces the sharpest form of the coercion: comply or lose the income on which survival depends. The economic dependency converts a nominal choice into a non-choice, which is precisely what this reading identifies as disqualifying any claim that the intervention remains consensual.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, workers_facing_termination_for_noncompliance, payer,
    powerless, biographical, trapped, national).

% Bears elevated infection risk if mandates are not enforced, and would object strenuously to their removal, but this reading does not treat their exposure as a claim that licenses invading another person's body — their situation is real but structurally excluded from the victim set this reading recognizes, because no duty to protect them can be discharged through non-consensual intervention on someone else.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_and_vulnerable_populations, excluded,
    powerless, biographical, trapped, national).

% Argues the mandate discharges a collective obligation to protect vulnerable populations and healthcare infrastructure. Under this reading, this constituency bears zero coercion itself — no medical intervention is imposed on advocates who already consent — so it registers no extraction, only a rejected justification.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicates challenges to mandate authority, weighing bodily autonomy claims against state police-power doctrine. Its rulings determine whether this reading's categorical claim or a balancing framework governs enforcement going forward.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, courts_and_constitutional_review_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__bodily_autonomy_primary, public_health_agencies).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate purports to solve a collective-action problem in communicable disease control: individual non-vaccination imposes externalized infection risk that voluntary uptake alone may under-supply.
% TRANSFER_FUNCTION: The arrangement moves bodily decision authority from the individual to the state/employer, and moves income, employment security, and freedom of movement away from noncompliant individuals toward institutions administering compliance and toward manufacturers whose products are mandated.
% ABSENT_VOICES: Individuals who refuse on bodily-sovereignty grounds are formally present in exemption hearings but structurally absent from the framing of the mandate itself — the policy is set before their objection is heard, and the exemption process treats their claim as an exception to justify rather than a boundary to respect.
% DISAPPEARANCE_RATIONALE: If mandate authority vanished overnight, employment, travel, and institutional access would no longer be conditioned on medical compliance; noncompliant individuals would regain employment and mobility immediately, while public health agencies would lose their principal coercive lever and have to rely entirely on voluntary uptake and persuasion.
% FOUNDING_PROBLEM: Communicable disease outbreaks that voluntary vaccination uptake alone did not contain quickly enough to prevent healthcare system strain and continued transmission.
% FOUNDING_PROBLEM_CORROBORATION: Public health agencies and employers attest the problem remains live (variant emergence, healthcare capacity). Civil liberties organizations, some constitutional scholars, and courts in jurisdictions that have struck down mandates attest that, under a bodily-sovereignty framework, no epidemiological finding can supply the missing element — individual consent — so the 'problem' as originally framed was never one that this kind of intervention was entitled to solve, regardless of its live or dead status.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.81 at interval end) because, from this reading's premise, the mandate structure extracts bodily decision authority itself from every noncompliant individual, with income and mobility as the enforcement levers — the harm is the coercion, not merely its downstream medical effect. Suppression tracks enforcement intensity (0.72, having peaked near 0.82 during acute mandate expansion) because the constraint's persistence depends on active exclusion mechanisms (employment conditions, travel restrictions, access gating) rather than voluntary uptake. Theater ratio is moderate-low (0.28) reflecting that most enforcement activity is functionally coercive rather than merely performative, though some compliance-reporting apparatus is largely symbolic. Accessibility collapse (0.58) and resistance (0.74) are both mid-to-high: exemption pathways exist on paper but are narrow and discretionary, and this reading treats the mandate as having met, and continuing to meet, substantial organized resistance from objecting populations and courts.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health agencies, employers), the arrangement reads as legitimate collective action addressing a genuine externality. From the payer seats (unvaccinated individuals, objectors, workers under economic duress), the same structure reads as coercion regardless of the epidemiological justification offered, because this reading holds that no aggregate benefit calculus can supply consent. The engine computes these as structurally distinct experiences of the same enforcement apparatus; this reading does not average them — it asserts the payer-seat reading is the correct characterization of the constraint's moral status, while still authoring the agenda-setter seat's structural position honestly.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and employers sit at the beneficiary/agenda-setter end: they set the terms, administer enforcement, and are structurally shielded from the coercion they impose. Vaccine manufacturers are pure beneficiaries with arbitrage-grade exit (global market, liability shielding) and no coercion imposed on them. Unvaccinated individuals, objectors, and at-risk workers sit at the full-target end: trapped exit options, direct imposition of the constraint's costs. Immunocompromised populations are deliberately excluded from the victim set under this reading's own logic, even though they bear real infection risk — that exclusion is the structural delta this reading produces relative to the public-health-primary sibling, not an oversight. Public-health-primary advocates are excluded from the beneficiary/victim structure entirely because no coercion is imposed on parties who already consent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (containing communicable disease where voluntary uptake was insufficient) is marked contested rather than resolved: public health agencies attest it remains live, while this reading's own tradition holds that even a live epidemiological problem cannot license the missing element (consent), meaning the mandate's persistence is not explained by the original justification alone. This prevents mislabeling the arrangement as pure temporary emergency scaffolding — it functions as an ongoing extraction structure whose sunset has repeatedly been deferred by wave after wave of renewed policy, not resolved by any single wave's end.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_vs_balancing_framework_choice,
    'Is bodily sovereignty properly treated as a categorical (non-balanceable) claim, or is it one weighty factor among several in a proportionality analysis?',
    'This is not empirically resolvable — it is a foundational normative commitment about whether rights admit of balancing at all. Legal doctrine across jurisdictions splits on this question (strict scrutiny with narrow tailoring vs. categorical rules), and the split itself is evidence the question is live rather than settled.',
    'If categorical, any mandate enforcement mechanism is per se illegitimate regardless of threat severity, making this reading''s classification (snare, high extraction) stable across all outbreak conditions. If balancing is the correct framework, this constraint''s classification collapses into the proportionality_reading sibling, where ε varies with threat severity and duration.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_vs_balancing_framework_choice, preference, 'Whether rights claims of this kind are categorical or subject to interest-balancing — the foundational fork between this reading and the proportionality sibling.').

omega_variable(
    immunocompromised_exclusion_defensibility,
    'Is it defensible to exclude immunocompromised and other vulnerable populations from the victim set of a mandate-repeal scenario, given they bear the elevated infection risk that repeal would reintroduce?',
    'Compare outcomes (infection/mortality rates among vulnerable populations) in jurisdictions with and without mandate enforcement, controlling for voluntary uptake rates achieved through non-coercive means.',
    'If vulnerable-population harm from non-enforcement is large and consistently attributable to the absence of mandates specifically (rather than achievable through non-coercive alternatives), the exclusion becomes harder to defend even within this reading''s own framework, since the reading''s premise concerns the illegitimacy of the MEANS (non-consensual intervention on others), not indifference to the vulnerable population''s situation as such.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_exclusion_defensibility, empirical, 'Whether excluding immunocompromised populations from the victim set is defensible even on this reading''s own terms.').

omega_variable(
    coercion_vs_consequence_harm_conflation,
    'Does this reading''s definition of harm-as-coercion (rather than harm-as-medical-outcome) hold up when the coerced intervention is, ex post, medically beneficial to the coerced individual?',
    'Philosophical and legal analysis of whether autonomy violations retain their wrongness independent of paternalistic benefit — parallel to debates on informed consent doctrine in medical ethics generally.',
    'If autonomy violation is wrong independent of outcome, this reading''s ε stays stable regardless of vaccine efficacy data. If harm is partly a function of outcome, ε should in principle vary with the medical evidence on the intervention''s net benefit to the coerced individual — which this reading currently does not model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_consequence_harm_conflation, conceptual, 'Whether coercion-as-harm is independent of the medical outcome of the coerced intervention.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(publ_tr_t4, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 4, 0.2).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 8, 0.24).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 12, 0.26).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 16, 0.29).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 20, 0.31).
narrative_ontology:measurement(publ_tr_t24, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(publ_be_t4, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 4, 0.68).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 8, 0.79).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 12, 0.85).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 16, 0.83).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(publ_be_t24, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 24, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(publ_su_t4, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 8, 0.75).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 12, 0.82).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 16, 0.76).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(publ_su_t24, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the public_health_mandate_authority kernel. bodily_autonomy_primary (this file) treats coercion itself as the categorical harm and excludes immunocompromised populations from the victim set. public_health_primary treats the mandate as a protective obligation to the vulnerable commons and would author near-zero extractiveness on the arrangement while placing unvaccinated individuals partly in a beneficiary-adjacent (protected-by-others) or even obligated-payer role rather than victim role. proportionality_reading authors ε as a function of threat severity, alternatives, coercion magnitude, and duration rather than as a fixed value, producing a variable classification across outbreak phases. All three share the same underlying enforcement apparatus and stakeholder population but assign radically different beneficiary/victim structures and ε values because they differ on the foundational question of whether bodily autonomy claims are categorical or balanceable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
