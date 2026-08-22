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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Vaccine Mandate Regime — Bodily Autonomy Primacy Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story authors the bodily-autonomy-primacy reading of the contested
 *   vaccine mandate legitimacy kernel: medical self-sovereignty is treated as
 *   an absolute claim, and state coercion toward vaccination is categorically
 *   impermissible regardless of what outcome — including collective infection
 *   risk — would result from noncompliance. This is one reading among three
 *   (the others being public-health-primacy and risk-stratification), each of
 *   which is authored as its own constraint with its own ε, beneficiaries,
 *   and victims. Under this reading, the standing arrangement under contest
 *   is the enforced mandate regime itself, evaluated by this reading's own
 *   categorical lights: the mandate apparatus is read as high-extraction
 *   because it compels a medical intervention without regard to
 *   individualized risk, and — critically for this reading's structural delta
 *   — the categorical anti-mandate position it defends produces a foreseeable
 *   victim class (immunocompromised populations) who bear exposure costs
 *   neither side of the categorical fight internalizes.
 *
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
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Vaccine Mandate Regime — Bodily Autonomy Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '646ad420-0d2c-4699-b3e5-f7e1bec5845a').
narrative_ontology:cs_kernel_codification('646ad420-0d2c-4699-b3e5-f7e1bec5845a', distributed).
narrative_ontology:cs_authority_grounding('646ad420-0d2c-4699-b3e5-f7e1bec5845a', distributed).
narrative_ontology:cs_reading_relation('646ad420-0d2c-4699-b3e5-f7e1bec5845a', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('646ad420-0d2c-4699-b3e5-f7e1bec5845a', vaccine_mandate_legitimacy__risk_stratification_reading, coexists_with).
narrative_ontology:cs_axiom('646ad420-0d2c-4699-b3e5-f7e1bec5845a', foundational, bodily_sovereignty_admits_no_outcome_override).
narrative_ontology:cs_axiom_status(bodily_sovereignty_admits_no_outcome_override, holdable).
narrative_ontology:cs_axiom_grounding('646ad420-0d2c-4699-b3e5-f7e1bec5845a', bodily_sovereignty_admits_no_outcome_override, deontological).
narrative_ontology:cs_axiom('646ad420-0d2c-4699-b3e5-f7e1bec5845a', foundational, state_medical_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_medical_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('646ad420-0d2c-4699-b3e5-f7e1bec5845a', state_medical_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('646ad420-0d2c-4699-b3e5-f7e1bec5845a', informed_consent_doctrine_post_nuremberg).
narrative_ontology:cs_drift_state('646ad420-0d2c-4699-b3e5-f7e1bec5845a', contemporary_pandemic_mandate_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('646ad420-0d2c-4699-b3e5-f7e1bec5845a', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, employer_compliance_administrators).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_refusing_workers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_travelers).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, religious_and_conscience_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer mandate policy, set exemption criteria, and enforce compliance through licensing, employment conditions, and access restrictions. Frame the mandate as necessary collective protection but, on this reading, the agency's mandate authority itself is the object of contest — it collects legitimacy and continuity of function from enforcing the mandate regardless of whether coercion is proportionate to any individual's risk profile.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Implement mandate compliance as a condition of employment, shifting institutional liability onto individual workers. Benefit from a bright-line rule that removes discretion and litigation exposure; the mandate's categorical structure is administratively convenient regardless of medical nuance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, employer_compliance_administrators, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, employer_compliance_administrators, agenda_setter).

% Face termination, licensure loss, or exclusion from employment sectors for declining a medical intervention. On this reading their refusal is a categorical exercise of bodily self-sovereignty; the constraint treats that exercise as noncompliance to be coerced out of them, with no individualized outcome-based justification offered or available as a defense.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_refusing_workers, payer,
    powerless, biographical, trapped, national).

% Barred from cross-border travel, public venues, or transit access based on vaccination status alone. No individualized risk assessment is offered; status functions as a categorical marker of the state's power to condition mobility on submission to a medical procedure.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_travelers, payer,
    powerless, biographical, trapped, global).

% Hold sincere objections to the intervention on religious or conscience grounds. Exemption processes are narrow, discretionary, and frequently denied; the categorical mandate structure treats conscience claims as administrative friction to be minimized rather than a boundary condition on state authority.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, religious_and_conscience_objectors, payer,
    powerless, biographical, trapped, national).

% On this reading, this group enters the victim set indirectly: the categorical anti-mandate position this constraint defends removes a population-level protective layer, leaving medically vulnerable people exposed to elevated transmission risk from unvaccinated contacts they cannot avoid in workplaces, schools, and care settings. Their exposure is a foreseeable cost of the categorical rule, borne by a population with the least capacity to absorb it and no direct voice in the autonomy framing that produces it.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_populations, payer,
    powerless, biographical, trapped, national).

% Organize litigation, lobbying, and public campaigns around the categorical bodily-autonomy claim. Gain political capital, membership growth, fundraising, and precedent-setting legal wins from framing the mandate contest in absolute terms; their organizational interest is served by the categorical framing succeeding regardless of the epidemiological outcome in any particular case.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, mobile, national).

% Possess the risk-stratification data that would let mandate legitimacy be assessed case-by-case, but this reading's categorical framework treats their findings as irrelevant to the underlying question of state authority — outcome data cannot enter the argument because the claim is that coercion is impermissible regardless of outcome. Their expertise is structurally locked out of the dispute this reading stages.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunologists_and_epidemiologists, excluded,
    moderate, generational, analytical, national).

% Adjudicate mandate challenges and can strike down, narrow, or uphold mandate authority. Take testimony from advocacy groups, agencies, and affected individuals; their rulings determine which reading of the underlying kernel becomes enforceable law in a given jurisdiction.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, courts_and_legislatures, observer,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: On its own terms, this reading coordinates a boundary condition on state power: it establishes that medical decisions about one's own body cannot be compelled by the state under any circumstance, giving individuals and objecting communities a stable, categorical basis for refusal that does not depend on winning a case-by-case risk argument every time.
% TRANSFER_FUNCTION: Moves legal and political capital toward liberty advocacy organizations and toward public health agencies' own enforcement discretion (paradoxically, both sides gain organizational strength from the categorical fight), while moving elevated health risk and exclusion costs onto immunocompromised populations, unvaccinated individuals themselves who face employment and mobility penalties, and conscience objectors denied accommodation.
% ABSENT_VOICES: Immunocompromised populations are not organized parties to this reading's central dispute — they are neither the mandate's defenders nor its resisters, yet they bear a downstream exposure cost from whichever side prevails. Immunologists and epidemiologists are excluded by the categorical framing itself, which treats outcome data as inadmissible to the autonomy claim.
% DISAPPEARANCE_RATIONALE: If the categorical bodily-autonomy claim vanished as a live legal and political position overnight, mandate policy would default to risk-stratified or public-health-justified frameworks; litigation strategy, exemption law, and the organizational identity of liberty advocacy movements built around this claim would all have to reorganize around a different foundational argument.
% FOUNDING_PROBLEM: Historical precedent of state-compelled medical procedures (forced sterilization, non-consensual experimentation, coercive institutionalization) without individualized justification, which the categorical autonomy claim was built to foreclose entirely rather than adjudicate case by case.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists working outside both advocacy camps (drawing on the Nuremberg Code and Belmont Report tradition) corroborate that the founding problem — non-consensual state medical coercion — was real and remains a legitimate historical concern; however, public health historians and immunologists outside the liberty movement dispute that a categorical, outcome-independent rule is the correct or only remedy, noting that the founding harms involved no countervailing collective-action problem of the kind infectious disease presents.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extraction (0.68) reflects that mandate enforcement mechanisms (employment conditions, travel restrictions, licensure consequences) impose material costs on refusing individuals categorically, without an outcome-based off-ramp — the categorical structure is what makes the extraction total rather than case-sensitive. Suppression (0.72) captures the narrow, discretionary exemption processes and the trapped exit options for workers and travelers. Resistance (0.81) is high because this reading is actively, organizationally contested by liberty advocacy litigation and grassroots refusal — this is not a settled arrangement but one under continuous legal and political challenge. Theater ratio (0.28) is moderate: some enforcement infrastructure (accommodation review boards, exemption committees) performs deliberative process without materially changing outcomes for objectors. Accessibility collapse (0.45) is only moderate because, unlike a genuine mountain, meaningful legal and political alternatives to the categorical framing persist and are actively litigated — the alternatives have not collapsed, they are contested.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (public health agencies), the categorical resistance to their mandate authority reads as an obstacle to a coordination function they believe they are performing. From the payer seats (refusing workers, travelers, objectors), the same enforcement machinery reads as coercive extraction with no individualized escape valve. From the excluded seat (epidemiologists), the entire dispute is conducted in a vocabulary — categorical rights claims — that renders their outcome data structurally inadmissible, which is itself the point of this reading's construction.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health agencies and employer administrators occupy near-beneficiary positions in the sense that categorical rule enforcement is administratively convenient and legitimacy-preserving for them, regardless of medical nuance — this is what makes the classification tangled_rope rather than pure snare: there is a genuine coordination function (removing case-by-case litigation friction, providing legal clarity) riding alongside the extraction. Vaccine-refusing workers, unvaccinated travelers, and conscience objectors sit at the high-d target end: trapped exit, direct compelled cost. Immunocompromised populations are a distinctive addition on this reading's structural delta — they are victims not of the mandate's coercion but of the categorical anti-coercion position's downside, a second-order cost the autonomy framing does not internalize. Liberty advocacy movements are beneficiaries at low d: mobile exit, organizational gain from the fight's persistence independent of any individual case's outcome.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical non-consensual state medical coercion) is genuinely live as an abstract concern but its status as justification for a CATEGORICAL, outcome-independent rule in the specific context of infectious disease mandates is contested — this is precisely why founding_problem_status is 'contested' rather than 'dead': bioethicists outside both camps affirm the historical harm was real while disputing that categorical foreclosure (rather than proportionality review) is the correct inheritance from that history. The tangled_rope classification prevents this reading from being mislabeled as pure coordination (it has real extraction and a real victim class) or pure extraction (it does solve a genuine problem — protecting against non-consensual medical coercion — that risk-stratification approaches do not fully address for objectors who reject the state's risk calculus itself).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    categorical_claim_versus_proportionality_test,
    'Is bodily self-sovereignty genuinely an unconditional/categorical right that admits no outcome-based override, or is it better modeled as a strong presumption defeasible under a sufficiently severe collective-harm showing (i.e., collapsing into risk_stratification_reading under extreme conditions)?',
    'Doctrinal analysis of how courts have actually ruled under strict scrutiny in analogous compelled-medical-treatment cases (e.g., quarantine law, mandatory treatment for active tuberculosis) to see whether any categorical bodily-autonomy claim has ever survived a sufficiently severe public health showing.',
    'If courts have never sustained a truly categorical rule against severe collective harm, this reading''s core axiom is aspirational rather than a description of live legal doctrine, which would lower confidence in the reading''s practical (as opposed to rhetorical) legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_claim_versus_proportionality_test, conceptual, 'Whether the categorical bodily-autonomy claim is truly unconditional or a strong-but-defeasible presumption.').

omega_variable(
    immunocompromised_harm_attribution,
    'How much of the elevated risk borne by immunocompromised populations should be attributed causally to the categorical autonomy position specifically, versus to background epidemiological facts (viral transmissibility, healthcare system capacity) that would produce some exposure risk under any mandate regime short of universal compliance?',
    'Comparative epidemiological modeling of immunocompromised-population outcomes across jurisdictions with differing mandate stringency, controlling for background transmission and healthcare access.',
    'If the marginal harm attributable specifically to categorical-position-driven noncompliance is small relative to background risk, the victim classification for immunocompromised populations should be qualified as indirect and diffuse rather than a direct structural target of this reading; if large, it strengthens the tangled_rope classification''s victim gate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(immunocompromised_harm_attribution, empirical, 'Causal attribution of immunocompromised risk to this reading''s categorical position versus background epidemiology.').

omega_variable(
    advocacy_movement_capture_of_founding_claim,
    'Has the founding historical problem (non-consensual state medical coercion) been substantially resolved by modern informed-consent law and exemption processes, such that the categorical claim now persists primarily as organizational and fundraising infrastructure for liberty advocacy movements rather than as a live remedy for an ongoing harm?',
    'Track whether liberty advocacy organizations'' litigation and fundraising activity tracks new instances of non-consensual coercion versus tracks mandate policy cycles unrelated to novel coercive incidents.',
    'If advocacy activity tracks policy cycles rather than novel coercion incidents, this would support a piton-adjacent reading of the advocacy infrastructure itself (separate from this constraint) — persisting institutional structure defending a founding claim whose acute historical harm has substantially receded, even as the categorical legal claim itself remains contested and unresolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(advocacy_movement_capture_of_founding_claim, empirical, 'Whether advocacy movement persistence tracks live coercion incidents or self-sustaining organizational momentum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 4, 0.18).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 12, 0.65).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 12, 0.72).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the vaccine_mandate_legitimacy kernel, each authored as a separate constraint with its own ε per the ε-invariance principle. bodily_autonomy_primacy_reading (this file, ε=0.68) treats state coercion as categorically impermissible regardless of outcome and forecloses public_health_primacy_reading's core premise (collective-harm-justified authority) within any single legal framework, while coexisting with risk_stratification_reading as a live but distinct position (proportionality-based rather than categorical). The three readings share no single ε because they are not the same constraint measured three ways — they are three structurally distinct legitimacy claims with different beneficiary/victim sets, linked here for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
