% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_balance__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_balance__proportionality_reading, []).

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
 *   constraint_id: vaccine_mandate_balance__proportionality_reading
 *   human_readable: Proportionality-Gated Vaccine Mandate Framework
 *   domain: public_health_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint is the proportionality reading of the vaccine mandate
 *   balance kernel: mandates are permissible only when disease severity,
 *   transmission risk, and vaccine safety jointly clear a strict threshold,
 *   and exemptions must be substantively robust rather than nominal. This is
 *   deliberately NOT the public-health-primary reading (which would authorize
 *   mandates whenever voluntary compliance fails to reach herd immunity,
 *   regardless of a fine-grained severity threshold) and NOT the
 *   bodily-autonomy-primary reading (which would forbid compulsion
 *   categorically). The proportionality reading occupies the contested
 *   middle: it authorizes real compulsion, but only within a bounded,
 *   contestable band, and it is precisely the boundary cases — pathogens near
 *   the severity threshold, exemption claims near the robustness bar,
 *   occupational roles near the risk-aggregation line — where this reading's
 *   extraction is concentrated. ε is authored here as the standing
 *   arrangement of proportionality-gated mandate law as this reading's own
 *   framework sees it: moderate extraction, not the near-zero or near-maximal
 *   extraction the sibling readings would author for their own standing
 *   arrangements.
 *
 * KEY AGENTS:
 *   - public_health_authorities: agenda-setter, administers and applies the threshold test
 *   - immunocompromised_populations: beneficiary, dependent on mandates clearing the threshold
 *   - borderline_case_objectors: payer, compelled under a contested threshold judgment
 *   - exemption_seekers_denied_narrowly: payer, denied by the robustness requirement
 *   - low_risk_occupational_workers: payer, compelled under aggregate rather than individualized proportionality
 *   - vaccine_safety_reviewers: observer, supplies the empirical safety leg of the test
 *   - courts_and_judicial_review: observer, disciplines or rubber-stamps the test's application
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_balance__proportionality_reading, 0.38).
domain_priors:suppression_score(vaccine_mandate_balance__proportionality_reading, 0.42).
domain_priors:theater_ratio(vaccine_mandate_balance__proportionality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vaccine_mandate_balance__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_balance__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_balance__proportionality_reading, "Proportionality-Gated Vaccine Mandate Framework").
narrative_ontology:topic_domain(vaccine_mandate_balance__proportionality_reading, "public_health_ethics/constitutional_law").

domain_priors:requires_active_enforcement(vaccine_mandate_balance__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_balance__proportionality_reading, '7d72bc6e-f8b3-4ae4-832f-f31aaec667ae').
narrative_ontology:cs_kernel_codification('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', distributed).
narrative_ontology:cs_authority_grounding('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', distributed).
narrative_ontology:cs_reading_relation('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', vaccine_mandate_balance__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', vaccine_mandate_balance__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', foundational, compulsion_requires_calibrated_threshold).
narrative_ontology:cs_axiom_status(compulsion_requires_calibrated_threshold, holdable).
narrative_ontology:cs_axiom_grounding('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', compulsion_requires_calibrated_threshold, instrumental).
narrative_ontology:cs_axiom('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', secondary, exemption_robustness_is_legitimacy_condition).
narrative_ontology:cs_axiom_status(exemption_robustness_is_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', exemption_robustness_is_legitimacy_condition, conventional).
narrative_ontology:cs_reference_frame('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', graduated_scrutiny_public_health_jurisprudence).
narrative_ontology:cs_drift_state('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', post_covid19_mandate_litigation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7d72bc6e-f8b3-4ae4-832f-f31aaec667ae', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_balance__proportionality_reading, vaccine_mandate_balance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_balance__proportionality_reading, healthcare_system_capacity_planners).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, borderline_case_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, exemption_seekers_denied_narrowly).
narrative_ontology:constraint_victim(vaccine_mandate_balance__proportionality_reading, low_risk_occupational_workers).
narrative_ontology:constraint_vindicates(vaccine_mandate_balance__proportionality_reading, proportionality_doctrine_in_public_health_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the tiered proportionality test — weighing disease severity, transmission risk, and vaccine safety data against exemption robustness — and decide case-by-case whether a mandate clears the threshold. They gain legitimacy and institutional durability when courts and the public accept the test as principled rather than arbitrary; their exit from the framework itself is effectively unlimited since they author its application.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_balance__proportionality_reading, public_health_authorities, beneficiary).

% Rely on herd-level protection that only clears when mandates are permitted under high-severity, high-transmission conditions. They cannot vaccinate themselves out of risk (medical contraindication) and depend entirely on others' compliance being compelled during the narrow window the proportionality test allows it. When the test denies a mandate for a pathogen it judges insufficiently severe, they bear the exposure risk with no recourse.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, immunocompromised_populations, beneficiary,
    powerless, biographical, trapped, local).

% Object to a mandate imposed for a pathogen that sits near the proportionality threshold — severe enough to trigger compulsion under the test's judgment, but contestably so. They experience the compelled intervention as coercive precisely because the threshold that authorized it is itself disputed; their only recourse is litigation or a narrow exemption claim, both slow and uncertain.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, borderline_case_objectors, payer,
    moderate, biographical, constrained, regional).

% File for medical, religious, or conscience exemptions that the framework requires be 'robust' — meaning narrowly construed and evidentially demanding. Those whose claims fall just short of the robustness bar are compelled despite a genuine (if not fully qualifying) objection; the demand for robustness is what lets the framework claim it protects conscience while still denying most claims.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, exemption_seekers_denied_narrowly, payer,
    powerless, immediate, trapped, local).

% Work in settings mandated under a sector-wide rule calibrated to average transmission risk, even though their specific role carries below-average exposure (e.g., remote or low-contact positions folded into a facility-wide mandate). The proportionality test is applied at the population or institutional level, not the individual role level, so they absorb a compulsion whose individualized proportionality is weaker than the aggregate justification implies.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, low_risk_occupational_workers, payer,
    powerless, biographical, constrained, regional).

% Generate the safety data that feeds the proportionality calculation. They do not decide mandates but their findings determine which pathogens can ever clear the threshold; their independence from both public health agenda-setters and vaccine manufacturers is itself contested territory.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, vaccine_safety_reviewers, observer,
    institutional, generational, analytical, national).

% Adjudicate challenges to specific mandate applications, testing whether the proportionality framework was actually applied or merely invoked as post-hoc justification. Their rulings either discipline the framework toward genuine case-by-case rigor or allow it to calcify into rubber-stamp deference.
narrative_ontology:constraint_stakeholder(vaccine_mandate_balance__proportionality_reading, courts_and_judicial_review, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_balance__proportionality_reading, diffuse).
narrative_ontology:fixing_cost_class(vaccine_mandate_balance__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, non-categorical decision procedure that lets courts, legislatures, and health authorities determine when compelling vaccination is justified — avoiding both the paralysis of requiring unanimous consent during a lethal outbreak and the overreach of permitting compulsion for any pathogen regardless of severity.
% TRANSFER_FUNCTION: Moves the burden of uncertainty about where a pathogen sits on the severity/transmission/safety scale onto whichever party's case falls near the threshold — objectors near the line are compelled, exemption seekers whose claims fall just short of 'robust' are compelled, low-risk workers folded into aggregate mandates absorb a compulsion calibrated to others' risk. In exchange, immunocompromised and high-risk populations receive herd protection precisely when the test's judgment favors it.
% ABSENT_VOICES: Individuals whose specific medical or occupational risk profile falls between the coarse categories the test uses (e.g., partial immunocompromise, intermittent occupational exposure) have no seat in the threshold-setting process; the test is calibrated at population level and their fine-grained circumstance is not represented in either the beneficiary or objector camp explicitly.
% DISAPPEARANCE_RATIONALE: Public health authorities and immunocompromised populations would say the world rearranges badly — without a principled threshold, mandates either vanish entirely (leaving high-risk groups exposed) or become unconstrained (inviting compulsion for trivial pathogens). Borderline objectors and denied exemption seekers would say the world barely changes for them personally in the moment of compulsion, since the proportionality test's fine distinctions are precisely what they experience as arbitrary line-drawing rather than protection.
% FOUNDING_PROBLEM: Courts and legislatures needed a principled middle path between two absolutist positions — unconditional mandate authority and unconditional bodily autonomy — that could be applied consistently across genuinely different pathogens (smallpox versus seasonal flu) without collapsing into ad hoc political decision-making.
% FOUNDING_PROBLEM_CORROBORATION: Comparative public health law scholarship (e.g., analyses of Jacobson v. Massachusetts progeny and its international analogues) attests the proportionality problem remains genuinely unresolved and recurs with each new pathogen; this corroboration comes from legal academics and bioethicists outside the administering health agencies, though it should be noted that much of the scholarship shares an institutional orientation favorable to some mandate authority existing at all.
narrative_ontology:disappearance_verdict(vaccine_mandate_balance__proportionality_reading, contested).
narrative_ontology:founding_problem_status(vaccine_mandate_balance__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_balance__proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_balance__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_balance__proportionality_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_balance__proportionality_reading_tests).
:- end_tests(vaccine_mandate_balance__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.38) is moderate and rises modestly over the interval as the threshold test accumulates case law and institutional habit — courts and agencies build precedent that makes borderline denials easier to justify by reference to prior borderline denials, a mild ratchet rather than a sharp one. Suppression (0.42) reflects that compulsion is real but bounded: unlike a categorical mandate regime, this reading's suppression is exercised only within the band the test authorizes, and it fluctuates as case law tightens and loosens the practical meaning of 'robust' exemption. Theater ratio (0.28) captures a real but non-trivial performative element — proportionality tests can be invoked as post-hoc rationalization for decisions already made on other grounds (political pressure, resource constraints), and courts' willingness to treat the test as substantive rather than pretextual varies. Accessibility collapse (0.40) is moderate: for those near the threshold, meaningful alternatives (delay, negotiate individualized accommodation) partly persist, unlike a mountain's near-total collapse. Resistance (0.55) is comparatively high because the threshold's contestability invites exactly the kind of litigation and public argument that a settled categorical rule would not.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities sit near the beneficiary end: they administer the test, gain legitimacy from its principled framing, and are functionally unconstrained in their own exit (they can revise the threshold's application). Immunocompromised populations are structural beneficiaries when the test favors mandates but are trapped and powerless — their benefit is conditional and precarious, not a a secure entitlement. Borderline objectors, narrowly-denied exemption seekers, and low-risk occupational workers are the targets: the extraction this reading authors is concentrated precisely on those whose case sits near the test's own boundaries, which is a structural feature of a proportionality test rather than an incidental one — the closer to the line, the more contestable the compulsion, and contestable compulsion is where this reading's ε lives.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading is designed as an anti-mandatrophy device relative to its siblings: it exists specifically to prevent the public-health-primary reading's categorical authorization from persisting past the point where disease parameters justify it (a mandate for a since-attenuated pathogen), and to prevent the bodily-autonomy-primary reading's categorical prohibition from blocking mandates in a genuine smallpox-level emergency. Its own mandatrophy risk is different: the test's founding problem (calibrating compulsion to genuine severity) remains live, but the institutional apparatus that applies it can accumulate precedent-driven inertia — denying exemptions because prior similar exemptions were denied, not because the current case was freshly weighed. The measurement series' mild upward drift in extractiveness models this precedent-hardening risk without asserting the founding problem itself has died.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_calibration_authority,
    'Who has legitimate authority to set the specific numeric or qualitative cut points for ''strict proportionality'' (what severity level, what transmission rate, what safety profile) — public health agencies, legislatures, or courts — and does the answer change the classification from tangled_rope toward rope (if courts genuinely discipline the test) or toward snare (if the test is administratively self-certifying)?',
    'Track judicial reversal rates on mandate challenges over the interval: a high reversal rate for insufficiently justified mandates indicates genuine external discipline of the threshold; a near-zero reversal rate indicates the test functions as rubber-stamp deference.',
    'If courts substantively discipline the threshold, the tangled_rope classification is well-supported (genuine coordination function with real constraint on extraction). If judicial review is consistently deferential, the framework drifts toward a snare wearing proportionality language as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_calibration_authority, empirical, 'Whether judicial review substantively disciplines the proportionality threshold or merely ratifies administrative determinations.').

omega_variable(
    pathogen_dependent_epsilon_variance,
    'Given that this reading''s own logic makes legitimacy pathogen-dependent, is a single ε value for ''the proportionality reading'' coherent, or does the reading itself require decomposition by pathogen class (smallpox-tier vs. seasonal-flu-tier mandates)?',
    'Author separate proportionality-reading constraint stories for high-severity (smallpox/measles-tier) and low-severity (seasonal flu-tier) pathogen applications if empirical ε divergence between them exceeds what a single story can honestly represent; per the ε-invariance principle, if measuring by pathogen severity class yields materially different ε, that is two constraints, not one.',
    'This story''s ε (0.38) represents an averaged/typical case across the threshold''s operating range; a smallpox-tier application of this same proportionality logic would likely show much lower ε (mandate clearly justified, minimal contestable extraction), while a seasonal-flu-tier application would show much higher ε (mandate barely justified if at all, extraction concentrated on nearly everyone compelled). This averaging is a modeling simplification flagged here rather than hidden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pathogen_dependent_epsilon_variance, conceptual, 'Whether the proportionality reading''s ε should be decomposed further by pathogen severity class rather than represented as a single value.').

omega_variable(
    robustness_bar_gaming,
    'Is the requirement that exemptions be ''robust'' a genuine safeguard against frivolous claims, or does it function primarily to suppress the exemption pathway while preserving its rhetorical existence?',
    'Compare exemption grant rates before and after ''robustness'' requirements were formalized in a given jurisdiction, controlling for underlying claim quality; a sharp drop in grant rates without a corresponding drop in claim merit indicates the bar functions as suppression theater.',
    'If robustness functions mainly as suppression, the theater_ratio for this specific mechanism is understated in the aggregate score and the exemption_seekers_denied_narrowly victim group''s extraction is higher than the story''s aggregate ε suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(robustness_bar_gaming, empirical, 'Whether the exemption robustness requirement is a genuine quality filter or a suppression mechanism dressed as due process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_balance__proportionality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_balance__proportionality_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(vacc_tr_t4, vaccine_mandate_balance__proportionality_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(vacc_tr_t8, vaccine_mandate_balance__proportionality_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_balance__proportionality_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(vacc_tr_t16, vaccine_mandate_balance__proportionality_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_balance__proportionality_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_balance__proportionality_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vacc_be_t4, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(vacc_be_t8, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(vacc_be_t16, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 16, 0.37).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_balance__proportionality_reading, base_extractiveness, 24, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t4, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(vacc_su_t8, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 8, 0.44).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(vacc_su_t16, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_balance__proportionality_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_balance__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, public_health_primary).
narrative_ontology:affects_constraint(vaccine_mandate_balance__proportionality_reading, bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the vaccine_mandate_balance kernel. public_health_primary authorizes mandates on a lower, herd-immunity-failure threshold (more permissive, higher ε for individual objectors, lower ε for collective risk-bearers). bodily_autonomy_primary forecloses compulsion categorically regardless of disease parameters (ε ≈ 0 for objectors under that reading's own framework, since no compulsion occurs). This proportionality reading occupies the contested middle with pathogen-dependent, threshold-gated legitimacy. Each story's ε is authored independently per the ε-invariance principle; they are linked via affects_constraints because the three readings compete for adoption in the same legal and policy discourse, and a court or legislature's adoption of one reading structurally forecloses or influences the operating space of the others in that jurisdiction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
