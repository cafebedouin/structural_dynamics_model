% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__proportionality_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: public_health_mandate_authority__proportionality_reading
 *   human_readable: Proportionality Sliding Scale for Public Health Mandate Legitimacy
 *   domain: public health law/constitutional rights/bioethics
 *
 * SUMMARY:
 *   The proportionality reading instantiates one of three live readings of
 *   the public health mandate authority kernel. The constraint it generates:
 *   the legitimacy of a coercive public health mandate is not categorical but
 *   assessed against a sliding scale — severity of the threat, availability
 *   of less coercive alternatives, magnitude of the coercion imposed, and
 *   duration of the imposition. The result is a dynamic constraint rather
 *   than a fixed victim boundary: the same compulsory measure that is
 *   legitimate against an Ebola-level pathogen is an uncompensated imposition
 *   against a mild seasonal respiratory virus, and which group bears the
 *   burden shifts with the assessment. The ε referent is the standing
 *   arrangement under contest — mandate authority as actually operated across
 *   the interval (routine healthcare-worker influenza requirements, the
 *   COVID-19 emergency enforcement surge, the post-emergency residue) —
 *   assessed by this reading's own lights: high-threat coercion is largely
 *   ratified, low-threat continuations and threat-inflated assessments are
 *   not. Sibling readings are separate constraints with their own ε and
 *   victim structures: public_health_primary (mandate as obligation to the
 *   vulnerable commons) and bodily_autonomy_primary (mandate as categorical
 *   bodily violation). The measured ε of 0.55 is a property of this reading
 *   alone. KEY AGENTS (by structural relationship): public_health_agencies
 *   (agenda-setter and beneficiary, institutional/constrained);
 *   constitutional_courts (agenda-setter of the test itself,
 *   institutional/analytical); unvaccinated_conscientious_objectors (target
 *   subset, moderate/identity_locked); unvaccinated_hesitant (target subset,
 *   powerless/constrained); immunocompromised_vulnerable (dual-positioned,
 *   powerless/trapped); medically_contraindicated_patients (excluded voice,
 *   powerless/trapped); civil_liberties_advocates (beneficiary,
 *   organized/mobile); bioethics_commissions (analytical observer,
 *   institutional/analytical).
 *
 * KEY AGENTS:
 *   - public_health_agencies: agenda-setter and beneficiary (institutional/constrained) — administers mandates and supplies the severity assessments the test consumes
 *   - constitutional_courts: agenda-setter of the test itself (institutional/analytical) — adjudicates the four factors, bears none of their costs
 *   - unvaccinated_conscientious_objectors: primary target subset (moderate/identity_locked) — refusal fused with religious or political identity
 *   - unvaccinated_hesitant: target subset (powerless/constrained) — bears the same exclusions without resources to contest them
 *   - immunocompromised_vulnerable: dual-positioned (powerless/trapped) — protected when the scale ratifies mandates, exposed when it withholds them
 *   - medically_contraindicated_patients: excluded voice (powerless/trapped) — mandates reach them with no compliance path; absent from the alternatives inquiry
 *   - civil_liberties_advocates: beneficiary (organized/mobile) — the factors are their litigation framework
 *   - bioethics_commissions: analytical observer (institutional/analytical) — publishes calibration frameworks, holds no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, 0.55).
domain_priors:suppression_score(public_health_mandate_authority__proportionality_reading, 0.5).
domain_priors:theater_ratio(public_health_mandate_authority__proportionality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(public_health_mandate_authority__proportionality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__proportionality_reading, "Proportionality Sliding Scale for Public Health Mandate Legitimacy").
narrative_ontology:topic_domain(public_health_mandate_authority__proportionality_reading, "public health law/constitutional rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__proportionality_reading, '73642f0b-4471-4d03-8e20-938f2905d6f1').
narrative_ontology:cs_kernel_codification('73642f0b-4471-4d03-8e20-938f2905d6f1', formalized).
narrative_ontology:cs_authority_grounding('73642f0b-4471-4d03-8e20-938f2905d6f1', lineage).
narrative_ontology:cs_interpretation_layer_present('73642f0b-4471-4d03-8e20-938f2905d6f1').
narrative_ontology:cs_reading_relation('73642f0b-4471-4d03-8e20-938f2905d6f1', public_health_mandate_authority__public_health_primary, influences).
narrative_ontology:cs_reading_relation('73642f0b-4471-4d03-8e20-938f2905d6f1', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_axiom('73642f0b-4471-4d03-8e20-938f2905d6f1', foundational, mandate_legitimacy_threat_conditional).
narrative_ontology:cs_axiom_status(mandate_legitimacy_threat_conditional, holdable).
narrative_ontology:cs_axiom_grounding('73642f0b-4471-4d03-8e20-938f2905d6f1', mandate_legitimacy_threat_conditional, deontological).
narrative_ontology:cs_axiom('73642f0b-4471-4d03-8e20-938f2905d6f1', foundational, least_restrictive_alternative_requirement).
narrative_ontology:cs_axiom_status(least_restrictive_alternative_requirement, holdable).
narrative_ontology:cs_axiom_grounding('73642f0b-4471-4d03-8e20-938f2905d6f1', least_restrictive_alternative_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('73642f0b-4471-4d03-8e20-938f2905d6f1', threat_calibrated_conditional_authority).
narrative_ontology:cs_drift_state('73642f0b-4471-4d03-8e20-938f2905d6f1', post_emergency_recalibration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73642f0b-4471-4d03-8e20-938f2905d6f1', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_conscientious_objectors).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, unvaccinated_hesitant).
narrative_ontology:constraint_victim(public_health_mandate_authority__proportionality_reading, immunocompromised_vulnerable).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__proportionality_reading, immunocompromised_vulnerable).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, proportionality_principle).
narrative_ontology:constraint_vindicates(public_health_mandate_authority__proportionality_reading, least_restrictive_means_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assess pathogen severity, design and impose vaccination, testing, and isolation requirements, and defend their necessity and calibration in litigation. Their epidemiological assessments are the primary evidentiary input courts weigh in mandate challenges. They retain conditional authority to compel compliance and bear the burden of documenting that the threat justifies the means imposed; their severity assessments are rarely independently re-derived before enforcement begins.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, public_health_agencies, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, public_health_agencies, beneficiary).

% Hear challenges to mandates and decide whether the imposed means fit the demonstrated threat. They articulate and revise the factors courts weigh, and their rulings determine which requirements stand or fall. They bear no direct cost from the requirements they review and receive no direct protection from the mandates they uphold.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Refuse vaccination on principled or religious grounds and lose employment, venue access, or freedom of movement as a result. For a substantial subset, refusal is fused with religious or political identity such that compliance is not a live option at any price; others in this group litigate or relocate. The justification for what they bear shifts with official threat assessments they do not produce and cannot contest on the merits.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_conscientious_objectors, payer,
    moderate, biographical, identity_locked, national).

% Delay or decline vaccination out of distrust, access barriers, or inertia, and face the same exclusions and penalties as principled objectors with fewer resources to contest them. They comply when penalties rise, relocation is rarely feasible, and their exposure tracks enforcement intensity rather than their own risk choices.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, unvaccinated_hesitant, payer,
    powerless, biographical, constrained, national).

% Cannot rely on their own vaccination for full protection and depend on high community coverage and isolation requirements for safety. When requirements are imposed and maintained they gain protection; when assessments conclude the threat no longer justifies them, or when they are lifted prematurely, they bear the resulting infection risk directly. There is no individual purchase equivalent to the collective protection they depend on.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, immunocompromised_vulnerable, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(public_health_mandate_authority__proportionality_reading, immunocompromised_vulnerable, beneficiary).

% Cannot be vaccinated for documented medical reasons, so requirements aimed at the unvaccinated reach them with no compliance path available — employment exclusion or venue bans they cannot satisfy by any choice of their own. They are rarely called to testify about what accommodations actually exist when courts weigh the availability of less restrictive alternatives.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, medically_contraindicated_patients, excluded,
    powerless, biographical, trapped, national).

% Litigate against mandates they judge overbroad and defend the requirement that the state demonstrate necessity, proportionate means, and time limits. Their docket and doctrinal influence depend on courts taking the calibration factors seriously; they pivot to other causes when mandate activity subsides.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, civil_liberties_advocates, beneficiary,
    organized, generational, mobile, national).

% Publish frameworks and guidance on when coercive public health measures are ethically defensible, advise legislatures and agencies, and evaluate past mandate episodes. They hold no enforcement power and receive no compliance; their influence runs through argument, appointment, and retrospective review.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__proportionality_reading, bioethics_commissions, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_health_mandate_authority__proportionality_reading, public_health_agencies).
narrative_ontology:fixing_cost_class(public_health_mandate_authority__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a real adjudication problem: how to decide which coercive health measures are justified without either rubber-stamping agency judgment or banning non-consensual intervention outright. The four factors give courts a shared, revisable test, give agencies a legitimacy framework whose demands scale with circumstances, and coordinate citizen expectations about when compliance can be compelled.
% TRANSFER_FUNCTION: Moves decision authority over bodies, employment, and movement from individuals to public health authorities, conditional on the state's demonstrated threat assessment. In enforcement phases it transfers compliance and liberty from mandate subjects toward collective protection goals; it also transfers litigation costs and adjudication burden onto challengers.
% ABSENT_VOICES: Medically contraindicated patients — for whom no compliance path exists — are absent from the availability-of-alternatives inquiry. Populations outside the issuing jurisdiction affected by precedent-setting rulings have no seat. Principled objectors appear in court as rights-claimants rather than as agents with reasons, so the record rarely contains their actual deliberative position.
% DISAPPEARANCE_RATIONALE: If the sliding scale vanished overnight, courts would fall back to categorical rules — near-deferential rational-basis review or near-categorical bodily-integrity protection — and mandate law would reorganize around whichever extreme each jurisdiction picked. Agencies would lose the framework they satisfy to retain coercive authority, challengers would lose the factors they litigate under, and the current settlement in which mandate power is conditional and revisable would collapse into fixed rules.
% FOUNDING_PROBLEM: How to reconcile the state's police power over the body with individual liberty during epidemics — crystallized in the early twentieth-century compulsory vaccination litigation, where courts needed a test that neither rubber-stamped health officers nor categorically forbade non-consensual intervention.
% FOUNDING_PROBLEM_CORROBORATION: Courts across jurisdictions attest the calibration problem in opinions both upholding and striking mandates; the 1985 Siracusa Principles codify proportionality factors for public health emergencies from outside any national benefiting party; bioethics scholarship and civil liberties litigants on both sides of mandate disputes treat the factors as the operative framework. Public health agencies also attest the problem is live, but the corroboration does not rest on them.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(public_health_mandate_authority__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__proportionality_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.55) because the standing arrangement mixes ratified coercion with unratified: during severe-threat phases large impositions are proportionate under this reading's own lights, while low-threat continuations, healthcare-worker requirements maintained past the emergency, and assessments produced by the agencies they justify are not. Suppression (0.5) is real but contingent — employment exclusion, venue bans, fines — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream by the engine. Theater (0.4) reflects a test genuinely applied in landmark rulings but increasingly dominated by the severity factor, with alternatives, coercion-magnitude, and duration functioning as decoration in most opinions. Accessibility collapse (0.45): within adjudication the four factors crowd out categorical arguments, but voluntary, targeted, and accommodative policy alternatives remain live outside the courtroom. Resistance (0.55): categorical objector movements refuse the scale's premises, agencies resist calibration through the assessments they control, and legislatures oscillate. The measurement series runs on one shared grid and traces epidemic-wave oscillation rather than monotonic drift: extraction dips when threat is high and coercion is ratified (t4, the 2009 pandemic), rises in inter-epidemic lulls (t8–t12), peaks in mixed proportionality at maximum enforcement (t16, the COVID surge), and climbs further as residual mandates persist past the threat that justified them (t20). The oscillation's extractive mechanism is the phase lag — coercion persisting after its justification expires — not the waves themselves; intermittent enforcement against an identity-fused population also functions as intermittent reinforcement. Base_properties were measured at interval end (t20), on the rising-extraction, post-emergency segment of the cycle.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the agency seat the sliding scale is a legitimacy framework it satisfies and, through its control of severity assessments, partly writes. From the court seat it is a workable doctrine that disciplines without deciding. From the objector seat it is a moving target that legitimizes coercion in proportion to assessments the objector does not produce and cannot contest on the merits. From the immunocompromised seat it is a protection guarantee that can be withdrawn by the same analysis that grants it. Courts and agencies hold the same power atom but different exit options (analytical vs constrained) and opposite relationships to the test — author and subject — which is why the inter-institutional pair computes asymmetrically despite equal nominal standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive d. Agencies are declared beneficiaries (they retain conditional coercive authority) and derive low d; civil liberties advocates are beneficiaries with arbitrage-grade exit and derive low d. The three victim groups derive high d: conscientious objectors (identity-locked, which sits them nearer the full-target end than mobile objectors would sit), hesitant objectors (constrained), and the immunocompromised (trapped). The immunocompromised seat is genuinely dual-positioned — protected when the scale ratifies mandates, exposed when it withholds them — and the derivation places them in the victim set per the declared structure; the dynamic boundary is carried by the dynamic_victim_boundary_location omega rather than a directionality override, because an override keyed to a power atom would also move the medically contraindicated, whose position as pure imposition-bearers is not dynamic. No overrides are used: the derivation chain produces the right d from the declared structure for every seat. On the receipt surface: in the extractive phases the gains (retained coercive authority, compliance) demonstrably accrue to public_health_agencies; fixing the gaming channel would require rebuilding the doctrinal settlement across jurisdictions, which is prohibitive relative to the benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification resists two mislabels. Reading the scale as pure coordination would miss the asymmetric extraction: the party the test disciplines (the agency) also supplies its decisive input, so the coordination function carries a built-in gaming channel, and the burdened set pays adjudication costs for a framework they did not design. Reading it as pure extraction would miss the genuine coordination: categorical rules are worse for every seat including the objectors' own long-run position, the test has struck real mandates, and its function is exercised, not vestigial. Tangled rope holds both halves. On obsolescence: the founding problem (calibrating police power to epidemic threat) is live — pathogens recur — so no zombie flag fires. But the post-emergency segment of the series (rising theater and extraction at t20 as residual mandates persist under low threat) is where piton-flavored drift would begin if threat stays low and the enforcement machinery persists performatively; the theater_ratio series is the early-warning trace of exactly that transition. The mandate has not outlived its function; parts of its current enforcement have. Identity-lock dynamics: if the conscientious-objector identity frame broke — if refusal decoupled from religious or political self-concept — that seat's exit atom would shift from identity_locked toward constrained, effective extraction on it would fall, and the payer seat would come to resemble ordinary regulated parties rather than a locked population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_assessment_gaming,
    'Do agency threat-severity assessments that drive the sliding scale track epidemiological reality or institutional self-interest?',
    'Independent re-derivation of the severity assessments underlying major mandate decisions against contemporaneous surveillance data, with attention to assessments made after enforcement had already begun.',
    'If assessments are systematically inflated, the scale operates as a rubber stamp, the doctrine''s extractive component is larger than the measured base rate, and judicial calibration is largely theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_assessment_gaming, empirical, 'Whether the test''s primary input is honest measurement or self-serving assessment.').

omega_variable(
    dynamic_victim_boundary_location,
    'At what point on the threat scale does a mandate cross from legitimate collective protection to uncompensated imposition — and who sits in the burdened set on each side?',
    'Correlate case-level proportionality outcomes with contemporaneous threat metrics (transmission, severity, health-system load) to locate the empirical crossing band and identify which groups bear uncompensated burden on each side of it.',
    'The victim set is not fixed: at severe-threat assessments the burdened set is mandate resisters bearing ratified coercion; at mild-threat assessments the same people bear unratified coercion, while the immunocompromised enter the burdened set when protection is withheld. Resolving the band determines whether this reading''s extraction concentrates on objectors or on the vulnerable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dynamic_victim_boundary_location, conceptual, 'Where the legitimacy threshold sits and which group is burdened on each side of it.').

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is the proportionality_reading of the public_health_mandate_authority kernel; would the sibling readings (public_health_primary, bodily_autonomy_primary) change the structural classification?',
    'Generate the sibling readings as separate constraint stories and compare computed types and victim sets: the obligation reading fixes victims at mandate resisters with low measured extraction; the categorical sovereignty reading fixes victims at all mandate subjects with high measured extraction; this reading makes both victim sets conditional on the four-factor assessment.',
    'The disagreement is located in whether mandate legitimacy is categorical or conditional. Under the obligation reading this reading''s measured extraction largely disappears (mandates are duties); under the sovereignty reading it approaches totality (all mandates are violations). The sliding-scale ε of 0.55 is a property of this reading alone, not of the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Committer structure: one of three readings; epsilon and victim structure are reading-indexed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the pressure mandate subjects experience structural (employment exclusion, venue bans, fines) or partly internalized (identity fusion that makes compliance unthinkable regardless of penalty)?',
    'Compare compliance trajectories after penalty escalation across objector subgroups: purely cost-responsive groups converge toward compliance as penalties rise; identity-fused groups hold at high cost. Post-mandate compliance surveys and behavioral data decompose the two mechanisms.',
    'If a large share of the objector seat is identity-fused, effective pressure on that seat exceeds the structural measure and persists after enforcement is relaxed; the payer seat''s exit atom should be read as heterogeneous rather than uniform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Decomposition of the constraint''s suppressive force into external barriers and internalized commitment.').

omega_variable(
    factor_independence_vs_severity_dominance,
    'Are the four factors (severity, alternatives, coercion magnitude, duration) assessed independently, or does severity dominate so that the other three are decorative?',
    'Code judicial mandate opinions for factor-by-factor analysis: count opinions in which the alternatives, coercion-magnitude, or duration factors changed the outcome independently of the severity finding.',
    'If severity dominates, the scale''s coordination function is thinner than it appears, the theater ratio is higher than measured, and the doctrine drifts toward deference dressed as calibration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(factor_independence_vs_severity_dominance, empirical, 'Whether the test is a genuine four-factor calibration or severity review with extra steps.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__proportionality_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(publ_tr_t0, observed).
narrative_ontology:measurement(publ_tr_t4, public_health_mandate_authority__proportionality_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement_basis(publ_tr_t4, observed).
narrative_ontology:measurement(publ_tr_t8, public_health_mandate_authority__proportionality_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement_basis(publ_tr_t8, observed).
narrative_ontology:measurement(publ_tr_t12, public_health_mandate_authority__proportionality_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(publ_tr_t12, observed).
narrative_ontology:measurement(publ_tr_t16, public_health_mandate_authority__proportionality_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement_basis(publ_tr_t16, observed).
narrative_ontology:measurement(publ_tr_t20, public_health_mandate_authority__proportionality_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(publ_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__proportionality_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(publ_be_t0, observed).
narrative_ontology:measurement(publ_be_t4, public_health_mandate_authority__proportionality_reading, base_extractiveness, 4, 0.38).
narrative_ontology:measurement_basis(publ_be_t4, observed).
narrative_ontology:measurement(publ_be_t8, public_health_mandate_authority__proportionality_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement_basis(publ_be_t8, observed).
narrative_ontology:measurement(publ_be_t12, public_health_mandate_authority__proportionality_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement_basis(publ_be_t12, observed).
narrative_ontology:measurement(publ_be_t16, public_health_mandate_authority__proportionality_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement_basis(publ_be_t16, observed).
narrative_ontology:measurement(publ_be_t20, public_health_mandate_authority__proportionality_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(publ_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__proportionality_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(publ_su_t0, observed).
narrative_ontology:measurement(publ_su_t4, public_health_mandate_authority__proportionality_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement_basis(publ_su_t4, observed).
narrative_ontology:measurement(publ_su_t8, public_health_mandate_authority__proportionality_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement_basis(publ_su_t8, observed).
narrative_ontology:measurement(publ_su_t12, public_health_mandate_authority__proportionality_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(publ_su_t12, observed).
narrative_ontology:measurement(publ_su_t16, public_health_mandate_authority__proportionality_reading, suppression_requirement, 16, 0.72).
narrative_ontology:measurement_basis(publ_su_t16, observed).
narrative_ontology:measurement(publ_su_t20, public_health_mandate_authority__proportionality_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(publ_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__public_health_primary).
narrative_ontology:affects_constraint(public_health_mandate_authority__proportionality_reading, public_health_mandate_authority__bodily_autonomy_primary).

% DUAL FORMULATION NOTE:
% Constraint family: the public_health_mandate_authority kernel decomposes into three reading-stories — public_health_primary, proportionality_reading (this file), and bodily_autonomy_primary — because a single story cannot hold a stable ε across categorical and conditional legitimacy claims (ε-invariance: measuring mandate legitimacy by an unconditional-obligation observable yields low ε, by an absolute-sovereignty observable yields high ε, so these are different constraints, not one constraint viewed two ways). This reading sits structurally between the siblings: it ratifies part of what the obligation reading endorses and vetoes part of what the sovereignty reading prohibits, so its ε (0.55) and its conditional victim set differ from both. The obligation reading is upstream (the traditional settlement this reading refines and disciplines); this file links both siblings via affects_constraints, and the sibling files should link back.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
