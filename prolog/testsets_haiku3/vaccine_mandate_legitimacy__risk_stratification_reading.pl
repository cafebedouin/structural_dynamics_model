% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__risk_stratification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__risk_stratification_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: vaccine_mandate_legitimacy__risk_stratification_reading
 *   human_readable: Vaccine Mandate Legitimacy: Risk-Stratification Reading
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the vaccine mandate
 *   legitimacy kernel: the risk-stratification reading. It holds that state
 *   coercion via vaccine mandate is justified ONLY when the target
 *   population's actuarial risk from vaccine-preventable disease meets a
 *   threshold sufficient to override proportionality objections. Blanket
 *   mandates (applied to all citizens regardless of risk) fail this test and
 *   are therefore illegitimate under this reading. Targeted mandates (applied
 *   to high-risk strata: elderly, immunocompromised, occupationally exposed)
 *   pass the test and are legitimate. The constraint's legitimacy is
 *   contingent on accurate risk quantification; disagreement over risk models
 *   translates into disagreement over mandate scope.
 *
 * KEY AGENTS:
 *   - public_health_authority: agenda-setter, defines and enforces mandate scope; faces pressure to stratify rather than blanket
 *   - high_risk_populations: beneficiaries, protected by mandate when properly targeted; cannot exit without catastrophic personal risk
 *   - low_risk_individuals: payers, bear mandate cost for populations they are not part of; exit constrained by occupational/access rules
 *   - epidemiologists: observers, provide risk quantification that becomes the boundary-setting mechanism; disagreement over models is disagreement over legitimacy
 *   - courts: observers, adjudicate whether mandates meet proportionality threshold
 *   - civil_liberties_advocates: excluded, hold bodily_autonomy_primacy_reading; would argue any mandate is illegitimate
 *   - public_health_maximalists: excluded, hold public_health_primacy_reading; would argue aggregate benefit justifies blanket mandates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, 0.58).
domain_priors:suppression_score(vaccine_mandate_legitimacy__risk_stratification_reading, 0.62).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__risk_stratification_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__risk_stratification_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__risk_stratification_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__risk_stratification_reading, "Vaccine Mandate Legitimacy: Risk-Stratification Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__risk_stratification_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__risk_stratification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__risk_stratification_reading, '5615554e-2163-4af1-9134-9bb8498e3a83').
narrative_ontology:cs_kernel_codification('5615554e-2163-4af1-9134-9bb8498e3a83', formalized).
narrative_ontology:cs_authority_grounding('5615554e-2163-4af1-9134-9bb8498e3a83', lineage).
narrative_ontology:cs_interpretation_layer_present('5615554e-2163-4af1-9134-9bb8498e3a83').
narrative_ontology:cs_reading_relation('5615554e-2163-4af1-9134-9bb8498e3a83', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, influences).
narrative_ontology:cs_reading_relation('5615554e-2163-4af1-9134-9bb8498e3a83', vaccine_mandate_legitimacy__public_health_primacy_reading, influences).
narrative_ontology:cs_axiom('5615554e-2163-4af1-9134-9bb8498e3a83', foundational, proportionality_constrains_coercive_mandate_scope).
narrative_ontology:cs_axiom_status(proportionality_constrains_coercive_mandate_scope, holdable).
narrative_ontology:cs_axiom_grounding('5615554e-2163-4af1-9134-9bb8498e3a83', proportionality_constrains_coercive_mandate_scope, deontological).
narrative_ontology:cs_axiom('5615554e-2163-4af1-9134-9bb8498e3a83', foundational, actuarial_risk_stratification_is_legitimacy_requirement).
narrative_ontology:cs_axiom_status(actuarial_risk_stratification_is_legitimacy_requirement, holdable).
narrative_ontology:cs_axiom_grounding('5615554e-2163-4af1-9134-9bb8498e3a83', actuarial_risk_stratification_is_legitimacy_requirement, empirically_contingent).
narrative_ontology:cs_reference_frame('5615554e-2163-4af1-9134-9bb8498e3a83', proportionality_constrained_public_health).
narrative_ontology:cs_drift_state('5615554e-2163-4af1-9134-9bb8498e3a83', contemporary_post_pandemic, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5615554e-2163-4af1-9134-9bb8498e3a83', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_populations).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, immunocompromised_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_individuals_under_blanket_mandate).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_with_medical_contraindications).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__risk_stratification_reading, moderate_risk_populations).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__risk_stratification_reading, moderate_risk_populations).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, proportionality_principle_in_public_health_coercion).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__risk_stratification_reading, risk_differentiation_as_legitimacy_constraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proposes and implements vaccine mandates, typically presented as blanket policies protecting collective health. This reading constrains that authority: only mandates targeted to actuarial risk strata can be justified; blanket mandates fail proportionality scrutiny under this framework. The authority faces pressure to implement risk-stratified policies rather than population-wide requirements, which requires epidemiological assessment and ongoing threshold calibration.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Elderly, immunocompromised, and medically vulnerable individuals who benefit from mandate protection when the mandate is properly stratified to their risk category. Under this reading, they are the legitimate targets of coercive policy; the mandate's justification derives from their protection. Exit (refusing vaccination) exposes them to catastrophic personal harm; the constraint functions as their protection mechanism.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, high_risk_populations, beneficiary,
    powerless, biographical, trapped, national).

% Younger, healthier individuals whose individualized actuarial risk from vaccine-preventable disease is substantially lower than the population average. Under a blanket mandate, they bear coercive policy cost (vaccination requirement, occupational or access restrictions if non-compliant) that is not justified by their personal risk profile under this reading. Their exit options include medical exemption (if genuine contraindication exists) or geographic exit (moving to non-mandate jurisdiction), but both are costly and incomplete.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, low_risk_individuals_under_blanket_mandate, payer,
    moderate, biographical, constrained, national).

% People with documented medical conditions (severe prior allergic reactions, myocarditis history, rare genetic disorders) that make vaccination medically inadvisable. Under a properly stratified system, they receive exemption. Under a blanket mandate, they face the coercive choice: vaccinate despite contraindication (risking serious harm) or accept occupational/access restrictions. Their exit is constrained by the medical identity itself — they cannot simply change risk category.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, individuals_with_medical_contraindications, payer,
    moderate, biographical, identity_locked, national).

% Working-age adults with occupational exposure, chronic comorbidities, or household vulnerability to severe disease. Under risk stratification, they fall into intermediate mandate zones where the actuarial case for coercion is present but weaker than for highest-risk groups. They receive mandate protection calibrated to their genuine risk; they also bear the cost of compliance. The constraint's legitimacy hinges on whether their assigned stratum is accurately calibrated.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, moderate_risk_populations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, moderate_risk_populations, payer).

% Technical authorities who must define the risk thresholds that distinguish legitimate from illegitimate mandates under this reading. They provide the actuarial data and model predictions; their work becomes the boundary-setting mechanism. Disagreement over model assumptions or risk quantification translates directly into disagreement over which populations fall into mandatory vs. voluntary categories.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, epidemiologists_and_risk_modelers, observer,
    analytical, biographical, analytical, national).

% Adjudicate whether mandates meet the proportionality constraint this reading instantiates. They must evaluate whether the public health authority's risk stratification is sound, whether the threshold is appropriately calibrated, and whether the coercive scope matches the actuarial justification. Their role is to invalidate blanket mandates while permitting targeted ones.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, courts_and_constitutional_reviewers, observer,
    institutional, generational, analytical, national).

% Enact mandate legislation or delegate authority to health agencies. Under this reading, they face pressure to legislate risk thresholds explicitly rather than grant blanket discretion. They may resist (preferring the administrative simplicity of blanket mandates) or accommodate (building stratification into the statutory mandate). Their choices determine whether the constraint functions as a real limitation or devolves into theater.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, political_representatives, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__risk_stratification_reading, political_representatives, observer).

% Organizations advocating bodily autonomy as an absolute constraint, independent of risk. They would argue this reading concedes too much — that any state-mandated vaccination is illegitimate, regardless of risk stratification. Their position (bodily_autonomy_primacy_reading) is excluded from the decision-making that implements this reading, though they litigate and lobby to shift the kernel frame.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, civil_liberties_advocates, excluded,
    organized, generational, constrained, national).

% Advocates arguing for public health authority to mandate on the basis of collective benefit alone, treating individual risk stratification as an administrative burden that compromises effectiveness. They hold the public_health_primacy_reading, which this reading constrains. Their exclusion is structural: the risk-stratification reading subordinates aggregate health outcomes to individual proportionality.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_maximalists, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__risk_stratification_reading, public_health_authority).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__risk_stratification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables public health intervention against vaccine-preventable disease while respecting proportionality constraints: society coordinates on vaccination as the mechanism of disease control, but only applies coercion to populations whose personal actuarial risk is substantial enough to justify state intrusion into bodily autonomy. The coordination problem is: how to protect high-risk groups without blanket coercion that loses legitimacy across the population.
% TRANSFER_FUNCTION: Transfers bodily autonomy and occupational/social freedom from low-risk individuals to a collective good (disease prevention) in high-risk strata, but only where the actuarial case justifies it. The constraint limits WHERE that transfer occurs: high-risk yes, low-risk no. Payment is not monetary but experiential — vaccination burden, occupational restrictions, surveillance infrastructure.
% ABSENT_VOICES: Individuals medically unable to voice their concerns (severely immunocompromised, non-English speakers in policy discussions, incarcerated populations) are often absent from risk-stratification deliberations. Those who distrust the risk-quantification process itself — who reject the epidemiological premises — are structurally excluded because their dissent would dispute the reading's foundational axiom.
% DISAPPEARANCE_RATIONALE: If this constraint vanished and mandates reverted to blanket authority (or no mandates), vaccine coverage would fragment by risk category and jurisdiction; public health authority would either collapse into maximalism (blanket mandates justified by aggregate benefit) or into autonomy-primacy (no mandates); epidemiological outcomes would diverge sharply by risk stratum, with catastrophic mortality in high-risk populations where mandate removal eliminated protective pressure.
% FOUNDING_PROBLEM: Early pandemic policy imposed uniform vaccine mandates across populations with vastly different actuarial risk from vaccine-preventable disease, creating proportionality objections (why mandate vaccination for low-risk youth?) that delegitimized mandates broadly and motivated resistance even in high-risk populations where mandates were justified.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and bioethicists outside the public-health-maximalist camp (constitutional law reviewers, human rights organizations, medical ethicists emphasizing autonomy) attest the proportionality problem. Epidemiological data confirms risk heterogeneity. This attests the problem is real; disagreement exists only over whether proportionality constrains mandate authority (this reading says yes; public_health_primacy says no).
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__risk_stratification_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__risk_stratification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__risk_stratification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__risk_stratification_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__risk_stratification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__risk_stratification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.58 at interval end) because the constraint requires public health authority to defend mandate scope using actuarial data, which creates friction and administrative cost that authority often resists or evades. Early pandemic policy imposed blanket mandates precisely because stratification is harder; the constraint's enforcement would increase that friction. Suppression is similarly elevated (0.62) because implementing stratification requires surveillance infrastructure (risk category identification, occupational exposure documentation, exemption adjudication) and policing of boundaries — who really qualifies for mandatory vs. voluntary categories. Theater rises over the interval (0.25 to 0.41) as mandates are implemented: early phases showed genuine risk-based logic; later phases show more performative invocations of risk while boundary enforcement loosens or tightens for non-epidemiological reasons (political pressure, litigation risk, public compliance fatigue). The temporal measurements share one grid: every metric is authored at every time point, enabling the engine to detect drift type and rate.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats experience radically different constraint dynamics. High-risk beneficiaries see this reading as a constraint ON public authority (preventing it from abandoning them when low-risk populations resist mandates). Low-risk payers see it as a constraint AGAINST them (justifying coercion via epidemiological framing that treats their individual risk as irrelevant). The public health authority sees it as either a helpful legitimacy-maintenance device (by excluding low-risk populations from mandate scope, the constraint preserves mandate compliance in high-risk populations) or as a constraint reducing its policy flexibility (inability to impose blanket mandates that would be simpler to administer). Courts and epidemiologists see it as a technical constraint requiring accurate risk quantification — where disagreement over risk models becomes disagreement over legitimacy. The engine computes these as different directionalities: high-risk beneficiaries get d near 0 (the constraint subsidizes their protection); low-risk payers get d near 1 (the constraint keeps them inside coercive scope); authority gets d shifted by the strategic tension between simplicity and legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   High-risk populations declare as beneficiaries: the mandate exists to protect them. Their directionality is low (d ≈ 0.2) because the constraint flows in their favor — it ensures mandates target them and prevents abandonment. Low-risk individuals_under_blanket_mandate declare as victims: they are excluded from the mandate's beneficiary scope but included in its coercive reach when blanket mandates override proportionality. Their directionality is high (d ≈ 0.85) because they bear mandate cost that the constraint claims is unjustified for them. Individuals with medical contraindications are similarly targets (d ≈ 0.8) because the constraint traps them in an exemption category that may not be honored under blanket mandates. The public health authority sits at moderate directionality (d ≈ 0.5–0.6): the constraint expands its mandate-setting discretion (can enforce more legitimately by stratifying) but also constrains it (cannot use blanket authority). Moderate-risk populations are ambiguous; they are authorized-beneficiaries (the mandate targets them) but also bear costs (compliance, surveillance, occupational restrictions); their directionality depends on whether the actuarial case for their stratum is compelling (if yes, d ≈ 0.3; if marginal, d ≈ 0.65).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids the mandatrophy trap by grounding legitimacy in a live, contested problem: whether proportionality constrains coercive public health authority. The founding problem is real — early blanket mandates did generate proportionality objections — and the founding problem status is LIVE (risk stratification is an active policy debate). The constraint's function is not atrophied; it remains contested between this reading (proportionality constrains mandate scope) and the public_health_primacy_reading (collective benefit justifies blanket mandates). The theater ratio creeps upward (0.25 → 0.41) suggesting some policy theaters adopt risk-stratification language while retaining blanket enforcement underneath, but this is a degenerate version of the constraint, not the constraint itself. Mandatrophy would occur if jurisdictions formally committed to risk stratification but never actually implemented it — mandates remain blanket in practice while officially defending them as stratified. The constraint's test is whether the authorized mechanism (risk quantification and stratified scope) is genuinely deployed or merely performed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    risk_threshold_calibration,
    'What actuarial risk profile justifies mandate legitimacy under this reading — is it defined by absolute disease severity, relative population risk, occupational exposure, age cohort, or a weighted combination? Where is the threshold drawn, and what changes it?',
    'Comparative constitutional and regulatory analysis across jurisdictions implementing risk stratification; expert testimony from epidemiologists and ethicists on threshold defensibility; empirical outcomes comparing mandate compliance in stratified vs. blanket systems.',
    'If thresholds are subjective or politically malleable, the constraint dissolves into theater — mandates are called ''stratified'' while the threshold itself is the actual policy lever. If thresholds are transparent and technically justified, the constraint has real bite. The victim set size (low_risk_individuals) depends directly on threshold placement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(risk_threshold_calibration, empirical, 'Actuarial threshold definition and its policy implications').

omega_variable(
    epistemic_authority_over_risk_quantification,
    'Who has the authority to define which risk models are legitimate for mandate-scope decisions — public health agencies, courts, international bodies, affected populations, epidemiologists? Disagreement over model assumptions is disagreement over mandate legitimacy.',
    'Constitutional and statutory precedent on administrative deference; case law from mandate challenges questioning risk model premises; testimony from non-government epidemiologists on model consensus or contestation.',
    'If public health agencies own risk quantification without external review, this reading''s constraint becomes advisory only — agencies define stratification to suit their policy preferences. If courts or independent bodies can contest risk models, the constraint has enforcement power. This determines whether stratification is a real limitation or a legitimacy cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_authority_over_risk_quantification, conceptual, 'Authority structure governing risk model selection and revision').

omega_variable(
    proportionality_vs_collective_benefit_boundary,
    'Can this reading''s proportionality constraint coexist with public_health_primacy_reading''s collective-benefit mandate, or do they foreclose each other when both are applied to the same population?',
    'Theoretical: examine whether a mandate justified by public health (collective harm reduction) can simultaneously fail proportionality (individual burden exceeds individual risk). If yes, the readings coexist and influence each other; if no, they foreclose and one must yield.',
    'If they coexist, policy can oscillate between readings depending on political pressure. If they foreclose, jurisdictions must choose one reading, and constitutional doctrine will embed that choice. The victim set (who pays mandate cost) is stable under coexistence, unstable under foreclosure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_vs_collective_benefit_boundary, conceptual, 'Logical compatibility of proportionality and collective-benefit framings').

omega_variable(
    suppression_mechanism_in_stratification,
    'Is the elevated suppression (0.62) structural (surveillance infrastructure necessary to implement stratification) or internalized (individuals internalize the risk-category assessment and comply without external coercion)?',
    'Empirical: compare suppression requirements in jurisdictions with transparent risk stratification vs. opaque or poorly-explained stratification; measure compliance and occupational restriction enforcement; post-mandate removal, measure trajectory of residual vaccine skepticism.',
    'If structural, fixing the constraint requires reducing surveillance. If internalized, suppression persists after mandate removal because individuals have adopted the risk-category frame. Mixed: surveilled agents may internalize the frame over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_stratification, empirical, 'Structural vs. internalized suppression in risk-stratification mandates').

omega_variable(
    identity_lock_in_contraindication_category,
    'Do individuals with medical contraindications experience their exclusion from mandates as liberation or as identity-lock — as they fused their self-concept with ''medically different'' or ''unvaccinated,'' does the exemption become their primary identity even when the medical reason resolves?',
    'Longitudinal study of exemption-classified individuals post-pandemic; measure vaccine uptake trajectories; qualitative interviews on how exemption status affected identity and future health choices.',
    'If identity-lock occurs, the constraint''s victim mitigation (exemptions for contraindicated individuals) may backfire by cementing a lasting ''unvaccinated'' identity. If liberation occurs, exemptions resolve the constraint without creating downstream extraction. This affects whether the constraint''s injustice is temporary (mandate period) or durable (identity persistence).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_contraindication_category, empirical, 'Identity-lock dynamics in medical exemption categories').

omega_variable(
    kernel_contest_sibling_foreclosure,
    'Do the three readings (bodily_autonomy_primacy, public_health_primacy, risk_stratification) logically foreclose each other, or can they coexist as competing constitutional doctrines held by different parties?',
    'Constitutional and judicial analysis: examine whether accepting one reading''s core premise requires rejecting another''s. Empirical: observe whether jurisdictions adopt different readings over time, or whether constitutional entrenchment locks one in.',
    'If they foreclose: this reading either wins and becomes hegemonic (bodily_autonomy and public_health readings become minority dissents), or loses (one of the other readings wins). If coexistence persists: the kernel remains contested, mandates oscillate between readings depending on political moment, and this reading functions as a compromise position that all parties resist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_contest_sibling_foreclosure, conceptual, 'Logical foreclosure or coexistence of the three sibling readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__risk_stratification_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(vacc_tr_t0, observed).
narrative_ontology:measurement(vacc_tr_t3, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(vacc_tr_t3, observed).
narrative_ontology:measurement(vacc_tr_t6, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(vacc_tr_t6, observed).
narrative_ontology:measurement(vacc_tr_t12, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement_basis(vacc_tr_t12, observed).
narrative_ontology:measurement(vacc_tr_t18, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 18, 0.41).
narrative_ontology:measurement_basis(vacc_tr_t18, observed).
narrative_ontology:measurement(vacc_tr_t24, vaccine_mandate_legitimacy__risk_stratification_reading, theater_ratio, 24, 0.41).
narrative_ontology:measurement_basis(vacc_tr_t24, observed).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(vacc_be_t0, observed).
narrative_ontology:measurement(vacc_be_t3, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement_basis(vacc_be_t3, observed).
narrative_ontology:measurement(vacc_be_t6, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 6, 0.52).
narrative_ontology:measurement_basis(vacc_be_t6, observed).
narrative_ontology:measurement(vacc_be_t12, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(vacc_be_t12, observed).
narrative_ontology:measurement(vacc_be_t18, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement_basis(vacc_be_t18, observed).
narrative_ontology:measurement(vacc_be_t24, vaccine_mandate_legitimacy__risk_stratification_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement_basis(vacc_be_t24, observed).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(vacc_su_t0, observed).
narrative_ontology:measurement(vacc_su_t3, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 3, 0.52).
narrative_ontology:measurement_basis(vacc_su_t3, observed).
narrative_ontology:measurement(vacc_su_t6, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 6, 0.56).
narrative_ontology:measurement_basis(vacc_su_t6, observed).
narrative_ontology:measurement(vacc_su_t12, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement_basis(vacc_su_t12, observed).
narrative_ontology:measurement(vacc_su_t18, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 18, 0.62).
narrative_ontology:measurement_basis(vacc_su_t18, observed).
narrative_ontology:measurement(vacc_su_t24, vaccine_mandate_legitimacy__risk_stratification_reading, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(vacc_su_t24, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__risk_stratification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__risk_stratification_reading, 0.12).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__risk_stratification_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).

% DUAL FORMULATION NOTE:
% vaccine_mandate_legitimacy is a contested kernel with three constraint readings. This file instantiates risk_stratification_reading, which constrains mandate legitimacy via proportionality: blanket mandates illegitimate, targeted mandates permissible. Sibling readings (bodily_autonomy_primacy, public_health_primacy) instantiate different constraints from the same kernel. The three readings coexist as competing doctrines in public health and constitutional law; none has foreclosed the others. This reading influences both siblings by introducing proportionality as a constraint all must answer to.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(vaccine_mandate_legitimacy__risk_stratification_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
