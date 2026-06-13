% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__bodily_autonomy_primary, []).

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
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: legitimate_health_intervention__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary — Medical Intervention Legitimacy
 *   domain: medical_ethics/constitutional_law/public_health_policy
 *
 * SUMMARY:
 *   This constraint instantiates the BODILY AUTONOMY PRIMARY reading of the
 *   contested kernel 'legitimate_health_intervention'. Under this reading, a
 *   state-issued or employment-enforced medical mandate (vaccination,
 *   treatment protocol, quarantine requirement) violates bodily integrity as
 *   a fundamental right, regardless of public-health benefit. The legitimacy
 *   claim is: informed consent is necessary, not merely desirable; coercion
 *   via employment conditionality or access restrictions is illegitimate
 *   extraction, not justified public health. This reading sits in direct
 *   tension with sibling readings: the proportionality_reading accepts
 *   mandates when disease threat is severe and alternatives are exhausted;
 *   the public_health_primary reading endorses mandates whenever population
 *   benefit exceeds individual cost. The bodily-autonomy-primary reading
 *   forecloses both by asserting bodily integrity is inviolable, not subject
 *   to proportionality calculation or utilitarian trade-off. The constraint's
 *   operation shows rising extractiveness over its interval: disease threat
 *   declined (t=0 to t=36), yet mandate enforcement intensified, theater
 *   ratio climbed (compliance theater replaced disease-control necessity),
 *   and suppression hardened. This temporal signature is diagnostic of a
 *   constraint that has outlived its founding problem and is now sustained by
 *   institutional inertia and agenda-setter interests rather than the
 *   public-health coordination it claims.
 *
 * KEY AGENTS:
 *   - public_health_authority: institutional agenda-setter, collects compliance leverage and maintains enforcement machinery
 *   - mandate_subject_individuals: powerless, identity-locked payers; face employment and access coercion
 *   - access_restricted_workers: moderate-power payers, concentrated early enforcement targets, high exit cost
 *   - conscientious_objectors: identity-locked payers explicitly excluded from legitimacy conversation; their refusal grounds are not recognized
 *   - disease_prevention_coalition: beneficiary, collects professional authority and research prominence from mandate outcomes
 *   - mandate_beneficiary_population: diffuse beneficiaries, receive disease-reduction benefit without bearing enforcement cost
 *   - civil_liberties_organizations: excluded advocates, present only as litigation adversaries, not as co-adjudicators
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, 0.68).
domain_priors:suppression_score(legitimate_health_intervention__bodily_autonomy_primary, 0.71).
domain_priors:theater_ratio(legitimate_health_intervention__bodily_autonomy_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimate_health_intervention__bodily_autonomy_primary, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__bodily_autonomy_primary, "Bodily Autonomy Primary — Medical Intervention Legitimacy").
narrative_ontology:topic_domain(legitimate_health_intervention__bodily_autonomy_primary, "medical_ethics/constitutional_law/public_health_policy").

domain_priors:requires_active_enforcement(legitimate_health_intervention__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__bodily_autonomy_primary, '5edccff3-8789-4bb2-95a2-e3a6f8751e1c').
narrative_ontology:cs_kernel_codification('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', fixed_text).
narrative_ontology:cs_authority_grounding('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', extraction).
narrative_ontology:cs_reading_relation('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', legitimate_health_intervention__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', foundational, bodily_integrity_inviolable_categorical).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable_categorical, holdable).
narrative_ontology:cs_axiom_grounding('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', bodily_integrity_inviolable_categorical, deontological).
narrative_ontology:cs_axiom('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', foundational, informed_consent_necessary_not_contingent).
narrative_ontology:cs_axiom_status(informed_consent_necessary_not_contingent, holdable).
narrative_ontology:cs_axiom_grounding('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', informed_consent_necessary_not_contingent, deontological).
narrative_ontology:cs_reference_frame('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', bodily_integrity_inviolable).
narrative_ontology:cs_drift_state('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', contemporary_mandate_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5edccff3-8789-4bb2-95a2-e3a6f8751e1c', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, public_health_authority).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, disease_prevention_coalition).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, mandate_subject_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, access_restricted_workers).
narrative_ontology:constraint_victim(legitimate_health_intervention__bodily_autonomy_primary, conscientious_objectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__bodily_autonomy_primary, mandate_beneficiary_population).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, bodily_integrity_as_fundamental_right).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__bodily_autonomy_primary, informed_consent_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues and enforces medical mandates (vaccination, treatment protocols, quarantine requirements) framed as disease-control measures. Justifies mandates by reference to population-level epidemiological data, infection risk, and public benefit. Administers exemption policy, handles compliance verification, and maintains enforcement infrastructure (employment-access leverage, legal penalties). Faces growing litigation from bodily-autonomy claimants and legislative pressure; maintains authority by invoking emergency powers, delegated expertise, and public-health consensus claims.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, public_health_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Face coercive pressure to undergo medical intervention (vaccine, treatment, quarantine) as a condition of employment, school attendance, public-facility access, or professional licensing. Those who refuse face employment termination, educational exclusion, social-access restrictions (travel, healthcare, commerce), or legal penalties. Exit requires geographic relocation to non-mandate jurisdictions, career abandonment, or sustained legal exposure. Many experience refusal as grounded in identity-constitutive commitments (conscience, religious doctrine, bodily-autonomy principle), making exit psychologically severe even when structural barriers are theoretically removable.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_subject_individuals, payer,
    powerless, biographical, identity_locked, national).

% Healthcare workers, educators, childcare providers, emergency responders, and essential workers are among the first and most severely targeted by employment-conditional mandates. They face immediate termination (not gradual pressure) if non-compliant. Career retraining is costly (years of education) and may be blocked if licensing boards impose mandate conditions. Their exit options are severely constrained: abandon the profession (sunk cost), relocate to non-mandate jurisdiction (high friction), or comply under coercion. Those who comply experience the mandate as enforced extraction, not voluntary coordination.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, access_restricted_workers, payer,
    moderate, biographical, constrained, national).

% Individuals whose refusal is grounded in religious, philosophical, or ethical commitments to bodily integrity or medical non-intervention. They face identical employment and access restrictions as non-compliers, but their refusal is constitutive of identity and worldview. Exit would require abandoning foundational commitments, which is experientially impossible. They are structurally excluded from the agenda-setting conversation: authority does not recognize conscience-based refusal as a legitimate basis for accommodation; refusal is treated as non-compliance, not as a protected voice in legitimacy deliberation. Their exclusion from the seat-set means their concerns never reach the agenda-setter.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, conscientious_objectors, excluded,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(legitimate_health_intervention__bodily_autonomy_primary, conscientious_objectors, payer).

% Epidemiologists, infectious-disease specialists, public-health researchers, and disease-control organizations cite mandate effectiveness in raising intervention coverage and reducing disease transmission and mortality. They collect professional authority, research prominence, and institutional influence through mandate-supporting publications and policy testimony. They do not directly collect extraction revenue but do collect credibility expansion and research-funding allocation tied to mandate-supporting evidence. Their interest aligns with mandate persistence; budget and career incentives reward demonstrated mandate efficacy.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, disease_prevention_coalition, beneficiary,
    organized, generational, analytical, global).

% Population members who benefit from reduced disease transmission due to high intervention coverage mandated in others. They collect the coordination benefit (lower infection risk, reduced healthcare demand) without undergoing intervention themselves or bearing employment-coercion cost. Their situation is asymmetrically favorable: they are protected by others' mandated compliance. They have constrained exit from the disease-reduction benefit (they cannot opt out of lower disease risk), but face no direct compliance cost.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, mandate_beneficiary_population, beneficiary,
    powerless, biographical, constrained, national).

% Analyze the constraint from disciplinary and comparative perspective, outside the beneficiary-payer operational structure. They document the structural tension between public-health outcomes (high coverage, low transmission) and bodily-autonomy claims (refusal must be honored); produce framework comparisons (this bodily-autonomy-primary reading vs. proportionality vs. public-health-primary); and note that different jurisdictions adopt different readings, revealing the kernel as fundamentally contested rather than settled.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, medical_ethics_scholars, observer,
    analytical, generational, analytical, global).

% Advocate that mandate-as-enforced violates bodily integrity rights and that informed refusal must be honored regardless of public-health projections. They argue for mandate redesign (opt-in rather than opt-out, recognized exemptions, conscience-based delay windows) or abolition. They are structurally excluded from agenda-setting: the authority does not seat them as co-adjudicators of mandate legitimacy; their advocacy operates through litigation, legislative lobbying, and public pressure, not through recognized voice in design deliberation. Their exclusion is structural, not accidental—the authority has deliberately confined them to adversary status.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__bodily_autonomy_primary, civil_liberties_organizations, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_health_intervention__bodily_autonomy_primary, public_health_authority).
narrative_ontology:fixing_cost_class(legitimate_health_intervention__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Raises medical-intervention coverage (vaccination, treatment adherence, quarantine participation) to population-scale levels that reduce disease transmission and population-level morbidity/mortality. The coordination problem is: individual-level voluntary uptake falls below the threshold needed for herd immunity, outbreak containment, or disease elimination; individual refusal creates negative externalities (unvaccinated individuals transmit disease; non-compliers maintain disease reservoir). The mandate solves this by making non-compliance individually costly (employment conditionality, access restrictions), which aggregates to population-level coverage that would not emerge from voluntary choice alone.
% TRANSFER_FUNCTION: Transfers bodily autonomy (the right to refuse or delay medical intervention) from individuals to the public-health authority. The authority collects compliance leverage (employment-conditionality enforcement, access-gating, legal penalty authority) and uses it to achieve high intervention coverage. Mandate subjects bear the cost: forced or coerced medical exposure, employment precarity, social exclusion, identity-integrity violation (for those whose refusal is identity-constitutive). The mandated benefit (disease reduction) flows diffusely to the entire population, while the compliance cost is concentrated on mandate subjects.
% ABSENT_VOICES: Civil-liberties organizations, conscientious objectors (as a recognized class, not as individual exceptions), and individual bodily-autonomy claimants are structurally excluded from the agenda-setting conversation. They are present as litigation adversaries and public opposition but are not seated as co-adjudicators of mandate legitimacy. Their exclusion shapes the constraint's persistence: if they were seated with recognized voice in design deliberation, mandate structure might shift toward opt-out, recognized exemptions, conscience-based delay windows, or transparency about the trade-off between coverage and autonomy. Their absence from the conversation means the mandate is designed to maximize compliance, not to balance autonomy and health outcomes.
% DISAPPEARANCE_RATIONALE: If the mandate-as-enforced disappeared overnight, employment restrictions would lift immediately, access-gating would end, and individuals would regain uncoerced choice over medical interventions. Disease transmission would rise from post-mandate baselines; population-level morbidity would increase in proportion to voluntary-uptake shortfalls relative to mandate-achieved coverage. The authority would lose the enforcement machinery and compliance leverage that sustained high intervention coverage. Healthcare systems would face higher disease burden from reduced coverage. Both the individual domain (autonomy restored) and population domain (disease burden increased) would rearrange significantly.
% FOUNDING_PROBLEM: Infectious disease poses measurable population-level harm; individual-level voluntary uptake of preventive or therapeutic intervention falls below herd-immunity or outbreak-control thresholds; public-health authority seeks mechanism to raise compliance above voluntary levels without violence or surveillance infrastructure collapse.
% FOUNDING_PROBLEM_CORROBORATION: Public-health authorities and epidemiologists attest the founding problem is live: disease threat persists across jurisdictions and voluntary uptake remains below target thresholds. Medical ethicists, biostatisticians, and civil-liberties scholars attest the founding problem has shifted in many jurisdictions: disease prevalence has fallen below crisis levels, yet mandates persist and even intensify, suggesting the founding problem has been solved and the arrangement is sustained for institutional-inertia or extraction reasons rather than disease-control necessity. Independent data from comparative jurisdictions shows variation: some maintain mandates despite low current threat (problem dead), others lifted mandates despite continued threat (problem live), and some retain mandates as threat oscillates (problem genuinely contested). No single corroborating voice from outside the public-health authority gives unified testimony; the heterogeneity itself is the data.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(legitimate_health_intervention__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness trajectory (0.42 → 0.68) reflects two phases: Early (t=0–12): genuine coordination extraction. Disease threat is real, voluntary uptake falls short, mandate raises compliance and reduces transmission. The extraction is moderate because both beneficiary and payer populations see epidemiological benefit. Middle (t=12–24): extraction rises as threat declines. As disease prevalence falls, the founding problem weakens, yet enforcement machinery does not proportionally reduce. The authority maintains mandate-as-enforced to secure high compliance, even as the public-health justification weakens. Late (t=24–36): extraction plateaus at high level (0.68). Threat is low, voluntary uptake could handle remaining disease control, yet mandate persists. The theater ratio climbs (0.18 → 0.42), indicating performative maintenance—compliance theater replaces functional disease control. Suppression requirement rises steeply (0.48 → 0.71) and stays elevated: growing resistance from mandate subjects and civil-liberties advocates forces authority to spend more institutional capacity defending the constraint (litigation, exemption management, public messaging). The gap between suppression (0.71) and extractiveness (0.68) suggests the constraint is near the boundary where enforcement cost approaches extraction benefit—a piton-candidate signature. However, the constraint retains a genuine coordinating function (disease reduction remains real, even if below crisis threshold), so classification is tangled_rope, not piton. If founding_problem_status is confirmed 'dead' and theater_ratio continues above 0.5, recurrent audit should flag for reclassification. The claim/metric gap is authorized: the authority claims rope (legitimate coordination) while the metrics describe tangled_rope (asymmetric extraction disguised as coordination). The engine measures that divergence; the story does not reconcile them.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is extreme. From the public_health_authority seat, the constraint is rope: it solved a real problem (disease control), maintains coordination benefit (ongoing transmission reduction), and operates with community support. From the mandate_subject_individuals seat, the constraint is snare: it coerces unwanted medical exposure, uses employment and access as extortion leverage, and persists because the authority benefits from control, not because disease threat justifies it. From the access_restricted_workers seat, it is snare with concentrated harm: enforcement targets them first and most severely, making the asymmetry visible. From the disease_prevention_coalition seat, it is rope or even mountain (coordinated disease control is a public good, mandate just implements it). From the mandate_beneficiary_population seat, it is rope (they collect benefit without cost). From the civil_liberties_organizations seat, it is pure snare (bodily coercion is illegitimate regardless of outcome). The engine computes each seat's type from the power/exit/beneficiary/victim data; this perspectival fracture is exactly the structural signal tangled_rope is supposed to detect: genuine coordination function + asymmetric extraction + multiple institutional seats experiencing opposite types.
 *
 * DIRECTIONALITY LOGIC:
 *   Public_health_authority: d = 0.0–0.1 (full beneficiary). Controls mandate design, collects compliance leverage, maintains enforcement infrastructure, and enjoys professional-authority amplification from the constraint's operation. Power is institutional (high), exit is arbitrage (very mobile—can shift to next public-health problem). Derives positive rents from the constraint; the engine computes low/negative effective extraction for this seat. Mandate_subject_individuals: d = 0.9–1.0 (full target). Lack individual or collective power, face identity-locked exit (refusal fuses with identity commitments), and bear the constraint's enforced costs (unwanted medical exposure, employment risk, social exclusion). High suppression prevents exit; effective extraction is amplified for this seat. Access_restricted_workers: d = 0.75–0.85 (mostly target). Moderate power individually but concentrated vulnerability as an occupational class. Constrained exit (career retraining is costly). Early enforcement targets them; they have more leverage than powerless mandate subjects but less than the authority. Disease_prevention_coalition: d = 0.0–0.15 (near beneficiary). Collects professional authority, research prominence, and institutional influence; does not directly bear enforcement cost. Organized power, generational time horizon, analytical exit. Mandate_beneficiary_population: d = 0.2–0.3 (slight beneficiary). Receives disease-reduction benefit but also diffuse compliance-theater cost (surveillance normalization, emergency-governance normalization). Constrained exit, powerless individual power. Deriv: structured as beneficiary but not fully insulated from constraint operation. No override needed; structural derivation produces the right d.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is NOT yet mandatrophic but shows mandatrophy-like drift. Founding_problem_status = 'contested' is the key: public-health authorities say the problem is live (ongoing disease threat), while epidemiologists and ethicists cite jurisdiction-dependent data showing disease has fallen below crisis thresholds in some regions yet mandates persist. If founding_problem_status shifts to 'dead' (confirmed by independent data, not by the authority's self-report), and if theater_ratio rises above 0.5 while suppression remains elevated, then the constraint enters mandatrophic territory: it persists by institutional inertia, not by solving its founding problem. The current metrics do not quite meet mandatrophy criteria (theater is 0.42, not clearly >0.5; suppression is high but not disconnected from actual resistance), but the trajectory is worrying. The constraint is a candidate for early mandatrophy intervention: if disease threat is indeed low, proportional public-health response would involve scaling down enforcement, restoring individual choice, and maintaining only those interventions (surveillance, targeted treatment, voluntary uptake support) that address real ongoing risk. The tangled_rope classification is currently correct (genuine coordination + asymmetric extraction), but continued drift toward piton is plausible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    disease_threat_measurement_ambiguity,
    'Is disease threat at time t genuinely declining, or is threat declining because the mandate is working—making the mandate causally responsible for its own justification?',
    'Counterfactual disease modeling (what would prevalence be without mandate, controlling for natural immunity and behavioral changes) and cross-jurisdictional comparison: identical-population jurisdictions with and without mandates, tracking disease levels over identical time windows.',
    'If threat decline is mandate-caused, the founding problem remains live (the mandate is solving it), and the constraint''s justification is intact. If threat decline is natural or driven by other interventions, the founding problem is dead, and the constraint is extraction sustained by inertia. This is diagnostic for mandatrophy determination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disease_threat_measurement_ambiguity, empirical, 'Whether disease-threat decline is causal consequence of mandate or independent of mandate.').

omega_variable(
    identity_locked_exit_suppression_mechanism,
    'Is the measured suppression (0.71) structural (legal barriers, employment-conditionality enforcement, access-gating infrastructure) or internalized (mandate subjects have fused identity with refusal, making exit psychologically impossible even if structural barriers lifted)?',
    'Post-mandate-lift trajectory: if suppression persists after mandate enforcement is removed, the internalized component is substantial; if suppression collapses, it was purely structural. Also: interviews with formerly mandate-subject individuals in post-mandate jurisdictions, tracking how identity-fusion persists or dissolves.',
    'If internalized, the constraint''s true suppression exceeds the measured structural metric—the target carries the suppression with them after exit, limiting re-engagement. This raises the effective extraction ceiling. If purely structural, suppression ends when enforcement ends. Affects prognosis for constraint unwinding.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_exit_suppression_mechanism, empirical, 'Suppression source: structural enforcement vs. internalized identity fusion.').

omega_variable(
    kernel_reading_foreclosure_ambiguity,
    'Does the bodily_autonomy_primary reading''s core premise (bodily integrity is inviolable) logically foreclose the public_health_primary reading''s core premise (legitimacy derives from population benefit), or do they coexist as genuinely alternative framings within a contested kernel?',
    'Formal logic check: can a single institutional framework hold both ''bodily integrity is inviolable'' and ''population benefit can override individual refusal''? Answer: no, not coherently. BUT: can two different institutions (a bioethics authority and a public-health authority) hold incompatible readings of the same kernel? Answer: yes, and do. The ambiguity is whether the kernel is one contested claim (single framework, logical incoherence implies foreclosure) or one name for two incompatible claims (two frameworks, no logical foreclosure). The source material treats it as contested, implying kernel status, but the boundaries are blurred.',
    'If foreclosure holds, the bodily_autonomy_primary reading is incompatible with public_health_primary, and institutional coexistence is incoherent—one must give. If coexistence holds, both readings remain live and the kernel is genuinely underdetermined by any single logic. This affects how the committer structure routes through the corpus: foreclosure => one reading will eventually win institutional authority; coexistence => both persist and generate recurrent policy conflict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_ambiguity, conceptual, 'Whether bodily autonomy and public-health legitimacy are logically foreclosing or genuinely alternative readings.').

omega_variable(
    conscientious_objector_legitimacy_seat,
    'Are conscientious objectors (with identity-constitutive refusal) a distinct seat with legitimacy claim to be heard, or are they a non-agent category (their refusal is a symptom of non-compliance, not a basis for mandate redesign)?',
    'Comparative jurisdiction analysis: do jurisdictions that recognize conscience-based exemptions or delay windows show different constraint metrics (lower theater, different suppression profile) than those denying recognition? Also: do conscientious objectors'' post-mandate outcomes (employment recovery, social reintegration) differ from non-objector mandate subjects?',
    'If conscientious objectors are a legitimate seat, the constraint''s exclusion of their voice (role=excluded in the stakeholder model) is diagnostic of captured or inadequate agenda-setting. Mandate redesign to honor conscience-based delay or refusal would alter the extraction profile and likely lower suppression. If they are not a distinct seat, the constraint''s operation is correct in ignoring their claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conscientious_objector_legitimacy_seat, preference, 'Whether conscience-based refusal grounds a legitimate seat at the mandate-legitimacy table.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(legi_tr_t0, observed).
narrative_ontology:measurement(legi_tr_t6, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(legi_tr_t6, observed).
narrative_ontology:measurement(legi_tr_t12, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 12, 0.28).
narrative_ontology:measurement_basis(legi_tr_t12, observed).
narrative_ontology:measurement(legi_tr_t18, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 18, 0.34).
narrative_ontology:measurement_basis(legi_tr_t18, observed).
narrative_ontology:measurement(legi_tr_t24, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 24, 0.39).
narrative_ontology:measurement_basis(legi_tr_t24, observed).
narrative_ontology:measurement(legi_tr_t30, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(legi_tr_t30, observed).
narrative_ontology:measurement(legi_tr_t36, legitimate_health_intervention__bodily_autonomy_primary, theater_ratio, 36, 0.42).
narrative_ontology:measurement_basis(legi_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(legi_be_t0, observed).
narrative_ontology:measurement(legi_be_t6, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 6, 0.48).
narrative_ontology:measurement_basis(legi_be_t6, observed).
narrative_ontology:measurement(legi_be_t12, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 12, 0.56).
narrative_ontology:measurement_basis(legi_be_t12, observed).
narrative_ontology:measurement(legi_be_t18, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(legi_be_t18, observed).
narrative_ontology:measurement(legi_be_t24, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 24, 0.65).
narrative_ontology:measurement_basis(legi_be_t24, observed).
narrative_ontology:measurement(legi_be_t30, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 30, 0.67).
narrative_ontology:measurement_basis(legi_be_t30, observed).
narrative_ontology:measurement(legi_be_t36, legitimate_health_intervention__bodily_autonomy_primary, base_extractiveness, 36, 0.68).
narrative_ontology:measurement_basis(legi_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(legi_su_t0, observed).
narrative_ontology:measurement(legi_su_t6, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 6, 0.54).
narrative_ontology:measurement_basis(legi_su_t6, observed).
narrative_ontology:measurement(legi_su_t12, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 12, 0.62).
narrative_ontology:measurement_basis(legi_su_t12, observed).
narrative_ontology:measurement(legi_su_t18, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 18, 0.68).
narrative_ontology:measurement_basis(legi_su_t18, observed).
narrative_ontology:measurement(legi_su_t24, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 24, 0.71).
narrative_ontology:measurement_basis(legi_su_t24, observed).
narrative_ontology:measurement(legi_su_t30, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(legi_su_t30, observed).
narrative_ontology:measurement(legi_su_t36, legitimate_health_intervention__bodily_autonomy_primary, suppression_requirement, 36, 0.71).
narrative_ontology:measurement_basis(legi_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(legitimate_health_intervention__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__proportionality_reading).
narrative_ontology:affects_constraint(legitimate_health_intervention__bodily_autonomy_primary, legitimate_health_intervention__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'legitimate_health_intervention'. The sibling readings 'proportionality_reading' and 'public_health_primary' are separate constraint stories instantiating different normative framings of the same kernel. Each reading has distinct beneficiary/victim structure, ε value, and classification. The bodily_autonomy_primary reading (this story) treats bodily integrity as inviolable and frames mandate enforcement as extraction. The public_health_primary reading treats population benefit as primary and frames mandates as legitimate coordination. The proportionality_reading occupies a middle position. All three coexist in live policy debate but produce structurally different constraint models. See the network links and omega variables in all three files for the cross-reading architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimate_health_intervention__bodily_autonomy_primary, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
