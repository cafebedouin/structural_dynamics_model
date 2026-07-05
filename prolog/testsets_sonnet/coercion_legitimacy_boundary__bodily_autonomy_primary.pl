% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Categorical Bar to Compelled Medical Intervention
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the bodily_autonomy_primary reading of the
 *   coercion_legitimacy_boundary kernel: a categorical doctrine holding that
 *   medical intervention without consent is impermissible regardless of the
 *   collective epidemiological benefit that compulsion would produce. Under
 *   this reading, the refusing individual's consent right is dispositive and
 *   admits no disease-severity or transmission-dynamics balancing test. The
 *   structural delta from the parent kernel domain is realized here as a
 *   moderate, non-enforcement-driven ε: immunocompromised individuals and
 *   unvaccinated infants enter the victim set as those exposed to risk the
 *   doctrine prevents any authority from mitigating, and bodily-autonomy
 *   advocacy groups and exemption claimants enter the beneficiary set as
 *   those whose legal position the categorical standard entrenches. This is a
 *   distinct constraint from the proportionality_reading and
 *   public_health_primary siblings — each has its own ε, its own
 *   beneficiary/victim structure, and its own classification; they are not
 *   measurement variants of one constraint.
 *
 * KEY AGENTS:
 *   - vaccine_refusing_individuals: primary beneficiary (moderate/mobile) — retains full autonomy under the categorical standard
 *   - immunocompromised_individuals: primary payer (powerless/trapped) — bears the herd-immunity externality with no recourse
 *   - bodily_autonomy_advocacy_groups: agenda-setter (organized/arbitrage) — litigates to entrench the doctrine
 *   - public_health_departments: excluded institutional actor — barred from compelling intervention regardless of outbreak severity
 *   - courts_and_constitutional_scholars: analytical observer — adjudicates the doctrinal boundary over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.35).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Categorical Bar to Compelled Medical Intervention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '7483b084-8c7e-46d5-bdc0-ae2619b1407d').
narrative_ontology:cs_kernel_codification('7483b084-8c7e-46d5-bdc0-ae2619b1407d', formalized).
narrative_ontology:cs_authority_grounding('7483b084-8c7e-46d5-bdc0-ae2619b1407d', lineage).
narrative_ontology:cs_interpretation_layer_present('7483b084-8c7e-46d5-bdc0-ae2619b1407d').
narrative_ontology:cs_reading_relation('7483b084-8c7e-46d5-bdc0-ae2619b1407d', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('7483b084-8c7e-46d5-bdc0-ae2619b1407d', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('7483b084-8c7e-46d5-bdc0-ae2619b1407d', foundational, consent_is_categorically_dispositive).
narrative_ontology:cs_axiom_status(consent_is_categorically_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('7483b084-8c7e-46d5-bdc0-ae2619b1407d', consent_is_categorically_dispositive, deontological).
narrative_ontology:cs_axiom('7483b084-8c7e-46d5-bdc0-ae2619b1407d', foundational, collective_benefit_cannot_ground_bodily_compulsion).
narrative_ontology:cs_axiom_status(collective_benefit_cannot_ground_bodily_compulsion, holdable).
narrative_ontology:cs_axiom_grounding('7483b084-8c7e-46d5-bdc0-ae2619b1407d', collective_benefit_cannot_ground_bodily_compulsion, deontological).
narrative_ontology:cs_reference_frame('7483b084-8c7e-46d5-bdc0-ae2619b1407d', post_nuremberg_informed_consent_absolutism).
narrative_ontology:cs_drift_state('7483b084-8c7e-46d5-bdc0-ae2619b1407d', contemporary_outbreak_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7483b084-8c7e-46d5-bdc0-ae2619b1407d', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusing_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_advocacy_groups).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, religious_and_philosophical_exemption_claimants).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, infants_too_young_to_vaccinate).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, frontline_healthcare_workers).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, individual_sovereignty_over_bodily_integrity).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains full legal standing to decline any medical intervention, including vaccination, regardless of the collective epidemiological consequence. The categorical bar on non-consensual intervention is what protects this choice from being overridden by public health authorities, employers, or courts.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, vaccine_refusing_individuals, beneficiary,
    moderate, biographical, mobile, national).

% Litigates and lobbies to keep the categorical-consent standard entrenched in constitutional doctrine, drafting model legislation and filing amicus briefs. Benefits reputationally and organizationally from every case where the standard is upheld, and treats any proportionality carve-out as an existential threat to the doctrine.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_advocacy_groups, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, bodily_autonomy_advocacy_groups, beneficiary).

% Uses the categorical consent bar to secure exemptions from school, employment, and travel vaccination requirements without needing to demonstrate any disease-specific rationale — the bar's categorical nature is precisely what makes case-by-case scrutiny of the sincerity or reasonableness of the objection unnecessary.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, religious_and_philosophical_exemption_claimants, beneficiary,
    moderate, biographical, mobile, regional).

% Cannot be vaccinated or cannot mount an effective immune response themselves, and depends entirely on herd immunity maintained by others' vaccination for protection against preventable disease. The categorical bar means no authority can compel the surrounding population to vaccinate to protect them; their protection is contingent on voluntary compliance they cannot enforce and cannot flee from indefinitely.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% Below the age threshold for vaccination against diseases like measles or pertussis, and entirely dependent on the vaccination status of surrounding adults and older children for protection. Have no voice in the consent framework at all — they are neither the ones refusing nor the ones with standing to object to the risk imposed on them.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, infants_too_young_to_vaccinate, payer,
    powerless, immediate, trapped, local).

% Treats unvaccinated patients and works alongside unvaccinated colleagues in enclosed clinical settings where infectious exposure risk is elevated. Occupational exit means leaving the profession or the facility; remaining means absorbing exposure risk the categorical consent standard prevents institutions from mitigating through mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, frontline_healthcare_workers, payer,
    moderate, biographical, constrained, local).

% Would argue for disease-severity-calibrated mandates during outbreaks but is structurally barred by the categorical standard from compelling intervention regardless of transmission dynamics or outbreak severity. Their epidemiological modeling and outbreak-response authority is present in policy debate but has no doctrinal purchase against a categorical bar.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_departments, excluded,
    institutional, generational, constrained, national).

% Adjudicates and analyzes the doctrinal boundary between individual consent and collective harm-prevention, producing the case law and scholarship that either entrenches or erodes the categorical standard over time.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, courts_and_constitutional_scholars, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, predictable, non-negotiable rule — no medical intervention without consent — that protects every individual from being subjected to case-by-case, discretion-laden state or institutional judgments about whose bodily integrity may be overridden for whose benefit.
% TRANSFER_FUNCTION: Moves epidemiological risk from the vaccine-refusing population (who retain full autonomy and bear no compulsion cost) to the immunocompromised, unvaccinated infants, and exposed healthcare workers, who bear the elevated risk of preventable disease transmission that a compelled-vaccination regime would have reduced.
% ABSENT_VOICES: Immunocompromised individuals and infants have no direct voice in the consent framework — the doctrine is adjudicated between refusing individuals and the state, with those exposed to the refusal's consequences appearing only as third-party externalities in litigation, never as parties whose consent or refusal is itself at issue.
% DISAPPEARANCE_RATIONALE: If the categorical bar dissolved into a pure proportionality or public-health-primary standard, jurisdictions could compel vaccination during outbreaks, employers and schools could impose stricter mandates without categorical exemption routes, and current refusal-based legal strategies would lose their doctrinal foundation — bodily autonomy advocacy groups would lose their central organizing claim and litigation posture entirely.
% FOUNDING_PROBLEM: Built to prevent a documented historical pattern: state and institutional actors (from involuntary sterilization programs to non-consensual medical experimentation) using collective-benefit rationales to justify severe bodily violations of individuals, particularly marginalized populations, without any check on the scope or proportionality of the justification.
% FOUNDING_PROBLEM_CORROBORATION: Bioethicists and legal historians outside the bodily-autonomy advocacy movement corroborate that the founding problem — unchecked state power to violate bodily integrity under collective-benefit cover — remains a live historical concern, citing continuing debates over compelled psychiatric treatment and reproductive coercion. However, public health scholars and epidemiologists, also outside the beneficiary set, attest that applying the same categorical bar to low-risk, well-studied preventive vaccination during active outbreaks addresses a founding problem the doctrine was never built for, and that the mismatch is being exploited by the beneficiary groups rather than resolved by them.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) rather than high because the doctrine's harm is diffuse and probabilistic — it does not extract a direct transfer so much as it forecloses a protective mechanism others would otherwise have, imposing risk rather than certain loss. Suppression is moderate (0.35): the doctrine does not coerce anyone into refusing vaccination, but it does actively suppress public health departments' capacity to compel intervention, which is a real structural foreclosure even though it operates by omission rather than command. Resistance is high (0.72) because immunocompromised advocates, healthcare institutions, and epidemiologists actively contest the categorical standard in litigation and policy debate. Theater ratio is low (0.2) — the doctrine's coordination function (protecting against unchecked bodily violation) is genuinely operative, not merely performed, even though its distributive consequence is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the vaccine-refusing individual's seat, this constraint is experienced as a rope — protection against state overreach, no coercion, freely exercised choice. From the immunocompromised individual's seat, the same structural arrangement computes as extractive: they bear a health risk imposed by others' exercised choice, with no doctrinal mechanism to compel protection. The engine computes these divergent per-seat classifications from the same structural data; the categorical framing does not resolve which seat's experience is 'correct' — both are structurally accurate descriptions of different positions relative to the same rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Vaccine-refusing individuals and exemption claimants sit near the beneficiary end of directionality: the doctrine directly protects their choice and they bear no compulsion cost. Bodily autonomy advocacy groups are also beneficiaries in an organizational sense — the doctrine's persistence is their institutional raison d'être. Immunocompromised individuals, infants too young to vaccinate, and frontline healthcare workers sit near the target end: they bear elevated risk that a proportionality or public-health-primary standard would have reduced, and their exit options are trapped or constrained rather than mobile. Public health departments are excluded rather than positioned on the beneficiary/victim axis — they are a structurally foreclosed institutional actor, not a party who gains or loses directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unchecked collective-benefit rationales justifying severe bodily violations — remains genuinely live in domains like compelled psychiatric treatment and reproductive coercion, which argues against treating this doctrine as pure mandatrophy. But applying the identical categorical bar to well-studied, low-risk preventive vaccination during active measles or pertussis outbreaks extends a doctrine built for one class of harm (irreversible, high-severity, historically abused interventions) to a structurally different class (low-risk, reversible, well-evidenced preventive care) without re-examining whether the original justification transfers. This is the seam the proportionality_reading sibling exists to probe.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the bodily_autonomy_primary reading disagree with its siblings — is it about WHETHER collective benefit can ever override consent (categorical vs. balancing), or about WHERE the threshold for compulsion sits (severity-calibrated vs. uniform)?',
    'Doctrinal analysis of case law: identify whether courts adopting this reading reject balancing tests entirely (supporting a forecloses relation to public_health_primary) or merely set the threshold very high (supporting an influences or coexists_with relation).',
    'If the disagreement is categorical (no balancing permitted under any circumstance), this reading forecloses public_health_primary within any single legal framework. If it is a threshold disagreement, the readings coexist as different points on a shared proportionality logic, which would better describe this reading as a limiting case of proportionality_reading rather than a distinct kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether bodily_autonomy_primary is categorically distinct from or a limiting case of the proportionality reading.').

omega_variable(
    herd_immunity_externality_attribution,
    'Is the elevated risk borne by immunocompromised individuals and infants properly attributed to the categorical consent doctrine itself, or to the underlying biological fact that some individuals cannot be protected without population-level compliance regardless of the legal standard?',
    'Comparative epidemiological analysis of outbreak outcomes in jurisdictions with categorical consent standards versus jurisdictions with compulsory vaccination mandates, controlling for baseline vaccination uptake absent any legal compulsion.',
    'If uptake is similarly high without compulsion, the doctrine''s marginal contribution to the externality is small and the victim-set attribution should be tempered. If compulsion demonstrably raises uptake and outbreak protection, the doctrine bears more direct causal responsibility for the exposure risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herd_immunity_externality_attribution, empirical, 'Whether the doctrine causally produces the immunocompromised/infant exposure risk or merely fails to prevent an exposure risk that would exist regardless.').

omega_variable(
    advocacy_group_capture_vs_genuine_representation,
    'Do bodily autonomy advocacy groups genuinely represent the population of individuals with sincere, severe autonomy concerns (e.g. historical experimentation victims'' descendants, disability rights communities), or have they been substantially captured by vaccine-refusal-specific interests that use the categorical framing instrumentally?',
    'Track the funding sources, membership composition, and litigation docket of major bodily-autonomy advocacy organizations over the measurement interval; compare rhetoric addressing historical abuses versus rhetoric addressing routine vaccination refusal.',
    'If capture is substantial, the beneficiary classification should distinguish between the doctrine''s legitimate constituency (abuse survivors, disability advocates) and an instrumentalizing constituency (vaccine refusers) whose stake in the categorical framing is narrower than the doctrine''s stated purpose.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(advocacy_group_capture_vs_genuine_representation, empirical, 'Whether advocacy for the categorical standard represents its founding constituency or has been captured by a narrower interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement(coer_tr_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 4, 0.14).
narrative_ontology:measurement(coer_tr_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 8, 0.16).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 12, 0.17).
narrative_ontology:measurement(coer_tr_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 16, 0.18).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.19).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(coer_be_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(coer_be_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(coer_be_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(coer_su_t4, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 4, 0.24).
narrative_ontology:measurement(coer_su_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 8, 0.27).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 12, 0.29).
narrative_ontology:measurement(coer_su_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 16, 0.31).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.33).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 24, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the coercion_legitimacy_boundary kernel. bodily_autonomy_primary holds consent as categorically dispositive (forecloses public_health_primary's collective-override premise within a single framework); proportionality_reading holds legitimacy on a severity-calibrated continuum (coexists_with this reading, since courts and legislatures hold both categorical-rights framings and severity-calibrated framings as live doctrinal options depending on the intervention type). Each reading carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
