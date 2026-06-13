% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy Primary: Medical Mandate Legitimacy via Consent Requirement
 *   domain: constitutional/medical/ethics
 *
 * SUMMARY:
 *   A state invokes disease control authority to mandate a medical
 *   intervention (vaccination) and suspends individual informed-consent
 *   requirements, enforcing compliance through employment termination,
 *   education exclusion, and public-benefit denial. This reading — bodily
 *   autonomy primary — asserts that fundamental bodily integrity cannot be
 *   overridden by collective benefit calculations, regardless of the
 *   disease's severity or the vaccine's safety profile. The state becomes a
 *   rights violator the moment consent is bypassed. Unvaccinated refusers and
 *   those with documented objections enter the victim set. This is ONE
 *   reading of the contested kernel 'mandate legitimacy scope'; the
 *   public_health_primary and proportionality_reading siblings frame the same
 *   situation differently.
 *
 * KEY AGENTS:
 *   - vaccine_mandated_individuals: Powerless; exit is trapped (relocation or non-compliance); face compulsory medical intervention backed by employment/education/benefit loss.
 *   - medical_refusers: Powerless; exit is trapped; hold genuine conscience objections; coercion operates through credential suspension and livelihood threat.
 *   - state_health_authority: Institutional agenda-setter; sets enforcement policy; under this reading, is a rights violator regardless of public health rationale.
 *   - vulnerable_populations: Powerless beneficiaries; gain herd immunity protection; but under this reading, their benefit does NOT justify overriding others' bodily autonomy.
 *   - medical_practitioners: Organized; constrained exit (licensing discipline); required to administer mandate; some experience internal conflict with medical ethics.
 *   - courts_applying_bodily_autonomy_doctrine: Institutional observers; their deference to health authority or application of balancing tests determines whether this reading's axioms hold force.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.82).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.79).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Bodily Autonomy Primary: Medical Mandate Legitimacy via Consent Requirement").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "constitutional/medical/ethics").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, 'fefdbc11-c7fb-4518-9d6e-5b7687453fd5').
narrative_ontology:cs_kernel_codification('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', fixed_text).
narrative_ontology:cs_authority_grounding('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', lineage).
narrative_ontology:cs_interpretation_layer_present('fefdbc11-c7fb-4518-9d6e-5b7687453fd5').
narrative_ontology:cs_reading_relation('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', mandate_legitimacy_scope__public_health_primary, coexists_with).
narrative_ontology:cs_reading_relation('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', foundational, bodily_integrity_inviolable).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable, holdable).
narrative_ontology:cs_axiom_grounding('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', bodily_integrity_inviolable, deontological).
narrative_ontology:cs_axiom('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', foundational, informed_consent_nonderogable).
narrative_ontology:cs_axiom_status(informed_consent_nonderogable, holdable).
narrative_ontology:cs_axiom_grounding('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', informed_consent_nonderogable, deontological).
narrative_ontology:cs_reference_frame('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', consent_requirement_constitutional_floor).
narrative_ontology:cs_drift_state('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', pandemic_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fefdbc11-c7fb-4518-9d6e-5b7687453fd5', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_mandated_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medical_refusers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medical_practitioners).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, bodily_integrity_inviolability).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, consent_requirement_nonderogable).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, individual_rights_trumps_collective).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face medical intervention (vaccination) imposed by state authority without informed consent or meaningful opt-out pathway. Their refusal to comply triggers loss of employment, education access, or public benefit eligibility. No credible exit exists within the jurisdiction; exit means relocation or underground non-compliance.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vaccine_mandated_individuals, payer,
    powerless, biographical, trapped, national).

% Hold medical or religious objections to the mandated intervention grounded in conscience or core belief. The mandate treats their objection as invalid — the state's determination of medical necessity overrides their bodily autonomy claim. Coercion operates through employment termination, credential suspension, or legal penalty. Exit would require abandoning the identity/belief system, not merely relocating.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_refusers, payer,
    powerless, biographical, identity_locked, national).

% Sets and enforces the mandate. Justifies it as protecting vulnerable populations from serious communicable disease. Under the bodily autonomy reading, the authority is using state coercive power to override individual consent, making it a rights violator regardless of the public health rationale.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_health_authority, agenda_setter,
    institutional, generational, analytical, national).

% Immunocompromised, very young, or elderly individuals who cannot be vaccinated themselves or who have reduced vaccine efficacy. They benefit from herd immunity thresholds maintained by high vaccination rates. However, under the bodily autonomy reading, their benefit does not justify overriding the autonomy of those forced to be vaccinated.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, vulnerable_populations, beneficiary,
    powerless, biographical, trapped, national).

% Required to administer the mandate and enforce consent waivers or implicit consent through coercive employment conditions. Many face licensing discipline if they refuse or document patient objections. Some internalize the public health authority's framing; others experience the mandate as a violation of medical ethics principles (autonomy, beneficence, non-maleficence).
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_practitioners, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__bodily_autonomy_primary, medical_practitioners, payer).

% Enact or omit specific statutory prohibitions on medical mandates. In the bodily autonomy reading, legislatures that permit mandates are delegitimizing themselves by allowing state override of fundamental rights. Legislatures that require explicit informed consent and meaningful opt-out pathways align with this reading.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, legislative_bodies, observer,
    institutional, generational, analytical, national).

% Adjudicate mandate challenges under constitutional bodily autonomy doctrines. Under this reading, courts are the critical check on state overreach. If courts defer to health authority expertise or apply a compelling-interest test, they are treating bodily autonomy as a weaker right than this reading asserts.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, courts_applying_bodily_autonomy_doctrine, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(mandate_legitimacy_scope__bodily_autonomy_primary, state_health_authority).
narrative_ontology:fixing_cost_class(mandate_legitimacy_scope__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading asserts no coordination function — the mandate is not solving a collective action problem; it is imposing a unilateral state choice on individual bodies. Any framing of the mandate as coordination is, in this reading, a cover story for extraction.
% TRANSFER_FUNCTION: Transfers bodily integrity and medical autonomy from mandate targets (vaccine_mandated_individuals, medical_refusers) to the state and to beneficiaries of herd immunity. The cost is borne by those forced to accept medical intervention against their will; the benefit accrues to vulnerable populations and to the state's epidemiological targets, not to those paying the cost.
% ABSENT_VOICES: Persons with documented severe adverse reactions to the vaccine (myocarditis, thrombosis, permanent disability) are structurally absent from mandate-design conversations — their testimony about the cost they bore is excluded. Persons whose religious or philosophical objections reflect genuine conscience (not mere preference) are absent from deliberations framed as purely technical/medical. Workers in precarious employment are absent from policy discussions dominated by institutions (universities, hospitals, governments) that experience the mandate as administrative rather than coercive.
% DISAPPEARANCE_RATIONALE: If medical mandates disappeared, the state would lose a coercive tool for disease control and would be forced to rely on persuasion, incentives, and voluntary participation. Vulnerable populations would face higher communicable-disease risk. Employment, education, and public-benefit access would cease being conditional on medical compliance. The distribution of bodily autonomy would shift dramatically — from state-controlled to individual-held. Hundreds of thousands of workers would be re-hired or re-credentialed. The political identity landscape would shift (mandate resistance movements would dissolve or reorient).
% FOUNDING_PROBLEM: Communicable disease spread poses a threat to public health. Historical pandemics killed millions. Modern public health uses vaccination to interrupt transmission chains and protect those who cannot be vaccinated. The problem addressed by mandates is the gap between individual rational incentives (not getting vaccinated if disease risk is low) and collective welfare (vaccinating enough people to protect vulnerable populations).
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists and public health authorities attest the founding problem persists: new pathogens emerge, vaccination remains a primary prevention tool, and vulnerable populations remain at risk from communicable disease. However, the bodily autonomy reading asserts that the founding problem's persistence does NOT justify abandoning the consent requirement — it justifies better persuasion, not coercion. This corroboration comes from medical ethicists (Beauchamp, Childress), constitutional scholars (Sunstein, Tribe), and human-rights frameworks (UN Siracusa Principles) that treat bodily integrity as inviolable. The founding_problem_status='live' reflects that disease risk persists; but this reading's mandate is not the only way to address it.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the mandate transfers bodily integrity and medical autonomy from targets to the state/beneficiaries without consent or compensation. The measurement series shows extractiveness rising sharply in the first 18 time points (initial rollout, enforcement hardening) and plateauing thereafter — indicating the constraint reaches its stable coercive level once employment/education exclusions are normalized and non-compliance becomes economically infeasible. Suppression is similarly high (0.79) and rises over time: initially, coercion operates through framing (public health emergency, protection narrative); over time, it shifts to explicit punishment (termination letters, credential denial), requiring active enforcement machinery. Theater ratio rises early (0.12 → 0.24) as performative elements accumulate (health briefings, consent forms signed under duress, 'voluntary' vaccine clinics in workplaces), then stabilizes as the theater becomes institutionalized — the performance is no longer novel, it is routine. Accessibility collapse (0.71) reflects that alternatives (non-vaccination in the jurisdiction) collapse once the mandate is understood: employment and education become inaccessible; geographic relocation is the only real exit. Resistance (0.68) is substantial: refusers mobilize politically, file constitutional challenges, refuse compliance despite penalties. The constraint persists not because it is accepted, but because state coercive power is sufficient to override resistance.
 *
 * PERSPECTIVAL GAP:
 *   The state_health_authority seat and the vaccine_mandated_individuals seat compute drastically different type classifications. From the authority's institutional position, the mandate is coordination (solving disease control via collective vaccination) with acceptable costs (some employment friction, justified by vulnerable-population protection). From the mandate targets' position, the constraint is pure extraction (state taking bodily autonomy without consent), and the beneficiaries are not even consulted about whether they accept the sacrifice of others' autonomy on their behalf. The engine computes both seats' type from power, exit options, and directionality — this gap is the measurement point: a claimed coordination function that feels like snare from the target's seat is exactly how false coordination is detected.
 *
 * DIRECTIONALITY LOGIC:
 *   vaccine_mandated_individuals: powerless, trapped exit, clear victims → directionality near 1.0 (full target). medical_refusers: same structural position → directionality near 1.0. vulnerable_populations: powerless, trapped, but beneficiaries not targets → directionality near 0.0 (subsidized by the constraint). state_health_authority: institutional, sets the rules, agenda-setter → directionality variable but structurally extractive from the perspective of those coerced. This reading asserts that state coercion of medical intervention has no beneficiary who willingly accepted it — vulnerable populations benefit incidentally, not by conscious trade. Therefore, the constraint is structurally a snare from every seat except the authority's, and from the authority's seat it is only defensible by suppressing the target seats' objections.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (communicable disease threat) is live, but the mandate's persistence depends entirely on state enforcement machinery — on the suppression requirement rising to 0.79 by the measurement endpoint. A core mandatrophy signal: the mandate does NOT persist through voluntary participation or coordination around shared values. It persists through coercion. If the state withdrew the mandate, vaccination rates would fall sharply, indicating the arrangement was never truly coordinate — it was extraction dressed as coordination. This reading does not declare mandatrophy_resolved=true (the constraint is still being enforced, still extracting), but the structure is present: a founding problem that is live, but whose solution depends entirely on overriding the autonomy of the solution's targets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_vs_coercive_framing,
    'Can consent be meaningfully given when refusal triggers employment termination or education exclusion? Is the consent form signed under duress genuine consent?',
    'Examine post-mandate employment data: did vaccination rates drop sharply in the non-mandated segment? Document testimony from those who ''chose'' vaccination after termination threats. Analyze whether consent-form signatures preceded or followed coercive conditions.',
    'If consent under duress is incoherent (yes), the constraint''s framing as a consent-based intervention is theater, and extractiveness is even higher than measured. If consent under duress is salvageable (no), the bodily autonomy reading''s premise weakens — the state''s framing might be partially defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_vs_coercive_framing, conceptual, 'The coherence of consent under coercive employment conditions').

omega_variable(
    beneficiary_acceptance_asymmetry,
    'Did vulnerable populations consent to receiving herd immunity protection at the cost of others'' forced vaccination? Or was protection imposed on them without their agreement to that moral trade?',
    'Solicit testimony from immunocompromised and elderly populations about whether they viewed the mandate as a justified protection. Compare mandate rollout communications (did they ask vulnerable populations first?) to actual policy (was their acceptance sought, or was the mandate imposed unilaterally?)',
    'If vulnerable populations did NOT consent to receiving benefit at the cost of others'' autonomy violation, the beneficiary justification collapses entirely — the constraint extracts from targets to provide unsolicited benefit to others. If they did consent and actively advocated for the mandate, the constraint moves toward rope, though this reading still asserts their benefit cannot override autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_acceptance_asymmetry, empirical, 'Whether beneficiaries actively consented to or merely benefited from the mandate').

omega_variable(
    medical_ethics_doctrine_conflict,
    'Does the mandate violate the Hippocratic principle of non-maleficence (do no harm) and the principle of autonomy in medical ethics, or does population-level disease prevention override individual-level medical ethics?',
    'Review medical ethics scholarship and licensing boards'' positions on mandates. Survey practicing physicians about perceived conflict between mandate enforcement and medical ethics training.',
    'If medical ethics doctrine unambiguously opposes mandates, state enforcement of mandates places practitioners in an ethical bind and reveals the constraint is extraction (state is forcing practitioners to violate their ethics). If modern medical ethics balances autonomy against population benefit, the reading''s axiom is contested within medicine itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_ethics_doctrine_conflict, conceptual, 'Internal conflict between mandate enforcement and medical ethics principles').

omega_variable(
    identity_locked_mechanism_in_refusers,
    'For those classified as medical_refusers (identity_locked exit), is the lock genuinely identity-based (religious conscience, philosophical principle) or identity-displaced (secondary identification with refusal subculture)?',
    'Longitudinal study: post-mandate, do refusers'' refusal stance persist, shift, or dissolve as community/identity effects change? Do those with religious objections remain steadfast across changing conditions?',
    'If the lock is genuine identity (core conscience), the exit_options classification is correct and the constraint''s suppression of refusers is more severe. If the lock dissolves when social pressure reverses, it is actually constrained rather than identity_locked, and the constraint''s suppression appears milder.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_mechanism_in_refusers, empirical, 'Whether refuser identity is foundational or displaced by community identification').

omega_variable(
    kernel_reading_divergence,
    'Does the bodily autonomy reading, in instantiating a non-negotiable consent requirement, logically foreclose the public_health_primary reading''s state authority to compel, or do they merely coexist as competing commitments held by different authority structures?',
    'Analyze the axioms: bodily-autonomy-inviolability (this reading) vs. state-protection-authority (public_health_primary). Can both axioms be held within a single constitutional or legal framework, or does accepting one require rejecting the other?',
    'If they coexist (both holdable within different traditions), the reading relationship is coexists_with. If bodily autonomy logically entails state cannot compel medical intervention, the relation is forecloses. The classification affects how the engine models the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Logical relationship between bodily-autonomy and state-protection axioms across readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(mand_tr_t0, projected).
narrative_ontology:measurement(mand_tr_t6, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 6, 0.15).
narrative_ontology:measurement_basis(mand_tr_t6, observed).
narrative_ontology:measurement(mand_tr_t12, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 12, 0.19).
narrative_ontology:measurement_basis(mand_tr_t12, observed).
narrative_ontology:measurement(mand_tr_t18, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(mand_tr_t18, observed).
narrative_ontology:measurement(mand_tr_t24, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(mand_tr_t24, observed).
narrative_ontology:measurement(mand_tr_t30, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(mand_tr_t30, observed).
narrative_ontology:measurement(mand_tr_t36, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 36, 0.28).
narrative_ontology:measurement_basis(mand_tr_t36, observed).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement_basis(mand_be_t0, projected).
narrative_ontology:measurement(mand_be_t6, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 6, 0.72).
narrative_ontology:measurement_basis(mand_be_t6, observed).
narrative_ontology:measurement(mand_be_t12, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 12, 0.78).
narrative_ontology:measurement_basis(mand_be_t12, observed).
narrative_ontology:measurement(mand_be_t18, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 18, 0.81).
narrative_ontology:measurement_basis(mand_be_t18, observed).
narrative_ontology:measurement(mand_be_t24, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 24, 0.82).
narrative_ontology:measurement_basis(mand_be_t24, observed).
narrative_ontology:measurement(mand_be_t30, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(mand_be_t30, observed).
narrative_ontology:measurement(mand_be_t36, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 36, 0.82).
narrative_ontology:measurement_basis(mand_be_t36, observed).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(mand_su_t0, projected).
narrative_ontology:measurement(mand_su_t6, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement_basis(mand_su_t6, observed).
narrative_ontology:measurement(mand_su_t12, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 12, 0.74).
narrative_ontology:measurement_basis(mand_su_t12, observed).
narrative_ontology:measurement(mand_su_t18, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 18, 0.78).
narrative_ontology:measurement_basis(mand_su_t18, observed).
narrative_ontology:measurement(mand_su_t24, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 24, 0.79).
narrative_ontology:measurement_basis(mand_su_t24, observed).
narrative_ontology:measurement(mand_su_t30, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 30, 0.79).
narrative_ontology:measurement_basis(mand_su_t30, observed).
narrative_ontology:measurement(mand_su_t36, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 36, 0.79).
narrative_ontology:measurement_basis(mand_su_t36, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__bodily_autonomy_primary, 0.08).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__proportionality_reading).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'mandate_legitimacy_scope'. Sibling readings (proportionality_reading and public_health_primary) are separate constraint stories with different beneficiary structures, different ε values, and different classes. All three are linked via network.affects_constraints. Each reading instantiates the SAME state mandate policy through DIFFERENT normative lenses, producing different classifications. The ε-invariance principle requires separate stories because the readings have structurally different interpretations of who benefits and who pays.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__bodily_autonomy_primary, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
