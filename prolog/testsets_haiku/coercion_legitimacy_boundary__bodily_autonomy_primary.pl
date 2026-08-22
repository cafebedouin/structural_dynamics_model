% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Bodily Autonomy Primary: Categorical Impermissibility of Non-Consensual Medical Intervention
 *   domain: medical_ethics/constitutional_law/public_health_policy
 *
 * SUMMARY:
 *   This constraint embodies one reading of the coercion_legitimacy_boundary
 *   kernel: the position that medical intervention without informed consent
 *   is categorically impermissible, regardless of collective benefit or
 *   disease severity. This reading treats bodily autonomy as a natural limit
 *   on state authority. The measured extractiveness (0.42) is moderate
 *   because the constraint does impose costs on immunocompromised populations
 *   (exposure risk) and public-health institutions (inability to mandate
 *   interventions). However, the reading's proponents argue these costs are
 *   the price of respecting a foundational human right, not extractive harm.
 *   The constraint's ε is not derived from its measured costs but from its
 *   claimed naturalness — low accessibility_collapse (0.38) because the
 *   boundary can in principle be understood and accepted; high resistance
 *   (0.72) because public-health authorities and disease-vulnerable
 *   populations actively contest it in practice.
 *
 * KEY AGENTS:
 *   - Individual medical subjects (protected beneficiaries, holding bodily autonomy rights)
 *   - Immunocompromised populations (classified as payers/victims under the expected structural delta, bearing exposure risk)
 *   - Public health authorities (constrained from mandate enforcement; pay the cost of voluntary-only coordination)
 *   - Medical practitioners (bound by informed-consent requirements; operationally burdened)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.28).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.42).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, mountain).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy Primary: Categorical Impermissibility of Non-Consensual Medical Intervention").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "medical_ethics/constitutional_law/public_health_policy").

domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, 'ff6206a5-09a2-4950-bef3-a7fa0edd726b').
narrative_ontology:cs_kernel_codification('ff6206a5-09a2-4950-bef3-a7fa0edd726b', fixed_text).
narrative_ontology:cs_authority_grounding('ff6206a5-09a2-4950-bef3-a7fa0edd726b', lineage).
narrative_ontology:cs_interpretation_layer_present('ff6206a5-09a2-4950-bef3-a7fa0edd726b').
narrative_ontology:cs_reading_relation('ff6206a5-09a2-4950-bef3-a7fa0edd726b', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_reading_relation('ff6206a5-09a2-4950-bef3-a7fa0edd726b', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_axiom('ff6206a5-09a2-4950-bef3-a7fa0edd726b', foundational, bodily_integrity_inviolable_categorical).
narrative_ontology:cs_axiom_status(bodily_integrity_inviolable_categorical, holdable).
narrative_ontology:cs_axiom_grounding('ff6206a5-09a2-4950-bef3-a7fa0edd726b', bodily_integrity_inviolable_categorical, deontological).
narrative_ontology:cs_axiom('ff6206a5-09a2-4950-bef3-a7fa0edd726b', foundational, consent_trumps_collective_benefit).
narrative_ontology:cs_axiom_status(consent_trumps_collective_benefit, holdable).
narrative_ontology:cs_axiom_grounding('ff6206a5-09a2-4950-bef3-a7fa0edd726b', consent_trumps_collective_benefit, deontological).
narrative_ontology:cs_reference_frame('ff6206a5-09a2-4950-bef3-a7fa0edd726b', inviolable_bodily_integrity_doctrine).
narrative_ontology:cs_drift_state('ff6206a5-09a2-4950-bef3-a7fa0edd726b', contemporary_public_health_emergency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ff6206a5-09a2-4950-bef3-a7fa0edd726b', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, legal_protection_frameworks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individual_medical_subjects).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_populations).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons whose bodily autonomy is recognized as inviolable under this reading. They retain the right to refuse medical intervention even when collective health arguments are deployed. The constraint protects them from state-mandated intervention; their exit is choosing non-participation in the intervention itself, which the reading permits.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individual_medical_subjects, beneficiary,
    moderate, biographical, mobile, national).

% Individuals unable to mount immune responses to vaccination or disease prevention. Under this reading's enforcement (non-mandate of others), they bear exposure risk from unvaccinated persons. They cannot exit their medical vulnerability; their safety depends on voluntary cooperation from others, which this reading prioritizes less than individual autonomy.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_populations, payer,
    powerless, biographical, trapped, national).

% State entities tasked with disease prevention and population health. Under this reading, their authority to compel medical intervention is categorically restricted, regardless of epidemiological rationale. They must work through persuasion, incentive, or voluntary programs. They can enforce the constraint's boundaries (preventing non-consensual intervention) but cannot override the autonomy boundary even under crisis conditions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, agenda_setter).

% Physicians and health professionals operate under the constraint: they cannot perform medical interventions without informed consent, even when public health rationales exist. They bear the operational burden of maintaining informed-consent processes and documenting refusals. Their exit is limited — leaving medical practice or jurisdiction avoids the constraint.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_practitioners, payer,
    organized, biographical, constrained, national).

% Constitutional and statutory rights protections (Fifth Amendment takings jurisprudence, bodily integrity doctrine, informed-consent case law). Listed as beneficiary because this reading vindicates these frameworks' authority and permanence.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, legal_protection_frameworks, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(coercion_legitimacy_boundary__bodily_autonomy_primary, legal_protection_frameworks).

% Non-agent category representing pathogens and epidemiological forces. Excluded because epidemiological necessity (the public_health_primary reading's foundation) is not a seat at the table; it is a fact one reading subordinates to autonomy and another reading prioritizes.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, disease_transmission_vectors, excluded,
    analytical, immediate, analytical, universal).
narrative_ontology:stakeholder_non_agent(coercion_legitimacy_boundary__bodily_autonomy_primary, disease_transmission_vectors).

% Courts and oversight bodies that adjudicate constitutional limits on state power. They observe the boundary this reading maintains and evaluate whether departures are justified.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, proportionality_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__bodily_autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__bodily_autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading solves no collective-action problem. It establishes a boundary: the constraint is NOT a coordination mechanism but a limit on the state's authority to coordinate via coercion. Individual autonomy in medical decisions is the stated function; it precludes solving public-health coordination through mandate.
% TRANSFER_FUNCTION: The constraint transfers authority away from public-health aggregation and toward individual choice. What moves is decision-making power: from state/collective harm-prevention to the individual. No extraction occurs — the reading frames the boundary as preventing extraction of bodily compliance.
% ABSENT_VOICES: Public-health epidemiologists making severity-based arguments would advocate for the proportionality reading and object to categorical autonomy primacy. Disease-bearing populations unable to protect themselves (the immunocompromised) would argue for the public_health_primary reading. These voices are excluded from the categorical framing because this reading structurally cannot accommodate harm-balancing against autonomy.
% DISAPPEARANCE_RATIONALE: Proponents of this reading argue that if the constraint vanished, coercive medical intervention would become possible, violating a foundational human right — the world rearranges toward tyranny. Proponents of the public_health_primary reading argue that if the categorical autonomy constraint vanished, disease-prevention mandates could save vulnerable populations — a different rearrangement. The dispute is not about facts but about which value arranges the world.
% FOUNDING_PROBLEM: The founding problem is the historical fact of non-consensual medical interventions: forced sterilization, unethical human experimentation, and coercive vaccination. The constraint was built to prevent recurrence of medical tyranny by establishing bodily autonomy as inviolable.
% FOUNDING_PROBLEM_CORROBORATION: Bioethics scholarship and international human rights frameworks (Declaration of Helsinki, Nuremberg Code, Geneva Conventions) corroborate that coercive medical intervention has occurred and remains a risk. Medical historians outside the beneficiary set document the specific harms. However, the question of whether the founding problem justifies the CATEGORICAL reading (versus a proportionality reading that admits some mandates for severe diseases) remains contested — the proportionality reading's proponents argue the founding problem is solved by oversight, not by categorical prohibition.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, contested).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, ExtMetricName, E),
    domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(coercion_legitimacy_boundary__bodily_autonomy_primary),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim is Mountain: the constraint is presented as natural law grounded in human bodily integrity. Accessibility_collapse at 0.38 reflects that the boundary can be articulated and understood, but not that it is universally accepted — in public-health crises, the boundary becomes controversial. Resistance at 0.72 is high because public-health institutions and vulnerable populations actively push back, especially during epidemics. Suppression is low (0.28) because the constraint does not require heavy enforcement machinery to maintain — it prevents something (mandatory intervention) rather than requiring something; the enforcement cost is mainly documenting consent/refusal. Theater_ratio is very low (0.12) because the constraint's operationalization is straightforward: practitioners obtain informed consent and document it. The extractiveness value (0.42) tracks the cost asymmetry: beneficiaries (autonomy holders) gain the right to refuse; payers (vulnerable populations in contact with refusers) bear the exposure cost. This asymmetry is why beneficiaries are listed: the constraint's operation benefits the individual-autonomy claim-holders. The moderate ε reflects that the cost is real but the reading denies it is extraction in the pejorative sense — it is the price of respecting inviolable rights. The metrics and claim are independent: the claim is Mountain; the metrics show a constraint with real costs and contestation. The engine measures the divergence.
 *
 * PERSPECTIVAL GAP:
 *   The autonomy-holder seat and the immunocompromised seat should compute very differently. From the autonomy-holder position, this is a Mountain — a natural law limiting authority. From the immunocompromised position, the same constraint is Tangled Rope — a coordination (protecting individual autonomy) that asymmetrically extracts from them (exposure without choice). The engine computes per-seat classifications; this seated divergence is structural. Public-health authorities compute it as a snare if they believe mandate is necessary for disease prevention — the constraint blocks their authority without their consent. The asymmetry comes from the different power levels and exit options: individual medical subjects have mobile exit (refuse the intervention); immunocompromised people have trapped exit (cannot leave their medical vulnerability).
 *
 * DIRECTIONALITY LOGIC:
 *   Individual medical subjects are structural beneficiaries (ε = 0.0 → 0.2 range): they gain the right to refuse. Their directionality is near-beneficiary (d ≈ 0.1–0.2). Immunocompromised populations are structural targets/payers (ε = 0.6–0.8 range): they bear exposure risk from unvaccinated contacts without recourse to mandate. Their directionality is near-target (d ≈ 0.7–0.9). Public-health authorities are constrained payers (d ≈ 0.5–0.6): they lose authority to use mandate but retain authority to persuade, incentivize, and quarantine infected individuals. Medical practitioners sit between (d ≈ 0.4–0.5): operationally burdened by consent requirements but not extraction targets; they gain protection against liability for forced intervention.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevention of medical tyranny) is still live, and the reading maintains the constraint as its guard. However, the constraint may exhibit mandatrophy at the seat level: public-health authorities may increasingly perform the consent theater without materially changing autonomy — documenting 'emergency exception' overrides, using public-health emergency declarations to bypass consent, or deploying incentives so aggressive they become coercive in fact. The measurement series tracks suppression_requirement rising slightly (0.22 → 0.28) and theater_ratio remaining low, which does NOT show mandatrophy yet. Theater would rise sharply (0.4+) if authorities were increasingly performing consent-gathering as ritual while effectively mandating anyway. The low theater suggests the constraint still functions; the rising suppression_requirement suggests the cost of maintaining it (enforcement machinery to prevent unauthorized intervention) is edging upward.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is bodily autonomy a natural law (emergent from irreducible human physicality) or a constructed legal/ethical boundary that could be otherwise framed?',
    'Genealogical analysis of bodily autonomy doctrine: does the boundary track an irreducible physical fact, or does it reflect contingent historical choices about what authority can legitimately do? Cross-cultural comparison of medical consent norms; analysis of whether the boundary persists under different institutional framings.',
    'If bodily autonomy is a natural law, the constraint''s low ε and high accessibility_collapse reflect detection of a real limit on coercive authority. If constructed, the beneficiaries (individual rights frameworks) are themselves deriving rent from the appearance of naturalness, and the constraint is a Tangled Rope masquerading as a Mountain — ε would reset upward.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Whether bodily autonomy is an irreducible fact or a contestable framing choice.').

omega_variable(
    immunocompromised_victim_status_ambiguity,
    'Are immunocompromised populations properly classified as victims of this constraint, or are they casualties of a separate public-health coordination failure?',
    'Definitional: if they are victims, then the constraint CREATES a cost asymmetry (beneficiary autonomy, victim exposure risk), elevating the reading toward Tangled Rope. If they are casualties of a different problem (lack of voluntary vaccination coordination), the constraint merely fails to solve the public-health problem — it does not extract from them.',
    'Victim classification slides the type toward Tangled Rope (beneficiary = autonomy claimants; victim = exposed populations). Non-victim classification maintains Mountain type. The boundary between ''victims of the constraint'' and ''populations harmed by what the constraint permits'' is the crux.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_victim_status_ambiguity, conceptual, 'Whether exposure risk from non-intervention makes a group victims of the autonomy-protecting constraint itself.').

omega_variable(
    public_health_necessity_overrides,
    'Does the categorical reading admit ANY exception for extreme disease severity or imminent mass harm?',
    'Textual analysis of the doctrinal tradition: does any major statement of bodily autonomy doctrine include catastrophic exceptions? Review of emergency-override jurisprudence. Interview with doctrine proponents about limiting cases (airborne Ebola, 50% mortality, mandatory quarantine of infected individuals — do these breach autonomy or fall into a separate category?).',
    'If exceptions exist, ε rises (the boundary is contingent, not categorical) and the reading may converge toward proportionality_reading. If no exceptions are admitted, the reading''s categorical claim is reinforced but may diverge further from implementability under crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_health_necessity_overrides, conceptual, 'Whether the bodily autonomy boundary admits exception under extreme conditions.').

omega_variable(
    reading_contest_empirical_ground,
    'This is one reading of the coercion_legitimacy_boundary kernel. What empirical data, if obtained, would favor one sibling reading over another?',
    'Outcome tracking from jurisdictions instantiating different readings: bodily-autonomy-primary regimes (opt-out medical policies) vs. proportionality regimes (mandate-by-disease-severity) vs. public-health-primary regimes (mandate-default). Measure: disease outcomes, autonomy violations, emergency-override frequency, public trust in health institutions, medical refusal rates, morbidity in immunocompromised populations.',
    'Empirical divergence between readings informs which reading''s axioms are being falsified in practice. Does bodily autonomy protection correlate with lower vaccination and worse population health? Does proportionality reading reduce emergency overrides? Does public-health-primary reading increase trust or erode it?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_empirical_ground, empirical, 'What real-world outcomes would differentiate the sibling readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.08).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 5, 0.09).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.11).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 15, 0.12).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.12).
narrative_ontology:measurement(coer_tr_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 25, 0.12).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(coer_be_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 25, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 5, 0.24).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.26).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 15, 0.28).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.29).
narrative_ontology:measurement(coer_su_t25, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 25, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.12).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the coercion_legitimacy_boundary kernel. The kernel dispute concerns what authority a state legitimately holds over medical decisions and when (if ever) collective harm overrides individual autonomy. bodily_autonomy_primary asserts categorical primacy of autonomy. proportionality_reading asserts severity-scaled authority (measles yes, flu no). public_health_primary asserts state authority to mandate when collective harm is substantial. Each reading has different ε, different beneficiary/victim structure, and different type. All three are linked via network.affects_constraints, and all three carry omega variables documenting the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
