% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition on Torture and Degrading Treatment (Common Article 3)
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes an absolute,
 *   non-derogable prohibition on torture and degrading treatment of
 *   detainees, prisoners of war, and protected persons. This constraint
 *   embodies the absolute_prohibition reading of the contested
 *   humane_treatment_standard kernel: the claim is that no circumstances, no
 *   matter how severe the security threat, permit crossing the torture
 *   threshold. Detainees retain full personhood and rights-holder status
 *   regardless of their classification or the conflict context. The
 *   constraint is presented as grounded in human dignity—an irreducible
 *   principle—not in consequentialist calculation or security balance. This
 *   reading forecloses the contextual_necessity reading (which permits
 *   exceptions) and coexists with the proportionality_balancing reading as
 *   competing framings of how humane standards should govern interrogation.
 *
 * KEY AGENTS:
 *   - detainees: powerless, trapped, bear no institutional role but are the direct beneficiaries of the constraint
 *   - state_military_intelligence: institutional, constrained exit, bear the operational cost of narrowed interrogation methods
 *   - state_government: institutional, agenda-setter, ratifies and enforces the standard as sovereign law
 *   - international_humanitarian_law_bodies: institutional, observer, interpret the standard and investigate violations
 *   - rival_security_doctrines: institutional, excluded, would argue for enhanced interrogation under necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.15).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.08).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.15).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, mountain).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition on Torture and Degrading Treatment (Common Article 3)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, '4c06c86c-fbad-4062-9fb1-72eae2eada58').
narrative_ontology:cs_kernel_codification('4c06c86c-fbad-4062-9fb1-72eae2eada58', fixed_text).
narrative_ontology:cs_authority_grounding('4c06c86c-fbad-4062-9fb1-72eae2eada58', lineage).
narrative_ontology:cs_interpretation_layer_present('4c06c86c-fbad-4062-9fb1-72eae2eada58').
narrative_ontology:cs_reading_relation('4c06c86c-fbad-4062-9fb1-72eae2eada58', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('4c06c86c-fbad-4062-9fb1-72eae2eada58', humane_treatment_standard__proportionality_balancing, coexists_with).
narrative_ontology:cs_axiom('4c06c86c-fbad-4062-9fb1-72eae2eada58', foundational, torture_categorically_impermissible).
narrative_ontology:cs_axiom_status(torture_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('4c06c86c-fbad-4062-9fb1-72eae2eada58', torture_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('4c06c86c-fbad-4062-9fb1-72eae2eada58', foundational, human_dignity_non_negotiable).
narrative_ontology:cs_axiom_status(human_dignity_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('4c06c86c-fbad-4062-9fb1-72eae2eada58', human_dignity_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('4c06c86c-fbad-4062-9fb1-72eae2eada58', absolute_non_derogable_protection).
narrative_ontology:cs_drift_state('4c06c86c-fbad-4062-9fb1-72eae2eada58', post_9_11_security_doctrine_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4c06c86c-fbad-4062-9fb1-72eae2eada58', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, prisoners_of_war).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, protected_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_military_intelligence).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, human_dignity_inalienable).
narrative_ontology:constraint_vindicates(humane_treatment_standard__absolute_prohibition, fundamental_rights_non_derogable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals captured or detained in armed conflict or detention settings. The constraint establishes that they retain rights to humane treatment and protection from torture regardless of their status, origin, or the circumstances of their detention. They cannot negotiate or exit this status; the constraint's operation protects them absolutely from interrogation methods that constitute torture or degrading treatment.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, biographical, trapped, universal).

% Military and intelligence services operate within armed conflict or counterterrorism contexts where they seek to extract actionable information from detainees. They bear the cost of the constraint through operational restrictions: interrogation methods permitted under the absolute prohibition are narrower and often yield information more slowly than methods the constraint forbids. They cannot invoke security necessity or military advantage to cross the torture threshold.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_military_intelligence, payer,
    institutional, generational, constrained, national).

% Ratifying states adopt and enforce Common Article 3 as binding international law, incorporating it into domestic military codes and detention protocols. The state is the administrative enforcer of the standard: it trains interrogators, reviews detention practices, and is held accountable by international bodies for violations. Its commitment is framed as inherent to sovereignty and the rule of law, not as a concession.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_government, agenda_setter,
    institutional, generational, analytical, national).

% The International Committee of the Red Cross, the UN Human Rights Committee, and international courts interpret Common Article 3 and investigate alleged violations. They hold states accountable, document practices, and reaffirm the non-derogable status of the prohibition. They produce the authoritative reading of what constitutes torture or degrading treatment under the constraint.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_humanitarian_law_bodies, observer,
    institutional, civilizational, analytical, global).

% Security frameworks that claim enhanced interrogation techniques (waterboarding, stress positions, sensory deprivation) are permissible when state security interests are sufficiently grave are structurally excluded from the absolute prohibition reading. These doctrines would argue that necessity and proportionality should govern interrogation, not categorical bans. They cannot be represented within the absolute prohibition frame.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, rival_security_doctrines, excluded,
    institutional, generational, trapped, national).

% Persons harmed by terrorist attacks or insurgent violence occupy an analytical seat: they have interests in effective security and investigation but are not parties to the constraint's direct operation (they are neither detainees nor interrogators). Some security advocates invoke their suffering to argue for relaxed interrogation standards; the absolute prohibition reading treats this as a false trade-off — security and humane treatment are structurally compatible.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, victims_of_terrorism, observer,
    powerless, biographical, trapped, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__absolute_prohibition, diffuse).
narrative_ontology:fixing_cost_class(humane_treatment_standard__absolute_prohibition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal floor of protection for vulnerable persons in armed conflict and detention, ensuring consistent application of humane standards across jurisdictions and conflict types. Creates a shared framework so that all parties to conflict know the binding obligations and can coordinate mutual compliance without fear of escalation in torture methods.
% TRANSFER_FUNCTION: Transfers constraints on state interrogation methods: the state bears the cost of using slower, less coercive methods; detainees gain protection from torture and degrading treatment. No money changes hands; the transfer is in the form of restricted operational liberty for state actors and expanded bodily integrity protections for detainees.
% ABSENT_VOICES: Security officials who believe enhanced interrogation is necessary in catastrophic scenarios; regimes that practice torture but deny it; populations in attacked states who might demand 'any means necessary' against those who attack them. These voices are systematically excluded from the absolute prohibition framing — they would argue for contextual exceptions, which the constraint forbids.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished, state interrogation practices would differentiate by jurisdiction and conflict intensity; torture and degrading treatment would become permissible under security necessity claims; detainees would lose the floor protection and would depend on the goodwill and restraint of individual states. The Geneva Convention framework would collapse into a patchwork of unequal protections, and the mutual assurance that all parties face the same humane standards would evaporate.
% FOUNDING_PROBLEM: In the aftermath of World War II and faced with evidence of systematic torture and inhuman treatment by occupying powers and organized combatants, the international community established that certain protections for captured and detained persons must be non-negotiable: no military advantage, security emergency, or ideological conflict justifies torture. The constraint was built to prevent recurrence of the atrocities documented in Nazi concentration camps and Japanese POW detention.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations (Amnesty International, Human Rights Watch), UN fact-finding missions, and documented cases from contemporary conflicts (Syria, Yemen, Afghanistan) attest that torture and degrading treatment remain widespread despite the constraint. Ratifying states continue to face violations and investigation, confirming the founding problem persists. Academic legal scholars outside the benefiting parties (human rights experts, humanitarian law specialists) and victim testimony corroborate both the problem and the constraint's continued necessity.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, ExtMetricName, E),
    domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(humane_treatment_standard__absolute_prohibition),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored as low (0.15 endpoint) because the constraint is claimed as a natural law grounding human dignity, not as an institutional extraction mechanism. No actor collects rents from the prohibition; the beneficiaries (detainees) gain protection without paying; the payers (state intelligence services) pay through operational constraint, not through organized extraction. However, extractiveness is NOT zero because: (1) the absolute prohibition does enforce a particular reading against rival framings (the contextual_necessity and proportionality_balancing readings that would permit exceptions), and (2) states that ratify face domestic pressure and operational costs, creating resistance. Suppression is very low (0.08) because the constraint operates through law and norm adoption, not through coercion—states ratify voluntarily and enforcement is through reputation and accountability bodies, not through force. Theater is low (0.12) because the constraint's function is real: it does prevent documented interrogation methods and it does protect detainees. The spike in both metrics at 2001 reflects post-9/11 pressure when rival security doctrines (contextual_necessity) gained institutional traction and some states adopted enhanced interrogation despite the constraint, briefly raising both extractiveness (defensive cost of enforcing the absolute prohibition against legal-reframing attempts) and theater (increased rhetorical defense of the constraint by its advocates). By 2026, metrics decline as the constraint's norm status has restabilized and documented torture has become less institutionally defensible.
 *
 * PERSPECTIVAL GAP:
 *   From the detainee's and international human rights perspective, the constraint is a mountain: an irreducible baseline that neither state authority nor security necessity can overcome. From the state security perspective, especially in high-threat contexts, the constraint is experienced as rope (genuine coordination on shared humane standards) shadowed by extraction (the operational cost of foregoing interrogation methods that might yield faster intelligence). The engine computes this perspectival divergence from the beneficiary (detainees) and payer (intelligence services) seats: beneficiaries perceive the constraint as structurally fixed (high accessibility_collapse: alternatives to torture protection are unthinkable); payers perceive it as enforced (higher resistance from security practitioners who believe necessity should override). The author's claim (mountain) and the metrics (non-zero extractiveness due to the defense against contextual_necessity readings) are intentionally independent: the engine's classification will reflect the actual structural dynamics, not the absolute_prohibition reading's normative assertion.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees are full beneficiaries (d ≈ 0.0): the constraint subsidizes their protection absolutely. State military intelligence is a partial payer (d ≈ 0.7) through operational constraint, though institutional ratification means the overall state position is beneficiary-aligned (d ≈ 0.2). The structural asymmetry is between the detainee's powerless trapped status (total protection) and the intelligence service's institutional constrained status (operational cost imposed by own government's commitment). This asymmetry is what the absolute_prohibition reading must defend: it asserts that detainee protection trumps state security discretion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing torture recurrence post-1945) remains live: contemporary documentation of torture in Syria, Yemen, and other conflicts confirms the threat. The constraint has not become functionally obsolete. However, mandatrophy is contested at the level of READING: the contextual_necessity and proportionality_balancing readings argue that the absolute_prohibition mandate has outlived the post-WWII context and should now be subordinated to security calculation. The absolute_prohibition reading rejects this subordination by asserting that human dignity is not context-dependent. The measurement spike in theater_ratio at 2001-2015 (post-9/11 period when rival readings gained institutional traction) reflects this contestation: states claiming to uphold humane standards while adopting enhanced interrogation performed the constraint theatrically rather than functionally. The decline by 2026 suggests rhetorical consolidation back toward the absolute prohibition norm, though documented violations persist. Mandatrophy does NOT apply: the founding problem is still live and the constraint still functions to prevent its recurrence, despite contestation over readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturality_vs_constructed_reading,
    'Is the absolute prohibition on torture a natural law grounding in inalienable human dignity, or is it a constructed institutional reading adopted for political reasons after WWII?',
    'Philosophical analysis of the concept of human dignity and its status in different ethical frameworks; historical investigation of why the absolute prohibition was chosen over alternative formulations (like proportionality or contextual balancing); examination of whether the prohibition appears in pre-1945 legal traditions.',
    'If the absolute prohibition is grounded in genuine human dignity (natural law), the constraint is a mountain and rival readings are indefensible. If it is a contingent historical choice, it is a rope or tangled_rope defending a particular reading against alternatives, and the contextual_necessity and proportionality_balancing readings gain structural legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturality_vs_constructed_reading, conceptual, 'Whether the absolute prohibition is natural law or constructed institutional framing.').

omega_variable(
    security_exception_boundary,
    'What threshold of state security threat, if any, would justify an exception to the absolute prohibition? Or is the prohibition truly non-derogable under all circumstances?',
    'Examination of actual state practice during existential security crises (nuclear war threat, pandemic-level bioterrorism, imminent genocide planning) to determine whether the prohibition holds; constitutional law analysis of whether non-derogable rights have limiting clauses; comparative study of state behavior during maximum-threat scenarios.',
    'If states universally breach the prohibition under existential threat, the constraint is experienced as contextual_necessity (coexists with absolute_prohibition), not as an absolute mountain. If the prohibition holds even under maximum threat, it is genuinely non-derogable. If some states hold and others breach, the reading_relations move toward coexists_with rather than forecloses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_exception_boundary, empirical, 'Whether the absolute prohibition holds under all circumstance or has latent exception conditions.').

omega_variable(
    rival_reading_institutional_power,
    'Do contextual_necessity and proportionality_balancing readings gain institutional traction only when security threats spike (2001), or do they have independent doctrinal roots in state security law?',
    'Historical analysis of security doctrine development independent of specific conflict periods; examination of academic legal theory on necessity and proportionality in armed conflict; analysis of state ratification patterns and reservation-filing on Common Article 3.',
    'If rival readings are contingent on threat spikes, they are strategic reframings (coexists_with). If they have independent doctrinal legitimacy, the boundary between absolute_prohibition and the alternatives is less clear-cut and may shift toward coexists_with framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rival_reading_institutional_power, empirical, 'The institutional stability and independence of rival readings from security-threat conditions.').

omega_variable(
    enforcement_capacity_asymmetry,
    'Why do wealthy states with strong international law capacity (US, UK) show higher violation rates and longer histories of enhanced interrogation than weaker states with less law-enforcement capacity?',
    'Empirical investigation of torture allegations across state types; comparison of violation rates by state capacity, legal tradition, and security doctrine; analysis of whether enforcement capacity correlates with violation or compliance.',
    'If high-capacity states violate more often, the constraint''s enforcement depends not on strength but on normative commitment, suggesting the absolute_prohibition reading is holding normatively despite institutional violations. If violations distribute randomly, the constraint''s enforcement is weakly institution-dependent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_asymmetry, empirical, 'Empirical pattern in enforcement asymmetry across state types and institutional capacities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.05).
narrative_ontology:measurement_basis(huma_tr_t1949, observed).
narrative_ontology:measurement(huma_tr_t1970, humane_treatment_standard__absolute_prohibition, theater_ratio, 1970, 0.08).
narrative_ontology:measurement_basis(huma_tr_t1970, observed).
narrative_ontology:measurement(huma_tr_t1990, humane_treatment_standard__absolute_prohibition, theater_ratio, 1990, 0.1).
narrative_ontology:measurement_basis(huma_tr_t1990, observed).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.16).
narrative_ontology:measurement_basis(huma_tr_t2001, observed).
narrative_ontology:measurement(huma_tr_t2015, humane_treatment_standard__absolute_prohibition, theater_ratio, 2015, 0.14).
narrative_ontology:measurement_basis(huma_tr_t2015, observed).
narrative_ontology:measurement(huma_tr_t2026, humane_treatment_standard__absolute_prohibition, theater_ratio, 2026, 0.12).
narrative_ontology:measurement_basis(huma_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.08).
narrative_ontology:measurement_basis(huma_be_t1949, observed).
narrative_ontology:measurement(huma_be_t1970, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1970, 0.11).
narrative_ontology:measurement_basis(huma_be_t1970, observed).
narrative_ontology:measurement(huma_be_t1990, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1990, 0.13).
narrative_ontology:measurement_basis(huma_be_t1990, observed).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.18).
narrative_ontology:measurement_basis(huma_be_t2001, observed).
narrative_ontology:measurement(huma_be_t2015, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2015, 0.16).
narrative_ontology:measurement_basis(huma_be_t2015, observed).
narrative_ontology:measurement(huma_be_t2026, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2026, 0.15).
narrative_ontology:measurement_basis(huma_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.05).
narrative_ontology:measurement_basis(huma_su_t1949, observed).
narrative_ontology:measurement(huma_su_t1970, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1970, 0.06).
narrative_ontology:measurement_basis(huma_su_t1970, observed).
narrative_ontology:measurement(huma_su_t1990, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1990, 0.07).
narrative_ontology:measurement_basis(huma_su_t1990, observed).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.12).
narrative_ontology:measurement_basis(huma_su_t2001, observed).
narrative_ontology:measurement(huma_su_t2015, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2015, 0.09).
narrative_ontology:measurement_basis(huma_su_t2015, observed).
narrative_ontology:measurement(huma_su_t2026, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2026, 0.08).
narrative_ontology:measurement_basis(huma_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__absolute_prohibition, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__contextual_necessity).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, detainee_rights_regime).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, interrogation_standards_framework).
narrative_ontology:affects_constraint(humane_treatment_standard__absolute_prohibition, international_accountability_doctrine).

% DUAL FORMULATION NOTE:
% The humane_treatment_standard kernel decomposes into three constraint stories, each instantiating a different reading of how Common Article 3 should govern detention and interrogation. The absolute_prohibition reading (this story) holds that no circumstances permit torture and that detainees retain full rights-holder status. The contextual_necessity reading would permit enhanced interrogation when state security imperatives override. The proportionality_balancing reading would require case-by-case weighing of detainee dignity against security needs. These are not three measurements of one constraint — they are three structurally distinct constraints with different beneficiary/victim structures, different ε values, and different classifications. The absolute_prohibition reading (this file) forecloses the contextual_necessity reading (logically contradictory core premises: 'never permit torture' vs. 'permit under necessity') and coexists with the proportionality_balancing reading (different parties hold each as their framing of the same kernel commitment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
