% ============================================================================
% CONSTRAINT STORY: udhr_authority__binding_universalism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__binding_universalism_reading, []).

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
 *   constraint_id: udhr_authority__binding_universalism_reading
 *   human_readable: UDHR Binding Universalism: Individual Rights Enforceability Against States
 *   domain: international_law/human_rights
 *
 * SUMMARY:
 *   The binding universalism reading instantiates the claim that UDHR
 *   provisions establish justiciable individual rights enforceable against
 *   states regardless of whether those states consented to the tribunal's
 *   jurisdiction or to binding interpretation of UDHR articles. Under this
 *   reading, international human rights tribunals possess coercive authority
 *   to interpret UDHR as self-executing law, and states are bound by tribunal
 *   orders even without explicit treaty ratification or consent. This is ONE
 *   READING of the contested UDHR authority kernel — the kernel is the UDHR
 *   text and the legitimacy claim grounded in it; the reading is the specific
 *   interpretation that vests tribunals with binding universal
 *   enforceability. Sibling readings (aspirational_sovereignty_reading,
 *   customary_emergence_reading) interpret the same kernel differently,
 *   assigning different authority structures and enforcement mechanisms. This
 *   story models ONLY the binding universalism reading's structural dynamics.
 *
 * KEY AGENTS:
 *   - Individual rights claimants (powerless, trapped) — the intended beneficiaries of the constraint; they gain enforceable claims against their own states regardless of state consent
 *   - Human rights tribunals (institutional, agenda-setter) — administer the constraint by interpreting UDHR as binding and issuing coercive orders to states
 *   - Non-consenting states (institutional, payer) — bear the extraction of state autonomy by being bound to tribunal authority they never accepted
 *   - Liberal democratic states (institutional, beneficiary + agenda_setter) — ratified treaties willingly and benefit from the constraint while helping shape tribunal interpretation
 *   - International civil society (organized, beneficiary) — gains institutional leverage and standing to enforce norms through tribunal systems
 *   - Treaty negotiators and sovereigntist states (excluded) — would object that tribunal interpretation expanded UDHR beyond negotiated intent and improperly subordinates state autonomy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, 0.68).
domain_priors:suppression_score(udhr_authority__binding_universalism_reading, 0.52).
domain_priors:theater_ratio(udhr_authority__binding_universalism_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(udhr_authority__binding_universalism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__binding_universalism_reading, tangled_rope).
narrative_ontology:human_readable(udhr_authority__binding_universalism_reading, "UDHR Binding Universalism: Individual Rights Enforceability Against States").
narrative_ontology:topic_domain(udhr_authority__binding_universalism_reading, "international_law/human_rights").

domain_priors:requires_active_enforcement(udhr_authority__binding_universalism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__binding_universalism_reading, '29893845-84e7-4480-a6c2-6e1ede461ab9').
narrative_ontology:cs_kernel_codification('29893845-84e7-4480-a6c2-6e1ede461ab9', formalized).
narrative_ontology:cs_authority_grounding('29893845-84e7-4480-a6c2-6e1ede461ab9', extraction).
narrative_ontology:cs_interpretation_layer_present('29893845-84e7-4480-a6c2-6e1ede461ab9').
narrative_ontology:cs_reading_relation('29893845-84e7-4480-a6c2-6e1ede461ab9', udhr_authority__aspirational_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('29893845-84e7-4480-a6c2-6e1ede461ab9', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('29893845-84e7-4480-a6c2-6e1ede461ab9', foundational, individual_rights_justiciable_without_state_consent).
narrative_ontology:cs_axiom_status(individual_rights_justiciable_without_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('29893845-84e7-4480-a6c2-6e1ede461ab9', individual_rights_justiciable_without_state_consent, deontological).
narrative_ontology:cs_axiom('29893845-84e7-4480-a6c2-6e1ede461ab9', foundational, tribunal_interpretation_binds_non_consenting_states).
narrative_ontology:cs_axiom_status(tribunal_interpretation_binds_non_consenting_states, holdable).
narrative_ontology:cs_axiom_grounding('29893845-84e7-4480-a6c2-6e1ede461ab9', tribunal_interpretation_binds_non_consenting_states, conventional).
narrative_ontology:cs_reference_frame('29893845-84e7-4480-a6c2-6e1ede461ab9', tribunal_binding_universal_enforceability).
narrative_ontology:cs_drift_state('29893845-84e7-4480-a6c2-6e1ede461ab9', contemporary_sovereigntist_backlash_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('29893845-84e7-4480-a6c2-6e1ede461ab9', '').
narrative_ontology:cs_kernel_id(udhr_authority__binding_universalism_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, human_rights_tribunals).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, international_civil_society).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, non_consenting_states).
narrative_ontology:constraint_victim(udhr_authority__binding_universalism_reading, sovereignty_constrained_governments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_authority__binding_universalism_reading, liberal_democratic_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons claiming violations of UDHR-enumerated rights (freedom from torture, arbitrary detention, discrimination, etc.) gain a legal apparatus to petition international tribunals and compel state compliance without requiring their state's consent to jurisdiction. They benefit from the universalist reading because it vests them with actionable claims against their own state regardless of domestic law.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, individual_rights_claimants, beneficiary,
    powerless, biographical, trapped, universal).

% International courts and quasi-judicial bodies (ICJ, regional human rights courts, UN treaty committees) claim jurisdiction to interpret and enforce UDHR provisions as binding law, issuing remedial orders and reputational judgments against states. They administer the constraint by interpreting UDHR articles as self-executing rights and binding customary norms, even when the respondent state claims it never consented to the tribunal's authority.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, human_rights_tribunals, agenda_setter,
    institutional, generational, analytical, universal).

% States that did not ratify specific human rights treaties, or that ratified with reservations, or that reject individual petition mechanisms, nonetheless face binding rulings from international tribunals claiming universal jurisdiction over UDHR rights. They bear the cost of compliance with orders issued by institutions they never explicitly consented to, and face reputational, diplomatic, and (in some contexts) economic pressure to obey. Exit options are severely constrained: international isolation, sanctions, or formal withdrawal from the treaty system carry high costs.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, non_consenting_states, payer,
    institutional, generational, constrained, universal).

% States that ratified UDHR-implementing treaties discover that tribunal interpretation progressively expands the scope of enforceability, producing obligations beyond what state negotiators understood or authorized. They are constrained by the fact that withdrawal or non-compliance triggers isolation, secondary sanctions, and delegitimization in the international community. The constraint extracts state autonomy in lawmaking, detention policy, and internal security.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, sovereignty_constrained_governments, payer,
    powerful, generational, constrained, universal).

% NGOs, human rights organizations, and advocacy networks benefit from the universalist reading by gaining standing to file complaints, submit amicus briefs, and shape tribunal reasoning. They coordinate globally to enforce UDHR norms and leverage tribunals to advance rights agendas, gaining institutional leverage they would not possess in purely domestic politics.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, international_civil_society, beneficiary,
    organized, generational, mobile, global).

% States that never ratified key human rights treaties would argue that the universalist reading imposes obligations they never accepted. Their voice is excluded from the tribunal system's formal authority structure, yet they are bound by the same interpretations as ratifying states. They cannot participate in treaty amendment processes and cannot escape the constraint without full international isolation.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, non_ratifying_states, excluded,
    powerful, generational, trapped, universal).

% States with strong domestic rights protections benefit from the universalist reading because it locks other states into similar standards and creates a level playing field for global governance. They co-author tribunal precedents and benefit from the reputational premium the constraint confers. Their exit options are better (they comply easily and face no real constraint) and they benefit from agenda-setting power in tribunal interpretation.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, liberal_democratic_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__binding_universalism_reading, liberal_democratic_states, agenda_setter).

% State diplomats who negotiated UDHR and subsequent protocols often understood them as aspirational or subject to state consent for binding force. Under the universalist reading, their negotiated intent is overridden by tribunal interpretation, and they have no formal seat in the constraint's administration. Their objections to expanded enforceability are treated as mere political disagreement, not valid revisions to the legal scope.
narrative_ontology:constraint_stakeholder(udhr_authority__binding_universalism_reading, treaty_negotiators, excluded,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__binding_universalism_reading, human_rights_tribunals).
narrative_ontology:fixing_cost_class(udhr_authority__binding_universalism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global standard for individual rights protections (freedom from torture, arbitrary detention, discrimination, etc.) that applies universally regardless of state acceptance. Solves the coordination problem of states being able to opt out of human dignity floors by claiming national sovereignty.
% TRANSFER_FUNCTION: Transfers state autonomy in lawmaking and detention policy to international tribunals and the global human rights regime; individuals gain enforceable claims against their own states without domestic consent; tribunals gain coercive authority over state behavior; civil society organizations gain leverage to enforce norms they could not enforce domestically.
% ABSENT_VOICES: States that reject the universalist reading entirely (sovereigntist states, non-ratifying states, states with competing legal traditions) are structurally excluded from the tribunal system's formal authority. Authoritarian governments would argue that the constraint imposes rights frameworks they did not accept and violates their right to self-determination. Skeptics of international law would object that the reading over-interprets UDHR beyond what text and negotiating history support.
% DISAPPEARANCE_RATIONALE: If tribunals lost the authority to enforce UDHR as binding law regardless of state consent, individual claimants would revert to domestic remedies only; states would recover unilateral control over rights protections; international civil society would lose institutional leverage; and the global human rights regime would collapse to purely voluntary compliance or treaty-based systems requiring explicit state consent. The constraint's disappearance would reorganize international law back to traditional state-consent sovereignty.
% FOUNDING_PROBLEM: Post-World War II, states sought to prevent atrocities by establishing universal human rights standards. The founding problem was: how to create binding rights protections that constrain even non-consenting states, so that sovereignty could not shield genocidal regimes.
% FOUNDING_PROBLEM_CORROBORATION: Liberal democratic states and human rights advocates attest the founding problem is still live — atrocities persist, states still commit torture and arbitrary detention, and only binding universal enforcement prevents worse outcomes. Sovereigntist states and international law skeptics attest the founding problem has shifted: the constraint now functions as enforced homogenization of legal systems and subordination of state autonomy, not as a floor against atrocity. Independent scholarly sources (legal historians, international relations theorists outside the human rights community) show that tribunal interpretation has substantially expanded UDHR's scope beyond the 1948 negotiating consensus.
narrative_ontology:disappearance_verdict(udhr_authority__binding_universalism_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__binding_universalism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__binding_universalism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_authority__binding_universalism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__binding_universalism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__binding_universalism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(udhr_authority__binding_universalism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(udhr_authority__binding_universalism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.68 at interval end) reflects the constraint's high impact on state autonomy: tribunals interpret UDHR as binding law regardless of state consent, extracting from states the right to unilateral lawmaking on rights issues. The suppression value (0.52) reflects moderate active force: states must maintain compliance apparatus, face reputational pressure, and navigate the risk of tribunal orders. Suppression is NOT extremely high because liberal states comply willingly and the tribunal system has limited enforcement mechanisms beyond reputational/diplomatic pressure; it is NOT low because non-consenting and sovereigntist states face genuine coercion. Theater ratio (0.28) is moderate-low: the rights-protection narrative is substantially real (tribunals do enforce genuine rights), but a growing share of tribunal activity defends the constraint's own authority structure (jurisdiction expansions, self-perpetuation) rather than individual rights. The measurement series span 75 years (from UDHR adoption ~1948 to contemporary ~2023), showing extractiveness rising from 0.28 (early aspirational phase) to 0.68 (contemporary binding enforcement), suppression plateauing at moderate levels, and theater ratio stabilizing once the constraint matured. The plateau in extractiveness after year 60 reflects that the constraint reached a steady state of enforceability: further extraction is limited by the ceiling of state compliance leverage, not by tribunal ambition.
 *
 * PERSPECTIVAL GAP:
 *   The tribunal and liberal-state seats should compute as beneficiaries receiving governance authority and reputational benefits from the constraint. The payer seats (non-consenting and sovereigntist states) should compute as targets under high directionality because they bear extraction of autonomy they never accepted. Individual claimants and civil society should compute as beneficiaries because the constraint vests them with enforceable legal standing. The perspectival gap manifests starkly: from the tribunal's position, the constraint is real coordination solving a genuine problem (preventing atrocities). From the sovereigntist state's position, the same structure operates as enforced homogenization and subordination of legitimate self-determination. The engine computes directionality from beneficiary/victim declarations and exit options; the authored claim (tangled_rope) reflects that BOTH coordination and extraction are structurally present. Do NOT reconcile the claim to the metrics — the divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual claimants are beneficiaries (d near 0.0) because the constraint vests them with enforceable claims at zero cost; tribunals are beneficiaries/agenda_setters (d near 0.1–0.2) because they gain institutional authority and reputational power; liberal democratic states are beneficiaries (d near 0.15) because they ratified willingly and comply easily, gaining the reputational premium and the level playing field the constraint provides. Non-consenting states are victims (d near 0.85–0.95) because they are bound to authority they never accepted and bear extraction of autonomy; sovereigntist states are victims (d near 0.9) because the constraint subordinates their legal systems to external interpretation. Civil society is a beneficiary (d near 0.2) because it gains standing and leverage without administrative burden. Treaty negotiators and excluded states would have d near 1.0 if given a seat (fully targeted by tribunal authority they cannot influence), but they are excluded from the stakeholder system so their directionality is not modeled. The directionality derivation works: beneficiaries get low d (low effective extraction on them), victims get high d (high effective extraction on them), which supports the tangled_rope classification (coordination function for beneficiaries, extraction on payers).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint shows signs of mandatrophy drift. The founding problem (preventing atrocities by establishing universal rights floors) was live at t0 and remains genuinely important at t1, BUT tribunal interpretation has expanded UDHR well beyond the atrocity-prevention floor. Modern tribunal cases enforce positive social rights (education, healthcare), remedial justice standards, and victim participation rights that were not negotiated into the 1948 consensus. The constraint's manifest function (preventing state atrocities) and its enacted function (enforcing tribunal authority over state law) are increasingly decoupled. Sovereigntist states argue the founding problem has shifted — the real problem is NO LONGER state atrocities but tribunal overreach and loss of democratic lawmaking. The theater ratio stabilizes at 0.28 because tribunal activity increasingly defends its own jurisdiction and interpretive authority, not just individual rights. The mandatrophy is not fully resolved — the constraint still produces genuine rights protections — but the divergence between founding and enacted functions is substantive. An omega variable addresses this below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tribunal_authority_vs_state_consent,
    'Is tribunal authority to enforce UDHR grounded in binding international law that exists regardless of state consent, or is it grounded in the consent of signatory states that accepted human rights treaties?',
    'Examine the legal sources tribunals cite: if the cited authority is the treaty text + tribunal precedent (closed hermeneutic circle), the grounding is self-referential; if the cited authority includes state customary practice and opinio juris, the grounding is consensual. A state''s formal objection to tribunal jurisdiction, maintained over decades without reversal, would demonstrate the absence of binding authority over that state.',
    'If tribunal authority requires actual state consent, non-consenting and sovereigntist states are not bound, and the constraint degrades from tangled_rope (coordination + extraction) to snare (pure extraction maintained by reputational coercion). If tribunal authority is truly binding regardless of consent, the constraint stands as tangled_rope, and mandatrophy is present but not resolved.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tribunal_authority_vs_state_consent, empirical, 'Whether tribunal enforceability is grounded in state consent or in universalist authority independent of consent.').

omega_variable(
    mandatrophy_atrocity_prevention_drift,
    'Has the constraint''s primary function drifted from preventing state atrocities (the founding problem) to enforcing tribunal authority and expanding rights doctrine beyond the 1948 consensus?',
    'Analyze tribunal caseload over time: categorize cases as atrocity-prevention (torture, genocide, arbitrary detention) vs. doctrine-expansion (social rights, remedial justice, procedural participation). If atrocity cases decline as a proportion of total caseload while doctrine-expansion cases rise, the drift is factual. Interview tribunal administrators and sovereigntist state representatives about their perception of the constraint''s function.',
    'If mandatrophy is confirmed, the constraint has resolved the founding problem (atrocities are less common, treaty regimes established) while persisting through institutional inertia and self-maintenance. The theater_ratio plateau at 0.28 supports this: tribunal activity increasingly defends the constraint''s authority structure rather than individual rights. A mandatrophy resolution would demote the constraint from tangled_rope to piton (theater-maintained, no concentrated beneficiary profiting, no concentrated payer willing to fix it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_atrocity_prevention_drift, empirical, 'Whether the constraint has drifted from atrocity prevention to tribunal authority self-maintenance.').

omega_variable(
    reading_identity_axiom_overriding,
    'Is the binding_universalism reading''s foundational axiom (that individual rights are justiciable regardless of state consent) being overridden by state practice and evolving opinio juris that reasserts consent-based authority?',
    'Track state responses to tribunal orders: are states formally withdrawing from human rights treaties, refusing to implement judgments, or building consensus around a consent-based alternative framework? A sustained pattern of non-compliance without retaliation, or successful treaty amendment to restore state consent requirements, would demonstrate axiom overriding.',
    'If the axiom is being overridden, the reading''s structural identity is dissolving, and the constraint may be shifting toward the customary_emergence_reading (where binding force derives from evolved custom rather than inherent universalism) or toward the aspirational_sovereignty_reading (where consent is restored as a gate). The engine would detect this as an axiom_overriding drift state in cs_structure.drift_state.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_identity_axiom_overriding, empirical, 'Whether the binding universalism axiom is being overridden by state reassertion of consent-based authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__binding_universalism_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t0, udhr_authority__binding_universalism_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(udhr_tr_t0, observed).
narrative_ontology:measurement(udhr_tr_t10, udhr_authority__binding_universalism_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement_basis(udhr_tr_t10, observed).
narrative_ontology:measurement(udhr_tr_t20, udhr_authority__binding_universalism_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement_basis(udhr_tr_t20, observed).
narrative_ontology:measurement(udhr_tr_t30, udhr_authority__binding_universalism_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(udhr_tr_t30, observed).
narrative_ontology:measurement(udhr_tr_t45, udhr_authority__binding_universalism_reading, theater_ratio, 45, 0.26).
narrative_ontology:measurement_basis(udhr_tr_t45, observed).
narrative_ontology:measurement(udhr_tr_t60, udhr_authority__binding_universalism_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(udhr_tr_t60, observed).
narrative_ontology:measurement(udhr_tr_t75, udhr_authority__binding_universalism_reading, theater_ratio, 75, 0.28).
narrative_ontology:measurement_basis(udhr_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(udhr_be_t0, udhr_authority__binding_universalism_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(udhr_be_t0, observed).
narrative_ontology:measurement(udhr_be_t10, udhr_authority__binding_universalism_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement_basis(udhr_be_t10, observed).
narrative_ontology:measurement(udhr_be_t20, udhr_authority__binding_universalism_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(udhr_be_t20, observed).
narrative_ontology:measurement(udhr_be_t30, udhr_authority__binding_universalism_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement_basis(udhr_be_t30, observed).
narrative_ontology:measurement(udhr_be_t45, udhr_authority__binding_universalism_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement_basis(udhr_be_t45, observed).
narrative_ontology:measurement(udhr_be_t60, udhr_authority__binding_universalism_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(udhr_be_t60, observed).
narrative_ontology:measurement(udhr_be_t75, udhr_authority__binding_universalism_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(udhr_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(udhr_su_t0, udhr_authority__binding_universalism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(udhr_su_t0, observed).
narrative_ontology:measurement(udhr_su_t10, udhr_authority__binding_universalism_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement_basis(udhr_su_t10, observed).
narrative_ontology:measurement(udhr_su_t20, udhr_authority__binding_universalism_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(udhr_su_t20, observed).
narrative_ontology:measurement(udhr_su_t30, udhr_authority__binding_universalism_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement_basis(udhr_su_t30, observed).
narrative_ontology:measurement(udhr_su_t45, udhr_authority__binding_universalism_reading, suppression_requirement, 45, 0.51).
narrative_ontology:measurement_basis(udhr_su_t45, observed).
narrative_ontology:measurement(udhr_su_t60, udhr_authority__binding_universalism_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(udhr_su_t60, observed).
narrative_ontology:measurement(udhr_su_t75, udhr_authority__binding_universalism_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement_basis(udhr_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__binding_universalism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__binding_universalism_reading, 0.18).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__aspirational_sovereignty_reading).
narrative_ontology:affects_constraint(udhr_authority__binding_universalism_reading, udhr_authority__customary_emergence_reading).

% DUAL FORMULATION NOTE:
% The UDHR authority kernel is contested across three structurally distinct readings: binding_universalism_reading (this constraint) asserts tribunal authority regardless of state consent; aspirational_sovereignty_reading asserts UDHR is moral guidance requiring explicit state consent for binding force; customary_emergence_reading asserts UDHR evolved from aspiration to binding custom through state practice. Each reading instantiates a different constraint with different ε, different beneficiary/victim structures, and different types. They are linked as a constraint family through affects_constraints edges. The epsilon values diverge because the readings assess DIFFERENT referents (tribunal authority vs. moral guidance vs. evolved custom) under DIFFERENT authority structures. Do not merge them into one constraint with measurement-basis parameters.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(udhr_authority__binding_universalism_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
