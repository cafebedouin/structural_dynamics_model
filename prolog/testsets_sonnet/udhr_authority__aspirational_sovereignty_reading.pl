% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__aspirational_sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR as Aspirational Moral Guidance Requiring State Consent
 *   domain: international_law/political_philosophy/human_rights
 *
 * SUMMARY:
 *   This story instantiates the aspirational-sovereignty reading of the UDHR
 *   authority kernel: the UDHR is a moral and diplomatic reference document
 *   whose provisions become legally binding only when a state affirmatively
 *   consents through treaty ratification, and international tribunals lack
 *   coercive jurisdiction absent that consent. This is a distinct constraint
 *   from the binding-universalism reading (which treats UDHR rights as
 *   justiciable regardless of consent) and the customary-emergence reading
 *   (which treats decades of state practice as having crystallized binding
 *   custom independent of fresh consent). Each reading has its own epsilon:
 *   this reading's extractiveness is low because the constraint imposes
 *   almost no coercive cost on states — its cost falls instead on individuals
 *   who lack the standing this reading withholds. The three readings are not
 *   measurement variants of one constraint; they are structurally distinct
 *   claims about where authority sits, and are authored as three separate
 *   stories linked via network edges.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary beneficiary (institutional/arbitrage) — retain veto over binding obligation
 *   - individual_rights_claimants: Primary target (powerless/trapped) — bear the enforcement gap
 *   - international_tribunals: Secondary institutional actor (institutional/analytical) — structurally denied coercive jurisdiction under this reading
 *   - international_law_scholars: Analytical observer — track the kernel contest across readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__aspirational_sovereignty_reading, 0.18).
domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, 0.12).
domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__aspirational_sovereignty_reading, rope).
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR as Aspirational Moral Guidance Requiring State Consent").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international_law/political_philosophy/human_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, '7ce48396-4649-4632-b8a7-f9520090ce78').
narrative_ontology:cs_kernel_codification('7ce48396-4649-4632-b8a7-f9520090ce78', fixed_text).
narrative_ontology:cs_authority_grounding('7ce48396-4649-4632-b8a7-f9520090ce78', distributed).
narrative_ontology:cs_reading_relation('7ce48396-4649-4632-b8a7-f9520090ce78', udhr_authority__binding_universalism_reading, forecloses).
narrative_ontology:cs_reading_relation('7ce48396-4649-4632-b8a7-f9520090ce78', udhr_authority__customary_emergence_reading, influences).
narrative_ontology:cs_axiom('7ce48396-4649-4632-b8a7-f9520090ce78', foundational, state_consent_necessary_for_binding_obligation).
narrative_ontology:cs_axiom_status(state_consent_necessary_for_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('7ce48396-4649-4632-b8a7-f9520090ce78', state_consent_necessary_for_binding_obligation, conventional).
narrative_ontology:cs_axiom('7ce48396-4649-4632-b8a7-f9520090ce78', secondary, sovereign_equality_requires_veto_over_external_legal_imposition).
narrative_ontology:cs_axiom_status(sovereign_equality_requires_veto_over_external_legal_imposition, holdable).
narrative_ontology:cs_axiom_grounding('7ce48396-4649-4632-b8a7-f9520090ce78', sovereign_equality_requires_veto_over_external_legal_imposition, conventional).
narrative_ontology:cs_reference_frame('7ce48396-4649-4632-b8a7-f9520090ce78', consent_based_westphalian_sovereignty).
narrative_ontology:cs_drift_state('7ce48396-4649-4632-b8a7-f9520090ce78', post_cold_war_treaty_proliferation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ce48396-4649-4632-b8a7-f9520090ce78', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, diplomatic_negotiators).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, non_ratifying_governments).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, individual_rights_claimants).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, stateless_persons).
narrative_ontology:constraint_victim(udhr_authority__aspirational_sovereignty_reading, victims_of_state_abuse_without_treaty_recourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain full discretion over whether UDHR principles become binding domestic or international law by choosing whether to ratify specific covenants and treaties. Can cite the UDHR rhetorically for legitimacy while declining the obligations that would flow from treaty ratification. Exit from any binding consequence is built into the reading itself: consent is the gate.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, sovereign_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__aspirational_sovereignty_reading, sovereign_states, agenda_setter).

% Use the UDHR's declaratory status as negotiating room: it lets states affirm human rights language in communiques and resolutions without binding commitments, preserving flexibility in bilateral and multilateral relations.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, diplomatic_negotiators, beneficiary,
    organized, biographical, mobile, global).

% Governments that have not ratified specific human rights covenants point to the UDHR's aspirational status to justify non-binding compliance, avoiding both domestic legal exposure and international tribunal jurisdiction.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, non_ratifying_governments, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals whose rights are violated by a state that has not consented to binding obligations have no tribunal with coercive jurisdiction to appeal to under this reading; the UDHR offers moral condemnation but no enforceable remedy. Their exit is bounded entirely by their own state's consent choices, which they do not control.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, individual_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Lack even the mediated protection a consenting state might extend to its nationals; under a consent-gated reading, no state's ratification choice creates an obligation running to them, leaving the UDHR's guarantees purely rhetorical for this group.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, stateless_persons, payer,
    powerless, biographical, trapped, global).

% Suffer direct harms (arbitrary detention, torture, suppression) from states that have signed the UDHR declaration but not ratified enforcement-capable covenants; under this reading, the moral guidance carries no coercive weight and their only recourse is diplomatic pressure or domestic remedy, both of which the offending state controls.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, victims_of_state_abuse_without_treaty_recourse, payer,
    powerless, immediate, trapped, national).

% Under this reading, tribunals lack coercive jurisdiction over non-consenting states; their rulings on UDHR-derived claims are persuasive at best. They would argue for binding force but structurally cannot compel it without the treaty ratification this reading makes a precondition.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, international_tribunals, excluded,
    institutional, generational, analytical, global).

% Document violations and press for accountability but find the consent-gated reading undercuts legal leverage; they are not party to the state-consent mechanism that this reading makes decisive and can only operate through advocacy and reputational pressure.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, human_rights_ngos, excluded,
    organized, biographical, constrained, global).

% Debate whether the UDHR's authority is best modeled as aspirational (this reading), binding universalist, or customary-emergent; their scholarship shapes which reading dominant courts and foreign ministries cite, without itself resolving the kernel contest.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared moral vocabulary and reference point that lets states coordinate on human rights norms rhetorically without surrendering sovereign discretion over binding legal commitment — a genuine, low-cost coordination device for diplomatic communication.
% TRANSFER_FUNCTION: Moves reputational and moral capital toward states that invoke the UDHR's language while allowing them to withhold the enforceable protections that would otherwise flow to individual rights-claimants; the cost of non-enforcement falls on those without treaty-based recourse.
% ABSENT_VOICES: Individual rights claimants, stateless persons, and victims of state abuse have no seat in the state-consent mechanism this reading makes decisive — they are the parties most affected by the enforcement gap but structurally excluded from the negotiations that would close it.
% DISAPPEARANCE_RATIONALE: States and diplomats would report little practical change if the aspirational-sovereignty reading vanished, since consent-based treaty mechanisms would continue to govern most binding obligations regardless. Rights advocates and affected individuals would contest this, arguing that displacing this reading in favor of binding universalism would materially open new avenues of legal recourse — hence the verdict is genuinely disputed between the parties rather than settled by either.
% FOUNDING_PROBLEM: The UDHR was drafted in 1948 to articulate a common moral standard across sharply divided post-war ideological blocs (Western liberal democracies, Soviet bloc, newly decolonizing states) without requiring immediate legal harmonization that no bloc would accept — declaratory consensus was the only form of agreement then achievable.
% FOUNDING_PROBLEM_CORROBORATION: State governments and their legal advisors attest the founding problem (achieving consensus across incompatible legal systems without premature binding commitment) remains live and justifies continued consent-gating. Independent international law scholars and UN human rights treaty body reports attest that the original problem has been substantially superseded by seven decades of subsequent treaty-making, and that the consent-gate now functions primarily to shield persistent violators rather than to preserve genuine pluralism — this corroboration comes from scholarship and treaty-body findings outside the state governments that benefit from the reading.
narrative_ontology:disappearance_verdict(udhr_authority__aspirational_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(udhr_authority__aspirational_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__aspirational_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(udhr_authority__aspirational_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__aspirational_sovereignty_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__aspirational_sovereignty_reading_tests).
:- end_tests(udhr_authority__aspirational_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) because, from the reading's own structural logic, states bear almost no cost — the constraint imposes voluntary, self-selected obligation. Suppression is likewise low (0.12): there is no active machinery coercing states into compliance beyond what they consent to. Theater ratio is moderate and rising (0.25 to 0.40 over the interval) because declaratory invocation of the UDHR in diplomatic communications has grown steadily even as binding treaty ratification rates have plateaued in many regions — an increasing share of UDHR-citation activity is performative affirmation rather than movement toward consent-based obligation. Accessibility collapse is low-moderate (0.25): the aspirational framing keeps alternative interpretive readings (binding universalism, customary emergence) fully alive in scholarly and judicial discourse; this reading has not foreclosed them. Resistance is moderate (0.35), reflecting decades of scholarly and NGO pushback against the consent-gate framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign states and non-ratifying governments sit near the full-beneficiary end: the reading's entire function is to preserve their discretion, so directionality derives low d for them. Individual rights claimants, stateless persons, and victims of state abuse sit near the full-target end: they bear the consequence of the enforcement gap this reading creates and have no exit — they cannot choose their state's ratification posture. International tribunals and NGOs are excluded rather than coordinated: they have no seat in the consent mechanism this reading makes decisive.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (achieving cross-ideological consensus in 1948 without forcing premature binding commitment) was genuinely live at drafting and prevented the entire project from collapsing under Cold War-era disagreement. Whether that problem remains live in 2024 is exactly the contested question this story routes through the R5 fields: state governments say yes, external scholarship and treaty-body findings say the problem has been substantially resolved by seventy years of subsequent treaty infrastructure, leaving the consent-gate now serving mainly to shield holdout violators. The classification avoids mislabeling this reading as pure extraction (it did solve a real coordination problem at founding) or as pure coordination (its persistence past that founding moment now falls disproportionately on parties with no voice in the consent mechanism).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    udhr_kernel_reading_contest,
    'Is the UDHR''s authority correctly modeled as consent-gated (this reading), as directly binding regardless of consent (binding_universalism_reading), or as having migrated into binding customary international law through accumulated state practice (customary_emergence_reading)?',
    'Track ICJ and regional human rights tribunal rulings that either affirm consent-gating, assert direct justiciability, or find customary status for specific UDHR provisions (e.g., prohibition on torture, genocide) independent of treaty ratification; a sustained pattern of tribunals finding customary or peremptory (jus cogens) status for a provision would structurally undermine this reading for that provision.',
    'If tribunals and state practice increasingly find customary-law status for core UDHR provisions, this reading''s low-extractiveness, low-suppression profile no longer describes those provisions accurately — the customary_emergence_reading would supersede this reading for that subset, and the constraint family''s network structure would need edges reflecting the erosion of the consent-gate for those specific rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(udhr_kernel_reading_contest, conceptual, 'Which of the three sibling readings of UDHR authority best describes current international legal practice, and whether that description is shifting over time.').

omega_variable(
    founding_problem_obsolescence,
    'Has the 1948 founding problem (achieving consensus across incompatible post-war legal-ideological blocs without forcing premature binding commitment) been substantially resolved by subsequent covenant infrastructure (ICCPR, ICESCR, regional human rights courts), such that the consent-gate now serves a different function than the one it was built for?',
    'Compare ratification rates and enforcement outcomes for the core covenants across the period 1966-2024; if near-universal ratification of enforcement-capable instruments has occurred for most rights, the consent-gate''s persistence for the remainder increasingly reflects holdout-shielding rather than genuine pluralism-preservation.',
    'If the founding problem is substantially dead, the aspirational-sovereignty reading functions closer to a scaffold whose sunset condition has been met but not declared, or a piton maintained by inertia and diplomatic convenience rather than continued coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the 1948 consensus-building rationale for consent-gating still applies given decades of subsequent treaty-making.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_tr_t1948, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1948, 0.25).
narrative_ontology:measurement(udhr_tr_t1966, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1966, 0.3).
narrative_ontology:measurement(udhr_tr_t1984, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1984, 0.34).
narrative_ontology:measurement(udhr_tr_t2000, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2000, 0.37).
narrative_ontology:measurement(udhr_tr_t2012, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2012, 0.39).
narrative_ontology:measurement(udhr_tr_t2024, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(udhr_be_t1948, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1948, 0.08).
narrative_ontology:measurement(udhr_be_t1966, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1966, 0.1).
narrative_ontology:measurement(udhr_be_t1984, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1984, 0.13).
narrative_ontology:measurement(udhr_be_t2000, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(udhr_be_t2012, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2012, 0.17).
narrative_ontology:measurement(udhr_be_t2024, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2024, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(udhr_authority__aspirational_sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__aspirational_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(udhr_authority__aspirational_sovereignty_reading, 0.05).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, udhr_authority__customary_emergence_reading).

% DUAL FORMULATION NOTE:
% Three-story constraint family decomposing the natural-language label 'UDHR authority' per the epsilon-invariance principle. This story (aspirational_sovereignty_reading) authors low extractiveness/suppression because its structural claim is that no binding obligation exists absent state consent. The binding_universalism_reading sibling authors a structurally distinct, higher-extraction profile because it claims direct justiciability against non-consenting states. The customary_emergence_reading sibling occupies an intermediate position, tracking the gradual erosion of the consent-gate through accumulated state practice. All three share the same underlying text (the UDHR) but instantiate different authority structures over it — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
