% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction under the Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   Under the hybrid complementarity reading, the Rome Statute system holds
 *   residual criminal jurisdiction that reaches past case-by-case consent,
 *   yet operates only through complementarity deference and voluntary state
 *   cooperation. The arrangement solves a real problem: a permanent backstop
 *   court replaced the per-crisis scramble for ad hoc tribunals, and the
 *   admissibility rule kept sovereigns willing to ratify. The same structure
 *   extracts asymmetrically: legal exposure concentrates on nationals of weak
 *   party states while powerful non-party states self-exempt and even
 *   sanction the prosecutors, and victims in unwilling states absorb
 *   indefinite delay when deference meets cooperation failure. The
 *   claim/metric gap is deliberate: the arrangement is CLAIMED here as
 *   tangled_rope, and the authored metrics describe moderately-high,
 *   actively-enforced, slowly-intensifying extraction riding on a genuine
 *   coordination function. The engine computes per-seat classifications from
 *   the structural data; the claim is not tuned to any predicted output. This
 *   file is one reading of the rome_statute_jurisdiction kernel; the
 *   universalist and sovereigntist readings are separate constraints with
 *   their own epsilon values, linked through the network section.
 *
 * KEY AGENTS:
 *   - - assembly_of_states_parties: Agenda setter (institutional/constrained) — administers budget, elections, amendments; keeps the machinery alive
 *   - - icc_office_of_the_prosecutor: Agenda setter (institutional/identity_locked) — selects situations, applies admissibility deference, depends wholly on state cooperation
 *   - - states_parties: Primary beneficiary, secondary payer (organized/constrained) — retain domestic primacy, fund the backstop
 *   - - powerful_nonparty_states: Structural beneficiary (powerful/arbitrage) — self-exempt while the norm disciplines others
 *   - - united_nations_security_council: Intermittent agenda setter (institutional/arbitrage) — refers or starves situations by veto
 *   - - atrocity_victims_in_unwilling_states: Primary target (powerless/trapped) — bear deferred justice when deference meets non-cooperation
 *   - - leaders_of_weak_party_states: Target (moderate/trapped) — carry prosecution exposure their powerful counterparts escape
 *   - - victims_of_nonparty_state_atrocities: Excluded voice (powerless/trapped) — no path to the forum absent a veto-blocked referral
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.62).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.63).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction under the Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'fc3917bf-8d5d-47d4-9f96-b733cae40532').
narrative_ontology:cs_kernel_codification('fc3917bf-8d5d-47d4-9f96-b733cae40532', fixed_text).
narrative_ontology:cs_authority_grounding('fc3917bf-8d5d-47d4-9f96-b733cae40532', lineage).
narrative_ontology:cs_interpretation_layer_present('fc3917bf-8d5d-47d4-9f96-b733cae40532').
narrative_ontology:cs_reading_relation('fc3917bf-8d5d-47d4-9f96-b733cae40532', rome_statute_jurisdiction__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('fc3917bf-8d5d-47d4-9f96-b733cae40532', rome_statute_jurisdiction__universalist_reading, forecloses).
narrative_ontology:cs_axiom('fc3917bf-8d5d-47d4-9f96-b733cae40532', foundational, complementarity_defers_to_genuine_domestic_proceedings).
narrative_ontology:cs_axiom_status(complementarity_defers_to_genuine_domestic_proceedings, holdable).
narrative_ontology:cs_axiom_grounding('fc3917bf-8d5d-47d4-9f96-b733cae40532', complementarity_defers_to_genuine_domestic_proceedings, conventional).
narrative_ontology:cs_axiom('fc3917bf-8d5d-47d4-9f96-b733cae40532', foundational, residual_authority_extends_beyond_case_by_case_consent).
narrative_ontology:cs_axiom_status(residual_authority_extends_beyond_case_by_case_consent, holdable).
narrative_ontology:cs_axiom_grounding('fc3917bf-8d5d-47d4-9f96-b733cae40532', residual_authority_extends_beyond_case_by_case_consent, deontological).
narrative_ontology:cs_axiom('fc3917bf-8d5d-47d4-9f96-b733cae40532', secondary, state_cooperation_is_enforcement_precondition).
narrative_ontology:cs_axiom_status(state_cooperation_is_enforcement_precondition, holdable).
narrative_ontology:cs_axiom_grounding('fc3917bf-8d5d-47d4-9f96-b733cae40532', state_cooperation_is_enforcement_precondition, conventional).
narrative_ontology:cs_reference_frame('fc3917bf-8d5d-47d4-9f96-b733cae40532', consent_mediated_residual_authority).
narrative_ontology:cs_drift_state('fc3917bf-8d5d-47d4-9f96-b733cae40532', post_cooperation_crisis_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fc3917bf-8d5d-47d4-9f96-b733cae40532', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, powerful_nonparty_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_unwilling_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, leaders_of_weak_party_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Elects the judges and prosecutor, adopts the budget after annual contribution fights, and considers amendments. Each member state pays assessed contributions and absorbs diplomatic friction for the court's sake. Dismantling or starving the arrangement would require coordinated withdrawal that members have been unwilling to pay the reputational cost of, so the body maintains the machinery year after year.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, assembly_of_states_parties, agenda_setter,
    institutional, generational, constrained, global).

% Selects situations and cases, screens them through the admissibility test that defers to genuine domestic proceedings, and litigates before the judges. It owns no police: evidence, witnesses, and arrested persons arrive only if states hand them over. Its institutional purpose is fused with the mandate it administers, so retreating from the mandate would dissolve the office itself.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_office_of_the_prosecutor, agenda_setter,
    institutional, biographical, identity_locked, global).

% Keep primary prosecutorial jurisdiction inside their own courts under the admissibility rule while gaining a permanent backstop that deters atrocity and absorbs cases their own systems cannot or will not take. They fund the court and staff its governing body. Withdrawal is legally available but carries visible cost, as the one completed withdrawal demonstrated.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, states_parties, payer).

% Remain outside the treaty, shield their personnel through non-membership and bilateral non-surrender agreements, and engage selectively through Security Council votes and, at times, sanctions against the court's own prosecutors. They contribute nothing to the budget yet benefit as the accountability norm disciplines rivals and as the docket concentrates on situations they do not control.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, powerful_nonparty_states, beneficiary,
    powerful, generational, arbitrage, global).

% Can refer situations to the court, binding even non-party states, or suspend investigations for renewable twelve-month periods. Permanent-member vetoes block referrals touching their own forces or clients. It feeds the court selectively while its own members sit largely beyond the court's reach, shaping the docket without bearing its obligations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, united_nations_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% Turn to the court precisely when their domestic systems are unwilling or unable to act, then bear indefinite delay when deference to those systems meets cooperation failure: warrants unexecuted for years, suspects traveling freely, reparations funded from indigent defendants. They have no alternative forum of comparable reach and no vote in the bodies that decide the docket.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_unwilling_states, payer,
    powerless, generational, trapped, global).

% Face indictment, travel restrictions, and asset measures issued from The Hague. They lack the great-power shields that keep comparable actors beyond the court's reach; some evade arrest for years by circulating among non-party or friendly states, living under warrant rather than resolving it.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, leaders_of_weak_party_states, payer,
    moderate, biographical, trapped, regional).

% Suffer atrocities committed by forces of powerful non-party states or their protected allies. No path leads to the court for them except a Security Council referral their patrons can veto. They would object that the mechanism's balance sets the price of their justice at zero, but they are not seated anywhere in the arrangement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_nonparty_state_atrocities, excluded,
    powerless, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, powerful_nonparty_states).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the impunity-gap problem for mass atrocity without demanding universal sovereign submission: a permanent standing court replaces per-crisis ad hoc tribunals, while the admissibility rule preserves domestic first refusal on prosecution, which kept ratification feasible for states that would never accept a supranational criminal court with plenary reach.
% TRANSFER_FUNCTION: Moves prosecutorial authority, and the political cost of pursuing atrocity cases, from national systems to a permanent international court in situations where states are unwilling or unable to act; moves budget resources from states parties to the court; and concentrates individual legal exposure on persons inside situations the court can reach.
% ABSENT_VOICES: Victims of atrocities by powerful non-party states' forces are structurally absent: the referral route to them runs through a Security Council their patrons can veto. Accused persons from powerful states are likewise absent from the docket rather than from the conversation. States that would trade contributions for real enforcement powers have no seat where that trade could be struck.
% DISAPPEARANCE_RATIONALE: If the Statute's jurisdictional scheme vanished overnight, every atrocity response would revert to negotiated ad hoc tribunals or nothing; sitting cases and warrants would collapse; states parties would lose the backstop they fund; and the accumulated expectation that atrocity attracts some standing process would unwind within a decade.
% FOUNDING_PROBLEM: Ad hoc justice was reactive, expensive, and selective: Nuremberg tried the losers, the Yugoslavia and Rwanda tribunals took years to build after the fact, and atrocities in states unable or unwilling to prosecute met no institution at all.
% FOUNDING_PROBLEM_CORROBORATION: UN Commission of Inquiry findings, successive Secretary-General reports on the protection of civilians, and documentation by Amnesty International and Human Rights Watch all attest that mass atrocity with no willing or able domestic forum continues. Academic international criminal law scholarship corroborates independently. The court's own promotional materials make the same claim but carry no evidentiary weight here; the external sources suffice.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.62 at interval end because the arrangement's costs land unevenly: the docket concentrated for its first two decades on situations in weak states, cooperation failures left warrants unexecuted for years, and the beneficiaries of that pattern include states that never accepted the court's jurisdiction over anyone. Suppression is 0.63 and unscaled by construction: it reflects the structural fact that the court commands no enforcement organ and that the admissibility rule forecloses parallel national proceedings, channeling victims into a forum whose output depends on the goodwill of the very states under scrutiny. Theater is 0.32: convictions and functioning trials are real, but a growing share of activity is annual-session rhetoric, cooperation resolutions nobody obeys, and positive-complementarity declarations without follow-through. Accessibility collapse is 0.45 because alternatives genuinely persist: ad hoc and hybrid tribunals have been re-created for new crises, and national universal-jurisdiction proceedings continue in several European courts. Resistance is 0.60: an organized non-cooperation campaign, a completed withdrawal, announced withdrawals, budget withholding threats, and direct sanctions against the prosecutor. The temporal series run on one shared seven-point grid (2002-2024) so every tracked metric is authored at every examined time point; trajectories rise monotonically rather than cyclically, though the underlying history cycles (crisis, referral, non-cooperation standoff, recalibration) — the net drift, not the cycle, is what the grid records, and the cycle's intermittent-reinforcement quality is noted as context rather than modeled as oscillation. On coalition: the payer seats are not without resources — weak states coordinate as an ASP voting bloc and extracted concessions during budget fights — but their coalition power has moved budgets, not dockets or enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the assembly_of_states_parties and states_parties seats the arrangement computes rope-like: a cheap insurance policy preserving domestic primacy against a catastrophic tail risk. From the atrocity_victims_in_unwilling_states and leaders_of_weak_party_states seats the same structure computes as enforced extraction: a forum they cannot refuse, whose reach stops precisely where great-power interests begin. From the powerful_nonparty_states seat the constraint is nearly costless and mildly useful — a disciplining norm aimed elsewhere. The engine derives these divergences from the declared roles, power atoms, and exit options; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   States parties declare as beneficiaries (with a payer secondary role): the admissibility rule subsidizes their retained primacy, so their derived directionality sits well below symmetric. Powerful non-party states declare as beneficiaries on the strength of exemption: the arrangement's selective operation accrues to them as impunity-plus-discipline-of-others, placing them near the beneficiary pole despite contributing nothing. Atrocity victims in unwilling states and leaders of weak party states declare as victims: the former bear the good's non-delivery, the latter its delivery, and both sit near the full-target pole with trapped exits. The prosecutor's office is identity-locked rather than mobile: its mandate is its constitution. No directionality overrides are authored because the beneficiary/victim declarations plus exit options already produce the correct relationships; the one subtle case — powerful non-party states as beneficiaries — is captured by declaring them beneficiaries rather than by overriding a derived target value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy is declared and none should be inferred: atrocities with no willing or able domestic forum continue, and the arrangement still performs its original function, imperfectly. The tangled_rope classification earns its keep by blocking two symmetrical mislabels. Calling the arrangement a snare erases the genuine coordination achievement — a standing court that ended the ad hoc scramble and keeps a deterrence shadow over official conduct — and would predict pure-suppression dynamics the record does not show. Calling it a rope erases the asymmetric extraction — the docket skew, the cooperation gap that falls hardest on those with least exit, the self-exemption of the powerful — and would license treating the current allocation of justice as a fair price of coordination. The hybrid reading's own structure demands the hybrid category: consent-grounded legitimacy and residual universal aspiration are both load-bearing, and the extraction flows through the seam between them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This constraint is one reading of the rome_statute_jurisdiction kernel (hybrid_complementarity_reading). What would each sibling reading change structurally if adopted instead?',
    'Doctrinal analysis locating the disagreement on the necessity/sufficiency of state consent: the sovereigntist reading deletes residual non-consensual authority (shrinking the reachable population and the victim set of non-delivery); the universalist reading deletes the complementarity and consent bars (expanding extraction to all states including non-parties and opening the forum to victims of non-party-state forces).',
    'Sovereigntist adoption would lower epsilon and shrink the beneficiary set; universalist adoption would raise epsilon toward all states and convert victims_of_nonparty_state_atrocities from excluded to payer. Classification of this file applies only under the hybrid reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: one of three readings of a contested jurisdictional kernel.').

omega_variable(
    complementarity_genuineness,
    'Is complementarity operating as a genuine coordination device (avoiding duplicative prosecution, catalyzing domestic capacity) or as a deference ratchet that launders indefinite impunity?',
    'Compare domestic prosecution rates and case quality before and after admissibility rulings and positive-complementarity programs in specific situations (for example, Colombian transitional-justice proceedings or Guinean domestic trials following ICC pressure).',
    'If deference dominates, the arrangement slides toward snare-flavored extraction and the coordination-function gate weakens; if capacity-building dominates, the rope component strengthens and effective extraction falls for the beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_genuineness, empirical, 'Whether the admissibility rule coordinates or merely delays.').

omega_variable(
    docket_selectivity_driver,
    'Is the concentration of cases on weak-state situations intrinsic to the mechanism, or contingent on current referral politics and where atrocity coincides with state incapacity?',
    'Counterfactual docket analysis: examine situations where powerful-state forces allegedly committed statutory crimes within party-state territory, and track whether proprio motu and territorial-jurisdiction theories produce cases against powerful-state nationals over time.',
    'If intrinsic, the extraction is a structural property of complementarity-as-designed and the tangled_rope reading hardens; if contingent, extraction may fall with political change and the arrangement sits closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(docket_selectivity_driver, empirical, 'Whether asymmetric application is designed-in or circumstantial.').

omega_variable(
    enforcement_failure_attribution,
    'Are enforcement failures attributable to the Statute''s design (deliberately no enforcement organ, cooperation-dependent by construction) or to contingent state defiance that a differently-configured politics would cure?',
    'Compare execution rates of requests across situations and across time, controlling for target-state capacity: uniformly low execution regardless of capacity indicates design; variable execution tracking political alignment indicates contingency.',
    'Design attribution makes the cooperation gap part of the constraint''s intrinsic extraction profile; contingency attribution makes it a repairable externality and lowers the structural suppression estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_failure_attribution, conceptual, 'Whether the enforcement gap is architectural or political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 2002, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.14).
narrative_ontology:measurement(rome_tr_t2006, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2006, 0.18).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2014, 0.26).
narrative_ontology:measurement(rome_tr_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2018, 0.29).
narrative_ontology:measurement(rome_tr_t2021, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2021, 0.31).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.32).

% Extraction over time
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.34).
narrative_ontology:measurement(rome_be_t2006, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2006, 0.42).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2010, 0.49).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement(rome_be_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2018, 0.58).
narrative_ontology:measurement(rome_be_t2021, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2021, 0.6).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.38).
narrative_ontology:measurement(rome_su_t2006, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2006, 0.44).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2010, 0.49).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement(rome_su_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2018, 0.58).
narrative_ontology:measurement(rome_su_t2021, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2021, 0.61).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Rome Statute jurisdiction' decomposes into three structurally distinct constraints, one per reading of the kernel. Epsilon differs across the family because the referent shifts with the reading: the universalist reading authors epsilon against a mandate binding all states (highest extraction, largest victim set including non-party-state victims); the sovereigntist reading authors epsilon against a strictly consent-bound framework (lowest extraction, minimal non-party reach); this hybrid reading authors epsilon against the standing arrangement as operated — residual authority gated by complementarity deference and cooperation (intermediate extraction, asymmetrically distributed). Each file links the other two through affects_constraints; the upstream textual kernel is common, and each reading's axioms contradict the siblings' on the necessity and sufficiency of consent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
