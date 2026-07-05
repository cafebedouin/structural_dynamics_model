% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__jurisdictional_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__jurisdictional_sovereignty, []).

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
 *   constraint_id: border_control_legitimacy__jurisdictional_sovereignty
 *   human_readable: Border Control as Balanced Jurisdictional Authority (Proportionality-Constrained Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This story instantiates the jurisdictional-sovereignty reading of the
 *   border control legitimacy kernel: sovereignty grants a state authority to
 *   regulate rights and obligations within its territory, but this authority
 *   does not automatically extend to unconstrained border closure. Legitimacy
 *   on this reading is a balancing act among three obligations —
 *   international protection duties, domestic labor-market needs, and the
 *   political consent of the resident population — mediated through
 *   proportionality and necessity review. This is structurally distinct from
 *   the sovereignty_primary reading (which treats exclusion as constitutive
 *   of statehood, immune to external balancing tests) and from the
 *   freedom_of_movement_primary reading (which treats territorial exclusion
 *   as presumptively illegitimate against a background right to move). Each
 *   reading is authored as its own constraint story with its own ε; this file
 *   does not average across them.
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: agenda_setter (institutional/analytical) — designs and operates the balancing regime, bound by proportionality review
 *   - receiving_state_citizens: beneficiary (organized/constrained) — consent to and shape admission policy through political process
 *   - excluded_asylum_seekers: payer (powerless/trapped) — bear exclusion despite protection claims, have no vote in the consenting polity
 *   - undocumented_labor_migrants: payer (powerless/trapped) — absorbed into the labor market the formal quota system fails to serve
 *   - displaced_citizens_denied_diaspora_return: payer (powerless/constrained) — the reading's distinctive citizen-side victim class
 *   - international_human_rights_bodies: observer (institutional/analytical) — monitor compliance without direct enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, 0.58).
domain_priors:suppression_score(border_control_legitimacy__jurisdictional_sovereignty, 0.62).
domain_priors:theater_ratio(border_control_legitimacy__jurisdictional_sovereignty, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(border_control_legitimacy__jurisdictional_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__jurisdictional_sovereignty, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__jurisdictional_sovereignty, "Border Control as Balanced Jurisdictional Authority (Proportionality-Constrained Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__jurisdictional_sovereignty, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__jurisdictional_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__jurisdictional_sovereignty, 'f0408966-360e-4b2a-b549-e40857dc5046').
narrative_ontology:cs_kernel_codification('f0408966-360e-4b2a-b549-e40857dc5046', distributed).
narrative_ontology:cs_authority_grounding('f0408966-360e-4b2a-b549-e40857dc5046', distributed).
narrative_ontology:cs_reading_relation('f0408966-360e-4b2a-b549-e40857dc5046', border_control_legitimacy__sovereignty_primary, influences).
narrative_ontology:cs_reading_relation('f0408966-360e-4b2a-b549-e40857dc5046', border_control_legitimacy__freedom_of_movement_primary, influences).
narrative_ontology:cs_axiom('f0408966-360e-4b2a-b549-e40857dc5046', foundational, sovereignty_is_jurisdictional_not_exclusionary_by_default).
narrative_ontology:cs_axiom_status(sovereignty_is_jurisdictional_not_exclusionary_by_default, holdable).
narrative_ontology:cs_axiom_grounding('f0408966-360e-4b2a-b549-e40857dc5046', sovereignty_is_jurisdictional_not_exclusionary_by_default, conventional).
narrative_ontology:cs_axiom('f0408966-360e-4b2a-b549-e40857dc5046', foundational, legitimacy_requires_tripartite_balance_of_protection_labor_consent).
narrative_ontology:cs_axiom_status(legitimacy_requires_tripartite_balance_of_protection_labor_consent, holdable).
narrative_ontology:cs_axiom_grounding('f0408966-360e-4b2a-b549-e40857dc5046', legitimacy_requires_tripartite_balance_of_protection_labor_consent, instrumental).
narrative_ontology:cs_reference_frame('f0408966-360e-4b2a-b549-e40857dc5046', postwar_refugee_convention_settlement).
narrative_ontology:cs_drift_state('f0408966-360e-4b2a-b549-e40857dc5046', contemporary_securitized_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f0408966-360e-4b2a-b549-e40857dc5046', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__jurisdictional_sovereignty, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_incumbents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__jurisdictional_sovereignty, state_administrative_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, undocumented_labor_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_denied_diaspora_return).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and operates admission and exclusion regimes, adjudicates asylum claims, negotiates labor migration quotas, and is legally bound (on this reading) to justify enforcement decisions against proportionality and necessity tests rather than exercising unconstrained discretion. Bears the institutional cost of litigation and international scrutiny when enforcement is found disproportionate.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Consent (via electoral and political processes) to the terms of admission and exclusion; benefit from labor-market protection, public-service capacity management, and perceived security, but also bear costs when enforcement failures (visible disorder, perceived unfairness) undermine confidence in the regime's legitimacy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, receiving_state_citizens, beneficiary,
    organized, biographical, constrained, national).

% Benefit from admission controls that limit labor-market competition in sectors where they compete directly with migrant labor; lobby for quotas and enforcement that protect wage floors, while employers in other sectors push for looser admission where labor is scarce.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, domestic_labor_incumbents, beneficiary,
    organized, biographical, constrained, national).

% Seek protection from persecution or violence; face adjudication processes that, even under a proportionality standard, can deny entry or return them to danger. Have no vote in the receiving state's consent process and depend entirely on the state's willingness to apply protection obligations honestly rather than as pretext for exclusion.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, excluded_asylum_seekers, payer,
    powerless, biographical, trapped, global).

% Work in the receiving state's economy, often filling labor needs the formal quota system does not meet, while remaining subject to detention, deportation, and exclusion from legal protections. Their labor is absorbed by the economy that officially bars their entry, exposing the gap between labor need and admission policy.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, undocumented_labor_migrants, payer,
    powerless, biographical, trapped, national).

% Citizens or long-settled residents whose family reunification or return is blocked or delayed by the same jurisdictional machinery that screens non-citizens, illustrating that the sovereignty-as-jurisdiction reading's enforcement apparatus produces citizen-side casualties, not only migrant-side ones — the dual victim structure this reading is built to acknowledge.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, displaced_citizens_denied_diaspora_return, payer,
    powerless, biographical, constrained, national).

% Monitor compliance with protection obligations (non-refoulement, family unity, proportionality in detention) and issue findings when enforcement crosses from balanced jurisdictional regulation into rights violation. Their findings carry no independent enforcement power but shape the legitimacy discourse the reading depends on.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Have strong interest in migration corridors (remittances, emigration pressure relief) but are not parties to the receiving state's internal balancing of protection obligations, labor needs, and public consent — their interests are represented only indirectly, through bilateral negotiation the receiving state can decline.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__jurisdictional_sovereignty, labor_sending_states, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally bounded mechanism for a state to regulate who enters and remains within its territory, balancing three genuine, potentially conflicting demands: international protection obligations toward those fleeing danger, domestic labor-market needs, and the political consent of the resident population to admission policy.
% TRANSFER_FUNCTION: Moves the burden of unmet migration demand (labor shortages, protection needs, family reunification) onto excluded migrants and, secondarily, onto citizens and residents whose family or return claims are processed through the same constrained apparatus; moves political and economic benefit (labor-market protection, perceived order, sovereign discretion) toward the resident citizenry and the administrative apparatus that operates the regime.
% ABSENT_VOICES: Excluded asylum seekers and undocumented migrants have no vote in the consent process that legitimates the regime and no standing before the domestic courts that apply proportionality review in most jurisdictions; labor-sending states are negotiated with, not seated as parties to the domestic balancing test itself.
% DISAPPEARANCE_RATIONALE: If the jurisdictional-authority-with-balancing-constraints framework disappeared and no legitimacy standard replaced it, states would either revert to unconstrained sovereign discretion (sovereignty_primary reading) or open borders under a rights-primary standard (freedom_of_movement_primary reading) — either shift would restructure labor markets, asylum systems, and domestic politics substantially; the current arrangement is not a background fact but an actively maintained legal-political settlement.
% FOUNDING_PROBLEM: Post-WWII international law needed to reconcile the persistence of state territorial sovereignty (the Westphalian order) with emerging international protection obligations (the 1951 Refugee Convention and its progeny) and the practical reality that industrial economies require labor migration that pure exclusion would forbid.
% FOUNDING_PROBLEM_CORROBORATION: UNHCR and international human rights bodies attest the protection-obligation component remains live and increasingly violated in practice; labor economists and sending-state governments attest the labor-need component is live and structurally under-addressed by formal quota systems; receiving-state governments (the administering party) assert the balance is functioning as designed. Independent migration scholarship (outside all three interested parties) documents a widening gap between the balancing framework's formal legitimacy claims and enforcement practice — supporting a contested rather than settled status.
narrative_ontology:disappearance_verdict(border_control_legitimacy__jurisdictional_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__jurisdictional_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__jurisdictional_sovereignty, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__jurisdictional_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__jurisdictional_sovereignty, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__jurisdictional_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__jurisdictional_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) and suppression (0.62) are moderate-high but well below what a sovereignty_primary reading would authorize, because the proportionality/necessity constraint genuinely limits how far exclusion enforcement can go before triggering legitimacy crisis — the constraint is not free-floating sovereign discretion. Theater ratio (0.42) is substantial: much enforcement activity (visible border infrastructure, high-profile removals) functions as public-consent signaling distinct from the underlying labor and protection balancing the framework claims to perform. Accessibility collapse (0.5) is moderate — legal channels exist and are not fully closed, unlike a pure exclusion regime, but they are narrow enough that many with genuine claims cannot use them. Resistance (0.6) reflects active contestation from rights bodies, labor advocates, and diaspora communities against enforcement practices exceeding the reading's own proportionality standard.
 *
 * PERSPECTIVAL GAP:
 *   From the administrative apparatus's seat, the regime is a functioning legal balance, actively defended against both over-permissive and over-restrictive drift. From the excluded asylum seeker's or undocumented migrant's seat, the same proportionality apparatus operates as enforced exclusion whose 'balancing' rarely resolves in their favor. The engine computes these as structurally different seat-classifications from the same authored data — the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving-state citizens and domestic labor incumbents sit near the beneficiary end: they consent to and shape the regime and receive labor-market protection and perceived order. The administrative apparatus is the agenda-setter with institutional power and analytical exit — it operates the machinery rather than experiencing it as a constraint. Excluded asylum seekers, undocumented labor migrants, and displaced citizens sit near the full-target end: trapped or constrained exit, no vote in the consent process, and bearing the transfer directly. This dual victim structure — both non-citizen migrants AND citizen-diaspora claimants — is the reading's distinguishing structural feature relative to its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling sovereignty, protection obligations, and labor need under international law) remains partially live — labor shortages and protection crises persist — but enforcement practice has drifted toward serving public-consent-signaling and administrative convenience (rising theater_ratio) rather than the balancing function itself. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine, if imperfectly executed, coordination function (the proportionality-bounded balancing test) as pure extraction; classifying it as tangled_rope rather than rope prevents ignoring the concentrated, asymmetric costs borne by excluded parties who have no voice in the consent process that legitimates the regime against them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_capture,
    'Does the proportionality/necessity balancing test genuinely constrain enforcement discretion, or has it been captured into a legitimating ritual that rationalizes whatever level of exclusion domestic politics demands?',
    'Longitudinal analysis of judicial and administrative proportionality review outcomes: track the rate at which enforcement actions are actually struck down or modified versus rubber-stamped, across jurisdictions and over time.',
    'If the test rarely binds in practice, the jurisdictional_sovereignty reading collapses functionally toward sovereignty_primary despite its formal commitments, and the tangled_rope classification should shift toward snare. If it meaningfully binds, the tangled_rope classification with genuine coordination function is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_capture, empirical, 'Whether proportionality review is a real constraint or a legitimating formality.').

omega_variable(
    consent_process_representativeness,
    'Does ''public consent'' in this reading reflect the considered judgment of the resident population, or is it manufactured/amplified by political actors who benefit from restrictionist framing regardless of actual public preference distribution?',
    'Comparative survey research on stated migration preferences versus enacted policy stringency; analysis of whether policy tracks median voter preference or is systematically more restrictive than polling suggests.',
    'If consent is manufactured rather than genuinely aggregated, the ''public consent'' leg of the three-part balance is not doing real legitimating work, which would push the classification toward tangled_rope with a weaker coordination claim, closer to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_process_representativeness, empirical, 'Whether the public-consent component of legitimacy reflects real preference aggregation.').

omega_variable(
    labor_need_admission_gap,
    'Is the persistent gap between formal labor-migration quotas and actual labor absorption (via undocumented labor) evidence that the balancing framework structurally under-serves the labor-need component by design, or a policy failure correctable within the framework?',
    'Comparative analysis of quota-setting processes against independently measured labor demand; examine whether quota shortfalls are politically strategic (to preserve wage-protection benefits for incumbents) or genuinely miscalibrated.',
    'If the gap is structural and strategic, domestic_labor_incumbents'' beneficiary status is doing more classificatory work than the coordination story admits, strengthening the tangled_rope reading''s extraction component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_need_admission_gap, conceptual, 'Whether the labor-need shortfall is structural extraction or correctable miscalibration.').

omega_variable(
    kernel_reading_selection_criterion,
    'What structural or normative criterion determines which of the three kernel readings (sovereignty_primary, jurisdictional_sovereignty, freedom_of_movement_primary) a given state''s legal order actually instantiates, versus merely claims rhetorically?',
    'Comparative constitutional and international-law analysis: does the state''s domestic court system actually apply proportionality/necessity review to exclusion decisions (jurisdictional_sovereignty), treat exclusion as non-justiciable (sovereignty_primary), or apply a presumption against exclusion (freedom_of_movement_primary)?',
    'Misidentifying which reading a given state''s practice actually instantiates would misattribute this story''s ε and victim structure to a state that is actually operating under a sibling reading — the decomposition into three separate constraints depends on correctly locating each jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_criterion, conceptual, 'How to determine which kernel reading a given jurisdiction''s practice actually instantiates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__jurisdictional_sovereignty, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 0, 0.22).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 8, 0.28).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 16, 0.33).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 24, 0.37).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 32, 0.4).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__jurisdictional_sovereignty, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 8, 0.44).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 16, 0.49).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 32, 0.56).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__jurisdictional_sovereignty, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 24, 0.57).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__jurisdictional_sovereignty, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__jurisdictional_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__jurisdictional_sovereignty, freedom_of_movement_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_control_legitimacy kernel, each authored as a separate ε-invariant story per the ε-invariance principle. sovereignty_primary treats territorial exclusion as constitutive of statehood with no external balancing test (lower expected resistance, higher expected accessibility_collapse, narrower victim acknowledgment). freedom_of_movement_primary treats exclusion as presumptively illegitimate against a background right to move (different beneficiary/victim inversion, likely classifying much enforcement as snare rather than tangled_rope). This story (jurisdictional_sovereignty) occupies the structural middle, with genuine coordination function constrained by proportionality review and a distinctive dual-victim acknowledgment (migrants and displaced citizens both). All three should be read as a constraint family; do not average their ε values or treat them as observational perspectives on one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
