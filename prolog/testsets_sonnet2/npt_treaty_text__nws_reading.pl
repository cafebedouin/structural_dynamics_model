% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Read as Binding Non-Proliferation / Aspirational Disarmament (NWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This story authors the nuclear-weapon-states' reading of the Nuclear
 *   Non-Proliferation Treaty kernel: Article II's prohibition on NNWS
 *   acquiring weapons is treated as an immediate, binding, verifiable
 *   obligation, while Article VI's call to 'pursue negotiations in good faith
 *   on effective measures relating to cessation of the nuclear arms race at
 *   an early date' is treated as aspirational and non-justiciable — a
 *   statement of direction rather than a timetable. Under this reading, the
 *   ambiguity in 'at an early date' is a genuine interpretive resource that
 *   the recognized weapon states control through their procedural dominance
 *   of Review Conferences and their monopoly on the bilateral arms-control
 *   processes that stand in for Article VI compliance. This is a DIFFERENT
 *   constraint from the nnws_reading (which treats Article VI as a binding,
 *   time-bound obligation and non-proliferation as conditional restraint
 *   purchasing NWS compliance) and from the withdrawal_threshold_reading
 *   (which concerns Article X exit thresholds, not the
 *   proliferation/disarmament balance). Each reading carries its own epsilon;
 *   this story's high extractiveness (0.72) reflects the asymmetric burden
 *   this specific reading imposes on NNWS and near-threshold states, not an
 *   average across readings.
 *
 * KEY AGENTS:
 *   - recognized_nuclear_weapon_states: agenda-setter and beneficiary, institutional power, arbitrage exit — controls treaty interpretation machinery and bears no enforceable disarmament obligation under this reading
 *   - non_nuclear_weapon_states: primary payer, moderate power, constrained exit — accepted permanent restraint and safeguards burden for a disarmament promise unenforceable under this reading
 *   - iaea_safeguards_department: agenda-setter/payer, organized power — administers a verification architecture asymmetrically weighted toward horizontal proliferation
 *   - disarmament_advocacy_coalitions: excluded voice — presses the binding-obligation reading (ICJ 1996) with no procedural standing to compel it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.72).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.61).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Read as Binding Non-Proliferation / Aspirational Disarmament (NWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, 'adf0da42-3b7e-4df8-b709-3fdd6a5603ba').
narrative_ontology:cs_kernel_codification('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', fixed_text).
narrative_ontology:cs_authority_grounding('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', extraction).
narrative_ontology:cs_interpretation_layer_present('adf0da42-3b7e-4df8-b709-3fdd6a5603ba').
narrative_ontology:cs_reading_relation('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', foundational, article_vi_as_hortatory_direction).
narrative_ontology:cs_axiom_status(article_vi_as_hortatory_direction, holdable).
narrative_ontology:cs_axiom_grounding('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', article_vi_as_hortatory_direction, conventional).
narrative_ontology:cs_axiom('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', secondary, proliferation_asymmetry_as_stabilizing).
narrative_ontology:cs_axiom_status(proliferation_asymmetry_as_stabilizing, holdable).
narrative_ontology:cs_axiom_grounding('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', proliferation_asymmetry_as_stabilizing, instrumental).
narrative_ontology:cs_reference_frame('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', cold_war_bargain_equilibrium).
narrative_ontology:cs_drift_state('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', post_2015_review_conference_breakdown, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('adf0da42-3b7e-4df8-b709-3fdd6a5603ba', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, recognized_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nws_defense_industrial_base).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, near_threshold_states_denied_technology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, iaea_safeguards_department).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, nuclear_apartheid_as_stable_equilibrium).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, verification_asymmetry_as_technically_neutral).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five treaty-recognized nuclear weapon states control the Review Conference process, chair key subsidiary bodies, and hold veto power at the UN Security Council that shapes enforcement of Article II obligations on others while treating Article VI as a non-binding statement of direction. They interpret 'pursue negotiations in good faith' as satisfied by episodic bilateral arms reduction talks rather than a timetable, and they retain, modernize, and in some cases expand their arsenals across the treaty's life without triggering any compliance mechanism.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, recognized_nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, recognized_nuclear_weapon_states, beneficiary).

% Accepted permanent non-nuclear status and intrusive IAEA safeguards in exchange for a disarmament commitment they cannot enforce and civilian nuclear cooperation that is itself gated by supplier-state discretion. Their exit options are narrow: withdrawing under Article X invites sanctions and pariah status, while remaining inside the treaty means permanent verification burden with no reciprocal obligation they can compel.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).

% States with civilian nuclear ambitions or latent weapons-relevant capability face export control regimes (Nuclear Suppliers Group) built and administered largely by the same states that hold weapons, blocking technology transfer justified by proliferation risk while the underlying weapons stockpiles of the controlling states go unaddressed by any comparable regime.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, near_threshold_states_denied_technology, payer,
    powerless, biographical, trapped, national).

% Administers inspection and verification of non-nuclear weapon states' civilian facilities on a budget structurally weighted toward horizontal proliferation detection; has no comparable mandate or resourcing to verify NWS arsenal reductions or Article VI progress, which are treated as matters for bilateral or plurilateral negotiation outside the safeguards system.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_safeguards_department, agenda_setter,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea_safeguards_department, payer).

% Modernization programs, delivery system upgrades, and warhead life-extension contracts continue uninterrupted because the treaty imposes no enforceable ceiling or drawdown schedule on the recognized weapon states; the ambiguity of 'at an early date' in Article VI has never crystallized into a binding date, funding no different than if the article did not exist.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nws_defense_industrial_base, beneficiary,
    organized, generational, arbitrage, national).

% Civil society coalitions and the states behind the Treaty on the Prohibition of Nuclear Weapons argue Article VI creates a genuine legal obligation with a reasonable-time standard, citing the ICJ's 1996 advisory opinion; they have no seat in NPT Review Conference decision-making, which proceeds by consensus among states parties dominated procedurally by NWS positions.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_advocacy_coalitions, excluded,
    moderate, generational, constrained, global).

% Records annual resolutions on disarmament progress and hears competing state interpretations of treaty obligations, but its resolutions are non-binding and cannot compel either the NWS reading or the NNWS reading to yield.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, un_general_assembly_first_committee, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, recognized_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates against horizontal proliferation: a shared verification architecture (IAEA safeguards) and a mutual restraint bargain reduce the number of independent nuclear-armed actors, which is a real collective security good for all parties including the recognized weapon states.
% TRANSFER_FUNCTION: Moves compliance costs, technology access restrictions, and verification burden onto non-nuclear-weapon states and near-threshold states, while the corresponding disarmament transfer from nuclear weapon states back to the collective good remains unenforced and largely unrealized across five decades.
% ABSENT_VOICES: States and coalitions advancing the Treaty on the Prohibition of Nuclear Weapons, and non-nuclear-weapon states seeking a binding timetable reading of Article VI, raise their objections at Review Conferences and in UN fora but hold no mechanism to compel a different interpretation; the consensus procedure of the Review Conference structurally favors the status quo reading held by the states whose compliance would need to change.
% DISAPPEARANCE_RATIONALE: If this specific reading of the treaty text vanished — if 'at an early date' were instead read as a binding, justiciable timetable — nuclear weapon states would face a genuinely novel compliance exposure they do not currently carry; NNWS advocates say the world would rearrange substantially toward accountability, while NWS diplomats maintain the practical arms-control landscape (bilateral reduction treaties, deterrence postures) would proceed largely unchanged because enforcement capacity does not exist regardless of the reading adopted.
% FOUNDING_PROBLEM: In 1968, the treaty was built to prevent the spread of nuclear weapons beyond the states that already possessed them, using a bargain: non-nuclear states forgo weapons and accept safeguards, nuclear states pursue eventual disarmament and share peaceful nuclear technology.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states' own diplomatic statements at Review Conferences attest the non-proliferation function remains live and central. Independent corroboration from outside the beneficiary set — the International Court of Justice's 1996 advisory opinion, UN Secretary-General reports, and multiple Non-Aligned Movement and TPNW-sponsor state submissions — attests that the disarmament half of the founding bargain is treated by NWS as substantially inert rather than a genealogically equal obligation, and that this asymmetry is the specific object of dispute rather than a settled matter.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is authored high because under this reading the treaty's coordination function (preventing horizontal proliferation) is real but is bundled with an asymmetric extraction: NNWS bear the full compliance and verification cost while NWS bear none of the corresponding disarmament cost, and this reading treats that asymmetry as the treaty's correct, intended meaning rather than a drift or failure. Suppression (0.61) reflects the active enforcement apparatus — export control regimes, safeguards inspections, sanctions threats for Article X withdrawal — that keeps NNWS inside the bargain, alongside the near-absence of any comparable mechanism constraining NWS arsenals. Theater ratio (0.48) is moderate-to-high because Review Conferences and NPT diplomatic language increasingly perform disarmament commitment (final documents reaffirming 'unequivocal undertakings') without operational follow-through, a pattern that has intensified since the 1995 indefinite extension and 2000 Review Conference commitments went substantially unmet. Accessibility collapse (0.58) and resistance (0.55) reflect that alternatives exist and are actively pressed (the TPNW, ICJ advisory opinion, NAM coalition positions) but have not displaced this reading's procedural dominance.
 *
 * DIRECTIONALITY LOGIC:
 *   Recognized nuclear weapon states sit at the beneficiary end: they set the interpretive terms, control Review Conference procedure, and face no compliance mechanism for Article VI under this reading — directionality derives naturally from their agenda_setter/beneficiary dual role and arbitrage exit. Non-nuclear-weapon states and near-threshold states sit toward the target end: constrained and trapped exit options respectively, bearing the safeguards and export-control burden with no reciprocal lever. The IAEA safeguards department is structurally interesting — it is itself constrained (budget and mandate shaped by member states, disproportionately NWS-influenced) even though it administers enforcement against NNWS; it is both an enforcement agent and a resource-constrained payer of institutional cost, hence the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear proliferation while building toward disarmament) remains genealogically live in the narrow proliferation-prevention sense — new state and non-state proliferation risks are real and current. But under this reading the disarmament half of the founding bargain has been read out of binding force entirely rather than merely delayed, which risks converting the coordination function into permanent extraction: the treaty continues to solve a real problem (horizontal proliferation) while the reciprocal obligation that made the bargain legitimate at signing goes unaddressed indefinitely. Classifying this as tangled_rope rather than snare preserves the genuine coordination value of non-proliferation verification while still registering the asymmetric extraction this specific reading licenses — collapsing it to snare would erase the real security good; collapsing it to rope would erase the documented asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status_ambiguity,
    'Is Article VI''s ''pursue negotiations in good faith... at an early date'' language a binding legal obligation with a reasonable-time standard (per the ICJ''s 1996 advisory opinion), or a hortatory statement of aspiration with no enforceable content?',
    'An authoritative adjudication — an ICJ contentious case (not merely advisory), a binding arbitral ruling, or unanimous state-party reinterpretation at a Review Conference — would resolve the ambiguity. None has occurred in the treaty''s history; the 1996 advisory opinion is persuasive but non-binding.',
    'If Article VI is authoritatively read as binding, this reading (nws_reading) would be structurally displaced by the nnws_reading, and the classification would likely shift toward snare as the extraction becomes harder to justify as any part of a legitimate bargain. If confirmed as aspirational, this reading''s tangled_rope classification is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_status_ambiguity, conceptual, 'Central kernel ambiguity: whether Article VI creates enforceable disarmament obligations.').

omega_variable(
    verification_asymmetry_neutrality,
    'Is the IAEA safeguards system''s near-total focus on horizontal (NNWS) proliferation verification, versus negligible institutional capacity to verify NWS arsenal reductions, a technically neutral consequence of what is verifiable, or a structural choice reflecting NWS control of the institution''s mandate and budget?',
    'Comparative institutional analysis of proposed but unadopted NWS verification mechanisms (e.g., historical proposals for arsenal declaration and dismantlement verification) and the political history of why they were not funded or mandated.',
    'If the asymmetry is shown to be a deliberate institutional choice rather than a technical necessity, it strengthens the case that this reading''s coordination story is partly cover for extraction, supporting the tangled_rope (or even snare-leaning) classification. If shown to be primarily a verifiability constraint (NWS arsenals are genuinely harder to verify without compromising deterrence-relevant secrecy), it supports treating some of the asymmetry as inherent to the coordination problem rather than extractive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(verification_asymmetry_neutrality, empirical, 'Whether verification-budget asymmetry is a technical necessity or an institutional capture artifact.').

omega_variable(
    reading_selection_evidentiary_basis,
    'What specific structural signals justify treating the NWS reading, rather than the NNWS reading, as this story''s primary framing — given that both readings are held simultaneously by different treaty parties with no adjudicating body?',
    'Documented practice: this story is authored from the observable institutional practice (Review Conference procedure, actual enforcement patterns, actual funding allocation) rather than from either party''s stated legal position, since practice reveals which reading currently governs outcomes regardless of which reading is textually more defensible.',
    'If future institutional practice shifts toward binding-timetable enforcement of Article VI (e.g., through TPNW normative pressure or a changed Review Conference consensus), the nws_reading''s descriptive accuracy would degrade and the nnws_reading would become the better account of operative practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_evidentiary_basis, conceptual, 'Why this story selects the NWS reading as the operative framing despite kernel ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nws_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nws_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nws_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_text__nws_reading, theater_ratio, 2005, 0.4).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__nws_reading, theater_ratio, 2015, 0.44).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nws_reading, theater_ratio, 2025, 0.48).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nws_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nws_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nws_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_text__nws_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__nws_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nws_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nws_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nws_reading, suppression_requirement, 1980, 0.46).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__nws_reading, suppression_requirement, 1995, 0.52).
narrative_ontology:measurement(npt__su_t2005, npt_treaty_text__nws_reading, suppression_requirement, 2005, 0.56).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_text__nws_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nws_reading, suppression_requirement, 2025, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nws_reading, 0.1).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iaea_safeguards_verification_regime).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, nuclear_suppliers_group_export_controls).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the npt_treaty_text kernel, each authored as a separate constraint per the ε-invariance principle. nws_reading (this file) authors non-proliferation as binding and disarmament as aspirational, producing high extractiveness concentrated on NNWS and near-threshold states. nnws_reading authors the inverse framing — disarmament as binding under Article VI, non-proliferation as conditional restraint — and would carry a different beneficiary/victim structure and likely a different (lower, NWS-directed) extraction profile. withdrawal_threshold_reading addresses an orthogonal dimension of the same treaty (Article X exit threshold) and is linked for kernel completeness rather than because it shares this story's proliferation/disarmament axis. All three should be read as siblings, not as three measurements of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
