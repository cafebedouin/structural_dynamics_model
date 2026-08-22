% ============================================================================
% CONSTRAINT STORY: state_killing_authority__retributive_desert
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__retributive_desert, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: state_killing_authority__retributive_desert
 *   human_readable: Capital Punishment: Retributive Desert Reading (Lex Talionis)
 *   domain: criminal_justice/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the retributive_desert reading of the
 *   state_killing_authority kernel. The reading claims that murderers forfeit
 *   their right to life and that proportional punishment (lex talionis)
 *   requires death for death. The constraint operates by constructing the
 *   murdered person as a posthumous beneficiary (vindicated by execution),
 *   the condemned person as a rights-forfeited payer (extraction = life), and
 *   the state as an agenda-setter grounded in a proportionality norm rather
 *   than consequentialist outcome. The claimed type is snare: the
 *   coordination story (proportionality as limit on state power) is cover for
 *   an extraction that is irreversible, actively enforced, and suppresses
 *   alternatives (abolition, life without parole, restorative justice). The
 *   victim survivor who opposes execution is structurally excluded — their
 *   dissent would break the moral cover.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__retributive_desert, 0.78).
domain_priors:suppression_score(state_killing_authority__retributive_desert, 0.92).
domain_priors:theater_ratio(state_killing_authority__retributive_desert, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, extractiveness, 0.78).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(state_killing_authority__retributive_desert, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__retributive_desert, snare).
narrative_ontology:human_readable(state_killing_authority__retributive_desert, "Capital Punishment: Retributive Desert Reading (Lex Talionis)").
narrative_ontology:topic_domain(state_killing_authority__retributive_desert, "criminal_justice/constitutional_law").

domain_priors:requires_active_enforcement(state_killing_authority__retributive_desert).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__retributive_desert, 'a3287707-069b-4e83-a92d-b6c572c1e506').
narrative_ontology:cs_kernel_codification('a3287707-069b-4e83-a92d-b6c572c1e506', formalized).
narrative_ontology:cs_authority_grounding('a3287707-069b-4e83-a92d-b6c572c1e506', lineage).
narrative_ontology:cs_interpretation_layer_present('a3287707-069b-4e83-a92d-b6c572c1e506').
narrative_ontology:cs_reading_relation('a3287707-069b-4e83-a92d-b6c572c1e506', state_killing_authority__categorical_abolition, forecloses).
narrative_ontology:cs_reading_relation('a3287707-069b-4e83-a92d-b6c572c1e506', state_killing_authority__deterrence_instrument, coexists_with).
narrative_ontology:cs_axiom('a3287707-069b-4e83-a92d-b6c572c1e506', foundational, murderer_forfeits_right_to_life).
narrative_ontology:cs_axiom_status(murderer_forfeits_right_to_life, holdable).
narrative_ontology:cs_axiom_grounding('a3287707-069b-4e83-a92d-b6c572c1e506', murderer_forfeits_right_to_life, deontological).
narrative_ontology:cs_axiom('a3287707-069b-4e83-a92d-b6c572c1e506', foundational, lex_talionis_proportionality).
narrative_ontology:cs_axiom_status(lex_talionis_proportionality, holdable).
narrative_ontology:cs_axiom_grounding('a3287707-069b-4e83-a92d-b6c572c1e506', lex_talionis_proportionality, deontological).
narrative_ontology:cs_axiom('a3287707-069b-4e83-a92d-b6c572c1e506', secondary, state_authority_grounded_in_desert_not_outcome).
narrative_ontology:cs_axiom_status(state_authority_grounded_in_desert_not_outcome, holdable).
narrative_ontology:cs_axiom_grounding('a3287707-069b-4e83-a92d-b6c572c1e506', state_authority_grounded_in_desert_not_outcome, deontological).
narrative_ontology:cs_reference_frame('a3287707-069b-4e83-a92d-b6c572c1e506', classical_retributive_justice).
narrative_ontology:cs_drift_state('a3287707-069b-4e83-a92d-b6c572c1e506', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a3287707-069b-4e83-a92d-b6c572c1e506', '').
narrative_ontology:cs_kernel_id(state_killing_authority__retributive_desert, state_killing_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, murder_victim).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, state_retributive_authority).
narrative_ontology:constraint_beneficiary(state_killing_authority__retributive_desert, public_moral_order_adherents).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, condemned_person).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(state_killing_authority__retributive_desert, public_moral_order_adherents).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, lex_talionis_proportionality).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, forfeiture_doctrine).
narrative_ontology:constraint_vindicates(state_killing_authority__retributive_desert, moral_desert_retribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The murdered person is posthumously constructed as a beneficiary through the claim that execution vindicates their moral standing. They have no agency, no voice, and no exit — the constraint operates in their name after death. The vindication is symbolic and contestable; survivors or proxies may reject the claim that execution honors the victim.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, murder_victim, beneficiary,
    powerless, immediate, trapped, local).

% A person sentenced to death under this reading forfeits their right to life as the price of their crime. They bear the full extraction of the constraint — loss of life — with no exit. The forfeiture doctrine treats their rights as extinguished by their act, but the application is irreversible and allows no correction for error.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, condemned_person, payer,
    powerless, immediate, trapped, local).

% The state apparatus (legislature, courts, corrections) that authorizes, adjudicates, and carries out executions. It claims legitimacy from the proportionality norm and the forfeiture doctrine. It controls the process, sets the rules, and can modify or abolish the practice — but does so while claiming the practice is morally compelled, not discretionary.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, state_retributive_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Citizens who experience the moral order as sustained by proportional punishment. They benefit from the sense that justice is done, that the social contract is enforced, that 'an eye for an eye' maintains cosmic balance. They also pay — through tax cost, moral complicity in state killing, and the risk of error. Their identity is fused with the retributive frame; exit means abandoning a core moral commitment.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, public_moral_order_adherents, beneficiary,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(state_killing_authority__retributive_desert, public_moral_order_adherents, payer).

% Family members of murder victims who oppose the death penalty — often because it prolongs trauma, does not bring closure, or violates their moral/religious convictions. They are structurally excluded from the beneficiary set the reading constructs; the reading claims to speak for 'the victim' while actual survivors may reject the claim. Their exclusion is the constraint's way of securing its moral cover.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, victim_survivors_opposing_execution, excluded,
    moderate, biographical, constrained, local).

% Analysts who trace the doctrinal history, empirical record, and comparative practice of capital punishment. They see the full structure: the forfeiture claim, the proportionality norm, the error rate, the racial disparity, the international trend toward abolition. They neither collect nor pay; they map the constraint's operation across seats.
narrative_ontology:constraint_stakeholder(state_killing_authority__retributive_desert, legal_scholars_constitutional_analysts, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate social order by anchoring punishment in a proportionality principle that limits state power (only death for death) and gives moral meaning to the sanction. The coordination story: without desert-based proportionality, punishment becomes either arbitrary tyranny or pure utility calculation.
% TRANSFER_FUNCTION: Transfers the condemned person's life to the state's account of justice satisfied. The victim (posthumously) receives vindication; the public moral order receives confirmation; the state receives authority legitimation. The condemned pays with their life. The transfer is framed as restoration of balance, not extraction.
% ABSENT_VOICES: The murdered person (dead, cannot consent to being used as justification). Victim survivors who oppose execution (excluded from the 'vindication' claim). The condemned person (no exit, no voice after sentencing). Future generations who inherit the practice and its errors (no representation in the present decision). Abolitionist jurisdictions that have rejected the practice (their experience is treated as irrelevant).
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, jurisdictions retaining capital punishment would lose their primary moral justification. The legal architecture (forfeiture doctrine, proportionality review, 'death is different' jurisprudence) would collapse or require new grounding. The practice might persist on deterrence grounds, but the retributive license — the claim that the state *must* kill to do justice — would be gone. The moral order adherents would face identity crisis. The condemned would gain a reprieve (life without parole). The world rearranges.
% FOUNDING_PROBLEM: How to justify state killing without sliding into arbitrary power or pure utility? The retributive desert reading was built to answer: the state kills only those who have forfeited their right to life by taking another's, and only in strict proportion — death for death. This was the constraint on sovereign power: you may kill, but only the killer, and only once.
% FOUNDING_PROBLEM_CORROBORATION: The reading's proponents (prosecutors, victims' rights organizations, originalist jurists) attest the problem is live: evil exists, proportionality is the only barrier to tyranny, abolition abandons victims. Abolitionist jurists, international human rights bodies, and the majority of democratic nations (which have abolished) attest the founding problem is dead or transformed: the constraint on sovereign power is now *prohibition* of state killing, not its proportional calibration; the forfeiture doctrine is a fiction that masks state power; error and disparity prove the practice cannot be limited as claimed. No neutral arbiter corroborates either side — the dispute is the structure.
narrative_ontology:disappearance_verdict(state_killing_authority__retributive_desert, world_rearranges).
narrative_ontology:founding_problem_status(state_killing_authority__retributive_desert, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_killing_authority__retributive_desert, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(state_killing_authority__retributive_desert, 'none', 1).
narrative_ontology:epsilon_provenance(state_killing_authority__retributive_desert, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__retributive_desert_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_killing_authority__retributive_desert, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_killing_authority__retributive_desert_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.78: the constraint takes a life irreversibly; the 'proportionality' framing does not reduce the magnitude of extraction, only its moral presentation. Suppression 0.92: the constraint's persistence depends on excluding rival payment routing — here, excluding abolition, excluding victim survivors who oppose execution, excluding international norm evolution, excluding error correction. Theater 0.35: the procedural machinery (appeals, proportionality review, 'death is different' jurisprudence) is real but increasingly performative — it manages the constraint's legitimacy more than it limits its operation. Accessibility collapse 0.72: once the forfeiture/proportionality frame is accepted, alternatives appear as moral abandonment of victims. Resistance 0.68: sustained abolitionist movement, judicial dissent, international pressure, declining use — but the constraint holds.
 *
 * PERSPECTIVAL GAP:
 *   From the state_retributive_authority seat: the constraint is a rope — it coordinates punishment within moral limits, prevents arbitrary power, gives meaning to justice. From the condemned_person seat: it is a snare — pure extraction, no exit, enforced by the full machinery of the state. From the public_moral_order_adherents seat: it is a tangled_rope — genuine coordination of moral meaning mixed with extraction they bear (tax, complicity, error risk) but cannot disentangle because their identity is fused to the frame. The engine computes these divergences from the structural data; the claimed_type (snare) reflects the authoring seat's structural judgment.
 *
 * DIRECTIONALITY LOGIC:
 *   Murder victim (posthumous beneficiary, d near 0.0 — constraint claims to subsidize their vindication). Condemned person (full target, d = 1.0 — constraint extracts life with no exit). State retributive authority (beneficiary/administrator, d near 0.15 — collects authority legitimation, controls rules, but bears institutional cost and error risk). Public moral order adherents (identity-locked, d ~ 0.4 — benefit from moral coherence, pay complicity cost, cannot exit without identity fracture). Victim survivors opposing execution (excluded, d not computed — they are outside the coordination). Analytical observers (d = 0.5 — symmetric analytical seat).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — constraining sovereign killing power through proportionality — is contested. The reading claims the problem is live (evil exists, only death answers death). The abolitionist corroboration says the problem is dead (the constraint on sovereign power is now prohibition, not calibration). The constraint persists not because the founding problem is solved, but because the identity-locked beneficiary set (moral order adherents) and the institutional agenda-setter (state authority) are mutually reinforcing: the state provides the ritual; the adherents provide the democratic license. This is mandatrophy — the mandate (proportional limit on state killing) has been inverted into a license for state killing, and the inversion is maintained by the identity fusion of the beneficiary set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    forfeiture_doctrine_coherence,
    'Is the forfeiture doctrine (murderer forfeits right to life) a coherent normative principle or a performative fiction that masks state power?',
    'Comparative analysis: jurisdictions that retain capital punishment vs. those that abolished — do the retentionist jurisdictions actually limit killing to ''death for death'' (no felony murder, no terrorism expansion, no political crimes)? If the doctrine expands beyond strict lex talionis, it is a fiction.',
    'If fiction, the constraint''s claimed coordination function (proportionality as limit) collapses; the constraint is pure extraction (snare) with no coordination remainder. If coherent, a residual coordination function exists (tangled_rope possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(forfeiture_doctrine_coherence, conceptual, 'Whether the forfeiture/proportionality principle genuinely limits the practice or serves as expanding cover.').

omega_variable(
    victim_vindication_authenticity,
    'Does execution actually vindicate the murdered person, or is ''vindication'' a claim made by third parties using the dead as moral props?',
    'Empirical study of victim survivors'' stated preferences over time — do those who initially support execution report vindication/closure? Do those who oppose report that the claim harms them? Longitudinal data on ''closure'' narratives.',
    'If vindication is largely a third-party claim, the murder_victim beneficiary is a constructed fiction — the constraint has no genuine beneficiary, only payers (condemned) and extractors (state). This would strengthen the snare classification and the false-summit pattern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_vindication_authenticity, empirical, 'Whether the posthumous beneficiary claim reflects the victim''s interest or the living''s projection.').

omega_variable(
    error_irreversibility_as_extraction,
    'Does the irreversibility of execution (no correction for error) function as a feature of the extraction — making the constraint''s extraction absolute and unaccountable?',
    'Track exoneration rates in capital cases vs. non-capital; analyze whether the ''death is different'' procedural superstructure actually reduces error or manages its visibility. Compare jurisdictions with/without death penalty on wrongful conviction discovery.',
    'If error is systematically concealed or uncorrectable, the constraint''s extraction is not just high — it is structurally unaccountable. This would push effective extraction toward 1.0 for the condemned seat regardless of procedural theater.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(error_irreversibility_as_extraction, empirical, 'Whether irreversibility makes the extraction structurally unaccountable.').

omega_variable(
    retributive_vs_deterrence_boundary,
    'In practice, does the retributive_desert reading functionally converge with the deterrence_instrument reading (state kills for utility, retribution is the cover), or do they diverge in observable policy (e.g., scope of death-eligible crimes, evidentiary standards)?',
    'Legislative history analysis: when death-eligible crimes expand beyond murder (terrorism, drug kingpin, treason), which reading''s logic is invoked? Judicial opinions: do proportionality reviews actually constrain, or do they ratify?',
    'If convergent, the retributive reading is a veneer on deterrence_instrument — the kernel has two readings that collapse into one practice. If divergent, the retributive constraint genuinely limits scope (death only for murder, strict proportionality) — a real coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retributive_vs_deterrence_boundary, conceptual, 'Whether retributive and deterrence readings are structurally distinct in operation or practically fused.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__retributive_desert, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t1787, state_killing_authority__retributive_desert, theater_ratio, 1787, 0.25).
narrative_ontology:measurement(stat_tr_t1868, state_killing_authority__retributive_desert, theater_ratio, 1868, 0.3).
narrative_ontology:measurement(stat_tr_t1972, state_killing_authority__retributive_desert, theater_ratio, 1972, 0.35).
narrative_ontology:measurement(stat_tr_t1976, state_killing_authority__retributive_desert, theater_ratio, 1976, 0.4).
narrative_ontology:measurement(stat_tr_t2000, state_killing_authority__retributive_desert, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(stat_tr_t2024, state_killing_authority__retributive_desert, theater_ratio, 2024, 0.35).

% Extraction over time
narrative_ontology:measurement(stat_be_t1787, state_killing_authority__retributive_desert, base_extractiveness, 1787, 0.65).
narrative_ontology:measurement(stat_be_t1868, state_killing_authority__retributive_desert, base_extractiveness, 1868, 0.72).
narrative_ontology:measurement(stat_be_t1972, state_killing_authority__retributive_desert, base_extractiveness, 1972, 0.75).
narrative_ontology:measurement(stat_be_t1976, state_killing_authority__retributive_desert, base_extractiveness, 1976, 0.78).
narrative_ontology:measurement(stat_be_t2000, state_killing_authority__retributive_desert, base_extractiveness, 2000, 0.77).
narrative_ontology:measurement(stat_be_t2024, state_killing_authority__retributive_desert, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t1787, state_killing_authority__retributive_desert, suppression_requirement, 1787, 0.85).
narrative_ontology:measurement(stat_su_t1868, state_killing_authority__retributive_desert, suppression_requirement, 1868, 0.88).
narrative_ontology:measurement(stat_su_t1972, state_killing_authority__retributive_desert, suppression_requirement, 1972, 0.9).
narrative_ontology:measurement(stat_su_t1976, state_killing_authority__retributive_desert, suppression_requirement, 1976, 0.92).
narrative_ontology:measurement(stat_su_t2000, state_killing_authority__retributive_desert, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(stat_su_t2024, state_killing_authority__retributive_desert, suppression_requirement, 2024, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__retributive_desert, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(state_killing_authority__retributive_desert, 0.12).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__deterrence_instrument).
narrative_ontology:affects_constraint(state_killing_authority__retributive_desert, state_killing_authority__categorical_abolition).

% DUAL FORMULATION NOTE:
% This story is one of three in the state_killing_authority constraint family. All three share the kernel (state authority to kill) but instantiate different constraints with different ε, different beneficiary/victim structures, and different types. retributive_desert: ε=0.78, snare, victim as posthumous beneficiary via vindication. deterrence_instrument: ε varies with empirical claim, tangled_rope, condemned as utility-bearing unit. categorical_abolition: ε≈0, mountain, life as inalienable right (no forfeiture). The family demonstrates ε-invariance: same kernel label, structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_killing_authority__retributive_desert, powerless, 1.0).
constraint_indexing:directionality_override(state_killing_authority__retributive_desert, institutional, 0.15).
constraint_indexing:directionality_override(state_killing_authority__retributive_desert, organized, 0.4).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
