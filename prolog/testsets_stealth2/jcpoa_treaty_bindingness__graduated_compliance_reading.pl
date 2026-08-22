% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA as Graduated Reciprocal Commitment (Performance-Calibrated Reading)
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   Adopted in 2015, the arrangement examined here is a scaled reciprocal
 *   commitment: Iran's obligations (centrifuge limits, stockpile ceilings,
 *   continuous inspection) are indexed to verified performance, and
 *   counterparties' responses to non-performance are graduated -
 *   clarification, partial relief suspension, renewed Security Council
 *   measures - through a Joint Commission that prioritizes de-escalation over
 *   formal breach findings. This file instantiates one reading of the
 *   contested kernel jcpoa_treaty_bindingness (the
 *   graduated_compliance_reading); the binding-multilateral and
 *   transactional-provisional readings are separate constraint stories with
 *   their own epsilon values, linked only through the network layer. The
 *   interval traces the reading's arc: functioning reciprocity through 2017,
 *   unilateral suspension of the relief side in 2018, incremental
 *   counter-breaches from 2019, a stalled restoration process through
 *   2021-2022, and snapback reactivation in 2025 - with the graduated
 *   machinery persisting throughout, increasingly producing process without
 *   its benefit side. Epsilon is authored for the standing arrangement as
 *   this reading assesses it: moderate-to-substantial, driven by the
 *   asymmetry between verifiable, hard obligations on one side and
 *   politically reversible relief on the other. KEY AGENTS (by structural
 *   relationship): - joint_commission_member_states: Agenda-setter
 *   (institutional/mobile) - administers the graduated ladder, holds the
 *   collective snapback trigger - us_executive_sanctions_authority:
 *   Agenda-setter (institutional/arbitrage) - controls the relief valve
 *   through waiver and secondary-restriction decisions -
 *   iranian_state_negotiators: Primary target with secondary benefit
 *   (institutional/constrained) - delivers performance, receives politically
 *   contingent relief - iranian_civilian_economy: Primary target
 *   (moderate/trapped) - absorbs inflation and shortage at every calibration
 *   step - iranian_oil_export_sector: Primary target (organized/trapped) -
 *   the commodity stream whose permitted volumes index relief -
 *   pragmatic_diplomacy_establishments: Beneficiary
 *   (organized/identity_locked) - professional standing invested in the
 *   framework's survival - european_export_industries: Beneficiary with
 *   secondary exposure (powerful/mobile) - engagement contracts, snapback
 *   write-downs - iaea_inspection_directorate: Beneficiary
 *   (institutional/constrained) - mandate, funding, continuous access -
 *   israeli_gulf_security_establishments: Excluded (powerful/arbitrage) -
 *   bears regional externalities, operates outside the commission -
 *   us_congressional_opposition: Excluded (institutional/arbitrage) - held
 *   the domestic levers determining relief flow -
 *   academic_nonproliferation_analysts: Analytical observer
 *   (analytical/analytical) - tracks breakout timelines and calibration
 *   precedent
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.71).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.64).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA as Graduated Reciprocal Commitment (Performance-Calibrated Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '4a8c84ef-655f-4b4b-a46a-7cfc3797c486').
narrative_ontology:cs_kernel_codification('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', formalized).
narrative_ontology:cs_authority_grounding('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', practice).
narrative_ontology:cs_interpretation_layer_present('4a8c84ef-655f-4b4b-a46a-7cfc3797c486').
narrative_ontology:cs_reading_relation('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_axiom('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', foundational, proportionality_governs_enforcement).
narrative_ontology:cs_axiom_status(proportionality_governs_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', proportionality_governs_enforcement, conventional).
narrative_ontology:cs_axiom('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', foundational, bindingness_tracks_reciprocal_performance).
narrative_ontology:cs_axiom_status(bindingness_tracks_reciprocal_performance, holdable).
narrative_ontology:cs_axiom_grounding('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', bindingness_tracks_reciprocal_performance, instrumental).
narrative_ontology:cs_reference_frame('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', performance_calibrated_reciprocity_framework).
narrative_ontology:cs_drift_state('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', post_unilateral_withdrawal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4a8c84ef-655f-4b4b-a46a-7cfc3797c486', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_establishments).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, european_export_industries).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_inspection_directorate).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, joint_commission_member_states).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_oil_export_sector).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state_negotiators).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state_negotiators).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, european_export_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convene the Joint Commission, receive the inspectorate's periodic reports, and decide which responses follow which findings - clarifying sessions, partial suspension of relief measures, or recommendation of renewed Security Council measures. Collectively hold the trigger that restores pre-2016 United Nations restrictions. Individually exposed to domestic pressure to defect from the common line: some members weigh commercial ties to Tehran, others weigh transatlantic relations.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, joint_commission_member_states, agenda_setter,
    institutional, generational, mobile, global).

% Administers the largest share of the relief side through waiver decisions, licensing, and enforcement of secondary restrictions against foreign banks and firms handling Iranian oil. Its participation is the single biggest component of what the other side receives, and it demonstrated in 2018 that it can suspend that participation unilaterally while keeping the financial chokepoints that make restrictions bite worldwide.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_executive_sanctions_authority, agenda_setter,
    institutional, biographical, arbitrage, global).

% Delivers the performance side: centrifuge counts, stockpile ceilings, facility conversions, and inspection access, all continuously verified. Receives asset releases, oil-export allowances, and banking reconnection whose delivery depends on counterparties' domestic politics rather than any mirror-image verification. Cannot walk away without forfeiting receipts and inviting isolation, but retains incremental breach as the one lever that reliably moves counterparties.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state_negotiators, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state_negotiators, beneficiary).

% Households, importers, and manufacturers price the arrangement daily: currency value, medicine imports, spare parts, and food channels all track how much relief is actually flowing. Even before 2018, large banks' fear of foreign penalties kept much of the promised reconnection from materializing. When relief withdraws, this seat absorbs the inflation and shortages; it holds no seat in the commission that calibrates the steps.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_economy, payer,
    moderate, immediate, trapped, national).

% Sells the commodity whose permitted volumes index the whole exchange. Tanker routing, insurance, and buyer availability expand and contract with waiver decisions made in Washington and enforcement choices made in third-country ports. Individual buyers and insurers can be replaced; the sector cannot relocate its reservoirs or its customer geography.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_oil_export_sector, payer,
    organized, immediate, trapped, global).

% Foreign ministries, negotiating teams, and surrounding policy communities whose professional standing is bound up with the framework's survival. Validation of the negotiation-first approach flows to them while it holds; collapse would repudiate career-defining commitments. Successor staff inherit the file with the expectation that the ladder of calibrated responses remains the reference point for any future arrangement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_establishments, beneficiary,
    organized, biographical, identity_locked, continental).

% Aerospace, automotive, energy, and shipping firms that positioned for re-engagement: aircraft sales, port investments, energy field development. Collected early orders after Implementation Day, then wrote down projects when secondary restrictions returned in 2018 - a major energy company abandoned a multibillion-dollar gas project and aircraft deliveries shrank. Capital can move to other markets; signed contracts and in-country assets could not.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, european_export_industries, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, european_export_industries, payer).

% Receives the mandate, funding, and access the arrangement created: continuous surveillance at declared sites, environmental sampling, centrifuge monitoring, and regular access to enrichment facilities. Its reports are the evidentiary input to every calibration decision the commission makes. Stepping back from the file would blind the entire system it anchors.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_inspection_directorate, beneficiary,
    institutional, generational, constrained, global).

% Bear the regional externalities of the bargain - a legitimated enrichment program, freed resources for regional clients, and inspection ceilings they regard as too permissive - without having held a seat in the negotiation or a vote in the commission. Responded through channels outside the arrangement: legislative lobbying, intelligence disclosures, sabotage operations, and eventually direct strikes on program facilities.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, israeli_gulf_security_establishments, excluded,
    powerful, generational, arbitrage, regional).

% Held the domestic levers that determined whether relief actually flowed: waiver-review hearings, new sanctions legislation, certification requirements. Never accepted the arrangement's premise, operated entirely outside its architecture, and supplied the political base for the 2018 withdrawal.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_congressional_opposition, excluded,
    institutional, biographical, arbitrage, national).

% Track breakout estimates, stockpile growth, inspection coverage, and the precedent value of each calibration decision. Publish the timelines and comparisons every other seat cites. Hold no lever; their product is the shared factual record.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, academic_nonproliferation_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, diffuse).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a multi-party verification-and-reciprocity problem: converts a dispersed enrichment program into a continuously inspected one, synchronizes sanctions relief across economies that would otherwise free-ride or defect, and routes responses to non-performance through a shared escalation ladder so that no single counterparty's retaliation sets the terms for all.
% TRANSFER_FUNCTION: Moves verified restraint - centrifuge counts, stockpile ceilings, inspection access - from Iran to the commission members and the inspectorate; moves calibrated relief - asset releases, oil-export allowances, banking reconnection - from the counterparty economies to Iran; moves decision authority over responses to the Joint Commission's collective process.
% ABSENT_VOICES: Israeli and Gulf security establishments bore regional externalities with no seat in the negotiation or the commission; Iranian parliamentary and hardline constituencies could object but held no commission vote; the U.S. Congress ratified nothing and operated outside the architecture while controlling relief-critical levers; populations in proxy-theater countries affected by regional resource shifts were unrepresented anywhere.
% DISAPPEARANCE_RATIONALE: Enrichment and stockpile caps lapse immediately, enhanced monitoring ends, and the legal architecture for restoring United Nations measures expires with nothing replacing it; relief-side expectations unwind, oil flows reprice, and regional powers accelerate hedging programs. Every seated party's arrangements depend on the framework's continued existence, even in its degraded state.
% FOUNDING_PROBLEM: Verifiably extend the time an undeclared dash to weapons-grade material would require to roughly a year, and give the dispute over the program a venue other than airstrikes or unchecked regional proliferation.
% FOUNDING_PROBLEM_CORROBORATION: The parties dispute status along predictable lines, so corroboration must come from outside the beneficiary set: the inspectorate's board-level reporting documents both the 2015-2018 verification achievements and the post-2019 accumulation beyond caps; independent institute-based breakout analyses and adversarial parties' intelligence disclosures attest that the original problem has partly returned in altered form. No beneficiary-only attestation exists.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.71, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71 at interval end) reflects the asymmetry this reading itself predicts when reciprocity breaks: obligations on the Iranian side are precise, continuously verified, and hard to suspend, while the relief side proved politically reversible - after 2018 the performance side persisted while the benefit side did not. Suppression (0.64) is authored as a raw structural property, unscaled by power or scope: secondary financial restrictions reach any bank touching Iranian oil regardless of commission findings, and the snapback trigger hangs over every calibration step; the engine scales only extractiveness, by directionality and scope. Theater (0.48) sits just under the substitution threshold: commission meetings, dispute-resolution referrals, and reporting cycles continued through years in which they could not restore the benefit side - real machinery, increasingly ceremonial output. Accessibility collapse is low (0.35) because alternatives stayed live and were exercised: unilateral withdrawal, incremental breach, kinetic operations. Resistance is high (0.68): congressional opposition, counter-breaches, regional adversaries. All three tracked series share one grid (t = 0, 2, 4, 6, 8, 10 mapping 2015-2025). The suppression series is the enforcement story: intensification through the maximum-pressure era (peak 0.67 at t=6), decay during the stalled-restoration years (0.60 at t=8), reactivation with the 2025 snapback notification (0.64). Claim and metrics are authored independently: the claimed type states what this reading takes the structure to be; the metrics describe how it actually operated.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical facts. The commission members and the sanctions authority sit at the coordination center: from there the ladder is calibrated statecraft and every step was chosen. The Iranian civilian seats sit at the receiving end of every downward calibration: from there the same steps arrive as weather, without a vote. The Iranian state occupies both sides at once - it delivers performance and collects the political value of relief - so its computed position splits from its own civilians'. European industry collected the upside early and the write-downs later, making its seat swing with the enforcement cycle. The diplomacy establishments' identity fusion with the framework means their seat reads persistence as success even as output turns ceremonial. The engine computes these divergences from the structural data; nothing in this file adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (diplomacy establishments, export industries, inspectorate, commission members) derive low directionality - the arrangement subsidizes their positions. Victim declarations (civilian economy, oil sector) derive high directionality - every calibration step lands on them. Dual seats resolve intermediate: the Iranian state is payer with beneficiary secondary role; European industry is beneficiary with payer secondary role. Trapped exits push the Iranian civilian seats toward the full-target end; the sanctions authority's arbitrage-grade exit pulls it toward the beneficiary end despite its agenda-setting role. The global scope of the financial chokepoints makes verification of relief delivery harder, which scales effective extraction modestly upward for the target seats. No directionality overrides were needed: the beneficiary/victim declarations plus exit atoms produce the correct ordering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - verifiable breakout delay and a non-kinetic venue for the dispute - is contested rather than dead: the underlying problem partly returned in altered form after 2019, so the mismatch consumer reads status=contested against verdict=world_rearranges and finds no dead-mandate flag. The analysis earns its keep on the adjacent error: with theater at 0.48 and the benefit side collapsed, the machinery invites a decayed-institution reading. What blocks that reading is the load-bearing residue - inspection data production continued, and the ladder remained the reference point every restoration attempt negotiated toward. Mandatrophy discipline keeps the classification from sliding to a purely vestigial verdict while the measurement layer watches theater approach the substitution threshold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel jcpoa_treaty_bindingness (graduated_compliance_reading). What would adopting a sibling reading - binding_multilateral_reading or transactional_provisional_reading - change structurally?',
    'Comparative classification across the three linked story files: hold the referent (the standing arrangement) fixed and observe how epsilon, victim sets, and enforcement profiles shift under each reading''s own lights.',
    'Under the binding-multilateral reading, unilateral relief withdrawal counts as breach and the suppression profile shifts onto the withdrawing party; under the transactional-provisional reading, voidability concentrates discretion in whichever party claims bad faith, and extraction becomes discretionary rather than calibrated. The disagreement is located in the source of bindingness (enacted text vs. calibrated practice vs. continuing consent) and in who may alter or terminate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexicality of the JCPOA bindingness kernel: epsilon and classification are properties of this reading, not of the colloquial label.').

omega_variable(
    proportionality_calibration_ratchet,
    'Is the graduated-response ladder actually calibrated to violation severity, or does any breach license a maximal response (ratchet behavior)?',
    'Cross-case comparison of Joint Commission and Security Council responses to graded violations: heavy-water exceedances, stockpile-cap breaches, 20 percent accumulation, and 60 percent accumulation - did response magnitude track severity?',
    'If responses ratchet rather than scale, the reading''s core premise fails in operation, effective extraction amplifies for the target seats, and the claimed calibration is cover for discretionary punishment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calibration_ratchet, empirical, 'Whether enforcement magnitude tracks violation severity as the graduated reading requires.').

omega_variable(
    relief_symmetry_ambiguity,
    'Are the counterparties'' relief obligations enforceably symmetric with the Iranian compliance obligations, or structurally reversible in a way the performance side is not?',
    'Compare penalty histories: consequences that followed Iranian breaches (snapback threats, referral processes) against consequences that followed the 2018 unilateral suspension of relief delivery (none operating through the commission).',
    'Confirmed asymmetry recodes the reciprocity premise as cover: the arrangement would operate as enforced performance exchanged for discretionary payment, drifting target-seat classifications toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relief_symmetry_ambiguity, empirical, 'Whether the reciprocal exchange is structurally symmetric or one-sidedly enforceable.').

omega_variable(
    deescalation_priority_function,
    'Does the dispute-resolution mechanism actually prioritize de-escalation over formal closure, as this reading claims, or does it function as procedural delay that lets violations consolidate?',
    'Outcome audit of dispute-resolution activations (the January 2020 E3 referral; the August 2025 snapback notification): did processes return parties to prior performance levels, or merely timestamp deterioration while stocks and enrichment grew?',
    'A delay-function finding raises theater_ratio further past the substitution threshold and supports a vestigial-drift hypothesis for the machinery; a genuine de-escalation record supports the coordination reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deescalation_priority_function, empirical, 'Whether dispute resolution de-escalates or launders delay.').

omega_variable(
    identity_lock_persistence,
    'Does the framework persist because it still coordinates, or because the diplomatic establishments'' careers and institutional self-concepts are fused with its survival?',
    'Counterfactual staffing test: examine whether successor diplomatic staff without ownership of the 2015 negotiation maintain the calibrated ladder as the reference point, or discard it for ad hoc bilateral management.',
    'Identity-driven persistence inflates the apparent coordination function; if rotation breaks the lock and the ladder lapses, the residual structure is maintenance of form, and classification drifts toward the vestigial type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, conceptual, 'Coordination function versus identity fusion among the framework''s professional custodians.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_grad_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(jcpoa_grad_tr_t0, observed).
narrative_ontology:measurement(jcpoa_grad_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.16).
narrative_ontology:measurement_basis(jcpoa_grad_tr_t2, observed).
narrative_ontology:measurement(jcpoa_grad_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement_basis(jcpoa_grad_tr_t4, observed).
narrative_ontology:measurement(jcpoa_grad_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement_basis(jcpoa_grad_tr_t6, observed).
narrative_ontology:measurement(jcpoa_grad_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.43).
narrative_ontology:measurement_basis(jcpoa_grad_tr_t8, observed).
narrative_ontology:measurement(jcpoa_grad_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement_basis(jcpoa_grad_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(jcpoa_grad_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(jcpoa_grad_be_t0, observed).
narrative_ontology:measurement(jcpoa_grad_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.47).
narrative_ontology:measurement_basis(jcpoa_grad_be_t2, observed).
narrative_ontology:measurement(jcpoa_grad_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.57).
narrative_ontology:measurement_basis(jcpoa_grad_be_t4, observed).
narrative_ontology:measurement(jcpoa_grad_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement_basis(jcpoa_grad_be_t6, observed).
narrative_ontology:measurement(jcpoa_grad_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.69).
narrative_ontology:measurement_basis(jcpoa_grad_be_t8, observed).
narrative_ontology:measurement(jcpoa_grad_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(jcpoa_grad_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_grad_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(jcpoa_grad_su_t0, observed).
narrative_ontology:measurement(jcpoa_grad_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.52).
narrative_ontology:measurement_basis(jcpoa_grad_su_t2, observed).
narrative_ontology:measurement(jcpoa_grad_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.63).
narrative_ontology:measurement_basis(jcpoa_grad_su_t4, observed).
narrative_ontology:measurement(jcpoa_grad_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement_basis(jcpoa_grad_su_t6, observed).
narrative_ontology:measurement(jcpoa_grad_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement_basis(jcpoa_grad_su_t8, observed).
narrative_ontology:measurement(jcpoa_grad_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(jcpoa_grad_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the colloquial label 'the JCPOA's bindingness' into three structurally distinct constraints per the epsilon-invariance principle: binding_multilateral_reading (epsilon anchored to enacted-text supremacy and consensus-gated change), graduated_compliance_reading (this file; epsilon anchored to performance-calibrated reciprocity), and transactional_provisional_reading (epsilon anchored to continuing consent, voidable on unilateral bad-faith determination). Each carries its own beneficiaries, victims, and enforcement profile. Dependency direction: the binding reading supplies the legal substrate this reading operationalizes; the transactional reading denies the shared premise both others rest on. All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
