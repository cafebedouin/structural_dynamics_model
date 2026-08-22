% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: JCPOA Graduated Compliance Reading — Scaled Reciprocal Commitment with Proportional Enforcement
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The colloquial
 *   label 'the JCPOA' conflates three structurally distinct commitments about
 *   what kind of instrument it is; per the epsilon-invariance principle this
 *   file authors only the graduated_compliance_reading: a scaled reciprocal
 *   commitment whose enforcement is tied to proportional compliance
 *   assessment. On this reading the arrangement converts a binary
 *   comply-or-collapse standoff into a continuous dial — enrichment ceilings
 *   traded for sanctions relief, breaches answered by proportionally scaled
 *   relief withdrawal rather than total rupture, disputes handled by a
 *   standing Joint Commission that prioritizes de-escalation over formal
 *   legal closure. The arrangement as designed coordinates genuine
 *   verification goods among mutually distrustful parties; the arrangement as
 *   operated developed a pronounced asymmetry, with graduated penalties
 *   firing against one party while the counterparty relief became deliverable
 *   or withholdable at a single seat's discretion. KEY AGENTS (by structural
 *   relationship): see key_agents. The claim/metric gap is deliberate:
 *   claimed_type states what this reading believes is structurally true (a
 *   hybrid with a real coordination core and a real paying seat); the metrics
 *   describe how the standing arrangement actually behaved across the
 *   interval. The engine computes per-seat classifications from the
 *   structural data; the authored claim does not adjudicate them.
 *
 * KEY AGENTS:
 *   - - e3_eu_coordinators: Agenda-setting administrator (institutional/constrained) — co-chairs the Joint Commission, owns the dispute-resolution calendar, initiates snapback procedures
 *   - - us_executive_branch: Agenda-setting co-author (institutional/mobile) — holds a unilateral relief-suspension lever through domestic waiver law; shortest time horizon at the table
 *   - - iranian_state: Primary cost-bearing party with beneficiary residue (organized/trapped) — exchanged physically irreversible nuclear concessions for politically reversible relief
 *   - - iranian_civil_economy: Diffuse cost bearer (powerless/trapped) — absorbs every relief withdrawal with no seat in the process
 *   - - european_trade_enterprises: Partial-engagement beneficiary (powerful/arbitrage) — monetized the relief window, hedges by diversifying markets
 *   - - pragmatic_diplomacy_establishment: Identity-fused beneficiary (organized/identity_locked) — professional method vindicated when the framework functions
 *   - - iaea_verification_mandate: Mandate-collecting beneficiary (institutional/constrained) — its monitoring reports are the shared evidentiary base all parties cite
 *   - - eastern_p5_parties: Legitimacy beneficiary (institutional/mobile) — veto-position governance frame and opened trade channels at low cost
 *   - - regional_nonparty_powers: Excluded objectors (powerful/mobile) — neighboring states barred from the table that contest the bargain's premises
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.62).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.61).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Reading — Scaled Reciprocal Commitment with Proportional Enforcement").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '16f62452-5caf-4faf-9d7b-ade2e16b9c09').
narrative_ontology:cs_kernel_codification('16f62452-5caf-4faf-9d7b-ade2e16b9c09', formalized).
narrative_ontology:cs_authority_grounding('16f62452-5caf-4faf-9d7b-ade2e16b9c09', distributed).
narrative_ontology:cs_reading_relation('16f62452-5caf-4faf-9d7b-ade2e16b9c09', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('16f62452-5caf-4faf-9d7b-ade2e16b9c09', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('16f62452-5caf-4faf-9d7b-ade2e16b9c09', foundational, enforcement_must_scale_with_violation_severity).
narrative_ontology:cs_axiom_status(enforcement_must_scale_with_violation_severity, holdable).
narrative_ontology:cs_axiom_grounding('16f62452-5caf-4faf-9d7b-ade2e16b9c09', enforcement_must_scale_with_violation_severity, conventional).
narrative_ontology:cs_axiom('16f62452-5caf-4faf-9d7b-ade2e16b9c09', foundational, deescalation_precedes_legal_closure).
narrative_ontology:cs_axiom_status(deescalation_precedes_legal_closure, holdable).
narrative_ontology:cs_axiom_grounding('16f62452-5caf-4faf-9d7b-ade2e16b9c09', deescalation_precedes_legal_closure, instrumental).
narrative_ontology:cs_reference_frame('16f62452-5caf-4faf-9d7b-ade2e16b9c09', scaled_reciprocal_commitment_baseline).
narrative_ontology:cs_drift_state('16f62452-5caf-4faf-9d7b-ade2e16b9c09', post_unilateral_relief_suspension, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('16f62452-5caf-4faf-9d7b-ade2e16b9c09', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_establishment).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, european_trade_enterprises).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_mandate).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, eastern_p5_parties).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civil_economy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, european_trade_enterprises).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, proportional_compliance_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, verifiable_reversibility_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Co-chair the Joint Commission, set the dispute-resolution agenda, and initiate snapback procedures at the Security Council. They receive compliance reporting, convene the parties, and judge when enrichment breaches warrant proportional relief withdrawal. Leaving the framework would forfeit the diplomatic capital invested in negotiating and defending it, so they remain inside while pressing for restoration of the original terms.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, e3_eu_coordinators, agenda_setter,
    institutional, generational, constrained, continental).

% Co-negotiated the arrangement and retains a distinct lever: domestic law ties its sanctions relief to periodic executive waivers, so this seat can suspend its own relief deliveries without consensus. It participates in Joint Commission sessions and holds a snapback vote. Its electoral cycle gives it the shortest time horizon at the table, and its exit cost is low because its relief contributions are reversible by design.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_executive_branch, agenda_setter,
    institutional, biographical, mobile, national).

% Accepted enrichment caps, centrifuge limits, stockpile shipping requirements, and intrusive inspections in exchange for sanctions relief. Its nuclear concessions took years to build and cannot be quickly rebuilt once dismantled, while the relief it receives can be suspended by political decision. When enrichment metrics rise, graduated penalties fall on it; when counterparties under-deliver relief, no symmetrical mechanism compensates it. Walking away forfeits sunk concessions and invites isolation; staying means absorbing one-directional adjustment.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state, payer,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state, beneficiary).

% Ordinary firms, importers, and households absorb whatever relief withdrawal follows enrichment breaches: banking channels narrow, import costs rise, investment freezes on uncertainty. They had no seat in the negotiation and no voice in the Joint Commission; their exposure is set entirely by decisions taken above them.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civil_economy, payer,
    powerless, biographical, trapped, national).

% Re-entered the Iranian market during the relief window — aircraft sales, energy contracts, banking relationships — and booked revenue from partial engagement. They also carry compliance risk: contracts strand whenever relief is withdrawn, and they hedge by diversifying trade elsewhere, which keeps their outside options strong and their losses bounded.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, european_trade_enterprises, beneficiary,
    powerful, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, european_trade_enterprises, payer).

% Arms-control professionals and diplomatic services whose method — negotiated, verifiable, incremental restraint — is vindicated when the framework functions. Their careers, networks, and professional self-concept are built around engagement as the working alternative to coercion; abandoning the framework would repudiate that method, so they defend it through successive crises.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_establishment, beneficiary,
    organized, generational, identity_locked, global).

% Receives expanded inspection authority, funding, and reporting centrality from the arrangement. Its monitoring reports are the shared evidentiary base every party cites. Its budget and access depend on continued member-state support for the machinery, giving it a direct stake in the framework's survival.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_mandate, beneficiary,
    institutional, civilizational, constrained, global).

% Two Security Council permanent members that signed the arrangement and profit from its legitimating frame: it channels nonproliferation governance through a body where they hold veto power, and it opened sanctioned trade channels. They bear few costs from the framework's stresses and retain freedom to deepen bilateral ties regardless of its fate.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, eastern_p5_parties, beneficiary,
    institutional, generational, mobile, continental).

% Neighboring states excluded from the negotiation that object that the framework legitimizes a threshold nuclear capability while leaving their security concerns unaddressed. They lobby national capitals against restoration, pursue parallel defense acquisitions, and would demand a different bargain entirely if admitted.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_nonparty_powers, excluded,
    powerful, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__graduated_compliance_reading, e3_eu_coordinators).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__graduated_compliance_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a binary comply-or-collapse standoff into a continuous, verifiable dial: shared enrichment metrics, intrusive but bounded inspection, a pre-agreed ladder of proportional responses, and a standing Joint Commission so disputes adjust calibration instead of triggering crisis escalation. Verification standardization solves a genuine collective-action problem among adversaries who otherwise cannot trust each other's measurements.
% TRANSFER_FUNCTION: Moves enforceable restraint — enrichment ceilings, stockpile limits, centrifuge counts, inspection access — from Iran to the verifying parties, and reversible economic relief — sanctions suspension, trade and banking access — from the sanctioning parties to Iran. When enrichment metrics rise, the machinery moves relief back out proportionally; nothing in the design moves equivalent compensation back when relief under-delivers.
% ABSENT_VOICES: Regional non-party powers would object that the framework legitimizes a threshold capability while ignoring their security concerns; they stand outside the Joint Commission lobbying national capitals. Iranian civil society and the private economy bear the enforcement costs but had no seat in a state-to-state bargain. Both absences shaped the apparent consensus: unanimity inside the room partly reflects who was never invited.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would end the only standing verification channel into the Iranian program, remove the pre-agreed de-escalation ladder just as enrichment metrics climb, strand European firms' legal certainty, and return all parties to ad hoc crisis management — the regional proliferation cascade the framework suppressed would resume organizing immediately.
% FOUNDING_PROBLEM: Built to solve: a covert-to-threshold nuclear program advancing under failing interim arrangements, with sanctions spiraling without stopping enrichment and military strikes live as the alternative — the founding task was converting an unverifiable race into measured, inspectable, reversible restraint bought with sequenced relief.
% FOUNDING_PROBLEM_CORROBORATION: IAEA monitoring reports corroborate the technical problem and its partial management from outside the benefiting parties, and independent arms-control analysis attests both the original threat and the framework's degradation. Regional non-party powers attest the opposite verdict — that the founding problem was never solved, only deferred — so corroboration exists for the problem's reality while its resolution status remains disputed across seats.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction ends the interval at 0.62: the design intends calibration, but the standing arrangement as operated withdraws relief proportionally while delivering no symmetrical compensation, and the paying party's sunk concessions cannot be repossessed — by this reading's own lights that operational asymmetry is the dominant extractive fact. Suppression (0.61) is authored as a raw structural property, unscaled by power or scope: the snapback threat, the secondary-sanctions shadow, and intrusive-but-bounded inspection constitute the coercive surface; the engine scales only extractiveness, by directionality and scope. Theater rises from 0.15 to 0.44 as proportionality assessments and Joint Commission sessions continue past the point where the underlying reciprocity reliably functions. Accessibility_collapse is 0.45: understanding the graduated design does not close alternatives — rival readings of the same instrument, military options, and full-normalization paths all remain live — which is precisely why resistance (0.68) stayed high: cap exceedances, a co-author's unilateral relief suspension, and sustained external lobbying all met the machinery head-on. The measurement series run on one shared time grid (points 0, 2, 4, 6, 8, 10) so every tracked metric is authored at every examined point. Claim and metrics are independent authored facts.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structure. From the administering seat (e3_eu_coordinators) the arrangement is a functioning instrument it built and calendars — the coordination half dominates. From the trapped payer seats (iranian_state, iranian_civil_economy) the same machinery operates as one-directional penalty with no compensating channel — the extraction half dominates. Arbitrage-grade exit damps the experienced burden at european_trade_enterprises: stranded contracts hurt, but diversification bounds the loss, so that seat computes milder than the trapped payers despite sharing the payer residue. The identity_locked diplomacy establishment experiences challenge to the framework as challenge to its method, deepening its beneficiary-side reading. Coalition potential for the powerless seat is thin: iranian_civil_economy's interests are mediated entirely by the state that bargained on its behalf, so class-level coalition formation has no independent channel into the Joint Commission. The engine computes these divergences from power, exit, and role data; nothing in the authored claim settles them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (pragmatic_diplomacy_establishment, european_trade_enterprises, iaea_verification_mandate, eastern_p5_parties) drive those seats toward the beneficiary end; victim declarations (iranian_state, iranian_civil_economy) drive them toward the target end. iranian_state carries both declarations deliberately: it receives relief (beneficiary residue) but the enforcement asymmetry and trapped exit dominate, so its net position sits target-side — the dual declaration encodes the reciprocity this reading affirms while the exit atom encodes the asymmetry it concedes. Exit options differentiate same-power seats: two institutional agenda-setters diverge because one is constrained (forfeited diplomatic capital binds it to the table) and one is mobile (waiver lever, electoral horizon); two powerful seats diverge because one holds arbitrage (trade diversion) and one holds mobility without material stakes in the bargain. No directionality overrides are authored: the derivation chain distinguishes every seat that needs distinguishing through exit options, and the dual-role case is handled by declaring both roles rather than overriding the arithmetic. Suppression stays unscaled; scope amplification touches extractiveness only, with the continental-to-global spread of the party seats raising verification difficulty modestly.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope gate forces both halves into the open: the coordination achievement (shared metrics, inspection access, a de-escalation ladder that repeatedly pulled crises back from rupture) and the paying seat (irreversible concessions against reversible relief, with graduated penalties firing in one direction only). Reading the arrangement as pure cooperation hides the asymmetry; reading it as pure extortion discards verification goods that every party, the paying party included, consumed. Mandatrophy risk here runs toward piton rather than snare: if relief restoration fails permanently, Joint Commission sessions and proportionality assessments could persist as ritual while the bargain's substance stays dormant — the theater_ratio series (0.15 rising to 0.44) tracks exactly that drift, and the agenda_setter/payer pair is named accordingly. founding_problem_status is authored contested rather than dead, so the mismatch consumer finds no dead-problem-plus-world_rearranges flag; the honest signal available is the rising theater series itself, which is why the temporal data is included on a non-mountain story.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_bindingness_reading_ambiguity,
    'This story instantiates the graduated_compliance_reading of the jcpoa_treaty_bindingness kernel; how would classification shift under the binding_multilateral_reading or the transactional_provisional_reading?',
    'Classify the sibling stories separately and compare per-seat outputs; divergence between the three classifications localizes which structural element (modification procedure versus response function versus voidability condition) drives the difference.',
    'The binding reading raises effective burden on signatory seats (consensus lock-in adds trap force); the transactional reading raises suppression for all parties (unilateral exit threat dominates the response function); neither changes the declared coordination function, which all three readings share.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_bindingness_reading_ambiguity, conceptual, 'Committer-frame routing: one kernel, three readings, classification indexed to reading.').

omega_variable(
    proportionality_calibration_seat,
    'Who calibrates ''proportional'' — is the violation-severity metric genuinely shared, or does the calibrating seat measure its own case?',
    'Audit Joint Commission dispute records across the interval: whether severity assessments were adopted by consensus or asserted unilaterally, and whether relief-withdrawal magnitudes tracked the assessed severity.',
    'Unilateral calibration converts graduated enforcement into discretionary penalty power and pushes the arrangement from tangled_rope toward snare; consensus calibration stabilizes the rope component and validates this reading''s core premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calibration_seat, empirical, 'Whether the graduated dial is jointly held or seat-captured.').

omega_variable(
    reversibility_asymmetry_origin,
    'Is the asymmetry between the paying party''s physically irreversible concessions and the counterparties'' politically reversible relief intrinsic to the design or an artifact of implementation sequencing?',
    'Counterfactual comparison with arrangements that sequenced relief delivery ahead of dismantlement, or codified relief durability in domestic law before concessions began.',
    'Intrinsic asymmetry entrenches the tangled_rope classification with a persistent paying seat; artifact-of-sequence implies a recoverable rope whose extraction was a bargaining-order failure rather than a structural property.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reversibility_asymmetry_origin, empirical, 'Design-intrinsic versus implementation-artifact origin of the core asymmetry.').

omega_variable(
    enforcement_symmetry_record,
    'Does the graduated machinery bind all parties symmetrically, or does proportional response in practice fire only against the paying party?',
    'Enumerate Joint Commission rulings and snapback activations by target party across the interval, including any instance where an under-delivering counterparty faced proportional consequence.',
    'A one-directional activation record confirms the victim-side directionality declared here and supports the elevated epsilon; a symmetric record would lower it and strengthen the pure-coordination reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_symmetry_record, empirical, 'Directional symmetry of the graduated enforcement record.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.22).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.34).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.41).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.46).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.44).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.63).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.42).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.66).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).

% DUAL FORMULATION NOTE:
% Constraint family per the epsilon-invariance principle: the colloquial label 'the JCPOA' conflates three structurally distinct commitments, each with its own epsilon, beneficiary structure, and classification. This graduated_compliance_reading links to both siblings via affects_constraints. Upstream/downstream: the binding_multilateral_reading is the legal-form claim this reading's defenders cite when resisting unilateral exit, so it shapes this reading's persistence conditions; the transactional_provisional_reading competes for the same response-function slot and loses ground with every demonstrated proportional-adjustment episode, which is why this reading's relation to it is influences rather than coexists_with.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
