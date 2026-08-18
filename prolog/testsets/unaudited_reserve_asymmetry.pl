% ============================================================================
% CONSTRAINT STORY: unaudited_reserve_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unaudited_reserve_asymmetry, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unaudited_reserve_asymmetry
 *   human_readable: Unaudited Reserve Asymmetry (Ship's Cask vs. Notched Ration)
 *   domain: organizational/maritime-logistics
 *
 * SUMMARY:
 *   Aboard a provisioning ship, a reserve cask of emergency food exists
 *   alongside a pencil ledger that is supposed to record its exact level. The
 *   general crew — Farro among them — draws daily rations against a visible,
 *   physically notched measure: the notch is the entire scarcity they can
 *   perceive. Meanwhile, an officer class holding the reserve's key-ring
 *   draws against the cask in daylight, sometimes without logging the draw,
 *   sometimes logging a figure below what the cask actually holds. The
 *   scarcity the rationed crew experiences is therefore not the ship's true
 *   scarcity — it is a manufactured proxy, calibrated to a ledger figure that
 *   the keyholders themselves control and that no independent party can check
 *   against the cask. The claimed type here is snare: the reserve's stated
 *   coordination function (emergency buffering against unforeseeable
 *   shortage) is real in principle, but it functions as cover for a structure
 *   that persists specifically because it is unaudited and because exit for
 *   the rationed crew (confined aboard ship) does not exist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unaudited_reserve_asymmetry, 0.81).
domain_priors:suppression_score(unaudited_reserve_asymmetry, 0.72).
domain_priors:theater_ratio(unaudited_reserve_asymmetry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unaudited_reserve_asymmetry, extractiveness, 0.81).
narrative_ontology:constraint_metric(unaudited_reserve_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unaudited_reserve_asymmetry, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unaudited_reserve_asymmetry, accessibility_collapse, 0.44).
narrative_ontology:constraint_metric(unaudited_reserve_asymmetry, resistance, 0.51).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unaudited_reserve_asymmetry, snare).
narrative_ontology:human_readable(unaudited_reserve_asymmetry, "Unaudited Reserve Asymmetry (Ship's Cask vs. Notched Ration)").
narrative_ontology:topic_domain(unaudited_reserve_asymmetry, "organizational/maritime-logistics").

domain_priors:requires_active_enforcement(unaudited_reserve_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unaudited_reserve_asymmetry, thess_and_officers_with_reserve_access).
narrative_ontology:constraint_victim(unaudited_reserve_asymmetry, general_ration_crew_farro_and_peers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unaudited_reserve_asymmetry, ships_captain).
narrative_ontology:constraint_vindicates(unaudited_reserve_asymmetry, reserve_stewardship_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the key-ring to the reserve cask and the pencil ledger that records its level. Draws against the cask in daylight without logging the draw, or logs a figure lower than what remains untouched. Justifies any scrutiny by citing stewardship duty over the ship's true survival margin. Because the ledger and the lock are held by the same hand, no one can independently verify whether the paper figure and the cask level match — and the gap between them is exactly the room this seat operates in.
narrative_ontology:constraint_stakeholder(unaudited_reserve_asymmetry, thess_and_officers_with_reserve_access, beneficiary,
    institutional, biographical, arbitrage, local).
narrative_ontology:stakeholder_secondary_role(unaudited_reserve_asymmetry, thess_and_officers_with_reserve_access, agenda_setter).

% Draws rations against a notched measure that is visible, physical, and non-negotiable — the notch is the only scarcity they can see or contest. They have no access to the cask, no view of the ledger, and no keyring. Their hunger is calibrated to the notch, not to the actual reserve; if the reserve is fuller than recorded, their scarcity is manufactured rather than discovered. Confined aboard ship, there is no exit until landfall, and no lateral channel to compare notes against the officers' books.
narrative_ontology:constraint_stakeholder(unaudited_reserve_asymmetry, general_ration_crew_farro_and_peers, payer,
    powerless, immediate, trapped, local).

% Nominally responsible for provisioning records but lacks independent access to the cask itself — the clerk transcribes whatever figure the keyholder reports and has no instrument to cross-check it. Would object to the unaudited draws if given cask access or a counting method, but is structurally kept out of the one room that would let the transcription mean anything.
narrative_ontology:constraint_stakeholder(unaudited_reserve_asymmetry, ships_quartermaster_clerk, excluded,
    moderate, biographical, constrained, local).

% Sits above the daily ration dispute and receives the pencil figure as settled fact, treating it as sufficient basis for voyage planning and crew discipline. Benefits indirectly from the appearance of a managed, sufficient reserve — a comfortable command narrative — without personally auditing the cask, and therefore has no independent means of detecting the asymmetry even if motivated to look.
narrative_ontology:constraint_stakeholder(unaudited_reserve_asymmetry, ships_captain, observer,
    institutional, generational, analytical, local).
narrative_ontology:stakeholder_secondary_role(unaudited_reserve_asymmetry, ships_captain, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unaudited_reserve_asymmetry, thess_and_officers_with_reserve_access).
narrative_ontology:fixing_cost_class(unaudited_reserve_asymmetry, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A shared reserve buffer genuinely solves a real problem: pooling emergency provisions against unforeseeable shortage (storms, delayed landfall, spoilage) so that no single ration cycle failure sinks the voyage.
% TRANSFER_FUNCTION: Moves actual food volume from the general ration allotment's true entitlement into an unaudited surplus accessible only to keyholders — the crew's notch-measured hunger is calibrated against a reserve figure that may not reflect what the cask actually holds, transferring slack from the many to the few holding the key.
% ABSENT_VOICES: The quartermaster's clerk, who transcribes the ledger but cannot verify it against the cask, would object to unaudited draws if given the means to check; the general crew has no representative in the room where the reserve figure is set at all.
% DISAPPEARANCE_RATIONALE: If the asymmetry were resolved overnight — reserve audited, ledger reconciled to cask, draws logged and visible to ration recipients — daily notch allotments would likely increase to reflect true surplus, keyholder daylight draws would end or require justification, and crew trust in command's provisioning claims would shift substantially; the ship's actual survival margin, not merely its recorded one, would become common knowledge.
% FOUNDING_PROBLEM: Ships historically needed a discretionary emergency reserve because voyages run longer than planned and a rigid, fully-visible ration schedule leaves no margin for storms, spoilage, or delay — someone competent needed authority to hold back and dispense surplus at their judgment.
% FOUNDING_PROBLEM_CORROBORATION: Officers with reserve access attest the discretionary buffer remains necessary for voyage safety. No corroboration from outside the benefiting parties exists aboard: the clerk cannot verify the cask independently, the crew has no access to the ledger, and the captain relies on the same unaudited figure the keyholders report — there is no seat on this ship positioned to confirm or deny the founding problem's current status from outside the beneficiary group.
narrative_ontology:disappearance_verdict(unaudited_reserve_asymmetry, world_rearranges).
narrative_ontology:founding_problem_status(unaudited_reserve_asymmetry, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unaudited_reserve_asymmetry, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-09',
    'uke_narrative', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'temperature=0.2,max_tokens=8192').
narrative_ontology:story_seed(unaudited_reserve_asymmetry, 'none', 1).
narrative_ontology:epsilon_provenance(unaudited_reserve_asymmetry, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unaudited_reserve_asymmetry_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unaudited_reserve_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unaudited_reserve_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high and rising (0.42 to 0.81 across the interval) because the gap between the pencil figure and the actual cask level appears to widen the longer the voyage runs without audit — each unlogged or under-logged daylight draw compounds. Suppression is substantial (0.72) but not total: the notch measure itself is a form of structural suppression (it fixes what the crew can perceive as their entitlement) while nothing prevents an audit in principle — the suppression is maintained by keyring exclusivity and information asymmetry, not by force. Theater ratio rises over the interval (0.28 to 0.58) because as scrutiny risk increases, more of the ledger-keeping activity becomes performative reconciliation rather than honest accounting — the pencil figure increasingly exists to be shown, not to be true.
 *
 * PERSPECTIVAL GAP:
 *   From the keyholder officer seat, this looks like prudent discretionary stewardship of a necessary emergency margin — a coordination function they are entitled to administer with judgment. From the rationed crew's seat, the identical structure is pure extraction: their hunger is real and immediate, calibrated against a figure they cannot see or challenge, while surplus flows invisibly to those who hold the lock. The engine computes these as different seat-types from the same structural data — the divergence is not an error, it is the finding.
 *
 * DIRECTIONALITY LOGIC:
 *   Officers with reserve access are the clearest beneficiaries: they collect the undocumented surplus and control both the physical access (keyring) and the informational record (ledger) that would otherwise expose the gap — d sits near the full-beneficiary end, reinforced by arbitrage-grade exit (they can adjust the figure at will). The general crew is the clear target: trapped aboard ship, calibrated to a notch they cannot renegotiate, with zero visibility into the reserve's true state — d sits near the full-target end. The captain is a secondary beneficiary (comfortable command narrative) but not a keyholder, so directionality there is milder than the officers' — this asymmetry between two nominally institutional-power seats is deliberate and is why an override is not needed: the captain's exit is analytical/observational rather than arbitrage-grade, so the derivation chain already separates them without manual correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The reserve's founding problem — buffering against unpredictable shortage — is not obviously dead; ships genuinely can run into extended voyages. But the arrangement as currently operated has drifted from solving that problem to exploiting the absence of audit that solving it originally required. Classifying this as snare rather than tangled_rope turns on whether a genuine coordination function is still being served: the beneficiaries here are not identifiably coordinating anything for the crew's benefit — the crew derives no protective value from a reserve they cannot access, verify, or draw on in the emergency it is nominally held for. The coordination story is cover, not substance, which is the snare signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reserve_necessity_vs_capture,
    'Is the discretionary, unaudited reserve arrangement a genuinely necessary emergency-buffering mechanism that happens to be currently exploited, or has it always functioned primarily as an officer perquisite with the emergency-buffer justification as post-hoc cover?',
    'A full audit reconciling the ledger against the actual cask level, cross-referenced against historical voyage records showing whether the reserve was ever drawn upon for genuine emergencies versus routine daylight officer consumption.',
    'If the reserve was rarely or never used for genuine emergencies, the coordination story collapses entirely and the classification is unambiguously snare with no residual coordination credit. If emergency use is documented, some tangled-rope coordination credit may be warranted even under continued extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reserve_necessity_vs_capture, empirical, 'Whether the reserve''s coordination function is real-but-exploited or fabricated cover.').

omega_variable(
    notch_calibration_provenance,
    'Was the notch-measured ration originally calibrated against the true total provisions (cask plus visible stock), or was it calibrated only against the visible stock from the start, making the crew''s scarcity artificial by design rather than by later drift?',
    'Historical provisioning records at voyage departure, if they exist independently of the current keyholders'' ledger, showing the notch''s original derivation.',
    'If the notch was always calibrated short of true total supply, this reclassifies the asymmetry as a founding design feature rather than an emergent drift — strengthening the snare reading and weakening any residual mandatrophy defense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(notch_calibration_provenance, empirical, 'Whether the crew''s ration scarcity was designed-in or drifted-into.').

omega_variable(
    clerk_independence_ambiguity,
    'Could the quartermaster''s clerk have obtained independent cask-verification authority at any point, or was exclusion from the cask itself part of the enforcement structure protecting the asymmetry?',
    'Review of the ship''s standing orders regarding provisioning authority and whether any prior clerk sought and was denied cask access.',
    'If exclusion was actively enforced rather than merely customary, this strengthens requires_active_enforcement as a structural fact rather than an inferred one, and supports a higher suppression reading tied specifically to information control.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clerk_independence_ambiguity, empirical, 'Whether the clerk''s exclusion from cask verification is enforced or merely customary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unaudited_reserve_asymmetry, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unau_tr_t0, unaudited_reserve_asymmetry, theater_ratio, 0, 0.28).
narrative_ontology:measurement(unau_tr_t8, unaudited_reserve_asymmetry, theater_ratio, 8, 0.34).
narrative_ontology:measurement(unau_tr_t16, unaudited_reserve_asymmetry, theater_ratio, 16, 0.41).
narrative_ontology:measurement(unau_tr_t24, unaudited_reserve_asymmetry, theater_ratio, 24, 0.48).
narrative_ontology:measurement(unau_tr_t32, unaudited_reserve_asymmetry, theater_ratio, 32, 0.54).
narrative_ontology:measurement(unau_tr_t40, unaudited_reserve_asymmetry, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(unau_be_t0, unaudited_reserve_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(unau_be_t8, unaudited_reserve_asymmetry, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(unau_be_t16, unaudited_reserve_asymmetry, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(unau_be_t24, unaudited_reserve_asymmetry, base_extractiveness, 24, 0.71).
narrative_ontology:measurement(unau_be_t32, unaudited_reserve_asymmetry, base_extractiveness, 32, 0.77).
narrative_ontology:measurement(unau_be_t40, unaudited_reserve_asymmetry, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(unau_su_t0, unaudited_reserve_asymmetry, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unau_su_t8, unaudited_reserve_asymmetry, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(unau_su_t16, unaudited_reserve_asymmetry, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(unau_su_t24, unaudited_reserve_asymmetry, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(unau_su_t32, unaudited_reserve_asymmetry, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(unau_su_t40, unaudited_reserve_asymmetry, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unaudited_reserve_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(unaudited_reserve_asymmetry, 0.1).

% DUAL FORMULATION NOTE:
% This story addresses only the reserve-asymmetry constraint (unaudited keyholder draw vs. ledger figure). A sibling constraint could be written for the notch-ration mechanism itself as experienced independent of reserve knowledge (i.e., is the notch calibration a separate constraint on the crew even absent any officer misconduct?) — that would be a distinct ε and likely a rope or scaffold rather than a snare, since the notch alone, honestly calibrated, is a legitimate rationing coordination device. Not authored here to preserve ε-invariance; flagged for potential decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
