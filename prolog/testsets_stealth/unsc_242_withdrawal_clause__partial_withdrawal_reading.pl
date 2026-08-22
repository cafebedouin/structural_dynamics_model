% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause — Partial-Withdrawal Reading (Discretionary Scope)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the UNSC 242 withdrawal-clause
 *   kernel: the partial-withdrawal reading, under which the scope of required
 *   withdrawal is discretionary — fixed by negotiation rather than by the
 *   text — because the drafters' intent is said to be encoded in the
 *   indefinite English article ('from territories', not 'from the
 *   territories'), and because the secure-and-recognized-boundaries principle
 *   licenses retention of strategic terrain pending agreement. The standing
 *   arrangement under contest — the thing this story's epsilon is ABOUT — is
 *   that discretionary-retention regime itself, assessed by this reading's
 *   own lights; the maximalist alternative is a different constraint in a
 *   different file and is not averaged in here. The reading instantiates a
 *   ledger: textual indefiniteness is converted into negotiating leverage,
 *   with the occupying power and the process mediators collecting from the
 *   open scope and the claimant parties paying through deferred and unfixed
 *   entitlement. The claim/metric gap is deliberate: the reading CLAIMS
 *   tangled_rope (genuine disengagement coordination plus asymmetric
 *   extraction through the same structure) while the metrics are authored
 *   independently from descriptive observation of the arrangement's actual
 *   operation.
 *
 * KEY AGENTS:
 *   - occupying_power: primary beneficiary ([powerful]/[constrained]) — retains strategic terrain, controls facts on the ground, also bears holding costs
 *   - process_mediators: agenda-setting beneficiary ([institutional]/[arbitrage]) — control the phased sequence; open scope sustains their role
 *   - territorial_claimant_states: primary target ([organized]/[constrained]) — no fixed enforcement line, episodic leverage only
 *   - displaced_resident_populations: primary target ([powerless]/[trapped]) — no enforceable return date; lives spent inside the negotiating horizon
 *   - unrepresented_territory_residents: excluded voice ([powerless]/[trapped]) — subject to the outcome, absent from the table
 *   - international_judicial_bodies: analytical observer ([institutional]/[analytical]) — construes the clause, shapes legitimacy, executes nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.63).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.63).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC 242 Withdrawal Clause — Partial-Withdrawal Reading (Discretionary Scope)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'f6329856-c7e1-42b5-b9c6-aff9dc52b0de').
narrative_ontology:cs_kernel_codification('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', fixed_text).
narrative_ontology:cs_authority_grounding('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', lineage).
narrative_ontology:cs_interpretation_layer_present('f6329856-c7e1-42b5-b9c6-aff9dc52b0de').
narrative_ontology:cs_reading_relation('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', foundational, withdrawal_scope_discretionary_by_drafting_intent).
narrative_ontology:cs_axiom_status(withdrawal_scope_discretionary_by_drafting_intent, holdable).
narrative_ontology:cs_axiom_grounding('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', withdrawal_scope_discretionary_by_drafting_intent, empirically_contingent).
narrative_ontology:cs_axiom('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', foundational, secure_boundaries_license_interim_retention).
narrative_ontology:cs_axiom_status(secure_boundaries_license_interim_retention, holdable).
narrative_ontology:cs_axiom_grounding('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', secure_boundaries_license_interim_retention, instrumental).
narrative_ontology:cs_axiom('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', secondary, negotiated_settlement_precedes_prescription).
narrative_ontology:cs_axiom_status(negotiated_settlement_precedes_prescription, holdable).
narrative_ontology:cs_axiom_grounding('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', negotiated_settlement_precedes_prescription, conventional).
narrative_ontology:cs_reference_frame('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', drafters_flexibility_compromise).
narrative_ontology:cs_drift_state('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', post_archival_drafting_history_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f6329856-c7e1-42b5-b9c6-aff9dc52b0de', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, process_mediators).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimant_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_resident_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the territories whose return the clause addresses. Under this reading it may retain strategic terrain pending negotiation of secure and recognized boundaries, and it administers day-to-day control on the ground, building civilian presence that hardens the de facto line while talks proceed. It also bears real costs — garrison burden, episodic diplomatic isolation, exposure to the rival reading in fora it does not control. Full unilateral withdrawal is physically available but carries severe security and political costs, so exit is constrained rather than closed.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter).

% Broker the phased sequence: special missions, disengagement frameworks, conferences, successive plans. The open scope is what keeps them indispensable — a fixed withdrawal line would close the mediation market they staff. When one framework stalls they redeploy to a new format, moving between processes rather than exiting the role.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, process_mediators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unsc_242_withdrawal_clause__partial_withdrawal_reading, process_mediators, beneficiary).

% Assert sovereignty over the territories and receive no fixed entitlement date or boundary line. Their leverage is episodic — coalition politics, embargo, war — and each concluded round of talks has left them worse positioned than the last. Compulsory adjudication is unavailable because jurisdiction requires consent they cannot obtain; abandoning the diplomatic track risks losing the international-legitimacy standing that is their main remaining asset.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_claimant_states, payer,
    organized, generational, constrained, regional).

% Displaced in the 1967 conflict and their descendants. Discretionary scope means no enforceable return date exists; individual lives run out inside a negotiating horizon that keeps receding. Return is contingent on agreements they do not sign and cannot influence, and statelessness closes ordinary mobility exits.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_resident_populations, payer,
    powerless, biographical, trapped, regional).

% Live under the administration of the retained territories without party status in the framework that will fix their borders and civic status. They would object on self-determination grounds if seated; representation arrived late, partially, and through proxies, never as a direct place in the process.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, unrepresented_territory_residents, excluded,
    powerless, biographical, trapped, local).

% Claim competence to construe the clause through advisory opinions and doctrinal codification. Their readings tend to favor the broader withdrawal obligation, but they hold no execution power; they shape legitimacy conditions for the competing readings rather than outcomes on the ground.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_judicial_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common reference framework that let belligerents move from open warfare to negotiated disengagement: it names the subjects talks would address — withdrawal linked to termination of belligerency, secure and recognized boundaries, freedom of navigation — solving once, centrally, the problem of what a settlement process would even be about.
% TRANSFER_FUNCTION: Moves negotiating leverage and time from the territorial claimants to the occupying power: each year of discretionary scope transfers de facto control and bargaining position. It also moves diplomatic attention, mediating rents, and process-control to the brokers who manage the phasing.
% ABSENT_VOICES: The displaced populations and residents of the retained territories had no seat; the claimant states were largely outside the drafting room in which the text was produced; proponents of the broader withdrawal obligation were not represented among the drafters. Their objections enter only retrospectively, through General Assembly majorities and advisory opinions that the framework's enforcement machinery does not execute.
% DISAPPEARANCE_RATIONALE: If the discretionary-scope framework vanished overnight, the parties would rearrange around one of two alternatives: the broader obligation would bind as the operative reading, forcing withdrawal negotiations on materially different terms, or no agreed framework would exist at all, returning the parties to declared belligerency. Either way the current settlement architecture — mediation roles, phased processes, the de facto lines — dissolves.
% FOUNDING_PROBLEM: After the June 1967 war: how to convert battlefield facts into a stable settlement — providing the victorious power secure and recognized boundaries and recognition while restoring territory, ending the state of belligerency, without dictating final lines in the resolution itself.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the UN Secretariat record and the Jarring mission correspondence, which attest the immediate postwar problem was real; by published drafting histories built on non-party archives; and by claimant-state foreign ministries, which attest the founding problem existed but argue its acute phase was resolved decades ago and the framework now perpetuates dispute. The benefiting parties' own attestation dominates public defense of the reading, which is precisely why the archival and claimant-side sources matter.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.63, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.63 at interval end) rather than high because the arrangement is conditional and phased: retention is framed as provisional pending agreement, and real withdrawals have occurred under its logic (the Sinai disengagements and the Egypt-Israel territorial settlement operated within this framework). But extraction accumulates: the series rises monotonically as provisional retention hardened into civilian presence, administrative integration, and annexation measures, each round of talks concluding with the de facto line further east — the T17 accumulation signature. Suppression (0.55) is a raw structural property, unscaled by power or scope: it reflects the active blocking of the rival reading (veto protection of the text against reinterpretation, withholding of consent from compulsory adjudication) and the closure of unilateral exits for the claimants. The suppression_requirement series DECLINES across the interval — this is enforcement decay, not liberalization: early decades required heavy great-power expenditure to hold the discretionary reading against maximalist pressure; as facts consolidated, less active enforcement was needed to produce the same suppression, so the requirement fell while the suppressed condition persisted. Theater_ratio rises from 0.25 to 0.50: the early process produced physical disengagement, while the later conference cycle increasingly manages appearances around a core question it does not move — proxy-process activity substituting for settlement function (Goodhart drift reaching the threshold at interval end). Accessibility_collapse is 0.45: the rival reading remains fully articulable and periodically ascendant in General Assembly and judicial fora, so alternatives are suppressed in effect but not collapsed in availability. Resistance is 0.70: rejection fronts, embargo leverage, repeated Assembly majorities, and adverse advisory opinions constitute sustained active resistance. All three series share one time grid (points 0, 11, 22, 33, 44, 55 — approximately 1967, 1978, 1989, 2000, 2011, 2022) so no metric is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats should compute a fundamentally different constraint than the payer seats. From the occupying power's seat, the arrangement is managed flexibility it paid for in wars and garrisons — a framework that made orderly disengagement possible where the alternative was perpetual belligerency. From the process mediators' seat, it is the indispensable architecture without which no talks could convene. From the claimant states' seat, it is an open-ended deferral machine that converts their sovereign claims into permanently negotiable assets. From the displaced populations' seat, it is the reason no enforceable return date exists. Same text, same clause, four different constraints experienced — the engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying_power and process_mediators are declared beneficiaries and derive directionality near the subsidy end: the open scope subsidizes the former with retained terrain and the latter with a permanent mediation mandate. The occupying_power's d sits slightly above a pure beneficiary's because it bears genuine holding costs (declared in its situation), but no override is needed — the derivation handles the dual positioning through the secondary agenda_setter role. The territorial_claimant_states derive high d as organized payers with constrained exit; the displaced_resident_populations derive the highest d in the story — powerless, trapped, bearing the transfer in the currency of lifetimes. The unrepresented_territory_residents carry high d as excluded voices whose exclusion is itself part of the enforcement object. The international_judicial_bodies sit analytically neutral: they collect no rents and bear no extraction, but their readings feed the legitimacy conditions both substantive readings compete for.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (converting postwar facts into a stable settlement) is genuinely contested: its acute phase — ceasefire consolidation, disengagement, the first territorial returns — was substantially addressed, which is why this is not a snare wearing a coordination costume; the coordination content is real and produced physical results. But the framework's transitional justification has blurred into steady-state operation: a process originally justified as the bridge FROM war TO settlement now functions as a permanent feature whose continuation is the mediators' mandate and the occupying power's shield. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges: no dead-problem zombie flag fires, but the rising theater series independently signals the drift toward performative maintenance. Classifying this as tangled_rope rather than rope prevents laundering the extraction (deferred entitlement, unfixed lines) as mere coordination cost; classifying it as tangled_rope rather than snare preserves the genuine coordination achievement (disengagements that actually happened) from being written off as cover. The mandatrophy question — resolved or not — turns on the process_perpetuation omega below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the indefinite English article encode a deliberate drafters'' intent for discretionary withdrawal scope, or is it a drafting artifact with no determinate intent behind it?',
    'Systematic archival analysis of the documented drafting history (working papers, delegate correspondence, contemporaneous internal memoranda), weighed against the canonical interpretive materials and the French-language text; the interpretive_authority_structure sibling determines whose finding binds.',
    'If the article choice was artifact rather than intent, this reading loses its textual anchor and collapses toward the maximalist sibling: the victim set expands to include the occupying power''s retained territories, epsilon recomputes upward, and the ledger function (indefiniteness as leverage) dissolves. If intent is established, the reading stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether the discretionary reading''s textual anchor reflects intent or accident — the core committer contest of this kernel.').

omega_variable(
    secure_boundaries_scope,
    'Does the secure-and-recognized-boundaries clause license PRE-negotiation retention of strategic territories, or does it merely condition the FINAL settlement without suspending the withdrawal obligation in the interim?',
    'Legal-analytic separation of interim obligations from final-status conditions, tested against state practice in comparable territorial settlements and against the clause''s own negotiating record.',
    'If the clause only conditions the final settlement, the retention currently practiced is extraction riding on the constraint rather than within-constraint coordination — effective extraction rises sharply and the tangled_rope balance shifts toward snare. If it licenses interim retention, part of the measured extraction is the price of the security coordination itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_scope, conceptual, 'Whether the secure-boundaries principle operates as a retention license or only as a final-settlement condition.').

omega_variable(
    enforcement_line_absence,
    'Is the harm to the claimant victims the ABSENCE of a fixed enforcement line (a structural gap no party maintains), or the ACTIVE SUPPRESSION of every avenue by which one could be created?',
    'Compare outcomes for claimants in analogous disputes who obtained adjudicated lines through compulsory or consent-based mechanisms versus this dispute''s blocked avenues; trace which specific closures (jurisdiction withheld, reinterpretation vetoed) were actively maintained and by whom.',
    'If the harm is structural absence, suppression is lower than authored and the constraint is closer to a neglected coordination gap; if the closures are actively maintained, the authored suppression stands and the enforcement object is the claimants'' recourse itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_line_absence, empirical, 'Structural-gap versus active-suppression account of the victims'' position.').

omega_variable(
    process_perpetuation_question,
    'Is the phased-withdrawal process still transitional — moving measurably toward final settlement — or has it become self-perpetuating maintenance whose continuation serves the mediators'' mandate and the occupier''s shield?',
    'Test whether any phase in the recent decades moved the final-status question measurably: net territorial change attributable to negotiated process, binding-line progress, or closure of any open issue. Zero net movement across a long window indicates self-perpetuation.',
    'If self-perpetuating, the scaffold-like transitional justification is dead and the classification drifts toward inertial/theatrical maintenance — mandatrophy resolves true and the theater_ratio trend becomes the primary signal. If transitional movement continues, the arrangement retains its live coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(process_perpetuation_question, empirical, 'Whether the process is a live transition or self-perpetuating maintenance.').

omega_variable(
    mediator_neutrality_ambiguity,
    'Are the process mediators genuine neutral coordinators whose fees reflect brokerage service, or rent-collectors whose income and relevance structurally depend on the dispute remaining unresolved?',
    'Compare mediator behavior across disputes they resolved quickly versus slowly: did incentives ever favor closure, and did mediator resources scale with process duration or with settlement delivery?',
    'If mediators are structurally invested in non-resolution, their beneficiary declaration hardens and the coordination function is partly theatrical — extraction concentrates further and the arrangement moves toward captured-mediator snare flavor. If neutrality holds, their rents are coordination overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediator_neutrality_ambiguity, empirical, 'Neutral-broker versus rent-dependent-mediator account of the agenda-setting beneficiary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(u242_partial_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(u242_partial_tr_t11, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 11, 0.28).
narrative_ontology:measurement(u242_partial_tr_t22, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 22, 0.34).
narrative_ontology:measurement(u242_partial_tr_t33, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 33, 0.42).
narrative_ontology:measurement(u242_partial_tr_t44, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 44, 0.47).
narrative_ontology:measurement(u242_partial_tr_t55, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 55, 0.5).

% Extraction over time
narrative_ontology:measurement(u242_partial_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(u242_partial_be_t11, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 11, 0.5).
narrative_ontology:measurement(u242_partial_be_t22, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 22, 0.53).
narrative_ontology:measurement(u242_partial_be_t33, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 33, 0.57).
narrative_ontology:measurement(u242_partial_be_t44, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 44, 0.6).
narrative_ontology:measurement(u242_partial_be_t55, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 55, 0.63).

% Suppression requirement over time
narrative_ontology:measurement(u242_partial_su_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(u242_partial_su_t11, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 11, 0.71).
narrative_ontology:measurement(u242_partial_su_t22, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 22, 0.67).
narrative_ontology:measurement(u242_partial_su_t33, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 33, 0.62).
narrative_ontology:measurement(u242_partial_su_t44, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 44, 0.58).
narrative_ontology:measurement(u242_partial_su_t55, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 55, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, resource_allocation).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the Resolution 242 withdrawal obligation'. The label conflates three structurally distinct claims: (1) the SUBSTANTIVE SCOPE question — this file (partial_withdrawal_reading: discretionary scope, retention licensed) versus maximal_withdrawal_reading (mandatory full withdrawal per the Charter default and the French text); and (2) the META question of interpretive authority — interpretive_authority_structure (who resolves the ambiguity: court, drafters, or practice). Each member carries its own epsilon, victim set, and classification; the substantive siblings differ sharply in epsilon because one licenses retention and the other forbids it. The authority-structure sibling is UPSTREAM of both substantive readings: whichever institution adjudicates the ambiguity effectively selects which substantive constraint binds, so legitimacy shifts at the authority layer propagate to both scope readings. This file links both siblings per the family rule; orphan stories with no network connections are a code smell.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
