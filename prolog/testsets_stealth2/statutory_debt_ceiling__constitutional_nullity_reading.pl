% ============================================================================
% CONSTRAINT STORY: statutory_debt_ceiling__constitutional_nullity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statutory_debt_ceiling__constitutional_nullity_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: statutory_debt_ceiling__constitutional_nullity_reading
 *   human_readable: Statutory Debt Ceiling as Constitutionally Void Ceremonial Shell (Fourteenth Amendment Section 4 Nullity Reading)
 *   domain: constitutional_law/political_economy/fiscal_governance
 *
 * SUMMARY:
 *   This story instantiates the constitutional_nullity_reading of the
 *   statutory_debt_ceiling kernel: the claim that Section 4 of the Fourteenth
 *   Amendment — 'The validity of the public debt of the United States...
 *   shall not be questioned' — renders the statutory debt limit void, a
 *   statute incapable of validly conditioning payment of obligations Congress
 *   has already appropriated. On this reading the standing arrangement
 *   possesses near-zero valid extractive content: Treasury is obligated to
 *   borrow as appropriations require, congressional ceiling votes confer no
 *   operative legal effect, and what persists on the ground is a ceremonial
 *   shell — statutory text, periodic staged votes, brinkmanship episodes —
 *   maintained by institutional inertia. The epsilon referent is the standing
 *   ceiling arrangement as this reading assesses it, never the arrangement a
 *   sibling reading would install. Claim and metrics are authored
 *   independently: the claim (a spent instrument surviving as performance)
 *   and the descriptive metrics (low extraction, high ceremony,
 *   rising-but-void enforcement machinery) are stated from the authoring seat
 *   without reconciliation to any predicted engine output.
 *
 * KEY AGENTS:
 *   - congress_party_leadership: agenda-setter and staged performer (institutional/mobile) — schedules the periodic ceiling votes, collects position-taking value from the ceremony, retains unilateral repeal power it declines to exercise
 *   - treasury_department: administering payer (institutional/constrained) — runs borrowing and payment operations around a limit whose binding force is constitutionally disputed; bears the planning overhead and holds the uninvoked validity theory
 *   - public_bondholders: diffuse-cost bearer (organized/mobile/global) — hold debt whose repayment the Constitution guarantees; absorb episodic repricing noise from the ceremonial cycle
 *   - federal_program_operators: trapped diffuse-cost bearer (moderate/trapped/national) — schedule payments under appropriation law while contingency-planning for ceremony-driven delay scenarios
 *   - minority_leverage_faction: identity-locked performer-payer (organized/identity_locked/national) — invests strategy and credibility in a leverage instrument this reading holds void
 *   - constitutional_scholars: analytical observer — produce the Section 4 supersession literature this reading instantiates; adjudicate nothing
 *   - judiciary: abstaining observer (institutional/analytical) — the seat that could settle validity and has declined on standing and ripeness grounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statutory_debt_ceiling__constitutional_nullity_reading, 0.12).
domain_priors:suppression_score(statutory_debt_ceiling__constitutional_nullity_reading, 0.62).
domain_priors:theater_ratio(statutory_debt_ceiling__constitutional_nullity_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(statutory_debt_ceiling__constitutional_nullity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statutory_debt_ceiling__constitutional_nullity_reading, piton).
narrative_ontology:human_readable(statutory_debt_ceiling__constitutional_nullity_reading, "Statutory Debt Ceiling as Constitutionally Void Ceremonial Shell (Fourteenth Amendment Section 4 Nullity Reading)").
narrative_ontology:topic_domain(statutory_debt_ceiling__constitutional_nullity_reading, "constitutional_law/political_economy/fiscal_governance").

domain_priors:requires_active_enforcement(statutory_debt_ceiling__constitutional_nullity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statutory_debt_ceiling__constitutional_nullity_reading, 'b5ee48eb-ab9a-45cd-95e8-49a563068624').
narrative_ontology:cs_kernel_codification('b5ee48eb-ab9a-45cd-95e8-49a563068624', formalized).
narrative_ontology:cs_authority_grounding('b5ee48eb-ab9a-45cd-95e8-49a563068624', distributed).
narrative_ontology:cs_reading_relation('b5ee48eb-ab9a-45cd-95e8-49a563068624', statutory_debt_ceiling__coordination_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('b5ee48eb-ab9a-45cd-95e8-49a563068624', statutory_debt_ceiling__extraction_snare_reading, coexists_with).
narrative_ontology:cs_axiom('b5ee48eb-ab9a-45cd-95e8-49a563068624', foundational, section_four_supersedes_debt_limit_statutes).
narrative_ontology:cs_axiom_status(section_four_supersedes_debt_limit_statutes, holdable).
narrative_ontology:cs_axiom_grounding('b5ee48eb-ab9a-45cd-95e8-49a563068624', section_four_supersedes_debt_limit_statutes, conventional).
narrative_ontology:cs_axiom('b5ee48eb-ab9a-45cd-95e8-49a563068624', secondary, unconditional_payment_of_authorized_obligations).
narrative_ontology:cs_axiom_status(unconditional_payment_of_authorized_obligations, holdable).
narrative_ontology:cs_axiom_grounding('b5ee48eb-ab9a-45cd-95e8-49a563068624', unconditional_payment_of_authorized_obligations, conventional).
narrative_ontology:cs_reference_frame('b5ee48eb-ab9a-45cd-95e8-49a563068624', section_four_constitutional_supremacy).
narrative_ontology:cs_drift_state('b5ee48eb-ab9a-45cd-95e8-49a563068624', contemporary_brinkmanship_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b5ee48eb-ab9a-45cd-95e8-49a563068624', '').
narrative_ontology:cs_kernel_id(statutory_debt_ceiling__constitutional_nullity_reading, statutory_debt_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statutory_debt_ceiling__constitutional_nullity_reading, congress_party_leadership).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, public_bondholders).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, federal_program_operators).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, minority_leverage_faction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department).
narrative_ontology:constraint_vindicates(statutory_debt_ceiling__constitutional_nullity_reading, fourteenth_amendment_section_four_public_debt_clause).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the legislative calendar for the periodic debt-limit votes and stages their passage, pairing each with messaging about fiscal responsibility, and collects position-taking value from the recurring ceremony. Holds the votes needed to repeal the limit outright in ordinary majorities but consistently declines, because touching the instrument forces the constitutional question and surrenders a recurring messaging asset. Exit from the arrangement is one vote away and is never taken.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, congress_party_leadership, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, congress_party_leadership, beneficiary).

% Conducts federal borrowing auctions and payment scheduling. Maintains extraordinary-measures playbooks, deadline forecasts, and payment-prioritization contingencies — administrative machinery built around a limit whose legal warrant is disputed in a long-running constitutional debate. Its lawyers hold a developed theory under which the limit cannot validly bind payment of appropriated obligations, but the department has never acted on it unilaterally, citing market-disruption risk and the absence of judicial cover. Bears the planning overhead year after year; cannot exit without forcing a constitutional confrontation it does not control.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(statutory_debt_ceiling__constitutional_nullity_reading, treasury_department, agenda_setter).

% Hold Treasury securities whose repayment the Constitution's debt-validity clause guarantees regardless of any statutory limit. Absorb episodic repricing turbulence whenever the ceremonial cycle manufactures a deadline drama, and price that turbulence as a persistent tail premium. Their protection originates in the Constitution, not the statute, so the recurring ritual delivers them downside noise with no offsetting service. Holdings are liquid and globally reallocable, so exit is open.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, public_bondholders, payer,
    organized, biographical, mobile, global).

% Agencies, grantees, and contractors that schedule payments under appropriation law. During each standoff episode they must build contingency plans for delayed disbursements even though their funding is lawfully appropriated, absorbing planning costs and delivery uncertainty. Statutory payment calendars bind them regardless of their own views about the limit's validity, so they cannot exit the exposure.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, federal_program_operators, payer,
    moderate, immediate, trapped, national).

% A legislative caucus whose fiscal strategy is organized around debt-limit standoffs: extracting negotiating concessions by threatening the default scenario the ceremony stages. Each cycle ends in retreat or partial concession once market pressure builds, eroding credibility and consuming agenda space that alternative tactics would occupy. The tactic is fused with the caucus's identity — nominating electorates punish members who skip the ritual — so exiting the strategy costs independently of its payoff.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, minority_leverage_faction, payer,
    organized, biographical, identity_locked, national).

% Academic lawyers and historians who develop, defend, and critique the Section 4 supersession argument. They supply the validity case, publish the counterarguments, and train the officials who might someday act on the theory. They adjudicate nothing and bear none of the arrangement's costs.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% Federal courts that would settle whether the debt-limit statute can validly condition payment of public obligations. Have declined every invitation — standing doctrine, ripeness, political-question avoidance — leaving the validity element permanently unadjudicated while lower-stakes fiscal litigation proceeds around it.
narrative_ontology:constraint_stakeholder(statutory_debt_ceiling__constitutional_nullity_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statutory_debt_ceiling__constitutional_nullity_reading, diffuse).
narrative_ontology:fixing_cost_class(statutory_debt_ceiling__constitutional_nullity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Descriptively, the arrangement gives Congress a single recurring vote at which the aggregate debt limit is reviewed and adjusted, replacing per-issuance authorization with one summary instrument; on this reading the instrument's coordinating force is void, and the vote survives as ceremony.
% TRANSFER_FUNCTION: As written, the instrument moves no resources — it appropriates nothing and authorizes no outlay. As performed, each cycle converts scheduled debt service into temporary negotiating leverage for whichever faction controls the vote, transferring concession value from the executive to that faction; this reading holds every such transfer void of legal warrant — a voluntary executive gift rather than a valid extraction.
% ABSENT_VOICES: Bondholders and program recipients are discussed but never seated at the negotiation. Treasury's legal advisers hold the validity judgment and are structurally prevented from rendering it publicly. Federal courts — the only seat with authority to settle the question — abstain on standing and ripeness grounds, so the conversation proceeds indefinitely without its deciding voice.
% DISAPPEARANCE_RATIONALE: Appropriations are already binding payment commands and the Constitution's debt clause already guarantees their validity; delete the ceiling overnight and Treasury executes the same borrowing schedule the next morning. What vanishes is the ceremonial vote cycle, the episodic brinkmanship, and the tail premium priced around it — arrangements of performance, not dependence.
% FOUNDING_PROBLEM: After 1917 (consolidated in 1939), Congress sought a single summary instrument to supervise aggregate federal debt without re-authorizing each bond issue individually — macro-control over the debt path at one vote instead of hundreds.
% FOUNDING_PROBLEM_CORROBORATION: No voice from outside the arrangement's performers attests the founding problem as live: congressional research analyses document that the limit alters issuance timing, not debt totals; rating-agency assessments treat it as a pure default-risk overlay with no fiscal-discipline content; and the constitutional-law literature treats its supervisory purpose as superseded by binding appropriations plus the Section 4 payment command. The only voices asserting liveness are the factions that perform the ritual.
narrative_ontology:disappearance_verdict(statutory_debt_ceiling__constitutional_nullity_reading, world_unchanged).
narrative_ontology:founding_problem_status(statutory_debt_ceiling__constitutional_nullity_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(statutory_debt_ceiling__constitutional_nullity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statutory_debt_ceiling__constitutional_nullity_reading, 0.12, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).
:- end_tests(statutory_debt_ceiling__constitutional_nullity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12 at interval end) because the reading's core claim is that the ceiling cannot validly extract: the small residual represents the real costs of unconstitutional deference (extraordinary-measures gymnastics, tail risk premia), not valid operation. Suppression (0.62) is authored as a raw structural property, unscaled: the enforcement machinery — deadline choreography, market-pressure messaging, payment-prioritization contingencies — is intense and has intensified across the interval, even though this reading holds its legal warrant void. Theater_ratio is high (0.82) because nearly all remaining activity is performance: votes whose product is immediately signed and ignored, speeches addressed to bond desks, deadlines manufactured and deferred. Accessibility_collapse is low (0.18) because understanding the ceiling as void opens alternatives rather than closing them — the 'alternative' (borrow per appropriations) is the legally commanded course. Resistance (0.58) reflects sustained contestation: a developed scholarly literature, executive flirtation with the theory, and repeated refusals to concede leverage, none yet decisive. The temporal series run on one shared eight-point grid (1979-2025) so every metric is authored at every examined time point; the mild oscillation in theater and enforcement (1995-96, 2011, 2013 peaks; troughs under unified government) tracks divided-government cycles, and the oscillation itself functions as intermittent reinforcement — each resolved crisis resets the credibility of the next threat while transferring nothing of legal validity. The suppression_requirement series is authored deliberately: the story specifically traces the buildup of enforcement machinery (from routine raises through extraordinary-measures doctrine to repeated deadline standoffs), which is an enforcement-capacity trajectory, not merely a shift in extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same statutory text. From the leadership seat the arrangement is a recurring stage it owns: a messaging asset with repeal power one vote away. From the bondholder seat it is pure downside noise layered on a constitutional guarantee they already hold. From the program-operator seat it is contingent dread — lawful funds that might arrive late. From the leverage-faction seat it is a fused identity strategy whose payoffs this reading scores as void-warranted executive gifts. From the scholarly and judicial seats it is an unanswered validity question. The nullity reading predicts these experiences converge toward inertness the moment the validity element is settled; the perspectival gap IS the unadjudicated premise. The engine computes per-seat classifications from the structural data; this story's claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership sits near the beneficiary pole: it collects position-taking rents from the ceremony, holds mobile exit (repeal is a majority vote), and bears almost none of the arrangement's costs — derived d is low, and its effective extraction is damped toward subsidy. The declared victim groups sit near the target pole, but their effective extraction is heavily damped by the near-zero base epsilon: bondholders (mobile exit, global scope) least trapped, program operators (trapped, national) most exposed to the ceremony's timing effects, the leverage faction paying in credibility and foregone strategy. Treasury occupies an intermediate-high position the derivation approximates well from its payer role, constrained exit, and administrator secondary role — it bears disproportionate compliance overhead while administering the very machinery it could legally refuse; no override is needed because the beneficiary/victim declarations plus exit atoms already place it correctly. Scholars and the judiciary are analytical seats contributing no directional mass. National-to-global scope slightly amplifies verification difficulty for the payer seats, but with epsilon this low the amplification moves little.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — replacing per-issuance debt authorization with a single summary instrument of congressional macro-control — is dead on this reading: binding appropriations now constitute the payment command, and the Section 4 clause guarantees their validity irrespective of any ceiling. What persists is the shell: text, votes, brinkmanship. The genealogy interview resolves cleanly here: founding_problem_status=dead paired with disappearance_verdict=world_unchanged produces no zombie flag, correctly, because nothing material depends on the arrangement. The contrast with the snare sibling is instructive: the same dead founding problem under that reading pairs with world_rearranges (removing the ceiling removes live minority leverage) and trips the capture flag. Decomposing the kernel into three epsilon-invariant stories is what allows the framework to distinguish a world where the ceiling is a spent ceremony from a world where it is a loaded weapon — the difference is a single unadjudicated legal premise, not a difference in the statute's text.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the statutory_debt_ceiling kernel — the constitutional_nullity_reading. What would the sibling readings (coordination_scaffold_reading, extraction_snare_reading) change structurally if adopted instead?',
    'Adoption of the scaffold sibling re-instantiates the arrangement with a genuine coordination floor and near-symmetric directionalities; adoption of the snare sibling re-instantiates it with high epsilon and a capturable gain seat in the leverage faction. The choosing actor is whichever institution first settles the validity premise.',
    'Classification flips from the void-shell profile authored here to a scaffold profile (low extraction, coordination function live) or a snare profile (high extraction, concentrated capturer). Gain_flow ceases to be diffuse under the snare sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame routing: this story is one of three competing readings of a single statutory kernel; the siblings are separate constraint files, not internal hedges.').

omega_variable(
    section_four_adjudication_absence,
    'Has any authoritative body ever adjudicated on the merits whether Section 4''s debt-validity command supersedes the ceiling''s operative force?',
    'A judicial ruling reached on the merits (courts have so far declined on standing and ripeness grounds), or a Treasury invocation of the theory that forces adjudication.',
    'If the ceiling is held valid, this reading collapses toward the snare or scaffold siblings and epsilon rises sharply; if the nullity claim is vindicated, the ceiling becomes an operative nullity, theater_ratio approaches 1.0, and the shell''s remaining enforcement loses all warrant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(section_four_adjudication_absence, empirical, 'The validity element on which all three sibling readings diverge has never been settled by any court.').

omega_variable(
    deference_extraction_boundary,
    'How much of the ceiling''s observed bite is valid operation versus unconstitutional deference to a void limit?',
    'The same adjudication trigger as section_four_adjudication_absence, supplemented by counterfactual analysis of episodes in which the executive treated minor ceiling headroom as ignorable.',
    'Determines whether the residual 0.12 extractiveness reflects any valid extractive content at all or is wholly self-inflicted compliance cost; a pure-deference verdict drives effective epsilon toward zero for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deference_extraction_boundary, conceptual, 'The nullity reading scores all observed bite as deference; no current measurement apparatus can separate the two without a validity ruling.').

omega_variable(
    treasury_invocation_propensity,
    'Would the Treasury ever unilaterally treat the ceiling as void and borrow as appropriations require, without political cover?',
    'Revealed behavior in a future deadline crisis pitting a unified government against the ceremonial cycle, or explicit doctrinal statements from successive Treasury legal counsel.',
    'If yes, the shell terminates as an inert relic and this reading becomes simple description; if no, the shell persists indefinitely on institutional risk aversion alone, and the piton profile hardens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treasury_invocation_propensity, empirical, 'Institutional risk aversion and market-disruption fear currently prevent the one act that would end the arrangement.').

omega_variable(
    dormant_shell_reloadability,
    'Is the ceiling a spent instrument surviving as ceremony, or a deliberately kept reloadable one whose extraction capacity is merely parked between activations?',
    'Examine whether post-suspension reinstatements restore binding force or merely resume the ceremony: if each expiration date is a loaded spring rather than an artifact, the shell retains live function.',
    'A reloadable verdict would reclassify the arrangement away from the spent-instrument profile toward a dormant variant of the snare sibling, with high conditional epsilon and the leverage faction as capturer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dormant_shell_reloadability, conceptual, 'Suspension-and-reinstatement cycles keep the instrument''s expiry architecture intact; whether that architecture is vestigial or latent is unresolved.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statutory_debt_ceiling__constitutional_nullity_reading, 1979, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sdc_nullity_tr_t1979, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1979, 0.3).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1979, observed).
narrative_ontology:measurement(sdc_nullity_tr_t1985, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1985, observed).
narrative_ontology:measurement(sdc_nullity_tr_t1995, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 1995, 0.52).
narrative_ontology:measurement_basis(sdc_nullity_tr_t1995, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2003, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2003, 0.46).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2003, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2011, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2011, 0.76).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2011, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2016, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2016, 0.64).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2016, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2021, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2021, 0.7).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2021, observed).
narrative_ontology:measurement(sdc_nullity_tr_t2025, statutory_debt_ceiling__constitutional_nullity_reading, theater_ratio, 2025, 0.82).
narrative_ontology:measurement_basis(sdc_nullity_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(sdc_nullity_be_t1979, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1979, 0.04).
narrative_ontology:measurement_basis(sdc_nullity_be_t1979, observed).
narrative_ontology:measurement(sdc_nullity_be_t1985, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1985, 0.05).
narrative_ontology:measurement_basis(sdc_nullity_be_t1985, observed).
narrative_ontology:measurement(sdc_nullity_be_t1995, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 1995, 0.07).
narrative_ontology:measurement_basis(sdc_nullity_be_t1995, observed).
narrative_ontology:measurement(sdc_nullity_be_t2003, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2003, 0.06).
narrative_ontology:measurement_basis(sdc_nullity_be_t2003, observed).
narrative_ontology:measurement(sdc_nullity_be_t2011, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2011, 0.11).
narrative_ontology:measurement_basis(sdc_nullity_be_t2011, observed).
narrative_ontology:measurement(sdc_nullity_be_t2016, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2016, 0.09).
narrative_ontology:measurement_basis(sdc_nullity_be_t2016, observed).
narrative_ontology:measurement(sdc_nullity_be_t2021, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2021, 0.1).
narrative_ontology:measurement_basis(sdc_nullity_be_t2021, observed).
narrative_ontology:measurement(sdc_nullity_be_t2025, statutory_debt_ceiling__constitutional_nullity_reading, base_extractiveness, 2025, 0.12).
narrative_ontology:measurement_basis(sdc_nullity_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(sdc_nullity_su_t1979, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1979, 0.2).
narrative_ontology:measurement_basis(sdc_nullity_su_t1979, observed).
narrative_ontology:measurement(sdc_nullity_su_t1985, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1985, 0.23).
narrative_ontology:measurement_basis(sdc_nullity_su_t1985, observed).
narrative_ontology:measurement(sdc_nullity_su_t1995, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement_basis(sdc_nullity_su_t1995, observed).
narrative_ontology:measurement(sdc_nullity_su_t2003, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2003, 0.36).
narrative_ontology:measurement_basis(sdc_nullity_su_t2003, observed).
narrative_ontology:measurement(sdc_nullity_su_t2011, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2011, 0.61).
narrative_ontology:measurement_basis(sdc_nullity_su_t2011, observed).
narrative_ontology:measurement(sdc_nullity_su_t2016, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2016, 0.51).
narrative_ontology:measurement_basis(sdc_nullity_su_t2016, observed).
narrative_ontology:measurement(sdc_nullity_su_t2021, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2021, 0.56).
narrative_ontology:measurement_basis(sdc_nullity_su_t2021, observed).
narrative_ontology:measurement(sdc_nullity_su_t2025, statutory_debt_ceiling__constitutional_nullity_reading, suppression_requirement, 2025, 0.62).
narrative_ontology:measurement_basis(sdc_nullity_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, coordination_scaffold_reading).
narrative_ontology:affects_constraint(statutory_debt_ceiling__constitutional_nullity_reading, extraction_snare_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'debt ceiling' decomposes into three structurally distinct claims sharing one statutory referent but differing in epsilon: a valid coordination instrument (coordination_scaffold_reading), a valid extraction weapon (extraction_snare_reading), and a constitutionally void shell (this file). The validity premise — whether Section 4 supersedes the ceiling's operative force — is the single element on which the readings diverge; it is unadjudicated, so each reading is authored as its own epsilon-invariant story with its own beneficiaries, victims, and type. Edges run family-wide: each sibling lists the others in affects_constraints, and the upstream element (the validity premise) governs which downstream epsilon obtains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
